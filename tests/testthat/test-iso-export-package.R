package_fixture <- function() {
  f <- distribution_fixture()
  conn <- datapond:::get_conn(f$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='eng', resource_character_encoding='utf8'")
  DBI::dbDisconnect(conn)
  datapond:::save_dataset_feature_catalogue(1, list(title = "Package test attributes",
    version = "1", version_date = "2026-10-01", language_code = "eng", producer_organisation_id = 1L), f$path)
  f$root <- iso_package_fixture_files(f)
  f
}

test_that("package assessment selects cited files and keeps external links offline", {
  core_validation_engine()
  f <- package_fixture()
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "INSERT INTO supporting_document_tbl
    (dataset_id, document_rel_path, document_name, active, include_in_metadata)
    VALUES (1, 'documents/not-selected.pdf', 'Not selected', 1, 0),
           (1, 'documents/inactive.pdf', 'Inactive', 0, 1),
           (1, 'https://example.test/report.pdf', 'External report', 1, 1)")
  dir.create(file.path(f$root, "scripts"))
  writeLines("# Recorded method", file.path(f$root, "scripts", "method.R"))
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_process_step_tbl
    (dataset_id, step_order, description, script_reference)
    VALUES (1, 1, 'Apply the documented method.', 'scripts/method.R')")
  a <- datapond:::assess_iso_metadata_package(1, f$path)
  expect_true(a$validation$valid, info = paste(a$validation$issues$message, collapse = "\n"))
  expect_true(a$package$valid)
  expect_setequal(a$package$files$path, c("documents/survey-protocol.pdf", "scripts/method.R"))
  expect_true("https://example.test/report.pdf" %in% a$package$external)
  report <- datapond:::iso_package_report(a, "pinned")
  expect_false("source" %in% names(report$package$files))
  expect_false(grepl(f$path, jsonlite::toJSON(report), fixed = TRUE))
  unlink(file.path(f$root, "documents", "survey-protocol.pdf"))
  check <- datapond:::iso_package_dependencies(a, 1, f$path)
  expect_false(check$valid)
  expect_match(check$issues$message, "Missing supporting document")
  # XML rules remain separate from package availability.
  expect_true(a$validation$valid)
})

test_that("unsafe, escaping and colliding package paths are refused", {
  core_validation_engine()
  f <- package_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  a <- datapond:::assess_iso_metadata_package(1, f$path)
  ns <- xml2::xml_ns(a$document)
  node <- xml2::xml_find_first(a$document,
    "//cit:CI_Citation[@id='datapond-lineage-document-1']//cit:linkage/gco:CharacterString", ns)
  for (path in c("../outside.pdf", "/tmp/outside.pdf", "file:///tmp/outside.pdf",
                 "documents/%2e%2e/private.pdf", "./documents/survey-protocol.pdf",
                 "metadata.xml", "validation-report.json", "metadata.xml/child",
                 "documents/survey-protocol.pdf#fragment", a$feature_catalogue_filename)) {
    xml2::xml_set_text(node, path)
    check <- datapond:::iso_package_dependencies(a, 1, f$path)
    expect_false(check$valid, info = path)
  }
  outside <- tempfile(); writeLines("Outside dataset", outside)
  on.exit(unlink(outside), add = TRUE)
  skip_on_os("windows")
  expect_true(file.symlink(outside, file.path(f$root, "documents", "escape.pdf")))
  xml2::xml_set_text(node, "documents/escape.pdf")
  check <- datapond:::iso_package_dependencies(a, 1, f$path)
  expect_match(check$issues$message, "outside the dataset")
})

test_that("publication stages a full package and never replaces earlier exports", {
  core_validation_engine()
  f <- package_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  parent <- tempfile(); dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  writeLines("Unrelated user file", file.path(parent, "keep.txt"))
  paths <- datapond:::write_iso_wave3_documents(1, f$path, parent)
  first <- dirname(paths[["metadata"]])
  expect_true(all(file.exists(paths)))
  expect_identical(readLines(file.path(first, "documents/survey-protocol.pdf")),
    readLines(file.path(f$root, "documents/survey-protocol.pdf")))
  before <- tools::md5sum(paths)
  paths2 <- datapond:::write_iso_wave3_documents(1, f$path, parent)
  expect_false(identical(dirname(paths2[["metadata"]]), first))
  expect_identical(tools::md5sum(paths), before)
  expect_identical(readLines(file.path(parent, "keep.txt")), "Unrelated user file")
  r <- jsonlite::read_json(paths[["validation"]], simplifyVector = TRUE)
  expect_true(r$validation$valid)
  expect_true(r$package$valid)
  expect_identical(r$package$files$md5, unname(tools::md5sum(file.path(first, r$package$files$path))))
  expect_length(list.files(parent, pattern = "^\\.iso-stage-", all.files = TRUE), 0L)
})

test_that("failed copies, changed source bytes, staged validation and publication leave no partial package", {
  core_validation_engine()
  f <- package_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  parent <- tempfile(); dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  paths <- datapond:::write_iso_wave3_documents(1, f$path, parent)
  before <- tools::md5sum(paths)
  existing <- list.files(parent, all.files = TRUE)
  a <- datapond:::assess_iso_metadata_package(1, f$path)
  fail_with <- function(binding, replacement, pattern) {
    testthat::local_mocked_bindings(!!!stats::setNames(list(replacement), binding), .package = "datapond")
    expect_error(datapond:::iso_publish_metadata_package(a, parent, "pinned"), pattern)
    expect_identical(list.files(parent, all.files = TRUE), existing)
    expect_identical(tools::md5sum(paths), before)
  }
  fail_with("iso_package_copy", function(...) FALSE, "could not copy")
  fail_with("iso_package_publish", function(...) FALSE, "could not publish")
  fail_with("validate_iso_content_documents", function(...) list(valid = FALSE), "staged XML validation")
  fail_with("validate_iso_export_semantics", function(...) list(valid = FALSE), "staged semantic validation")
  writeLines("Changed since assessment", file.path(f$root, "documents/survey-protocol.pdf"))
  expect_error(datapond:::iso_publish_metadata_package(a, parent, "pinned"), "unchanged supporting file")
  expect_identical(list.files(parent, all.files = TRUE), existing)
})

test_that("ZIP download rechecks metadata and contains portable document references", {
  core_validation_engine()
  skip_if(!nzchar(Sys.which("zip")), "zip executable unavailable")
  f <- package_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  target <- tempfile(fileext = ".zip")
  on.exit(unlink(target), add = TRUE)
  datapond:::iso_download_metadata_package(1, f$path, target, "pinned")
  contents <- utils::unzip(target, list = TRUE)$Name
  expect_true(any(endsWith(contents, "/metadata.xml")))
  expect_true(any(endsWith(contents, "/documents/survey-protocol.pdf")))
  expect_false(any(grepl("/data/", contents, fixed = TRUE)))
  conn <- datapond:::get_conn(f$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date=NULL")
  DBI::dbDisconnect(conn)
  before <- tools::md5sum(target)
  expect_error(datapond:::iso_download_metadata_package(1, f$path, target, "pinned"), "ISO export was not written")
  expect_identical(tools::md5sum(target), before)
})

test_that("report guidance distinguishes date policy, file issues and tools", {
  expect_identical(datapond:::iso_export_guidance("Missing metadata creation date")$field, "iso_profile-metadata_creation_date")
  expect_identical(datapond:::iso_export_guidance("Missing supporting document", stage = "package")$section, "supporting_documents")
  expect_identical(datapond:::iso_export_guidance("Install lxml", stage = "engine")$section, "")
  ui <- as.character(datapond:::mod_iso_export_ui("iso_export"))
  expect_match(ui, "Checks the saved record")
  expect_match(ui, "Pinned ISO rules")
  expect_match(ui, "<summary>")
})

test_that("app report checks saved records, navigates issues and invalidates on record changes", {
  local_trusted_access_fixture()

  core_validation_engine()
  f <- package_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date='2016-03-02'")
  DBI::dbDisconnect(conn)
  state <- shiny::reactiveValues(data_pond = f$path, current_dataset_id = 1L, refresh_datasets = 0L)
  navigated <- NULL
  shiny::testServer(datapond:::mod_iso_export_server, args = list(state = state,
    open_section = function(section, field) navigated <<- c(section, field)), {
    session$flushReact()
    expect_null(assessment())
    session$setInputs(assess = 1L, policy = "pinned")
    expect_identical(assessment()$validation$status, "fail")
    expect_identical(assessment()$validation$datapond_profile$status, "pass")
    expect_match(output$downloads$html, "blocked")
    session$setInputs(policy = "datapond")
    expect_match(output$downloads$html, "Validate &amp; download")
    expect_match(output$report$html, "warnings / unknown values")
    expect_match(output$report$html, "Feature catalogue:")
    session$setInputs(open_issue = 1L)
    expect_identical(navigated, c("metadata_review", "iso_profile-metadata_creation_date"))
    state$refresh_vocab <- 1L
    session$flushReact()
    expect_null(assessment())
    session$setInputs(assess = 2L)
    state$refresh_datasets <- 1L
    session$flushReact()
    expect_null(assessment())
    session$setInputs(assess = 3L)
    expect_false(is.null(assessment()))
    state$current_dataset_id <- NULL
    session$flushReact()
    expect_null(assessment())
    session$setInputs(assess = 4L)
    expect_match(error(), "Select and save a dataset")
  })
})

test_that("missing optional validation tools block publishing with actionable guidance", {
  f <- package_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  withr::local_envvar(DATAPOND_ISO_PYTHON = "")
  a <- datapond:::assess_iso_metadata_package(1, f$path)
  expect_identical(a$validation$status, "unavailable")
  expect_identical(a$validation$datapond_profile$status, "unavailable")
  expect_true(a$package$valid)
  rows <- datapond:::iso_export_issues(a)
  expect_true(any(grepl("Python", rows$message)))
  parent <- tempfile(); dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  expect_error(datapond:::iso_publish_metadata_package(a, parent, "datapond"), "ISO export was not written")
  expect_length(list.files(parent), 0L)
})
