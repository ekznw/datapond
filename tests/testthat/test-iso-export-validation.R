iso_validation_example <- function() {
  datapond:::iso_validation_resource(
    "extdata", "iso_validation", "1.3.0", "upstream", "schemas.isotc211.org",
    "19115", "-1", "mdb", "1.3.0", "examples", "D.1Minimal.xml"
  )
}

require_iso_validation_engine <- function() {
  python <- Sys.getenv("DATAPOND_ISO_PYTHON", unset = Sys.which("python3"))
  skip_if(!nzchar(python), "Optional ISO validator requires Python 3 and lxml")
  probe <- tryCatch(processx::run(python, c("-c", "import lxml.etree, lxml.isoschematron"),
                                error_on_status = FALSE), error = function(e) NULL)
  skip_if(is.null(probe) || probe$status != 0L, "Optional ISO validator requires lxml")
}

test_that("the official minimal document passes offline pinned validation", {
  require_iso_validation_engine()
  report <- datapond:::validate_iso_metadata_xml(iso_validation_example())
  expect_true(report$valid, info = paste(report$issues$message, collapse = "\n"))
  expect_identical(report$status, "pass")
  expect_identical(report$schema_package, "1.3.0")
  expect_equal(nrow(report$checks), 15L)
  expect_true(all(report$checks$valid))
  # Published namespace corrections must never disappear from the report.
  expect_equal(nrow(report$adaptations), 7L)
  expect_equal(sum(grepl("Bind gco", report$adaptations$change)), 3L)
  expect_true(any(grepl("LI_Source constraint", report$adaptations$change, fixed = TRUE)))
  expect_match(report$coverage, "not crosswalk coverage", fixed = TRUE)
})

test_that("XSD detects missing mandatory contact and invalid property order", {
  require_iso_validation_engine()
  document <- xml2::read_xml(iso_validation_example())
  contact <- xml2::xml_find_first(document, "/mdb:MD_Metadata/mdb:contact", xml2::xml_ns(document))
  xml2::xml_remove(contact)
  report <- datapond:::validate_iso_metadata_xml(document)
  expect_false(report$valid)
  expect_identical(report$status, "fail")
  expect_true(any(report$issues$stage == "xsd"))
  expect_true(any(grepl("contact", report$issues$message)))

  document <- xml2::read_xml(iso_validation_example())
  contact <- xml2::xml_find_first(document, "/mdb:MD_Metadata/mdb:contact", xml2::xml_ns(document))
  xml2::xml_add_child(xml2::xml_root(document), contact)
  xml2::xml_remove(contact)
  report <- datapond:::validate_iso_metadata_xml(document)
  expect_false(report$valid)
  expect_false(report$checks$valid[report$checks$stage == "xsd"])
})

test_that("Schematron catches a conditional omission that XSD permits", {
  require_iso_validation_engine()
  document <- xml2::read_xml(iso_validation_example())
  xml2::xml_remove(xml2::xml_find_all(document, "//cit:CI_Organisation/cit:name", xml2::xml_ns(document)))
  report <- datapond:::validate_iso_metadata_xml(document)
  expect_false(report$valid)
  expect_true(report$checks$valid[report$checks$stage == "xsd"])
  expect_true(any(report$issues$stage == "schematron"))
  expect_true(all(nzchar(report$issues$location)))
  expect_true(any(grepl("name|logo", report$issues$message, ignore.case = TRUE)))
})

test_that("older namespaces are an exporter defect, not a demand for more fields", {
  require_iso_validation_engine()
  document <- xml2::read_xml('<mdb:MD_Metadata xmlns:mdb="http://standards.iso.org/iso/19115/-3/mdb/2.0"/>')
  report <- datapond:::validate_iso_metadata_xml(document)
  expect_false(report$valid)
  expect_identical(report$status, "fail")
  expect_identical(report$issues$stage, "namespace")
  expect_match(report$issues$message, "update the serializer", fixed = TRUE)
})

test_that("malformed XML and entity declarations cannot pass", {
  require_iso_validation_engine()
  path <- tempfile(fileext = ".xml")
  on.exit(unlink(path), add = TRUE)
  writeLines("<broken>", path)
  report <- datapond:::validate_iso_metadata_xml(path)
  expect_identical(report$status, "fail")
  expect_identical(report$issues$stage, "xml")
  writeLines('<!DOCTYPE metadata [<!ENTITY test SYSTEM "file:///not-to-be-read">]><metadata>&test;</metadata>', path)
  report <- datapond:::validate_iso_metadata_xml(path)
  expect_false(report$valid)
  expect_match(report$issues$message, "DTD or entity", fixed = TRUE)
})

test_that("missing tooling and damaged pinned resources are unavailable, never pass", {
  report <- datapond:::validate_iso_metadata_xml(iso_validation_example(), python = "")
  expect_false(report$valid)
  expect_identical(report$status, "unavailable")
  require_iso_validation_engine()
  bundle <- tempfile("iso-bundle-")
  dir.create(bundle)
  on.exit(unlink(bundle, recursive = TRUE), add = TRUE)
  writeLines("altered", file.path(bundle, "test.xsd"))
  jsonlite::write_json(list(schema_package = "1.3.0", resources = list(
    list(url = "https://example.test/test.xsd", path = "test.xsd", sha256 = "wrong")
  )), file.path(bundle, "manifest.json"), auto_unbox = TRUE)
  example <- iso_validation_example()
  resource <- datapond:::iso_validation_resource
  local_mocked_bindings(iso_validation_resource = function(...) {
    if (identical(list(...)[[1]], "extdata")) bundle else resource(...)
  }, .package = "datapond")
  report <- datapond:::validate_iso_metadata_xml(example)
  expect_identical(report$status, "unavailable")
  expect_false(report$valid)
  expect_match(report$issues$message, "checksum mismatch", fixed = TRUE)
})

test_that("candidate assembly attaches lineage without flattening its documents", {
  metadata <- xml2::read_xml(iso_validation_example())
  lineage <- xml2::read_xml('<mrl:LI_Lineage xmlns:mrl="http://standards.iso.org/iso/19115/-3/mrl/2.0" xmlns:gco="http://standards.iso.org/iso/19115/-3/gco/1.0" xmlns:cit="http://standards.iso.org/iso/19115/-3/cit/2.0"><mrl:statement><gco:CharacterString>Derived from source surveys.</gco:CharacterString></mrl:statement><mrl:additionalDocumentation><cit:CI_Citation><cit:title><gco:CharacterString>Detailed method</gco:CharacterString></cit:title></cit:CI_Citation></mrl:additionalDocumentation></mrl:LI_Lineage>')
  local_mocked_bindings(
    build_iso_wave3_metadata_xml = function(...) metadata,
    build_iso_lineage_xml = function(...) lineage, .package = "datapond"
  )
  document <- datapond:::build_iso_metadata_candidate_xml(1L, "unused")
  # Round-trip to ensure copied namespaces remain bound after writing XML.
  document <- xml2::read_xml(as.character(document))
  ns <- c(mdb = "https://schemas.isotc211.org/19115/-1/mdb/1.3",
          mrl = "http://standards.iso.org/iso/19115/-3/mrl/2.0")
  expect_length(xml2::xml_find_all(document, "/mdb:MD_Metadata/mdb:resourceLineage/mrl:LI_Lineage", ns), 1L)
  expect_match(as.character(document), "Derived from source surveys.", fixed = TRUE)
  expect_match(as.character(document), "Detailed method", fixed = TRUE)
})

test_that("assembly failure is reported separately from XML conformance", {
  local_mocked_bindings(build_iso_metadata_candidate_xml = function(...) {
    stop("A primary metadata point of contact is required")
  }, .package = "datapond")
  result <- datapond:::assess_iso_metadata_export(1L, "unused")
  expect_null(result$document)
  expect_false(result$validation$valid)
  expect_identical(result$validation$status, "not-built")
  expect_identical(result$validation$issues$stage, "assembly")
  expect_match(result$validation$issues$message, "point of contact", fixed = TRUE)
  expect_identical(result$crosswalk_version, datapond:::current_iso_crosswalk_version())
})

test_that("core spatial database exports validate and retain document lineage", {
  require_iso_validation_engine()
  ponds <- character()
  on.exit(unlink(ponds, recursive = TRUE), add = TRUE)
  for (kind in c("vector", "raster")) {
    fixture <- iso_export_candidate_pond(kind)
    ponds <- c(ponds, fixture$path)
    result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "core")
    expect_s3_class(result$document, "xml_document")
    expect_identical(result$validation$status, "pass", info = paste(result$validation$issues$message, collapse = "\n"))
    expect_identical(result$validation$export_scope, "core")
    text <- as.character(result$document)
    expect_match(text, paste("ISO", kind, "fixture"), fixed = TRUE)
    expect_match(text, "Survey protocol", fixed = TRUE)
    expect_match(text, "documents/survey-protocol.pdf", fixed = TRUE)
    expect_match(text, paste("DataPond crosswalk", result$crosswalk_version), fixed = TRUE)
  }
})

test_that("a geographic table requires explicit coverage without inferring a new scope", {
  fixture <- iso_export_candidate_pond("tabular")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path)
  expect_null(result$document)
  expect_identical(result$validation$status, "not-built")
  expect_match(result$validation$issues$message, "bounding box", fixed = TRUE)
  expect_match(result$validation$issues$message, "area covered", fixed = TRUE)
  expect_false(grepl("coordinate reference system", result$validation$issues$message, fixed = TRUE))
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  expect_identical(DBI::dbGetQuery(conn, "SELECT metadata_resource_scope_code FROM entry_tbl")$metadata_resource_scope_code,
                   "dataset")
  expect_equal(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM dataset_spatial_summary_tbl")$n, 0L)
})
