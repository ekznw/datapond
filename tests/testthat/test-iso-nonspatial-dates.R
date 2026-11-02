nonspatial_fixture <- function(scope = "dataset", area = NA_character_) {
  f <- distribution_fixture("tabular")
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_resource_scope_code=?,geographic_extent_name=?,
    resource_language_code='eng',resource_character_encoding='utf8'",
    params = list(scope, area))
  datapond:::save_dataset_feature_catalogue(1, list(title = "Tabular catalogue"), f$path)
  f
}

test_that("named geographic and genuinely nongeographic tables validate without invented coordinates", {
  core_validation_engine()
  for (scope in c("dataset", "nonGeographicDataset")) {
    area <- if (scope == "dataset") "KwaZulu-Natal, South Africa" else NA_character_
    f <- nonspatial_fixture(scope, area)
    a <- datapond:::assess_iso_metadata_export(1, f$path)
    expect_true(a$validation$valid, info = paste(a$validation$issues$message, collapse = "\n"))
    expect_true(a$validation$datapond_profile$valid)
    expect_true(a$validation$feature_catalogue$valid)
    expect_identical(core_xpath_text(a$document, "//mdb:resourceScope/*"), scope)
    expect_length(core_xpath_text(a$document, "//gex:EX_GeographicBoundingBox"), 0L)
    expect_length(core_xpath_text(a$document, "//mdb:referenceSystemInfo"), 0L)
    if (scope == "dataset") {
      expect_identical(core_xpath_text(a$document, "//gex:geographicIdentifier//mcc:code/*"), area)
    } else {
      expect_identical(core_xpath_text(a$document, "//mdb:MD_MetadataScope/mdb:name/*"), "ISO tabular fixture")
      expect_length(core_xpath_text(a$document, "//gex:geographicIdentifier"), 0L)
    }
    status <- datapond:::get_dataset_completion_status(1, f$path, NULL)
    expect_identical(status$status[status$section == "Geographic discovery"],
      if (scope == "dataset") "complete" else "not_applicable")
    expect_identical(status$status[status$section == "Spatial metadata"], "not_applicable")
    unlink(f$path, recursive = TRUE)
  }
})

test_that("place keywords cannot supply coverage and named areas do not conceal invalid coordinates", {
  f <- nonspatial_fixture()
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  datapond:::ensure_columns(conn, "entry_tbl", "keyword_geo")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET keyword_geo='KwaZulu-Natal'")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "explicitly recorded area covered")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET geographic_extent_name='KwaZulu-Natal'")
  DBI::dbExecute(conn, "INSERT INTO dataset_spatial_summary_tbl(dataset_id,bbox_status,xmin,ymin,xmax,ymax)
    VALUES (1,'valid',29,-30,31,-28)")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "coordinate reference system")
  DBI::dbExecute(conn, "UPDATE dataset_spatial_summary_tbl SET xmin=999")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "valid WGS84 bounds")
})

test_that("schema migration preserves unknown creation and save captures only genuine new records", {
  f <- nonspatial_fixture("nonGeographicDataset")
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date=NULL")
  datapond:::ensure_iso_profile_schema(conn)
  datapond:::ensure_iso_profile_schema(conn)
  expect_true(is.na(DBI::dbGetQuery(conn, "SELECT metadata_creation_date FROM entry_tbl")[[1]]))
  datapond:::save_entry_dynamic("entry_tbl", list(title = "Updated older record"), f$path, id = 1)
  expect_true(is.na(DBI::dbGetQuery(conn, "SELECT metadata_creation_date FROM entry_tbl")[[1]]))
  id <- datapond:::save_entry_dynamic("entry_tbl", list(uuid = "new-record", title = "New record"), f$path)
  stamp <- DBI::dbGetQuery(conn, "SELECT metadata_creation_date FROM entry_tbl WHERE id=?", params = list(id))[[1]]
  expect_match(stamp, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
  datapond:::save_entry_dynamic("entry_tbl", list(title = "Later edit"), f$path, id = id)
  expect_identical(DBI::dbGetQuery(conn, "SELECT metadata_creation_date FROM entry_tbl WHERE id=?", params = list(id))[[1]], stamp)
  # Transfers bypass application INSERT capture, retaining unknown or date-only origins.
  for (value in c(NA_character_, "2016-03-02")) {
    imported <- datapond:::transfer_insert_row(conn, "entry_tbl",
      data.frame(uuid = uuid::UUIDgenerate(), title = "Imported", metadata_creation_date = value))
    expect_identical(DBI::dbGetQuery(conn, "SELECT metadata_creation_date FROM entry_tbl WHERE id=?", params = list(imported))[[1]], value)
  }
})

test_that("review timestamps establish creation only for explicitly identified creation events", {
  f <- nonspatial_fixture("nonGeographicDataset")
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date=NULL")
  DBI::dbExecute(conn, "INSERT INTO metadata_review_tbl(dataset_id,review_timestamp,update_type)
    VALUES (1,'2026-10-02T10:00:00Z','routine_review')")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "not evidence of creation")
  DBI::dbExecute(conn, "INSERT INTO metadata_review_tbl(dataset_id,review_timestamp,update_type)
    VALUES (1,'2026-09-02T09:00:00+02:00','new_metadata_record; contact_update')")
  d <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_identical(core_xpath_text(d, "/mdb:MD_Metadata/mdb:dateInfo//cit:date/*"),
    c("2026-09-02T09:00:00+02:00", "2026-10-02T10:00:00Z"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date='2016-03-02'")
  d <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_identical(core_xpath_text(d, "/mdb:MD_Metadata/mdb:dateInfo//gco:Date"), "2016-03-02")
})

test_that("date precision policy preserves pinned failures and never waives other errors", {
  core_validation_engine()
  f <- nonspatial_fixture("dataset", "Survey region")
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  for (date in c("2016", "2016-03", "2016-03-02")) {
    DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date=?", params = list(date))
    a <- datapond:::assess_iso_metadata_export(1, f$path)
    expect_identical(a$validation$status, "fail")
    expect_true(all(a$validation$issues$rule == "$hasAtLeastOneCreationDate"))
    expect_identical(a$validation$datapond_profile$status, "pass")
    expect_identical(a$validation$datapond_profile$version, "1.0.0")
    expect_identical(a$validation$datapond_profile$version,
      datapond:::load_current_iso_crosswalk()$validation_policy$version)
    expect_equal(nrow(a$validation$datapond_profile$accepted_deviations), 2L)
    expect_identical(core_xpath_text(a$document, "/mdb:MD_Metadata/mdb:dateInfo//gco:Date"), date)
  }
  good <- as.character(a$document)
  for (change in c("invalid", "missing", "unrelated")) {
    d <- xml2::read_xml(good); ns <- xml2::xml_ns(d)
    if (change == "invalid") xml2::xml_set_text(xml2::xml_find_first(d, "/mdb:MD_Metadata/mdb:dateInfo//gco:Date", ns), "2016-02-31")
    if (change == "missing") xml2::xml_set_attr(xml2::xml_find_first(d, "/mdb:MD_Metadata/mdb:dateInfo//cit:CI_DateTypeCode", ns), "codeListValue", "revision")
    if (change == "unrelated") xml2::xml_remove(xml2::xml_find_all(d, "//mri:topicCategory", ns))
    r <- datapond:::validate_iso_metadata_xml(d)
    expect_false(r$valid)
    expect_false(r$datapond_profile$valid)
    expect_gt(nrow(r$datapond_profile$issues), 0L)
  }
  r <- datapond:::validate_iso_metadata_xml(a$document, python = "")
  expect_identical(r$datapond_profile$status, "unavailable")
  bad_catalogue <- xml2::read_xml(as.character(a$feature_catalogue))
  xml2::xml_set_text(xml2::xml_find_first(bad_catalogue, "./cat:name/*", xml2::xml_ns(bad_catalogue)), "Different title")
  r <- datapond:::validate_iso_content_documents(a$document, bad_catalogue, a$feature_catalogue_filename)
  expect_false(r$datapond_profile$valid)
  expect_true(any(r$datapond_profile$issues$stage == "catalogue-link"))
})

test_that("profile writing is explicit and carries both validation outcomes", {
  core_validation_engine()
  f <- nonspatial_fixture("nonGeographicDataset")
  iso_package_fixture_files(f)
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date='2016-03-02'")
  output <- tempfile(); dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  expect_error(datapond:::write_iso_wave3_documents(1, f$path, output), "ISO export was not written")
  expect_length(list.files(output), 0L)
  paths <- datapond:::write_iso_wave3_documents(1, f$path, output, validation_policy = "datapond")
  r <- jsonlite::read_json(paths[["validation"]], simplifyVector = TRUE)
  expect_identical(r$selected_policy, "datapond")
  expect_identical(r$validation$status, "fail")
  expect_identical(r$validation$datapond_profile$status, "pass")
  before <- tools::md5sum(paths)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code=NULL")
  expect_error(datapond:::write_iso_wave3_documents(1, f$path, output, "datapond"), "ISO export was not written")
  expect_identical(tools::md5sum(paths), before)
})

test_that("profile editor stages and clears area and historical date without hydration writes", {
  local_trusted_access_fixture()

  f <- nonspatial_fixture("dataset", "Stored region")
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  state <- shiny::reactiveValues(data_pond = f$path, refresh_person = 0L, refresh_org = 0L)
  shiny::testServer(datapond:::mod_iso_profile_server, args = list(state = state), {
    session$flushReact()
    api <- session$returned
    expect_false("metadata_creation_date" %in% names(api$profile()))
    api$load(1)
    session$setInputs(geographic_extent_name = "", metadata_creation_date = "")
    expect_identical(api$profile()$geographic_extent_name, "")
    expect_identical(api$profile()$metadata_creation_date, "")
    session$setInputs(metadata_creation_date = "2016-02-31")
    expect_false(api$validate()$valid)
    session$setInputs(metadata_creation_date = "2016-03-02")
    expect_true(api$validate()$valid)
    expect_identical(datapond:::get_dataset_record(1, f$path)$geographic_extent_name, "Stored region")
  })
})

test_that("profile persistence round-trips, clears and preserves optional historical values", {
  f <- nonspatial_fixture("dataset", "Stored region")
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  record <- datapond:::get_dataset_record(1, f$path)
  profile <- record[c("metadata_uuid", "metadata_resource_scope_code",
    "metadata_language_code", "metadata_character_encoding")]
  save <- function(values) datapond:::save_iso_profile_sections(1, values, "environment", data.frame(),
    datapond:::get_metadata_responsibilities(1, f$path), "2026-10-01", f$path)
  save(c(profile, list(geographic_extent_name = "New region", metadata_creation_date = "2016-03")))
  r <- datapond:::get_dataset_record(1, f$path)
  expect_identical(r$geographic_extent_name, "New region")
  expect_identical(r$metadata_creation_date, "2016-03")
  save(profile)
  expect_identical(datapond:::get_dataset_record(1, f$path)$metadata_creation_date, "2016-03")
  expect_error(save(c(profile, list(geographic_extent_name = "Do not write", metadata_creation_date = "2016-02-31"))),
    "not a valid ISO")
  expect_identical(datapond:::get_dataset_record(1, f$path)$geographic_extent_name, "New region")
  save(c(profile, list(geographic_extent_name = "", metadata_creation_date = "")))
  r <- datapond:::get_dataset_record(1, f$path)
  expect_true(is.na(r$geographic_extent_name))
  expect_true(is.na(r$metadata_creation_date))
})
