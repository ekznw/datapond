wave2_fixture <- function() {
  pond <- tempfile("iso-wave2-")
  dir.create(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  datapond:::ensure_columns(conn, "entry_tbl", c("abstract", "metadata_creation_date"))
  DBI::dbExecute(conn, "
    INSERT INTO organisation_tbl (organisation_name_full, organisation_code)
    VALUES ('Wave 2 Office', 'W2')
  ")
  organisation_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "
    INSERT INTO persons_tbl (name_first, name_last, email, organisation_id)
    VALUES ('Wanda', 'Curator', 'wanda@example.test', ?)
  ", params = list(organisation_id))
  person_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "
    INSERT INTO entry_tbl (
      uuid, title, citation_date, data_distribution_url,
      temporal_coverage_start_date, temporal_coverage_end_date,
      embargo_status, embargo_reason, embargo_end_date
    ) VALUES (
      'wave2-resource', 'Wave 2 fixture', '2025-01-01', 'https://example.test/catalogue',
      '2020-01-01', '2020-12-31', 'Yes', 'Pending release', '2027-01-31'
    )
  ")
  dataset_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = 'Wave 2 fixture abstract.',
    metadata_creation_date = '2024-12-01 10:00:00' WHERE id = ?", params = list(dataset_id))
  DBI::dbDisconnect(conn)
  datapond:::init_db(pond)
  list(path = pond, dataset_id = as.integer(dataset_id), person_id = as.integer(person_id),
       organisation_id = as.integer(organisation_id))
}

test_that("Wave 2 migration backfills deterministic compatibility values idempotently", {
  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::init_db(fixture$path)

  online <- datapond:::get_dataset_online_resources(fixture$dataset_id, fixture$path)
  temporal <- datapond:::get_dataset_temporal_extents(fixture$dataset_id, fixture$path)
  rights <- datapond:::get_dataset_release_rights(fixture$dataset_id, fixture$path)
  quality <- datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)
  expect_equal(nrow(online), 1L)
  expect_identical(online$resource_url, "https://example.test/catalogue")
  expect_identical(online$is_primary, 1L)
  expect_equal(nrow(temporal), 1L)
  expect_identical(temporal$start_value, "2020-01-01")
  expect_identical(rights$embargo_reason, "Pending release")
  expect_equal(nrow(quality), 0L)

  conn <- datapond:::get_conn(fixture$path)
  compatibility <- DBI::dbGetQuery(
    conn, "SELECT * FROM dataset_temporal_coverage_compat_vw WHERE dataset_id = ?",
    params = list(fixture$dataset_id)
  )
  DBI::dbDisconnect(conn)
  expect_identical(compatibility$temporal_coverage_end_date, "2020-12-31")
})

test_that("Wave 2 structured records support create update reload and delete", {
  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  user <- fixture$person_id
  datapond:::replace_dataset_alternate_titles(
    fixture$dataset_id, c("Short title", "Former title"), fixture$path, user
  )
  datapond:::replace_dataset_online_resources(
    fixture$dataset_id,
    data.frame(
      resource_url = c("https://example.test/data", "https://example.test/info"),
      resource_name = c("Download", "Landing page"), resource_description = c("Package", "Record"),
      protocol = c("HTTPS", "HTTPS"), function_code = c("download", "information"),
      media_type = c("application/zip", "text/html"), is_primary = c(1L, 0L)
    ), fixture$path, user
  )
  datapond:::replace_dataset_distribution_formats(
    fixture$dataset_id,
    data.frame(specification_title = "GeoPackage", format_version = "1.3",
               media_type = "application/geopackage+sqlite3", format_uri = "https://www.geopackage.org/"),
    fixture$path, user
  )
  datapond:::replace_dataset_quality_results(
    fixture$dataset_id,
    data.frame(
      scope_code = "dataset", data_file_id = NA_integer_, attribute_id = NA_integer_,
      measure_identifier = "DQ-COMP-01", measure_name = "Completeness test",
      measure_type_code = "completeness", evaluation_method = "Compared against signed field sheets",
      evaluation_date = "2025-06-30", result_type_code = "quantitative",
      result_value = "98.5", result_unit = "percent", conformance_specification = NA_character_,
      explanation = "Two expected records were absent", pass = NA_integer_
    ), fixture$path, user
  )
  datapond:::replace_dataset_temporal_extents(
    fixture$dataset_id,
    data.frame(
      extent_label = c("Survey one", "Ongoing monitoring"),
      start_value = c("2020-01-01", "2024"), end_value = c("2020-12-31", NA),
      start_precision = c("day", "year"), end_precision = c("day", "day"),
      start_indeterminate = c(NA, NA), end_indeterminate = c(NA, "now"),
      description = c("Baseline", "Current programme")
    ), fixture$path, user
  )

  expect_identical(datapond:::get_dataset_alternate_titles(fixture$dataset_id, fixture$path)$alternate_title,
                   c("Short title", "Former title"))
  expect_equal(nrow(datapond:::get_dataset_online_resources(fixture$dataset_id, fixture$path)), 2L)
  expect_identical(datapond:::get_dataset_distribution_formats(fixture$dataset_id, fixture$path)$specification_title, "GeoPackage")
  expect_identical(datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)$measure_identifier, "DQ-COMP-01")
  expect_equal(nrow(datapond:::get_dataset_temporal_extents(fixture$dataset_id, fixture$path)), 2L)

  datapond:::replace_dataset_alternate_titles(fixture$dataset_id, "Updated title", fixture$path, user)
  datapond:::replace_dataset_quality_results(fixture$dataset_id, data.frame(), fixture$path, user)
  expect_identical(datapond:::get_dataset_alternate_titles(fixture$dataset_id, fixture$path)$alternate_title, "Updated title")
  expect_equal(nrow(datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)), 0L)

  datapond:::replace_dataset_alternate_titles(fixture$dataset_id, character(), fixture$path, user)
  datapond:::replace_dataset_online_resources(fixture$dataset_id, data.frame(), fixture$path, user)
  datapond:::replace_dataset_distribution_formats(fixture$dataset_id, data.frame(), fixture$path, user)
  datapond:::replace_dataset_temporal_extents(fixture$dataset_id, data.frame(), fixture$path, user)
  expect_equal(nrow(datapond:::get_dataset_alternate_titles(fixture$dataset_id, fixture$path)), 0L)
  expect_equal(nrow(datapond:::get_dataset_online_resources(fixture$dataset_id, fixture$path)), 0L)
  expect_equal(nrow(datapond:::get_dataset_distribution_formats(fixture$dataset_id, fixture$path)), 0L)
  expect_equal(nrow(datapond:::get_dataset_temporal_extents(fixture$dataset_id, fixture$path)), 0L)
})

test_that("rights are authoritative in licensing and dual-written to legacy entry fields", {
  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::save_dataset_release_rights(
    fixture$dataset_id,
    list(
      embargo_status = "Yes", embargo_reason = "Article review",
      embargo_start_date = "2026-01-01", embargo_end_date = "2026-09-30",
      release_addressee = "Approved partners", release_statement = "Release after acceptance.",
      dissemination_constraint_code = "approvedRecipients"
    ), fixture$path, fixture$person_id
  )
  rights <- datapond:::get_dataset_release_rights(fixture$dataset_id, fixture$path)
  conn <- datapond:::get_conn(fixture$path)
  entry <- DBI::dbGetQuery(conn, "SELECT embargo_status, embargo_reason, embargo_end_date FROM entry_tbl WHERE id = ?", params = list(fixture$dataset_id))
  DBI::dbDisconnect(conn)
  expect_identical(rights$embargo_start_date, "2026-01-01")
  expect_identical(rights$release_statement, "Release after acceptance.")
  expect_identical(entry$embargo_reason, "Article review")
  expect_identical(entry$embargo_end_date, "2026-09-30")
})

test_that("Wave 2 validates scale and prevents implicit quality promotion", {
  expect_identical(datapond:::validate_equivalent_scale_denominator("25000"), 25000L)
  expect_error(datapond:::validate_equivalent_scale_denominator(0), "positive")
  expect_error(datapond:::validate_equivalent_scale_denominator(12.5), "positive")
  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  expect_equal(nrow(datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)), 0L)
  expect_error(
    datapond:::replace_dataset_quality_results(
      fixture$dataset_id,
      data.frame(scope_code = "dataset", measure_identifier = "", measure_type_code = "completeness",
                 evaluation_method = "", result_type_code = "quantitative", result_value = "1"),
      fixture$path
    ),
    "explicit measure identifier"
  )
})

test_that("Wave 2 completion status is conditional and actionable", {
  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  before <- as.data.frame(datapond:::get_dataset_completion_status(
    fixture$dataset_id, fixture$path, NULL
  ))
  rights_before <- before[before$section == "Rights and releasability", ]
  expect_identical(rights_before$status, "partial")
  expect_match(rights_before$missing_fields, "Embargo start date", fixed = TRUE)
  expect_identical(
    before$status[before$section == "ISO data quality"], "not_started"
  )

  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET equivalent_scale_denominator = 10000 WHERE id = ?",
                 params = list(fixture$dataset_id))
  DBI::dbDisconnect(conn)
  datapond:::save_dataset_release_rights(
    fixture$dataset_id,
    list(embargo_status = "Yes", embargo_reason = "Review", embargo_start_date = "2026-01-01",
         embargo_end_date = "2027-01-31", release_addressee = "Partners",
         release_statement = "Release after review.", dissemination_constraint_code = "approvedRecipients"),
    fixture$path
  )
  after <- as.data.frame(datapond:::get_dataset_completion_status(
    fixture$dataset_id, fixture$path, NULL
  ))
  expect_identical(after$status[after$section == "Equivalent scale"], "complete")
  expect_identical(after$status[after$section == "Temporal extents"], "complete")
  expect_identical(after$status[after$section == "Rights and releasability"], "complete")
})

test_that("Wave 2 XML consumes structured additions", {
  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET equivalent_scale_denominator = 25000 WHERE id = ?", params = list(fixture$dataset_id))
  DBI::dbDisconnect(conn)
  datapond:::save_iso_profile_sections(
    fixture$dataset_id,
    profile = list(metadata_uuid = "22222222-3333-4444-8555-666666666666",
                   metadata_resource_scope_code = "service", metadata_language_code = "eng",
                   metadata_character_encoding = "utf8"),
    topic_categories = character(), citation_dates = data.frame(),
    responsibilities = data.frame(person_id = fixture$person_id, organisation_id = fixture$organisation_id,
                                  role_code = "pointOfContact", is_primary = 1L),
    primary_publication_date = "2025-01-01", base_path = fixture$path,
    user_person_id = fixture$person_id
  )
  datapond:::replace_dataset_alternate_titles(fixture$dataset_id, "Service short title", fixture$path)
  datapond:::replace_dataset_distribution_formats(
    fixture$dataset_id,
    data.frame(specification_title = "GeoPackage", format_version = "1.3", media_type = "application/geopackage+sqlite3", format_uri = NA_character_),
    fixture$path
  )
  datapond:::replace_dataset_quality_results(
    fixture$dataset_id,
    data.frame(scope_code = "dataset", data_file_id = NA_integer_, attribute_id = NA_integer_,
      measure_identifier = "DQ-01", measure_name = "Completeness", measure_type_code = "completeness",
      quality_element_code = "DQ_CompletenessOmission",
      evaluation_method = "Compared to register", evaluation_date = "2025-01-02",
      result_type_code = "quantitative", result_value = "99", result_unit = "percent",
      conformance_specification = NA_character_, explanation = NA_character_, pass = NA_integer_), fixture$path
  )
  datapond:::save_dataset_release_rights(
    fixture$dataset_id,
    list(embargo_status = "Yes", embargo_reason = "Review", embargo_start_date = "2025-01-01",
         embargo_end_date = "2027-01-31", release_addressee = "Partners",
         release_statement = "Release to partners.", dissemination_constraint_code = "approvedRecipients"),
    fixture$path
  )
  xml <- datapond:::build_iso_wave2_metadata_xml(fixture$dataset_id, fixture$path)
  text <- as.character(xml)
  expect_match(text, "Service short title", fixed = TRUE)
  expect_match(text, "25000", fixed = TRUE)
  expect_match(text, "GeoPackage", fixed = TRUE)
  expect_match(text, "DQ-01", fixed = TRUE)
  expect_match(text, "Release to partners.", fixed = TRUE)
  expect_match(text, paste("DataPond crosswalk", datapond:::current_iso_crosswalk_version()), fixed = TRUE)
})

test_that("Wave 2 UI exposes the structured editors", {
  expect_match(as.character(datapond:::mod_wave2_alternate_titles_ui("w2")), "Alternate titles", fixed = TRUE)
  expect_match(as.character(datapond:::mod_wave2_temporal_extents_ui("w2")), "Advanced temporal options", fixed = TRUE)
  expect_match(as.character(datapond:::mod_wave2_distribution_ui("w2")), "Online access", fixed = TRUE)
  expect_match(as.character(datapond:::mod_wave2_quality_ui("w2")), "Record tests you have actually performed", fixed = TRUE)
  access_html <- as.character(datapond:::mod_data_access_ui("access"))
  expect_match(access_html, "Embargo start date", fixed = TRUE)
  expect_match(access_html, "Release statement", fixed = TRUE)
})

test_that("normal online-resource details receive safe defaults", {
  defaults <- datapond:::wave2_online_resource_defaults(
    "https://example.test/data/results.csv",
    "download"
  )
  expect_identical(defaults$protocol, "WWW:DOWNLOAD-1.0-http--download")
  expect_identical(defaults$media_type, "text/csv")

  explicit <- datapond:::wave2_online_resource_defaults(
    "https://example.test/data",
    "information",
    "CUSTOM",
    "application/example"
  )
  expect_identical(explicit$protocol, "CUSTOM")
  expect_identical(explicit$media_type, "application/example")
})

test_that("Wave 2 server hydrates staged editors without database writes", {
  local_trusted_access_fixture()

  fixture <- wave2_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  state <- shiny::reactiveValues(
    data_pond = fixture$path, refresh_datasets = 0L,
    current_dataset_id = fixture$dataset_id
  )
  shiny::testServer(
    datapond:::mod_iso_wave2_server,
    args = list(state = state),
    {
      session$flushReact()
      load(as.integer(fixture$dataset_id))
      session$flushReact()
      expect_equal(nrow(online_resources()), 1L)
      expect_equal(nrow(temporal()), 1L)
      expect_equal(nrow(quality()), 0L)
    }
  )
})
