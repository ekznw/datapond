test_that("core XML preserves database values and validates with structured and document lineage", {
  core_validation_engine()
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  source <- datapond:::save_dataset_lineage_source(fixture$dataset_id, list(
    title = "Field observations", description = "GPS observations from the survey.",
    identifier = "OBS-1", identifier_code_space = "Survey register",
    citation_date = "2026-08-31", citation_date_type_code = "publication",
    source_uri = "https://example.test/survey", equivalent_scale_denominator = 10000
  ), fixture$path)
  step <- datapond:::save_dataset_lineage_process_step(fixture$dataset_id, list(
    description = "Checked and combined observations.", rationale = "Remove duplicate records.",
    step_datetime = "2026-09-01 09:15:20+02:00", tool_software_reference = "R",
    script_reference = "scripts/check.R", output_description = "Checked observations."
  ), fixture$path)
  datapond:::set_dataset_lineage_step_sources(fixture$dataset_id, step, source, fixture$path)
  result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "core")
  expect_identical(result$validation$status, "pass", info = paste(result$validation$issues$message, collapse = "\n"))
  expect_identical(result$export_scope, "core")
  expect_match(result$validation$coverage, "Excludes specialist", fixed = TRUE)
  xml <- result$document
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  row <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(fixture$dataset_id))
  expect_identical(core_xpath_text(xml, "/mdb:MD_Metadata/mdb:metadataIdentifier//mcc:code/gco:CharacterString"), row$metadata_uuid)
  expect_identical(core_xpath_text(xml, "//mri:citation//cit:identifier//mcc:code/gco:CharacterString"), c(row$uuid, row$citation_key))
  expect_identical(core_xpath_text(xml, "//mri:citation//cit:title/gco:CharacterString"), row$title)
  expect_identical(core_xpath_text(xml, "//mri:citation//cit:edition/gco:CharacterString"), row$version)
  for (field in c("abstract", "purpose")) expect_identical(
    core_xpath_text(xml, paste0("//mri:", field, "/gco:CharacterString")), row[[field]])
  expect_identical(core_xpath_text(xml, "//mri:credit/gco:CharacterString"), row$dataset_credit)
  expect_identical(core_xpath_text(xml, "/mdb:MD_Metadata/mdb:dateInfo//gco:DateTime"), "2026-09-30T08:30:00")
  expect_identical(core_xpath_text(xml, "//gex:EX_GeographicBoundingBox/*/gco:Decimal"), c("29", "31", "-30", "-28"))
  expect_identical(core_xpath_text(xml, "//mrs:referenceSystemIdentifier//mcc:code/gco:CharacterString"), "4326")
  expect_identical(core_xpath_text(xml, "//mrs:referenceSystemIdentifier//mcc:codeSpace/gco:CharacterString"), "EPSG")
  ns <- xml2::xml_ns(xml)
  expect_identical(core_xpath_text(xml, "//mrl:stepDateTime/gml:TimeInstant/gml:timePosition"), "2026-09-01T09:15:20+02:00")
  expect_identical(xml2::xml_attr(xml2::xml_find_all(xml, "//mrl:LI_ProcessStep/mrl:source", ns), "xlink:href", ns = ns),
                   paste0("#datapond-lineage-source-", source))
  expect_identical(core_xpath_text(xml, "//mrl:additionalDocumentation//cit:linkage/gco:CharacterString"), "documents/survey-protocol.pdf")
  # Reordering must not duplicate source/step records or invalidate references.
  expect_length(xml2::xml_find_all(xml, "//mrl:LI_Source", ns), 1L)
  expect_length(xml2::xml_find_all(xml, "//mrl:LI_ProcessStep", ns), 1L)
  again <- datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path)
  expect_identical(as.character(again), as.character(xml))
})

test_that("partial resource dates and date-only lineage retain their recorded precision", {
  core_validation_engine()
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  datapond:::replace_dataset_citation_dates(fixture$dataset_id, data.frame(
    date_value = c("2020", "2026-09"), date_type_code = c("creation", "revision"),
    date_precision = c("year", "month"), date_note = NA_character_), fixture$path,
    primary_publication_date = "2026-10-01")
  datapond:::save_dataset_lineage_process_step(fixture$dataset_id, list(
    description = "Checked the source register.", step_datetime = "2026-09-01"), fixture$path)
  result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "core")
  expect_true(result$validation$valid, info = paste(result$validation$issues$message, collapse = "\n"))
  expect_identical(core_xpath_text(result$document, "//mri:citation//cit:date/gco:Date"), c("2026-10-01", "2020", "2026-09"))
  expect_identical(core_xpath_text(result$document, "//mrl:stepDateTime//gml:timePosition"), "2026-09-01")
  expect_false(grepl("2020-01-01|2026-09-01T00:00", as.character(result$document)))
})

test_that("metadata dates follow stored creation and review history without inventing timestamps", {
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date = NULL")
  expect_error(datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path), "Missing metadata creation date")
  # Insert in reverse chronological order; both offsets denote actual instants.
  for (value in c("2026-10-02T10:00:00+02:00", "2026-10-02T09:00:00+04:00"))
    DBI::dbExecute(conn, "INSERT INTO metadata_review_tbl(dataset_id, review_timestamp, update_type) VALUES (?, ?, 'new_metadata_record')", params = list(fixture$dataset_id, value))
  xml <- datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path)
  expect_identical(core_xpath_text(xml, "/mdb:MD_Metadata/mdb:dateInfo//gco:DateTime"),
                   c("2026-10-02T09:00:00+04:00", "2026-10-02T10:00:00+02:00"))
  expect_identical(core_xpath_text(xml, "/mdb:MD_Metadata/mdb:dateInfo//cit:CI_DateTypeCode"), c("creation", "lastUpdate"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date = '2026-09-01T11:12:13.5Z'")
  xml <- datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path)
  expect_identical(core_xpath_text(xml, "/mdb:MD_Metadata/mdb:dateInfo//gco:DateTime")[1], "2026-09-01T11:12:13.5Z")
  # An explicit calendar date wins over the fallback. It must remain a date.
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date = '2026-09-01'")
  xml <- datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path)
  expect_identical(core_xpath_text(xml, "/mdb:MD_Metadata/mdb:dateInfo//gco:Date"), "2026-09-01")
  core_validation_engine()
  report <- datapond:::validate_iso_metadata_xml(xml)
  expect_true(report$checks$valid[report$checks$stage == "xsd"])
  # The pinned creation-date rule requires DateTime even though XSD accepts
  # Date. Keep this limitation visible rather than manufacture midnight.
  expect_identical(report$status, "fail")
  expect_true(any(report$issues$rule == "$hasAtLeastOneCreationDate"))
})

test_that("native CRS identifiers are deduplicated with inclusion and active flags respected", {
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  for (i in seq_along(c("EPSG:4326", "EPSG:32736", "EPSG:3857"))) {
    DBI::dbExecute(conn, "INSERT INTO data_file_spatial_metadata_tbl
      (dataset_id, data_file_rel_path_full, crs_auth_code, crs_name, crs_status, include_in_dataset_bbox)
      VALUES (?, ?, ?, ?, 'known', ?)", params = list(fixture$dataset_id, paste0("data/source", i, ".gpkg"),
        c("EPSG:4326", "EPSG:32736", "EPSG:3857")[i], c("WGS 84", "WGS 84 / UTM 36S", "Excluded CRS")[i],
        if (i == 3L) 0L else 1L))
  }
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl(dataset_id, data_file_rel_path_full, active)
    VALUES (?, 'data/inactive.gpkg', 0)", params = list(fixture$dataset_id))
  DBI::dbExecute(conn, "INSERT INTO data_file_spatial_metadata_tbl
    (dataset_id, data_file_rel_path_full, crs_auth_code, crs_status, include_in_dataset_bbox)
    VALUES (?, 'data/inactive.gpkg', 'EPSG:3857', 'known', 1)", params = list(fixture$dataset_id))
  DBI::dbExecute(conn, "UPDATE dataset_spatial_summary_tbl SET xmin = 29.125")
  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)
  xml <- datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path)
  expect_identical(core_xpath_text(xml, "//mrs:referenceSystemIdentifier//mcc:code/gco:CharacterString"), c("32736", "4326"))
  expect_setequal(core_xpath_text(xml, "//mrs:referenceSystemIdentifier//mcc:description/gco:CharacterString"), c("WGS 84", "WGS 84 / UTM 36S"))
  expect_identical(core_xpath_text(xml, "//gex:westBoundLongitude/gco:Decimal"), "29.125")
})

test_that("core export rejects missing or inconsistent values before creating misleading XML", {
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = ''")
  expect_error(datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path), "Missing dataset abstract")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = 'Restored abstract'")
  DBI::dbExecute(conn, "UPDATE dataset_spatial_summary_tbl SET ymin = 999")
  expect_error(datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path), "valid WGS84 bounds")
  DBI::dbExecute(conn, "UPDATE dataset_spatial_summary_tbl SET ymin = -30")
  DBI::dbExecute(conn, "UPDATE dataset_manual_spatial_extent_tbl SET crs_auth_code = NULL, crs_input = 'Local grid'")
  expect_error(datapond:::build_iso_core_metadata_xml(fixture$dataset_id, fixture$path), "structured CRS authority/code")
  for (date in c("2026-02-30", "2026-13", "2026-10-01T10:30", "2026-10-01T25:30:00", "2026-10-01T10:30:00+15:00"))
    expect_error(datapond:::iso_export_date(date), "not a valid ISO date/time")
})

test_that("the corrected source rule detects a title-only lineage source", {
  core_validation_engine()
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  datapond:::save_dataset_lineage_source(fixture$dataset_id, list(title = "A source needing description"), fixture$path)
  result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "core")
  expect_false(result$validation$valid)
  expect_true(result$validation$checks$valid[result$validation$checks$stage == "xsd"])
  expect_true(any(result$validation$issues$rule == "$hasDescriptionOrScope"))
  expect_true(any(grepl("LI_Source constraint", result$validation$adaptations$change, fixed = TRUE)))
})

test_that("subset assessment leaves ambiguous quality types visible in the complete candidate", {
  core_validation_engine()
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  datapond:::replace_dataset_distribution_formats(fixture$dataset_id, data.frame(
    specification_title = "GeoPackage", format_version = "1.3", media_type = "application/geopackage+sqlite3"), fixture$path)
  datapond:::replace_dataset_quality_results(fixture$dataset_id, data.frame(
    scope_code = "dataset", measure_identifier = "DQ-01", measure_name = "Completeness",
    measure_type_code = "completeness", evaluation_method = "Compared to register",
    result_type_code = "descriptive", explanation = "Review is pending."), fixture$path)
  distribution <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "distribution")
  expect_true(distribution$validation$valid)
  core <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "core")
  complete <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path)
  expect_true(core$validation$valid)
  expect_identical(core$export_scope, "core")
  expect_identical(complete$export_scope, "all")
  expect_identical(complete$validation$status, "not-built")
  expect_match(complete$validation$issues$message, "choose a specific test")
  expect_match(as.character(distribution$document), "GeoPackage", fixed = TRUE)
})
