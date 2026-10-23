legacy_patch_fixture <- function() {
  pond <- tempfile("legacy-patch-")
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn))
  datapond:::ensure_columns(conn, "entry_tbl", c("keyword_topic", "keyword_object", "software_desc"))
  DBI::dbExecute(conn, "INSERT INTO organisation_tbl
    (organisation_code, organisation_name_full, organisation_department, organisation_address)
    VALUES ('OLD', 'Older organisation', 'Research', 'Original free-text address')")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl
    (uuid, title, citation_date, keyword_object, keyword_topic, software_desc,
     data_distribution_url, temporal_coverage_start_date, temporal_coverage_end_date,
     embargo_status, embargo_reason)
    VALUES ('legacy-patch-record', 'Older dataset', '2020-03', 'Forest', 'forest; Birds',
     'Original software description', 'https://example.test/older', '2010', '2012', 'Yes', 'Original embargo')")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl
    (dataset_id, source_description, process_description, lineage_statement, lineage_notes)
    VALUES (1, 'Original source', 'Original process', 'Original narrative', 'Original notes')")
  DBI::dbExecute(conn, "INSERT INTO dataset_license_tbl
    (dataset_id, license_identifier, required_attribution) VALUES (1, 'CC-BY-4.0', 0)")
  pond
}

test_that("startup migrates older records and preserves necessary source information", {
  pond <- legacy_patch_fixture()
  on.exit(unlink(pond, recursive = TRUE))
  expect_true(datapond:::init_db(pond))
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  expect_true(all(datapond:::legacy_patch_v1_status(conn)$pending == 0L))
  entry <- DBI::dbReadTable(conn, "entry_tbl")
  expect_identical(entry$keyword_object, "Forest; Birds")
  expect_true(is.na(entry$keyword_topic))
  expect_identical(entry$software_desc, "Original software description")
  expect_identical(entry$citation_date, "2020-03")
  expect_identical(DBI::dbReadTable(conn, "dataset_citation_date_tbl")$date_precision, "month")
  expect_identical(DBI::dbReadTable(conn, "dataset_online_resource_tbl")$resource_url, "https://example.test/older")
  expect_identical(DBI::dbReadTable(conn, "dataset_temporal_extent_tbl")$start_precision, "year")
  expect_identical(DBI::dbReadTable(conn, "party_address_tbl")$delivery_point, "Original free-text address")
  expect_identical(DBI::dbReadTable(conn, "organisation_tbl")$organisation_address, "Original free-text address")
  expect_identical(DBI::dbReadTable(conn, "dataset_lineage_source_tbl")$description, "Original source")
  expect_identical(DBI::dbReadTable(conn, "dataset_lineage_process_step_tbl")$description, "Original process")
  lineage <- DBI::dbReadTable(conn, "dataset_lineage_tbl")
  expect_identical(lineage$source_description, "Original source")
  expect_match(lineage$lineage_statement, "Original notes", fixed = TRUE)
  rights <- DBI::dbReadTable(conn, "dataset_license_tbl")
  expect_identical(rights$rights_basis_code, "review_required")
  expect_identical(rights$required_attribution, 0L) # explicit saved choice, not a template reset
  expect_identical(rights$embargo_reason, "Original embargo")
})

test_that("completed records skip conversion code and cleared values stay cleared", {
  pond <- legacy_patch_fixture()
  on.exit(unlink(pond, recursive = TRUE))
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  tables <- c("dataset_citation_date_tbl", "dataset_online_resource_tbl", "dataset_temporal_extent_tbl",
    "party_address_tbl", "dataset_lineage_source_tbl", "dataset_lineage_process_step_tbl")
  for (table in tables) DBI::dbExecute(conn, paste("DELETE FROM", table))
  DBI::dbExecute(conn, "UPDATE dataset_license_tbl SET rights_basis_code = NULL, access_level_code = NULL,
    embargo_status = NULL, embargo_reason = NULL, required_attribution = 0")
  DBI::dbExecute(conn, "UPDATE organisation_tbl SET organisational_unit_name = NULL")
  # Completed records no longer depend on the historical converter bodies.
  callbacks <- paste0("legacy_patch_v1_", names(datapond:::legacy_patch_v1_components()))
  replacements <- stats::setNames(rep(list(function(...) stop("Old converter reran")), length(callbacks)), callbacks)
  testthat::local_mocked_bindings(!!!replacements, .package = "datapond")
  expect_true(datapond:::init_db(pond))
  for (table in tables) expect_equal(nrow(DBI::dbReadTable(conn, table)), 0L)
  rights <- DBI::dbReadTable(conn, "dataset_license_tbl")
  expect_true(is.na(rights$rights_basis_code))
  expect_true(is.na(rights$embargo_reason))
  expect_true(is.na(DBI::dbReadTable(conn, "organisation_tbl")$organisational_unit_name))
  compatibility <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_temporal_coverage_compat_vw")
  expect_true(is.na(compatibility$temporal_coverage_start_date))
  # Retained snapshots must still exist even after structured values are cleared.
  expect_identical(DBI::dbReadTable(conn, "entry_tbl")$temporal_coverage_start_date, "2010")
})

test_that("a failed startup patch rolls back every conversion and is retryable", {
  pond <- legacy_patch_fixture()
  on.exit(unlink(pond, recursive = TRUE))
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "CREATE TRIGGER fail_patch BEFORE UPDATE ON dataset_lineage_tbl
    BEGIN SELECT RAISE(ABORT, 'Injected patch failure'); END")
  expect_error(datapond:::init_db(pond), "Injected patch failure")
  for (table in c("dataset_citation_date_tbl", "dataset_online_resource_tbl", "party_address_tbl")) {
    expect_equal(nrow(DBI::dbReadTable(conn, table)), 0L)
  }
  expect_true(is.na(DBI::dbReadTable(conn, "entry_tbl")$legacy_citation_v1_migrated_at))
  expect_true(is.na(DBI::dbReadTable(conn, "organisation_tbl")$legacy_party_v1_migrated_at))
  expect_identical(DBI::dbReadTable(conn, "entry_tbl")$keyword_topic, "forest; Birds")
  expect_true(is.na(DBI::dbReadTable(conn, "dataset_license_tbl")$rights_basis_code))
  DBI::dbExecute(conn, "DROP TRIGGER fail_patch")
  expect_true(datapond:::init_db(pond))
  expect_true(all(datapond:::legacy_patch_v1_status(conn)$pending == 0L))
})

test_that("older records arriving after a completed startup are still migrated", {
  pond <- legacy_patch_fixture()
  on.exit(unlink(pond, recursive = TRUE))
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  first <- DBI::dbReadTable(conn, "dataset_lineage_tbl")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (uuid, title, citation_date)
    VALUES ('late-legacy', 'Older colleague record', '2001')")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl (dataset_id, source_description)
    VALUES (2, 'Late source')")
  datapond:::init_db(pond)
  expect_equal(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM dataset_citation_date_tbl WHERE dataset_id = 2")$n, 1L)
  expect_identical(DBI::dbGetQuery(conn, "SELECT * FROM dataset_lineage_tbl WHERE dataset_id = 1"), first)
  expect_identical(DBI::dbGetQuery(conn, "SELECT description FROM dataset_lineage_source_tbl WHERE dataset_id = 2")$description, "Late source")
})

test_that("existing structured metadata wins over old snapshots on first upgrade", {
  pond <- legacy_patch_fixture()
  on.exit(unlink(pond, recursive = TRUE))
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "INSERT INTO dataset_citation_date_tbl
    (dataset_id, date_value, date_type_code, date_precision) VALUES (1, '2025', 'publication', 'year')")
  DBI::dbExecute(conn, "INSERT INTO dataset_online_resource_tbl
    (dataset_id, resource_url) VALUES (1, 'https://example.test/reviewed')")
  DBI::dbExecute(conn, "UPDATE dataset_license_tbl SET rights_basis_code = 'external_permission', embargo_reason = 'Reviewed reason'")
  datapond:::init_db(pond)
  expect_identical(DBI::dbReadTable(conn, "dataset_citation_date_tbl")$date_value, "2025")
  expect_identical(DBI::dbReadTable(conn, "dataset_online_resource_tbl")$resource_url, "https://example.test/reviewed")
  expect_identical(DBI::dbReadTable(conn, "dataset_license_tbl")$rights_basis_code, "external_permission")
  expect_identical(DBI::dbReadTable(conn, "dataset_license_tbl")$embargo_reason, "Reviewed reason")
})

test_that("current licensing saves are not reinterpreted as legacy choices", {
  pond <- legacy_patch_fixture()
  on.exit(unlink(pond, recursive = TRUE))
  datapond:::save_dataset_license(1L, data.frame(license_identifier = "CC-BY-4.0"),
    list(required_attribution = FALSE), pond)
  datapond:::init_db(pond)
  rights <- datapond:::get_dataset_license(1L, pond)
  expect_true(is.na(rights$rights_basis_code))
  expect_true(is.na(rights$access_level_code))
  expect_identical(rights$required_attribution, 0L)
})

test_that("portable bundles migrate once after child rows arrive and carry completion markers", {
  source <- legacy_patch_fixture()
  target <- tempfile("patch-import-")
  third <- tempfile("patch-reimport-")
  on.exit(unlink(c(source, target, third), recursive = TRUE))
  datapond:::ensure_pond_structure(source)
  folder <- datapond:::create_dataset_folder_structure(source, "older_dataset", "legacy-patch-record")
  conn <- datapond:::get_conn(source)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path = ?, data_path = ?",
    params = list(folder$dataset_path, folder$data_path))
  # A real older bundle has no new completion columns at all.
  for (field in c("legacy_citation_v1_migrated_at", "legacy_wave2_v1_migrated_at")) {
    DBI::dbExecute(conn, paste("DROP VIEW IF EXISTS dataset_temporal_coverage_compat_vw"))
    DBI::dbExecute(conn, paste("ALTER TABLE entry_tbl DROP COLUMN", field))
  }
  DBI::dbDisconnect(conn)
  bundle <- datapond:::create_dataset_transfer_bundle(1L, source)
  importer <- function(pond) {
    datapond:::ensure_pond_structure(pond)
    datapond:::init_db(pond)
    datapond:::seed_pond_parties(pond)
    datapond:::get_users(pond)$id[1]
  }
  user <- importer(target)
  imported <- datapond:::import_dataset_transfer_bundle(bundle$path, target, user)
  conn <- datapond:::get_conn(target)
  expect_true(all(datapond:::legacy_patch_v1_status(conn)$pending == 0L))
  expect_equal(nrow(DBI::dbReadTable(conn, "dataset_lineage_source_tbl")), 1L)
  expect_equal(nrow(DBI::dbReadTable(conn, "dataset_citation_date_tbl")), 1L)
  # Delete converted rows while preserving original snapshots, then transfer.
  for (table in c("dataset_lineage_source_tbl", "dataset_citation_date_tbl", "dataset_temporal_extent_tbl")) {
    DBI::dbExecute(conn, paste("DELETE FROM", table))
  }
  DBI::dbDisconnect(conn)
  current_bundle <- datapond:::create_dataset_transfer_bundle(imported$dataset_id, target, user)
  third_user <- importer(third)
  datapond:::import_dataset_transfer_bundle(current_bundle$path, third, third_user)
  datapond:::init_db(third)
  conn <- datapond:::get_conn(third)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  for (table in c("dataset_lineage_source_tbl", "dataset_citation_date_tbl", "dataset_temporal_extent_tbl")) {
    expect_equal(nrow(DBI::dbReadTable(conn, table)), 0L)
  }
  expect_identical(DBI::dbReadTable(conn, "entry_tbl")$citation_date, "2020-03")
})
