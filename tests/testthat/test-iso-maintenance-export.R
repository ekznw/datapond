maintenance_fixture <- function(kind = "vector") {
  f <- distribution_fixture(kind)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  datapond:::ensure_columns(conn, "entry_tbl", c("data_complete_status", "update_frequency"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET data_complete_status='started', update_frequency='Annually',
    resource_language_code='eng', resource_character_encoding='utf8'")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='1', max_value='10', data_field_units='m'")
  datapond:::save_dataset_feature_catalogue(1, list(title = "Maintenance fixture catalogue"), f$path)
  DBI::dbExecute(conn, "INSERT INTO persons_tbl(name_first,name_last,organisation_id,email,role_title)
    VALUES ('Maya',NULL,1,'maya@example.test','Metadata officer')")
  DBI::dbExecute(conn, "INSERT INTO metadata_review_tbl(dataset_id,review_timestamp,review_date,
    reviewer_id,reviewer_name,reviewer_role,metadata_section,update_reason,update_summary,notes,
    update_type,metadata_status,source_of_change,app_version) VALUES
    (1,'2026-10-17 12:34:56.123+02:00','2020-01-01',1,'STALE NAME','maintainer','citation; lineage',
      'Correct citation','Added source references','PRIVATE REVIEW NOTE','metadata_correction','needs_attention','INTERNAL SOURCE','INTERNAL VERSION'),
    (1,'2026-10-18 10:00:00+02:00',NULL,1,'STALE NAME','maintainer','lineage','Check sources','Confirmed references',NULL,NULL,NULL,NULL,NULL),
    (1,'','2026-10',NULL,'Former Reviewer','Senior ecologist','about','Review description',NULL,NULL,NULL,NULL,NULL,NULL),
    (1,'2026-10-19 09:00:00+02:00',NULL,1,NULL,'pointOfContact','general','Review record',NULL,NULL,NULL,NULL,NULL,NULL)")
  f
}

test_that("maintenance exports preserve event values and contact ownership across scopes", {
  core_validation_engine()
  for (kind in c("vector", "raster")) {
    f <- maintenance_fixture(kind)
    for (scope in c("core", "all")) {
      a <- datapond:::assess_iso_metadata_export(1, f$path, sections = scope)
      expect_true(a$validation$valid, info = paste(a$validation$issues$message, collapse = "\n"))
      expect_true(a$semantics$valid, info = paste(a$semantics$issues$message, collapse = "\n"))
      xml <- a$document; ns <- xml2::xml_ns(xml)
      maintenance <- xml2::xml_find_all(xml, "//mdb:metadataMaintenance/mmi:MD_MaintenanceInformation", ns)
      expect_length(maintenance, 1L)
      expect_identical(core_xpath_text(xml, "//mri:status/*"), "underDevelopment")
      expect_identical(core_xpath_text(xml, "//mri:resourceMaintenance//mmi:MD_MaintenanceFrequencyCode"), "annually")
      expect_identical(core_xpath_text(maintenance, "mmi:maintenanceAndUpdateFrequency/*"), "unknown")
      expect_identical(core_xpath_text(maintenance, "mmi:maintenanceDate//cit:date/*"),
        c("2026-10-17T12:34:56.123+02:00", "2026-10-18T10:00:00+02:00", "2026-10", "2026-10-19T09:00:00+02:00"))
      expect_identical(core_xpath_text(maintenance, "mmi:maintenanceDate//cit:dateType/*"), rep("lastRevision", 4))
      expect_identical(core_xpath_text(maintenance, "mmi:maintenanceScope//mcc:level/*"), "metadata")
      notes <- core_xpath_text(maintenance, "mmi:maintenanceNote/*")
      expect_length(notes, 5L)
      expect_match(notes[2], "Metadata sections: citation; lineage", fixed = TRUE)
      expect_match(notes[2], "Reason: Correct citation\nSummary: Added source references", fixed = TRUE)
      expect_match(notes[4], "Recorded reviewer role: Senior ecologist", fixed = TRUE)
      expect_identical(core_xpath_text(maintenance, "mmi:contact//cit:CI_Individual/cit:name/*"), c("Maya", "Former Reviewer", "Maya"))
      expect_identical(core_xpath_text(maintenance, "mmi:contact//cit:role/*"), c("custodian", "pointOfContact"))
      expect_length(xml2::xml_find_all(maintenance, "mmi:contact//cit:role[@gco:nilReason='unknown']", ns), 1L)
      expect_identical(core_xpath_text(maintenance, "mmi:contact//cit:electronicMailAddress/*"), rep("maya@example.test", 2))
      expect_length(xml2::xml_find_all(xml, "//mri:citation//cit:citedResponsibleParty", ns), 0L)
      expect_false(grepl("STALE NAME|PRIVATE REVIEW NOTE|INTERNAL SOURCE|INTERNAL VERSION|needs_attention|metadata_correction|2020-01-01", as.character(xml)))
    }
    unlink(f$path, recursive = TRUE)
  }
})

test_that("unknown resource values retain text and blank values do not invent status", {
  core_validation_engine()
  f <- maintenance_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET data_complete_status='Awaiting funding & review', update_frequency='Every decade; subject to funding'")
  a <- datapond:::assess_iso_metadata_export(1, f$path, sections = "core")
  expect_true(a$validation$valid)
  expect_identical(core_xpath_text(a$document, "//mri:supplementalInformation/*"), "Recorded dataset status: Awaiting funding & review")
  expect_identical(core_xpath_text(a$document, "//mri:resourceMaintenance//mmi:maintenanceNote/*"), "Recorded update frequency: Every decade; subject to funding")
  expect_true(any(grepl("status", a$validation$warnings$location)))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET data_complete_status='complete', update_frequency='not planned'")
  xml <- datapond:::build_iso_core_metadata_xml(1, f$path)
  expect_identical(core_xpath_text(xml, "//mri:status/*"), "completed")
  expect_identical(core_xpath_text(xml, "//mri:resourceMaintenance//mmi:MD_MaintenanceFrequencyCode"), "notPlanned")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET data_complete_status=' ', update_frequency=NULL, lifecycle_status='released'")
  DBI::dbExecute(conn, "DELETE FROM metadata_review_tbl")
  xml <- datapond:::build_iso_core_metadata_xml(1, f$path)
  expect_length(xml2::xml_find_all(xml, "//mri:status|//mri:resourceMaintenance|//mdb:metadataMaintenance", xml2::xml_ns(xml)), 0L)
})

test_that("maintenance date and linked-person failures cannot silently fall back", {
  f <- maintenance_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE metadata_review_tbl SET review_timestamp='invalid', review_date='2026-10-01' WHERE id=1")
  expect_error(datapond:::build_iso_core_metadata_xml(1, f$path), "not a valid ISO date")
  DBI::dbExecute(conn, "UPDATE metadata_review_tbl SET review_timestamp='', review_date='2026-02-30' WHERE id=1")
  expect_error(datapond:::build_iso_core_metadata_xml(1, f$path), "not a valid ISO date")
  DBI::dbExecute(conn, "UPDATE metadata_review_tbl SET review_date=NULL WHERE id=1")
  expect_error(datapond:::build_iso_core_metadata_xml(1, f$path), "review date is missing")
  DBI::dbExecute(conn, "PRAGMA foreign_keys=OFF")
  DBI::dbExecute(conn, "UPDATE metadata_review_tbl SET review_date='2026-10-01', reviewer_id=999 WHERE id=1")
  expect_error(datapond:::build_iso_core_metadata_xml(1, f$path), "contact link was not found")
})

test_that("frequency aliases retain the distinction between twice yearly and every two years", {
  codes <- c("biannually", "biennially", "asNeeded", "notPlanned")
  expect_identical(datapond:::iso_maintenance_code("biannually", codes), "biannually")
  expect_identical(datapond:::iso_maintenance_code("biennially", codes), "biennially")
  expect_identical(datapond:::iso_maintenance_code("as needed", codes), "asNeeded")
  expect_identical(datapond:::iso_maintenance_code("unknown custom", codes), NA_character_)
})
