remaining_mapping_fixture <- function(kind = "vector") {
  f <- distribution_fixture(kind)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  datapond:::ensure_columns(conn, "entry_tbl", c("software_desc", "cell_size", "bbox", "epsg_code"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='eng', resource_character_encoding='utf8',
    software_desc='Legacy toolkit 1',cell_size='25 m'")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='1',max_value='10',data_field_units='m'")
  datapond:::save_dataset_feature_catalogue(1, list(title = "Remaining mapping catalogue"), f$path)
  DBI::dbExecute(conn, "INSERT INTO data_file_spatial_metadata_tbl
    (dataset_id,data_file_id,data_file_rel_path_full,layer_name,spatial_type,crs_auth_code,crs_status,
     include_in_dataset_bbox,geometry_type,feature_count,raster_nrow,raster_ncol,raster_nlyr,raster_res_x,raster_res_y)
    VALUES (1,1,?,'survey',?,'EPSG:4326','known',1,'POLYGON',12,100,200,3,0.02,0.01)",
    params = list(if (kind == "vector") "data/sites.gpkg" else "data/elevation.tif", kind))
  DBI::dbExecute(conn, "UPDATE dataset_spatial_summary_tbl SET
    derivation_method='included_source_envelope',contributor_keys='spatial:1; manual:1'")
  DBI::dbExecute(conn, "UPDATE dataset_manual_spatial_extent_tbl SET source_note='Survey footprint supplied by the field team'")
  DBI::dbExecute(conn, "INSERT INTO dataset_temporal_extent_tbl
    (dataset_id,extent_label,start_value,start_precision,end_value,end_precision,description)
    VALUES (1,'Survey period','2016','year','2018','year','Intermittent sampling within this period')")
  f
}

test_that("remaining vector and grid mappings retain values and validate together", {
  core_validation_engine()
  for (kind in c("vector", "raster")) {
    f <- remaining_mapping_fixture(kind)
    a <- datapond:::assess_iso_metadata_export(1, f$path)
    expect_true(a$validation$valid, info = paste(a$validation$issues$message, collapse = "\n"))
    expect_true(a$semantics$valid, info = paste(a$semantics$issues$message, collapse = "\n"))
    xml <- a$document; ns <- xml2::xml_ns(xml)
    expect_identical(core_xpath_text(xml, "//mri:spatialRepresentationType/*"), if (kind == "vector") "vector" else "grid")
    supplemental <- core_xpath_text(xml, "//mri:supplementalInformation/*")
    expect_length(supplemental, 1L)
    expect_match(supplemental, "Legacy toolkit 1", fixed = TRUE)
    descriptions <- core_xpath_text(xml, "//mri:extent/gex:EX_Extent/gex:description/*")
    expect_true(any(grepl("Survey footprint supplied by the field team", descriptions, fixed = TRUE)))
    expect_true(any(grepl("Survey period\nIntermittent sampling within this period", descriptions, fixed = TRUE)))
    expect_identical(core_xpath_text(xml, "//gml:beginPosition"), "2016")
    expect_length(xml2::xml_find_all(xml, "//mrl:LI_ProcessStep", ns), 0L)
    if (kind == "vector") {
      expect_identical(core_xpath_text(xml, "//msr:geometricObjectType/*"), "surface")
      expect_identical(core_xpath_text(xml, "//msr:geometricObjectCount/*"), "12")
      expect_identical(core_xpath_text(xml, "//mri:distance/gco:Distance"), "25")
      expect_match(supplemental, "data/sites.gpkg / survey", fixed = TRUE)
    } else {
      expect_identical(core_xpath_text(xml, "//msr:dimensionName/*"), c("row", "column"))
      expect_identical(core_xpath_text(xml, "//msr:dimensionSize/*"), c("100", "200"))
      expect_equal(as.numeric(core_xpath_text(xml, "//msr:resolution/*")), c(0.01, 0.02))
      expect_identical(xml2::xml_attr(xml2::xml_find_all(xml, "//msr:resolution/*", ns), "uom"), c("deg", "deg"))
      expect_identical(core_xpath_text(xml, "//msr:numberOfDimensions/@gco:nilReason"), "unknown")
      expect_length(xml2::xml_find_all(xml, "//mri:distance", ns), 0L)
      expect_match(supplemental, "Recorded band/layer count: 3", fixed = TRUE)
    }
    unlink(f$path, recursive = TRUE)
  }
})

test_that("multipart counts, unknown geometry and grid dimensions do not assert invented facts", {
  f <- remaining_mapping_fixture()
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  for (geometry in c("MULTIPOLYGON", "polygons", "POLYGON, MULTIPOLYGON", "GEOMETRY")) {
    DBI::dbExecute(conn, "UPDATE data_file_spatial_metadata_tbl SET geometry_type=?", params = list(geometry))
    xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
    expect_length(core_xpath_text(xml, "//msr:geometricObjectCount/*"), 0L)
    expect_match(core_xpath_text(xml, "//mri:supplementalInformation/*"), "Recorded feature count: 12", fixed = TRUE)
    if (geometry == "GEOMETRY") expect_identical(core_xpath_text(xml, "//msr:geometricObjectType/@gco:nilReason"), "unknown")
    else expect_identical(core_xpath_text(xml, "//msr:geometricObjectType/*"), "surface")
  }
  DBI::dbExecute(conn, "UPDATE data_file_spatial_metadata_tbl SET spatial_type='raster',raster_nlyr=1,crs_auth_code='EPSG:32735'")
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_identical(core_xpath_text(xml, "//msr:numberOfDimensions/*"), "2")
  expect_identical(core_xpath_text(xml, "//msr:resolution/gco:Measure/@uom"), c("m", "m"))
  expect_identical(core_xpath_text(xml, "//msr:cellGeometry/@gco:nilReason"), "unknown")
})

test_that("spatial links, inclusion and malformed measurements are checked", {
  f <- remaining_mapping_fixture("raster")
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(id,uuid,title) VALUES (2,'other-resource','Other resource')")
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl(id,dataset_id,data_file_rel_path_full) VALUES (999,2,'data/other.tif')")
  for (case in list(c("raster_nrow", "-1", "positive integer"), c("raster_ncol", "1.5", "positive integer"),
                   c("raster_res_y", "0", "finite positive"), c("raster_nlyr", "0", "positive integer"),
                   c("crs_auth_code", "LOCAL:unknown", "locally resolvable"),
                   c("data_file_id", "999", "one file"))) {
    old <- DBI::dbGetQuery(conn, paste0("SELECT ", case[1], " AS value FROM data_file_spatial_metadata_tbl"))$value
    DBI::dbExecute(conn, paste0("UPDATE data_file_spatial_metadata_tbl SET ", case[1], "=?"), params = list(case[2]))
    expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), case[3])
    DBI::dbExecute(conn, paste0("UPDATE data_file_spatial_metadata_tbl SET ", case[1], "=?"), params = list(old))
  }
  DBI::dbExecute(conn, "UPDATE data_file_spatial_metadata_tbl SET include_in_dataset_bbox=0")
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_length(core_xpath_text(xml, "//msr:dimensionSize/*"), 0L)
  DBI::dbExecute(conn, "UPDATE data_file_spatial_metadata_tbl SET include_in_dataset_bbox=1")
  DBI::dbExecute(conn, "UPDATE data_file_tbl SET active=0")
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_length(core_xpath_text(xml, "//msr:dimensionSize/*"), 0L)
})

test_that("legacy values are bounded fallbacks and extent notes require actual contributors", {
  f <- remaining_mapping_fixture()
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET bbox='29; -30; 31; -28',epsg_code='4326',cell_size='about one kilometre'")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_process_step_tbl(dataset_id,description,tool_software_reference)
    VALUES (1,'Recorded processing','Current toolkit 2')")
  DBI::dbExecute(conn, "UPDATE dataset_spatial_summary_tbl SET contributor_keys='spatial:1'")
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_false(grepl("Legacy toolkit 1|Survey footprint supplied", as.character(xml)))
  expect_match(core_xpath_text(xml, "//mri:supplementalInformation/*"), "Legacy cell size: about one kilometre", fixed = TRUE)
  for (table in c("dataset_spatial_summary_tbl", "data_file_spatial_metadata_tbl", "dataset_manual_spatial_extent_tbl"))
    DBI::dbExecute(conn, paste0("DELETE FROM ", table))
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_identical(core_xpath_text(xml, "//gex:westBoundLongitude/*"), "29")
  expect_identical(core_xpath_text(xml, "//mrs:referenceSystemIdentifier//mcc:code/*"), "4326")
  expect_true(any(grepl("legacy WGS84", core_xpath_text(xml, "//gex:description/*"), fixed = TRUE)))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET epsg_code='32735'")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "explicit EPSG:4326")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET epsg_code='4326'")
  DBI::dbExecute(conn, "INSERT INTO dataset_spatial_summary_tbl(dataset_id,bbox_status) VALUES (1,'invalid')")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "valid bounding box")
})

test_that("temporal endpoints retain descriptions but reject inconsistent stored values", {
  f <- remaining_mapping_fixture()
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_temporal_extent_tbl SET end_value='2015'")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "later than")
  DBI::dbExecute(conn, "UPDATE dataset_temporal_extent_tbl SET end_value='2018-02-31',end_precision='day'")
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "valid")
})

test_that("transfer and release audit do not manufacture lineage, publishers or publication dates", {
  f <- remaining_mapping_fixture()
  on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  baseline <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  DBI::dbExecute(conn, "INSERT INTO persons_tbl(name_first,name_last) VALUES ('PRIVATE','RELEASER')")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET released_by_person_id=1,released_at='2037-12-31T23:59:59Z'")
  DBI::dbExecute(conn, "INSERT INTO dataset_import_tbl
    (dataset_id,source_dataset_uuid,source_data_pond_id,bundle_id,bundle_format_version,archive_name,exported_at)
    VALUES (1,'fixture-vector','PRIVATE-POND','PRIVATE-BUNDLE',1,'PRIVATE-ARCHIVE.zip','2036-01-01')")
  DBI::dbExecute(conn, "UPDATE data_file_tbl SET file_size_kb=123456")
  DBI::dbExecute(conn, "UPDATE dataset_license_tbl SET licence_notes='PRIVATE-LICENCE',license_template_notes='PRIVATE-TEMPLATE'")
  after <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_identical(as.character(after), as.character(baseline))
  expect_false(grepl("PRIVATE|2037-12-31|123456", as.character(after)))
  expect_length(core_xpath_text(after, "//mrd:transferSize/*"), 0L)
  # The legacy access text already had an intentional export fallback; it now
  # has a named mapping rather than being misclassified with template audit.
  DBI::dbExecute(conn, "UPDATE dataset_license_tbl SET access_level_code=NULL,access_level='Legacy access terms'")
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_true("Access: Legacy access terms" %in% core_xpath_text(xml, "//mco:otherConstraints/*"))
  DBI::dbExecute(conn, "UPDATE dataset_license_tbl SET access_level_code='internal'")
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  expect_false(grepl("Legacy access terms", as.character(xml), fixed = TRUE))
})

test_that("every crosswalk mapping has a reviewed disposition and an evidence index", {
  source_root <- datapond_test_source_root()
  crosswalk <- datapond:::load_current_iso_crosswalk()
  coverage <- crosswalk$coverage
  expect_identical(coverage$version, datapond:::current_iso_crosswalk_version())
  mappings <- crosswalk$mappings$mappings
  ids <- vapply(mappings, `[[`, "", "id")
  expect_identical(vapply(coverage$mappings, `[[`, "", "id"), ids)
  expect_identical(anyDuplicated(ids), 0L)
  for (i in seq_along(ids)) {
    row <- coverage$mappings[[i]]
    expect_true(row$disposition %in% c("implemented", "migration", "local"), info = ids[i])
    expect_true(nzchar(row$note), info = ids[i])
    expect_identical(row$disposition == "local", startsWith(mappings[[i]]$target, "DataPond:"), info = ids[i])
    # Source checkout evidence; runtime installations need only the ledger.
    paths <- c(unlist(row$implementation), unlist(row$tests))
    expect_true(all(file.exists(file.path(source_root, paths))), info = ids[i])
  }
  expect_false(any(vapply(mappings, `[[`, "", "status") == "gap"))
  expect_true("GAP-018" %in% vapply(crosswalk$gaps$gaps, `[[`, "", "id"))
})
