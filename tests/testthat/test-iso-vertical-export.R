vertical_export_fixture <- function(kind = "vector") {
  f <- distribution_fixture(kind)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='eng', resource_character_encoding='utf8'")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='1', max_value='10', data_field_units='m'")
  datapond:::save_dataset_feature_catalogue(1, list(title = "Vertical fixture catalogue"), f$path)
  rows <- data.frame(
    extent_label = c("Terrain", "Water depth", "Survey", "Pressure", "Local height", "Custom reference"),
    minimum_value = c(-12.123456789012345, 0.25, 100, 700, -1, 5),
    maximum_value = c(123.12345678901235, 48.5, 900, 1013.25, 1, 15),
    unit_symbol = c("m", "m", "ftUS", "hPa", "m", "ft"),
    unit_uri = c("http://qudt.org/vocab/unit/M", NA, NA, "urn:example:unit:hPa", NA, "http://qudt.org/vocab/unit/FT"),
    vertical_crs_name = c("EGM2008 height", "MSL depth", "NAVD88 height (ftUS)", "Station pressure coordinates", "Local benchmark", "Custom depth datum"),
    vertical_crs_identifier = c("EPSG:3855", "EPSG:5715", "EPSG:6360", "LOCAL:pressure", NA, NA),
    vertical_crs_uri = c("https://epsg.io/3855", "urn:ogc:def:crs:EPSG::5715", NA, "urn:example:crs:pressure", NA, "https://example.test/crs/custom"),
    vertical_crs_type = c("geoidal", "depth", "geoidal", "barometric", "other", "depth"),
    description = c("Terrain & survey <extent>", "Positive depths", "Survey feet stay survey feet", "Observed pressure range", "Benchmark definition not recorded", "Custom documentation"))
  datapond:::replace_dataset_vertical_extents(1, rows, f$path)
  f
}

test_that("vertical coverage retains values, units and CRS definitions in complete exports", {
  core_validation_engine()
  for (kind in c("vector", "raster")) {
    f <- vertical_export_fixture(kind)
    before <- datapond:::get_dataset_vertical_extents(1, f$path)
    a <- datapond:::assess_iso_metadata_export(1, f$path, sections = "all")
    expect_true(a$validation$valid, info = paste(a$validation$issues$message, collapse = "\n"))
    expect_true(a$semantics$valid, info = paste(a$semantics$issues$message, collapse = "\n"))
    xml <- a$document; ns <- xml2::xml_ns(xml)
    vertical <- xml2::xml_find_all(xml, "//gex:EX_VerticalExtent", ns)
    expect_length(vertical, nrow(before))
    expect_identical(as.numeric(core_xpath_text(vertical, "gex:minimumValue/*")), before$minimum_value)
    expect_identical(as.numeric(core_xpath_text(vertical, "gex:maximumValue/*")), before$maximum_value)
    expect_length(xml2::xml_find_all(vertical, "gex:minimumValue/@uom|gex:maximumValue/@uom", ns), 0L)
    expect_identical(core_xpath_text(vertical, "gex:verticalCRSId//mcc:code/*"), c("3855", "5715", "6360", "pressure"))
    expect_identical(core_xpath_text(vertical, "gex:verticalCRSId//mcc:codeSpace/*"), c("EPSG", "EPSG", "EPSG", "LOCAL"))
    definitions <- core_xpath_text(vertical, ".//rbc:definition/*")
    expect_length(definitions, 3L)
    expect_match(definitions[1], 'VDATUM["EGM2008 geoid"]', fixed = TRUE)
    expect_match(definitions[2], ',down,', fixed = TRUE)
    expect_match(definitions[3], 'LENGTHUNIT["US survey foot"', fixed = TRUE)
    expect_identical(core_xpath_text(vertical, ".//mrs:referenceSystemType/*"), rep("vertical", 3))
    expect_identical(xml2::xml_attr(xml2::xml_find_all(vertical, "gex:verticalCRS[@xlink:href]", ns), "xlink:href", ns = ns),
      before$vertical_crs_uri[!is.na(before$vertical_crs_uri)])
    details <- core_xpath_text(xml, "//gex:EX_Extent[gex:verticalElement]/gex:description/*")
    for (i in seq_len(nrow(before))) {
      expect_match(details[i], paste0("Label: ", before$extent_label[i]), fixed = TRUE)
      expect_match(details[i], paste0("Note: ", before$description[i]), fixed = TRUE)
      expect_match(details[i], paste0("Unit: ", before$unit_symbol[i]), fixed = TRUE)
      expect_match(details[i], paste0("Vertical CRS: ", before$vertical_crs_name[i]), fixed = TRUE)
      expect_match(details[i], paste0("Recorded reference type: ", before$vertical_crs_type[i]), fixed = TRUE)
      if (!is.na(before$unit_uri[i])) expect_match(details[i], before$unit_uri[i], fixed = TRUE)
    }
    expect_length(xml2::xml_find_all(vertical[[5]], "gex:verticalCRS[@gco:nilReason='unknown']", ns), 1L)
    expect_length(xml2::xml_find_all(vertical[[5]], "gex:verticalCRSId|.//rbc:definition", ns), 0L)
    expect_equal(sum(grepl("unit and axis compatibility are unverified", a$validation$warnings$reason)), 2L)
    expect_identical(before, datapond:::get_dataset_vertical_extents(1, f$path))
    unlink(f$path, recursive = TRUE)
  }
})

test_that("vertical export rejects contradictory or malformed saved references", {
  f <- vertical_export_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  # Synthetic corruption bypasses UI and CHECK constraints to exercise export.
  DBI::dbExecute(conn, "PRAGMA ignore_check_constraints=ON")
  cases <- list(
    list("minimum_value=maximum_value+1", "cannot exceed"),
    list("unit_symbol='ft', unit_uri=NULL", "does not match"),
    list("unit_uri='http://qudt.org/vocab/unit/FT'", "unit symbol conflicts"),
    list("vertical_crs_uri='https://www.opengis.net/def/crs/EPSG/0/5715'", "different EPSG codes"),
    list("vertical_crs_identifier='EPSG:4326', vertical_crs_uri=NULL", "not a standalone vertical CRS"),
    list("vertical_crs_identifier='EPSG:4979', vertical_crs_uri=NULL", "not a standalone vertical CRS"),
    list("vertical_crs_identifier='EPSG:999999999', vertical_crs_uri=NULL", "cannot be resolved"),
    list("vertical_crs_identifier='EPSG:invalid'", "numeric code"),
    list("vertical_crs_type='depth'", "reference type conflicts"),
    list("vertical_crs_type='barometric'", "reference type conflicts"),
    list("vertical_crs_name=' '", "CRS name are required"),
    list("unit_symbol=' '", "Vertical unit"),
    list("vertical_crs_uri='file:///private/crs.xml'", "local file references"),
    list("unit_uri='file:///private/unit.xml'", "local file references")
  )
  original <- datapond:::get_dataset_vertical_extents(1, f$path)[1, ]
  fields <- c("minimum_value", "maximum_value", "unit_symbol", "unit_uri", "vertical_crs_name", "vertical_crs_identifier", "vertical_crs_uri", "vertical_crs_type")
  for (case in cases) {
    DBI::dbExecute(conn, paste0("UPDATE dataset_vertical_extent_tbl SET ", paste0(fields, "=?", collapse = ","), " WHERE sort_order=1"), params = unname(as.list(original[fields])))
    DBI::dbExecute(conn, paste0("UPDATE dataset_vertical_extent_tbl SET ", case[[1]], " WHERE sort_order=1"))
    expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), case[[2]], info = case[[1]])
  }
  DBI::dbExecute(conn, "UPDATE dataset_vertical_extent_tbl SET minimum_value=? WHERE sort_order=1", params = list(-Inf))
  expect_error(datapond:::build_iso_metadata_candidate_xml(1, f$path), "finite numbers")
  # Subset assessments continue to declare their limited scope.
  expect_s3_class(datapond:::build_iso_core_metadata_xml(1, f$path), "xml_document")
})

test_that("URI-only EPSG and opaque identifiers retain their meanings and order", {
  core_validation_engine()
  f <- vertical_export_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_vertical_extent_tbl SET vertical_crs_identifier=NULL,
    vertical_crs_uri='https://www.opengis.net/def/crs/EPSG/0/3855' WHERE sort_order=1")
  DBI::dbExecute(conn, "UPDATE dataset_vertical_extent_tbl SET vertical_crs_identifier='urn:example:pressure:v1' WHERE sort_order=4")
  DBI::dbExecute(conn, "UPDATE dataset_vertical_extent_tbl SET sort_order=20 WHERE sort_order=2")
  a <- datapond:::assess_iso_metadata_export(1, f$path)
  expect_true(a$validation$valid)
  ids <- core_xpath_text(a$document, "//gex:verticalCRSId//mcc:code/*")
  expect_identical(ids, c("3855", "6360", "urn:example:pressure:v1", "5715"))
  expect_identical(core_xpath_text(a$document, "//gex:verticalCRSId//mcc:codeSpace/*"), rep("EPSG", 3))
  DBI::dbExecute(conn, "UPDATE dataset_vertical_extent_tbl SET unit_symbol='ft' WHERE sort_order=3")
  failure <- datapond:::assess_iso_metadata_export(1, f$path)
  expect_identical(failure$validation$status, "not-built")
  expect_null(failure$document)
  expect_match(failure$validation$issues$message, "does not match")
})

test_that("pinned validation rejects illegal vertical units and absent CRS properties", {
  core_validation_engine()
  f <- vertical_export_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  xml <- datapond:::build_iso_metadata_candidate_xml(1, f$path); ns <- xml2::xml_ns(xml)
  minimum <- xml2::xml_find_first(xml, "//gex:EX_VerticalExtent/gex:minimumValue", ns)
  xml2::xml_set_attr(minimum, "uom", "m")
  bad <- datapond:::validate_iso_metadata_xml(xml)
  expect_false(bad$valid)
  expect_true(any(bad$issues$stage == "xsd"))
  xml2::xml_set_attr(minimum, "uom", NULL)
  first <- xml2::xml_find_first(xml, "//gex:EX_VerticalExtent", ns)
  xml2::xml_remove(xml2::xml_find_all(first, "gex:verticalCRS|gex:verticalCRSId", ns))
  bad <- datapond:::validate_iso_metadata_xml(xml)
  expect_false(bad$valid)
  expect_true(any(bad$issues$stage == "schematron" & grepl("neither a vertical CRS", bad$issues$message)))
})
