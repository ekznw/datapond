quality_fixture <- function(kind = "vector") {
  fixture <- distribution_fixture(kind)
  rows <- data.frame(
    scope_code = c("dataset", "attribute", "dataset", "dataset"),
    data_file_id = c(1L, 1L, NA, NA), attribute_id = c(NA, 1L, NA, NA),
    measure_identifier = c("MISSING-01", "DOMAIN-02", "TIME-03", "FORMAT-04"),
    measure_name = c("Missing observations", "Allowed values", "Observation dates", "Format compliance"),
    measure_type_code = c("completeness", "logicalConsistency", "temporalQuality", "logicalConsistency"),
    quality_element_code = c("DQ_CompletenessOmission", "DQ_DomainConsistency", "DQ_TemporalValidity", "DQ_FormatConsistency"),
    evaluation_method = c("Compared with the signed register.", "Checked the allowed-value list.", "Reviewed survey dates.", "Checked format specification."),
    evaluation_date = c("2026-10-01", "2026-10-02T09:10:11.50+02:00", "2026", "2026-09"),
    result_type_code = c("quantitative", "conformance", "descriptive", "conformance"),
    result_value = c("00.50", NA, NA, NA), result_unit = c("percent", NA, NA, NA),
    conformance_specification = c(NA, "Allowed-value register v2", NA, "Format specification v1.3"),
    explanation = c("One expected observation was absent.", "Two codes are outside the domain.", "Observation dates match the survey period.", NA),
    pass = c(NA, 0L, NA, 1L), stringsAsFactors = FALSE)
  datapond:::replace_dataset_quality_results(fixture$dataset_id, rows, fixture$path)
  fixture
}

test_that("quality results validate with core, lineage, distribution and licensing", {
  core_validation_engine()
  for (kind in c("vector", "raster")) {
    fixture <- quality_fixture(kind)
    result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "quality")
    expect_identical(result$validation$status, "pass", info = paste(result$validation$issues$message, collapse = "\n"))
    expect_identical(result$export_scope, "quality")
    expect_identical(result$validation$quality_schema_package, "ISO 19157 1.2.0")
    expect_match(result$validation$coverage, "Excludes content", fixed = TRUE)
    xml <- result$document; ns <- xml2::xml_ns(xml)
    rows <- datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)
    expect_identical(xml2::xml_name(xml2::xml_find_all(xml, "//mdq:report/*", ns)), rows$quality_element_code)
    expect_identical(core_xpath_text(xml, "//mdq:measureIdentification//mcc:code/*"), rows$measure_identifier)
    expect_identical(core_xpath_text(xml, "//mdq:nameOfMeasure/*"), rows$measure_name)
    expect_identical(core_xpath_text(xml, "//mdq:scope//mcc:MD_ScopeCode"), rows$scope_code)
    expect_match(core_xpath_text(xml, "//mcc:attributes/*"), "Attribute: value", fixed = TRUE)
    expect_match(core_xpath_text(xml, "//mdq:scope//mcc:other/*"), if (kind == "vector") "sites.gpkg" else "elevation.tif", fixed = TRUE)
    expect_identical(core_xpath_text(xml, "//mdq:DQ_QuantitativeResult/mdq:value/gco:Record/gco:field/gco:Field/gco:value/*"), "00.50")
    expect_identical(core_xpath_text(xml, "//mdq:valueUnit/gml:UnitDefinition/gml:name"), "percent")
    expect_identical(core_xpath_text(xml, "//mdq:pass/gco:Boolean"), c("false", "true"))
    expect_identical(core_xpath_text(xml, "//mdq:specification//cit:title/*"), rows$conformance_specification[c(2,4)])
    expect_identical(core_xpath_text(xml, "//mdq:DQ_DescriptiveResult/mdq:statement/*"), rows$explanation[c(1,3)])
    expect_identical(core_xpath_text(xml, "//mdq:DQ_ConformanceResult/mdq:explanation/*"), rows$explanation[2])
    expect_identical(core_xpath_text(xml, "//mdq:DQ_EvaluationMethod/mdq:dateTime/gco:DateTime"), rows$evaluation_date[2])
    expect_identical(core_xpath_text(xml, "//mdq:evaluationMethodDescription/*"), c(
      paste0(rows$evaluation_method[1], "\nEvaluation date (day precision): 2026-10-01"), rows$evaluation_method[2],
      paste0(rows$evaluation_method[3], "\nEvaluation date (year precision): 2026"),
      paste0(rows$evaluation_method[4], "\nEvaluation date (month precision): 2026-09")))
    expect_false(grepl("T00:00:00Z", as.character(xml), fixed = TRUE))
    expect_length(xml2::xml_find_all(xml, "//mrl:LI_Lineage", ns), 1L)
    expect_length(xml2::xml_find_all(xml, "//mrd:MD_Distribution", ns), 1L)
    expect_identical(core_xpath_text(xml, "//mco:reference//cit:title/*"), "Creative Commons Attribution 4.0")
    expect_identical(as.character(datapond:::build_iso_quality_metadata_xml(fixture$dataset_id, fixture$path)), as.character(xml))
    if (kind == "vector") {
      datapond:::save_dataset_feature_catalogue(fixture$dataset_id, list(title = "Quality fixture attributes"), fixture$path)
      conn <- datapond:::get_conn(fixture$path)
      DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='eng', resource_character_encoding='utf8'")
      DBI::dbDisconnect(conn)
      all <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path)
      expect_true(all$validation$valid, info = paste(all$validation$issues$message, collapse = "\n"))
      expect_length(xml2::xml_find_all(all$document, "//mdq:DQ_DataQuality", xml2::xml_ns(all$document)), 4L)
    }
    unlink(fixture$path, recursive = TRUE)
  }
})

test_that("every precise measure choice maps to a concrete ISO element", {
  core_validation_engine()
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  elements <- datapond:::iso_quality_elements()
  rows <- data.frame(quality_element_code = elements$code, measure_type_code = elements$category,
    measure_identifier = paste0("TEST-", seq_len(nrow(elements))), evaluation_method = "Explicit manual review.",
    result_type_code = "descriptive", explanation = "Review found no discrepancy.")
  datapond:::replace_dataset_quality_results(fixture$dataset_id, rows, fixture$path)
  result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = "quality")
  expect_true(result$validation$valid, info = paste(result$validation$issues$message, collapse = "\n"))
  expect_identical(xml2::xml_name(xml2::xml_find_all(result$document, "//mdq:report/*", xml2::xml_ns(result$document))), elements$code)
  expect_length(xml2::xml_find_all(result$document, "//mdq:dateTime", xml2::xml_ns(result$document)), 0L)
})

test_that("broad legacy types remain saved but cannot silently become a specific test", {
  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  rows <- data.frame(measure_type_code = "completeness", measure_identifier = "LEGACY", evaluation_method = "Review",
    result_type_code = "descriptive", explanation = "Reviewed.")
  datapond:::replace_dataset_quality_results(fixture$dataset_id, rows, fixture$path)
  stored <- datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)
  expect_true(is.na(stored$quality_element_code))
  expect_error(datapond:::build_iso_quality_metadata_xml(fixture$dataset_id, fixture$path), "LEGACY: choose a specific test")
  rows$quality_element_code <- "DQ_DomainConsistency"
  expect_error(datapond:::replace_dataset_quality_results(fixture$dataset_id, rows, fixture$path), "does not match")
  expect_identical(datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)$measure_identifier, "LEGACY")
})

test_that("quality export rejects damaged scope links, missing values and invalid dates", {
  fixture <- quality_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  row <- datapond:::get_dataset_quality_results(fixture$dataset_id, fixture$path)[1, , drop = FALSE]
  check <- function(changes) datapond:::iso_quality_export_row(modifyList(as.list(row), changes), conn, fixture$dataset_id)
  expect_error(check(list(result_value = NA_character_)), "no value")
  expect_error(check(list(evaluation_date = "2026-02-30")), "not a valid ISO date")
  expect_error(check(list(quality_element_code = "NotAnElement")), "unknown ISO quality element")
  expect_error(check(list(data_file_id = 999L)), "does not belong")
  expect_error(check(list(scope_code = "attribute", attribute_id = 999L)), "does not belong")
  expect_error(check(list(scope_code = "attribute", attribute_id = NA_integer_)), "requires a linked attribute")
  expect_error(check(list(conformance_specification = "Old result")), "another result type")
  # Same-dataset check, not just existence of the identifier.
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(uuid, title) VALUES ('foreign-quality', 'Another dataset')")
  other_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl(dataset_id, file_name, data_file_rel_path_full) VALUES (?, 'foreign.csv', 'data/foreign.csv')", params = list(other_id))
  foreign_file <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  expect_error(check(list(data_file_id = foreign_file)), "does not belong")
})

test_that("the corrected MDQ rule catches an empty measure reference and XSD checks results", {
  core_validation_engine()
  fixture <- quality_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  xml <- datapond:::build_iso_quality_metadata_xml(fixture$dataset_id, fixture$path)
  ns <- xml2::xml_ns(xml)
  xml2::xml_remove(xml2::xml_children(xml2::xml_find_first(xml, "//mdq:DQ_MeasureReference", ns)))
  report <- datapond:::validate_iso_metadata_xml(xml)
  expect_identical(report$status, "fail")
  expect_true(any(report$issues$rule == "$hasIdentifier or $hasName"))
  expect_equal(sum(grepl("/mdq/1.2.0/mdq.sch", report$adaptations$source, fixed = TRUE)), 4L)
  xml <- datapond:::build_iso_quality_metadata_xml(fixture$dataset_id, fixture$path)
  xml2::xml_remove(xml2::xml_find_first(xml, "//mdq:DQ_ConformanceResult/mdq:pass", xml2::xml_ns(xml)))
  report <- datapond:::validate_iso_metadata_xml(xml)
  expect_false(report$checks$valid[report$checks$stage == "xsd"])
})

test_that("unit URIs remain references and missing units are not fabricated", {
  core_validation_engine()
  fixture <- quality_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_quality_result_tbl SET result_unit = 'urn:example:units:percent' WHERE result_type_code = 'quantitative'")
  xml <- datapond:::build_iso_quality_metadata_xml(fixture$dataset_id, fixture$path)
  expect_true(datapond:::validate_iso_metadata_xml(xml)$valid)
  ns <- xml2::xml_ns(xml)
  expect_identical(xml2::xml_attr(xml2::xml_find_first(xml, "//mdq:valueUnit", ns), "xlink:href", ns), "urn:example:units:percent")
  DBI::dbExecute(conn, "UPDATE dataset_quality_result_tbl SET result_unit = NULL WHERE result_type_code = 'quantitative'")
  xml <- datapond:::build_iso_quality_metadata_xml(fixture$dataset_id, fixture$path)
  expect_length(xml2::xml_find_all(xml, "//mdq:valueUnit", xml2::xml_ns(xml)), 0L)
})

test_that("the quality editor preserves precise test selection and timestamp text on save and reload", {
  local_trusted_access_fixture()

  fixture <- iso_export_candidate_pond("vector")
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  state <- shiny::reactiveValues(data_pond = fixture$path, refresh_datasets = 0L,
                                current_dataset_id = fixture$dataset_id)
  shiny::testServer(datapond:::mod_iso_wave2_server, args = list(state = state), {
    session$flushReact()
    load(fixture$dataset_id)
    session$setInputs(quality_scope = "dataset", quality_data_file = "", quality_attribute = "",
      quality_measure_identifier = "UI-TEST", quality_measure_name = "Missing data",
      quality_measure_type = "DQ_CompletenessOmission", quality_method = "Compared with register.",
      quality_date = "2026-10-02T09:10:11.50+02:00", quality_result_type = "descriptive",
      quality_result_value = "", quality_result_unit = "", quality_specification = "",
      quality_explanation = "No missing observations.", quality_pass = "")
    session$setInputs(add_quality_result = 1L)
    expect_equal(nrow(quality()), 1L)
    expect_identical(quality()$quality_element_code, "DQ_CompletenessOmission")
    expect_identical(quality()$measure_type_code, "completeness")
    expect_identical(quality()$evaluation_date, "2026-10-02T09:10:11.50+02:00")
    datapond:::replace_dataset_quality_results(fixture$dataset_id, quality(), fixture$path)
    load(fixture$dataset_id)
    expect_identical(quality()$quality_element_code, "DQ_CompletenessOmission")
    expect_identical(quality()$evaluation_date, "2026-10-02T09:10:11.50+02:00")
  })
})
