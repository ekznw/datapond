semantic_fixture <- function(kind = "vector", scope = "dataset", creation = "2026-09-30T08:30:00Z") {
  f <- distribution_fixture(kind)
  conn <- datapond:::get_conn(f$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='zul',resource_character_encoding='utf16',
    metadata_resource_scope_code=?,metadata_creation_date=?,geographic_extent_name=?",
    params = list(scope, creation, if (kind == "tabular" && scope == "dataset") "KwaZulu-Natal" else NA_character_))
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='1',max_value='10',data_field_units='m'")
  DBI::dbExecute(conn, "INSERT INTO dataset_temporal_extent_tbl(dataset_id,extent_label,start_value,start_precision,end_value,end_precision)
    VALUES (1,'Survey','2016','year','2018-02','month')")
  datapond:::save_dataset_feature_catalogue(1, list(title = "Semantic fixture catalogue",
    version = "1", version_date = "2026-10-01", language_code = "zul", producer_organisation_id = 1L), f$path)
  datapond:::replace_dataset_quality_results(1, data.frame(
    measure_identifier = c("Q1", "Q2", "Q3"), measure_type_code = c("completeness", "logicalConsistency", "temporalQuality"),
    quality_element_code = c("DQ_CompletenessOmission", "DQ_DomainConsistency", "DQ_TemporalValidity"),
    evaluation_method = "Recorded test method", result_type_code = c("quantitative", "conformance", "descriptive"),
    result_value = c("0.5", NA, NA), result_unit = c("percent", NA, NA),
    conformance_specification = c(NA, "Allowed values v1", NA), pass = c(NA, 0L, NA),
    explanation = c("Missing value proportion", "Observed invalid code", "Date range checked")), f$path)
  f$root <- iso_package_fixture_files(f)
  f
}

test_that("vector, raster, geographic table, nongeographic and date-only packages meet local acceptance", {
  core_validation_engine()
  scenarios <- list(c("vector", "dataset"), c("raster", "dataset"),
    c("tabular", "dataset"), c("tabular", "nonGeographicDataset"),
    c("tabular", "nonGeographicDataset", "2016"))
  for (case in scenarios) {
    partial <- length(case) == 3L
    f <- semantic_fixture(case[1], case[2], if (partial) case[3] else "2026-09-30T08:30:00Z")
    parent <- tempfile(); dir.create(parent)
    paths <- datapond:::write_iso_wave3_documents(1, f$path, parent, if (partial) "datapond" else "pinned")
    r <- jsonlite::read_json(paths[["validation"]], simplifyVector = TRUE)
    expect_identical(r$validation$status, if (partial) "fail" else "pass")
    expect_identical(r$validation$datapond_profile$status, "pass")
    expect_identical(r$semantics$status, "pass", info = paste(r$semantics$issues$message, collapse = "\n"))
    expect_true(r$package$valid)
    expect_identical(r$receiving_catalogue$status, "not-assessed")
    d <- xml2::read_xml(paths[["metadata"]])
    ns <- xml2::xml_ns(d)
    expect_identical(core_xpath_text(d, "//mri:citation/cit:CI_Citation/cit:title/*"), paste("ISO", case[1], "fixture"))
    expect_identical(core_xpath_text(d, "//gml:beginPosition"), "2016")
    expect_identical(core_xpath_text(d, "//gml:endPosition"), "2018-02")
    expect_identical(core_xpath_text(d, "//mdq:DQ_ConformanceResult/mdq:pass/*"), "false")
    expect_identical(core_xpath_text(d, "//mdq:DQ_QuantitativeResult/mdq:value//gco:value/*"), "0.5")
    expect_identical(core_xpath_text(d, "/mdb:MD_Metadata/mdb:defaultLocale//lan:MD_CharacterSetCode"), "UTF-8")
    if (case[1] != "raster") {
      expect_identical(core_xpath_text(d, "//mri:defaultLocale//lan:MD_CharacterSetCode"), "UTF-16")
      expect_true(file.exists(paths[["feature_catalogue"]]))
    }
    expect_identical(datapond:::get_dataset_record(1, f$path)$resource_character_encoding, "utf16")
    expect_false(any(grepl("/19115/resources/Codelist/", xml2::xml_attr(xml2::xml_find_all(d, "//*[@codeList]"), "codeList"))))
    unlink(f$path, recursive = TRUE); unlink(parent, recursive = TRUE)
  }
})

test_that("code membership and registry errors fail semantics even when XSD permits them", {
  core_validation_engine()
  f <- semantic_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  document <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  for (attribute in c("codeListValue", "codeList")) {
    d <- xml2::read_xml(as.character(document))
    node <- xml2::xml_find_first(d, "//cit:CI_RoleCode", xml2::xml_ns(d))
    xml2::xml_set_attr(node, attribute, if (attribute == "codeListValue") "inventedRole" else "https://example.test/wrong-list")
    pinned <- datapond:::validate_iso_metadata_xml(d)
    expect_true(pinned$checks$valid[pinned$checks$stage == "xsd"])
    s <- datapond:::validate_iso_export_semantics(d)
    expect_identical(s$status, "fail")
    expect_true(any(s$issues$rule == if (attribute == "codeListValue") "SEM-CODE" else "SEM-CODE-URI"))
  }
  d <- xml2::read_xml(as.character(document))
  node <- xml2::xml_find_first(d, "//lan:LanguageCode", xml2::xml_ns(d))
  xml2::xml_set_attr(node, "codeListValue", "xxx-invalid")
  expect_false(datapond:::validate_iso_export_semantics(d)$valid)
  xml2::xml_set_attr(node, "codeListValue", "eng")
  node <- xml2::xml_find_first(d, "/mdb:MD_Metadata/mdb:defaultLocale//lan:MD_CharacterSetCode", xml2::xml_ns(d))
  xml2::xml_set_attr(node, "codeListValue", "UTF-16")
  expect_true(any(datapond:::validate_iso_export_semantics(d)$issues$rule == "SEM-ENCODING"))
})

test_that("semantic bounds and time comparisons respect precision, time zones and antimeridian extents", {
  core_validation_engine()
  f <- semantic_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  document <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  set <- function(d, xpath, text) xml2::xml_set_text(xml2::xml_find_first(d, xpath, xml2::xml_ns(d)), text)
  for (case in list(
    c("//gex:northBoundLatitude/*", "91", "SEM-BOUNDS"),
    c("//gex:northBoundLatitude/*", "-40", "SEM-BOUNDS"),
    c("//gml:beginPosition", "2019", "SEM-ORDER"),
    c("//gml:beginPosition", "2016-02-30", "SEM-DATE"))) {
    d <- xml2::read_xml(as.character(document)); set(d, case[1], case[2])
    s <- datapond:::validate_iso_export_semantics(d)
    expect_identical(s$status, "fail")
    expect_true(any(s$issues$rule == case[3]))
  }
  d <- xml2::read_xml(as.character(document))
  set(d, "//gex:westBoundLongitude/*", "170")
  set(d, "//gex:eastBoundLongitude/*", "-170")
  expect_true(datapond:::validate_iso_export_semantics(d)$valid)
  set(d, "//gml:beginPosition", "2018")
  s <- datapond:::validate_iso_export_semantics(d)
  expect_true(s$valid)
  expect_true(any(grepl("overlap", s$warnings$reason)))
  set(d, "//gml:beginPosition", "2018-01-01T10:00:00+02:00")
  set(d, "//gml:endPosition", "2018-01-01T08:30:00Z")
  expect_true(datapond:::validate_iso_export_semantics(d)$valid)
  set(d, "//gml:endPosition", "2018-01-01T07:30:00Z")
  expect_true(any(datapond:::validate_iso_export_semantics(d)$issues$rule == "SEM-ORDER"))
  set(d, "//gml:endPosition", "2018-01-01T07:30:00")
  s <- datapond:::validate_iso_export_semantics(d)
  expect_true(s$valid)
  expect_true(any(grepl("zoned and unzoned", s$warnings$reason)))
})

test_that("internal XML references and invalid sample ranges cannot escape semantic checks", {
  core_validation_engine()
  f <- semantic_fixture("raster"); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  d <- datapond:::build_iso_metadata_candidate_xml(1, f$path)
  ns <- xml2::xml_ns(d)
  node <- xml2::xml_find_first(d, "//mrc:MD_SampleDimension/mrc:maxValue/*", ns)
  xml2::xml_set_text(node, "-1")
  expect_true(any(datapond:::validate_iso_export_semantics(d)$issues$rule == "SEM-BOUNDS"))
  xml2::xml_set_text(node, "NaN")
  expect_false(datapond:::validate_iso_export_semantics(d)$valid)
  xml2::xml_set_text(node, "10")
  id_node <- xml2::xml_find_first(d, "//gml:TimePeriod", ns)
  link <- xml2::xml_add_child(xml2::xml_root(d), "mdb:parentMetadata")
  xml2::xml_set_attr(link, "xlink:href", "#missing")
  expect_true(any(datapond:::validate_iso_export_semantics(d)$issues$rule == "SEM-LINK"))
  xml2::xml_set_attr(link, "xlink:href", paste0("#", xml2::xml_attr(id_node, "id")))
  # Explicitly duplicate a normal id; XSD ID typing is not assumed.
  xml2::xml_set_attr(link, "id", "duplicate")
  xml2::xml_set_attr(id_node, "id", "duplicate")
  expect_true(any(datapond:::validate_iso_export_semantics(d)$issues$rule == "SEM-ID"))
})

test_that("missing engines and altered semantic resources are unavailable and block export", {
  core_validation_engine()
  f <- semantic_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  a <- datapond:::assess_iso_metadata_package(1, f$path)
  expect_identical(datapond:::validate_iso_export_semantics(a$document, python = "")$status, "unavailable")
  bundle <- tempfile(); dir.create(bundle)
  on.exit(unlink(bundle, recursive = TRUE), add = TRUE)
  original <- datapond:::iso_validation_resource("extdata", "iso_semantics", "1.0.0")
  file.copy(list.files(original, full.names = TRUE), bundle)
  writeLines("Changed registry", file.path(bundle, "iso-codelists.xml"))
  a$semantics <- datapond:::validate_iso_export_semantics(a$document, bundle = bundle)
  expect_identical(a$semantics$status, "unavailable")
  expect_match(a$semantics$issues$message, "checksum")
  output <- tempfile(); dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  expect_error(datapond:::iso_publish_metadata_package(a, output, "datapond"), "ISO export was not written")
  expect_length(list.files(output), 0L)
})

test_that("saved mixed-precision temporal errors block both policies without changing the record", {
  local_trusted_access_fixture()

  core_validation_engine()
  f <- semantic_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_temporal_extent_tbl SET start_value='2020',start_precision='year',
    end_value='2019-12',end_precision='month'")
  a <- datapond:::assess_iso_metadata_package(1, f$path)
  expect_false(a$semantics$valid)
  expect_true(any(a$semantics$issues$rule == "SEM-ORDER"))
  output <- tempfile(); dir.create(output); on.exit(unlink(output, recursive = TRUE), add = TRUE)
  for (policy in c("pinned", "datapond"))
    expect_error(datapond:::iso_publish_metadata_package(a, output, policy), "starts later")
  expect_identical(DBI::dbGetQuery(conn, "SELECT start_value FROM dataset_temporal_extent_tbl")$start_value, "2020")
  state <- shiny::reactiveValues(data_pond = f$path, current_dataset_id = 1L)
  shiny::testServer(datapond:::mod_iso_export_server, args = list(state = state, open_section = function(...) NULL), {
    session$flushReact()
    session$setInputs(assess = 1L, policy = "datapond")
    expect_match(output$downloads$html, "blocked")
    expect_match(output$report$html, "Export semantics / code lists")
    expect_match(output$report$html, "Open Temporal extents")
  })
})
