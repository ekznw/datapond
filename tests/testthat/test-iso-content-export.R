content_fixture <- function(kind = "vector") {
  fixture <- distribution_fixture(kind)
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='eng', resource_character_encoding='utf8'")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET data_field_alias='Measured value',
    data_field_units='m', min_value='-01.50', max_value='120.00', missing_count=4, value_count=100,
    example_values='1;2;3', profile_updated_at='2026-10-01 12:00:00'")
  DBI::dbDisconnect(conn)
  datapond:::save_dataset_feature_catalogue(fixture$dataset_id, list(title = "Documented fixture attributes",
    version = "2.1", version_date = "2026-10-01", language_code = "eng", producer_organisation_id = 1L), fixture$path)
  datapond:::replace_dataset_quality_results(fixture$dataset_id, data.frame(
    measure_identifier = "REVIEW-01", measure_type_code = "logicalConsistency", quality_element_code = "DQ_ConceptualConsistency",
    evaluation_method = "Reviewed saved field descriptions.", result_type_code = "descriptive",
    explanation = "Descriptions match the data dictionary."), fixture$path)
  fixture
}

test_that("populated vector and raster content passes with distribution, lineage and quality", {
  core_validation_engine()
  for (kind in c("vector", "raster")) {
    fixture <- content_fixture(kind)
    for (scope in c("content", "all")) {
      result <- datapond:::assess_iso_metadata_export(fixture$dataset_id, fixture$path, sections = scope)
      expect_true(result$validation$valid, info = paste(result$validation$issues$message, collapse = "\n"))
      xml <- result$document; ns <- xml2::xml_ns(xml)
      expect_length(xml2::xml_find_all(xml, "//mrl:LI_Lineage", ns), 1L)
      expect_length(xml2::xml_find_all(xml, "//mdq:DQ_DataQuality", ns), 1L)
      expect_length(xml2::xml_find_all(xml, "//mrd:MD_Distribution", ns), 1L)
      expect_length(xml2::xml_find_all(xml, "//mrc:complianceCode|//mrc:includedWithDataset", ns), 0L)
      rows <- datapond:::iso_content_attributes(fixture$dataset_id, fixture$path, coverage = kind == "raster")
      if (kind == "vector") {
        fc <- result$feature_catalogue
        expect_true(result$validation$feature_catalogue$valid)
        expect_identical(core_xpath_text(fc, "//gfc:memberName"), rows$data_field_name)
        expect_identical(core_xpath_text(fc, "//gfc:FC_FeatureAttribute/gfc:definition/*"), rows$data_field_desc)
        expect_identical(core_xpath_text(fc, "//gfc:designation/*"), rows$data_field_alias)
        expect_identical(core_xpath_text(fc, "//gfc:valueType/gco:TypeName/gco:aName/*"), rows$attribute_type)
        expect_identical(core_xpath_text(fc, "//gfc:valueMeasurementUnit/gml:name"), rows$data_field_units)
        expect_identical(core_xpath_text(fc, "//gfc:cardinality/*"), character())
        expect_true(any(grepl("cardinality", result$validation$feature_catalogue$warnings$location)))
        expect_length(xml2::xml_find_all(fc, "//gfc:listedValue", xml2::xml_ns(fc)), 0L)
        expect_identical(as.character(fc), as.character(datapond:::build_iso_19110_feature_catalogue_xml(fixture$dataset_id, fixture$path)))
      } else {
        expect_null(result$feature_catalogue)
        expect_identical(core_xpath_text(xml, "//mrc:sequenceIdentifier/gco:MemberName/gco:aName/*"), rows$data_field_name)
        expect_identical(core_xpath_text(xml, "//mrc:description/*"), rows$data_field_desc)
        expect_identical(core_xpath_text(xml, "//mrc:name/mcc:MD_Identifier/mcc:code/*"), rows$data_field_alias)
        expect_identical(core_xpath_text(xml, "//mrc:minValue/gco:Real"), rows$min_value)
        expect_identical(core_xpath_text(xml, "//mrc:maxValue/gco:Real"), rows$max_value)
        expect_identical(core_xpath_text(xml, "//mrc:units/gml:UnitDefinition/gml:name"), rows$data_field_units)
        expect_identical(core_xpath_text(xml, "//mrc:otherPropertyType//gco:fieldName/*"),
                         c("data_file", "value_count", "missing_count", "profile_updated_at"))
        expect_identical(core_xpath_text(xml, "//mrc:otherProperty//gco:value/*"),
                         c(rows$feature_type_name, as.character(rows$value_count), as.character(rows$missing_count), rows$profile_updated_at))
        expect_true(any(grepl("contentType", result$validation$warnings$location)))
      }
    }
    unlink(fixture$path, recursive = TRUE)
  }
})

test_that("tabular catalogues validate independently without inventing a spatial extent", {
  core_validation_engine()
  f <- content_fixture("tabular"); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  fc <- datapond:::build_iso_19110_feature_catalogue_xml(f$dataset_id, f$path)
  report <- datapond:::validate_iso_metadata_xml(fc, kind = "catalogue")
  expect_true(report$valid, info = paste(report$issues$message, collapse = "\n"))
  result <- datapond:::assess_iso_metadata_export(f$dataset_id, f$path, sections = "content")
  expect_identical(result$validation$status, "not-built")
})

test_that("catalogue XSD, local references and metadata links all gate pair validity", {
  core_validation_engine()
  f <- content_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  a <- datapond:::assess_iso_metadata_export(f$dataset_id, f$path, sections = "content")
  expect_true(a$validation$valid)
  check <- function(doc = a$document, fc = a$feature_catalogue, filename = a$feature_catalogue_filename)
    datapond:::validate_iso_content_documents(doc, fc, filename)
  expect_false(check(fc = NULL)$valid)
  expect_false(check(filename = "wrong.xml")$valid)
  fc <- xml2::read_xml(as.character(a$feature_catalogue))
  xml2::xml_remove(xml2::xml_find_first(fc, "//gfc:cardinality", xml2::xml_ns(fc)))
  invalid <- check(fc = fc)
  expect_false(invalid$valid)
  expect_true(any(invalid$issues$stage == "catalogue-xsd"))
  fc <- xml2::read_xml(as.character(a$feature_catalogue))
  ref <- xml2::xml_find_first(fc, "//gfc:FC_FeatureType/gfc:featureCatalogue", xml2::xml_ns(fc))
  xml2::xml_set_attr(ref, "xlink:href", "#missing")
  expect_true(any(check(fc = fc)$issues$stage == "catalogue-catalogue-references"))
  xml2::xml_set_attr(ref, "xlink:href", "#feature-1")
  expect_false(check(fc = fc)$valid) # Existing but wrong object is not a valid catalogue backlink.
  fc <- xml2::read_xml(as.character(a$feature_catalogue))
  xml2::xml_set_attr(xml2::xml_root(fc), "uuid", "stale-identifier")
  expect_false(check(fc = fc)$valid)
  fc <- xml2::read_xml(as.character(a$feature_catalogue))
  xml2::xml_set_text(xml2::xml_find_first(fc, "//cat:name/*", xml2::xml_ns(fc)), "Changed title")
  expect_false(check(fc = fc)$valid)
  unavailable <- datapond:::validate_iso_content_documents(a$document, a$feature_catalogue,
    a$feature_catalogue_filename, python = "")
  expect_identical(unavailable$status, "unavailable")
})

test_that("content groups preserve file and layer boundaries and exclude inactive files", {
  core_validation_engine()
  f <- content_fixture("raster"); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl(dataset_id,data_file_rel_path_full,file_name,data_file_format,active)
    VALUES (1,'other/elevation.tif','elevation.tif','raster',1), (1,'data/retired.tif','retired.tif','raster',0)")
  DBI::dbExecute(conn, "INSERT INTO dataset_attribute_tbl(dataset_id,data_file_id,data_file_rel_path_full,data_field_name,layer_name,attribute_type,min_value,max_value)
    VALUES (1,NULL,'other/elevation.tif','second','same','float32','0','2'),
           (1,1,'data/elevation.tif','third','same','float32','1','3'),
           (1,3,'data/retired.tif','retired',NULL,'float32','4','5')")
  rows <- datapond:::iso_content_attributes(1,f$path,TRUE)
  expect_setequal(rows$data_field_name, c("value","second","third"))
  expect_identical(rows$data_file_id[rows$data_field_name == "second"], 2L)
  xml <- datapond:::build_iso_content_metadata_xml(1,f$path)
  expect_length(xml2::xml_find_all(xml, "//mrc:MD_CoverageDescription", xml2::xml_ns(xml)), 3L)
  DBI::dbExecute(conn, "UPDATE data_file_tbl SET data_file_format='vector' WHERE id=2")
  mixed <- datapond:::assess_iso_metadata_export(1,f$path)
  expect_true(mixed$validation$valid, info=paste(mixed$validation$issues$message,collapse="\n"))
  expect_length(xml2::xml_find_all(mixed$document,"//mrc:MD_CoverageDescription",xml2::xml_ns(mixed$document)),2L)
  expect_identical(core_xpath_text(mixed$feature_catalogue,"//gfc:typeName"),"other/elevation.tif#layer=same")
  # A missing or cross-dataset file link cannot turn a raster into a vector field.
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(uuid,title) VALUES ('other-dataset','Other')")
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl(dataset_id,data_file_rel_path_full,file_name,data_file_format) VALUES (2,'foreign.tif','foreign.tif','raster')")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET data_file_id=4 WHERE data_field_name='second'")
  expect_error(datapond:::build_iso_content_metadata_xml(1,f$path), "link it to one data file")
})

test_that("missing catalogue, malformed statistics and unsafe filenames are actionable", {
  f <- content_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_feature_catalogue_tbl SET catalogue_uuid='../escape'")
  expect_error(datapond:::build_iso_19110_feature_catalogue_xml(1,f$path), "filename-safe")
  DBI::dbExecute(conn, "DELETE FROM dataset_feature_catalogue_tbl")
  a <- datapond:::assess_iso_metadata_export(1,f$path,sections="content")
  expect_identical(a$validation$status, "not-built")
  expect_match(a$validation$issues$message, "Save the feature catalogue identity")
  DBI::dbExecute(conn, "UPDATE data_file_tbl SET data_file_format='raster'")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='not numeric'")
  expect_error(datapond:::build_iso_content_metadata_xml(1,f$path), "finite numeric")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='999'")
  expect_error(datapond:::build_iso_content_metadata_xml(1,f$path), "minimum exceeds")
})

test_that("missing raster statistics and dataset language remain visible validation gaps", {
  core_validation_engine()
  f <- content_fixture("raster"); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value=NULL,max_value=NULL")
  a <- datapond:::assess_iso_metadata_export(1,f$path,sections="content")
  expect_false(a$validation$valid)
  expect_true(any(grepl("sample dimension",a$validation$issues$message)))
  DBI::dbExecute(conn, "UPDATE data_file_tbl SET data_file_format='vector'")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code=NULL")
  a <- datapond:::assess_iso_metadata_export(1,f$path,sections="content")
  expect_false(a$validation$valid)
  expect_true(any(grepl("Resource language",a$validation$issues$message)))
})

test_that("writer validates before publishing either document and successful links resolve", {
  core_validation_engine()
  f <- content_fixture(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  iso_package_fixture_files(f)
  output <- tempfile("content-export-"); dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  paths <- datapond:::write_iso_wave3_documents(1,f$path,output)
  expect_true(all(file.exists(paths)))
  expect_true(datapond:::validate_iso_content_documents(xml2::read_xml(paths[["metadata"]]),
    xml2::read_xml(paths[["feature_catalogue"]]), basename(paths[["feature_catalogue"]]))$valid)
  before <- tools::md5sum(paths)
  conn <- datapond:::get_conn(f$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code=NULL")
  DBI::dbDisconnect(conn)
  expect_error(datapond:::write_iso_wave3_documents(1,f$path,output), "ISO export was not written")
  expect_identical(tools::md5sum(paths), before)
})

test_that("dataset locale is optional, distinct from metadata locale, and clearable", {
  f <- iso_export_candidate_pond(); on.exit(unlink(f$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id=1")
  expect_true(is.na(record$resource_language_code))
  expect_true(is.na(record$resource_character_encoding))
  profile <- as.list(record[c("metadata_uuid", "metadata_resource_scope_code", "metadata_language_code", "metadata_character_encoding")])
  save <- function(value) datapond:::save_iso_profile_sections(1,value,"environment",
    datapond:::get_dataset_citation_dates(1,f$path)[c("date_value","date_type_code","date_precision","date_note")], datapond:::get_metadata_responsibilities(1,f$path),
    "2026-10-01",f$path)
  save(c(profile, list(resource_language_code="zul",resource_character_encoding="utf8")))
  save(profile) # Older callers do not erase the additional fields.
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id=1")
  expect_identical(record$resource_language_code,"zul")
  expect_identical(record$metadata_language_code,"eng")
  save(c(profile,list(resource_language_code="",resource_character_encoding="")))
  expect_true(is.na(DBI::dbGetQuery(conn,"SELECT resource_language_code FROM entry_tbl")$resource_language_code))
  expect_error(save(c(profile,list(resource_language_code="invalid"))),"Unknown dataset language")
})
