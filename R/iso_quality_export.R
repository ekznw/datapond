# ISO 19157 MDQ 1.2.0 result serialization --------------------------------

iso_quality_export_row <- function(row, conn, dataset_id) {
  value <- function(field) licensing_data_value(row, field)
  fail <- function(message) stop(paste0("Quality result ", value("measure_identifier"), ": ", message), call. = FALSE)
  elements <- iso_quality_elements()
  element <- value("quality_element_code")
  if (is.na(element)) fail("choose a specific test in Measure type before ISO export; the broad category alone is ambiguous.")
  index <- match(element, elements$code)
  if (is.na(index)) fail("unknown ISO quality element.")
  if (!identical(value("measure_type_code"), elements$category[index])) fail("quality element does not match the measure category.")
  if (is.na(value("measure_identifier")) || is.na(value("evaluation_method"))) fail("a measure identifier and evaluation method are required.")
  if (!value("scope_code") %in% iso_quality_scope_codes()) fail("unknown scope.")
  type <- value("result_type_code")
  if (!type %in% iso_quality_result_type_codes()) fail("unknown result type.")
  if (type == "quantitative" && is.na(value("result_value"))) fail("quantitative result has no value.")
  if (type == "conformance" && (is.na(value("conformance_specification")) ||
      is.na(value("pass")) || !value("pass") %in% c(0, 1))) fail("conformance needs a specification and an explicit pass or fail.")
  if (type == "descriptive" && is.na(value("explanation"))) fail("descriptive result has no explanation.")
  inactive_fields <- if (type == "quantitative") c("conformance_specification", "pass") else if (type == "conformance")
    c("result_value", "result_unit") else c("result_value", "result_unit", "conformance_specification", "pass")
  if (any(vapply(inactive_fields, function(field) !is.na(value(field)), logical(1))))
    fail("values for another result type are still populated; review them before export.")
  date <- if (is.na(value("evaluation_date"))) NULL else iso_export_date(value("evaluation_date"), "Quality evaluation date")
  file_id <- value("data_file_id")
  attribute_id <- value("attribute_id")
  attribute <- NULL
  if (!is.na(attribute_id)) {
    attribute <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_attribute_tbl WHERE id = ? AND dataset_id = ?",
                                params = list(as.integer(attribute_id), as.integer(dataset_id)))
    if (nrow(attribute) != 1L) fail("linked attribute does not belong to this dataset.")
    if (!is.na(file_id) && !is.na(attribute$data_file_id[1]) && as.integer(file_id) != attribute$data_file_id[1])
      fail("linked file and attribute disagree.")
    if (is.na(file_id)) file_id <- attribute$data_file_id[1]
    if (!non_empty_scalar(attribute$data_field_name[1])) fail("linked attribute has no field name.")
  }
  if (value("scope_code") == "attribute" && is.null(attribute)) fail("attribute scope requires a linked attribute.")
  scope_text <- character()
  if (!is.na(file_id)) {
    file <- DBI::dbGetQuery(conn, "SELECT * FROM data_file_tbl WHERE id = ? AND dataset_id = ?",
                           params = list(as.integer(file_id), as.integer(dataset_id)))
    if (nrow(file) != 1L) fail("linked file does not belong to this dataset.")
    filename <- licensing_data_value(file, "file_name")
    if (is.na(filename)) filename <- licensing_data_value(file, "data_file_rel_path_full")
    if (is.na(filename)) fail("linked file has no name.")
    scope_text <- paste0("Data file: ", basename(filename))
  }
  if (!is.null(attribute)) {
    layer <- licensing_data_value(attribute, "layer_name")
    scope_text <- c(scope_text, if (!is.na(layer)) paste0("Layer: ", layer),
                    paste0("Attribute: ", attribute$data_field_name[1]))
  }
  if (value("scope_code") == "featureType" && !length(scope_text))
    fail("feature-type scope needs an explicit linked file or attribute to identify its coverage.")
  list(row = row, element = element, date = date, scope_text = paste(scope_text, collapse = "; "))
}

iso_quality_add_unit <- function(result, unit, id) {
  if (!non_empty_scalar(unit)) return(invisible(NULL))
  property <- xml2::xml_add_child(result, "mdq:valueUnit")
  if (wave2_valid_uri(unit)) {
    xml2::xml_set_attr(property, "xlink:href", unit)
  } else {
    # Retain the recorded label as a local unit definition; no guessed UCUM
    # identifier, conversion factor or physical quantity is asserted.
    definition <- xml2::xml_add_child(property, "gml:UnitDefinition")
    xml2::xml_set_attr(definition, "gml:id", paste0("datapond-quality-unit-", id))
    identifier <- xml2::xml_add_child(definition, "gml:identifier", unit)
    xml2::xml_set_attr(identifier, "codeSpace", "urn:datapond:unit-label")
    xml2::xml_add_child(definition, "gml:name", unit)
  }
  invisible(property)
}

iso_wave2_add_quality_result <- function(root, checked) {
  row <- checked$row
  info <- xml2::xml_add_child(root, "mdb:dataQualityInfo")
  quality <- xml2::xml_add_child(info, "mdq:DQ_DataQuality")
  scope <- xml2::xml_add_child(xml2::xml_add_child(quality, "mdq:scope"), "mcc:MD_Scope")
  iso_profile_add_code(scope, "mcc:level", "mcc:MD_ScopeCode", row$scope_code,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#MD_ScopeCode")
  if (nzchar(checked$scope_text)) {
    description <- xml2::xml_add_child(xml2::xml_add_child(scope, "mcc:levelDescription"), "mcc:MD_ScopeDescription")
    property <- if (row$scope_code == "attribute") "mcc:attributes" else if (row$scope_code == "featureType") "mcc:features" else "mcc:other"
    iso_profile_add_text(description, property, checked$scope_text)
  }
  report <- xml2::xml_add_child(xml2::xml_add_child(quality, "mdq:report"), paste0("mdq:", checked$element))
  measure <- xml2::xml_add_child(xml2::xml_add_child(report, "mdq:measure"), "mdq:DQ_MeasureReference")
  identifier <- xml2::xml_add_child(xml2::xml_add_child(measure, "mdq:measureIdentification"), "mcc:MD_Identifier")
  iso_profile_add_text(identifier, "mcc:code", row$measure_identifier)
  iso_profile_add_text(measure, "mdq:nameOfMeasure", row$measure_name)
  evaluation <- xml2::xml_add_child(xml2::xml_add_child(report, "mdq:evaluationMethod"), "mdq:DQ_EvaluationMethod")
  method <- row$evaluation_method
  if (!is.null(checked$date)) {
    if (checked$date$precision == "dateTime") {
      xml2::xml_add_child(xml2::xml_add_child(evaluation, "mdq:dateTime"), "gco:DateTime", checked$date$value)
    } else {
      method <- paste0(method, "\nEvaluation date (", checked$date$precision, " precision): ", checked$date$value)
    }
  }
  iso_profile_add_text(evaluation, "mdq:evaluationMethodDescription", method)
  property <- xml2::xml_add_child(report, "mdq:result")
  if (row$result_type_code == "conformance") {
    result <- xml2::xml_add_child(property, "mdq:DQ_ConformanceResult")
    citation <- xml2::xml_add_child(xml2::xml_add_child(result, "mdq:specification"), "cit:CI_Citation")
    iso_profile_add_text(citation, "cit:title", row$conformance_specification)
    iso_profile_add_text(result, "mdq:explanation", row$explanation)
    xml2::xml_add_child(xml2::xml_add_child(result, "mdq:pass"), "gco:Boolean", if (row$pass == 1L) "true" else "false")
  } else if (row$result_type_code == "quantitative") {
    result <- xml2::xml_add_child(property, "mdq:DQ_QuantitativeResult")
    record <- xml2::xml_add_child(xml2::xml_add_child(result, "mdq:value"), "gco:Record")
    field <- xml2::xml_add_child(xml2::xml_add_child(record, "gco:field"), "gco:Field")
    iso_profile_add_text(field, "gco:value", row$result_value)
    iso_quality_add_unit(result, row$result_unit, row$id)
    if (non_empty_scalar(row$explanation)) {
      description <- xml2::xml_add_child(xml2::xml_add_child(report, "mdq:result"), "mdq:DQ_DescriptiveResult")
      iso_profile_add_text(description, "mdq:statement", row$explanation)
    }
  } else {
    result <- xml2::xml_add_child(property, "mdq:DQ_DescriptiveResult")
    iso_profile_add_text(result, "mdq:statement", row$explanation)
  }
  invisible(quality)
}

add_iso_quality_xml <- function(document, dataset_id, base_path) {
  iso_wave2_ensure_namespaces(document)
  xml2::xml_set_attr(xml2::xml_root(document), "xmlns:xlink", "http://www.w3.org/1999/xlink")
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  rows <- get_dataset_quality_results(dataset_id, base_path)
  checked <- lapply(seq_len(nrow(rows)), function(i) iso_quality_export_row(rows[i, , drop = FALSE], conn, dataset_id))
  for (row in checked) iso_wave2_add_quality_result(xml2::xml_root(document), row)
  iso_core_order_metadata(document)
}

#' Build core metadata, lineage, distribution, licensing and quality results
#'
#' Adds only explicitly recorded quality assessments. Broad quality categories
#' need a specific test selection for export. Does not establish full crosswalk
#' coverage, raster content or linked catalogue conformance.
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An xml2 metadata document for validation.
#' @keywords internal
build_iso_quality_metadata_xml <- function(dataset_id, base_path) {
  add_iso_quality_xml(build_iso_distribution_metadata_xml(dataset_id, base_path), dataset_id, base_path)
}
