# ISO raster content and feature catalogue serialization -------------------

# Export selection is stricter than the UI/status preview: resolve legacy file
# paths within this dataset, exclude inactive files and refuse ambiguous links.
iso_content_attributes <- function(dataset_id, base_path, coverage = FALSE) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  rows <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_attribute_tbl WHERE dataset_id = ?
    ORDER BY COALESCE(attribute_order, id), id", params = list(as.integer(dataset_id)))
  files <- DBI::dbGetQuery(conn, "SELECT * FROM data_file_tbl WHERE dataset_id = ?",
                          params = list(as.integer(dataset_id)))
  rows$feature_type_name <- rows$data_file_format <- rep(NA_character_, nrow(rows))
  keep <- rep(FALSE, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    matches <- if (!is.na(rows$data_file_id[i])) which(files$id == rows$data_file_id[i]) else
      which(!is.na(files$data_file_rel_path_full) & non_empty_scalar(rows$data_file_rel_path_full[i]) &
              files$data_file_rel_path_full == rows$data_file_rel_path_full[i])
    if (length(matches) != 1L) stop(paste0("Attribute ", rows$id[i],
      ": link it to one data file in this dataset before ISO content export."), call. = FALSE)
    file <- files[matches, , drop = FALSE]
    if (!is.na(file$active[1]) && file$active[1] == 0L) next
    format <- tolower(trimws(file$data_file_format[1]))
    if (!non_empty_scalar(format)) stop(paste0("Data file ", file$id[1],
      ": record its format before ISO content export."), call. = FALSE)
    raster <- format %in% c("raster", "virtual_raster", "esri_arcinfo_grid")
    keep[i] <- identical(raster, isTRUE(coverage))
    rows$data_file_id[i] <- file$id[1]
    rows$data_file_format[i] <- format
    rows$feature_type_name[i] <- wave3_text_default(file$data_file_rel_path_full[1], file$file_name[1])
    if (!non_empty_scalar(rows$data_field_name[i])) stop("An exported attribute has no field name.", call. = FALSE)
  }
  rows[keep, , drop = FALSE]
}

iso_content_groups <- function(rows) {
  if (!nrow(rows)) return(list())
  # Numeric file IDs and length-prefixed layer names avoid delimiter collisions.
  layer <- ifelse(is.na(rows$layer_name), "", rows$layer_name)
  key <- paste0(rows$data_file_id, ":", nchar(layer), ":", layer)
  lapply(unique(key), function(value) rows[key == value, , drop = FALSE])
}

iso_content_required <- function(parent, property, value, type = "gco:CharacterString") {
  node <- xml2::xml_add_child(parent, property)
  if (non_empty_scalar(value)) xml2::xml_add_child(node, type, as.character(value)) else
    xml2::xml_set_attr(node, "gco:nilReason", "unknown")
  invisible(node)
}

iso_content_type_name <- function(parent, property, value) {
  node <- xml2::xml_add_child(parent, property)
  if (non_empty_scalar(value)) {
    type <- xml2::xml_add_child(node, "gco:TypeName")
    iso_profile_add_text(type, "gco:aName", value)
  } else xml2::xml_set_attr(node, "gco:nilReason", "unknown")
  invisible(node)
}

iso_content_identifier <- function(parent, property, value) {
  if (!non_empty_scalar(value)) return(invisible(NULL))
  identifier <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "mcc:MD_Identifier")
  iso_profile_add_text(identifier, "mcc:code", value)
  invisible(identifier)
}

iso_content_unit <- function(parent, property, unit, id, direct = FALSE) {
  node <- xml2::xml_add_child(parent, property)
  if (!non_empty_scalar(unit)) {
    xml2::xml_set_attr(node, "gco:nilReason", "unknown")
  } else if (!direct && wave2_valid_uri(unit)) {
    xml2::xml_set_attr(node, "xlink:href", unit)
  } else {
    definition <- if (direct) node else xml2::xml_add_child(node, "gml:UnitDefinition")
    xml2::xml_set_attr(definition, "gml:id", paste0("datapond-content-unit-", id))
    identifier <- xml2::xml_add_child(definition, "gml:identifier", unit)
    xml2::xml_set_attr(identifier, "codeSpace", if (wave2_valid_uri(unit)) "urn:ietf:rfc:3986" else "urn:datapond:unit-label")
    xml2::xml_add_child(definition, "gml:name", unit)
  }
  invisible(node)
}

iso_content_record_type <- function(parent, property, names, types) {
  record <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "gco:RecordType")
  for (i in seq_along(names)) {
    field <- xml2::xml_add_child(xml2::xml_add_child(record, "gco:fieldType"), "gco:FieldType")
    iso_profile_add_text(field, "gco:fieldName", names[i])
    iso_content_type_name(field, "gco:fieldType", types[i])
  }
  invisible(record)
}

iso_content_catalogue <- function(dataset_id, base_path) {
  catalogue <- get_dataset_feature_catalogue(dataset_id, base_path)
  if (nrow(catalogue) != 1L) stop(
    "Save the feature catalogue identity before exporting vector/tabular attributes.", call. = FALSE)
  uuid <- catalogue$catalogue_uuid[1]
  if (!non_empty_scalar(uuid) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", uuid))
    stop("Feature catalogue identifier must be a filename-safe identifier.", call. = FALSE)
  if (!non_empty_scalar(catalogue$title[1])) stop("Feature catalogue needs a title.", call. = FALSE)
  catalogue
}

#' Build an ISO 19110 feature catalogue from saved vector/tabular attributes
#'
#' Uses the pinned GFC 2.2 schema and a saved catalogue identity. Uncaptured
#' cardinality and other required unknown values carry explicit nil reasons.
#' Profile examples and statistics are not asserted to be permitted values.
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An `xml_document`, or `NULL` when no eligible active attributes exist.
#' @keywords internal
build_iso_19110_feature_catalogue_xml <- function(dataset_id, base_path) {
  attributes <- iso_content_attributes(dataset_id, base_path)
  if (!nrow(attributes)) return(NULL)
  catalogue <- iso_content_catalogue(dataset_id, base_path)
  ns <- iso_profile_namespaces()
  document <- xml2::xml_new_root("gfc:FC_FeatureCatalogue",
    `xmlns:gfc` = iso_wave3_namespaces()[["gfc"]],
    `xmlns:cat` = "https://schemas.isotc211.org/19139/-/cat/1.2",
    `xmlns:gco` = ns[["gco"]], `xmlns:cit` = ns[["cit"]], `xmlns:mcc` = ns[["mcc"]],
    `xmlns:gml` = iso_wave3_namespaces()[["gml"]], `xmlns:xlink` = iso_wave3_namespaces()[["xlink"]],
    id = paste0("fc-", catalogue$catalogue_uuid[1]), uuid = catalogue$catalogue_uuid[1])
  root <- xml2::xml_root(document)
  iso_profile_add_text(root, "cat:name", catalogue$title[1])
  iso_profile_add_text(root, "cat:scope", "Saved attributes of active vector and tabular data files")
  iso_content_required(root, "cat:versionNumber", catalogue$version[1])
  iso_content_required(root, "cat:versionDate", catalogue$version_date[1], "gco:Date")
  iso_profile_add_text(root, "cat:language", catalogue$language_code[1])
  if (is.na(catalogue$producer_person_id[1]) && is.na(catalogue$producer_organisation_id[1])) {
    iso_content_required(root, "gfc:producer", NA_character_)
  } else {
    iso_contact_add_responsibility(root, "gfc:producer", catalogue$producer_person_id[1],
      catalogue$producer_organisation_id[1], "author", base_path)
  }
  iso_content_identifier(root, "gfc:identifier", catalogue$catalogue_uuid[1])
  groups <- iso_content_groups(attributes)
  for (index in seq_along(groups)) {
    rows <- groups[[index]]
    feature <- xml2::xml_add_child(xml2::xml_add_child(root, "gfc:featureType"), "gfc:FC_FeatureType")
    feature_id <- paste0("feature-", index)
    xml2::xml_set_attr(feature, "id", feature_id)
    type_name <- rows$feature_type_name[1]
    if (non_empty_scalar(rows$layer_name[1])) type_name <- paste0(type_name, "#layer=", utils::URLencode(rows$layer_name[1], reserved = TRUE))
    xml2::xml_add_child(feature, "gfc:typeName", type_name)
    # A file/layer name is not a semantic definition of a feature type.
    iso_content_required(feature, "gfc:definition", NA_character_)
    iso_content_required(feature, "gfc:isAbstract", "false", "gco:Boolean")
    for (i in seq_len(nrow(rows))) {
      attribute <- xml2::xml_add_child(xml2::xml_add_child(feature, "gfc:carrierOfCharacteristics"), "gfc:FC_FeatureAttribute")
      xml2::xml_set_attr(attribute, "id", paste0("attribute-", rows$id[i]))
      xml2::xml_set_attr(xml2::xml_add_child(attribute, "gfc:featureType"), "xlink:href", paste0("#", feature_id))
      xml2::xml_add_child(attribute, "gfc:memberName", rows$data_field_name[i])
      iso_content_required(attribute, "gfc:definition", rows$data_field_desc[i])
      iso_content_required(attribute, "gfc:cardinality", NA_character_)
      iso_profile_add_text(attribute, "gfc:designation", rows$data_field_alias[i])
      if (non_empty_scalar(rows$data_field_units[i])) iso_content_unit(attribute,
        "gfc:valueMeasurementUnit", rows$data_field_units[i], rows$id[i], direct = TRUE)
      iso_content_type_name(attribute, "gfc:valueType", rows$attribute_type[i])
    }
    xml2::xml_set_attr(xml2::xml_add_child(feature, "gfc:featureCatalogue"), "xlink:href", paste0("#fc-", catalogue$catalogue_uuid[1]))
  }
  document
}

iso_wave3_add_coverage_content <- function(root, dataset_id, base_path) {
  rows <- iso_content_attributes(dataset_id, base_path, coverage = TRUE)
  for (bands in iso_content_groups(rows)) {
    coverage <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:contentInfo"), "mrc:MD_CoverageDescription")
    xml2::xml_set_attr(coverage, "id", paste0("coverage-", bands$id[1]))
    iso_content_record_type(coverage, "mrc:attributeDescription", bands$data_field_name, bands$attribute_type)
    group <- xml2::xml_add_child(xml2::xml_add_child(coverage, "mrc:attributeGroup"), "mrc:MD_AttributeGroup")
    iso_content_required(group, "mrc:contentType", NA_character_)
    for (i in seq_len(nrow(bands))) {
      dimension <- xml2::xml_add_child(xml2::xml_add_child(group, "mrc:attribute"), "mrc:MD_SampleDimension")
      member <- xml2::xml_add_child(xml2::xml_add_child(dimension, "mrc:sequenceIdentifier"), "gco:MemberName")
      iso_profile_add_text(member, "gco:aName", bands$data_field_name[i])
      iso_content_type_name(member, "gco:attributeType", bands$attribute_type[i])
      iso_profile_add_text(dimension, "mrc:description", bands$data_field_desc[i])
      iso_content_identifier(dimension, "mrc:name", bands$data_field_alias[i])
      for (field in c("max_value", "min_value")) {
        value <- bands[[field]][i]
        if (!non_empty_scalar(value)) next
        if (!grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$", trimws(value)) ||
            !is.finite(suppressWarnings(as.numeric(value)))) stop(paste0("Raster attribute ", bands$id[i],
              ": ", field, " is not a finite numeric value."), call. = FALSE)
        iso_content_required(dimension, if (field == "max_value") "mrc:maxValue" else "mrc:minValue", trimws(value), "gco:Real")
      }
      if (non_empty_scalar(bands$min_value[i]) && non_empty_scalar(bands$max_value[i]) &&
          as.numeric(bands$min_value[i]) > as.numeric(bands$max_value[i])) stop("Raster minimum exceeds maximum.", call. = FALSE)
      if (any(vapply(c(bands$data_field_units[i], bands$min_value[i], bands$max_value[i]), non_empty_scalar, logical(1))))
        iso_content_unit(dimension, "mrc:units", bands$data_field_units[i], bands$id[i])
      # Named profiling evidence retains counts without calling them NoData codes
      # or inferring whether value_count includes/excludes missing observations.
      values <- c(data_file = bands$feature_type_name[i], layer = bands$layer_name[i],
                  value_count = as.character(bands$value_count[i]), missing_count = as.character(bands$missing_count[i]),
                  profile_updated_at = bands$profile_updated_at[i])
      values <- values[vapply(values, non_empty_scalar, logical(1))]
      iso_content_record_type(dimension, "mrc:otherPropertyType", names(values), rep("CharacterString", length(values)))
      record <- xml2::xml_add_child(xml2::xml_add_child(dimension, "mrc:otherProperty"), "gco:Record")
      for (value in values) {
        field <- xml2::xml_add_child(xml2::xml_add_child(record, "gco:field"), "gco:Field")
        iso_profile_add_text(field, "gco:value", value)
      }
    }
  }
  invisible(root)
}

add_iso_content_xml <- function(document, dataset_id, base_path) {
  iso_wave3_ensure_namespaces(document)
  root <- xml2::xml_root(document)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  resource <- DBI::dbGetQuery(conn, "SELECT resource_language_code, resource_character_encoding FROM entry_tbl WHERE id = ?",
                              params = list(as.integer(dataset_id)))
  language <- resource$resource_language_code[1]
  if (non_empty_scalar(language)) {
    identification <- xml2::xml_find_first(document, "//mri:MD_DataIdentification", xml2::xml_ns(document))
    locale <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:defaultLocale"), "lan:PT_Locale")
    iso_profile_add_code(locale, "lan:language", "lan:LanguageCode", language,
      "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#LanguageCode")
    if (non_empty_scalar(resource$resource_character_encoding[1]))
      iso_profile_add_code(locale, "lan:characterEncoding", "lan:MD_CharacterSetCode", resource$resource_character_encoding[1],
        "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#MD_CharacterSetCode") else
      iso_content_required(locale, "lan:characterEncoding", NA_character_)
  }
  if (nrow(iso_content_attributes(dataset_id, base_path))) {
    catalogue <- iso_content_catalogue(dataset_id, base_path)
    description <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:contentInfo"), "mrc:MD_FeatureCatalogueDescription")
    citation <- xml2::xml_add_child(xml2::xml_add_child(description, "mrc:featureCatalogueCitation"), "cit:CI_Citation")
    iso_profile_add_text(citation, "cit:title", catalogue$title[1])
    iso_wave3_add_citation_date(citation, catalogue$version_date[1], "revision")
    iso_profile_add_text(citation, "cit:edition", catalogue$version[1])
    iso_content_identifier(citation, "cit:identifier", catalogue$catalogue_uuid[1])
    online <- xml2::xml_add_child(xml2::xml_add_child(citation, "cit:onlineResource"), "cit:CI_OnlineResource")
    iso_profile_add_text(online, "cit:linkage", paste0("feature-catalogue-", catalogue$catalogue_uuid[1], ".xml"))
  }
  iso_wave3_add_coverage_content(root, dataset_id, base_path)
  iso_core_order_metadata(document)
}

#' Build core, distribution, quality and content metadata for assessment
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An xml2 metadata document; assess the linked catalogue separately.
#' @keywords internal
build_iso_content_metadata_xml <- function(dataset_id, base_path) {
  add_iso_content_xml(build_iso_quality_metadata_xml(dataset_id, base_path), dataset_id, base_path)
}
