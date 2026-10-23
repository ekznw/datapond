# ISO 19115-3 profile/discovery serialization for Wave 1B-1D ----

iso_profile_namespaces <- function() {
  c(
    mdb = "https://schemas.isotc211.org/19115/-1/mdb/1.3",
    mcc = "https://schemas.isotc211.org/19115/-1/mcc/1.3",
    lan = "https://schemas.isotc211.org/19115/-1/lan/1.3",
    cit = "https://schemas.isotc211.org/19115/-1/cit/1.3",
    mri = "https://schemas.isotc211.org/19115/-1/mri/1.3",
    mrs = "https://schemas.isotc211.org/19115/-1/mrs/1.3",
    mmi = "https://schemas.isotc211.org/19115/-1/mmi/1.3",
    gex = "https://schemas.isotc211.org/19115/-1/gex/1.3",
    gco = "https://schemas.isotc211.org/19103/-/gco/1.2"
  )
}

iso_profile_add_text <- function(parent, property, value, element = "gco:CharacterString") {
  value <- iso_optional_text(value)
  if (is.na(value)) return(invisible(NULL))
  property_node <- xml2::xml_add_child(parent, property)
  xml2::xml_add_child(property_node, element, value)
}

iso_profile_add_code <- function(parent, property, element, value, code_list) {
  value <- iso_optional_text(value)
  if (is.na(value)) return(invisible(NULL))
  reference <- iso_export_code_reference(element, value, code_list)
  value <- reference$value
  code_list <- reference$code_list
  property_node <- xml2::xml_add_child(parent, property)
  code_node <- xml2::xml_add_child(property_node, element, value)
  xml2::xml_set_attrs(code_node, c(codeList = code_list, codeListValue = value))
  invisible(code_node)
}

validate_iso_profile_export <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  record <- DBI::dbGetQuery(
    conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(as.integer(dataset_id))
  )
  if (nrow(record) != 1L) stop("Dataset record not found.", call. = FALSE)
  topics <- get_dataset_topic_categories(dataset_id, base_path)
  dates <- get_dataset_citation_dates(dataset_id, base_path)
  responsibilities <- get_metadata_responsibilities(dataset_id, base_path)
  errors <- character()
  required <- c(
    title = "dataset title", abstract = "dataset abstract",
    uuid = "resource UUID",
    metadata_uuid = "metadata-record UUID",
    metadata_resource_scope_code = "metadata resource scope",
    metadata_language_code = "metadata language",
    metadata_character_encoding = "metadata character encoding"
  )
  for (field in names(required)) if (!non_empty_scalar(record[[field]][1])) {
    errors <- c(errors, paste("Missing", required[[field]]))
  }
  if (record$metadata_resource_scope_code %in% c("dataset", "series") &&
      nrow(topics) == 0L) {
    errors <- c(errors, "Dataset/series scope requires at least one ISO topic category")
  }
  spatial <- tryCatch(iso_core_spatial_values(conn, dataset_id), error = function(e) e)
  if (inherits(spatial, "error")) {
    errors <- c(errors, conditionMessage(spatial))
  } else if (metadata_scope_requires_geographic_discovery(record$metadata_resource_scope_code)) {
    if (!nrow(spatial$bbox) && !non_empty_scalar(spatial$geographic_name))
      errors <- c(errors, "Dataset scope requires a valid bounding box or an explicitly recorded area covered")
    if (nrow(spatial$bbox) && !nrow(spatial$crs))
      errors <- c(errors, "Coordinate-based dataset extent requires a coordinate reference system")
  }
  metadata_dates <- tryCatch(iso_core_metadata_dates(conn, dataset_id, record), error = function(e) e)
  if (inherits(metadata_dates, "error")) errors <- c(errors, conditionMessage(metadata_dates))
  if (nrow(dates) == 0L || !any(dates$date_type_code == "publication")) {
    errors <- c(errors, "A primary publication date is required")
  }
  for (i in seq_len(nrow(dates))) {
    parsed <- tryCatch(iso_export_date(dates$date_value[i], paste("Citation date", i)), error = function(e) e)
    if (inherits(parsed, "error")) errors <- c(errors, conditionMessage(parsed)) else if (
      !identical(parsed$precision, dates$date_precision[i])
    ) errors <- c(errors, paste("Citation date", i, "does not match its recorded precision"))
  }
  if (nrow(responsibilities) == 0L || !any(
    responsibilities$role_code == "pointOfContact" & responsibilities$is_primary == 1L
  )) {
    errors <- c(errors, "A primary metadata point of contact is required")
  }
  if (nrow(responsibilities) && any(!nzchar(trimws(ifelse(is.na(responsibilities$person_name), "", responsibilities$person_name))) &
                                   !nzchar(trimws(ifelse(is.na(responsibilities$organisation_name), "", responsibilities$organisation_name))))) {
    errors <- c(errors, "A metadata responsibility has no named person or organisation; repair the linked contact.")
  }
  list(
    valid = length(errors) == 0L,
    errors = unique(errors),
    record = record,
    topics = topics,
    dates = dates,
    responsibilities = responsibilities,
    metadata_dates = metadata_dates,
    spatial = spatial
  )
}

iso_profile_add_responsibility <- function(root, row, base_path) {
  iso_contact_add_responsibility(root, "mdb:contact", row$person_id[1], row$organisation_id[1],
    row$role_code[1], base_path)
}

#' Build the ISO profile/discovery fragment for Wave 1B through 1D
#'
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An `xml_document` rooted at `mdb:MD_Metadata`.
#' @keywords internal
build_iso_profile_discovery_xml <- function(dataset_id, base_path) {
  validation <- validate_iso_profile_export(dataset_id, base_path)
  if (!isTRUE(validation$valid)) {
    stop(
      paste("ISO profile export cannot continue:", paste(validation$errors, collapse = "; ")),
      call. = FALSE
    )
  }
  namespaces <- iso_profile_namespaces()
  attrs <- paste0("xmlns:", names(namespaces), "=\"", namespaces, "\"", collapse = " ")
  document <- xml2::read_xml(paste0("<mdb:MD_Metadata ", attrs, "/>"))
  root <- xml2::xml_root(document)
  record <- validation$record

  identifier_property <- xml2::xml_add_child(root, "mdb:metadataIdentifier")
  identifier <- xml2::xml_add_child(identifier_property, "mcc:MD_Identifier")
  iso_profile_add_text(identifier, "mcc:code", record$metadata_uuid)

  locale_property <- xml2::xml_add_child(root, "mdb:defaultLocale")
  locale <- xml2::xml_add_child(locale_property, "lan:PT_Locale")
  iso_profile_add_code(
    locale, "lan:language", "lan:LanguageCode", record$metadata_language_code,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#LanguageCode"
  )
  iso_profile_add_code(
    locale, "lan:characterEncoding", "lan:MD_CharacterSetCode",
    record$metadata_character_encoding,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#MD_CharacterSetCode"
  )

  scope_property <- xml2::xml_add_child(root, "mdb:metadataScope")
  scope <- xml2::xml_add_child(scope_property, "mdb:MD_MetadataScope")
  iso_profile_add_code(
    scope, "mdb:resourceScope", "mcc:MD_ScopeCode",
    record$metadata_resource_scope_code,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#MD_ScopeCode"
  )
  if (!identical(record$metadata_resource_scope_code[1], "dataset"))
    iso_profile_add_text(scope, "mdb:name", record$title)

  responsibility_order <- order(
    validation$responsibilities$role_code,
    -as.integer(validation$responsibilities$is_primary),
    validation$responsibilities$id
  )
  for (i in responsibility_order) iso_profile_add_responsibility(
    root, validation$responsibilities[i, , drop = FALSE], base_path
  )

  for (date in validation$metadata_dates) iso_core_add_date(root, "mdb:dateInfo", date$value, date$type)

  standard_property <- xml2::xml_add_child(root, "mdb:metadataStandard")
  standard <- xml2::xml_add_child(standard_property, "cit:CI_Citation")
  iso_profile_add_text(standard, "cit:title", "ISO 19115-1:2014 geographic metadata - DataPond profile")
  iso_profile_add_text(
    standard, "cit:edition",
    paste("DataPond crosswalk", current_iso_crosswalk_version())
  )

  identification_property <- xml2::xml_add_child(root, "mdb:identificationInfo")
  identification <- xml2::xml_add_child(identification_property, "mri:MD_DataIdentification")
  citation_property <- xml2::xml_add_child(identification, "mri:citation")
  citation <- xml2::xml_add_child(citation_property, "cit:CI_Citation")
  iso_profile_add_text(citation, "cit:title", record$title)
  for (i in seq_len(nrow(validation$dates))) iso_core_add_date(
    citation, "cit:date", validation$dates$date_value[i], validation$dates$date_type_code[i]
  )
  for (i in seq_len(nrow(validation$dates))) if (non_empty_scalar(validation$dates$date_note[i])) {
    iso_profile_add_text(citation, "cit:otherCitationDetails", paste0("Citation date (",
      validation$dates$date_type_code[i], " ", validation$dates$date_value[i], "): ", validation$dates$date_note[i]))
  }
  iso_profile_add_text(citation, "cit:edition", record$version)
  iso_core_add_identifier(citation, "cit:identifier", record$uuid)
  iso_core_add_identifier(citation, "cit:identifier", record$citation_key, "DataPond citation key")
  iso_profile_add_text(identification, "mri:abstract", record$abstract)
  iso_profile_add_text(identification, "mri:purpose", record$purpose)
  iso_profile_add_text(identification, "mri:credit", record$dataset_credit)
  for (code in validation$topics$topic_category_code) {
    property <- xml2::xml_add_child(identification, "mri:topicCategory")
    xml2::xml_add_child(property, "mri:MD_TopicCategoryCode", code)
  }

  iso_contact_add_resource_parties(citation, identification, dataset_id, base_path)
  iso_add_supporting_documents(identification, "mri:additionalDocumentation", dataset_id, base_path, "include_in_metadata")
  iso_core_add_spatial(root, identification, validation$spatial)
  iso_add_maintenance(root, identification, record, dataset_id, base_path)
  iso_core_order_metadata(document)
}
