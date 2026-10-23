# Core ISO 19115 export helpers -------------------------------------------

# Preserve precision and time zone exactly. SQL timestamps need only the ISO
# separator; never manufacture a day, seconds, midnight or a UTC suffix.
iso_export_date <- function(value, label = "Date") {
  value <- iso_optional_text(value)
  if (is.na(value)) stop(paste(label, "is missing."), call. = FALSE)
  value <- sub(" ", "T", value, fixed = TRUE)
  precision <- infer_iso_date_precision(value)
  parsed <- tryCatch(validate_iso_date_value(value, precision), error = function(e) NULL)
  valid <- !is.null(parsed) && !startsWith(value, "0000")
  if (precision == "dateTime") {
    valid <- valid && grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]([.][0-9]+)?(Z|[+-]((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?$",
      value
    )
  }
  if (!valid) stop(paste0(label, " is not a valid ISO date/time at its recorded precision: ", value), call. = FALSE)
  list(value = value, precision = precision,
       element = if (precision == "dateTime") "gco:DateTime" else "gco:Date")
}

iso_core_metadata_dates <- function(conn, dataset_id, record) {
  events <- DBI::dbGetQuery(conn, "SELECT review_timestamp, update_type FROM metadata_review_tbl
    WHERE dataset_id = ? ORDER BY id", params = list(as.integer(dataset_id)))
  reviews <- events$review_timestamp
  reviews <- reviews[!is.na(reviews) & nzchar(trimws(reviews))]
  if (length(reviews)) {
    parsed <- lapply(reviews, iso_export_date, label = "Metadata review timestamp")
    if (any(vapply(parsed, function(x) x$precision != "dateTime", logical(1)))) {
      stop("Metadata review timestamps need their recorded date and time; no time will be invented.", call. = FALSE)
    }
    reviews <- vapply(parsed, `[[`, character(1), "value")
    zoned <- grepl("(Z|[+-][0-9]{2}:[0-9]{2})$", reviews)
    if (any(zoned) && !all(zoned)) {
      stop("Metadata review timestamps mix known and unspecified time zones; resolve this before ordering the reviews.", call. = FALSE)
    }
    # Unzoned timestamps are compared as recorded wall times, not relabelled UTC.
    times <- vapply(reviews, function(value) {
      instant <- as.numeric(as.POSIXct(sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "", value),
                                      format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
      if (grepl("[+-][0-9]{2}:[0-9]{2}$", value)) {
        zone <- substr(value, nchar(value) - 5L, nchar(value))
        offset <- as.numeric(substr(zone, 2, 3)) * 3600 + as.numeric(substr(zone, 5, 6)) * 60
        instant <- instant - if (startsWith(zone, "+")) offset else -offset
      }
      instant
    }, numeric(1))
    reviews <- reviews[order(times, seq_along(times))]
  }
  creation <- iso_optional_text(record$metadata_creation_date)
  if (is.na(creation)) {
    creation_events <- events$review_timestamp[vapply(events$update_type, function(types)
      "new_metadata_record" %in% split_stored_multi_value(types), logical(1))]
    creation_events <- creation_events[vapply(creation_events, non_empty_scalar, logical(1))]
    # Only explicitly classified creation events are evidence of creation.
    eligible <- reviews[reviews %in% sub(" ", "T", creation_events, fixed = TRUE)]
    if (length(eligible)) creation <- eligible[1]
  }
  if (is.na(creation)) stop(
    "Missing metadata creation date: supply the known metadata-record creation date or an explicitly recorded new_metadata_record event; an ordinary review or resource publication date is not evidence of creation.",
    call. = FALSE
  )
  rows <- list(c(iso_export_date(creation, "Metadata creation date"), list(type = "creation")))
  if (length(reviews)) rows[[2]] <- c(iso_export_date(utils::tail(reviews, 1)), list(type = "lastUpdate"))
  rows
}

iso_core_spatial_values <- function(conn, dataset_id) {
  bbox <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_spatial_summary_tbl
    WHERE dataset_id = ? AND LOWER(COALESCE(bbox_status, '')) = 'valid'",
    params = list(as.integer(dataset_id)))
  legacy <- iso_legacy_spatial_values(conn, dataset_id)
  if (!nrow(bbox) && nrow(legacy$bbox)) bbox <- legacy$bbox
  if (nrow(bbox)) {
    values <- suppressWarnings(as.numeric(unlist(bbox[1, c("xmin", "xmax", "ymin", "ymax")], use.names = FALSE)))
    if (any(!is.finite(values)) || any(abs(values[1:2]) > 180) ||
        any(abs(values[3:4]) > 90) || values[3] > values[4] ||
        !identical(toupper(trimws(bbox$bbox_crs[1])), "EPSG:4326")) {
      stop("The dataset spatial summary is marked valid but does not contain valid WGS84 bounds. Refresh or correct the spatial summary before exporting.", call. = FALSE)
    }
  }
  # Apply inclusion and active-file decisions before deduplicating native CRSs.
  extracted <- DBI::dbGetQuery(conn, "SELECT sm.crs_auth_code, sm.crs_name, sm.crs_input
    FROM data_file_spatial_metadata_tbl sm
    WHERE sm.dataset_id = ? AND COALESCE(sm.include_in_dataset_bbox, 0) = 1
      AND LOWER(COALESCE(sm.crs_status, '')) NOT IN ('', 'missing_crs', 'missing_or_unknown')
      AND NOT EXISTS (SELECT 1 FROM data_file_tbl f WHERE f.dataset_id = sm.dataset_id
        AND (f.id = sm.data_file_id OR f.data_file_rel_path_full = sm.data_file_rel_path_full)
        AND COALESCE(f.active, 1) = 0)
    ORDER BY sm.id", params = list(as.integer(dataset_id)))
  manual <- DBI::dbGetQuery(conn, "SELECT me.crs_auth_code, me.crs_name, me.crs_input
    FROM dataset_manual_spatial_extent_tbl me
    WHERE me.dataset_id = ? AND COALESCE(me.active, 1) = 1
      AND COALESCE(me.include_in_dataset_bbox, 0) = 1
      AND NOT EXISTS (SELECT 1 FROM data_file_tbl f WHERE f.id = me.data_file_id
        AND f.dataset_id = me.dataset_id AND COALESCE(f.active, 1) = 0)
    ORDER BY me.id", params = list(as.integer(dataset_id)))
  crs <- rbind(extracted, manual)
  if (!nrow(crs) && nrow(legacy$crs)) crs <- legacy$crs
  identifiers <- data.frame(code = character(), code_space = character(), description = character())
  for (i in seq_len(nrow(crs))) {
    authority <- iso_optional_text(crs$crs_auth_code[i])
    if (is.na(authority)) authority <- iso_optional_text(crs$crs_input[i])
    if (is.na(authority) || !grepl("^[A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_.-]+$", authority)) {
      stop("An included spatial source has no structured CRS authority/code. Resolve its CRS identifier before exporting; a CRS name or WKT is not an authority code.", call. = FALSE)
    }
    split <- strsplit(authority, ":", fixed = TRUE)[[1]]
    identifiers[nrow(identifiers) + 1L, ] <- list(split[2], toupper(split[1]), iso_optional_text(crs$crs_name[i]))
  }
  # Prefer a named row for duplicates, retaining distinct native CRSs.
  identifiers <- identifiers[order(is.na(identifiers$description)), , drop = FALSE]
  identifiers <- identifiers[!duplicated(identifiers[c("code_space", "code")]), , drop = FALSE]
  identifiers <- identifiers[order(identifiers$code_space, identifiers$code), , drop = FALSE]
  record <- DBI::dbGetQuery(conn, "SELECT geographic_extent_name FROM entry_tbl WHERE id=?",
                            params = list(as.integer(dataset_id)))
  list(bbox = bbox, crs = identifiers, description = iso_extent_derivation_note(conn, dataset_id, bbox),
       geographic_name = iso_optional_text(record$geographic_extent_name))
}

iso_core_add_date <- function(parent, property, value, type) {
  parsed <- iso_export_date(value)
  node <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "cit:CI_Date")
  iso_profile_add_text(node, "cit:date", parsed$value, parsed$element)
  iso_profile_add_code(node, "cit:dateType", "cit:CI_DateTypeCode", type,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#CI_DateTypeCode")
  invisible(node)
}

iso_core_add_identifier <- function(parent, property, code, code_space = NULL, description = NULL) {
  if (!non_empty_scalar(code)) return(invisible(NULL))
  node <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "mcc:MD_Identifier")
  iso_profile_add_text(node, "mcc:code", code)
  iso_profile_add_text(node, "mcc:codeSpace", code_space)
  iso_profile_add_text(node, "mcc:description", description)
  invisible(node)
}

iso_core_add_spatial <- function(root, identification, spatial) {
  for (i in seq_len(nrow(spatial$crs))) {
    reference <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:referenceSystemInfo"), "mrs:MD_ReferenceSystem")
    iso_core_add_identifier(reference, "mrs:referenceSystemIdentifier", spatial$crs$code[i],
                            spatial$crs$code_space[i], spatial$crs$description[i])
  }
  if (non_empty_scalar(spatial$geographic_name)) {
    extent <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:extent"), "gex:EX_Extent")
    description <- xml2::xml_add_child(xml2::xml_add_child(extent, "gex:geographicElement"), "gex:EX_GeographicDescription")
    iso_core_add_identifier(description, "gex:geographicIdentifier", spatial$geographic_name)
  }
  if (nrow(spatial$bbox)) {
    extent <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:extent"), "gex:EX_Extent")
    iso_profile_add_text(extent, "gex:description", spatial$description)
    box <- xml2::xml_add_child(xml2::xml_add_child(extent, "gex:geographicElement"), "gex:EX_GeographicBoundingBox")
    fields <- c(westBoundLongitude = "xmin", eastBoundLongitude = "xmax",
                southBoundLatitude = "ymin", northBoundLatitude = "ymax")
    for (property in names(fields)) {
      value <- format(spatial$bbox[[fields[[property]]]][1], scientific = FALSE, trim = TRUE,
                      digits = 17, decimal.mark = ".")
      iso_profile_add_text(box, paste0("gex:", property), value, "gco:Decimal")
    }
  }
}

# Composite builders append sections independently. Reorder only top-level
# properties and resource identification against the pinned sequence, keeping every node.
iso_core_order_metadata <- function(document) {
  root <- xml2::xml_root(document)
  sequence <- c("metadataIdentifier", "defaultLocale", "parentMetadata", "contact", "dateInfo",
    "metadataStandard", "metadataProfile", "alternativeMetadataReference", "otherLocale",
    "metadataLinkage", "spatialRepresentationInfo", "referenceSystemInfo", "metadataExtensionInfo",
    "identificationInfo", "contentInfo", "distributionInfo", "dataQualityInfo", "portrayalCatalogueInfo",
    "metadataConstraints", "applicationSchemaInfo", "metadataMaintenance", "resourceLineage",
    "metadataScope", "describes", "acquisitionInformation")
  iso_core_order_children(root, sequence)
  ns <- xml2::xml_ns(document)
  for (identification in xml2::xml_find_all(root, "mdb:identificationInfo/mri:MD_DataIdentification", ns)) {
    iso_core_order_children(identification, c("citation", "abstract", "purpose", "credit", "status",
      "pointOfContact", "spatialRepresentationType", "spatialResolution", "temporalResolution",
      "topicCategory", "extent", "additionalDocumentation", "processingLevel", "resourceMaintenance",
      "graphicOverview", "resourceFormat", "descriptiveKeywords", "resourceSpecificUsage",
      "resourceConstraints", "associatedResource", "defaultLocale", "otherLocale",
      "environmentDescription", "supplementalInformation"))
    for (citation in xml2::xml_find_all(identification, "mri:citation/cit:CI_Citation", ns))
      iso_core_order_children(citation, c("title", "alternateTitle", "date", "edition", "editionDate",
        "identifier", "citedResponsibleParty", "presentationForm", "series", "otherCitationDetails",
        "ISBN", "ISSN", "onlineResource", "graphic"))
  }
  document
}

iso_core_order_children <- function(root, sequence) {
  nodes <- xml2::xml_children(root)
  positions <- match(xml2::xml_name(nodes), sequence)
  for (i in order(positions, seq_along(positions), na.last = TRUE)) {
    xml2::xml_add_child(root, nodes[[i]], .copy = FALSE)
  }
  invisible(root)
}

iso_core_attach_lineage <- function(document, dataset_id, base_path) {
  lineage <- build_iso_lineage_xml(dataset_id, base_path)
  if (length(xml2::xml_children(xml2::xml_root(lineage)))) {
    property <- xml2::xml_add_child(xml2::xml_root(document), "mdb:resourceLineage")
    xml2::xml_add_child(property, xml2::xml_root(lineage))
  }
  iso_core_order_metadata(document)
}

#' Build the core ISO metadata document with resource lineage
#'
#' Core covers record identity, locale, metadata contacts/dates, standard,
#' resource citation/authors/contacts, selected supporting documents,
#' abstract/purpose/credit, resource progress/update frequency, metadata review
#' history and maintenance contacts, topics, WGS84 bounds, structured
#' CRS identifiers and the implemented lineage fields. Specialist Wave 2/3
#' sections are outside this scope; this is not a complete crosswalk export.
#'
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An xml2 document for core export assessment.
#' @keywords internal
build_iso_core_metadata_xml <- function(dataset_id, base_path) {
  iso_core_attach_lineage(build_iso_profile_discovery_xml(dataset_id, base_path), dataset_id, base_path)
}
