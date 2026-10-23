# ISO 19115-3 / 19157 Wave 2 serialization --------------------------------

iso_wave2_namespaces <- function() {
  c(
    gex = iso_profile_namespaces()[["gex"]],
    gml = "http://www.opengis.net/gml/3.2",
    mrd = "https://schemas.isotc211.org/19115/-1/mrd/1.3",
    mco = "https://schemas.isotc211.org/19115/-1/mco/1.3",
    mdq = "https://schemas.isotc211.org/19157/-/mdq/1.2"
  )
}

iso_wave2_ensure_namespaces <- function(document) {
  root <- xml2::xml_root(document)
  for (prefix in names(iso_wave2_namespaces())) {
    # xml_ns(document) also sees declarations local to lineage descendants;
    # those are not in scope for a new distribution/quality sibling.
    if (is.na(xml2::xml_attr(root, paste0("xmlns:", prefix))))
      xml2::xml_set_attr(root, paste0("xmlns:", prefix), iso_wave2_namespaces()[[prefix]])
  }
  invisible(document)
}

iso_wave2_add_time_position <- function(period, property, value, indeterminate) {
  node <- xml2::xml_add_child(period, property)
  value <- wave2_optional_text(value)
  indeterminate <- wave2_optional_text(indeterminate)
  if (!is.na(value)) xml2::xml_set_text(node, value)
  if (!is.na(indeterminate)) xml2::xml_set_attr(node, "indeterminatePosition", indeterminate)
  invisible(node)
}

#' Add Wave 2 sections to an ISO metadata document
#'
#' @param document An `xml_document` rooted at `mdb:MD_Metadata`.
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return The modified `xml_document`.
#' @keywords internal
add_iso_wave2_xml <- function(document, dataset_id, base_path) {
  iso_wave2_ensure_namespaces(document)
  root <- xml2::xml_root(document)
  ns <- xml2::xml_ns(document)
  identification <- xml2::xml_find_first(root, ".//mri:MD_DataIdentification", ns)
  if (inherits(identification, "xml_missing")) {
    property <- xml2::xml_add_child(root, "mdb:identificationInfo")
    identification <- xml2::xml_add_child(property, "mri:MD_DataIdentification")
  }
  citation <- xml2::xml_find_first(identification, ".//cit:CI_Citation", ns)
  if (inherits(citation, "xml_missing")) {
    property <- xml2::xml_add_child(identification, "mri:citation")
    citation <- xml2::xml_add_child(property, "cit:CI_Citation")
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave2_schema(conn)
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(as.integer(dataset_id)))
  if (nrow(record) != 1L) stop("Dataset record not found.", call. = FALSE)
  titles <- get_dataset_alternate_titles(dataset_id, base_path)
  temporal <- get_dataset_temporal_extents(dataset_id, base_path)

  for (title in titles$alternate_title) iso_profile_add_text(citation, "cit:alternateTitle", title)

  scale <- validate_equivalent_scale_denominator(record$equivalent_scale_denominator[1])
  if (!is.na(scale)) {
    property <- xml2::xml_add_child(identification, "mri:spatialResolution")
    resolution <- xml2::xml_add_child(property, "mri:MD_Resolution")
    equivalent_property <- xml2::xml_add_child(resolution, "mri:equivalentScale")
    fraction <- xml2::xml_add_child(equivalent_property, "mri:MD_RepresentativeFraction")
    denominator <- xml2::xml_add_child(fraction, "mri:denominator")
    xml2::xml_add_child(denominator, "gco:Integer", as.character(scale))
  }

  for (i in seq_len(nrow(temporal))) {
    for (endpoint in c("start", "end")) {
      value <- tryCatch(wave2_validate_temporal_value(temporal[[paste0(endpoint, "_value")]][i],
        temporal[[paste0(endpoint, "_precision")]][i], temporal[[paste0(endpoint, "_indeterminate")]][i], endpoint),
        error = function(e) stop(paste("Invalid temporal", endpoint, ":", conditionMessage(e)), call. = FALSE))
      if (!is.na(value$value)) {
        checked <- iso_export_date(value$value, "Temporal extent")
        if (!identical(checked$precision, value$precision)) stop("Temporal extent precision does not match its value.", call. = FALSE)
        temporal[[paste0(endpoint, "_value")]][i] <- checked$value
      }
    }
    if (non_empty_scalar(temporal$start_value[i]) && non_empty_scalar(temporal$end_value[i]) &&
        identical(temporal$start_precision[i], temporal$end_precision[i]) && temporal$start_precision[i] != "dateTime" &&
        temporal$start_value[i] > temporal$end_value[i]) stop("Temporal extent start cannot be later than its end.", call. = FALSE)
    property <- xml2::xml_add_child(identification, "mri:extent")
    extent <- xml2::xml_add_child(property, "gex:EX_Extent")
    notes <- c(iso_optional_text(temporal$extent_label[i]), iso_optional_text(temporal$description[i]))
    iso_profile_add_text(extent, "gex:description", paste(notes[!is.na(notes)], collapse = "\n"))
    temporal_property <- xml2::xml_add_child(extent, "gex:temporalElement")
    temporal_extent <- xml2::xml_add_child(temporal_property, "gex:EX_TemporalExtent")
    extent_property <- xml2::xml_add_child(temporal_extent, "gex:extent")
    period <- xml2::xml_add_child(extent_property, "gml:TimePeriod")
    xml2::xml_set_attr(period, "gml:id", paste0("temporal-", dataset_id, "-", i))
    iso_wave2_add_time_position(period, "gml:beginPosition", temporal$start_value[i], temporal$start_indeterminate[i])
    iso_wave2_add_time_position(period, "gml:endPosition", temporal$end_value[i], temporal$end_indeterminate[i])
  }

  document <- add_iso_distribution_xml(document, dataset_id, base_path)
  document <- add_iso_quality_xml(document, dataset_id, base_path)
  iso_core_order_metadata(document)
}

#' Build the Wave 1 and Wave 2 ISO metadata document
#'
#' @inheritParams build_iso_profile_discovery_xml
#' @return An `xml_document` rooted at `mdb:MD_Metadata`.
#' @keywords internal
build_iso_wave2_metadata_xml <- function(dataset_id, base_path) {
  add_iso_wave2_xml(
    build_iso_profile_discovery_xml(dataset_id, base_path),
    dataset_id,
    base_path
  )
}
