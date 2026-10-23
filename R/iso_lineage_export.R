# ISO 19115-3 lineage serialization ----

iso_lineage_namespaces <- function() {
  c(
    mrl = "https://schemas.isotc211.org/19115/-1/mrl/1.3",
    iso_profile_namespaces()[c("cit", "mcc", "mri", "gex", "gco")],
    gml = "http://www.opengis.net/gml/3.2",
    xlink = "http://www.w3.org/1999/xlink"
  )
}

iso_lineage_text <- function(parent, property, value, value_element = "gco:CharacterString") {
  value <- normalise_lineage_optional_text(value)
  if (is.na(value)) return(invisible(NULL))
  property_node <- xml2::xml_add_child(parent, property)
  xml2::xml_add_child(property_node, value_element, value)
}

iso_lineage_code <- function(parent, property, element, value, code_list) {
  value <- normalise_lineage_optional_text(value)
  if (is.na(value)) return(invisible(NULL))
  reference <- iso_export_code_reference(element, value, code_list)
  value <- reference$value
  code_list <- reference$code_list
  property_node <- xml2::xml_add_child(parent, property)
  code_node <- xml2::xml_add_child(property_node, element, value)
  xml2::xml_set_attrs(code_node, c(codeList = code_list, codeListValue = value))
  invisible(code_node)
}

iso_lineage_citation <- function(parent, source, base_path) {
  citation_property <- xml2::xml_add_child(parent, "mrl:sourceCitation")
  citation <- xml2::xml_add_child(citation_property, "cit:CI_Citation")
  iso_content_required(citation, "cit:title", source$title)

  identifier <- normalise_lineage_optional_text(source$identifier)
  if (!is.na(identifier)) {
    identifier_property <- xml2::xml_add_child(citation, "cit:identifier")
    identifier_node <- xml2::xml_add_child(identifier_property, "mcc:MD_Identifier")
    iso_lineage_text(identifier_node, "mcc:code", identifier)
    iso_lineage_text(identifier_node, "mcc:codeSpace", source$identifier_code_space)
  }

  citation_date <- normalise_lineage_optional_text(source$citation_date)
  if (!is.na(citation_date)) {
    iso_core_add_date(citation, "cit:date", citation_date,
      if (non_empty_scalar(source$citation_date_type_code)) source$citation_date_type_code else "publication")
  }

  uri <- normalise_lineage_optional_text(source$source_uri)
  if (!is.na(uri)) {
    online_property <- xml2::xml_add_child(citation, "cit:onlineResource")
    online <- xml2::xml_add_child(online_property, "cit:CI_OnlineResource")
    iso_lineage_text(online, "cit:linkage", uri, "gco:CharacterString")
  }
  iso_contact_add_responsibility(citation, "cit:citedResponsibleParty", source$responsible_person_id[1],
    source$responsible_organisation_id[1], source$role_code[1], base_path)
  iso_order_citation(citation)
  citation
}

iso_portable_reference <- function(value) {
  value <- normalise_lineage_optional_text(value)
  if (is.na(value)) return(NA_character_)
  normal <- gsub("\\\\", "/", value)
  if (grepl("^https?://", normal, ignore.case = TRUE)) return(normal)
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*:", normal)) return(NA_character_)
  if (startsWith(normal, "/") || grepl("^[A-Za-z]:/", normal)) return(NA_character_)
  if (grepl("(^|/)\\.\\.(/|$)", normal)) return(NA_character_)
  normal
}


iso_lineage_source_extent <- function(node, row) {
  start <- iso_optional_text(row$temporal_start[1]); end <- iso_optional_text(row$temporal_end[1])
  if (is.na(start) && is.na(end)) return(invisible(NULL))
  if (!is.na(start)) start <- iso_export_date(start, "Lineage source temporal start")$value
  if (!is.na(end)) end <- iso_export_date(end, "Lineage source temporal end")$value
  if (!is.na(start) && !is.na(end) && nchar(start) == nchar(end) && nchar(start) <= 10L && start > end)
    stop("Lineage source temporal start cannot be later than its end.", call. = FALSE)
  scope <- xml2::xml_add_child(xml2::xml_add_child(node, "mrl:scope"), "mcc:MD_Scope")
  # The source's scope level is not captured: do not assume it is a dataset.
  iso_content_required(scope, "mcc:level", NA_character_)
  extent <- xml2::xml_add_child(xml2::xml_add_child(scope, "mcc:extent"), "gex:EX_Extent")
  temporal <- xml2::xml_add_child(xml2::xml_add_child(extent, "gex:temporalElement"), "gex:EX_TemporalExtent")
  period <- xml2::xml_add_child(xml2::xml_add_child(temporal, "gex:extent"), "gml:TimePeriod")
  xml2::xml_set_attr(period, "gml:id", paste0("datapond-source-period-", row$id[1]))
  for (property in c("beginPosition", "endPosition")) {
    value <- if (property == "beginPosition") start else end
    position <- xml2::xml_add_child(period, paste0("gml:", property))
    if (is.na(value)) xml2::xml_set_attr(position, "indeterminatePosition", "unknown") else xml2::xml_set_text(position, value)
  }
  invisible(scope)
}

iso_lineage_source_resolution <- function(node, row) {
  scale <- validate_equivalent_scale_denominator(row$equivalent_scale_denominator[1])
  distance <- iso_optional_text(row$spatial_resolution_value[1])
  unit <- iso_optional_text(row$spatial_resolution_unit[1])
  if (!is.na(distance) && (!is.finite(suppressWarnings(as.numeric(distance))) || as.numeric(distance) <= 0))
    stop("Lineage source spatial resolution must be a positive number.", call. = FALSE)
  if (!is.na(distance) && is.na(unit)) stop("Lineage source spatial resolution needs its recorded unit.", call. = FALSE)
  if (is.na(distance) && !is.na(unit)) stop("Lineage source spatial resolution has a unit but no value.", call. = FALSE)
  if (is.na(scale) && is.na(distance)) return(invisible(NULL))
  resolution <- xml2::xml_add_child(xml2::xml_add_child(node, "mrl:sourceSpatialResolution"), "mri:MD_Resolution")
  # The pinned schema permits one resolution and one choice. Preserve a second
  # recorded representation in labelled source text rather than drop it.
  if (!is.na(scale)) {
    fraction <- xml2::xml_add_child(xml2::xml_add_child(resolution, "mri:equivalentScale"), "mri:MD_RepresentativeFraction")
    iso_profile_add_text(fraction, "mri:denominator", as.character(scale), "gco:Integer")
    if (!is.na(distance)) {
      description <- xml2::xml_find_first(node, "mrl:description/gco:CharacterString", xml2::xml_ns(node))
      text <- paste0("Spatial resolution: ", distance, " ", unit)
      if (inherits(description, "xml_missing")) iso_profile_add_text(node, "mrl:description", text) else
        xml2::xml_set_text(description, paste(xml2::xml_text(description), text, sep = "\n\n"))
    }
  } else {
    if (grepl("[[:space:]]", unit)) stop("Lineage distance unit needs a unit symbol or URI for ISO export.", call. = FALSE)
    measure <- xml2::xml_add_child(xml2::xml_add_child(resolution, "mri:distance"), "gco:Distance", distance)
    xml2::xml_set_attr(measure, "uom", unit)
  }
  invisible(resolution)
}

#' Build the ISO 19115-3 resource-lineage fragment for a dataset
#'
#' The returned document has `mrl:LI_Lineage` as its root. It is intentionally
#' composable so the metadata serializer can attach it below
#' `mdb:resourceLineage` without flattening lineage records. It includes recorded
#' source temporal scope/resolution, responsible parties, process processors and
#' portable references to explicitly selected authoritative documents.
#'
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An `xml_document` containing one `mrl:LI_Lineage` element.
#' @keywords internal
build_iso_lineage_xml <- function(dataset_id, base_path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required for ISO lineage export.", call. = FALSE)
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  summary <- DBI::dbGetQuery(
    conn, "SELECT * FROM dataset_lineage_tbl WHERE dataset_id = ?",
    params = list(as.integer(dataset_id))
  )
  sources <- get_dataset_lineage_sources(dataset_id, base_path)
  steps <- get_dataset_lineage_process_steps(dataset_id, base_path)

  namespaces <- iso_lineage_namespaces()
  namespace_attrs <- paste0(
    "xmlns:", names(namespaces), "=\"", unname(namespaces), "\"",
    collapse = " "
  )
  document <- xml2::read_xml(paste0("<mrl:LI_Lineage ", namespace_attrs, "/>"))
  lineage <- xml2::xml_root(document)
  if (nrow(summary) > 0L) {
    iso_lineage_text(lineage, "mrl:statement", summary$lineage_statement[1])
  }

  for (i in seq_len(nrow(sources))) {
    source_property <- xml2::xml_add_child(lineage, "mrl:source")
    source_node <- xml2::xml_add_child(source_property, "mrl:LI_Source")
    xml2::xml_set_attr(source_node, "id", paste0("datapond-lineage-source-", sources$id[i]))
    iso_lineage_text(source_node, "mrl:description", sources$description[i])
    row <- sources[i, , drop = FALSE]
    citation_fields <- c("title", "identifier", "citation_date", "source_uri", "responsible_person_id", "responsible_organisation_id")
    if (any(vapply(row[citation_fields], non_empty_scalar, logical(1)))) iso_lineage_citation(source_node, row, base_path)
    if (!is.na(row$supporting_document_id[1])) {
      linked <- DBI::dbGetQuery(conn, "SELECT * FROM supporting_document_tbl WHERE id = ? AND dataset_id = ? AND active = 1",
        params = list(row$supporting_document_id[1], as.integer(dataset_id)))
      if (nrow(linked) != 1L) stop("A lineage source links to an inactive or foreign supporting document.", call. = FALSE)
      link <- xml2::xml_add_child(source_node, "mrl:sourceMetadata")
      xml2::xml_set_attr(link, "xlink:href", paste0("#datapond-lineage-document-", row$supporting_document_id[1]))
    }

    iso_lineage_source_resolution(source_node, row)
    iso_lineage_source_extent(source_node, row)
    iso_core_order_children(source_node, c("description", "sourceSpatialResolution", "sourceReferenceSystem",
                                          "sourceCitation", "sourceMetadata", "scope", "sourceStep"))
  }

  for (i in seq_len(nrow(steps))) {
    step_property <- xml2::xml_add_child(lineage, "mrl:processStep")
    step_node <- xml2::xml_add_child(step_property, "mrl:LI_ProcessStep")
    description <- normalise_lineage_optional_text(steps$description[i])
    output <- normalise_lineage_optional_text(steps$output_description[i])
    if (!is.na(output) && (is.na(description) || !grepl(output, description, fixed = TRUE))) {
      description <- paste(c(if (!is.na(description)) description, paste0("Output: ", output)), collapse = "\n\n")
    }
    iso_lineage_text(step_node, "mrl:description", description)
    iso_lineage_text(step_node, "mrl:rationale", steps$rationale[i])
    if (non_empty_scalar(steps$step_datetime[i])) {
      value <- iso_export_date(steps$step_datetime[i], paste("Lineage process step", steps$id[i], "date/time"))
      instant <- xml2::xml_add_child(xml2::xml_add_child(step_node, "mrl:stepDateTime"), "gml:TimeInstant")
      xml2::xml_set_attr(instant, "gml:id", paste0("datapond-step-time-", steps$id[i]))
      xml2::xml_add_child(instant, "gml:timePosition", value$value)
    }
    iso_contact_add_responsibility(step_node, "mrl:processor", steps$processor_person_id[i],
      steps$processor_organisation_id[i], steps$role_code[i], base_path)
    input_ids <- if (is.na(steps$source_ids[i])) character() else {
      strsplit(steps$source_ids[i], ",", fixed = TRUE)[[1]]
    }
    for (source_id in input_ids[nzchar(input_ids)]) {
      if (!source_id %in% as.character(sources$id)) stop(
        paste("Lineage step", steps$id[i], "references a source outside this dataset:" , source_id), call. = FALSE)
      source_link <- xml2::xml_add_child(step_node, "mrl:source")
      xml2::xml_set_attr(source_link, "xlink:href", paste0("#datapond-lineage-source-", source_id))
    }
    reference <- normalise_lineage_optional_text(steps$tool_software_reference[i])
    script <- iso_portable_reference(steps$script_reference[i])
    if (non_empty_scalar(steps$script_reference[i]) && is.na(script))
      stop("Lineage process script needs a portable relative path or HTTP(S) URL before ISO export.", call. = FALSE)
    if (!is.na(reference) || !is.na(script)) {
      reference_property <- xml2::xml_add_child(step_node, "mrl:reference")
      citation <- xml2::xml_add_child(reference_property, "cit:CI_Citation")
      iso_lineage_text(citation, "cit:title", if (is.na(reference)) "Processing script" else reference)
      if (!is.na(script)) {
        online_property <- xml2::xml_add_child(citation, "cit:onlineResource")
        online <- xml2::xml_add_child(online_property, "cit:CI_OnlineResource")
        iso_lineage_text(online, "cit:linkage", script)
      }
    }
    iso_core_order_children(step_node, c("description", "rationale", "stepDateTime", "processor",
                                        "reference", "scope", "source"))
  }

  documents <- iso_add_supporting_documents(lineage, "mrl:additionalDocumentation", dataset_id, base_path, "include_in_lineage")
  linked_document_ids <- unique(sources$supporting_document_id)
  linked_document_ids <- linked_document_ids[!is.na(linked_document_ids)]
  missing_ids <- setdiff(linked_document_ids, documents$id)
  if (length(missing_ids) > 0L) {
    placeholders <- paste(rep("?", length(missing_ids)), collapse = ",")
    linked <- DBI::dbGetQuery(conn, paste0("
      SELECT * FROM supporting_document_tbl
      WHERE dataset_id = ? AND active = 1 AND id IN (", placeholders, ")
      ORDER BY document_rel_path, id
    "), params = c(list(as.integer(dataset_id)), as.list(missing_ids)))
    for (i in seq_len(nrow(linked))) iso_document_citation(lineage, "mrl:additionalDocumentation", linked[i, ],
      id_prefix = "datapond-lineage-document-", details = FALSE)
  }
  iso_core_order_children(lineage, c("statement", "scope", "additionalDocumentation", "source", "processStep"))
  document
}
