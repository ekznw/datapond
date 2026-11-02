# Distribution and legal constraints for the pinned ISO 1.3.0 model.

iso_export_distinct_text <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(trimws(values))]
  # Collapse whitespace for comparison only; preserve the first stored wording.
  # Case and punctuation can change meaning, so do not fuzzy-match legal text.
  values[!duplicated(gsub("[[:space:]]+", " ", trimws(values)))]
}

iso_export_organisation <- function(parent, property, name, role) {
  if (!non_empty_scalar(name)) return(invisible(NULL))
  responsibility <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "cit:CI_Responsibility")
  iso_profile_add_code(responsibility, "cit:role", "cit:CI_RoleCode", role,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#CI_RoleCode")
  party <- xml2::xml_add_child(xml2::xml_add_child(responsibility, "cit:party"), "cit:CI_Organisation")
  iso_profile_add_text(party, "cit:name", name)
  invisible(responsibility)
}

iso_export_mime_text <- function(parent, property, value, media_type) {
  if (!non_empty_scalar(media_type)) return(iso_profile_add_text(parent, property, value))
  node <- xml2::xml_add_child(parent, property)
  # When no resource name was recorded, the MIME label itself remains useful.
  text <- if (non_empty_scalar(value)) value else media_type
  mime <- xml2::xml_add_child(node, "gcx:MimeFileType", text)
  xml2::xml_set_attr(mime, "type", media_type)
  invisible(node)
}

iso_export_online_resource <- function(parent, property, row) {
  value <- function(field) licensing_data_value(row, field)
  url <- value("resource_url")
  if (!wave2_valid_uri(url)) stop("Online resource needs an absolute URI.", call. = FALSE)
  code <- value("function_code")
  if (!is.na(code) && !code %in% iso_online_resource_function_codes())
    stop("Unknown online-resource function code.", call. = FALSE)
  resource <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "cit:CI_OnlineResource")
  iso_profile_add_text(resource, "cit:linkage", url)
  iso_profile_add_text(resource, "cit:protocol", value("protocol"))
  iso_export_mime_text(resource, "cit:name", value("resource_name"), value("media_type"))
  iso_profile_add_text(resource, "cit:description", value("resource_description"))
  iso_profile_add_code(resource, "cit:function", "cit:CI_OnLineFunctionCode", code,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#CI_OnLineFunctionCode")
  invisible(resource)
}

iso_export_legal_constraints <- function(identification, record, rights, conn, base_path) {
  value <- function(field) licensing_data_value(rights, field)
  entry <- function(field) licensing_data_value(record, field)
  labelled <- function(label, text) if (non_empty_scalar(text)) paste0(label, ": ", text) else character()
  restriction <- function(parent, property, code) iso_profile_add_code(
    parent, property, "mco:MD_RestrictionCode", code,
    "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#MD_RestrictionCode")

  access <- value("access_level_code")
  access_map <- c(public = "unrestricted", public_request = "otherRestrictions",
    approved_partners = "restricted", internal = "restricted", embargoed = "restricted",
    not_distributable = "restricted")
  if (!is.na(access) && !access %in% names(access_map))
    stop("Unknown licensing access-level code.", call. = FALSE)
  permission <- value("redistribution_permission_code")
  permission_labels <- c(unrestricted = "Unrestricted reuse",
    permitted_with_attribution = "Permitted with attribution",
    permitted_with_conditions = "Permitted with additional conditions",
    approved_recipients_only = "Approved recipients only", prohibited = "Redistribution prohibited",
    unknown = "Redistribution permission unknown")
  if (!is.na(permission) && !permission %in% names(permission_labels))
    stop("Unknown redistribution-permission code.", call. = FALSE)
  sensitivity <- value("sensitivity_handling_code")
  sensitivity_labels <- c(none = "No sensitive content", removed_or_generalised = "Sensitive content removed or generalised",
    approved_recipients_only = "Sensitive content: approved recipients only",
    confidential = "Confidential or contractually restricted", review_required = "Sensitivity review required")
  if (!is.na(sensitivity) && !sensitivity %in% names(sensitivity_labels))
    stop("Unknown sensitivity-handling code.", call. = FALSE)

  limitations <- iso_export_distinct_text(c(value("use_constraints"),
    if (identical(as.integer(value("required_attribution")), 1L)) {
      if (non_empty_scalar(value("attribution_text"))) value("attribution_text") else "Attribution required."
    }, entry("use_limitations")))
  other <- iso_export_distinct_text(c(value("license_description"),
    if (!is.na(access)) labelled("Access", licensing_access_level_label(access)),
    if (is.na(access)) labelled("Access", value("access_level")),
    if (!is.na(permission)) unname(permission_labels[permission]),
    value("permission_reference"),
    if (!is.na(sensitivity) && sensitivity != "none") unname(sensitivity_labels[sensitivity]),
    if (is.na(sensitivity) && identical(as.integer(value("sensitive_data")), 1L)) "Sensitive data.",
    value("sensitivity_reason"), entry("access_constraints")))

  # Licence identity falls back independently of the existence of an embargo-only row.
  identity <- vapply(c("license_name", "license_identifier", "license_url"), value, character(1))
  has_licence <- any(!is.na(identity))
  legacy_licence <- if (!has_licence) entry("license_type") else NA_character_
  holder <- value("license_holder")
  holder_id <- value("rights_holder_organisation_id")
  if (!is.na(holder_id)) {
    organisation <- DBI::dbGetQuery(conn, "SELECT organisation_name_full FROM organisation_tbl WHERE id = ?",
                                   params = list(as.integer(holder_id)))
    if (nrow(organisation) != 1L || !non_empty_scalar(organisation$organisation_name_full))
      stop("Linked rights-holder organisation has no usable name.", call. = FALSE)
    holder <- organisation$organisation_name_full[1]
  }

  release_fields <- c("release_addressee", "release_statement", "dissemination_constraint_code")
  has_release <- any(vapply(release_fields, function(field) non_empty_scalar(value(field)), logical(1)))
  # Embargo facts are recorded as labelled restrictions, without inventing a
  # release decision or turning a date into a timestamp.
  embargo <- vapply(c("embargo_status", "embargo_reason", "embargo_start_date", "embargo_end_date"),
    function(field) { x <- value(field); if (is.na(x)) entry(field) else x }, character(1))
  if (non_empty_scalar(embargo["embargo_status"]) && embargo["embargo_status"] != "No") {
    other <- iso_export_distinct_text(c(other, labelled("Embargo", embargo["embargo_status"]),
      labelled("Embargo reason", embargo["embargo_reason"]),
      labelled("Embargo start", embargo["embargo_start_date"]), labelled("Embargo end", embargo["embargo_end_date"])))
  }
  if (!length(limitations) && !length(other) && !has_licence && is.na(legacy_licence) &&
      is.na(holder) && !has_release) return(invisible(NULL))

  legal <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:resourceConstraints"), "mco:MD_LegalConstraints")
  for (text in limitations) iso_profile_add_text(legal, "mco:useLimitation", text)
  if (has_licence || !is.na(legacy_licence)) {
    citation <- xml2::xml_add_child(xml2::xml_add_child(legal, "mco:reference"), "cit:CI_Citation")
    title <- value("license_name")
    if (is.na(title)) title <- if (has_licence) value("license_identifier") else legacy_licence
    if (is.na(title)) {
      xml2::xml_set_attr(xml2::xml_add_child(citation, "cit:title"), "gco:nilReason", "missing")
    } else iso_profile_add_text(citation, "cit:title", title)
    if (non_empty_scalar(value("license_identifier"))) {
      identifier <- xml2::xml_add_child(xml2::xml_add_child(citation, "cit:identifier"), "mcc:MD_Identifier")
      iso_profile_add_text(identifier, "mcc:code", value("license_identifier"))
    }
    if (non_empty_scalar(value("license_url"))) iso_export_online_resource(citation, "cit:onlineResource",
      list(resource_url = value("license_url"), function_code = "information"))
  }
  if (has_release) {
    release <- xml2::xml_add_child(xml2::xml_add_child(legal, "mco:releasability"), "mco:MD_Releasability")
    iso_export_organisation(release, "mco:addressee", value("release_addressee"), "user")
    iso_profile_add_text(release, "mco:statement", value("release_statement"))
    dissemination <- value("dissemination_constraint_code")
    if (!is.na(dissemination) && !dissemination %in% iso_dissemination_constraint_codes())
      stop("Unknown dissemination-constraint code.", call. = FALSE)
    restriction(release, "mco:disseminationConstraints", iso_dissemination_restriction_code(dissemination))
    # A code alone intentionally fails the ISO addressee-or-statement rule.
  }
  if (!is.na(holder_id)) iso_contact_add_responsibility(legal, "mco:responsibleParty",
    organisation_id = as.integer(holder_id), role = "rightsHolder", base_path = base_path) else
    iso_export_organisation(legal, "mco:responsibleParty", holder, "rightsHolder")
  if (!is.na(access)) restriction(legal, "mco:accessConstraints", unname(access_map[access]))
  if (has_licence || !is.na(legacy_licence)) restriction(legal, "mco:useConstraints", "licence")
  if (length(other)) restriction(legal, "mco:accessConstraints", "otherRestrictions")
  for (text in other) iso_profile_add_text(legal, "mco:otherConstraints", text)
  iso_core_order_children(legal, c("useLimitation", "constraintApplicationScope", "graphic", "reference",
    "releasability", "responsibleParty", "accessConstraints", "useConstraints", "otherConstraints"))
  invisible(legal)
}

add_iso_distribution_xml <- function(document, dataset_id, base_path) {
  iso_wave2_ensure_namespaces(document)
  root <- xml2::xml_root(document)
  xml2::xml_set_attr(root, "xmlns:gcx", "https://schemas.isotc211.org/19103/-/gcx/1.2")
  identification <- xml2::xml_find_first(root, "mdb:identificationInfo/mri:MD_DataIdentification", xml2::xml_ns(document))
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave2_schema(conn)
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(as.integer(dataset_id)))
  rights <- get_dataset_release_rights(dataset_id, base_path)
  iso_export_legal_constraints(identification, record, rights, conn, base_path)
  online <- get_dataset_online_resources(dataset_id, base_path)
  formats <- get_dataset_distribution_formats(dataset_id, base_path)
  distributors <- DBI::dbGetQuery(conn, "SELECT NULL AS person_id, organisation_id FROM dataset_organisation_tbl
    WHERE dataset_id = ? AND role = 'distributor' ORDER BY id", params = list(as.integer(dataset_id)))
  contacts <- DBI::dbGetQuery(conn, "SELECT person_id, organisation_id FROM poc_tbl
    WHERE dataset_id = ? AND role = 'distributor' ORDER BY id", params = list(as.integer(dataset_id)))
  distributors <- rbind(distributors, contacts)
  selected <- licensing_data_value(record, "data_distributor_organisation_id")
  if (!is.na(selected)) distributors <- rbind(data.frame(person_id = NA_integer_, organisation_id = as.integer(selected)), distributors)
  distributors <- unique(distributors)
  if (nrow(online) || nrow(formats) || nrow(distributors)) {
    distribution <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:distributionInfo"), "mrd:MD_Distribution")
    for (i in seq_len(nrow(formats))) {
      row <- formats[i, , drop = FALSE]
      if (!non_empty_scalar(row$specification_title)) stop("Distribution format needs a specification title.", call. = FALSE)
      format <- xml2::xml_add_child(xml2::xml_add_child(distribution, "mrd:distributionFormat"), "mrd:MD_Format")
      citation <- xml2::xml_add_child(xml2::xml_add_child(format, "mrd:formatSpecificationCitation"), "cit:CI_Citation")
      iso_export_mime_text(citation, "cit:title", row$specification_title, row$media_type)
      iso_profile_add_text(citation, "cit:edition", row$format_version)
      if (non_empty_scalar(row$format_uri)) iso_export_online_resource(citation, "cit:onlineResource",
        list(resource_url = row$format_uri, function_code = "information"))
    }
    for (i in seq_len(nrow(distributors))) {
      if (is.na(distributors$person_id[i]) && is.na(distributors$organisation_id[i]))
        stop("A distributor has no linked party.", call. = FALSE)
      distributor <- xml2::xml_add_child(xml2::xml_add_child(distribution, "mrd:distributor"), "mrd:MD_Distributor")
      iso_contact_add_responsibility(distributor, "mrd:distributorContact", distributors$person_id[i],
        distributors$organisation_id[i], "distributor", base_path)
    }
    if (nrow(online)) {
      transfer <- xml2::xml_add_child(xml2::xml_add_child(distribution, "mrd:transferOptions"), "mrd:MD_DigitalTransferOptions")
      for (i in seq_len(nrow(online))) iso_export_online_resource(transfer, "mrd:onLine", online[i, , drop = FALSE])
    }
  }
  iso_core_order_metadata(document)
}

#' Build core metadata, lineage, distribution and legal constraints
#'
#' Uses stored formats, online resources, distributor organisations and public
#' licensing fields. Does not include internal licence notes, quality, raster
#' content or feature catalogues, and is not complete crosswalk coverage.
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An xml2 metadata document for validation.
#' @keywords internal
build_iso_distribution_metadata_xml <- function(dataset_id, base_path) {
  add_iso_distribution_xml(build_iso_core_metadata_xml(dataset_id, base_path), dataset_id, base_path)
}
