# ISO 19110 and ISO metadata Wave 3 serialization -------------------------

iso_wave3_namespaces <- function() c(
  gfc = "https://schemas.isotc211.org/19110/-/gfc/2.2",
  mrc = "https://schemas.isotc211.org/19115/-1/mrc/1.3",
  gex = iso_profile_namespaces()[["gex"]],
  gml = "http://www.opengis.net/gml/3.2",
  rbc = "https://schemas.isotc211.org/19111/-/rbc/3.1",
  xlink = "http://www.w3.org/1999/xlink"
)

iso_wave3_ensure_namespaces <- function(document) {
  root <- xml2::xml_root(document)
  for (prefix in names(iso_wave3_namespaces())) if (is.na(xml2::xml_attr(root, paste0("xmlns:", prefix)))) {
    xml2::xml_set_attr(root, paste0("xmlns:", prefix), iso_wave3_namespaces()[[prefix]])
  }
  invisible(document)
}

iso_wave3_add_citation_date <- function(citation, value, type = "revision") {
  if (non_empty_scalar(value)) iso_core_add_date(citation, "cit:date", value, type)
  invisible(citation)
}

iso_wave3_add_thesaurus_keywords <- function(identification, record, base_path) {
  thesauri <- get_keyword_thesauri(base_path)
  if (nrow(thesauri)) {
    thesauri$field_code <- vapply(
      thesauri$field_code,
      canonical_keyword_field_code,
      character(1)
    )
    thesauri <- thesauri[!duplicated(tolower(thesauri$field_code)), , drop = FALSE]
  }

  conn <- get_conn(base_path)
  vocabulary <- keyword_seed_snapshot(conn)
  DBI::dbDisconnect(conn)
  keyword_types <- c(
    keyword_object = "theme",
    keyword_discipline = "discipline",
    keyword_geo = "place"
  )
  for (field in names(keyword_types)) {
    if (!field %in% names(record)) next
    keywords <- split_keyword_values(record[[field]][1])
    if (!length(keywords)) next
    match <- which(tolower(thesauri$field_code) == tolower(field))
    row <- if (length(match)) thesauri[match[1], , drop = FALSE] else NULL
    # A field's citation applies to approved vocabulary members, not free text.
    members <- vocabulary[vocabulary$field_code == field &
      vocabulary$approval_status == "approved", , drop = FALSE]
    choices <- stats::setNames(members$option_value, members$option_label)
    canonical <- vapply(keywords, function(term) normalise_keyword_values(term, choices)[1], character(1))
    cited <- !is.null(row) & tolower(canonical) %in% tolower(members$option_value)
    groups <- list(custom = keywords[!cited], controlled = keywords[cited])
    for (kind in names(groups)) {
      terms <- groups[[kind]]
      if (!length(terms)) next
      property <- xml2::xml_add_child(identification, "mri:descriptiveKeywords")
      group <- xml2::xml_add_child(property, "mri:MD_Keywords")
      for (keyword in terms) iso_profile_add_text(group, "mri:keyword", keyword)
      iso_profile_add_code(group, "mri:type", "mri:MD_KeywordTypeCode", keyword_types[[field]],
        "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#MD_KeywordTypeCode")
      if (kind != "controlled") next
      thesaurus_property <- xml2::xml_add_child(group, "mri:thesaurusName")
      citation <- xml2::xml_add_child(thesaurus_property, "cit:CI_Citation")
      iso_profile_add_text(citation, "cit:title", row$citation_title[1])
      iso_profile_add_text(citation, "cit:edition", row$edition[1])
      iso_core_add_identifier(citation, "cit:identifier", row$identifier[1])
      iso_wave3_add_citation_date(citation, row$edition_date[1], row$edition_date_type_code[1])
      if (non_empty_scalar(row$thesaurus_uri[1])) {
        online_property <- xml2::xml_add_child(citation, "cit:onlineResource")
        online <- xml2::xml_add_child(online_property, "cit:CI_OnlineResource")
        linkage <- xml2::xml_add_child(online, "cit:linkage")
        xml2::xml_add_child(linkage, "gco:CharacterString", row$thesaurus_uri[1])
      }
      if (!is.na(row$responsible_person_id[1]) || !is.na(row$responsible_organisation_id[1])) {
        iso_contact_add_responsibility(citation, "cit:citedResponsibleParty", row$responsible_person_id[1],
          row$responsible_organisation_id[1], row$responsible_role_code[1], base_path)
      }
      iso_order_citation(citation)
    }
  }
  invisible(TRUE)
}

#' Add Wave 3 sections to an ISO metadata document
#'
#' Includes ordered vertical extents with locally checked EPSG definitions,
#' thesaurus citations and content. Custom CRS references remain unverified;
#' a CRS name alone uses an explicit unknown nil reason. No coordinates or
#' saved units are transformed.
#'
#' @keywords internal
add_iso_wave3_xml <- function(document, dataset_id, base_path) {
  iso_wave3_ensure_namespaces(document)
  document <- iso_add_spatial_representation(document, dataset_id, base_path)
  root <- xml2::xml_root(document); ns <- xml2::xml_ns(document)
  identification <- xml2::xml_find_first(root, ".//mri:MD_DataIdentification", ns)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(as.integer(dataset_id)))
  if (nrow(record) != 1L) stop("Dataset record not found.", call. = FALSE)
  vertical <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_vertical_extent_tbl WHERE dataset_id = ? ORDER BY sort_order, id", params = list(as.integer(dataset_id)))
  iso_add_vertical_extents(identification, vertical)
  iso_wave3_add_thesaurus_keywords(identification, record, base_path)

  add_iso_content_xml(document, dataset_id, base_path)
  document
}

#' Build the Wave 1 through Wave 3 ISO metadata document
#' @keywords internal
build_iso_wave3_metadata_xml <- function(dataset_id, base_path) {
  add_iso_wave3_xml(build_iso_wave2_metadata_xml(dataset_id, base_path), dataset_id, base_path)
}

#' Write the linked Wave 3 metadata and feature-catalogue documents
#'
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @param output_dir Existing parent directory. Each successful export creates a
#'   separate uniquely named package directory; earlier packages are never replaced.
#' @param validation_policy Pinned rules by default; explicitly select
#'   `"datapond"` to use the declared creation-date precision policy.
#' @return Named paths for metadata, any linked catalogue and a validation
#'   report recording both outcomes and the selected policy. Explicitly cited
#'   local supporting documents and scripts are checked and copied. The complete
#'   package is staged, validated and published by a single directory rename.
#'   The declared semantic profile must also pass, regardless of XML policy.
#'   Both results and unassessed receiving-catalogue compatibility are recorded.
#' @keywords internal
write_iso_wave3_documents <- function(dataset_id, base_path, output_dir,
                                     validation_policy = c("pinned", "datapond")) {
  validation_policy <- match.arg(validation_policy)
  if (is.null(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !dir.exists(output_dir)) {
    stop("Choose an existing ISO export directory.", call. = FALSE)
  }
  assessment <- assess_iso_metadata_package(dataset_id, base_path)
  iso_publish_metadata_package(assessment, output_dir, validation_policy)
}
