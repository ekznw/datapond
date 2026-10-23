# Resource progress and metadata maintenance ------------------------------
# Code membership checked against ISO/TC 211's published catalogue (2026-10-19):
# https://schemas.isotc211.org/resources/codelists/
# Unknown local text is preserved; it is never inserted as an invented code.

iso_maintenance_code <- function(value, codes, aliases = character()) {
  value <- iso_optional_text(value)
  if (is.na(value)) return(NA_character_)
  key <- function(x) gsub("[[:space:]_-]+", "", tolower(x))
  lookup <- c(stats::setNames(codes, key(codes)), stats::setNames(unname(aliases), key(names(aliases))))
  unname(lookup[match(key(value), names(lookup))])
}

iso_maintenance_frequency <- function(parent, value) {
  code <- iso_maintenance_code(value,
    c("continual", "daily", "weekly", "fortnightly", "monthly", "quarterly", "biannually",
      "annually", "asNeeded", "irregular", "notPlanned", "unknown", "periodic", "semimonthly", "biennially"),
    c(yearly = "annually", "every year" = "annually", "twice a year" = "biannually",
      "every two years" = "biennially", "every two weeks" = "fortnightly"))
  iso_profile_add_code(parent, "mmi:maintenanceAndUpdateFrequency", "mmi:MD_MaintenanceFrequencyCode",
    if (is.na(code)) "unknown" else code,
    "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.mmi.MD_MaintenanceFrequencyCode")
  invisible(code)
}

iso_maintenance_resource <- function(identification, record) {
  status <- iso_optional_text(record$data_complete_status)
  if (!is.na(status)) {
    code <- iso_maintenance_code(status,
      c("completed", "historicalArchive", "obsolete", "onGoing", "planned", "required",
        "underDevelopment", "final", "pending", "retired", "superseded", "tentative",
        "valid", "accepted", "notAccepted", "withdrawn", "proposed", "deprecated"),
      c(complete = "completed", started = "underDevelopment"))
    if (is.na(code)) {
      iso_content_required(identification, "mri:status", NA_character_)
      iso_profile_add_text(identification, "mri:supplementalInformation", paste("Recorded dataset status:", status))
    } else iso_profile_add_code(identification, "mri:status", "mcc:MD_ProgressCode", code,
      "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.mcc.MD_ProgressCode")
  }
  # Release/archive workflow states do not establish production completeness.
  frequency <- iso_optional_text(record$update_frequency)
  if (!is.na(frequency)) {
    node <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:resourceMaintenance"), "mmi:MD_MaintenanceInformation")
    code <- iso_maintenance_frequency(node, frequency)
    if (is.na(code)) iso_profile_add_text(node, "mmi:maintenanceNote", paste("Recorded update frequency:", frequency))
  }
  invisible(identification)
}

iso_maintenance_reviewer <- function(row, conn) {
  id <- wave3_optional_integer(row$reviewer_id)
  name <- iso_optional_text(row$reviewer_name)
  if (!is.na(id)) {
    person <- iso_contact_record(conn, "persons_tbl", id)
    parts <- c(person$name_first, person$name_last)
    name <- paste(parts[vapply(parts, non_empty_scalar, logical(1))], collapse = " ")
    if (!nzchar(name)) stop("ISO maintenance reviewer has no usable name.", call. = FALSE)
  }
  # Legacy reviewer_role may be a job title, not a CI_RoleCode. Preserve it in
  # the event note and use an explicit unknown role instead of guessing.
  role <- tryCatch(iso_contact_role(row$reviewer_role), error = function(e) NA_character_)
  list(id = id, name = name, role = role)
}

iso_add_maintenance <- function(root, identification, record, dataset_id, base_path) {
  iso_maintenance_resource(identification, record)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  rows <- DBI::dbGetQuery(conn, "SELECT * FROM metadata_review_tbl WHERE dataset_id = ? ORDER BY id",
    params = list(as.integer(dataset_id)))
  if (!nrow(rows)) return(invisible(root))

  # MDB permits ONE metadataMaintenance property. Repeat dates and labelled
  # event notes inside it, and deduplicate contacts by identity and ISO role.
  node <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:metadataMaintenance"), "mmi:MD_MaintenanceInformation")
  iso_maintenance_frequency(node, "unknown")
  reviewers <- vector("list", nrow(rows))
  dates <- character(nrow(rows))
  for (i in seq_len(nrow(rows))) {
    value <- iso_optional_text(rows$review_timestamp[i])
    if (is.na(value)) value <- iso_optional_text(rows$review_date[i])
    date <- iso_export_date(value, "Metadata maintenance review date")
    dates[i] <- date$value
    # lastRevision describes a review, without asserting a publication or
    # promising a next update. Precision/time zone remain exactly as recorded.
    iso_core_add_date(node, "mmi:maintenanceDate", date$value, "lastRevision")
    reviewers[[i]] <- iso_maintenance_reviewer(rows[i, , drop = FALSE], conn)
  }
  scope <- xml2::xml_add_child(xml2::xml_add_child(node, "mmi:maintenanceScope"), "mcc:MD_Scope")
  iso_profile_add_code(scope, "mcc:level", "mcc:MD_ScopeCode", "metadata",
    "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.mcc.MD_ScopeCode")
  description <- xml2::xml_add_child(xml2::xml_add_child(scope, "mcc:levelDescription"), "mcc:MD_ScopeDescription")
  iso_profile_add_text(description, "mcc:other", "Metadata record review history")
  iso_profile_add_text(node, "mmi:maintenanceNote", "Metadata maintenance schedule is not recorded; review dates describe past events.")
  for (i in seq_len(nrow(rows))) {
    fields <- c(Reviewer = reviewers[[i]]$name, `Recorded reviewer role` = rows$reviewer_role[i],
      `Metadata sections` = rows$metadata_section[i], Reason = rows$update_reason[i], Summary = rows$update_summary[i])
    fields <- fields[vapply(fields, non_empty_scalar, logical(1))]
    note <- c(paste("Metadata review:", dates[i]), paste0(names(fields), ": ", fields))
    iso_profile_add_text(node, "mmi:maintenanceNote", paste(note, collapse = "\n"))
  }
  seen <- character()
  for (reviewer in reviewers) {
    if (is.na(reviewer$name)) next
    identity <- if (!is.na(reviewer$id)) paste0("person:", reviewer$id) else paste0("legacy-name:", reviewer$name)
    key <- paste(identity, reviewer$role, sep = "\r")
    if (key %in% seen) next
    seen <- c(seen, key)
    if (!is.na(reviewer$id)) {
      iso_contact_add_responsibility(node, "mmi:contact", person_id = reviewer$id,
        role = reviewer$role, base_path = base_path)
    } else {
      contact <- xml2::xml_add_child(xml2::xml_add_child(node, "mmi:contact"), "cit:CI_Responsibility")
      if (is.na(reviewer$role)) iso_content_required(contact, "cit:role", NA_character_) else
        iso_profile_add_code(contact, "cit:role", "cit:CI_RoleCode", reviewer$role,
          "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#CI_RoleCode")
      party <- xml2::xml_add_child(xml2::xml_add_child(contact, "cit:party"), "cit:CI_Individual")
      iso_profile_add_text(party, "cit:name", reviewer$name)
    }
  }
  invisible(root)
}
