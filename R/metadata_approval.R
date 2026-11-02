# Approval belongs to the saved metadata in this pond, not just an old status
# label. Keep the evidence local: imported/restored edits need a fresh review.
ensure_metadata_approval_table <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS metadata_approval_tbl (
    review_id INTEGER PRIMARY KEY, dataset_id INTEGER NOT NULL,
    metadata_hash TEXT NOT NULL,
    FOREIGN KEY (review_id) REFERENCES metadata_review_tbl(id),
    FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id))")
  invisible(TRUE)
}

metadata_approval_hash <- function(conn, dataset_id) {
  tables <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")$name
  tables <- tables[vapply(tables, function(table) "dataset_id" %in% DBI::dbListFields(conn, table), logical(1))]
  tables <- setdiff(tables, c(release_control_tables(), "metadata_review_tbl",
    "dataset_review_exchange_tbl", "dataset_import_tbl"))
  scoped <- stats::setNames(lapply(tables, transfer_table_rows, conn = conn, dataset_id = dataset_id), tables)
  entry <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id=?", params = list(as.integer(dataset_id)))
  ids <- c(list(entry), scoped)
  people <- transfer_reference_rows(conn, "persons_tbl", transfer_ids_from_columns(ids, "person_id$"))
  organisations <- transfer_reference_rows(conn, "organisation_tbl",
    transfer_ids_from_columns(c(ids, list(people)), "organisation_id$"))
  repeat {
    parents <- setdiff(na.omit(organisations$parent_organisation_id), organisations$id)
    if (!length(parents)) break
    organisations <- rbind(organisations, transfer_reference_rows(conn, "organisation_tbl", parents))
  }
  addresses <- if (DBI::dbExistsTable(conn, "party_address_tbl")) DBI::dbReadTable(conn, "party_address_tbl") else data.frame()
  if (nrow(addresses)) addresses <- addresses[
    addresses$person_id %in% people$id | addresses$organisation_id %in% organisations$id, , drop = FALSE]
  payload <- list(entry_tbl = entry, scoped = scoped, persons_tbl = people,
    organisation_tbl = organisations, party_address_tbl = addresses,
    user_tbl = data.frame(),
    keyword_thesaurus_tbl = transfer_reference_rows(conn, "keyword_thesaurus_tbl",
      transfer_ids_from_columns(ids, "thesaurus_id$")),
    license_template_tbl = transfer_reference_rows(conn, "license_template_tbl",
      transfer_ids_from_columns(ids, "license_template_id$")))
  review_fingerprint(payload)
}

release_metadata_gate <- function(dataset_id, base_path, conn, population_profile) {
  population_profile <- match.arg(population_profile, c("full", "rough"))
  status <- get_dataset_completion_status(dataset_id, base_path,
    dataset_overview_schema(), population_profile = population_profile)
  required <- dataset_status_requirement_level(status$section, population_profile) == "essential"
  incomplete <- required & !status$status %in% c("complete", "not_applicable")
  missing <- status[incomplete, ]
  label <- if (population_profile == "rough") "Essential" else "Full"
  if (!nrow(status) || nrow(missing)) stop(paste0(
    label, " metadata is incomplete. Complete these sections before freezing: ",
    paste(paste0(missing$section, ifelse(nzchar(missing$missing_fields),
      paste0(" (", missing$missing_fields, ")"), "")), collapse = "; "), "."), call. = FALSE)
  review <- DBI::dbGetQuery(conn, "SELECT r.*, p.name_first, p.name_last
    FROM metadata_review_tbl r LEFT JOIN persons_tbl p ON p.id=r.reviewer_id
    WHERE r.dataset_id=? ORDER BY r.review_timestamp DESC, r.id DESC LIMIT 1",
    params = list(as.integer(dataset_id)))
  if (nrow(review) != 1L || !identical(review$metadata_status[1], "approved") ||
      is.na(review$reviewer_id[1]) ||
      !any(vapply(review[c("name_first", "name_last")], metadata_present, logical(1))))
    stop("A named reviewer must save the latest metadata review with status Approved before freezing.", call. = FALSE)
  evidence <- if (DBI::dbExistsTable(conn, "metadata_approval_tbl")) DBI::dbGetQuery(conn,
    "SELECT metadata_hash FROM metadata_approval_tbl WHERE review_id=? AND dataset_id=?",
    params = list(review$id[1], as.integer(dataset_id))) else data.frame()
  if (nrow(evidence) != 1L || !identical(evidence$metadata_hash[1], metadata_approval_hash(conn, dataset_id)))
    stop("Metadata approval is missing or out of date. Have a reviewer approve the current saved metadata before freezing.", call. = FALSE)
  list(population_profile = population_profile, review_id = review$id[1],
    reviewer = trimws(paste(review$name_first[1], review$name_last[1])),
    reviewed_at = review$review_timestamp[1], metadata_hash = evidence$metadata_hash[1])
}
