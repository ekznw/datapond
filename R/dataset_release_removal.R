# Remove only an explicitly selected superseded ZIP. Keep the release identity,
# metadata snapshots, manifests and audit events; never reuse its version number.
release_removal_user <- function(conn, user_id) {
  user <- review_require_user(conn, user_id)
  if (!isTRUE(as.integer(user$is_admin[1]) == 1L)) stop("A pond administrator must remove frozen release ZIPs.")
  if (!is.null(shiny::getDefaultReactiveDomain())) access_require("admin")
  invisible(user)
}

release_removal_paths <- function(pond, row) {
  id <- row$release_id[1]
  root <- release_registered_folder(pond, row)
  pond <- normalizePath(pond, winslash = "/", mustWork = TRUE)
  # Even links pointing inside the pond could alias a retained archive.
  components <- c(file.path(pond, "arc"), release_store(pond), root,
    file.path(root, "dataset.zip"), file.path(root, "release.json"),
    file.path(pond, ".metadata_pond"),
    release_private(pond), release_private(pond, id))
  links <- Sys.readlink(components)
  if (any(!is.na(links) & nzchar(links))) stop("Release removal refuses symbolic links.")
  paths <- release_paths(pond, row)
  paths$journal <- file.path(dirname(paths$snapshot), "remove-operation.json")
  paths
}

release_removal_candidate <- function(conn, row, pond) {
  edition_protect_release(conn, row$release_id[1])
  if (!identical(row$state[1], "frozen")) stop("This release ZIP is removed or awaiting removal recovery.")
  paths <- release_removal_paths(pond, row)
  release_open_review(conn, row$dataset_id[1])
  retained <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_tbl
    WHERE dataset_id=? AND state='frozen' ORDER BY frozen_at DESC,rowid DESC LIMIT 1",
    params = list(row$dataset_id[1]))
  if (nrow(retained) != 1L || identical(retained$release_id[1], row$release_id[1]))
    stop("Keep the newest frozen release. Select a superseded release instead.")
  records <- DBI::dbGetQuery(conn, "SELECT dataset_path,data_path FROM entry_tbl")
  values <- c(records$dataset_path, records$data_path)
  values <- values[!is.na(values) & nzchar(values)]
  values <- vapply(values, normalizePath, character(1), winslash = "/", mustWork = FALSE)
  if (any(values == paths$root | startsWith(values, paste0(paths$root, "/"))))
    stop("This release backs a current archived dataset. Restore or change that working state before removal.")
  checkpoints <- dataset_release_checkpoints(row$dataset_id[1], pond)
  if (nrow(checkpoints)) {
    events <- DBI::dbGetQuery(conn, "SELECT operation_id FROM dataset_release_event_tbl WHERE release_id=?",
      params = list(row$release_id[1]))
    if (any(checkpoints$operation_id %in% events$operation_id))
      stop("A working-draft recovery checkpoint depends on this release; its ZIP must be retained.")
  }
  list(paths = paths, retained = retained)
}

#' @export
#' @noRd
preview_dataset_release_removal <- function(release_id, base_path, user_id) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  release_removal_user(conn, user_id)
  row <- release_row(conn, release_id)
  lock <- release_lock(base_path, row$dataset_id[1]); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_removal_preview(conn, row, base_path)
}

release_removal_preview <- function(conn, row, pond) {
  candidate <- release_removal_candidate(conn, row, pond)
  verify_dataset_release(row$release_id[1], pond)
  verify_dataset_release(candidate$retained$release_id[1], pond)
  list(release_id = row$release_id[1], dataset_id = row$dataset_id[1], version = row$version[1],
    source = candidate$paths$zip, bytes = unname(file.info(candidate$paths$zip)$size),
    record_hash = review_hash(row), retained_release_id = candidate$retained$release_id[1],
    retained_version = candidate$retained$version[1], retained_hash = review_hash(candidate$retained))
}

release_unlink_zip <- function(path) unlink(path, expand = FALSE)

release_finish_removal <- function(conn, pond, row, op) {
  paths <- release_removal_paths(pond, row)
  if (!identical(op$release_id, row$release_id[1]) ||
      !identical(as.integer(op$dataset_id), as.integer(row$dataset_id[1])) ||
      !identical(op$zip_sha256, row$zip_sha256[1])) stop("Invalid release removal journal.")
  event <- DBI::dbGetQuery(conn, "SELECT id FROM dataset_release_event_tbl
    WHERE operation_id=? AND release_id=? AND dataset_id=? AND action='remove_release_zip'",
    params = list(op$operation_id, row$release_id[1], row$dataset_id[1]))
  if (nrow(event) != 1L || !row$state[1] %in% c("removing", "removed"))
    stop("Release removal has not been committed.")
  if (file.exists(paths$zip)) {
    if (dir.exists(paths$zip) || !identical(release_sha(paths$zip), row$zip_sha256[1]))
      stop("Release ZIP changed before removal; it has been retained for inspection.")
    release_unlink_zip(paths$zip)
    if (file.exists(paths$zip)) stop("Could not remove the release ZIP. Use Recover interrupted lifecycle operation to retry.")
  }
  descriptor_path <- file.path(paths$root, "release.json")
  link <- Sys.readlink(descriptor_path)
  if (!is.na(link) && nzchar(link)) stop("Release descriptor cannot be a symbolic link.")
  descriptor <- jsonlite::read_json(descriptor_path)
  if (!identical(descriptor$release_id, row$release_id[1])) stop("Release descriptor identity differs.")
  descriptor$state <- "removed"
  descriptor$removed_at <- op$removed_at
  descriptor$removal_note <- op$note
  release_operation_write(descriptor, descriptor_path)
  DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET state='removed' WHERE release_id=?",
    params = list(row$release_id[1]))
  op$phase <- "complete"
  release_operation_write(op, paths$journal)
  invisible(row$release_id[1])
}

#' @export
#' @noRd
remove_dataset_release <- function(release_id, base_path, user_id, note, preview) {
  if (!metadata_present(note)) stop("Record why this frozen release ZIP is being removed.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_removal_user(conn, user_id)
  row <- release_row(conn, release_id)
  lock <- release_lock(base_path, row$dataset_id[1]); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  op <- NULL
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, "UPDATE entry_tbl SET id=id WHERE id=?", params = list(row$dataset_id[1]))
    release_removal_user(conn, user_id)
    row <- release_row(conn, release_id)
    current <- release_removal_preview(conn, row, base_path)
    if (missing(preview) || !identical(preview, current))
      stop("Release removal preview changed. Preview the selected release again.")
    paths <- release_removal_paths(base_path, row)
    op <- list(operation_id = uuid::UUIDgenerate(), release_id = release_id,
      dataset_id = row$dataset_id[1], zip_sha256 = row$zip_sha256[1],
      user_id = as.integer(user_id), note = note, removed_at = release_now(), phase = "prepared")
    release_operation_write(op, paths$journal)
    DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET state='removing' WHERE release_id=?", params = list(release_id))
    release_event(conn, row$dataset_id[1], release_id, op$operation_id, "remove_release_zip", user_id, note)
  })
  # Deletion occurs only after the intent and audit event commit. Recovery can
  # complete any interrupted unlink/descriptor update without deleting metadata.
  release_finish_removal(conn, base_path, release_row(conn, release_id), op)
  invisible(list(release_id = release_id, bytes_removed = preview$bytes))
}

release_recover_removals <- function(conn, pond, dataset_id, user_id) {
  messages <- character()
  for (path in release_journals(release_private(pond), "remove-operation.json")) {
    op <- jsonlite::read_json(path)
    if (op$dataset_id != dataset_id || op$phase %in% c("complete", "rolled_back")) next
    release_removal_user(conn, user_id)
    row <- release_row(conn, op$release_id)
    paths <- release_removal_paths(pond, row)
    if (!identical(normalizePath(path), normalizePath(paths$journal))) stop("Invalid removal journal location.")
    if (identical(row$state[1], "frozen")) {
      # The intent transaction rolled back. No archive bytes have been touched.
      op$phase <- "rolled_back"; release_operation_write(op, paths$journal)
      messages <- c(messages, paste("Cancelled uncommitted removal of", row$version[1]))
    } else {
      release_finish_removal(conn, pond, row, op)
      messages <- c(messages, paste("Completed release ZIP removal for", row$version[1]))
    }
  }
  messages
}
