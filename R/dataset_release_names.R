# Public folders are readable; the immutable release ID still disambiguates them.
# Private snapshots and journals remain keyed by that ID.
release_folder_name <- function(record, version, release_id) {
  slug <- function(value, fallback, limit) {
    value <- iconv(metadata_text(value, fallback), to = "ASCII//TRANSLIT", sub = "")
    value <- tolower(gsub("[^A-Za-z0-9._-]+", "_", value))
    value <- gsub("^[._-]+|[._-]+$", "", substr(value, 1L, limit))
    if (!nzchar(value)) fallback else value
  }
  key <- metadata_text(record$folder_key, metadata_text(record$title, "dataset"))
  paste0(slug(key, "dataset", 80L), "__v", slug(version, "unknown", 30L), "__", release_id)
}

release_registered_folder <- function(pond, row) {
  id <- row$release_id[1]
  relative <- row$relative_path[1]
  name <- basename(relative)
  if (length(id) != 1L || is.na(id) || !grepl("^[A-Za-z0-9_-]+$", id) ||
      length(relative) != 1L || is.na(relative) ||
      !identical(relative, paste0("arc/releases/", name)) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", name) ||
      !(identical(name, id) || endsWith(name, paste0("__", id)))) {
    stop("Release operation requires the registered arc/releases directory.")
  }
  file.path(normalizePath(pond, winslash = "/", mustWork = TRUE), relative)
}

rename_dataset_release_folder <- function(release_id, base_path, user_id) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  row <- release_row(conn, release_id)
  release_require_user(conn, user_id)
  lock <- release_lock(base_path, row$dataset_id[1]); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_open_review(conn, row$dataset_id[1])
  # Reuse strict path and symlink checks, without applying ZIP-removal policy.
  paths <- release_removal_paths(base_path, row)
  verify_dataset_release(release_id, base_path)
  record <- release_read_payload(paths$snapshot)$entry_tbl
  new_relative <- paste0("arc/releases/", release_folder_name(record, row$version[1], release_id))
  if (identical(row$relative_path[1], new_relative)) return(invisible(paths$root))
  next_row <- row; next_row$relative_path <- new_relative
  destination <- release_registered_folder(base_path, next_row)
  link <- Sys.readlink(destination)
  if (file.exists(destination) || (!is.na(link) && nzchar(link))) stop("The readable release folder already exists.")
  op <- list(operation_id = uuid::UUIDgenerate(), dataset_id = row$dataset_id[1],
    release_id = release_id, old_relative = row$relative_path[1], new_relative = new_relative,
    phase = "prepared")
  journal <- file.path(dirname(paths$snapshot), "rename-operation.json")
  release_operation_write(op, journal)
  committed <- FALSE
  moved <- FALSE
  tryCatch({
    DBI::dbWithTransaction(conn, {
      # Take the write lock and recheck the path before moving the directory.
      DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET relative_path=relative_path WHERE release_id=?", params = list(release_id))
      if (!identical(release_row(conn, release_id)$relative_path[1], op$old_relative)) stop("Release location changed; retry.")
      if (!file.rename(paths$root, destination)) stop("Could not rename the release folder.")
      moved <- TRUE
      DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET relative_path=? WHERE release_id=?",
        params = list(new_relative, release_id))
      entry <- release_entry(conn, row$dataset_id[1])
      for (field in c("dataset_path", "data_path")) {
        value <- metadata_text(entry[[field]])
        if (identical(value, paths$root) || startsWith(value, paste0(paths$root, "/"))) {
          value <- paste0(destination, substring(value, nchar(paths$root) + 1L))
          DBI::dbExecute(conn, paste0("UPDATE entry_tbl SET ", field, "=? WHERE id=?"),
            params = list(value, row$dataset_id[1]))
        }
      }
      release_event(conn, row$dataset_id[1], release_id, op$operation_id, "rename_release_folder", user_id,
        paste(op$old_relative, "->", new_relative))
    })
    committed <- TRUE
    verify_dataset_release(release_id, base_path)
    op$phase <- "complete"; release_operation_write(op, journal)
    invisible(destination)
  }, error = function(e) {
    if (!committed) {
      if (moved && !file.rename(destination, paths$root)) stop("Release rename needs recovery: ", conditionMessage(e))
      op$phase <- "rolled_back"; release_operation_write(op, journal)
    }
    stop(conditionMessage(e), call. = FALSE)
  })
}

release_recover_renames <- function(conn, pond, dataset_id) {
  messages <- character()
  for (file in release_journals(release_private(pond), "rename-operation.json")) {
    op <- jsonlite::read_json(file)
    if (op$dataset_id != dataset_id || op$phase %in% c("complete", "rolled_back")) next
    row <- release_row(conn, op$release_id)
    if (row$dataset_id[1] != dataset_id ||
        !identical(normalizePath(dirname(file)), normalizePath(release_private(pond, op$release_id)))) {
      stop("Invalid release rename journal.")
    }
    old_row <- new_row <- row
    old_row$relative_path <- op$old_relative; new_row$relative_path <- op$new_relative
    old <- release_registered_folder(pond, old_row); new <- release_registered_folder(pond, new_row)
    # Check both possible locations before any recovery move.
    release_removal_paths(pond, old_row); release_removal_paths(pond, new_row)
    if (identical(row$relative_path[1], op$new_relative)) {
      if (file.exists(old) || !dir.exists(new)) stop("Inspect the interrupted release rename folders before recovery.")
      op$phase <- "complete"
    } else if (identical(row$relative_path[1], op$old_relative)) {
      if (file.exists(new)) {
        if (file.exists(old) || !file.rename(new, old)) stop("Could not roll back the release folder rename.")
      }
      op$phase <- "rolled_back"
    } else stop("Release location changed after an interrupted rename.")
    verify_dataset_release(op$release_id, pond)
    release_operation_write(op, file)
    messages <- c(messages, paste("Recovered release folder rename:", op$phase))
  }
  messages
}
