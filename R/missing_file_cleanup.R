# Explicit removal of obsolete inventory metadata. Never deletes dataset files.
missing_file_cleanup_rows <- function(conn, dataset_id, pond) {
  entry <- release_entry(conn, dataset_id)
  root <- release_working_root(pond, entry)
  data_root <- metadata_text(entry$data_path, file.path(root, "data"))
  if (!nzchar(data_root)) data_root <- file.path(root, "data")
  roots <- c(data_file_tbl = data_root, supporting_document_tbl = root)
  fields <- c(data_file_tbl = "data_file_rel_path_full", supporting_document_tbl = "document_rel_path")
  labels <- c(data_file_tbl = "Data file", supporting_document_tbl = "Supporting document")
  result <- data.frame(key = character(), kind = character(), id = integer(), path = character())
  for (table in names(roots)) {
    rows <- DBI::dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE dataset_id=? ORDER BY id"),
      params = list(as.integer(dataset_id)))
    for (i in seq_len(nrow(rows))) {
      relative <- gsub("\\\\", "/", as.character(rows[[fields[[table]]]][i]))
      if (is.na(relative) || !nzchar(relative) || grepl("^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)", relative))
        stop("An inventory path is invalid. Correct it before removing missing records.")
      path <- file.path(roots[[table]], relative)
      # Existing directories are logical files (e.g. geodatabases and grids).
      # Even a broken symlink is retained: it is not evidence of a deleted file.
      link <- Sys.readlink(path)
      if (file.exists(path) || (!is.na(link) && nzchar(link))) next
      parent <- dirname(path)
      while (!dir.exists(parent) && dirname(parent) != parent) parent <- dirname(parent)
      if (file.access(parent, 5L) != 0L)
        stop("A file directory cannot be read. Restore access before checking missing records.")
      result <- rbind(result, data.frame(key = paste(table, rows$id[i], sep = ":"),
        kind = labels[[table]], id = rows$id[i], path = relative))
    }
  }
  result
}

preview_missing_file_cleanup <- function(dataset_id, base_path, user_id) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  review_require_user(conn, user_id)
  release_open_review(conn, dataset_id)
  rows <- missing_file_cleanup_rows(conn, dataset_id, base_path)
  list(dataset_id = as.integer(dataset_id), pond = normalizePath(base_path),
    metadata_hash = metadata_approval_hash(conn, dataset_id), rows = rows)
}

remove_missing_file_metadata <- function(dataset_id, base_path, user_id, preview, keys) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  user <- review_require_user(conn, user_id)
  if (is.null(preview) || !identical(preview$dataset_id, as.integer(dataset_id)) ||
      !identical(preview$pond, normalizePath(base_path))) stop("Preview missing files for this dataset again.")
  keys <- unique(as.character(keys))
  if (!length(keys) || anyNA(keys) || !all(keys %in% preview$rows$key))
    stop("Select missing records from the preview.")
  lock <- release_lock(base_path, dataset_id)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  committed <- FALSE
  on.exit(if (!committed) DBI::dbRollback(conn), add = TRUE, after = FALSE)
  release_open_review(conn, dataset_id)
  if (!identical(preview$metadata_hash, metadata_approval_hash(conn, dataset_id)))
    stop("Saved metadata changed. Preview missing files again.")
  current <- missing_file_cleanup_rows(conn, dataset_id, base_path)
  if (!all(keys %in% current$key)) stop("A selected file is present again. Preview missing files again.")
  selected <- current[match(keys, current$key), , drop = FALSE]
  for (i in seq_len(nrow(selected))) {
    id <- selected$id[i]
    if (selected$kind[i] == "Supporting document") {
      DBI::dbExecute(conn, "DELETE FROM dataset_lineage_step_source_tbl WHERE dataset_id=? AND source_id IN
        (SELECT id FROM dataset_lineage_source_tbl WHERE dataset_id=? AND supporting_document_id=?)",
        params = list(as.integer(dataset_id), as.integer(dataset_id), id))
      DBI::dbExecute(conn, "DELETE FROM dataset_lineage_source_tbl WHERE dataset_id=? AND supporting_document_id=?",
        params = list(as.integer(dataset_id), id))
      DBI::dbExecute(conn, "DELETE FROM supporting_document_tbl WHERE dataset_id=? AND id=?",
        params = list(as.integer(dataset_id), id))
    } else {
      # Older extracted records can have a path but no inventory ID.
      where <- "dataset_id=? AND (data_file_id=? OR data_file_rel_path_full=?)"
      params <- list(as.integer(dataset_id), id, selected$path[i])
      DBI::dbExecute(conn, paste0("DELETE FROM dataset_quality_result_tbl WHERE dataset_id=? AND
        (data_file_id=? OR attribute_id IN (SELECT id FROM dataset_attribute_tbl WHERE ", where, "))"),
        params = c(list(as.integer(dataset_id), id), params))
      for (table in c("dataset_attribute_tbl", "data_file_spatial_metadata_tbl"))
        DBI::dbExecute(conn, paste0("DELETE FROM ", table, " WHERE ", where), params = params)
      for (table in c("data_profile_run_tbl", "dataset_manual_spatial_extent_tbl"))
        DBI::dbExecute(conn, paste0("DELETE FROM ", table, " WHERE dataset_id=? AND data_file_id=?"),
          params = list(as.integer(dataset_id), id))
      DBI::dbExecute(conn, "DELETE FROM data_file_tbl WHERE dataset_id=? AND id=?",
        params = list(as.integer(dataset_id), id))
    }
  }
  if (any(selected$kind == "Data file")) rebuild_dataset_spatial_summary_conn(conn, dataset_id, user_id)
  # Approval and prepared documents must be renewed after changing inventories.
  if (DBI::dbExistsTable(conn, "metadata_approval_tbl"))
    DBI::dbExecute(conn, "DELETE FROM metadata_approval_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))
  DBI::dbExecute(conn, "INSERT INTO metadata_review_tbl
    (dataset_id, review_timestamp, review_date, reviewer_id, update_type, update_reason,
     update_summary, metadata_section, metadata_status, source_of_change)
    VALUES (?, ?, ?, ?, 'file_inventory_refresh', 'Removed selected missing file metadata.',
      ?, 'data_files; lineage; spatial_metadata; attribute_metadata', 'needs_attention', 'missing_file_cleanup')",
    params = list(as.integer(dataset_id), release_now(), format(Sys.Date(), "%Y-%m-%d"), user$person_id[1],
      sprintf("Removed %d data-file and %d supporting-document records and their linked metadata.",
        sum(selected$kind == "Data file"), sum(selected$kind == "Supporting document"))))
  DBI::dbCommit(conn); committed <- TRUE
  invisible(nrow(selected))
}

missing_file_cleanup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(
    shiny::tags$summary("Remove missing documents and data-file records"),
    shiny::p("Check the saved dataset folder for files you have removed. Select obsolete records to remove from the working dev/src metadata before saving your review and freezing a new version."),
    shiny::actionButton(ns("preview"), "Check for missing files", class = "datapond-requires-active-user"),
    shiny::uiOutput(ns("selection")),
    shiny::helpText("Removal is saved immediately. Linked file attributes, profiles, spatial extents and quality results, and lineage sources linked to removed documents are also removed. No files are deleted. Existing frozen releases retain their original metadata."),
    shiny::actionButton(ns("remove"), "Remove selected missing records", class = "datapond-requires-active-user"),
    shiny::textOutput(ns("result"))
  )
}

missing_file_cleanup_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    preview <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal("")
    protect <- function(expr) tryCatch(force(expr), error = function(e) {
      result(conditionMessage(e))
      shiny::showNotification(conditionMessage(e), type = "error")
    })
    shiny::observeEvent(list(state$data_pond, state$current_dataset_id, state$active_user_id, state$refresh_datasets), {
      access_require("read")
      preview(NULL)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$preview, {
      access_require("edit", touch = TRUE)
      preview(NULL)
      protect({
        preview(preview_missing_file_cleanup(state$current_dataset_id, state$data_pond, state$active_user_id))
        result(if (nrow(preview()$rows)) "Select the missing records you want to remove." else "No missing file records found.")
      })
    })
    output$selection <- shiny::renderUI({
      access_require("read")
      p <- preview(); if (is.null(p) || !nrow(p$rows)) return(NULL)
      shiny::checkboxGroupInput(session$ns("keys"), "Missing records to remove",
        choices = stats::setNames(p$rows$key, paste(p$rows$kind, p$rows$path, sep = ": ")), selected = character())
    })
    shiny::observeEvent(input$remove, {
      access_require("edit", touch = TRUE)
      protect({
        count <- remove_missing_file_metadata(state$current_dataset_id, state$data_pond, state$active_user_id, preview(), input$keys)
        preview(NULL)
        for (field in c("refresh_datasets", "refresh_metadata_reviews", "refresh_lineage", "refresh_supporting_docs",
                        "refresh_data_files_saved", "refresh_attributes", "refresh_spatial_metadata", "file_metadata_cleanup_tick"))
          state[[field]] <- (state[[field]] %||% 0L) + 1L
        result(paste("Removed", count, "missing record(s) from working metadata. Save a fresh review and prepare metadata.pdf again before freezing."))
      })
    })
    output$result <- shiny::renderText({ access_require("read"); result() })
  })
}
