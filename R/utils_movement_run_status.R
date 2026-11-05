# Keep one compact outcome per saved flow and per pond publication. Raw rclone
# logs remain temporary; only actionable errors from the latest run are saved.
movement_run_status_read <- function(directory, kind, keys) {
  if (!length(keys) || !file.exists(file.path(directory, "movement-control.sqlite"))) return(list())
  movement_control(directory, function(con) {
    if (!DBI::dbExistsTable(con, "run_status")) return(list())
    rows <- DBI::dbGetQuery(con, "SELECT kind, item_key, run_id, started, status, errors, owner_pid, owner_time FROM run_status WHERE kind = ?",
      params = list(kind))
    rows <- rows[rows$item_key %in% keys, , drop = FALSE]
    stats::setNames(lapply(seq_len(nrow(rows)), function(i) {
      row <- as.list(rows[i, , drop = FALSE])
      if (row$status == "running" && !movement_process_alive(row$owner_pid, row$owner_time))
        row$status <- "cancelled"
      row
    }), rows$item_key)
  })
}

movement_run_status_write <- function(directory, kind, key, run_id, started, status, errors = "") {
  movement_control(directory, function(con) {
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS run_status (
      kind TEXT NOT NULL, item_key TEXT NOT NULL, run_id TEXT NOT NULL,
      started REAL NOT NULL, status TEXT NOT NULL, errors TEXT NOT NULL DEFAULT '',
      owner_pid INTEGER, owner_time REAL,
      PRIMARY KEY (kind, item_key))")
    if (status == "running") {
      DBI::dbExecute(con, "INSERT INTO run_status (kind, item_key, run_id, started, status, errors, owner_pid, owner_time)
        VALUES (?, ?, ?, ?, ?, '', ?, ?) ON CONFLICT(kind, item_key) DO UPDATE SET
        run_id=excluded.run_id, started=excluded.started, status=excluded.status, errors='',
        owner_pid=excluded.owner_pid, owner_time=excluded.owner_time",
        params = list(kind, key, run_id, as.numeric(started), status, Sys.getpid(),
          as.numeric(ps::ps_create_time(ps::ps_handle()))))
    } else {
      DBI::dbExecute(con, "UPDATE run_status SET status = ?, errors = ?
        WHERE kind = ? AND item_key = ? AND run_id = ?",
        params = list(status, errors, kind, key, run_id))
    }
  })
  invisible(NULL)
}

movement_run_status_delete <- function(directory, kind, keys) {
  if (!length(keys) || !file.exists(file.path(directory, "movement-control.sqlite"))) return(invisible(NULL))
  movement_control(directory, function(con) {
    if (DBI::dbExistsTable(con, "run_status")) for (key in keys)
      DBI::dbExecute(con, "DELETE FROM run_status WHERE kind = ? AND item_key = ?",
        params = list(kind, key))
  })
  invisible(NULL)
}

movement_run_errors <- function(paths, fallback = NULL) {
  errors <- character(); count <- 0L; ignored <- 0L
  for (path in paths) {
    if (!file.exists(path)) next
    con <- file(path, "r", encoding = "UTF-8")
    tryCatch(repeat {
      lines <- readLines(con, n = 1000L, warn = FALSE)
      if (!length(lines)) break
      lines <- lines[grepl(" (ERROR|CRITICAL) *:", lines)]
      for (line in lines) {
        message <- sub("^.*? (ERROR|CRITICAL) *: *", "", line)
        if (grepl("^Attempt [0-9]+/[0-9]+ failed with [0-9]+ errors? and:", message)) next
        known_file <- grepl(paste0("\\.(", paste(movement_sharepoint_compat_extensions(), collapse = "|"),
          ")(:| |$)"), message, ignore.case = TRUE)
        known_change <- grepl("corrupt(ed)? on transfer|sizes? differ|checksum|hash(es)? (differ|differs|mismatch)|size mismatch", message, ignore.case = TRUE)
        if (known_file && known_change) { ignored <- ignored + 1L; next }
        count <- count + 1L
        message <- sub(" src\\(.*$", "", message)
        if (length(errors) < 3L && !message %in% errors)
          errors <- c(errors, substr(message, 1L, 240L))
      }
    }, finally = close(con))
  }
  if (!count && !ignored && !is.null(fallback) && nzchar(fallback)) {
    count <- 1L; errors <- substr(fallback, 1L, 240L)
  }
  list(count = count, ignored = ignored,
       summary = if (count) paste0(count, " error", if (count != 1L) "s", ": ", paste(errors, collapse = " | ")) else "")
}

movement_run_status_label <- function(row) {
  if (is.null(row)) return(c("No recorded run", "No run history"))
  c(format(as.POSIXct(row$started, origin = "1970-01-01"), "%Y-%m-%d %H:%M %Z"),
    paste0(switch(row$status, running = "Running", complete = "Completed", failed = "Failed",
      failed_known = "Failed on known document conversion errors", cancelled = "Interrupted", row$status),
      if (nzchar(row$errors)) paste0(" — ", row$errors) else " — No actionable errors"))
}
