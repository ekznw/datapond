# Shared per-user flow controls. No credentials or remote paths are stored in
# the queue. BEGIN IMMEDIATE makes admission atomic across R processes.
movement_control <- function(directory, action) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(directory, "movement-control.sqlite"))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA busy_timeout=1000")
  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  committed <- FALSE
  on.exit(if (!committed) DBI::dbRollback(con), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS queue (
    sequence INTEGER PRIMARY KEY AUTOINCREMENT, token TEXT UNIQUE NOT NULL,
    owner_pid INTEGER NOT NULL, owner_time REAL NOT NULL,
    child_pid INTEGER, child_time REAL, active INTEGER NOT NULL DEFAULT 0)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS speed (connection TEXT PRIMARY KEY, settings TEXT NOT NULL)")
  value <- action(con)
  DBI::dbCommit(con); committed <- TRUE
  value
}

movement_speed_profile <- function(preset = "cloud", transfers = 1, checkers = 2,
                                   tps = 2, burst = 1, bandwidth = 0) {
  if (length(preset) != 1L || !preset %in% c("local", "cloud", "custom")) stop("Choose a speed profile.")
  if (preset == "local") return(list(preset = preset, transfers = 4, checkers = 4, tps = 0, burst = 1, bandwidth = 0))
  if (preset == "cloud") return(list(preset = preset, transfers = 1, checkers = 2, tps = 2, burst = 1, bandwidth = 0))
  values <- list(transfers = transfers, checkers = checkers, tps = tps, burst = burst, bandwidth = bandwidth)
  for (name in names(values)) {
    x <- values[[name]]
    integer_field <- name %in% c("transfers", "checkers", "burst")
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < (if (integer_field) 1 else 0) ||
        (integer_field && x != floor(x))) stop("Enter valid speed settings: counts must be positive whole numbers; limits must be zero or greater.")
  }
  c(list(preset = preset), lapply(values, as.numeric))
}

movement_speed_read <- function(directory) {
  movement_control(directory, function(con) {
    rows <- DBI::dbGetQuery(con, "SELECT connection, settings FROM speed ORDER BY connection")
    stats::setNames(lapply(rows$settings, function(x)
      do.call(movement_speed_profile, jsonlite::fromJSON(x))), rows$connection)
  })
}

movement_speed_save <- function(directory, connection, profile) {
  profile <- do.call(movement_speed_profile, profile)
  movement_control(directory, function(con) DBI::dbExecute(con,
    "INSERT OR REPLACE INTO speed (connection, settings) VALUES (?, ?)",
    params = list(connection, as.character(jsonlite::toJSON(profile, auto_unbox = TRUE)))))
  invisible(profile)
}

movement_step_remotes <- function(step) {
  if (step[1] == "config") return(step[3])
  n <- if (step[1] %in% c("copy", "copyto", "sync", "move", "moveto")) 2L else 1L
  paths <- utils::head(step[-1], n)
  unique(sub(":.*$", "", paths[vapply(paths, movement_remote, logical(1))]))
}

movement_step_speed <- function(step, profiles = list()) {
  remotes <- movement_step_remotes(step)
  choices <- if (length(remotes)) lapply(remotes, function(remote)
    profiles[[remote]] %||% profiles[["__cloud"]] %||% movement_speed_profile("cloud")) else
      list(profiles[["__local"]] %||% movement_speed_profile("local"))
  # For remote-to-remote flows use the stricter limit at either end. Zero means
  # unlimited for API rate and bandwidth, so it must not override a finite cap.
  strict <- function(name, unlimited = FALSE) {
    values <- vapply(choices, `[[`, numeric(1), name)
    if (unlimited) values <- values[values > 0]
    if (length(values)) min(values) else 0
  }
  list(transfers = strict("transfers"), checkers = strict("checkers"),
       tps = strict("tps", TRUE), burst = strict("burst"), bandwidth = strict("bandwidth", TRUE))
}

movement_set_flag <- function(args, flag, value) {
  at <- which(args == flag)
  if (length(at) == 1L && !any(startsWith(args, paste0(flag, "=")))) {
    args[at + 1L] <- as.character(value)
    return(args)
  }
  if (length(at)) args <- args[-unique(c(at, at + 1L))]
  args <- args[!startsWith(args, paste0(flag, "="))]
  c(args, flag, as.character(value))
}

movement_apply_speed <- function(step, profiles = list()) {
  speed <- movement_step_speed(step, profiles)
  flags <- list("--transfers" = speed$transfers, "--checkers" = speed$checkers,
    "--tpslimit" = speed$tps, "--tpslimit-burst" = speed$burst,
    "--bwlimit" = if (speed$bandwidth > 0) paste0(speed$bandwidth, "M") else "0")
  for (flag in names(flags)) step <- movement_set_flag(step, flag, flags[[flag]])
  for (flag in c("--retries", "--low-level-retries")) {
    if (!flag %in% step) step <- c(step, flag, if (flag == "--retries") "3" else "10")
  }
  # Native pacing messages are DEBUG-level. Keep config credentials out of
  # debug output; ordinary cloud jobs are filtered to useful messages in the UI.
  if (length(movement_step_remotes(step)) && step[1] != "config")
    step <- movement_set_flag(step, "--log-level", "DEBUG")
  if (step[1] == "config") step <- movement_set_flag(step, "--log-level", "INFO")
  step
}

movement_process_alive <- function(pid, time) {
  if (is.na(pid) || is.na(time)) return(FALSE)
  tryCatch(ps::ps_is_running(ps::ps_handle(pid, as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))),
    no_such_process = function(e) FALSE, zombie_process = function(e) FALSE,
    # If permissions prevent inspection, keep the slot occupied rather than
    # risk admitting another process while its predecessor is still alive.
    error = function(e) TRUE)
}

movement_queue_prune <- function(con) {
  rows <- DBI::dbGetQuery(con, "SELECT * FROM queue")
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    if (!movement_process_alive(row$owner_pid, row$owner_time) &&
        !movement_process_alive(row$child_pid, row$child_time))
      DBI::dbExecute(con, "DELETE FROM queue WHERE token = ?", params = list(row$token))
  }
}

movement_queue_add <- function(job) {
  job$ticket <- new_uuid()
  movement_control(job$control_directory, function(con) {
    movement_queue_prune(con)
    DBI::dbExecute(con, "INSERT INTO queue (token, owner_pid, owner_time) VALUES (?, ?, ?)",
      params = list(job$ticket, Sys.getpid(), as.numeric(ps::ps_create_time(ps::ps_handle()))))
  })
}

movement_queue_release <- function(job) {
  if (is.null(job$ticket)) return(invisible(NULL))
  movement_control(job$control_directory, function(con)
    DBI::dbExecute(con, "DELETE FROM queue WHERE token = ?", params = list(job$ticket)))
  job$ticket <- NULL
  invisible(NULL)
}

movement_queue_admit <- function(job) {
  movement_control(job$control_directory, function(con) {
    movement_queue_prune(con)
    rows <- DBI::dbGetQuery(con, "SELECT token FROM queue ORDER BY sequence")
    position <- match(job$ticket, rows$token)
    if (is.na(position)) stop("This cloud queue entry is no longer available. Start the operation again.")
    job$queue_position <- position
    if (position != 1L) return(FALSE)
    # Launch and register the child while holding the admission transaction.
    movement_job_launch(job)
    DBI::dbExecute(con, "UPDATE queue SET active = 1, child_pid = ?, child_time = ? WHERE token = ?",
      params = list(job$process$get_pid(), tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle(job$process$get_pid()))),
        error = function(e) {
          if (!job$process$is_alive()) NA_real_ else stop(e)
        }), job$ticket))
    TRUE
  })
}

movement_throttle_message <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  # Do not treat a filename containing '429' as an HTTP response.
  pattern <- "pacer:.*(rate limited|low level retry)|[Rr]etry-[Aa]fter|[Tt]oo [Mm]any [Rr]equests|activityLimitReached|[Tt]hrottl|(^|[^[:alnum:]])(429|503) (Too|Service)|HTTP[^\n]*(429|503)"
  if (any(grepl(pattern, lines, ignore.case = TRUE)))
    "Cloud throttling / retry reported. rclone handles backoff and service Retry-After waits; allow it to continue rather than restarting."
  else ""
}

movement_job_feedback <- function(job) {
  if (is.null(job) || job$status != "running") return("")
  if (identical(job$phase, "queued")) return(paste0("Waiting in the shared cloud queue (position ", job$queue_position,
    "). Another app session may be using the connection. You can cancel while waiting."))
  job$throttle %||% ""
}
