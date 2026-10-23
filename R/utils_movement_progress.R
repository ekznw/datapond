# Presentation only: rclone retains control of transfers, pacing and retries.
movement_parse_progress <- function(text) {
  lines <- strsplit(gsub("\r", "\n", text), "\n", fixed = TRUE)[[1]]
  result <- list(bytes = NULL, files = NULL, checks = NULL, elapsed = NULL,
    rate = NULL, eta = NULL, percent = NULL, active = character(), events = character())
  in_files <- FALSE
  context <- ""
  for (line in lines) {
    plain <- trimws(sub("^.*? (DEBUG|INFO|NOTICE|WARNING|ERROR|CRITICAL)\\s*:\\s*", "", line, perl = TRUE))
    if (grepl("^\\[[0-9]+/[0-9]+\\]", plain)) { context <- plain; in_files <- FALSE; next }
    if (!nzchar(plain)) { in_files <- FALSE; next }
    # Both multiline Transferred: and --stats-one-line output are supported.
    bytes <- sub("^Transferred:\\s*", "", plain)
    match <- regmatches(bytes, regexec("^([0-9.]+ [[:alpha:]]+ / [0-9.]+ [[:alpha:]]+), ([0-9.]+%|-), (.*?), ETA (.*)$", bytes))[[1]]
    if (length(match)) {
      result$bytes <- match[2]; result$rate <- match[4]; result$eta <- match[5]
      result$percent <- if (match[3] == "-") NULL else max(0, min(100, as.numeric(sub("%", "", match[3]))))
      result$active <- character(); result$files <- NULL; result$checks <- NULL; result$elapsed <- NULL
      in_files <- FALSE
      next
    }
    if (grepl("^Transferred:\\s*[0-9]+ / [0-9]+", plain)) {
      result$files <- sub("^Transferred:\\s*", "", plain); next
    }
    if (grepl("^Checks:", plain)) { result$checks <- sub("^Checks:\\s*", "", plain); next }
    if (grepl("^Elapsed time:", plain)) { result$elapsed <- sub("^Elapsed time:\\s*", "", plain); next }
    if (plain %in% c("Transferring:", "Checking:")) { in_files <- TRUE; next }
    if (in_files && grepl("^[*] ", plain)) {
      result$active <- c(result$active, sub("^[*] ", "", plain)); next
    }
    in_files <- FALSE
    if (grepl("^(Listed|Deleted|Renamed):", plain) || grepl("^Errors:\\s*0([[:space:]]|$)", plain)) next
    result$events <- c(result$events, paste0(if (nzchar(context)) paste0(context, " | ") else "", line))
  }
  result$events <- utils::tail(result$events, 60L)
  result
}

movement_job_speed_summary <- function(job) {
  speeds <- unique(vapply(job$steps, function(step) {
    value <- function(flag) if (flag %in% step) step[match(flag, step) + 1L] else "unknown"
    paste0(value("--transfers"), " transfer(s), ", value("--checkers"), " check(s), API ",
      if (value("--tpslimit") == "0") "unlimited" else paste0(value("--tpslimit"), "/s"),
      ", burst ", value("--tpslimit-burst"), ", bandwidth ",
      if (value("--bwlimit") == "0") "unlimited" else paste0(sub("M$", "", value("--bwlimit")), " MiB/s"))
  }, character(1)))
  paste("Engine: rclone. Operation settings:", paste(speeds, collapse = "; "))
}

movement_job_report <- function(job, log = "", current_log = "", private = FALSE) {
  if (is.null(job) || !length(job$steps)) return(NULL)
  index <- min(max(1L, job$index), length(job$steps))
  step <- job$steps[[index]]
  preview <- "--dry-run" %in% step
  transfer <- step[1] %in% c("copy", "copyto", "sync")
  list(kind = if (private) "Connection / folder listing" else if (preview) "Preview \u2014 no destination changes" else
      if (transfer) "Live transfer \u2014 writes to destination" else "rclone operation",
    preview = preview, private = private, status = job$status, queued = identical(job$phase, "queued"),
    speed_summary = movement_job_speed_summary(job),
    started = format(job$started %||% Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    step = index, total = length(job$steps), label = if (length(job$labels) >= index) job$labels[index] else paste("Step", index),
    source = if (!private && transfer) step[2] else NULL,
    destination = if (!private && transfer) step[3] else NULL,
    progress = movement_parse_progress(if (private) "" else current_log),
    events = if (private) character() else movement_parse_progress(log)$events)
}

movement_progress_ui <- function(report) {
  if (is.null(report)) return(shiny::p(class = "help-block", "Operation monitor: progress and file actions will appear here when you preview or run a flow."))
  status <- if (report$queued) "Waiting in queue" else switch(report$status,
    running = "Running", complete = "Completed", cancelled = "Cancelled", failed = "Failed", report$status)
  progress <- report$progress
  metric <- function(label, value) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    shiny::div(class = "flow-progress-metric", shiny::tags$small(label), shiny::tags$strong(value))
  }
  shiny::div(class = "flow-progress-card",
    shiny::h4(report$kind),
    shiny::p(shiny::strong(status), " \u00b7 ", report$label, " \u00b7 Step ", report$step, " of ", report$total),
    shiny::p(class = "flow-field-help", "Source: rclone on the computer running DataPond. Operation started ", report$started,
      ". Counts below describe the displayed step, not the whole batch."),
    if (!is.null(report$source)) shiny::div(class = "flow-progress-paths",
      shiny::div(shiny::strong("From: "), report$source), shiny::div(shiny::strong("To: "), report$destination)),
    if (report$preview) shiny::p(class = "flow-field-help", "A dry run compares files and reports proposed actions. Any byte/file totals are estimates; no destination files are copied or deleted."),
    if (!report$queued && !report$private) shiny::tagList(
      if (!is.null(progress$percent)) shiny::div(class = "progress", shiny::div(
        class = "progress-bar", role = "progressbar", `aria-label` = if (report$preview) "Preview estimate" else "Step bytes transferred",
        `aria-valuemin` = "0", `aria-valuemax` = "100", `aria-valuenow` = progress$percent,
        style = paste0("width:", progress$percent, "%;"), paste0(progress$percent, "%"))),
      shiny::div(class = "flow-progress-metrics",
        metric(if (report$preview) "Estimated bytes" else "Bytes transferred", progress$bytes),
        metric(if (report$preview) "Planned files" else "Files transferred", progress$files),
        metric("Checks / listed", progress$checks), metric("Elapsed in step", progress$elapsed),
        if (!report$preview && report$status == "running") metric("Speed", progress$rate),
        if (!report$preview && report$status == "running") metric("Estimated remaining", progress$eta)),
      if (is.null(progress$bytes)) shiny::p(class = "flow-field-help", "No transfer totals reported for this step yet. Scanning or checking can take time before totals are known."),
      if (length(progress$active)) shiny::tags$details(open = "open", shiny::tags$summary("Files in the latest progress report"),
        shiny::tags$ul(lapply(progress$active, shiny::tags$li))),
      shiny::p(class = "flow-field-help", "Byte percentage and file percentage measure different things. File names may be shortened by rclone. The latest reported totals replace earlier snapshots.")
    )
  )
}
