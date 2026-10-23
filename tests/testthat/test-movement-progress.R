test_that("repeated rclone snapshots become current totals without hiding warnings", {
  log <- paste(c(
    "[1/2] Soils",
    "2026/10/23 12:24:04 INFO :",
    "Transferred: 1.076 GiB / 1.099 GiB, 98%, 2.084 MiB/s, ETA 11s",
    "Checks: 0 / 0, -, Listed 106",
    "Transferred: 77 / 91, 85%",
    "Elapsed time: 7m37.0s",
    "Transferring:",
    " * old.tif: 51% /10.286 MiB, 3.965 MiB/s, 1s",
    "2026/10/23 12:24:05 INFO : done.tif: Copied (new)",
    "2026/10/23 12:24:05 ERROR : HTTP 429: retrying",
    "2026/10/23 12:24:05 DEBUG : pacer: Rate limited, increasing sleep to 1s",
    "2026/10/23 12:24:06 INFO :",
    "Transferred: 1.080 GiB / 1.099 GiB, 98%, 2.187 MiB/s, ETA 8s",
    "Checks: 0 / 0, -, Listed 106",
    "Transferred: 78 / 91, 86%",
    "Elapsed time: 7m39.0s",
    "Errors: 0",
    "Transferring:",
    " * soil.tif: 97% /10.286 MiB, 3.333 MiB/s, 0s"
  ), collapse = "\n")
  value <- movement_parse_progress(log)
  expect_identical(value$bytes, "1.080 GiB / 1.099 GiB")
  expect_equal(value$percent, 98)
  expect_identical(value$files, "78 / 91, 86%")
  expect_identical(value$checks, "0 / 0, -, Listed 106")
  expect_identical(value$elapsed, "7m39.0s")
  expect_identical(value$eta, "8s")
  expect_identical(value$rate, "2.187 MiB/s")
  expect_length(value$active, 1L)
  expect_match(value$active, "^soil.tif")
  expect_length(value$events, 3L)
  expect_true(all(grepl("[1/2] Soils | ", value$events, fixed = TRUE)))
  expect_match(value$events[2], "ERROR : HTTP 429")
  expect_match(value$events[3], "pacer: Rate limited")
})

test_that("single-line previews retain proposed changes and unknown totals", {
  log <- paste(c(
    "2026/10/23 12:33:10 NOTICE: sample: Skipped copy as --dry-run is set (size 3)",
    "2026/10/23 12:33:10 NOTICE: extra: Skipped delete as --dry-run is set",
    "2026/10/23 12:33:10 NOTICE:           3 B / 3 B, 100%, 0 B/s, ETA -"
  ), collapse = "\n")
  value <- movement_parse_progress(log)
  expect_equal(value$percent, 100)
  expect_identical(value$bytes, "3 B / 3 B")
  expect_length(value$events, 2L)
  expect_match(value$events[1], "Skipped copy")
  expect_match(value$events[2], "Skipped delete")
  unknown <- movement_parse_progress("2026/10/23 12:33:10 INFO : 0 B / 0 B, -, 0 B/s, ETA -")
  expect_null(unknown$percent)
  expect_length(unknown$events, 0L)
  expect_null(movement_parse_progress("")$bytes)
  expect_match(movement_parse_progress("Errors: 1 (retrying)")$events, "Errors: 1")
})

test_that("operation reports distinguish previews, live work and private responses", {
  job <- list(steps = list(c("copy", "/source/<script>", "team:dest", "--dry-run")),
    index = 2L, status = "complete", phase = "complete", labels = "Soil flow",
    started = as.POSIXct("2026-10-23 12:24:04", tz = "UTC"))
  report <- movement_job_report(job, current_log = "Transferred: 1 MiB / 2 MiB, 50%, 1 MiB/s, ETA 1s")
  expect_true(report$preview)
  expect_equal(report$step, 1L)
  expect_equal(report$total, 1L)
  expect_match(report$started, "2026-10-23 12:24:04")
  html <- as.character(movement_progress_ui(report))
  expect_match(html, "Completed")
  expect_match(html, "no destination files are copied or deleted")
  expect_match(html, "Estimated bytes")
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, 'aria-valuenow="50"', fixed = TRUE)
  job$steps[[1]] <- job$steps[[1]][1:3]
  job$status <- "cancelled"; job$phase <- "cancelled"
  report <- movement_job_report(job)
  expect_false(report$preview)
  expect_match(report$kind, "Live transfer")
  expect_match(as.character(movement_progress_ui(report)), "Cancelled")
  expect_null(report$progress$percent)
  job$steps <- list(c("config", "create", "team", "onedrive", "token", "SECRET_TOKEN"))
  report <- movement_job_report(job, log = "SECRET_TOKEN", current_log = "SECRET_TOKEN", private = TRUE)
  expect_null(report$source)
  expect_null(report$destination)
  expect_length(report$events, 0L)
  expect_false(any(grepl("SECRET_TOKEN", unlist(report))))
  expect_false(grepl("SECRET_TOKEN", as.character(movement_progress_ui(report)), fixed = TRUE))
})

test_that("queued next steps never inherit an earlier step's progress", {
  path <- tempfile(); on.exit(unlink(path))
  writeLines("Transferred: 3 B / 3 B, 100%, 0 B/s, ETA -", path)
  job <- list(steps = list(c("copy", "a", "b"), c("copy", "c", "d")),
    logs = path, index = 2L, status = "running", phase = "queued")
  current <- movement_job_log(job, current = TRUE)
  expect_identical(current, "")
  report <- movement_job_report(job, log = movement_job_log(job), current_log = current)
  expect_true(report$queued)
  expect_null(report$progress$percent)
  expect_match(as.character(movement_progress_ui(report)), "Waiting in queue")
  job$logs <- c(path, path); job$index <- 3L; job$status <- "complete"; job$phase <- "complete"
  expect_match(movement_job_log(job, current = TRUE), "[2/2] Step 2", fixed = TRUE)
})

test_that("bounded rclone logs handle split UTF-8 characters and non-UTF-8 locales", {
  path <- tempfile(); on.exit(unlink(path))
  # Position the read boundary exactly on the continuation byte of e-acute.
  suffix <- charToRaw(paste0("\n2026/10/23 12:24:05 INFO : caf\u00e9.tif: Copied (new)\n"))
  bytes <- c(charToRaw("prefix"), charToRaw("\u00e9"),
    charToRaw(strrep("x", 32768 - 1 - length(suffix))), suffix)
  expect_identical(bytes[length(bytes) - 32768 + 1L], as.raw(0xa9))
  writeBin(bytes, path)
  job <- list(steps = list(c("copy", "a", "b")), logs = path, index = 1L)
  expect_no_warning(log <- movement_job_log(job))
  expect_match(log, "caf\u00e9.tif", fixed = TRUE)
  expect_no_warning(movement_parse_progress(log))
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_no_warning(log <- movement_job_log(job))
  expect_true(validUTF8(log))
  expect_no_warning(movement_parse_progress(log))
  expect_no_warning(movement_throttle_message(log))
  # A process can be in the middle of writing a multibyte character at EOF.
  writeBin(c(charToRaw("2026/10/23 12:24:05 ERROR : HTTP 429 caf"), as.raw(c(0xc3, 0xff))), path)
  expect_no_warning(log <- movement_job_log(job))
  expect_true(validUTF8(log))
  expect_match(movement_throttle_message(log), "rclone handles backoff")
  expect_match(movement_parse_progress(log)$events, "HTTP 429")
})
