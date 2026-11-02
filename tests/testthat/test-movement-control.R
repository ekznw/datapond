control_finish <- function(job) {
  deadline <- Sys.time() + 20
  while (job$status == "running" && Sys.time() < deadline) {
    if (!is.null(job$process)) job$process$wait(50)
    movement_job_poll(job)
  }
  expect_identical(job$status, "complete", info = paste(job$error, movement_job_log(job)))
}

test_that("cloud limits cover every operation and both ends use the stricter policy", {
  value <- function(args, flag) args[match(flag, args) + 1L]
  for (step in list(c("lsjson", "team:path"), c("copy", "team:a", "/tmp/local", "--dry-run"),
                   c("copyto", "/tmp/manifest.json", "team:pond/manifest.json"),
                   c("copy", "team:metadata", "/tmp/cache", "--files-from-raw", "/tmp/list"),
                   c("config", "create", "team", "onedrive", "--non-interactive"))) {
    args <- movement_apply_speed(step)
    expect_equal(value(args, "--transfers"), "1")
    expect_equal(value(args, "--checkers"), "2")
    expect_equal(value(args, "--tpslimit"), "2")
    expect_equal(value(args, "--tpslimit-burst"), "1")
    expect_equal(value(args, "--bwlimit"), "0")
    expect_equal(args[seq_along(step)], step)
  }
  profiles <- list(fast = movement_speed_profile("local"), slow = movement_speed_profile("custom",
    transfers = 2, checkers = 3, tps = 0.5, burst = 1, bandwidth = 3))
  step <- movement_apply_speed(c("copy", "fast:a", "slow:b"), profiles)
  expect_equal(value(step, "--tpslimit"), "0.5")
  expect_equal(value(step, "--bwlimit"), "3M")
  expect_equal(value(step, "--transfers"), "2")
  expect_identical(movement_apply_speed(step, profiles), step)
  expect_length(movement_step_remotes(c("copy", "E:/data", "/tmp/data", "--exclude", "remote:pattern")), 0)
  expect_equal(value(movement_apply_speed(c("lsjson", "/tmp")), "--tpslimit"), "0")
  expect_equal(value(movement_apply_speed(c("config", "update", "team")), "--log-level"), "INFO")
  expect_error(movement_speed_profile("custom", checkers = 1.2), "whole numbers")
  expect_error(movement_speed_profile("custom", tps = -1), "zero or greater")
  expect_error(movement_speed_profile("custom", bandwidth = NA_real_), "valid speed")
})

test_that("speed settings are persisted and shared without exposing credentials", {
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE))
  profile <- movement_speed_profile("custom", tps = 0.5, bandwidth = 2)
  movement_speed_save(root, "team", profile)
  movement_speed_save(root, "__local", movement_speed_profile("local"))
  expect_identical(movement_speed_read(root)$team, profile)
  rows <- movement_control(root, function(con) DBI::dbGetQuery(con, "PRAGMA table_info(queue)"))
  expect_false(any(c("token_value", "args", "path", "credentials") %in% rows$name))
  expect_false(movement_process_alive(2147483647L, 1))
  expect_true(movement_process_alive(Sys.getpid(), as.numeric(ps::ps_create_time(ps::ps_handle()))))
  expect_false(movement_process_alive(Sys.getpid(), 1)) # PID reuse must not hold a stale slot.
})

test_that("cloud queue is FIFO, cancellable, and independent of local transfers", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  source <- file.path(root, "source"); dir.create(source)
  writeBin(raw(1024^2), file.path(source, "large.bin"))
  withr::local_envvar(c(RCLONE_CONFIG_ALPHA_TYPE = "alias", RCLONE_CONFIG_ALPHA_REMOTE = source,
    RCLONE_CONFIG_BETA_TYPE = "alias", RCLONE_CONFIG_BETA_REMOTE = source))
  slow <- list("__cloud" = movement_speed_profile("custom", bandwidth = 1/1024))
  first <- movement_job_start(list(c("copy", "alpha:", file.path(root, "first"))),
    control_directory = root, profiles = slow)
  on.exit(movement_job_cancel(first), add = TRUE, after = FALSE)
  second <- movement_job_start(list(c("lsjson", "beta:")), control_directory = root)
  on.exit(movement_job_cancel(second), add = TRUE, after = FALSE)
  third <- movement_job_start(list(c("lsjson", "alpha:")), control_directory = root)
  on.exit(movement_job_cancel(third), add = TRUE, after = FALSE)
  expect_identical(second$phase, "queued")
  expect_null(second$process)
  expect_equal(third$queue_position, 3L)
  expect_match(movement_job_feedback(third), "shared cloud queue")
  expired <- movement_job_start(list(c("copy", "alpha:", file.path(root, "expired"))),
    control_directory = root, not_after = Sys.time() - 1)
  on.exit(movement_job_cancel(expired), add = TRUE, after = FALSE)
  expect_identical(expired$phase, "queued")
  local <- movement_job_start(list(c("lsjson", source)), control_directory = root)
  control_finish(local)
  expect_null(local$ticket)
  movement_job_cancel(second)
  expect_equal(second$status, "cancelled")
  expect_length(second$outputs, 0L)
  movement_job_cancel(first)
  control_finish(third)
  movement_job_poll(expired)
  expect_identical(expired$status, "failed")
  expect_match(expired$error, "preview expired while waiting")
  expect_false(dir.exists(file.path(root, "expired")))
  expect_match(paste(readLines(third$outputs[1]), collapse = ""), "large.bin")
  expect_equal(movement_control(root, function(con) DBI::dbGetQuery(con, "SELECT count(*) AS n FROM queue")$n), 0)
})

test_that("independent R processes share the queue and recover after an owner exits", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  source <- file.path(root, "source"); dir.create(source)
  writeBin(raw(1024^2), file.path(source, "large.bin"))
  withr::local_envvar(c(RCLONE_CONFIG_ALPHA_TYPE = "alias", RCLONE_CONFIG_ALPHA_REMOTE = source))
  repo <- datapond_test_source_root()
  script <- file.path(root, "owner.R")
  writeLines(c(
    "args <- commandArgs(TRUE); root <- args[1]; repo <- args[2]",
    "`%||%` <- function(x, y) if (is.null(x)) y else x",
    "new_uuid <- function() uuid::UUIDgenerate()",
    "source(file.path(repo, 'R/utils_data_movement.R'))",
    "source(file.path(repo, 'R/utils_movement_control.R'))",
    "job <- movement_job_start(list(c('copy', 'alpha:', file.path(root, 'copied'))), control_directory=root, profiles=list('__cloud'=movement_speed_profile('custom', bandwidth=1/1024)))",
    "saveRDS(job$process$get_pid(), file.path(root, 'ready.rds'))",
    "while (!file.exists(file.path(root, 'exit'))) Sys.sleep(.05)",
    "# Exit without releasing the ticket, simulating an abandoned app process.",
    "job$process$kill_tree(); job$process$wait(2000)"
  ), script)
  owner <- processx::process$new(file.path(R.home("bin"), "Rscript"), c(script, root, repo),
    stdout = file.path(root, "owner.out"), stderr = file.path(root, "owner.err"))
  on.exit(if (owner$is_alive()) owner$kill_tree(), add = TRUE, after = FALSE)
  deadline <- Sys.time() + 15
  while (!file.exists(file.path(root, "ready.rds")) && owner$is_alive() && Sys.time() < deadline) Sys.sleep(.05)
  expect_true(file.exists(file.path(root, "ready.rds")), info = paste(readLines(file.path(root, "owner.err")), collapse = "\n"))
  if (!file.exists(file.path(root, "ready.rds"))) return()
  waiting <- movement_job_start(list(c("lsjson", "alpha:")), control_directory = root)
  on.exit(movement_job_cancel(waiting), add = TRUE, after = FALSE)
  expect_identical(waiting$phase, "queued")
  expect_null(waiting$process)
  file.create(file.path(root, "exit"))
  owner$wait(5000)
  expect_false(owner$is_alive())
  control_finish(waiting)
  expect_equal(waiting$queue_position, 1L)
})

test_that("failed launches and failed cloud commands release the next job", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  withr::local_options(datapond.rclone = file.path(root, "missing-rclone"))
  expect_error(movement_job_start(list(c("lsjson", "alpha:")), control_directory = root))
  expect_equal(movement_control(root, function(con) DBI::dbGetQuery(con, "SELECT count(*) AS n FROM queue")$n), 0)
  options(datapond.rclone = unname(Sys.which("rclone")))
  withr::local_envvar(c(RCLONE_CONFIG_ALPHA_TYPE = "alias", RCLONE_CONFIG_ALPHA_REMOTE = root))
  bad <- movement_job_start(list(c("lsjson", "alpha:", "--invalid-datapond-flag")), control_directory = root)
  next_job <- movement_job_start(list(c("lsjson", "alpha:")), control_directory = root)
  on.exit(movement_job_cancel(next_job), add = TRUE, after = FALSE)
  bad$process$wait(5000); movement_job_poll(bad)
  expect_identical(bad$status, "failed")
  control_finish(next_job)
})

test_that("native throttling messages are visible without adding an app retry loop", {
  for (message in c("DEBUG : pacer: Rate limited, increasing sleep to 1s", "ERROR : HTTP 429",
                    "503 Service Unavailable", "Retry-After: 20", "activityLimitReached"))
    expect_match(movement_throttle_message(message), "rclone handles backoff")
  expect_identical(movement_throttle_message("INFO : sample429.csv: Copied (new)"), "")
  job <- new.env(); job$status <- "running"; job$phase <- "running"
  job$throttle <- movement_throttle_message("HTTP 429")
  expect_match(movement_job_feedback(job), "allow it to continue")
  job$status <- "complete"
  expect_identical(movement_job_feedback(job), "")
})

test_that("a throttled process retains its queue slot until it finishes", {
  skip_on_os("windows")
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  fake <- file.path(root, "rclone")
  writeLines(c("#!/bin/sh", "echo 'DEBUG : Too many requests. Trying again in 1 seconds.' >&2", "sleep 1"), fake)
  Sys.chmod(fake, "0755")
  withr::local_options(datapond.rclone = fake)
  job <- movement_job_start(list(c("lsjson", "alpha:")), control_directory = root)
  on.exit(movement_job_cancel(job), add = TRUE, after = FALSE)
  waiting <- movement_job_start(list(c("lsjson", "beta:")), control_directory = root)
  on.exit(movement_job_cancel(waiting), add = TRUE, after = FALSE)
  pid <- job$process$get_pid()
  job$process$wait(100); movement_job_poll(job)
  expect_match(movement_job_feedback(job), "Cloud throttling / retry reported")
  expect_match(movement_job_log(job), "Trying again in 1 seconds")
  expect_equal(job$process$get_pid(), pid)
  movement_job_poll(waiting)
  expect_null(waiting$process)
  control_finish(job)
  control_finish(waiting)
})

test_that("recent previews are reused, expire, and invalidate after settings changes", {
  local_trusted_access_fixture()

  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  source <- file.path(root, "source"); dir.create(source); writeLines("payload", file.path(source, "file"))
  shiny::testServer(mod_data_movement_server, args = list(pond = function() root,
    dataset_path = function() source, state_directory = file.path(root, "settings")), {
    finish <- function() {
      for (i in seq_len(120)) {
        if (!is.null(state$job$process)) state$job$process$wait(50)
        session$elapse(600)
        if (!is.null(state$job) && state$job$status != "running") break
      }
      expect_identical(state$job$status, "complete", info = state$status)
    }
    session$setInputs(source = source, destination = file.path(root, "dest"), mode = "copy", excludes = "", preview_transfer = 1)
    finish(); first <- state$job
    expect_match(output$preview_status, "Preview completed")
    session$setInputs(preview_transfer = 2)
    expect_identical(state$job, first)
    expect_match(state$status, "no new requests")
    saved_report <- state$report
    session$setInputs(browse_path = source, browse = 1)
    finish()
    expect_true(state$report$private)
    session$setInputs(preview_transfer = 20)
    expect_match(state$status, "no new requests")
    expect_identical(state$report, saved_report)
    expect_identical(output$operation_speed, saved_report$speed_summary)
    expect_match(output$operation_events, "Skipped copy")
    session$setInputs(fresh_preview = TRUE, preview_transfer = 3)
    finish()
    expect_false(identical(state$job, first))
    old <- state$preview; old$completed <- Sys.time() - 901; state$preview <- old
    session$setInputs(run = 1)
    expect_match(state$status, "more than 15 minutes")
    session$setInputs(preview_transfer = 4)
    finish()
    session$setInputs(speed_connection = "__local", speed_preset = "custom", speed_transfers = 2,
      speed_checkers = 3, speed_tps = 0, speed_burst = 1, speed_bandwidth = 4, save_speed = 1)
    expect_true(state$preview$stale)
    expect_equal(movement_speed_read(file.path(root, "settings"))[["__local"]]$bandwidth, 4)
    session$setInputs(run = 2)
    expect_match(state$status, "Settings changed")
    session$setInputs(preview_transfer = 5)
    finish()
    args <- state$preview$steps[[1]]
    expect_equal(args[match("--bwlimit", args) + 1L], "4M")
    # A different app session changing the profile also prevents execution.
    movement_speed_save(file.path(root, "settings"), "__local", movement_speed_profile("local"))
    session$setInputs(run = 3)
    expect_match(state$status, "Settings changed")
    expect_false(dir.exists(file.path(root, "dest")))
  })
})
