test_that("only the latest run outcome is retained per flow and publication", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  expect_length(movement_run_status_read(root, "flow", "one"), 0L)
  movement_run_status_write(root, "flow", "one", "first", 100, "running")
  movement_run_status_write(root, "flow", "one", "first", 100, "failed", "A failed transfer")
  movement_run_status_write(root, "flow", "two", "second", 110, "running")
  movement_run_status_write(root, "publication", "/pond", "third", 120, "running")
  movement_run_status_write(root, "flow", "one", "latest", 130, "running")
  movement_run_status_write(root, "flow", "one", "first", 100, "failed", "Late update")
  movement_run_status_write(root, "flow", "one", "latest", 130, "complete")
  rows <- movement_run_status_read(root, "flow", c("one", "two"))
  expect_identical(rows$one$status, "complete")
  expect_identical(rows$one$errors, "")
  expect_identical(rows$one$started, 130)
  expect_identical(rows$two$status, "running")
  expect_length(movement_run_status_read(root, "publication", "/pond"), 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "movement-control.sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM run_status")$n, 3L)
  movement_run_status_delete(root, "flow", "two")
  expect_null(movement_run_status_read(root, "flow", "two")$two)
})

test_that("known Office conversion mismatches do not count as actionable errors", {
  log <- tempfile(); on.exit(unlink(log))
  writeLines(c(
    "2026/11/03 17:02:10 ERROR : docs/DATA REQUEST FORM.doc: corrupted on transfer: quickxor hashes differ src(private path) secret",
    "2026/11/03 17:02:11 ERROR : docs/slides.pptx: corrupted on transfer: sizes differ src(private path)",
    "2026/11/03 17:02:11 ERROR : data/Soil_Template.xls: corrupted on transfer: sizes differ src(private path)",
    "2026/11/03 17:02:12 ERROR : Attempt 1/3 failed with 2 errors and: corrupted on transfer: sizes differ",
    "2026/11/03 17:02:13 ERROR : docs/report.html: permission denied",
    "2026/11/03 17:02:14 ERROR : data/table.csv: checksum mismatch src(private path)"
  ), log)
  result <- movement_run_errors(log, "rclone exited with an error")
  expect_identical(result$ignored, 3L)
  expect_identical(result$count, 2L)
  expect_match(result$summary, "report.html: permission denied", fixed = TRUE)
  expect_match(result$summary, "table.csv: checksum mismatch", fixed = TRUE)
  expect_false(grepl("private path|secret|DATA REQUEST", result$summary))
  writeLines("2026/11/03 17:02:10 ERROR : docs/letter.docx: corrupted on transfer: sizes differ", log)
  expect_identical(movement_run_errors(log, "rclone exited with an error")$summary, "")
})

test_that("a batch records only flows that reached their step", {
  local_trusted_access_fixture()
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  publication_path <- file.path(root, "pond"); dir.create(publication_path)
  shiny::testServer(mod_data_movement_server,
    args = list(pond = function() publication_path, dataset_path = function() NULL,
      state_directory = file.path(root, "settings")), {
    job <- new.env(parent = emptyenv())
    job$started <- Sys.time(); job$run_context <- list(kind = "flow",
      keys = c("first", "second", "third"), run_id = "batch", recorded = 0L)
    job$logs <- character(); job$index <- 1L; job$status <- "running"
    movement_run_status_write(file.path(root, "settings"), "flow", "first", "batch", job$started, "running")
    job$index <- 2L
    record_run_progress(job)
    rows <- movement_run_status_read(file.path(root, "settings"), "flow", c("first", "second", "third"))
    expect_identical(rows$first$status, "complete")
    expect_identical(rows$second$status, "running")
    expect_null(rows$third)
    job$status <- "cancelled"
    record_run_progress(job)
    rows <- movement_run_status_read(file.path(root, "settings"), "flow", c("first", "second", "third"))
    expect_identical(rows$second$status, "cancelled")
    expect_null(rows$third)
    log <- file.path(root, "known.log")
    writeLines("2026/11/03 17:02:10 ERROR : docs/form.doc: corrupted on transfer: sizes differ", log)
    publication <- new.env(parent = emptyenv())
    publication$started <- Sys.time(); publication$run_context <- list(kind = "publication",
      keys = publication_path, run_id = "publish", recorded = 0L)
    publication$logs <- log; publication$status <- "failed"; publication$error <- NULL
    movement_run_status_write(file.path(root, "settings"), "publication", publication_path,
      "publish", publication$started, "running")
    record_run_progress(publication)
    outcome <- movement_run_status_read(file.path(root, "settings"), "publication", publication_path)[[publication_path]]
    expect_identical(outcome$status, "failed_known")
    expect_identical(outcome$errors, "")
  })
})
