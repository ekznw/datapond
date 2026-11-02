publication_fixture <- function(root) {
  pond <- file.path(root, "pond")
  dir.create(pond)
  ensure_pond_structure(pond); init_db(pond)
  id <- ensure_data_pond_id(pond)
  folder <- file.path(pond, "src", "soil__abc")
  dir.create(file.path(folder, "data"), recursive = TRUE)
  writeLines("payload", file.path(folder, "data", "soil.txt"))
  con <- get_conn(pond); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT INTO entry_tbl (uuid,title,folder_key,dataset_path,data_pond_id) VALUES (?,?,?,?,?)",
    params = list("abc", "Soils", "soil", folder, id))
  pond
}

test_that("publication settings persist separately for each pond without remote work", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  a <- file.path(root, "a"); b <- file.path(root, "b"); dir.create(a); dir.create(b)
  settings <- file.path(root, "settings")
  expect_null(movement_publication_read(settings, a))
  one <- movement_publication_save(settings, a, "team:published/a", TRUE)
  two <- movement_publication_save(settings, b, "team:published/b", FALSE)
  expect_identical(movement_publication_read(settings, file.path(a, ".")), one)
  expect_identical(movement_publication_read(settings, b), two)
  expect_error(movement_publication_save(settings, a, file.path(a, "nested"), TRUE), "separate")
  expect_error(movement_publication_save(settings, a, "https://sharepoint.example/a", TRUE), "configured")
  expect_error(movement_publication_save(settings, a, "team:a", NA), "Choose whether")
  expect_identical(movement_publication_read(settings, a), one)
  expect_equal(movement_control(settings, function(con) DBI::dbGetQuery(con, "SELECT count(*) AS n FROM queue")$n), 0)
})

test_that("publication controls are directly available in Run flows", {
  html <- xml2::read_html(as.character(mod_data_movement_ui("flow")))
  for (id in c("publish_destination", "publish_data", "save_publication", "preview_publish")) {
    nodes <- xml2::xml_find_all(html, paste0("//*[@id='flow-", id, "']"))
    expect_length(nodes, 1L)
    expect_length(xml2::xml_find_all(nodes, "ancestor::div[@data-value='run']"), 1L)
  }
})

test_that("failed publication launch retains settings and restores them in a new session", {
  local_trusted_access_fixture()
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  fixture_pond <- publication_fixture(root); dest <- file.path(root, "published"); settings <- file.path(root, "settings")
  withr::local_options(datapond.rclone = file.path(root, "missing-rclone"))
  shiny::testServer(mod_data_movement_server, args = list(pond = function() fixture_pond,
    dataset_path = function() NULL, state_directory = settings), {
    session$flushReact()
    session$setInputs(publish_destination = dest, publish_data = TRUE, preview_publish = 1L)
    expect_null(state$preview)
    expect_false(dir.exists(dest))
    expect_identical(movement_publication_read(settings, fixture_pond)$destination, dest)
    expect_true(movement_publication_read(settings, fixture_pond)$include_data)
  })
  options(datapond.rclone = unname(Sys.which("rclone")))
  shiny::testServer(mod_data_movement_server, args = list(pond = function() fixture_pond,
    dataset_path = function() NULL, state_directory = settings), {
    messages <- list()
    root_session <- session$rootScope()
    root_session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    session$flushReact()
    expect_identical(messages$publish_destination$value, dest)
    expect_true(messages$publish_data$value)
    expect_null(state$preview) # Restoring settings never authorises an old run.
    session$setInputs(publish_destination = messages$publish_destination$value,
      publish_data = messages$publish_data$value, run = 1L)
    expect_match(state$status, "Complete a preview first")
    finish <- function() {
      for (i in seq_len(120)) {
        if (!is.null(state$job$process)) state$job$process$wait(50)
        session$elapse(600)
        if (!is.null(state$job) && state$job$status != "running") break
      }
      expect_identical(state$job$status, "complete", info = state$status)
    }
    session$setInputs(preview_publish = 1L)
    finish()
    expect_false(dir.exists(dest))
    expect_equal(state$preview$spec$kind, "publish")
    expect_match(state$job$labels[1], "Dataset: src/soil__abc", fixed = TRUE)
    # Changing an unrelated flow destination must not affect publication.
    session$setInputs(destination = "other:unrelated")
    expect_false(state$preview$stale)
    session$setInputs(run = 2L)
    finish()
    expect_true(file.exists(file.path(dest, "src", "soil__abc", "data", "soil.txt")))
    expect_true(file.exists(file.path(dest, ".datapond-catalogue", "catalogue.json")))
    expect_true(movement_publication_read(settings, fixture_pond)$include_data)
    session$setInputs(preview_publish = 2L)
    finish()
    session$setInputs(publish_data = FALSE, run = 3L)
    expect_match(state$status, "Settings changed")
  })
})

test_that("switching ponds clears or restores the matching publication choices", {
  local_trusted_access_fixture()
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  a <- file.path(root, "a"); b <- file.path(root, "b"); dir.create(a); dir.create(b)
  settings <- file.path(root, "settings")
  movement_publication_save(settings, a, "team:a", TRUE)
  active <- shiny::reactiveVal(a)
  shiny::testServer(mod_data_movement_server, args = list(pond = active,
    dataset_path = function() NULL, state_directory = settings), {
    messages <- list()
    root_session <- session$rootScope()
    root_session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    session$flushReact()
    expect_identical(messages$publish_destination$value, "team:a")
    active(b); session$flushReact()
    expect_null(state$publication_settings)
    expect_identical(messages$publish_destination$value, "")
    expect_false(messages$publish_data$value)
    active(a); session$flushReact()
    expect_identical(messages$publish_destination$value, "team:a")
    expect_true(messages$publish_data$value)
  })
})

test_that("retrying an interrupted publication checks and reuses completed dataset files", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  pond <- publication_fixture(root); dest <- file.path(root, "published")
  finish <- function(job) {
    deadline <- Sys.time() + 20
    while (job$status == "running" && Sys.time() < deadline) {
      job$process$wait(50); movement_job_poll(job)
    }
    job$status
  }
  plan <- movement_publish_plan(pond, dest, include_data = TRUE, dry_run = FALSE)
  on.exit(unlink(plan$snapshot$directory, recursive = TRUE), add = TRUE)
  interrupted <- movement_job_start(list(plan$steps[[1]], c("unknown-datapond-command"), plan$steps[[3]]))
  on.exit(movement_job_cancel(interrupted), add = TRUE, after = FALSE)
  expect_identical(finish(interrupted), "failed")
  file <- file.path(dest, "src", "soil__abc", "data", "soil.txt")
  expect_true(file.exists(file))
  expect_false(file.exists(file.path(dest, ".datapond-catalogue", "catalogue.json")))
  before <- file.info(file)$mtime
  retry <- movement_job_start(plan$steps)
  on.exit(movement_job_cancel(retry), add = TRUE, after = FALSE)
  expect_identical(finish(retry), "complete")
  expect_identical(file.info(file)$mtime, before)
  first_step <- paste(readLines(retry$logs[1]), collapse = "\n")
  expect_false(grepl("Copied", first_step, fixed = TRUE))
  expect_true(file.exists(file.path(dest, ".datapond-catalogue", "catalogue.json")))
})
