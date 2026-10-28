movement_fixture <- function() {
  pond <- tempfile("movement-pond-")
  dir.create(pond)
  ensure_pond_structure(pond); init_db(pond)
  id <- ensure_data_pond_id(pond)
  folder <- file.path(pond, "src", "soil__abc")
  dir.create(file.path(folder, "data"), recursive = TRUE)
  dir.create(file.path(folder, "docs"))
  writeLines("payload", file.path(folder, "data", "soil.txt"))
  writeLines("supporting", file.path(folder, "docs", "request.txt"))
  writeLines("<metadata/>", file.path(folder, "metadata.xml"))
  conn <- get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (uuid,title,folder_key,dataset_path,data_pond_id) VALUES (?,?,?,?,?)",
    params = list("abc", "Soils", "soil", folder, id))
  DBI::dbDisconnect(conn)
  pond
}

movement_wait <- function(job) {
  deadline <- Sys.time() + 20
  while (job$status == "running" && Sys.time() < deadline) {
    if (!is.null(job$process)) job$process$wait(100)
    movement_job_poll(job)
  }
  expect_identical(job$status, "complete", info = movement_job_log(job))
  invisible(job)
}

test_that("transfer endpoints preserve Windows, remote and spaced paths safely", {
  expect_true(movement_remote("sharepoint:Shared Documents"))
  expect_false(movement_remote("E:/data"))
  expect_false(movement_remote("E:\\data"))
  expect_equal(movement_endpoint("/"), "/")
  expect_error(movement_endpoint("--delete-excluded"), "Enter a folder")
  expect_error(movement_endpoint("https://org.sharepoint.com/site"), "configured")
  expect_error(movement_relative("src/../../metadata.db"), "Invalid")
  expect_error(movement_relative("/etc/passwd"), "Invalid")
  expect_error(movement_relative("src/a\nother"), "Invalid")
  expect_silent(movement_relative(".datapond-catalogue"))
  plan <- movement_transfer_plan("remote:folder with spaces", "backup:dataset")
  expect_true("--dry-run" %in% plan$args)
  expect_identical(plan$args[2], "remote:folder with spaces")
  expect_false(any(c("--ignore-errors", "--ignore-checksum", "--ignore-size") %in% plan$args))
  expect_error(movement_transfer_plan("remote:a", "remote:a/sub"), "non-overlapping")
  expect_error(movement_transfer_plan("remote:a", "other:", "sync"), "dedicated")
})

test_that("copy preview leaves destination unchanged; sync is explicitly destructive", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  source <- file.path(root, "source"); dest <- file.path(root, "destination")
  dir.create(source); dir.create(dest)
  writeLines("data", file.path(source, "new.txt"))
  writeLines("keep", file.path(dest, "extra.txt"))
  plan <- movement_transfer_plan(source, dest)
  movement_wait(movement_job_start(list(plan$args)))
  expect_false(file.exists(file.path(dest, "new.txt")))
  movement_wait(movement_job_start(list(movement_transfer_plan(source, dest, dry_run = FALSE)$args)))
  expect_true(file.exists(file.path(dest, "new.txt")))
  expect_true(file.exists(file.path(dest, "extra.txt")))
  movement_wait(movement_job_start(list(movement_transfer_plan(source, dest, "sync", dry_run = FALSE)$args)))
  expect_false(file.exists(file.path(dest, "extra.txt")))
})

test_that("failed jobs do not run later steps", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  job <- movement_job_start(list(c("unknown-datapond-command"), c("mkdir", file.path(root, "should-not-exist"))))
  job$process$wait(5000); movement_job_poll(job)
  expect_identical(job$status, "failed")
  expect_false(dir.exists(file.path(root, "should-not-exist")))
})

test_that("snapshots capture committed WAL transactions and protect writable ponds", {
  pond <- movement_fixture(); on.exit(unlink(pond, recursive = TRUE))
  conn <- get_conn(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "PRAGMA journal_mode=WAL")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET title='Committed WAL title'")
  snapshot <- movement_snapshot(pond); on.exit(unlink(snapshot$directory, recursive = TRUE), add = TRUE)
  copy <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(snapshot$directory), flags = RSQLite::SQLITE_RO)
  expect_equal(DBI::dbGetQuery(copy, "SELECT title FROM entry_tbl")$title, "Committed WAL title")
  DBI::dbDisconnect(copy)
  expect_false(file.exists(file.path(snapshot$directory, "src", "soil__abc", "data", "soil.txt")))
  expect_true(file.exists(file.path(snapshot$directory, "src", "soil__abc", "metadata.xml")))
  expect_identical(snapshot$manifest$datasets[[1]]$path, "src/soil__abc")
  expect_error(movement_transfer_plan(pond, tempfile()), "Publish pond")
})

test_that("remote metadata browsing is portable, offline and only fetches data on demand", {
  skip_if(!nzchar(Sys.which("rclone")))
  pond <- movement_fixture(); remote <- tempfile("published-"); cache <- tempfile("cache-")
  on.exit(unlink(c(pond, remote, cache), recursive = TRUE))
  plan <- movement_publish_plan(pond, remote, include_data = TRUE, dry_run = FALSE)
  on.exit(unlink(plan$snapshot$directory, recursive = TRUE), add = TRUE)
  movement_wait(movement_job_start(plan$steps))
  before <- tools::md5sum(file.path(remote, ".datapond-catalogue", "metadata.db"))
  prepared <- movement_cache_prepare(remote, cache)
  movement_wait(movement_job_start(prepared$steps))
  fetched <- movement_cache_fetch_plan(prepared)
  movement_wait(movement_job_start(fetched$steps))
  movement_cache_commit(prepared, fetched$manifest)
  unlink(fetched$list_file)
  expect_false(dir.exists(file.path(cache, "datasets")))
  rows <- movement_cache_inventory(cache)
  expect_equal(rows$title, "Soils")
  expect_equal(rows$stage, "src")
  expect_equal(rows$cached, "Metadata only")
  current <- movement_cache_current(cache)
  expect_error(get_conn(current$directory), "read-only remote cache")
  expect_error(validate_existing_data_pond(current$directory), "read-only remote cache")
  expect_error(ensure_pond_structure(current$directory), "read-only remote cache")
  expect_error(movement_transfer_plan(tempfile(), file.path(cache, "new-folder")), "read-only remote cache")
  download <- movement_download_plan(cache, rows$id)
  movement_wait(movement_job_start(download$steps))
  expect_true(file.exists(file.path(download$destination, "data", "soil.txt")))
  expect_true(file.exists(file.path(download$destination, "docs", "request.txt")))
  expect_identical(tools::md5sum(file.path(remote, ".datapond-catalogue", "metadata.db")), before)
  # Metadata stays browseable even when the remote goes away.
  unlink(remote, recursive = TRUE)
  expect_equal(movement_cache_inventory(cache)$title, "Soils")
})

test_that("corrupt refreshes retain a working catalogue and reject path traversal", {
  skip_if(!nzchar(Sys.which("rclone")))
  pond <- movement_fixture(); remote <- tempfile(); cache <- tempfile()
  on.exit(unlink(c(pond, remote, cache), recursive = TRUE))
  plan <- movement_publish_plan(pond, remote, dry_run = FALSE)
  on.exit(unlink(plan$snapshot$directory, recursive = TRUE), add = TRUE)
  movement_wait(movement_job_start(plan$steps))
  refresh <- function() {
    prepared <- movement_cache_prepare(remote, cache)
    movement_wait(movement_job_start(prepared$steps))
    fetched <- movement_cache_fetch_plan(prepared)
    movement_wait(movement_job_start(fetched$steps))
    unlink(fetched$list_file)
    list(prepared = prepared, manifest = fetched$manifest)
  }
  first <- refresh(); movement_cache_commit(first$prepared, first$manifest)
  second <- refresh()
  writeLines("bad database", get_db_path(second$prepared$directory))
  expect_error(movement_cache_commit(second$prepared, second$manifest), "incomplete")
  expect_equal(movement_cache_inventory(cache)$title, "Soils")
  manifest <- first$manifest; manifest$datasets[[1]]$path <- "src/../../elsewhere"
  expect_error(movement_validate_manifest(manifest), "Invalid")
  expect_error(movement_cache_prepare(tempfile(), cache), "different remote")
})

test_that("reference JSON routines import as data without execution", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "syncs.json")
  movement_json_write(list(syncs = list(list(name = "Test", src = "team:input", dest = "backup:output", excludes = c("**/.git/**", "**/tmp/**")))), path)
  profiles <- movement_read_profiles(path)
  expect_length(profiles, 1L)
  expect_equal(profiles[[1]]$excludes, c("**/.git/**", "**/tmp/**"))
})

test_that("movement tab exposes independent transfer and read-only cache workflows", {
  html <- as.character(mod_data_movement_ui("movement"))
  expect_match(html, "<h3>Flow</h3>", fixed = TRUE)
  expect_match(html, "Connections and flow design are setup tasks", fixed = TRUE)
  expect_match(html, 'data-value="remote"', fixed = TRUE)
  expect_match(html, "Preview flow")
  expect_match(html, "Sign in and discover libraries")
  expect_match(html, "Download / update selected dataset")
  expect_match(html, "Choose local pond cache")
})

test_that("Shiny previews are invalidated by edits and can execute asynchronously", {
  local_trusted_access_fixture()

  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root)
  source <- file.path(root, "source"); dest <- file.path(root, "dest")
  dir.create(source); writeLines("hello", file.path(source, "data.txt"))
  on.exit(unlink(root, recursive = TRUE))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() source, state_directory = file.path(root, "settings")
  ), {
    finish <- function() {
      for (i in seq_len(60)) {
        if (!is.null(state$job)) state$job$process$wait(100)
        session$elapse(600)
        if (!is.null(state$job) && state$job$status != "running") break
      }
    }
    session$setInputs(source = source, destination = dest, mode = "copy", excludes = "", delete_ack = FALSE)
    session$setInputs(preview_transfer = 1)
    finish()
    expect_match(state$status, "Preview complete")
    expect_true(state$report$preview)
    expect_identical(state$report$status, "complete")
    expect_match(output$operation_events, "Skipped copy")
    expect_false(dir.exists(dest))
    session$setInputs(destination = file.path(root, "changed"), run = 1)
    expect_match(state$status, "Settings changed")
    expect_false(dir.exists(file.path(root, "changed")))
    session$setInputs(destination = dest, preview_transfer = 2)
    finish()
    session$setInputs(run = 2)
    finish()
    expect_match(state$status, "completed successfully")
    expect_false(state$report$preview)
    expect_identical(state$report$status, "complete")
    expect_identical(state$report$destination, dest)
    expect_true(file.exists(file.path(dest, "data.txt")))
    session$setInputs(browse_path = source, browse = 1)
    finish()
    expect_equal(state$listing$Path, "data.txt")
  })
})

test_that("cancel stops a queued transfer without reporting completion", {
  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "source"))
  writeBin(raw(1024^2), file.path(root, "source", "large.bin"))
  job <- movement_job_start(list(c("copy", file.path(root, "source"), file.path(root, "dest"), "--bwlimit", "1k"),
                                 c("mkdir", file.path(root, "later"))),
    profiles = list("__local" = movement_speed_profile("custom", bandwidth = 1/1024)))
  job$process$wait(100)
  movement_job_cancel(job)
  expect_equal(movement_job_poll(job), "cancelled")
  expect_false(dir.exists(file.path(root, "later")))
})

test_that("Shiny chains catalogue fetches and evicts only selected cached files", {
  local_trusted_access_fixture()

  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root)
  pond <- movement_fixture(); remote <- file.path(root, "remote"); cache <- file.path(root, "cache")
  on.exit(unlink(c(root, pond), recursive = TRUE))
  publication <- movement_publish_plan(pond, remote, include_data = TRUE, dry_run = FALSE)
  on.exit(unlink(publication$snapshot$directory, recursive = TRUE), add = TRUE)
  movement_wait(movement_job_start(publication$steps))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() pond, dataset_path = function() NULL, state_directory = file.path(root, "settings")
  ), {
    finish <- function() {
      for (i in seq_len(120)) {
        if (!is.null(state$job)) state$job$process$wait(100)
        session$elapse(600)
        if (!is.null(state$job) && state$job$status != "running") break
      }
    }
    session$setInputs(remote = remote, cache = cache, remote_name = "Team pond", refresh_cache = 1)
    finish()
    expect_match(state$status, "Metadata ready")
    expect_equal(inventory()$title, "Soils")
    expect_false(dir.exists(file.path(cache, "datasets")))
    expect_type(movement_cache_details(cache, inventory()$id), "list")
    session$setInputs(catalogue_rows_selected = 1, download = 1)
    finish()
    expect_match(state$status, "Dataset downloaded")
    downloaded <- file.path(cache, "datasets", "src", "soil__abc", "data", "soil.txt")
    expect_true(file.exists(downloaded))
    session$setInputs(evict = 1, confirm_evict = 1)
    expect_false(file.exists(downloaded))
    expect_true(file.exists(file.path(remote, "src", "soil__abc", "data", "soil.txt")))
    expect_equal(movement_cache_inventory(cache)$cached, "Metadata only")
    # A second refresh leaves one metadata generation, with no duplicate DB copies.
    session$setInputs(refresh_cache = 2)
    finish()
    expect_length(list.files(cache, pattern = "^catalogue-"), 1L)
  })
})

test_that("rclone availability handles missing, working and failing executables", {
  withr::local_options(datapond.rclone = tempfile("missing-rclone-"))
  expect_false(movement_rclone_status()$available)
  testthat::local_mocked_bindings(run = function(...) list(status = 0L, stdout = "rclone v1.70.0\n- os/type: linux\n"), .package = "processx")
  status <- movement_rclone_status()
  expect_true(status$available)
  expect_identical(status$version, "rclone v1.70.0")
  testthat::local_mocked_bindings(run = function(...) list(status = 1L, stdout = "rclone v1.70.0\n"), .package = "processx")
  expect_false(movement_rclone_status()$available)
})

test_that("Flow offers installation help when rclone is missing and can recheck", {
  local_trusted_access_fixture()

  root <- tempfile("rclone-flow-status-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  available <- FALSE
  testthat::local_mocked_bindings(movement_rclone_status = function() {
    list(available = available, version = if (available) "rclone v1.70.0" else "")
  })
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() "", state_directory = root
  ), {
    expect_match(output$rclone_status$html, "https://rclone.org/install/", fixed = TRUE)
    expect_match(output$rclone_status$html, "was not found", fixed = TRUE)
    available <<- TRUE
    session$setInputs(check_rclone = 1L)
    expect_match(output$rclone_status$html, "rclone is available", fixed = TRUE)
    expect_false(grepl("https://rclone.org/install/", output$rclone_status$html, fixed = TRUE))
  })
})
