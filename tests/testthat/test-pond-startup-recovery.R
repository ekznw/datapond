test_that("reading pond settings never creates folders or a pond identity", {
  parent <- tempfile("pond-settings-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  missing <- file.path(parent, "disconnected", "pond")
  expect_no_warning(cfg <- datapond:::read_pond_config(missing))
  expect_identical(cfg$data_pond, missing)
  expect_null(cfg$data_pond_id)
  expect_false(dir.exists(dirname(missing)))

  expect_null(datapond:::read_pond_config(parent)$data_pond_id)
  expect_length(list.files(parent, all.files = TRUE, no.. = TRUE), 0L)
  id <- datapond:::ensure_data_pond_id(parent)
  expect_identical(datapond:::read_pond_config(parent)$data_pond_id, id)
})

test_that("missing saved ponds and empty mount points are never recreated", {
  parent <- tempfile("offline-pond-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    app_state_dir = function() file.path(parent, "app-config"), .package = "datapond"
  )
  pond <- file.path(parent, "unmounted", "pond")
  datapond:::write_last_data_pond_path(pond)
  fallback <- file.path(parent, "fallback")
  expect_error(datapond:::load_config(fallback), "catalogue is unavailable")
  expect_identical(datapond:::read_last_data_pond_path(), pond)
  expect_false(dir.exists(dirname(pond)))
  expect_false(dir.exists(fallback))

  dir.create(pond, recursive = TRUE)
  expect_error(datapond:::load_config(fallback), "catalogue is unavailable")
  expect_length(list.files(pond, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("legacy startup pointers cannot initialise unavailable storage", {
  parent <- tempfile("legacy-offline-pond-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  previous_dir <- setwd(parent)
  on.exit(setwd(previous_dir), add = TRUE, after = FALSE)
  testthat::local_mocked_bindings(
    app_state_dir = function() file.path(parent, "app-config"), .package = "datapond"
  )
  dir.create("data")
  pond <- file.path(parent, "missing", "pond")
  writeLines(pond, "data/last_data_pond_path")
  expect_error(datapond:::load_config(), "catalogue is unavailable")
  expect_false(dir.exists(dirname(pond)))
  expect_null(datapond:::read_last_data_pond_path())
  unlink("data/last_data_pond_path")
  saveRDS(list(data_pond = pond), "data/config.rds")
  expect_error(datapond:::load_config(), "catalogue is unavailable")
  expect_false(dir.exists(dirname(pond)))
  expect_null(datapond:::read_last_data_pond_path())
})

test_that("startup stays alive offline and reopens the same pond after reconnection", {
  local_trusted_access_fixture()

  parent <- tempfile("pond-recovery-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  old_options <- options(datapond.data_pond = NULL)
  on.exit(options(old_options), add = TRUE)
  testthat::local_mocked_bindings(
    app_state_dir = function() file.path(parent, "app-config"), .package = "datapond"
  )
  pond <- file.path(parent, "pond")
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  id <- datapond:::ensure_data_pond_id(pond)
  dataset_path <- file.path(pond, "dev", "kept_dataset")
  dir.create(dataset_path)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, paste(
    "INSERT INTO entry_tbl (uuid, title, citation_key, folder_key, dataset_path, lifecycle_status, data_pond_id)",
    "VALUES ('kept-dataset-uuid', 'Keep this record', 'kept_key', 'kept_dataset', ?, 'draft', ?)"
  ), params = list(dataset_path, id))
  DBI::dbDisconnect(conn)
  datapond:::write_last_data_pond_path(pond)
  offline <- file.path(parent, "offline")
  expect_true(file.rename(pond, offline))

  # Test both automatic reopening and explicitly requesting the saved pond.
  for (requested in list(NULL, pond)) {
    options(datapond.data_pond = requested)
    shiny::testServer(datapond:::server, {
      expect_s3_class(startup_error, "error")
      expect_false(session$isClosed())
      expect_false(dir.exists(pond))
      expect_identical(datapond:::read_last_data_pond_path(), pond)
      reloaded <- FALSE
      session$reload <- function() reloaded <<- TRUE
      session$setInputs(retry_pond_startup = 0L)
      session$setInputs(retry_pond_startup = 1L)
      expect_true(reloaded)
    })
  }

  options(datapond.data_pond = NULL)
  expect_true(file.rename(offline, pond))
  shiny::testServer(datapond:::server, {
    if (!is.null(startup_error)) stop(startup_error)
    expect_null(startup_error)
    expect_identical(state$data_pond, pond)
    expect_identical(state$data_pond_id, id)
    expect_identical(state$pond_config$data_pond_id, id)
    expect_identical(datapond:::get_datasets(pond)$title, "Keep this record")
  })
})

test_that("the startup recovery dialog explains the retry action", {
  registry <- data.frame(pond_name = "Survey pond", data_pond_path = "/missing/survey-pond")
  html <- as.character(datapond:::pond_startup_error_dialog("Missing: /pond/location", registry))
  expect_match(html, "Missing: /pond/location", fixed = TRUE)
  expect_match(html, "Your saved pond selection has been kept.", fixed = TRUE)
  expect_match(html, 'id="retry_pond_startup"', fixed = TRUE)
  expect_match(html, "Retry opening pond", fixed = TRUE)
  for (id in c("recovery_known_pond", "recovery_pond_path", "open_recovery_pond")) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  expect_match(html, "Survey pond", fixed = TRUE)
  expect_match(html, "(unavailable)", fixed = TRUE)
})

test_that("recovery can open another pond without changing the launch option", {
  local_trusted_access_fixture()

  parent <- tempfile("alternate-pond-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  missing <- file.path(parent, "offline")
  alternative <- file.path(parent, "alternative")
  old_options <- options(datapond.data_pond = missing)
  on.exit(options(old_options), add = TRUE)
  testthat::local_mocked_bindings(
    app_state_dir = function() file.path(parent, "app-config"), .package = "datapond"
  )
  datapond:::ensure_pond_structure(alternative)
  datapond:::init_db(alternative)
  alternative_id <- datapond:::ensure_data_pond_id(alternative)
  datapond:::register_connected_pond(alternative, "Available pond")
  datapond:::write_last_data_pond_path(missing)

  shiny::testServer(datapond:::server, {
    # Browser defaults are present when the recovered server starts its modules.
    html <- xml2::read_html(as.character(datapond:::ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$open_recovery_pond <- 0L
    do.call(session$setInputs, defaults)

    session$setInputs(recovery_pond_path = parent, open_recovery_pond = 1L)
    expect_null(startup_recovery$opened_state())
    expect_identical(datapond:::read_last_data_pond_path(), missing)
    expect_false(file.exists(file.path(parent, "metadata.db")))

    session$setInputs(recovery_pond_path = alternative, open_recovery_pond = 2L)
    recovered <- startup_recovery$opened_state()
    expect_false(is.null(recovered))
    expect_identical(recovered$data_pond, alternative)
    expect_identical(recovered$data_pond_id, alternative_id)
    expect_null(recovered$active_user_id)
    expect_identical(datapond:::read_last_data_pond_path(), alternative)
    expect_identical(getOption("datapond.data_pond"), missing)
    expect_false(dir.exists(missing))
    expect_false(session$isClosed())

    # The removed dialog's observers must no longer react after recovery.
    session$setInputs(recovery_pond_path = parent, open_recovery_pond = 3L)
    expect_identical(startup_recovery$opened_state(), recovered)
    expect_identical(datapond:::read_last_data_pond_path(), alternative)
  })
})
