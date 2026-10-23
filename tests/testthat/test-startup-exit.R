test_that("local exit stops the app and hosted exit closes only its session", {
  stopped <- 0L
  testthat::local_mocked_bindings(
    stopApp = function(...) stopped <<- stopped + 1L, .package = "shiny"
  )
  withr::local_options(datapond.stop_on_exit = TRUE)
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  datapond:::exit_data_pond(session)
  expect_identical(stopped, 1L)

  options(datapond.stop_on_exit = FALSE)
  datapond:::exit_data_pond(session)
  expect_true(session$isClosed())
  expect_identical(stopped, 1L)
})

test_that("the local launcher scopes and restores its exit policy", {
  withr::local_options(datapond.stop_on_exit = FALSE)
  testthat::local_mocked_bindings(runApp = function(...) {
    expect_true(getOption("datapond.stop_on_exit"))
    "exited"
  }, .package = "shiny")
  expect_identical(datapond::run_data_pond(launch.browser = FALSE), "exited")
  expect_false(getOption("datapond.stop_on_exit"))
})

test_that("both startup dialogs offer a working exit without selecting a user", {
  local_trusted_access_fixture()

  parent <- tempfile("startup-exit-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    app_state_dir = function() file.path(parent, "app-config"), .package = "datapond"
  )
  pond <- file.path(parent, "pond")
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  missing <- file.path(parent, "missing")
  withr::local_options(datapond.stop_on_exit = FALSE, datapond.data_pond = NULL)
  for (path in c(pond, missing)) {
    options(datapond.data_pond = path)
    # Record as a previously selected pond: absent storage must not be created.
    datapond:::write_last_data_pond_path(path)
    shiny::testServer(datapond:::server, {
      modal_html <- NULL
      session$sendModal <- function(type, message) {
        if (identical(type, "show")) modal_html <<- message$html
      }
      if (identical(path, pond)) {
        expect_null(startup_error)
        # Supply the select defaults that a real browser sends on connection.
        html <- xml2::read_html(as.character(datapond:::ui))
        selects <- xml2::xml_find_all(html, "//select")
        defaults <- stats::setNames(lapply(selects, function(node) {
          selected <- xml2::xml_find_all(node, "./option[@selected]")
          if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
          if (!length(selected)) return("")
          xml2::xml_attr(selected, "value")
        }), xml2::xml_attr(selects, "id"))
        defaults$exit_datapond <- 0L
        do.call(session$setInputs, defaults)
        modal_html <- as.character(datapond:::access_login_ui(path, configured = FALSE))
        expect_null(state$active_user_id)
      } else {
        expect_s3_class(startup_error, "error")
        modal_html <- as.character(datapond:::pond_startup_error_dialog("Unavailable"))
      }
      expect_match(modal_html, 'id="exit_datapond"', fixed = TRUE)
      expect_match(modal_html, "Exit DataPond", fixed = TRUE)
      session$setInputs(exit_datapond = 0L)
      expect_false(session$isClosed())
      session$setInputs(exit_datapond = 1L)
      expect_true(session$isClosed())
      expect_identical(datapond:::read_last_data_pond_path(), path)
      expect_false(dir.exists(missing))
    })
  }
})
