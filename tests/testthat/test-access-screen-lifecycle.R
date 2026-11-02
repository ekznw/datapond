test_that("sign-in and workspace share one replacement boundary", {
  markup <- htmltools::renderTags(datapond:::access_ui())
  html <- xml2::read_html(markup$html)
  expect_length(xml2::xml_find_all(html, "//*[@id='access_screen']"), 1L)
  expect_length(xml2::xml_find_all(html, "//*[@id='access_workspace']"), 0L)
  expect_match(markup$head, "access-session.js", fixed = TRUE)
})

test_that("failed workspace startup returns to sign-in and can be retried", {
  root <- tempfile("access-screen-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(root, "app"), .package = "datapond")
  pond <- file.path(root, "pond")
  datapond:::init_db(pond)
  code <- datapond::setup_pond_access(pond, "owner", first_name = "Pond", last_name = "Owner")
  password <- "workspace retry test passphrase"
  datapond:::access_activate(pond, "owner", code, password)
  attempts <- 0L
  shiny::testServer(function(input, output, session) {
    state <- shiny::reactiveValues(data_pond = pond, active_user_id = NULL, refresh_person = 0L)
    auth <- datapond:::access_server(input, output, session, state,
      switch_pond = function(path) NULL,
      on_login = function() {
        attempts <<- attempts + 1L
        if (attempts == 1L) stop("Simulated workspace startup failure")
      })
  }, {
    assert_screen <- function(logged_in) {
      html <- xml2::read_html(output$access_screen$html)
      expect_length(xml2::xml_find_all(html, "//*[@id='main_panel']"), as.integer(logged_in))
      expect_length(xml2::xml_find_all(html, "//form[@id='access_login_form']"), as.integer(!logged_in))
      expect_length(xml2::xml_find_all(html, "//*[@id='exit_datapond']"), as.integer(!logged_in))
    }
    session$setInputs(access_login = 0L)
    assert_screen(FALSE)
    session$setInputs(access_username = "owner", access_password = password, access_login = 1L)
    first_nonce <- auth$workspace_nonce
    assert_screen(TRUE)
    session$setInputs(access_login = 2L)
    expect_identical(auth$workspace_nonce, first_nonce)
    session$setInputs(access_workspace_ready = "stale-workspace")
    expect_false(auth$ready)
    session$setInputs(access_workspace_ready = first_nonce)
    expect_false(auth$authenticated)
    expect_null(auth$token)
    expect_null(auth$workspace_nonce)
    assert_screen(FALSE)

    session$setInputs(access_username = "owner", access_password = password, access_login = 3L)
    second_nonce <- auth$workspace_nonce
    expect_false(identical(second_nonce, first_nonce))
    session$setInputs(access_workspace_ready = first_nonce)
    expect_false(auth$ready)
    session$setInputs(access_workspace_ready = second_nonce)
    expect_true(datapond:::access_allowed("admin"))
    assert_screen(TRUE)
    session$setInputs(access_workspace_ready = "")
    session$setInputs(access_workspace_ready = second_nonce)
    expect_identical(attempts, 2L)
    auth$invalidate()
    session$flushReact()
    assert_screen(FALSE)
  })
})
