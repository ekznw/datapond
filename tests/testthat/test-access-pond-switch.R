test_that("sign-in keeps the pond controls inside a collapsed disclosure box", {
  for (configured in c(FALSE, TRUE)) {
    html <- xml2::read_html(as.character(datapond:::access_login_ui("/pond", configured,
      registry = datapond:::empty_connected_pond_registry())))
    panel <- xml2::xml_find_all(html, "//details[summary='Open another pond']")
    expect_length(panel, 1L)
    expect_true(is.na(xml2::xml_attr(panel, "open")))
    expect_identical(xml2::xml_text(xml2::xml_find_first(panel, "./summary")), "Open another pond")
    expect_identical(xml2::xml_attr(xml2::xml_find_first(panel,
      ".//input[@id='access_pond_path']"), "value"), "/pond")
    expect_length(xml2::xml_find_all(panel, ".//button[@id='access_open_pond']"), 1L)
  }
})

test_that("sign-in lists local saved ponds by recency, including unavailable ponds", {
  root <- tempfile("saved-login-ponds-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(root, "app"), .package = "datapond")
  available <- file.path(root, "available")
  missing <- file.path(root, "offline")
  datapond:::init_db(available)
  datapond:::register_connected_pond(available, "Survey pond")
  registry <- datapond:::read_connected_pond_registry()
  registry$last_seen_at <- "2026-01-01 12:00:00"
  offline <- registry
  offline$data_pond_id <- "offline-pond"
  offline$data_pond_path <- missing
  offline$pond_name <- "Field pond"
  offline$last_seen_at <- "2026-01-02 12:00:00"
  datapond:::write_connected_pond_registry(rbind(registry, offline))

  html <- xml2::read_html(as.character(datapond:::access_login_ui(available, TRUE)))
  choices <- xml2::xml_find_all(html, "//select[@id='access_known_pond']/option")
  expect_identical(xml2::xml_attr(choices, "value"), c("", missing, available))
  expect_match(xml2::xml_text(choices)[2], "Field pond", fixed = TRUE)
  expect_match(xml2::xml_text(choices)[2], "(unavailable)", fixed = TRUE)
  expect_match(xml2::xml_text(choices)[3], "Survey pond", fixed = TRUE)
  expect_false(dir.exists(missing))
})

test_that("the sign-in pond controls switch ponds without authentication", {
  root <- tempfile("login-switch-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  first <- file.path(root, "first")
  second <- file.path(root, "second")
  local_mocked_bindings(app_state_dir = function() file.path(root, "app"), .package = "datapond")
  datapond:::init_db(first)
  datapond:::init_db(second)
  code <- datapond::setup_pond_access(second, "owner", first_name = "Pond", last_name = "Owner")
  datapond:::access_activate(second, "owner", code, "a long owner passphrase")
  code <- datapond::setup_pond_access(first, "owner", first_name = "Pond", last_name = "Owner")
  datapond:::access_activate(first, "owner", code, "a long owner passphrase")
  before <- unname(tools::md5sum(datapond:::get_db_path(second)))
  withr::local_options(datapond.data_pond = first)
  inserted_tabs <- character()
  original_insert_tab <- shiny::insertTab
  local_mocked_bindings(insertTab = function(inputId, tab, ...) {
    inserted_tabs <<- c(inserted_tabs, htmltools::tagGetAttribute(tab, "data-value"))
    original_insert_tab(inputId, tab, ...)
  }, .package = "shiny")
  selected_path <- NULL
  local_mocked_bindings(updateTextInput = function(session, inputId, label = NULL,
      value = NULL, placeholder = NULL) {
    if (identical(inputId, "access_pond_path")) selected_path <<- value
  }, .package = "shiny")

  shiny::testServer(datapond:::server, {
    session$setInputs(access_known_pond = "")
    session$setInputs(access_known_pond = second)
    expect_identical(selected_path, second)
    expect_identical(auth$pond, first)
    session$setInputs(access_open_pond = 0L)
    session$setInputs(access_pond_path = root, access_open_pond = 1L)
    expect_identical(auth$pond, first)
    expect_false(file.exists(datapond:::get_db_path(root)))

    session$setInputs(access_pond_path = second, access_open_pond = 2L)
    expect_identical(state$data_pond, second)
    expect_identical(auth$pond, second)
    expect_identical(datapond:::read_last_data_pond_path(), second)
    expect_false(auth$authenticated)
    expect_length(inserted_tabs, 0L)
    expect_false(datapond:::access_allowed("read"))
    expect_identical(unname(tools::md5sum(datapond:::get_db_path(second))), before)

    session$setInputs(access_login = 0L)
    session$setInputs(access_username = "owner", access_password = "a long owner passphrase", access_login = 1L)
    # Supply workspace controls as the browser does after successful sign-in.
    html <- xml2::read_html(as.character(datapond:::ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$access_workspace_ready <- auth$workspace_nonce
    do.call(session$setInputs, defaults)
    expect_true(datapond:::access_allowed("read"))
    expect_setequal(inserted_tabs, c("parties", "keyword_admin"))
    expect_length(inserted_tabs, 2L)
    saved <- datapond:::read_connected_pond_registry()
    expect_true(second %in% saved$data_pond_path)
    expect_true(file.exists(file.path(root, "app", "connected_data_ponds.rds")))
    auth$invalidate()
    html <- xml2::read_html(as.character(datapond:::access_login_ui(second, TRUE)))
    expect_true(second %in% xml2::xml_attr(xml2::xml_find_all(html,
      "//select[@id='access_known_pond']/option"), "value"))
    # A new login creates fresh workspace DOM, even for the same administrator.
    session$flushReact()
    session$setInputs(access_login = 2L)
    defaults$access_workspace_ready <- auth$workspace_nonce
    do.call(session$setInputs, defaults)
    expect_true(datapond:::access_allowed("admin"))
    expect_identical(inserted_tabs, rep(c("keyword_admin", "parties"), 2L))
    session$flushReact()
    expect_length(inserted_tabs, 4L)
    # Switch from the authenticated app, then sign in to the other pond.
    session$setInputs(set_manual = 0L)
    session$setInputs(manual_path = first, set_manual = 1L)
    expect_identical(auth$pond, first)
    expect_identical(state$data_pond, first)
    expect_false(auth$authenticated)
    page <- xml2::read_html(output$access_screen$html)
    expect_length(xml2::xml_find_all(page, "//*[@id='main_panel']"), 0L)
    expect_length(xml2::xml_find_all(page, "//*[@id='exit_datapond']"), 1L)
    session$setInputs(access_username = "owner", access_password = "a long owner passphrase", access_login = 3L)
    defaults$access_workspace_ready <- auth$workspace_nonce
    do.call(session$setInputs, defaults)
    expect_true(datapond:::access_allowed("admin"))
    page <- xml2::read_html(output$access_screen$html)
    expect_length(xml2::xml_find_all(page, "//*[@id='main_panel']"), 1L)
    expect_length(xml2::xml_find_all(page, "//form[@id='access_login_form']"), 0L)
    expect_length(xml2::xml_find_all(page, "//*[@id='exit_datapond']"), 0L)
    expect_length(inserted_tabs, 6L)
  })
})
