test_that("overview polling redraws only for changed data or connected access", {
  root <- tempfile("overview-refresh-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(root, "app"), .package = "datapond")
  first <- file.path(root, "first")
  second <- file.path(root, "second")
  password <- "overview refresh test passphrase"
  for (pond in c(first, second)) {
    datapond:::ensure_pond_structure(pond)
    datapond:::init_db(pond)
    code <- datapond::setup_pond_access(pond, "owner", first_name = "Test", last_name = "Owner")
    datapond:::access_activate(pond, "owner", code, password)
  }
  datapond:::register_connected_pond(second, "Connected pond")
  con <- DBI::dbConnect(RSQLite::SQLite(), datapond:::get_db_path(second))
  DBI::dbExecute(con, "INSERT INTO entry_tbl (uuid, title, dataset_path, lifecycle_status)
    VALUES ('refresh-record', 'Original title', ?, 'draft')", params = list(file.path(second, "dev", "record")))
  DBI::dbDisconnect(con)
  withr::local_options(datapond.data_pond = first)

  shiny::testServer(datapond:::server, {
    session$setInputs(access_login = 0L)
    session$setInputs(access_username = "owner", access_password = password, access_login = 1L)
    html <- xml2::read_html(as.character(datapond:::ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$access_workspace_ready <- auth$workspace_nonce
    defaults$overview_current_only <- FALSE
    do.call(session$setInputs, defaults)
    datapond:::connected_pond_sign_in(second, "owner", password)
    session$flushReact()
    expect_true("Original title" %in% overview_inventory_data()$title)
    updates <- new.env(); updates$n <- 0L
    watcher <- shiny::observe({
      overview_inventory_data()
      updates$n <- updates$n + 1L
    })
    session$flushReact()
    before <- updates$n
    for (i in 1:3) {
      auth$tick <- auth$tick + 1L
      session$flushReact()
    }
    expect_identical(updates$n, before)

    session$setInputs(dataset_table_rows_selected = which(!overview_inventory_data()$active_pond))
    selected_key <- overview_selected_connection()$record_key
    # DT briefly reports no rows while replacing its widget.
    session$setInputs(dataset_table_rows_selected = integer())
    expect_identical(overview_selected_connection()$record_key, selected_key)

    # Simulate an external catalogue edit while this browser remains idle.
    con <- DBI::dbConnect(RSQLite::SQLite(), datapond:::get_db_path(second))
    DBI::dbExecute(con, "UPDATE entry_tbl SET title='Changed title' WHERE uuid='refresh-record'")
    DBI::dbDisconnect(con)
    Sys.setFileTime(datapond:::get_db_path(second), Sys.time() + 2)
    auth$tick <- auth$tick + 1L
    session$flushReact()
    expect_true("Changed title" %in% overview_inventory_data()$title)
    expect_identical(overview_selected_connection()$record_key, selected_key)
    expect_identical(overview_selected_connection()$title, "Changed title")
    expect_gt(updates$n, before)

    # Revocation still removes cached rows on the next security poll.
    datapond:::access_logout(second, auth$connected_tokens[[datapond:::access_path(second)]])
    auth$tick <- auth$tick + 1L
    session$flushReact()
    expect_false("Changed title" %in% overview_inventory_data()$title)
    expect_null(overview_selected_connection())
    expect_null(overview_current_id())
    expect_match(paste(attr(federated_pond_catalogue(), "unavailable"), collapse = " "), "Sign-in required")
    expect_true(datapond:::access_allowed("read"))
    before <- updates$n
    auth$tick <- auth$tick + 1L
    session$flushReact()
    expect_identical(updates$n, before)
    watcher$destroy()

    datapond:::connected_pond_sign_in(second, "owner", password)
    session$flushReact()
    session$setInputs(dataset_table_rows_selected = which(!overview_inventory_data()$active_pond))
    session$setInputs(overview_open_connected = 1L)
    expect_identical(state$data_pond, second)
    expect_identical(auth$pond, second)
    expect_false(auth$authenticated)
  })
})
