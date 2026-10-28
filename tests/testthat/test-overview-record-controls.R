test_that("visibility uses the preview while lifecycle deletion binds to its loaded record", {
  local_trusted_access_fixture()
  pond <- tempfile("overview-record-controls-")
  datapond:::init_db(pond)
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  folder <- file.path(pond, "dev", "first")
  dir.create(folder, recursive = TRUE)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO persons_tbl (id, name_first, name_last) VALUES (1, 'Admin', 'One'), (2, 'Owner', 'Two')")
  DBI::dbExecute(conn, "INSERT INTO user_tbl (id, person_id, user_name, is_active, is_admin) VALUES (1, 1, 'admin', 1, 1), (2, 2, 'owner', 1, 0)")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (id, uuid, title, created_by_user_id, dataset_path) VALUES (1, 'first', 'First dataset', 2, ?), (2, 'second', 'Second dataset', 2, ?)", params = list(folder, file.path(pond, "dev", "second")))
  DBI::dbDisconnect(conn)

  # Run the actual renderers and action observers with independent selections.
  expressions <- list()
  names <- c("overview_record_id", "lifecycle_record_id", "pending_record_delete")
  inputs <- c("toggle_dataset_hidden", "soft_delete_dataset", "restore_dataset_record",
    "hard_delete_dataset", "confirm_hard_delete_dataset")
  walk <- function(x) {
    if (missing(x) || !is.call(x)) return()
    assignment <- identical(x[[1]], as.name("<-")) &&
      (any(vapply(names, function(name) identical(x[[2]], as.name(name)), logical(1))) ||
        identical(x[[2]], quote(output$dataset_record_controls)) ||
        identical(x[[2]], quote(output$lifecycle_record_controls)))
    observer <- identical(x[[1]], quote(shiny::observeEvent)) &&
      any(vapply(inputs, function(name) identical(x[[2]], substitute(input$NAME,
        list(NAME = as.name(name)))), logical(1)))
    if (assignment || observer) { expressions[[length(expressions) + 1L]] <<- x; return() }
    for (part in as.list(x)[-1L]) walk(part)
  }
  walk(body(datapond:::server))
  expect_length(expressions, 10L)
  test_server <- function(input, output, session) {
    state <- shiny::reactiveValues(data_pond = pond, active_user_id = 1L,
      current_dataset_id = 2L, current_dataset_uuid = "second", refresh_datasets = 0L)
    auth <- shiny::reactiveValues(workspace_nonce = "first-login")
    overview_preview_id <- shiny::reactiveVal(1L)
    pond_overview_selection_cleared <- shiny::reactiveVal(FALSE)
    connected <- shiny::reactiveVal(FALSE)
    overview_current_id <- shiny::reactive(if (connected()) NULL else overview_preview_id())
    pond_overview_all_data <- shiny::reactive({
      state$refresh_datasets
      get_datasets(pond, include_hidden = TRUE, include_deleted = TRUE)
    })
    refresh_after_record_control <- function() state$refresh_datasets <- state$refresh_datasets + 1L
    for (expr in expressions) eval(expr)
  }
  environment(test_server) <- list2env(list(pond = pond, expressions = expressions), parent = asNamespace("datapond"))
  shiny::testServer(test_server, {
    session$flushReact()
    expect_false(grepl('id="hard_delete_dataset"', output$dataset_record_controls$html, fixed = TRUE))
    expect_false(grepl('id="soft_delete_dataset"', output$dataset_record_controls$html, fixed = TRUE))
    expect_null(output$lifecycle_record_controls)
    session$setInputs(main_dataset_tabs = "dataset_lifecycle")
    expect_match(output$lifecycle_record_controls$html, "Second dataset", fixed = TRUE)
    expect_match(output$lifecycle_record_controls$html, "Hard-delete record", fixed = TRUE)
    state$active_user_id <- 2L
    session$flushReact()
    expect_false(grepl("Hard-delete record", output$lifecycle_record_controls$html, fixed = TRUE))
    state$active_user_id <- NULL
    session$flushReact()
    expect_false(grepl('id="soft_delete_dataset"', output$lifecycle_record_controls$html, fixed = TRUE))
    state$active_user_id <- 1L
    connected(TRUE)
    session$flushReact()
    expect_null(output$dataset_record_controls)
    connected(FALSE)
    session$setInputs(toggle_dataset_hidden = 1L)
    expect_true(datapond:::dataset_record_permissions(1L, 1L, pond)$is_hidden)
    expect_false(datapond:::dataset_record_permissions(2L, 1L, pond)$is_hidden)
    state$current_dataset_id <- 1L
    overview_preview_id(2L)
    session$setInputs(soft_delete_dataset = 1L)
    expect_true(datapond:::dataset_record_permissions(1L, 1L, pond)$is_deleted)
    expect_false(datapond:::dataset_record_permissions(2L, 1L, pond)$is_deleted)
    expect_match(output$lifecycle_record_controls$html, "Restore record", fixed = TRUE)
    session$setInputs(restore_dataset_record = 1L)
    expect_false(datapond:::dataset_record_permissions(1L, 1L, pond)$is_deleted)
    session$setInputs(hard_delete_dataset = 1L)
    expect_identical(pending_record_delete()$id, 1L)
    state$current_dataset_id <- 2L
    session$setInputs(confirm_hard_delete_dataset = 1L)
    expect_null(pending_record_delete())
    expect_false(is.null(datapond:::get_dataset_record(1L, pond)))
    state$current_dataset_id <- 1L
    session$setInputs(hard_delete_dataset = 2L)
    auth$workspace_nonce <- "new-login"
    session$setInputs(confirm_hard_delete_dataset = 2L)
    expect_false(is.null(datapond:::get_dataset_record(1L, pond)))
    session$setInputs(hard_delete_dataset = 3L)
    session$setInputs(main_dataset_tabs = "pond_overview", confirm_hard_delete_dataset = 3L)
    expect_false(is.null(datapond:::get_dataset_record(1L, pond)))
    session$setInputs(soft_delete_dataset = 2L)
    expect_false(datapond:::dataset_record_permissions(1L, 1L, pond)$is_deleted)
    session$setInputs(main_dataset_tabs = "dataset_lifecycle", hard_delete_dataset = 4L)
    session$setInputs(confirm_hard_delete_dataset = 4L)
    expect_null(datapond:::get_dataset_record(1L, pond))
    expect_false(is.null(datapond:::get_dataset_record(2L, pond)))
    expect_null(state$current_dataset_id)
    expect_true(dir.exists(folder))
    expect_null(output$lifecycle_record_controls)
  })
})

test_that("lifecycle places guidance and record actions in its own tab", {
  html <- xml2::read_html(as.character(datapond:::ui))
  slot <- xml2::xml_find_all(html, "//*[@id='lifecycle_record_controls']")
  expect_length(slot, 1L)
  expect_identical(xml2::xml_attr(xml2::xml_find_first(slot,
    "ancestor::div[@data-value][1]"), "data-value"), "dataset_lifecycle")
  cards <- xml2::xml_find_all(html, "//section[@class='lifecycle-help-card']")
  expect_length(cards, 4L)
  expect_match(xml2::xml_text(cards[[4]]), "Neither action deletes dataset files", fixed = TRUE)
})
