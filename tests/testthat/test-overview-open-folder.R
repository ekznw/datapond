# Exercise the actual selection and button handlers without launching a desktop.
server_expression <- function(predicate) {
  found <- NULL
  walk <- function(x) {
    if (missing(x) || !is.call(x) || !is.null(found)) return()
    if (predicate(x)) { found <<- x; return() }
    for (part in as.list(x)[-1L]) walk(part)
  }
  walk(body(datapond:::server))
  stopifnot(!is.null(found))
  found
}

overview_observer_body <- function(input_name) {
  server_expression(function(x) {
    length(x) >= 3L &&
      as.character(x[[1]])[1] %in% c("observeEvent", "::") &&
      identical(x[[2]], substitute(input$NAME, list(NAME = as.name(input_name))))
  })[[3]]
}

test_that("overview table and map selections open their folder without hydrating the editor", {
  rows <- data.table::data.table(id = 1:2, active_pond = TRUE,
    dataset_path = c("/pond/A", "/pond/B"))
  e <- new.env(parent = asNamespace("datapond"))
  e$input <- list(main_dataset_tabs = "pond_overview", dataset_table_rows_selected = 2L)
  e$state <- list(dataset_path = "/pond/A", current_dataset_folder = "/pond/A")
  e$access_require <- function(...) TRUE
  e$access_allowed <- function(...) TRUE
  e$overview_preview_id <- shiny::reactiveVal(1L)
  e$overview_selected_connection <- shiny::reactiveVal(NULL)
  e$pond_overview_selection_cleared <- shiny::reactiveVal(FALSE)
  e$overview_current_id <- function() e$overview_preview_id()
  e$overview_inventory_data <- e$pond_overview_data <- function() rows
  opened <- NULL
  notice <- NULL
  e$open_folder <- function(path) opened <<- path
  e$showNotification <- function(message, ...) notice <<- message
  button <- overview_observer_body("open_dataset_folder")
  shiny::isolate({
    eval(overview_observer_body("dataset_table_rows_selected"), e)
    eval(button, e)
    expect_identical(opened, "/pond/B")
    expect_identical(e$state$dataset_path, "/pond/A")
    eval(overview_observer_body("open_data_folder"), e)
    expect_identical(opened, "/pond/B/data")
    e$state$data_pond <- "/pond"
    eval(overview_observer_body("overview_open_pond_folder"), e)
    expect_identical(opened, "/pond")
    all_rows <- rows
    rows <- rows[2]
    e$input$dataset_table_rows_selected <- 1L
    eval(overview_observer_body("dataset_table_rows_selected"), e)
    eval(button, e)
    expect_identical(opened, "/pond/B")
    rows <- all_rows
    eval(server_expression(function(x) identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("select_pond_map_dataset"))), e)
    e$select_pond_map_dataset(list(id = "1"))
    eval(button, e)
    expect_identical(opened, "/pond/A")
    e$select_pond_map_dataset(list(id = "2"))
    eval(button, e)
    expect_identical(opened, "/pond/B")
    opened <- NULL
    e$pond_overview_selection_cleared(TRUE)
    eval(button, e)
    expect_null(opened)
    expect_match(notice, "Select a dataset")
    e$pond_overview_selection_cleared(FALSE)
    rows <- rows[1]
    eval(button, e)
    expect_null(opened)
    expect_match(notice, "search results")
    e$overview_selected_connection(list(id = 2L))
    eval(button, e)
    expect_null(opened)
    expect_match(notice, "connected pond")
    e$input$main_dataset_tabs <- "dataset_workspace"
    eval(button, e)
    expect_identical(opened, "/pond/A")
  })
})

test_that("overview folder paths reject absent metadata", {
  resolve <- datapond:::pond_overview_folder_path
  expect_error(resolve(data.frame(id = 1, dataset_path = NA_character_), 1), "No folder path")
  expect_error(resolve(data.frame(id = integer()), NULL), "Select a dataset")
  expect_error(datapond:::open_folder(tempfile()), "Folder does not exist")
})

test_that("overview location repair targets only the preview without replacing another editor", {
  e <- new.env(parent = asNamespace("datapond"))
  e$state <- list(data_pond = "/pond", current_dataset_id = 1L,
    dataset_path = "/pond/A", data_path = "/pond/A/data", current_dataset_folder = "/pond/A")
  e$overview_record_id <- function() 2L
  e$access_require <- e$require_active_user_for_write <- function(...) TRUE
  e$reconcile_dataset_locations <- function(base_path, dataset_ids) {
    expect_identical(base_path, "/pond")
    expect_identical(dataset_ids, 2L)
    data.table::data.table(id = 2L, location_status = "ok")
  }
  e$get_dataset_record <- function(dataset_id, base_path) {
    expect_identical(dataset_id, 2L)
    list(dataset_path = "/pond/B", data_path = "/pond/B/data")
  }
  e$refresh_dataset_context <- function(...) NULL
  notice <- NULL
  e$showNotification <- function(message, ...) notice <<- message
  eval(overview_observer_body("repair_dataset_location"), e)
  expect_match(notice, "/pond/B", fixed = TRUE)
  expect_identical(e$state$dataset_path, "/pond/A")
  expect_identical(e$state$data_path, "/pond/A/data")
})

test_that("folder launcher detects missing executables and immediate launch failures", {
  opener <- datapond:::open_folder
  e <- new.env(parent = environment(opener))
  environment(opener) <- e
  e$Sys.info <- function() c(sysname = "Linux")
  e$Sys.which <- function(...) ""
  expect_error(opener(tempdir()), "No folder launcher")
  e$Sys.which <- function(...) "/usr/bin/xdg-open"
  e$system2 <- function(...) 1L
  expect_error(opener(tempdir()), "Could not start")
  folder <- tempfile("folder with spaces ")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE))
  e$system2 <- function(command, args, wait) {
    expect_identical(command, "/usr/bin/xdg-open")
    expect_identical(args, shQuote(normalizePath(folder)))
    expect_false(wait)
    0L
  }
  expect_true(opener(folder))
})
