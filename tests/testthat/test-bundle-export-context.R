test_that("bundle export context renders saved, absent and cleared datasets", {
  pond <- tempfile("bundle-context-")
  datapond:::init_db(pond)
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, paste(
    "INSERT INTO entry_tbl (id, title, dataset_path, uuid)",
    "VALUES (1, 'Example dataset', '/pond/example', 'example-uuid')"
  ))
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (id) VALUES (2)")
  DBI::dbDisconnect(conn)

  # Exercise the production renderer with the real record lookup contract.
  renderer <- NULL
  walk <- function(x) {
    if (missing(x) || !is.call(x) || !is.null(renderer)) return()
    if (identical(x[[1]], as.name("<-")) &&
        identical(x[[2]], quote(output$dataset_bundle_export_context))) {
      renderer <<- x
      return()
    }
    for (part in as.list(x)[-1L]) walk(part)
  }
  walk(body(datapond:::server))
  expect_false(is.null(renderer))
  test_server <- function(input, output, session) {
    state <- shiny::reactiveValues(current_dataset_id = NULL, data_pond = pond)
    access_require <- function(...) TRUE
    eval(renderer)
  }
  environment(test_server) <- list2env(list(pond = pond, renderer = renderer),
    parent = asNamespace("datapond"))
  shiny::testServer(test_server, {
    expect_match(output$dataset_bundle_export_context$html, "Select and save a dataset")
    state$current_dataset_id <- 1L
    session$flushReact()
    html <- output$dataset_bundle_export_context$html
    expect_match(html, "Example dataset")
    expect_match(html, "/pond/example", fixed = TRUE)
    expect_match(html, "example-uuid")
    state$current_dataset_id <- 2L
    session$flushReact()
    html <- output$dataset_bundle_export_context$html
    expect_match(html, "Untitled dataset")
    expect_match(html, "No folder")
    state$current_dataset_id <- 999L
    session$flushReact()
    expect_match(output$dataset_bundle_export_context$html, "Dataset not found")
    for (id in list(NULL, integer(), NA_integer_)) {
      state$current_dataset_id <- id
      session$flushReact()
      expect_match(output$dataset_bundle_export_context$html, "Select and save a dataset")
    }
  })
})
