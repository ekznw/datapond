test_that("pond search and selected location details surround the map", {
  html <- as.character(datapond:::pond_overview_ui())

  search_position <- regexpr(
    'id="pond_dataset_search"',
    html,
    fixed = TRUE
  )[[1L]]
  map_position <- regexpr(
    'id="pond_dataset_map"',
    html,
    fixed = TRUE
  )[[1L]]
  detail_position <- regexpr(
    'id="selected_dataset_location_details"',
    html,
    fixed = TRUE
  )[[1L]]

  expect_gt(search_position, 0L)
  expect_gt(map_position, search_position)
  expect_gt(detail_position, map_position)
  expect_gt(regexpr('id="pond_map_display"', html, fixed = TRUE)[[1]], map_position)
  expect_gt(regexpr("Points show bounding-box centres", html, fixed = TRUE)[[1]], map_position)
  expect_gt(regexpr('id="overview_dataset_abstract"', html, fixed = TRUE)[[1]], map_position)
  expect_match(html, 'id="overview_current_only"', fixed = TRUE)
  expect_match(
    html,
    'id="refresh_pond_overview"',
    fixed = TRUE
  )
  expect_match(
    html,
    "Clear the overview selection and refresh the dataset map",
    fixed = TRUE
  )
  expect_false(grepl(
    'id="current_dataset_folder"',
    html,
    fixed = TRUE
  ))
})


test_that("pond refresh clears only its selection and invalidates the map", {
  server_source <- paste(
    deparse(body(datapond:::server)),
    collapse = "\n"
  )

  expect_match(
    server_source,
    "input$refresh_pond_overview",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "pond_overview_selection_cleared(TRUE)",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "pond_overview_map_refresh\\(pond_overview_map_refresh\\(\\)\\s*\\+\\s*1L\\)"
  )
  expect_match(
    server_source,
    "select_overview_table_rows(integer())",
    fixed = TRUE
  )
})


test_that("pond inventory omits location and visibility detail columns", {
  overview <- data.table::data.table(
    id = 1L,
    overview_class = "Development metadata incomplete",
    display_title = "Test dataset",
    authors = "Example Author",
    pond_location = "Development (dev/)",
    lifecycle_label = "Draft",
    metadata_readiness = "Metadata incomplete",
    next_action = "Complete metadata",
    spatial_extent = "No valid bounding box",
    version = "1",
    citation_date = "2026",
    dataset_path = "/pond/dev/test",
    organisations = "Example Org",
    poc = "Example Contact",
    record_visibility = "Visible",
    location_status = "ok",
    record_owner = "Record Owner"
  )

  display <- datapond:::pond_overview_inventory_display(
    overview
  )

  expect_false(any(c(
    "Pond location",
    "Dataset folder",
    "Record visibility",
    "Location check",
    "Contacts",
    "Record owner"
  ) %in% names(display)))
  expect_true(all(c(
    "Title",
    "Lifecycle status",
    "Spatial extent",
    "Organisations"
  ) %in% names(display)))
})


test_that("pond inventory abbreviates multiple authors after the first", {
  expect_identical(
    datapond:::pond_overview_author_label(c(
      "First Author",
      "First Author, Second Author",
      "First Author, Second Author, Third Author",
      NA_character_,
      ""
    )),
    c(
      "First Author",
      "First Author et al.",
      "First Author et al.",
      NA_character_,
      NA_character_
    )
  )

  overview <- data.table::data.table(
    id = 1L,
    overview_class = "Development metadata incomplete",
    display_title = "Test dataset",
    authors = "First Author, Second Author",
    lifecycle_label = "Draft",
    metadata_readiness = "Metadata incomplete",
    next_action = "Complete metadata",
    spatial_extent = "No valid bounding box",
    version = "1.0.0",
    citation_date = "2026",
    organisations = "Example Org"
  )
  display <- datapond:::pond_overview_inventory_display(overview)
  expect_identical(display$Authors, "First Author et al.")
})


test_that("pond inventory keeps title authors and date in the compact view", {
  definitions <- datapond:::pond_overview_responsive_column_defs()
  essential <- definitions[vapply(
    definitions,
    function(definition) identical(definition$className, "all"),
    logical(1)
  )]

  expect_setequal(
    vapply(essential, function(definition) definition$targets, integer(1)),
    c(2L, 3L, 9L, 11L)
  )
  expect_identical(
    vapply(essential, function(definition) definition$responsivePriority, integer(1)),
    1:4
  )
  expect_identical(essential[[1]]$width, "300px")

  options <- datapond:::pond_overview_responsive_table_options()
  expect_false(options$scrollX)
  expect_false(options$responsive$details)
  expect_true(options$deferRender)
  expect_identical(options$columnDefs, definitions)
  expect_identical(
    datapond:::pond_overview_table_extensions(),
    c("Buttons", "Responsive")
  )

  server_source <- paste(deparse(body(datapond:::server)), collapse = "\n")
  expect_match(server_source, "pond_overview_responsive_table_options", fixed = TRUE)
  expect_match(server_source, "pond_overview_table_extensions", fixed = TRUE)
})

test_that("indexed overview search reuses text and keeps copies in different ponds distinct", {
  current <- data.table::data.table(id = 1L, title = "River samples", display_title = "River samples",
    authors = "A. Researcher", uuid = "same-uuid", dataset_path = "/current/src/rivers")
  other <- data.table::data.table(id = 1L, title = "River samples", uuid = "same-uuid",
    pond_name = "Team archive", stage = "src", connection_type = "Local pond",
    dataset_path = "/archive/src/rivers", open_path = "/archive", availability = "Local catalogue")
  rows <- merge_pond_overview_inventory(current, other, "Field pond", "/current")
  expect_equal(nrow(rows), 2L)
  expect_equal(length(unique(rows$record_key)), 2L)
  expect_equal(rows$id, c(1L, NA_integer_))
  testthat::local_mocked_bindings(pond_overview_search_index = function(...) stop("Index should already be cached"))
  expect_equal(filter_pond_overview_data(rows, "archive river")$pond_name, "Team archive")
  expect_equal(filter_pond_overview_data(rows, "field researcher")$pond_name, "Field pond")
  expect_equal(nrow(filter_pond_overview_data(rows, "unknown")), 0L)
  expect_equal(nrow(filter_pond_overview_data(rows, "")), 2L)
  expect_equal(current$id, 1L)
  expect_false("active_pond" %in% names(current))
})


test_that("lifecycle tab exposes selected and pond-wide folder verification", {
  html <- as.character(
    datapond:::mod_dataset_lifecycle_ui("lifecycle")
  )

  expect_match(
    html,
    'id="lifecycle-verify_selected_folder"',
    fixed = TRUE
  )
  expect_match(
    html,
    'id="lifecycle-verify_all_folders"',
    fixed = TRUE
  )
  expect_match(
    html,
    "same UUID-marker and lifecycle-root location check",
    fixed = TRUE
  )
})


test_that("folder reconciliation can be limited to selected datasets", {
  pond <- tempfile("selected-location-check-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "
    INSERT INTO entry_tbl (uuid, title, dataset_path)
    VALUES
      ('selected_001', 'Selected', ?),
      ('other_002', 'Other', ?)
    ",
    params = list(
      file.path(pond, "dev", "selected__selected_001"),
      file.path(pond, "dev", "other__other_002")
    )
  )
  ids <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM entry_tbl ORDER BY id"
  )$id
  DBI::dbDisconnect(conn)

  selected_report <- datapond:::reconcile_dataset_locations(
    pond,
    dataset_ids = ids[1]
  )

  expect_identical(selected_report$id, as.integer(ids[1]))
  expect_identical(nrow(selected_report), 1L)
})
