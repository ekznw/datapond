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
  expect_false(grepl(
    'id="current_dataset_folder"',
    html,
    fixed = TRUE
  ))
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
