spatial_status_row <- function(pond, dataset_id) {
  status <- datapond:::get_dataset_completion_status(
    dataset_id = dataset_id,
    base_path = pond,
    schema_grouped = NULL
  )

  status <- as.data.frame(status)
  status[
    status$section == "Spatial metadata",
    ,
    drop = FALSE
  ]
}


test_that("spatial reviews and manual extents update dataset status", {
  pond <- tempfile("spatial-status-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (title) VALUES ('Spatial status test')"
  )
  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  initial <- spatial_status_row(pond, dataset_id)
  expect_identical(initial$status, "not_started")
  expect_identical(initial$saved_records, 0L)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "
    INSERT INTO data_file_spatial_metadata_tbl (
      dataset_id,
      data_file_rel_path_full,
      layer_key,
      spatial_status,
      spatial_type
    )
    VALUES (?, 'data/test.gpkg', 'habitat', 'ok', 'vector')
    ",
    params = list(dataset_id)
  )
  spatial_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  extracted <- spatial_status_row(pond, dataset_id)
  expect_identical(extracted$status, "needs_review")
  expect_identical(extracted$required_missing, 1L)
  expect_identical(extracted$saved_records, 1L)

  datapond:::update_data_file_spatial_metadata_notes(
    spatial_metadata_id = spatial_id,
    base_path = pond,
    user_notes = "CRS and geometry reviewed."
  )

  reviewed <- spatial_status_row(pond, dataset_id)
  expect_identical(reviewed$status, "complete")
  expect_identical(reviewed$required_missing, 0L)

  datapond:::save_manual_spatial_extent(
    dataset_id = dataset_id,
    base_path = pond,
    extent_label = "Authoritative project extent",
    crs_value = "EPSG:4326",
    xmin = 30,
    ymin = -30,
    xmax = 31,
    ymax = -29,
    source_note = "Digitised from the approved project map."
  )

  with_extent <- spatial_status_row(pond, dataset_id)
  expect_identical(with_extent$status, "complete")
  expect_identical(with_extent$saved_records, 2L)
  expect_identical(with_extent$required_missing, 0L)
})
