test_that("a file review requires an active user", {
  pond <- tempfile("file-review-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  datapond:::init_db(pond)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (title) VALUES ('File review test')"
  )
  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO data_file_tbl",
      "(dataset_id, data_file_rel_path_full)",
      "VALUES (?, ?)"
    ),
    params = list(dataset_id, "grid/test_grid")
  )
  data_file_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  expect_error(
    datapond:::update_data_file_review(
      data_file_id = data_file_id,
      base_path = pond
    ),
    "user_id is required"
  )

  expect_error(
    datapond:::update_data_file_review(
      data_file_id = data_file_id,
      base_path = pond,
      data_file_description = "Reviewed ESRI grid",
      data_file_notes = "No active session user",
      content_hint = "original data",
      review_status = "described",
      include_in_attribute_scan = TRUE,
      user_id = NULL
    ),
    "user_id is required"
  )

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO persons_tbl (name_first, name_last) VALUES ('Test', 'User')"
  )
  person_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbExecute(
    conn,
    "INSERT INTO user_tbl (person_id, user_name) VALUES (?, 'test_user')",
    params = list(person_id)
  )
  user_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  expect_silent(
    datapond:::update_data_file_review(
      data_file_id = data_file_id,
      base_path = pond,
      data_file_description = "Reviewed ESRI grid",
      data_file_notes = "Saved by the active session user",
      content_hint = "original data",
      review_status = "described",
      include_in_attribute_scan = TRUE,
      user_id = user_id
    )
  )

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  saved <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT data_file_description, review_status,",
      "include_in_attribute_scan, updated_by_user_id",
      "FROM data_file_tbl WHERE id = ?"
    ),
    params = list(data_file_id)
  )

  expect_identical(saved$data_file_description, "Reviewed ESRI grid")
  expect_identical(saved$review_status, "described")
  expect_identical(saved$include_in_attribute_scan, 1L)
  expect_identical(saved$updated_by_user_id, as.integer(user_id))
})
