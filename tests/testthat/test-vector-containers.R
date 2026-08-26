make_multilayer_vector_container <- function(path, filetype) {
  first <- terra::vect(
    data.frame(
      x = c(18, 19),
      y = c(-29, -28),
      first_id = c(1L, 2L),
      label = c("alpha", "beta")
    ),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  second <- terra::vect(
    data.frame(
      x = c(20, 21),
      y = c(-27, -26),
      second_code = c("C", "D")
    ),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )

  tryCatch(
    {
      terra::writeVector(
        first,
        path,
        layer = "first_points",
        filetype = filetype,
        overwrite = TRUE
      )
      terra::writeVector(
        second,
        path,
        layer = "second_points",
        filetype = filetype,
        insert = TRUE
      )
    },
    error = function(e) {
      testthat::skip(paste(
        "GDAL cannot create the test container:",
        conditionMessage(e)
      ))
    }
  )

  path
}


test_that("file geodatabases are inventoried as atomic containers", {
  skip_if_not_installed("terra")
  skip_if_not_installed("ekznwr")

  root <- tempfile("atomic-gdb-inventory-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  gdb <- make_multilayer_vector_container(
    file.path(root, "multi.gdb"),
    "OpenFileGDB"
  )

  # This is the kind of internal file that previously leaked into inventory.
  writeLines("internal", file.path(gdb, "a00000001.freelist"))

  inventory <- datapond:::scan_dataset_files(root)

  expect_false("message" %in% names(inventory))
  expect_equal(nrow(inventory), 1L)
  expect_identical(inventory$data_file_rel_path_full, "multi.gdb")
  expect_identical(inventory$data_file_format, "esri_file_geodatabase")
  expect_true(inventory$is_container)
  expect_true(dir.exists(inventory$file_path))
  expect_false(any(grepl("\\.gdb/", inventory$file_path, ignore.case = TRUE)))
  expect_false(any(grepl("freelist$", inventory$file_path, ignore.case = TRUE)))
})


test_that("spatial and attribute metadata are extracted for every GDB layer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("ekznwr")

  root <- tempfile("multilayer-gdb-metadata-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  gdb <- make_multilayer_vector_container(
    file.path(root, "multi.gdb"),
    "OpenFileGDB"
  )
  inventory <- datapond:::scan_dataset_files(root)

  spatial <- datapond:::extract_spatial_metadata(inventory, root)
  attributes <- datapond:::profile_data_file_attributes(
    gdb,
    "esri_file_geodatabase"
  )

  expect_setequal(
    spatial$layer_name[spatial$spatial_status == "ok"],
    c("first_points", "second_points")
  )
  expect_true(all(spatial$feature_count == 2L))
  expect_setequal(
    unique(attributes$layer_name),
    c("first_points", "second_points")
  )
  expect_setequal(
    attributes[layer_name == "first_points", data_field_name],
    c("first_id", "label")
  )
  expect_identical(
    attributes[layer_name == "second_points", data_field_name],
    "second_code"
  )
})


test_that("generic multilayer containers use named layers", {
  skip_if_not_installed("terra")

  root <- tempfile("multilayer-gpkg-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  gpkg <- make_multilayer_vector_container(
    file.path(root, "multi.gpkg"),
    "GPKG"
  )

  spatial <- datapond:::extract_spatial_metadata_one(gpkg, "vector")
  attributes <- datapond:::profile_data_file_attributes(gpkg, "vector")

  expect_setequal(
    spatial$layer_name,
    c("first_points", "second_points")
  )
  expect_setequal(
    unique(attributes$layer_name),
    c("first_points", "second_points")
  )
})


test_that("ordinary single-layer vectors retain direct-read behaviour", {
  skip_if_not_installed("terra")

  root <- tempfile("single-layer-vector-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  path <- file.path(root, "single.geojson")
  vector <- terra::vect(
    data.frame(
      x = c(18, 19),
      y = c(-29, -28),
      identifier = c(1L, 2L)
    ),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  terra::writeVector(vector, path, filetype = "GeoJSON", overwrite = TRUE)

  spatial <- datapond:::extract_spatial_metadata_one(path, "vector")
  attributes <- datapond:::profile_data_file_attributes(path, "vector")

  expect_equal(nrow(spatial), 1L)
  expect_true(is.na(spatial$layer_name))
  expect_true(all(is.na(attributes$layer_name)))
  expect_identical(attributes$data_field_name, "identifier")
})


test_that("unreadable vector containers warn without throwing", {
  skip_if_not_installed("terra")

  root <- tempfile("unreadable-vector-container-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  path <- file.path(root, "empty.gdb")
  dir.create(path)
  warnings <- character()
  result <- withCallingHandlers(
    datapond:::extract_spatial_metadata_one(
      path,
      "esri_file_geodatabase"
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(result$spatial_status, "no_layers_readable")
  expect_true(any(grepl("No readable vector layers", warnings, fixed = TRUE)))
})


test_that("container layer keys are retained when metadata is saved", {
  skip_if_not_installed("terra")
  skip_if_not_installed("ekznwr")

  root <- tempfile("saved-gdb-metadata-")
  pond <- tempfile("saved-gdb-pond-")
  dir.create(root, recursive = TRUE)
  dir.create(pond, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  gdb <- make_multilayer_vector_container(
    file.path(root, "multi.gdb"),
    "OpenFileGDB"
  )
  inventory <- datapond:::scan_dataset_files(root)
  spatial <- datapond:::extract_spatial_metadata(inventory, root)
  spatial[, `:=`(
    data_file_rel_path_full = "multi.gdb",
    file_name = "multi.gdb"
  )]
  attributes <- datapond:::profile_data_file_attributes(
    gdb,
    "esri_file_geodatabase"
  )

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title) VALUES ('GDB test')")
  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  datapond:::save_data_file_scan(
    dataset_id,
    inventory,
    pond,
    data_dir = root
  )
  datapond:::save_data_file_spatial_metadata(dataset_id, spatial, pond)

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data_file_row <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM data_file_tbl WHERE dataset_id = ?",
    params = list(dataset_id)
  )
  datapond:::save_attribute_profile(
    conn = conn,
    dataset_id = dataset_id,
    data_file_row = data_file_row,
    data_pond_id = data_file_row$data_pond_id[[1L]],
    profile_dt = attributes,
    profile_type = "vector_attributes"
  )

  saved_spatial <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT data_file_rel_path_full, layer_name",
      "FROM data_file_spatial_metadata_tbl WHERE dataset_id = ?"
    ),
    params = list(dataset_id)
  )
  saved_attributes <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT data_file_rel_path_full, layer_name, data_field_name",
      "FROM dataset_attribute_tbl WHERE dataset_id = ?"
    ),
    params = list(dataset_id)
  )

  expect_true(all(saved_spatial$data_file_rel_path_full == "multi.gdb"))
  expect_setequal(
    saved_spatial$layer_name,
    c("first_points", "second_points")
  )
  expect_true(all(saved_attributes$data_file_rel_path_full == "multi.gdb"))
  expect_setequal(
    unique(saved_attributes$layer_name),
    c("first_points", "second_points")
  )
})
