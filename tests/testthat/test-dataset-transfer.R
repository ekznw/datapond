transfer_test_pond <- function(prefix) {
  pond <- tempfile(prefix)
  dir.create(pond, recursive = TRUE)
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  pond
}


transfer_test_user <- function(pond, first, last, email, user_name, admin = 0L) {
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO organisation_tbl",
      "(organisation_name_full, organisation_code) VALUES (?, ?)"
    ),
    params = list(paste(first, "Organisation"), toupper(substr(first, 1L, 3L)))
  )
  organisation_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1]
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO persons_tbl",
      "(name_first, name_last, email, organisation_id)",
      "VALUES (?, ?, ?, ?)"
    ),
    params = list(first, last, email, organisation_id)
  )
  person_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1]
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO user_tbl (person_id, user_name, is_active, is_admin)",
      "VALUES (?, ?, 1, ?)"
    ),
    params = list(person_id, user_name, as.integer(admin))
  )
  list(
    user_id = as.integer(DBI::dbGetQuery(
      conn,
      "SELECT last_insert_rowid() AS id"
    )$id[1]),
    person_id = as.integer(person_id),
    organisation_id = as.integer(organisation_id)
  )
}


test_that("dataset bundles round-trip files, metadata and source provenance", {
  source_pond <- transfer_test_pond("transfer-source-")
  target_pond <- transfer_test_pond("transfer-target-")
  on.exit(unlink(c(source_pond, target_pond), recursive = TRUE, force = TRUE), add = TRUE)

  editor <- transfer_test_user(
    source_pond,
    "Alice",
    "Editor",
    "alice@example.test",
    "alice"
  )
  importer <- transfer_test_user(
    target_pond,
    "Paul",
    "Importer",
    "paul@example.test",
    "paul",
    admin = 1L
  )

  nested_parent <- file.path(source_pond, "src", "programme")
  dir.create(nested_parent, recursive = TRUE)
  folder <- datapond:::create_dataset_folder_structure(
    base_path = nested_parent,
    folder_key = "portable_test",
    uuid = "transfer-uuid-001",
    parent_dir = ".",
    data_pond_id = datapond:::ensure_data_pond_id(source_pond)
  )
  writeLines("name,value\nexample,1", file.path(folder$data_path, "example.csv"))

  source_conn <- datapond:::get_conn(source_pond)
  DBI::dbExecute(
    source_conn,
    paste(
      "INSERT INTO entry_tbl",
      "(uuid, title, dataset_path, data_path, data_pond_id,",
      "created_by_user_id, lifecycle_status, folder_key)",
      "VALUES (?, ?, ?, ?, ?, ?, 'draft', 'portable_test')"
    ),
    params = list(
      "transfer-uuid-001",
      "Portable test",
      folder$dataset_path,
      folder$data_path,
      datapond:::ensure_data_pond_id(source_pond),
      editor$user_id
    )
  )
  source_dataset_id <- as.integer(DBI::dbGetQuery(
    source_conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1])
  DBI::dbExecute(
    source_conn,
    paste(
      "INSERT INTO authority_tbl",
      "(dataset_id, person_id, author_order, role)",
      "VALUES (?, ?, 1, 'author')"
    ),
    params = list(source_dataset_id, editor$person_id)
  )
  DBI::dbExecute(
    source_conn,
    paste(
      "INSERT INTO data_file_tbl",
      "(dataset_id, data_file_rel_path_full, file_name, file_path, data_pond_id)",
      "VALUES (?, 'example.csv', 'example.csv', ?, ?)"
    ),
    params = list(
      source_dataset_id,
      file.path(folder$data_path, "example.csv"),
      datapond:::ensure_data_pond_id(source_pond)
    )
  )
  DBI::dbDisconnect(source_conn)

  bundle <- datapond:::create_dataset_transfer_bundle(
    source_dataset_id,
    source_pond,
    editor$user_id
  )
  expect_true(file.exists(bundle$path))
  expect_identical(dirname(bundle$path), folder$dataset_path)

  manifest <- datapond:::read_dataset_transfer_manifest(bundle$path)
  expect_identical(manifest$source_parent_dir, "src")
  expect_identical(manifest$source_relative_parent, "programme")
  expect_identical(manifest$source_exported_by_user_id, editor$user_id)

  imported <- datapond:::import_dataset_transfer_bundle(
    bundle$path,
    target_pond,
    importer$user_id
  )
  record <- as.data.frame(datapond:::get_dataset_record(
    imported$dataset_id,
    target_pond
  ))
  provenance <- datapond:::get_dataset_import_provenance(
    imported$dataset_id,
    target_pond
  )

  expect_identical(record$uuid, "transfer-uuid-001")
  expect_identical(record$created_by_user_id, importer$user_id)
  expect_match(record$dataset_path, "/src/programme/", fixed = TRUE)
  expect_true(file.exists(file.path(record$data_path, "example.csv")))
  expect_equal(nrow(datapond:::get_dataset_authors(imported$dataset_id, target_pond)), 1L)
  expect_identical(provenance$source_data_pond_id, manifest$source_data_pond_id)
  expect_identical(provenance$source_created_by_user_id, editor$user_id)
  expect_identical(provenance$source_exported_by_user_id, editor$user_id)
  expect_identical(provenance$imported_by_user_id, importer$user_id)

  expect_error(
    datapond:::import_dataset_transfer_bundle(
      bundle$path,
      target_pond,
      importer$user_id
    ),
    "already exists"
  )
})


test_that("dataset transfer UI exposes drag-and-drop import and compression", {
  html <- as.character(datapond:::dataset_transfer_ui())
  expect_match(html, "Pond links", fixed = TRUE)
  expect_match(html, "Connected pond catalogues", fixed = TRUE)
  expect_match(html, "connected_dataset_table", fixed = TRUE)
  expect_match(html, "dataset_bundle_file", fixed = TRUE)
  expect_match(html, "Drag and drop a Data Pond ZIP here", fixed = TRUE)
  expect_match(html, "Compress selected dataset", fixed = TRUE)
  expect_match(html, "Import provenance", fixed = TRUE)
})
