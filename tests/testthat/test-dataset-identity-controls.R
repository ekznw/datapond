make_test_pond <- function(prefix = "dataset-identity-") {
  pond <- tempfile(prefix)
  dir.create(pond, recursive = TRUE)
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  pond
}


test_that("record-control migration preserves legacy entries", {
  pond <- tempfile("legacy-record-control-")
  dir.create(pond, recursive = TRUE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(pond, "metadata.db"))
  DBI::dbExecute(conn, paste(
    "CREATE TABLE entry_tbl (id INTEGER PRIMARY KEY, uuid TEXT UNIQUE,",
    "title TEXT, citation_date TEXT, version TEXT, abstract TEXT)"
  ))
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (uuid, title) VALUES ('legacy_uuid', 'Legacy')"
  )
  DBI::dbDisconnect(conn)

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  fields <- DBI::dbListFields(conn, "entry_tbl")
  expect_true(all(c(
    "created_by_user_id", "is_hidden", "hidden_at",
    "deleted_at", "deleted_by_user_id"
  ) %in% fields))
  legacy <- DBI::dbGetQuery(conn, "SELECT title, is_hidden FROM entry_tbl")
  expect_identical(legacy$title, "Legacy")
  expect_identical(legacy$is_hidden, 0L)
})


insert_test_dataset <- function(
  pond,
  uuid,
  dataset_path,
  owner_user_id = NA_integer_,
  title = "Test dataset"
) {
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO entry_tbl",
      "(uuid, title, dataset_path, data_path, data_pond_id, created_by_user_id)",
      "VALUES (?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      uuid,
      title,
      dataset_path,
      file.path(dataset_path, "data"),
      datapond:::ensure_data_pond_id(pond),
      owner_user_id
    )
  )
  DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[[1L]]
}


test_that("dataset roots contain identity markers and standard subfolders", {
  pond <- make_test_pond("dataset-marker-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  pond_id <- datapond:::ensure_data_pond_id(pond)
  created <- datapond:::create_dataset_folder_structure(
    base_path = pond,
    folder_key = "marker_test",
    uuid = "20260818_abc234",
    parent_dir = "dev",
    data_pond_id = pond_id
  )

  expect_true(all(dir.exists(file.path(
    created$dataset_path,
    c("data", "docs", "scripts")
  ))))
  marker <- datapond:::read_dataset_marker(created$dataset_path)
  expect_identical(marker$dataset_uuid, "20260818_abc234")
  expect_identical(marker$data_pond_id, pond_id)

  dir.create(file.path(created$dataset_path, "arbitrary", "nested"), recursive = TRUE)
  roots <- datapond:::discover_dataset_roots(pond)
  expect_equal(nrow(roots), 1L)
  expect_identical(roots$dataset_path, created$dataset_path)

  insert_test_dataset(pond, "20260818_abc234", created$dataset_path)
  unlink(datapond:::dataset_marker_path(created$dataset_path))
  expect_false(file.exists(datapond:::dataset_marker_path(created$dataset_path)))
  datapond:::backfill_dataset_markers(pond)
  expect_true(file.exists(datapond:::dataset_marker_path(created$dataset_path)))
})


test_that("location repair is recursive but stays within a lifecycle root", {
  pond <- make_test_pond("dataset-repair-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  uuid <- "20260818_def345"
  created <- datapond:::create_dataset_folder_structure(
    pond,
    "repair_test",
    uuid,
    parent_dir = "dev"
  )
  dataset_id <- insert_test_dataset(pond, uuid, created$dataset_path)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO data_file_tbl",
      "(dataset_id, data_file_rel_path_full, file_path)",
      "VALUES (?, 'example.csv', ?)"
    ),
    params = list(dataset_id, file.path(created$data_path, "example.csv"))
  )
  DBI::dbDisconnect(conn)

  nested_parent <- file.path(pond, "dev", "programme", "year")
  dir.create(nested_parent, recursive = TRUE)
  nested_path <- file.path(nested_parent, basename(created$dataset_path))
  expect_true(file.rename(created$dataset_path, nested_path))

  repaired <- datapond:::reconcile_dataset_locations(pond)
  expect_identical(
    repaired[id == dataset_id, location_status],
    "repaired_within_lifecycle_root"
  )
  record <- datapond:::get_dataset_record(dataset_id, pond)
  expect_identical(record$dataset_path, normalizePath(nested_path, winslash = "/"))

  conn <- datapond:::get_conn(pond)
  cached_path <- DBI::dbGetQuery(
    conn,
    "SELECT file_path FROM data_file_tbl WHERE dataset_id = ?",
    params = list(dataset_id)
  )$file_path[[1L]]
  DBI::dbDisconnect(conn)
  expect_identical(cached_path, file.path(record$data_path, "example.csv"))

  arc_path <- file.path(pond, "arc", basename(nested_path))
  expect_true(file.rename(nested_path, arc_path))
  conflict <- datapond:::reconcile_dataset_locations(pond)
  expect_identical(
    conflict[id == dataset_id, location_status],
    "lifecycle_location_conflict"
  )
  expect_true(conflict[id == dataset_id, lifecycle_location_conflict])
  expect_identical(
    datapond:::get_dataset_record(dataset_id, pond)$dataset_path,
    normalizePath(nested_path, mustWork = FALSE, winslash = "/")
  )
})


test_that("duplicate UUIDs and marker conflicts are reported", {
  pond <- make_test_pond("dataset-conflicts-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)
  pond_id <- datapond:::ensure_data_pond_id(pond)

  first <- file.path(pond, "src", "one__20260818_ghi456")
  second <- file.path(pond, "src", "nested", "two__20260818_ghi456")
  dir.create(first, recursive = TRUE)
  dir.create(second, recursive = TRUE)
  datapond:::write_dataset_marker(first, "20260818_ghi456", pond_id)
  datapond:::write_dataset_marker(second, "20260818_ghi456", pond_id)

  conflicting <- file.path(pond, "dev", "bad__20260818_jkm567")
  dir.create(conflicting, recursive = TRUE)
  datapond:::write_dataset_marker(conflicting, "20260818_mnp678", pond_id)

  roots <- datapond:::discover_dataset_roots(pond)
  expect_true(all(
    roots[folder_uuid == "20260818_ghi456", location_status] ==
      "duplicate_uuid"
  ))
  expect_identical(
    roots[dataset_path == normalizePath(conflicting, winslash = "/"),
          location_status],
    "uuid_marker_conflict"
  )
})


test_that("explicit lifecycle moves preserve nested relative folders", {
  pond <- make_test_pond("nested-lifecycle-move-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  uuid <- "20260818_tuv892"
  nested_parent <- file.path(pond, "dev", "programme", "year")
  dir.create(nested_parent, recursive = TRUE)
  created <- datapond:::create_dataset_folder_structure(
    base_path = nested_parent,
    folder_key = "nested_move",
    uuid = uuid,
    parent_dir = ".",
    data_pond_id = datapond:::ensure_data_pond_id(pond)
  )
  dataset_id <- insert_test_dataset(pond, uuid, created$dataset_path)

  expect_true(datapond:::move_dataset_folder(
    dataset_id,
    pond,
    target_stage = "src"
  ))
  expected <- file.path(
    pond,
    "src",
    "programme",
    "year",
    basename(created$dataset_path)
  )
  expect_true(dir.exists(expected))
  expect_identical(
    datapond:::get_dataset_record(dataset_id, pond)$dataset_path,
    normalizePath(expected, winslash = "/")
  )
})


test_that("record visibility and deletion enforce owner and admin permissions", {
  pond <- make_test_pond("dataset-controls-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, paste(
    "INSERT INTO persons_tbl (name_first, name_last) VALUES",
    "('Owner', 'One'), ('Admin', 'Two'), ('Other', 'Three')"
  ))
  DBI::dbExecute(conn, paste(
    "INSERT INTO user_tbl (person_id, user_name, is_admin) VALUES",
    "(1, 'owner', 0), (2, 'admin', 1), (3, 'other', 0)"
  ))
  DBI::dbDisconnect(conn)

  folder <- datapond:::create_dataset_folder_structure(
    pond,
    "controlled",
    "20260818_qrs789",
    parent_dir = "dev"
  )
  dataset_id <- insert_test_dataset(
    pond,
    "20260818_qrs789",
    folder$dataset_path,
    owner_user_id = 1L
  )

  expect_error(
    datapond:::set_dataset_hidden(dataset_id, TRUE, 3L, pond),
    "owner or an administrator"
  )
  datapond:::set_dataset_hidden(dataset_id, TRUE, 1L, pond)
  expect_equal(
    nrow(datapond:::get_datasets(pond, include_hidden = FALSE)),
    0L
  )
  expect_equal(nrow(datapond:::get_datasets(pond, include_hidden = TRUE)), 1L)

  datapond:::soft_delete_dataset_record(dataset_id, 1L, pond)
  permissions <- datapond:::dataset_record_permissions(dataset_id, 1L, pond)
  expect_true(permissions$is_deleted)
  datapond:::restore_dataset_record(dataset_id, 1L, pond)
  expect_false(datapond:::dataset_record_permissions(dataset_id, 1L, pond)$is_deleted)

  datapond:::hard_delete_dataset_record(dataset_id, 2L, pond)
  expect_equal(nrow(datapond:::get_datasets(pond, include_hidden = TRUE)), 0L)
  expect_true(dir.exists(folder$dataset_path))
})


test_that("organisation roles and POC organisation links remain independent", {
  pond <- make_test_pond("dataset-relationships-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, paste(
    "INSERT INTO organisation_tbl",
    "(organisation_name_full, organisation_name_short, organisation_code)",
    "VALUES ('Example Organisation', 'Example Org', 'EX')"
  ))
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title) VALUES ('Relationships')")
  DBI::dbDisconnect(conn)

  datapond:::insert_dataset_org(1L, 1L, "owner", pond)
  datapond:::insert_dataset_org(1L, 1L, "custodian", pond)
  links <- datapond:::get_dataset_org_links(1L, pond)
  expect_setequal(links$role, c("owner", "custodian"))
  expect_error(datapond:::insert_dataset_org(1L, 1L, "owner", pond))

  datapond:::insert_poc(
    dataset_id = 1L,
    person_id = NA_integer_,
    role = "custodian",
    base_path = pond,
    organisation_id = 1L
  )
  poc <- datapond:::get_dataset_poc_links(1L, pond)
  expect_true(is.na(poc$person_id))
  expect_identical(poc$organisation_id, 1L)
  expect_identical(poc$organisation_name_short, "Example Org")
})


test_that("reviewer defaults and pond search do not retain another user", {
  users <- data.frame(id = c(4L, 5L), name = c("One", "Two"))
  expect_identical(datapond:::default_metadata_reviewer_user_id(users, NULL), "")
  expect_identical(datapond:::default_metadata_reviewer_user_id(users, 5L), "5")

  overview <- data.table::data.table(
    id = 1:2,
    display_title = c("Wetland survey", "Forest inventory"),
    authors = c("A Person", "B Person"),
    organisations = c("Water Unit", "Forest Unit"),
    poc = c("Contact A", "Contact B"),
    citation_key = c("wetland2026", "forest2026"),
    pond_location = c("Development (dev/)", "Source (src/)"),
    lifecycle_label = c("Draft", "Draft"),
    metadata_readiness = c("Metadata incomplete", "Required metadata complete"),
    next_action = c("Complete metadata", "Ready to archive"),
    dataset_path = c("/pond/dev/wetland", "/pond/src/forest"),
    record_owner = c("Owner A", "Owner B"),
    record_visibility = "Visible",
    location_status = "ok"
  )
  filtered <- datapond:::filter_pond_overview_data(
    overview,
    "water wetland"
  )
  expect_identical(filtered$id, 1L)
})


test_that("role and data-type guidance is exposed in the UI", {
  expect_identical(
    unname(datapond:::dataset_organisation_role_choices()),
    c("owner", "custodian", "publisher", "distributor")
  )
  expect_match(
    datapond:::dataset_organisation_role_descriptor(),
    "not app permissions",
    fixed = TRUE
  )

  data_files_html <- as.character(datapond:::mod_data_files_ui("files"))
  expect_match(data_files_html, "original data", fixed = TRUE)
  expect_match(data_files_html, "Raw data contains direct", fixed = TRUE)

  poc_html <- as.character(datapond:::mod_poc_ui("poc"))
  expect_match(poc_html, "Point of contact organisation", fixed = TRUE)
})
