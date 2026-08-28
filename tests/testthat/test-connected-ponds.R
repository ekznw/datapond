connected_test_pond <- function(prefix, rows = list()) {
  pond <- tempfile(prefix)
  dir.create(pond, recursive = TRUE)
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  if (length(rows) > 0L) {
    conn <- datapond:::get_conn(pond)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    for (row in rows) {
      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO entry_tbl",
          "(uuid, title, citation_key, folder_key, dataset_path, lifecycle_status, data_pond_id)",
          "VALUES (?, ?, ?, ?, ?, 'draft', ?)"
        ),
        params = list(
          row$uuid,
          row$title,
          row$citation_key,
          row$folder_key,
          file.path(pond, row$stage, row$folder_key),
          datapond:::ensure_data_pond_id(pond)
        )
      )
    }
  }
  pond
}


test_that("connected pond registry stores names without modifying pond catalogues", {
  first <- connected_test_pond("connected-first-")
  second <- connected_test_pond("connected-second-")
  registry_file <- tempfile("connected-registry-", fileext = ".rds")
  on.exit(unlink(c(first, second, registry_file), recursive = TRUE, force = TRUE), add = TRUE)

  first_db <- file.path(first, "metadata.db")
  datapond:::set_data_pond_label(first, "Pond-owned repository name")
  expect_identical(
    datapond:::read_data_pond_identity_read_only(first)$pond_label,
    "Pond-owned repository name"
  )
  before_checksum <- unname(tools::md5sum(first_db))

  registry <- datapond:::register_connected_pond(
    first,
    "Primary repository",
    registry_file
  )
  registry <- datapond:::register_connected_pond(
    second,
    "Colleague pond",
    registry_file
  )
  expect_equal(nrow(registry), 2L)
  expect_setequal(registry$pond_name, c("Primary repository", "Colleague pond"))
  expect_identical(unname(tools::md5sum(first_db)), before_checksum)

  first_id <- datapond:::read_data_pond_identity_read_only(first)$data_pond_id
  renamed <- datapond:::rename_connected_pond(
    first_id,
    "Central metadata repo",
    registry_file
  )
  expect_identical(
    renamed$pond_name[renamed$data_pond_id == first_id],
    "Central metadata repo"
  )

  second_id <- datapond:::read_data_pond_identity_read_only(second)$data_pond_id
  remaining <- datapond:::remove_connected_pond(second_id, registry_file)
  expect_identical(remaining$data_pond_id, first_id)
})


test_that("connected inventories are lightweight, cached and read-only", {
  pond <- connected_test_pond(
    "connected-inventory-",
    rows = list(
      list(
        uuid = "connected-uuid-1",
        title = "Connected dataset",
        citation_key = "connected_key",
        folder_key = "connected_folder",
        stage = "src"
      )
    )
  )
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  cache <- new.env(parent = emptyenv())
  database <- file.path(pond, "metadata.db")
  before_checksum <- unname(tools::md5sum(database))
  first <- datapond:::read_connected_pond_inventory_cached(pond, cache)
  second <- datapond:::read_connected_pond_inventory_cached(pond, cache)

  expect_equal(first, second)
  expect_identical(first$title, "Connected dataset")
  expect_identical(first$stage, "src")
  expect_identical(unname(tools::md5sum(database)), before_checksum)
  expect_equal(length(ls(cache)), 1L)
})


test_that("connected dataset rows exclude the active pond without data.table NSE", {
  catalogue <- data.table::data.table(
    id = 1:4,
    active_pond = c(TRUE, FALSE, NA, FALSE),
    title = c("Active", "Connected one", "Unknown", "Connected two")
  )

  connected <- datapond:::connected_pond_dataset_rows(catalogue)

  expect_identical(connected$id, c(2L, 4L))
  expect_identical(connected$title, c("Connected one", "Connected two"))
  expect_identical(catalogue$id, 1:4)
})


test_that("cross-pond duplicates tolerate dev but flag src and arc", {
  catalogue <- data.table::data.table(
    id = 1:6,
    data_pond_id = c("pond-a", "pond-b", "pond-a", "pond-b", "pond-a", "pond-b"),
    pond_name = c("A", "B", "A", "B", "A", "B"),
    uuid = c("dev-copy", "dev-copy", "protected-copy", "protected-copy", NA, NA),
    citation_key = c("dev", "dev", "protected", "protected", "fallback", "fallback"),
    folder_key = c("dev", "dev", "protected", "protected", "fallback", "fallback"),
    title = c("Working", "Working", "Protected", "Protected", "Fallback", "Fallback"),
    stage = c("dev", "dev", "src", "arc", "dev", "src")
  )

  duplicates <- datapond:::cross_pond_duplicate_datasets(catalogue)
  expect_equal(nrow(duplicates), 3L)
  expect_false(duplicates[title == "Working", needs_resolution])
  expect_true(duplicates[title == "Protected", needs_resolution])
  expect_true(duplicates[title == "Fallback", needs_resolution])
  expect_identical(
    duplicates[title == "Working", status],
    "Tolerated duplicate development copy"
  )
})


test_that("connected pond UI is explicitly read-only and switch based", {
  overview_html <- as.character(datapond:::pond_overview_ui())
  links_html <- as.character(datapond:::dataset_transfer_ui())
  config_html <- as.character(datapond:::config_panel_ui())
  expect_false(grepl("connected_dataset_table", overview_html, fixed = TRUE))
  expect_match(links_html, "connected_dataset_table", fixed = TRUE)
  expect_match(links_html, "switch_to_selected_connected_pond", fixed = TRUE)
  expect_match(links_html, "read-only summaries", fixed = TRUE)
  expect_match(links_html, "Pond links", fixed = TRUE)
  expect_match(config_html, "connected_pond_input", fixed = TRUE)
  expect_match(config_html, "nickname / repository name", ignore.case = TRUE)
  expect_match(config_html, "Switch to selected pond", fixed = TRUE)
})
