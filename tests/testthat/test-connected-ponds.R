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

test_that("one offline pond does not block the other catalogues or lose its registration", {
  row <- function(title, uuid) list(list(
    uuid = uuid, title = title, citation_key = uuid, folder_key = uuid, stage = "dev"
  ))
  first <- connected_test_pond("available-connected-", row("Still available", "available-uuid"))
  second <- connected_test_pond("offline-connected-", row("Returns later", "offline-uuid"))
  offline <- paste0(second, "-disconnected")
  registry_file <- tempfile("mixed-registry-", fileext = ".rds")
  on.exit(unlink(c(first, second, offline, registry_file), recursive = TRUE), add = TRUE)
  datapond:::register_connected_pond(first, "First", registry_file)
  registry <- datapond:::register_connected_pond(second, "Second", registry_file)
  cache <- new.env(parent = emptyenv())
  before <- tools::md5sum(file.path(first, "metadata.db"))
  initial <- datapond:::build_connected_pond_catalogue(registry, cache, active_pond_path = first)
  expect_setequal(initial$title, c("Still available", "Returns later"))
  expect_true(file.rename(second, offline))

  expect_no_warning(remaining <- datapond:::build_connected_pond_catalogue(
    registry, cache, active_pond_path = first
  ))
  expect_identical(remaining$title, "Still available")
  expect_identical(datapond:::read_connected_pond_registry(registry_file), registry)
  expect_false(dir.exists(second))
  expect_identical(tools::md5sum(file.path(first, "metadata.db")), before)

  expect_true(file.rename(offline, second))
  restored <- datapond:::build_connected_pond_catalogue(registry, cache, active_pond_path = first)
  expect_setequal(restored$title, c("Still available", "Returns later"))
  expect_identical(datapond:::connected_pond_dataset_rows(restored)$title, "Returns later")
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
  expect_match(overview_html, 'id="dataset_table"', fixed = TRUE)
  expect_false(grepl('id="overview_connected_table"', overview_html, fixed = TRUE))
  expect_match(overview_html, "Open selected pond", fixed = TRUE)
  expect_match(links_html, "connected_dataset_table", fixed = TRUE)
  expect_match(links_html, "switch_to_selected_connected_pond", fixed = TRUE)
  expect_match(links_html, "read-only summaries", fixed = TRUE)
  expect_match(links_html, "Pond links", fixed = TRUE)
  expect_match(config_html, "connected_pond_input", fixed = TRUE)
  expect_match(config_html, 'id="connected_pond_select"', fixed = TRUE)
  expect_false(grepl('id="connected_ponds_table"', config_html, fixed = TRUE))
  expect_match(config_html, "nickname / repository name", ignore.case = TRUE)
  expect_match(config_html, "Switch to selected pond", fixed = TRUE)
})

connected_overview_remote_fixture <- function(root) {
  source <- connected_test_pond("overview-remote-source-", list(list(
    uuid = "remote-overview-uuid", title = "Remote soils", citation_key = "remote_soils",
    folder_key = "soils", stage = "arc"
  )))
  on.exit(unlink(source, recursive = TRUE))
  conn <- get_conn(source)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = 'Soil observations from the remote pond.'")
  DBI::dbDisconnect(conn)
  cache <- file.path(root, "remote-cache")
  snapshot <- movement_snapshot(source, file.path(cache, "catalogue-overview"))
  movement_cache_commit(list(cache = cache, directory = snapshot$directory, remote = "team:pond"), snapshot$manifest)
  list(name = "Team pond", cache = cache, remote = "team:pond")
}

test_that("overview combines local connections with offline remote metadata without writing catalogues", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  saved <- connected_overview_remote_fixture(root)
  local <- connected_test_pond("overview-local-", list(list(
    uuid = "local-overview-uuid", title = "Local rivers", citation_key = "rivers",
    folder_key = "rivers", stage = "src"
  )))
  on.exit(unlink(local, recursive = TRUE), add = TRUE)
  rows <- read_connected_pond_inventory(local)
  rows[, pond_name := "Local pond"]
  database <- get_db_path(movement_cache_current(saved$cache)$directory)
  before <- tools::md5sum(database)
  inventory <- build_connected_overview_inventory(rows, list(saved))
  expect_setequal(inventory$data$title, c("Local rivers", "Remote soils"))
  expect_length(inventory$unavailable, 0L)
  remote <- inventory$data[connection_type == "Remote cache (read-only)"]
  expect_equal(remote$stage, "arc")
  expect_equal(remote$dataset_path, "team:pond/arc/soils")
  expect_equal(remote$open_path, saved$cache)
  expect_equal(remote$availability, "Metadata only")
  expect_equal(read_connected_overview_abstract(as.list(remote)), "Soil observations from the remote pond.")
  stale <- as.list(remote); stale$uuid <- "different-record"
  expect_error(read_connected_overview_abstract(stale), "record has changed")
  expect_true(nzchar(remote$refreshed))
  expect_identical(tools::md5sum(database), before)
  expect_error(get_conn(movement_cache_current(saved$cache)$directory), "read-only")
  expect_equal(filter_pond_overview_data(inventory$data, "Team soils")$title, "Remote soils")
  expect_false(dir.exists(file.path(saved$cache, "datasets")))

  receipt <- file.path(saved$cache, "receipts", "arc_soils.json")
  movement_json_write(list(path = "arc/soils"), receipt)
  updated <- build_connected_overview_inventory(rows, list(saved))
  expect_match(updated$data[connection_type == "Remote cache (read-only)", availability], "Downloaded")
  missing <- list(name = "Disconnected drive", cache = file.path(root, "absent"), remote = "other:pond")
  mixed <- build_connected_overview_inventory(rows, list(saved, missing))
  expect_equal(nrow(mixed$data), 2L)
  expect_match(mixed$unavailable, "Disconnected drive")
  expect_false(dir.exists(missing$cache))
})

test_that("main overview lists remote caches and opens them in Flow without switching the active pond", {
  local_trusted_access_fixture()

  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  saved <- connected_overview_remote_fixture(root)
  settings <- file.path(root, "app-config"); dir.create(settings)
  saveRDS(list(saved), file.path(settings, "remote-pond-caches.rds"))
  active <- connected_test_pond("overview-active-", list(list(uuid = "current-uuid", title = "Current rivers",
    citation_key = "current_rivers", folder_key = "rivers", stage = "src")))
  on.exit(unlink(active, recursive = TRUE), add = TRUE)
  withr::local_options(list(datapond.data_pond = active))
  reads <- new.env(); reads$remote <- 0L; reads$local <- 0L
  remote_reader <- movement_cache_inventory
  local_reader <- read_connected_pond_inventory
  testthat::local_mocked_bindings(
    app_state_dir = function() settings,
    movement_remotes = function() data.frame(name = character(), type = character()),
    movement_cache_inventory = function(...) { reads$remote <- reads$remote + 1L; remote_reader(...) },
    read_connected_pond_inventory = function(...) { reads$local <- reads$local + 1L; local_reader(...) }
  )
  shiny::testServer(server, {
    html <- xml2::read_html(as.character(ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$overview_current_only <- FALSE
    do.call(session$setInputs, defaults)
    rows <- overview_inventory_data()
    expect_setequal(rows$title, c("Current rivers", "Remote soils"))
    # Capture any catalogue writes made by the initial workspace observers.
    auth$tick <- auth$tick + 1L
    session$flushReact()
    updates <- new.env(); updates$overview <- 0L; updates$connections <- 0L
    overview_watcher <- shiny::observe({
      overview_inventory_data()
      updates$overview <- updates$overview + 1L
    })
    connection_watcher <- shiny::observe({
      connected_pond_registry_display()
      updates$connections <- updates$connections + 1L
    })
    session$flushReact()
    before <- c(updates$overview, updates$connections)
    for (i in 1:3) {
      auth$tick <- auth$tick + 1L
      session$flushReact()
    }
    expect_identical(c(updates$overview, updates$connections), before)
    overview_watcher$destroy()
    connection_watcher$destroy()
    expect_equal(rows[active_pond == TRUE, title], "Current rivers")
    expect_true(is.na(rows[active_pond == FALSE, id]))
    display <- pond_overview_inventory_display(rows)
    expect_match(display$Pond[1], "\u2605 Current", fixed = TRUE)
    active_before <- state$data_pond
    dataset_before <- state$current_dataset_id
    session$setInputs(dataset_table_rows_selected = which(!rows$active_pond))
    expect_equal(state$current_dataset_id, dataset_before)
    expect_equal(overview_selected_abstract()$abstract, "Soil observations from the remote pond.")
    expect_match(output$overview_dataset_abstract$html, "Soil observations from the remote pond.", fixed = TRUE)
    session$setInputs(overview_open_connected = 1L)
    expect_equal(overview_remote_pond_request()$cache, saved$cache)
    expect_equal(movement_state$cache, saved$cache)
    expect_null(movement_state$job)
    expect_equal(state$data_pond, active_before)
    expect_false(dir.exists(file.path(saved$cache, "datasets")))
    remote_reads <- reads$remote; local_reads <- reads$local
    session$setInputs(pond_dataset_search = "absent title")
    session$elapse(300)
    expect_equal(nrow(overview_inventory_data()), 0L)
    session$setInputs(pond_dataset_search = "Team pond")
    session$elapse(300)
    expect_equal(nrow(overview_inventory_data()), 1L)
    expect_null(overview_selected_connection())
    expect_equal(reads$remote, remote_reads)
    expect_equal(reads$local, local_reads)
    state$refresh_datasets <- state$refresh_datasets + 1L
    session$flushReact()
    expect_equal(reads$remote, remote_reads) # Local edits must not reopen remote snapshots.
    session$setInputs(pond_dataset_search = "", overview_current_only = TRUE)
    session$elapse(300)
    expect_equal(overview_inventory_data()$title, "Current rivers")
    expect_true(all(overview_inventory_data()$active_pond))
    session$setInputs(overview_open_connected = 2L)
    expect_equal(state$data_pond, active_before)
  })
})

test_that("overview previews leave editors and full map rendering untouched until opened", {
  local_trusted_access_fixture()

  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  active <- connected_test_pond("preview-active-", lapply(1:2, function(i) list(
    uuid = paste0("preview-", i), title = paste("Preview", i), citation_key = paste0("preview", i),
    folder_key = paste0("preview", i), stage = "src")))
  on.exit(unlink(active, recursive = TRUE), add = TRUE)
  conn <- get_conn(active)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = ? WHERE id = 1", params = list("First abstract.\n<script>plain text</script>"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = 'Second abstract.' WHERE id = 2")
  DBI::dbDisconnect(conn)
  withr::local_options(datapond.data_pond = active)
  counts <- new.env(); counts$hydration <- 0L; counts$inventory <- 0L; counts$map <- 0L
  hydrate <- load_selected_dataset_into_state
  inventory <- build_pond_overview_data
  basemap <- add_datapond_basemap
  testthat::local_mocked_bindings(
    app_state_dir = function() root,
    movement_remotes = function() data.frame(name = character(), type = character()),
    load_selected_dataset_into_state = function(...) { counts$hydration <- counts$hydration + 1L; hydrate(...) },
    build_pond_overview_data = function(...) {
      counts$inventory <- counts$inventory + 1L
      rows <- inventory(...)
      rows[, `:=`(bbox_available = TRUE, west = 20, east = 21, south = -30, north = -29)]
      rows
    },
    add_datapond_basemap = function(...) { counts$map <- counts$map + 1L; basemap(...) }
  )
  shiny::testServer(server, {
    html <- xml2::read_html(as.character(ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$main_dataset_tabs <- "pond_overview"
    do.call(session$setInputs, defaults)
    rows <- overview_inventory_data()
    output$pond_dataset_map
    builds <- counts$inventory; maps <- counts$map
    ticks <- c(state$refresh_attributes, state$refresh_data_files_saved, state$refresh_lineage)
    session$setInputs(dataset_table_rows_selected = which(rows$id == 1L))
    expect_equal(overview_current_id(), 1L)
    expect_equal(overview_selected_abstract()$abstract, "First abstract.\n<script>plain text</script>")
    expect_match(output$overview_dataset_abstract$html, "&lt;script&gt;plain text&lt;/script&gt;", fixed = TRUE)
    session$setInputs(pond_dataset_map_marker_click = list(id = "2"))
    expect_equal(overview_current_id(), 2L)
    expect_equal(overview_selected_abstract()$abstract, "Second abstract.")
    expect_null(state$current_dataset_id)
    expect_null(last_hydrated_dataset_id())
    expect_equal(counts$hydration, 0L)
    expect_equal(c(state$refresh_attributes, state$refresh_data_files_saved, state$refresh_lineage), ticks)
    output$pond_dataset_map
    expect_equal(counts$inventory, builds)
    expect_equal(counts$map, maps)
    expect_identical(output$overview_editor_ready, "false")

    session$setInputs(overview_open_editor = 1L)
    expect_equal(state$current_dataset_id, 2L)
    expect_equal(last_hydrated_dataset_id(), 2L)
    expect_equal(counts$hydration, 1L)
    expect_identical(output$overview_editor_ready, "true")
    # Browser acknowledgement of the selected id must not hydrate twice.
    session$setInputs(dataset_select = "2", title = "Unsaved editor text")
    expect_equal(counts$hydration, 1L)
    session$setInputs(dataset_table_rows_selected = which(overview_inventory_data()$id == 1L))
    expect_equal(state$current_dataset_id, 2L)
    expect_equal(overview_current_id(), 1L)
    expect_false(select_overview_table_rows(which(overview_inventory_data()$id == 1L)))
    expect_equal(input$title, "Unsaved editor text")
    expect_equal(counts$hydration, 1L)
    expect_identical(output$overview_editor_ready, "false")
    expect_false(is.null(output$dataset_record_controls))

    # Delayed editor updates must not reclaim an explicit Overview selection.
    state$current_dataset_id <- 1L
    session$flushReact()
    state$current_dataset_id <- 2L
    session$flushReact()
    expect_equal(overview_current_id(), 1L)

    session$setInputs(main_dataset_tabs = "dataset_workspace")
    expect_equal(state$current_dataset_id, 1L)
    expect_equal(counts$hydration, 2L)
    conn <- get_conn(active)
    DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = NULL WHERE id = 1")
    DBI::dbDisconnect(conn)
    session$setInputs(refresh_pond_overview = 1L)
    expect_match(output$overview_dataset_abstract$html, "Select a dataset", fixed = TRUE)
    expect_null(selected_dataset_citation_record())
    expect_null(overview_record_id())
    session$setInputs(dataset_table_rows_selected = which(overview_inventory_data()$id == 1L))
    expect_match(output$overview_dataset_abstract$html, "No abstract recorded.", fixed = TRUE)
  })
})

test_that("connected catalogue cache notices commits in the SQLite WAL", {
  pond <- connected_test_pond("wal-connected-", list(list(uuid = "wal", title = "Before",
    citation_key = "wal", folder_key = "wal", stage = "src")))
  on.exit(unlink(pond, recursive = TRUE))
  conn <- get_conn(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "PRAGMA journal_mode=WAL")
  cache <- new.env(parent = emptyenv())
  expect_equal(read_connected_pond_inventory_cached(pond, cache)$title, "Before")
  before <- connected_pond_database_signature(pond)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET title = 'After'")
  expect_false(identical(connected_pond_database_signature(pond), before))
  expect_equal(read_connected_pond_inventory_cached(pond, cache)$title, "After")
})
