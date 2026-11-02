test_that("opening rejects non-pond folders without changing them", {
  folder <- tempfile("not-a-pond-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  expect_error(datapond:::validate_existing_data_pond(folder), "Create new pond")
  expect_length(list.files(folder, all.files = TRUE, no.. = TRUE), 0L)
  missing <- file.path(folder, "missing")
  expect_error(datapond:::validate_existing_data_pond(missing), "Create new pond")
  expect_false(dir.exists(missing))
  expect_error(datapond:::validate_existing_data_pond(" "), "Select an existing")

  database <- file.path(folder, "metadata.db")
  writeLines("not a SQLite database", database)
  before <- tools::md5sum(database)
  expect_error(datapond:::validate_existing_data_pond(folder), "valid Data Pond")
  expect_identical(tools::md5sum(database), before)
})

test_that("older catalogues can be opened without an existing pond identifier", {
  folder <- tempfile("legacy-pond-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  database <- file.path(folder, "metadata.db")
  conn <- DBI::dbConnect(RSQLite::SQLite(), database)
  DBI::dbExecute(conn, "CREATE TABLE entry_tbl (id INTEGER PRIMARY KEY, title TEXT)")
  DBI::dbDisconnect(conn)
  before <- tools::md5sum(database)
  expect_identical(datapond:::validate_existing_data_pond(folder), folder)
  expect_identical(tools::md5sum(database), before)
  expect_false(dir.exists(file.path(folder, ".metadata_pond")))
})

test_that("creation uses a new child folder and protects existing destinations", {
  parent <- tempfile("pond-parent-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  for (name in c("", "..", ".", "../escape", "a/b", "a\\b", "a:b", "CON", "LPT1.txt", "ends.")) {
    expect_error(datapond:::new_data_pond_path(parent, name))
  }
  expect_error(datapond:::new_data_pond_path(file.path(parent, "missing"), "Pond"), "existing parent")
  expected <- file.path(parent, "Wetland surveys")
  expect_identical(datapond:::new_data_pond_path(parent, " Wetland surveys "), expected)
  expect_false(dir.exists(expected)) # Preview never creates the destination.
  expect_identical(datapond:::create_data_pond_folder(parent, "Wetland surveys"), expected)
  writeLines("keep", file.path(expected, "important.txt"))
  expect_error(datapond:::create_data_pond_folder(parent, "Wetland surveys"), "already exists")
  expect_identical(readLines(file.path(expected, "important.txt")), "keep")
  file.create(file.path(parent, "existing-file"))
  expect_error(datapond:::create_data_pond_folder(parent, "existing-file"), "already exists")
})

test_that("configuration exposes both pond actions and creation explains its destination", {
  ui <- xml2::read_html(as.character(datapond:::config_panel_ui()))
  for (id in c("data_pond_input", "new_pond")) {
    button <- xml2::xml_find_first(ui, paste0("//*[@id='", id, "']"))
    expect_false(is.na(xml2::xml_name(button)))
    management <- xml2::xml_find_all(
      button,
      "ancestor::details[contains(concat(' ', normalize-space(@class), ' '), ' pond-management ')]"
    )
    expect_length(management, 1L)
  }
  expect_match(xml2::xml_text(ui), "Pond management", fixed = TRUE)
  expect_match(xml2::xml_text(ui), "Open or create a data pond", fixed = TRUE)
  dialog <- as.character(datapond:::new_data_pond_dialog("/tmp"))
  expect_match(dialog, "Create and open pond", fixed = TRUE)
  expect_match(dialog, 'id="new_pond_destination"', fixed = TRUE)
  expect_match(dialog, "Next, set up a pond administrator and sign in.", fixed = TRUE)
})

test_that("create and open actions switch ponds without copying or losing records", {
  local_trusted_access_fixture()

  parent <- tempfile("pond-flow-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  original <- file.path(parent, "Original")
  old_options <- options(datapond.data_pond = original)
  on.exit(options(old_options), add = TRUE)
  testthat::local_mocked_bindings(
    app_state_dir = function() file.path(parent, "app-config"), .package = "datapond"
  )
  shiny::testServer(datapond:::server, {
    # Supply the dropdown defaults normally sent by the browser.
    html <- xml2::read_html(as.character(datapond:::ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$create_pond <- 0L
    defaults$set_manual <- 0L
    do.call(session$setInputs, defaults)
    original_id <- state$data_pond_id
    conn <- datapond:::get_conn(original)
    dataset_path <- file.path(original, "dev", "original_dataset")
    dir.create(dataset_path)
    DBI::dbExecute(conn, paste(
      "INSERT INTO entry_tbl (uuid, title, citation_key, folder_key, dataset_path, lifecycle_status, data_pond_id)",
      "VALUES ('original-dataset-uuid', 'Keep original dataset', 'original_key', 'original_dataset', ?, 'draft', ?)"
    ), params = list(dataset_path, original_id))
    DBI::dbDisconnect(conn)
    session$setInputs(new_pond_parent = parent, new_pond_name = "New survey pond", create_pond = 1L)
    created <- file.path(parent, "New survey pond")
    expect_identical(state$data_pond, created)
    expect_false(identical(state$data_pond_id, original_id))
    expect_null(state$active_user_id)
    expect_true(all(dir.exists(file.path(created, c("src", "dev", "arc", ".metadata_pond")))))
    expect_true(all(file.exists(file.path(created, c("metadata.db", ".metadata_pond/data_pond_id", ".metadata_pond/config.rds")))))
    expect_equal(nrow(datapond:::get_datasets(created)), 0L)
    expect_gt(length(datapond:::get_keywords("keyword_topic", created)), 0L)
    expect_identical(datapond:::read_last_data_pond_path(), created)
    created_id <- state$data_pond_id
    session$setInputs(create_pond = 2L)
    expect_identical(state$data_pond_id, created_id)
    session$setInputs(manual_path = parent, set_manual = 1L)
    expect_identical(state$data_pond, created)
    expect_false(file.exists(file.path(parent, "metadata.db")))
    session$setInputs(manual_path = original, set_manual = 2L)
    expect_identical(state$data_pond, original)
    expect_identical(state$data_pond_id, original_id)
    expect_identical(datapond:::get_datasets(original)$title, "Keep original dataset")
  })
})
