test_that("population assessments use canonical dates and separate resource progress", {
  local_trusted_access_fixture()

  pond <- tempfile("population-profiles-")
  dir.create(pond)
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  datapond:::ensure_columns(conn, "entry_tbl", c("abstract", "purpose", "keyword_object", "data_complete_status", "version"))
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title, abstract, keyword_object)
    VALUES ('Observations', 'Observed species at survey sites.', 'biota')")
  dataset_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbDisconnect(conn)
  schema <- datapond:::dataset_overview_schema()
  # Resource progress is no longer a generated control or a required field.
  progress <- data.table::data.table(field = "data_complete_status", obligation = "M")
  expect_equal(nrow(datapond:::metadata_population_schema(progress, "full")), 0L)
  datapond:::replace_dataset_citation_dates(dataset_id, data.frame(
    date_value = "2024-01-01", date_type_code = "publication", date_precision = "day"
  ), pond)
  # Deliberately stale compatibility value must not create another missing date.
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET citation_date = NULL")
  DBI::dbDisconnect(conn)
  datapond:::save_dataset_lineage(dataset_id, list(
    lineage_source_mode = "form", lineage_statement = "Collected in field surveys.",
    source_description = "", source_datasets_text = "", process_description = "",
    processing_tools = "", processing_scripts = "", output_description = "",
    lineage_notes = "", lineage_status = "draft"
  ), pond)

  assess <- function(profile) as.data.frame(datapond:::get_dataset_completion_status(
    dataset_id, pond, schema, population_profile = profile
  ))
  rough <- assess("rough")
  full <- assess("full")
  status <- function(x, name) x$status[x$section == name]
  expect_identical(status(rough, "Citation"), "partial")
  expect_equal(rough$required_missing[rough$section == "Citation"], 1L) # version is required in both modes
  expect_identical(status(rough, "About"), "complete")
  expect_identical(status(rough, "Lineage"), "complete")
  expect_identical(status(full, "Lineage"), "partial")
  expect_identical(status(full, "Citation dates"), "complete")
  expect_equal(full$required_missing[full$section == "Citation"], 1L) # version only
  expect_equal(full$required_missing[full$section == "About"], 1L) # purpose only
  expect_identical(status(rough, "Licensing and sharing"), status(full, "Licensing and sharing"))
  expect_identical(status(rough, "Geographic discovery"), status(full, "Geographic discovery"))
  expect_identical(datapond:::dataset_status_requirement_level(
    c("Geographic discovery", "Licensing and sharing", "Citation dates"), "rough"
  ), c("optional", "essential", "essential"))
  # Assessment does not invent missing metadata or alter a stored record.
  record <- datapond:::get_dataset_record(dataset_id, pond)
  expect_true(is.na(record$citation_date[1]))
  expect_identical(datapond:::validate_iso_profile_export(dataset_id, pond)$valid, FALSE)

  selected_profile <- shiny::reactiveVal("rough")
  shiny::testServer(datapond:::mod_dataset_status_server, args = list(
    state = shiny::reactiveValues(current_dataset_id = dataset_id, data_pond = pond),
    schema_grouped = schema, population_profile = selected_profile
  ), {
    selected_profile("rough")
    session$flushReact()
    status <- as.data.frame(normalised_status_data())
    expect_identical(status$status[status$section == "Lineage"], "complete")
    expect_identical(status$requirement_level[status$section == "Citation dates"], "essential")
    selected_profile("full")
    session$flushReact()
    status <- as.data.frame(normalised_status_data())
    expect_identical(status$status[status$section == "Lineage"], "partial")
    expect_identical(status$requirement_level[status$section == "Citation dates"], "essential")
  })
})

test_that("summary edits preserve original legacy lineage in storage", {
  expect_identical(datapond:::lineage_summary_edit_values(list(), NULL)$source_description, "")
  pond <- tempfile("lineage-preservation-")
  dir.create(pond)
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title) VALUES ('Legacy lineage')")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl
    (dataset_id, lineage_statement, source_description, processing_tools)
    VALUES (1, 'Original summary', 'Original source text', 'Original tool')")
  DBI::dbDisconnect(conn)
  saved <- datapond:::get_dataset_lineage(1L, pond)
  values <- datapond:::lineage_summary_edit_values(list(
    lineage_statement = "Updated summary", lineage_source_mode = "form",
    lineage_status = "draft", lineage_notes = "Notes",
    source_description = "Browser overwrite"
  ), saved)
  datapond:::save_dataset_lineage(1L, values, pond)
  after <- datapond:::get_dataset_lineage(1L, pond)
  expect_identical(after$lineage_statement, "Updated summary")
  for (field in c("source_description", "source_datasets_text", "process_description",
                  "processing_tools", "processing_scripts", "output_description")) {
    expect_identical(after[[field]], saved[[field]])
  }
})

test_that("rough inventory still requires a document when document authority is selected", {
  pond <- tempfile("population-document-")
  dir.create(pond)
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title) VALUES ('Document authority')")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl
    (dataset_id, lineage_source_mode, lineage_statement)
    VALUES (1, 'supporting_documents', 'See the source report.')")
  DBI::dbDisconnect(conn)
  assessment <- as.data.frame(datapond:::get_dataset_completion_status(1L, pond, NULL, "rough"))
  expect_identical(assessment$status[assessment$section == "Lineage"], "partial")
})

test_that("legacy lineage has no second editable source or process form", {
  html <- xml2::read_html(as.character(datapond:::mod_lineage_ui("lineage")))
  for (field in c("source_description", "source_datasets_text", "process_description",
                  "processing_tools", "processing_scripts", "output_description")) {
    input <- xml2::xml_find_first(html, paste0("//*[@id='lineage-", field, "']"))
    expect_true(inherits(input, "xml_missing"))
  }
  expect_true(is.na(xml2::xml_attr(xml2::xml_find_first(html,
    "//*[@id='lineage-lineage_statement']"), "readonly")))
})
