lineage_structured_pond <- function() {
  pond <- tempfile("structured-lineage-")
  dir.create(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (uuid, title) VALUES ('lineage-uuid', 'Lineage test')")
  dataset_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbDisconnect(conn)
  list(path = pond, dataset_id = as.integer(dataset_id))
}

test_that("legacy lineage is backfilled once without splitting text", {
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "
    INSERT INTO dataset_lineage_tbl (
      dataset_id, lineage_statement, source_description, source_datasets_text,
      process_description, processing_tools
    ) VALUES (?, 'Summary', 'First source\nSecond source', 'A; B',
      'Cleaned, then joined', 'R; GDAL')
  ", params = list(fixture$dataset_id))
  datapond:::ensure_dataset_lineage_detail_tables(conn)
  datapond:::ensure_dataset_lineage_detail_tables(conn)
  DBI::dbDisconnect(conn)

  sources <- datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)
  steps <- datapond:::get_dataset_lineage_process_steps(fixture$dataset_id, fixture$path)
  expect_equal(nrow(sources), 1L)
  expect_equal(nrow(steps), 1L)
  expect_identical(sources$title, "A; B")
  expect_identical(sources$description, "First source\nSecond source")
  expect_identical(sources$value_origin_code, "legacy_backfill")
  expect_identical(steps$description, "Cleaned, then joined")
})

test_that("structured lineage supports create update order links and delete", {
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  source_one <- datapond:::save_dataset_lineage_source(
    fixture$dataset_id,
    list(title = "Observations", description = "Field records"),
    fixture$path
  )
  source_two <- datapond:::save_dataset_lineage_source(
    fixture$dataset_id,
    list(title = "Boundary", description = "Study boundary"),
    fixture$path
  )
  step <- datapond:::save_dataset_lineage_process_step(
    fixture$dataset_id,
    list(description = "Joined observations to the boundary", role_code = "processor"),
    fixture$path
  )
  datapond:::set_dataset_lineage_step_sources(
    fixture$dataset_id, step, c(source_one, source_two), fixture$path
  )

  datapond:::move_dataset_lineage_record(
    fixture$dataset_id, source_two, "up", "source", fixture$path
  )
  sources <- datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)
  expect_identical(sources$id, c(source_two, source_one))
  expect_identical(sources$source_order, 1:2)
  expect_setequal(
    strsplit(datapond:::get_dataset_lineage_process_steps(
      fixture$dataset_id, fixture$path
    )$source_ids, ",", fixed = TRUE)[[1]],
    as.character(c(source_one, source_two))
  )

  datapond:::save_dataset_lineage_source(
    fixture$dataset_id,
    list(title = "Updated observations"), fixture$path,
    source_id = source_one
  )
  expect_identical(
    datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)$title[2],
    "Updated observations"
  )
  datapond:::delete_dataset_lineage_source(
    fixture$dataset_id, source_two, fixture$path
  )
  expect_equal(nrow(datapond:::get_dataset_lineage_sources(
    fixture$dataset_id, fixture$path
  )), 1L)
  expect_identical(
    datapond:::get_dataset_lineage_process_steps(
      fixture$dataset_id, fixture$path
    )$source_ids,
    as.character(source_one)
  )
})

test_that("completion accepts a summary with a structured source or process step", {
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::save_dataset_lineage(
    fixture$dataset_id,
    list(
      lineage_source_mode = "form", lineage_statement = "Overall history",
      source_description = "", source_datasets_text = "", process_description = "",
      processing_tools = "", processing_scripts = "", output_description = "",
      lineage_notes = "", lineage_status = "reviewed"
    ),
    fixture$path
  )
  datapond:::save_dataset_lineage_source(
    fixture$dataset_id, list(title = "Authoritative source"), fixture$path
  )
  status <- datapond:::get_dataset_completion_status(
    fixture$dataset_id, fixture$path, NULL
  )
  lineage <- as.data.frame(status)[as.data.frame(status)$section == "Lineage", ]
  expect_identical(lineage$status, "complete")
  expect_equal(lineage$saved_records, 2L)
})

test_that("lineage XML emits repeatable sources, steps, and documentation", {
  skip_if_not_installed("xml2")
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::save_dataset_lineage(
    fixture$dataset_id,
    list(
      lineage_statement = "Overall history", lineage_source_mode = "form",
      source_description = "", source_datasets_text = "", process_description = "",
      processing_tools = "", processing_scripts = "", output_description = "",
      lineage_notes = "", lineage_status = "reviewed"
    ), fixture$path
  )
  source_one <- datapond:::save_dataset_lineage_source(
    fixture$dataset_id, list(title = "Source one", description = "First"), fixture$path
  )
  datapond:::save_dataset_lineage_source(
    fixture$dataset_id, list(title = "Source two", description = "Second"), fixture$path
  )
  step <- datapond:::save_dataset_lineage_process_step(
    fixture$dataset_id, list(description = "Combined sources"), fixture$path
  )
  datapond:::set_dataset_lineage_step_sources(
    fixture$dataset_id, step, source_one, fixture$path
  )
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "
    INSERT INTO supporting_document_tbl (
      dataset_id, document_name, document_rel_path, active, include_in_lineage
    ) VALUES (?, 'method.pdf', 'docs/method.pdf', 1, 1)
  ", params = list(fixture$dataset_id))
  DBI::dbDisconnect(conn)

  xml <- datapond:::build_iso_lineage_xml(fixture$dataset_id, fixture$path)
  ns <- xml2::xml_ns(xml)
  expect_length(xml2::xml_find_all(xml, ".//mrl:LI_Source", ns), 2L)
  expect_length(xml2::xml_find_all(xml, ".//mrl:LI_ProcessStep", ns), 1L)
  expect_length(xml2::xml_find_all(xml, ".//mrl:additionalDocumentation", ns), 1L)
})

test_that("lineage UI exposes ordered detailed editors and provenance", {
  html <- as.character(datapond:::mod_lineage_ui("lineage_test"))
  expect_match(html, "Ordered lineage sources", fixed = TRUE)
  expect_match(html, "Ordered process steps", fixed = TRUE)
  expect_match(html, "Start with the inputs", fixed = TRUE)
  expect_match(html, "Value origin", fixed = TRUE)
})

test_that("migration preserves unmatched legacy detail alongside structured rows", {
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  datapond:::save_dataset_lineage_source(fixture$dataset_id, list(title = "New source"), fixture$path)
  datapond:::save_dataset_lineage_process_step(fixture$dataset_id, list(description = "New step"), fixture$path)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl
    (dataset_id, lineage_statement, source_description, source_datasets_text,
     process_description, processing_tools, processing_scripts, output_description, lineage_notes)
    VALUES (?, 'Original summary', 'Old source description', 'A; B',
      'Old process', 'R; GDAL', 'scripts/build.R', 'Old output', 'Extra context')",
    params = list(fixture$dataset_id))
  datapond:::ensure_dataset_lineage_detail_tables(conn)
  datapond:::ensure_dataset_lineage_detail_tables(conn)
  sources <- datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)
  steps <- datapond:::get_dataset_lineage_process_steps(fixture$dataset_id, fixture$path)
  expect_identical(sources$title, c("New source", "A; B"))
  expect_identical(steps$description, c("New step", "Old process"))
  expect_identical(steps$tool_software_reference[2], "R; GDAL")
  expect_identical(steps$script_reference[2], "scripts/build.R")
  expect_identical(steps$output_description[2], "Old output")
  expect_identical(sources$is_provisional[2], 1L)
  saved <- datapond:::get_dataset_lineage(fixture$dataset_id, fixture$path)
  expect_identical(saved$lineage_statement, "Original summary\n\nAdditional lineage notes: Extra context")
  expect_identical(saved$lineage_notes, "Extra context")
  expect_false(is.na(saved$legacy_lineage_migrated_at))
  xml <- datapond:::build_iso_lineage_xml(fixture$dataset_id, fixture$path)
  expect_match(xml2::xml_text(xml2::xml_find_first(xml, ".//mrl:statement", xml2::xml_ns(xml))), "Extra context", fixed = TRUE)
  expect_match(as.character(xml), "Output: Old output", fixed = TRUE)

  # Delete every structured row and edit the narrative. Neither deletion nor
  # removal of the migrated note may be undone on reopening or export.
  for (id in sources$id) datapond:::delete_dataset_lineage_source(fixture$dataset_id, id, fixture$path)
  for (id in steps$id) datapond:::delete_dataset_lineage_process_step(fixture$dataset_id, id, fixture$path)
  datapond:::save_dataset_lineage(fixture$dataset_id,
    list(lineage_statement = "Reviewed summary", lineage_status = "reviewed"), fixture$path)
  datapond:::init_db(fixture$path)
  expect_equal(nrow(datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)), 0L)
  expect_equal(nrow(datapond:::get_dataset_lineage_process_steps(fixture$dataset_id, fixture$path)), 0L)
  expect_identical(datapond:::get_dataset_lineage(fixture$dataset_id, fixture$path)$lineage_statement, "Reviewed summary")
  status <- as.data.frame(datapond:::get_dataset_completion_status(fixture$dataset_id, fixture$path, NULL, "full"))
  expect_identical(status$status[status$section == "Lineage"], "partial")
  xml <- datapond:::build_iso_lineage_xml(fixture$dataset_id, fixture$path)
  expect_false(grepl("Extra context", as.character(xml), fixed = TRUE))
})

test_that("lineage migration recognises earlier backfills and rolls back on failure", {
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl
    (dataset_id, lineage_statement, source_description, process_description)
    VALUES (?, 'Summary', 'Old source', 'Old step')", params = list(fixture$dataset_id))
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_source_tbl
    (dataset_id, description, value_origin_code) VALUES (?, 'Edited migrated source', 'legacy_backfill')",
    params = list(fixture$dataset_id))
  DBI::dbExecute(conn, "CREATE TRIGGER fail_lineage_migration BEFORE UPDATE ON dataset_lineage_tbl
    BEGIN SELECT RAISE(ABORT, 'test migration failure'); END")
  expect_error(datapond:::ensure_dataset_lineage_detail_tables(conn), "test migration failure")
  expect_equal(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM dataset_lineage_process_step_tbl")$n, 0L)
  expect_true(is.na(DBI::dbGetQuery(conn, "SELECT legacy_lineage_migrated_at FROM dataset_lineage_tbl")[[1]]))
  DBI::dbExecute(conn, "DROP TRIGGER fail_lineage_migration")
  datapond:::ensure_dataset_lineage_detail_tables(conn)
  expect_identical(datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)$description,
                   "Edited migrated source")
  expect_equal(nrow(datapond:::get_dataset_lineage_process_steps(fixture$dataset_id, fixture$path)), 1L)
})

test_that("the simplified lineage form has one narrative and notes merge only once", {
  html <- as.character(datapond:::mod_lineage_ui("lineage"))
  expect_false(grepl("Previous lineage text", html, fixed = TRUE))
  expect_false(grepl("lineage-lineage_notes", html, fixed = TRUE))
  expect_match(html, "lineage-lineage_statement", fixed = TRUE)
  expect_identical(datapond:::merge_lineage_narrative("Same", "Same"), "Same")
  expect_identical(datapond:::merge_lineage_narrative(NA_character_, "Notes only"), "Notes only")
  expect_identical(datapond:::merge_lineage_narrative("Summary\n\nAdditional lineage notes: Note", "Note"),
                   "Summary\n\nAdditional lineage notes: Note")
})

test_that("blank value origins use defaults for new rows and preserve existing provenance", {
  fixture <- lineage_structured_pond()
  on.exit(unlink(fixture$path, recursive = TRUE), add = TRUE)
  source <- datapond:::save_dataset_lineage_source(fixture$dataset_id,
    list(title = "Survey", value_origin_code = ""), fixture$path)
  step <- datapond:::save_dataset_lineage_process_step(fixture$dataset_id,
    list(description = "Cleaned observations", value_origin_code = ""), fixture$path)
  expect_identical(datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)$value_origin_code, "user")
  expect_identical(datapond:::get_dataset_lineage_process_steps(fixture$dataset_id, fixture$path)$value_origin_code, "user")
  for (origin in c("extracted_metadata", "supporting_document", "legacy_backfill")) {
    datapond:::save_dataset_lineage_source(fixture$dataset_id,
      list(title = "Survey", value_origin_code = origin), fixture$path, source_id = source)
    datapond:::save_dataset_lineage_process_step(fixture$dataset_id,
      list(description = "Cleaned observations", value_origin_code = origin), fixture$path, process_step_id = step)
    datapond:::save_dataset_lineage_source(fixture$dataset_id,
      list(title = "Updated survey", value_origin_code = ""), fixture$path, source_id = source)
    datapond:::save_dataset_lineage_process_step(fixture$dataset_id,
      list(description = "Updated method", value_origin_code = ""), fixture$path, process_step_id = step)
    expect_identical(datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)$value_origin_code, origin)
    expect_identical(datapond:::get_dataset_lineage_process_steps(fixture$dataset_id, fixture$path)$value_origin_code, origin)
  }
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "INSERT INTO supporting_document_tbl
    (dataset_id, document_name, document_rel_path, active) VALUES (?, 'Methods', 'docs/methods.pdf', 1)",
    params = list(fixture$dataset_id))
  document_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbDisconnect(conn)
  linked <- datapond:::save_dataset_lineage_source(fixture$dataset_id,
    list(supporting_document_id = document_id, value_origin_code = ""), fixture$path)
  rows <- datapond:::get_dataset_lineage_sources(fixture$dataset_id, fixture$path)
  expect_identical(rows$value_origin_code[rows$id == linked], "supporting_document")
  expect_error(datapond:::save_dataset_lineage_source(fixture$dataset_id,
    list(value_origin_code = ""), fixture$path), "Provide a source title")
  expect_error(datapond:::save_dataset_lineage_process_step(fixture$dataset_id,
    list(value_origin_code = ""), fixture$path), "description is required")
})
