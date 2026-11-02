lineage_completion_row <- function(pond, dataset_id) {
  status <- datapond:::get_dataset_completion_status(
    dataset_id = dataset_id,
    base_path = pond,
    schema_grouped = NULL
  )

  as.data.frame(status)[
    as.data.frame(status)$section == "Lineage",
    ,
    drop = FALSE
  ]
}

lineage_test_data <- function(source_mode, source_description = "") {
  list(
    lineage_source_mode = source_mode,
    lineage_statement = "The dataset was assembled from field observations.",
    source_description = source_description,
    source_datasets_text = "",
    process_description = "",
    processing_tools = "",
    processing_scripts = "",
    output_description = "",
    lineage_notes = "",
    lineage_status = "reviewed"
  )
}

test_that("any supporting-document type can provide authoritative lineage", {
  pond <- tempfile("document-backed-lineage-pond-")
  dataset_path <- file.path(pond, "dataset")
  docs_path <- file.path(dataset_path, "docs")
  dir.create(docs_path, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines("Narrative report", file.path(docs_path, "project_history.pdf"))
  writeLines("Reproducible workflow", file.path(docs_path, "analysis.qmd"))

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (title) VALUES ('Document-backed lineage')"
  )
  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  datapond:::scan_supporting_documents(
    dataset_id = dataset_id,
    dataset_path = dataset_path,
    base_path = pond
  )
  docs <- datapond:::get_supporting_documents(dataset_id, pond)
  expect_setequal(docs$document_extension, c("pdf", "qmd"))

  datapond:::set_lineage_supporting_documents(
    dataset_id = dataset_id,
    document_ids = docs$id,
    base_path = pond
  )
  datapond:::save_dataset_lineage(
    dataset_id = dataset_id,
    data = lineage_test_data("supporting_documents"),
    base_path = pond
  )

  linked <- datapond:::get_lineage_supporting_documents(dataset_id, pond)
  expect_setequal(linked$id, docs$id)
  expect_setequal(linked$document_extension, c("pdf", "qmd"))
  expect_identical(lineage_completion_row(pond, dataset_id)$status, "complete")

  saved <- datapond:::get_dataset_lineage(dataset_id, pond)
  expect_identical(saved$lineage_source_mode, "supporting_documents")
  expect_identical(saved$source_description, "")
})

test_that("structured lineage remains complete without linked documents", {
  pond <- tempfile("structured-lineage-pond-")
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (title) VALUES ('Structured lineage')"
  )
  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  datapond:::save_dataset_lineage(
    dataset_id = dataset_id,
    data = lineage_test_data(
      "form",
      source_description = "Field observations supplied by the survey team."
    ),
    base_path = pond
  )

  expect_identical(lineage_completion_row(pond, dataset_id)$status, "complete")
})

test_that("a lineage document must belong to the selected dataset", {
  pond <- tempfile("lineage-document-boundary-pond-")
  first_path <- file.path(pond, "first")
  second_path <- file.path(pond, "second")
  dir.create(file.path(first_path, "docs"), recursive = TRUE)
  dir.create(file.path(second_path, "docs"), recursive = TRUE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines("Report", file.path(first_path, "docs", "report.pdf"))
  writeLines("Report", file.path(second_path, "docs", "report.pdf"))

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title) VALUES ('First')")
  first_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[[1L]]
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (title) VALUES ('Second')")
  second_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[[1L]]
  DBI::dbDisconnect(conn)

  datapond:::scan_supporting_documents(first_id, first_path, pond)
  datapond:::scan_supporting_documents(second_id, second_path, pond)
  second_doc <- datapond:::get_supporting_documents(second_id, pond)$id[[1L]]

  expect_error(
    datapond:::set_lineage_supporting_documents(first_id, second_doc, pond),
    "belonging to the selected dataset",
    fixed = TRUE
  )
})

test_that("lineage UI offers document authority without requiring a QMD", {
  html <- as.character(datapond:::mod_lineage_ui("lineage_test"))

  expect_match(html, "Where is the authoritative lineage detail?", fixed = TRUE)
  expect_match(html, "Supporting document(s)", fixed = TRUE)
  expect_match(html, "Detailed lineage sources and process steps", fixed = TRUE)
  expect_match(html, "Scan or manage supporting documents", fixed = TRUE)
})
