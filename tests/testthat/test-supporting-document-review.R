supporting_document_status <- function(pond, dataset_id) {
  status <- datapond:::get_dataset_completion_status(
    dataset_id = dataset_id,
    base_path = pond,
    schema_grouped = NULL
  )

  as.data.frame(status)[
    as.data.frame(status)$section == "Supporting documents",
    ,
    drop = FALSE
  ]
}


test_that("default supporting-document types can be reviewed in one pass", {
  pond <- tempfile("supporting-doc-review-pond-")
  dataset_path <- file.path(pond, "dataset")
  docs_path <- file.path(dataset_path, "docs")
  dir.create(docs_path, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines("Method", file.path(docs_path, "method_statement.md"))
  writeLines("Dictionary", file.path(docs_path, "data_dictionary.csv"))

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (title) VALUES ('Supporting document review')"
  )
  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbExecute(
    conn,
    "INSERT INTO persons_tbl (name_first, name_last) VALUES ('Review', 'User')"
  )
  person_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[[1L]]
  DBI::dbDisconnect(conn)

  datapond:::scan_supporting_documents(
    dataset_id = dataset_id,
    dataset_path = dataset_path,
    base_path = pond,
    user_person_id = person_id
  )

  before <- supporting_document_status(pond, dataset_id)
  expect_identical(before$status, "partial")
  expect_identical(before$required_missing, 2L)

  expect_identical(
    datapond:::accept_supporting_document_defaults(
      dataset_id = dataset_id,
      base_path = pond,
      user_person_id = person_id
    ),
    2L
  )

  after <- supporting_document_status(pond, dataset_id)
  expect_identical(after$status, "complete")
  expect_identical(after$required_missing, 0L)

  docs <- datapond:::get_supporting_documents(dataset_id, pond)
  expect_true(all(!is.na(docs$reviewed_at)))
  expect_true(all(docs$reviewed_by_person_id == person_id))
  expect_true(all(docs$document_type_user_set == 0L))

  changed_doc <- docs[1, , drop = FALSE]
  changed_rel_path <- changed_doc$document_rel_path
  changed_type <- if (changed_doc$document_type_auto_code == "other") {
    "lineage"
  } else {
    "other"
  }
  datapond:::upsert_supporting_document(
    dataset_id = dataset_id,
    row = changed_doc,
    class = list(
      code = changed_type,
      label = datapond:::supporting_document_type_label(changed_type),
      auto = "Updated automatic classification."
    ),
    base_path = pond,
    user_person_id = person_id
  )

  rescanned <- datapond:::get_supporting_documents(dataset_id, pond)
  changed_doc <- rescanned[
    rescanned$document_rel_path == changed_rel_path,
    ,
    drop = FALSE
  ]
  expect_true(is.na(changed_doc$reviewed_at))
  expect_identical(
    supporting_document_status(pond, dataset_id)$status,
    "partial"
  )
})


test_that("individual notes and type overrides remain available", {
  pond <- tempfile("supporting-doc-override-pond-")
  dataset_path <- file.path(pond, "dataset")
  docs_path <- file.path(dataset_path, "docs")
  dir.create(docs_path, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines("Report", file.path(docs_path, "report.md"))

  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (title) VALUES ('Supporting document override')"
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
  doc <- datapond:::get_supporting_documents(dataset_id, pond)[1, ]

  datapond:::update_supporting_document(
    doc_id = doc$id,
    data = list(
      document_type_code = "article",
      document_type_label = "Article",
      document_type_user_set = TRUE,
      document_type_user_text = "Published article, despite filename",
      include_in_lineage = FALSE,
      include_in_metadata = TRUE,
      notes = "Keep this explanatory note."
    ),
    base_path = pond
  )

  saved <- datapond:::get_supporting_documents(dataset_id, pond)[1, ]
  expect_identical(saved$document_type_code, "article")
  expect_identical(saved$document_type_user_set, 1L)
  expect_identical(
    saved$document_type_user_text,
    "Published article, despite filename"
  )
  expect_identical(saved$notes, "Keep this explanatory note.")
  expect_false(is.na(saved$reviewed_at))
  expect_identical(
    supporting_document_status(pond, dataset_id)$status,
    "complete"
  )
})
