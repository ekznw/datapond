test_that("bundled resources are available", {
  resources <- c(
    "schema_grouped.rds",
    "vocab_keywords.csv",
    "vocab_organisations.csv",
    "vocab_persons.csv"
  )

  paths <- vapply(
    resources,
    datapond:::datapond_resource,
    character(1)
  )

  expect_true(all(file.exists(paths)))
  expect_type(readRDS(paths[[1]]), "list")
})

test_that("the XLSX workbook dynamically produces the packaged schema", {
  stored <- readRDS(datapond:::datapond_resource("schema_grouped.rds"))
  generated <- datapond:::load_schema_grouped(
    datapond:::datapond_resource("metadata_schema.xlsx")
  )

  expect_equal(generated, stored)
})

test_that("the packaged Shiny application can be constructed", {
  expect_s3_class(
    datapond:::datapond_app(),
    "shiny.appobj"
  )
})

test_that("collapsing configuration allows the main panel to use full width", {
  html <- as.character(datapond:::ui)

  expect_match(html, 'id="main_panel"', fixed = TRUE)
  expect_match(html, "mainPanel.classList.add('col-sm-12')", fixed = TRUE)
  expect_match(html, "mainPanel.classList.add('col-sm-9')", fixed = TRUE)
})

test_that("text and textarea controls use their full layout-column width", {
  html <- as.character(datapond:::ui)

  expect_match(
    html,
    ".shiny-input-container:has(> input[type='text'].form-control)",
    fixed = TRUE
  )
  expect_match(
    html,
    ".shiny-input-container:has(> textarea.form-control)",
    fixed = TRUE
  )
})

test_that("Dataset status legend uses the shared earth palette", {
  html <- as.character(datapond:::mod_dataset_status_ui("status"))
  palette <- datapond:::dataset_status_palette()
  borders <- datapond:::dataset_status_border_palette()

  expected_codes <- c(
    unname(palette[c(
      "complete",
      "partial",
      "needs_review",
      "not_started"
    )]),
    unname(borders[c(
      "complete",
      "partial",
      "needs_review",
      "not_started"
    )])
  )

  for (colour in expected_codes) {
    expect_match(html, colour, fixed = TRUE)
  }
})

test_that("a new pond database can be initialized", {
  pond <- tempfile("metadata-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)

  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)

  db_path <- file.path(pond, "metadata.db")
  expect_true(file.exists(db_path))

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  tables <- DBI::dbListTables(conn)
  expect_true(all(c("entry_tbl", "persons_tbl", "data_file_tbl") %in% tables))
})

test_that("stored party vocabularies seed each new pond idempotently", {
  organisation_seed <- datapond:::load_organisations_csv(
    datapond:::datapond_resource("vocab_organisations.csv")
  )
  person_seed <- data.table::fread(
    datapond:::datapond_resource("vocab_persons.csv")
  )

  for (pond in c(tempfile("party-pond-a-"), tempfile("party-pond-b-"))) {
    datapond:::init_db(pond)

    datapond:::seed_pond_parties(pond)
    datapond:::seed_pond_parties(pond)

    expect_equal(
      nrow(datapond:::get_organisations(pond)),
      nrow(organisation_seed)
    )
    expect_equal(
      nrow(datapond:::get_persons(pond)),
      nrow(person_seed)
    )
    expect_equal(
      nrow(datapond:::get_users(pond)),
      sum(person_seed$is_user == 1L)
    )
  }
})

test_that("legacy duplicate emails do not prevent database startup", {
  pond <- tempfile("duplicate-email-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(conn, "DROP INDEX idx_persons_email_unique")
  DBI::dbExecute(
    conn,
    "
    INSERT INTO persons_tbl (name_first, name_last, email)
    VALUES
      ('First', 'Person', 'duplicate@example.org'),
      ('Second', 'Person', ' DUPLICATE@example.org ')
    "
  )
  DBI::dbDisconnect(conn)

  expect_warning(
    datapond:::init_db(pond),
    "unique email index was not created"
  )

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  expect_equal(
    DBI::dbGetQuery(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM persons_tbl
      WHERE LOWER(TRIM(email)) = 'duplicate@example.org'
      "
    )$n,
    2L
  )

  indexes <- DBI::dbGetQuery(conn, "PRAGMA index_list(persons_tbl)")
  expect_false("idx_persons_email_unique" %in% indexes$name)
})
