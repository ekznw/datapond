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

test_that("Pond overview exposes bibliography and citation controls", {
  html <- as.character(datapond:::ui)

  for (id in c(
    "copy_citation_key",
    "copy_quarto_citation",
    "copy_biblatex_entry",
    "refresh_pond_bibliography"
  )) {
    expect_match(
      html,
      paste0(
        'id="',
        id,
        '"'
      ),
      fixed = TRUE
    )
  }

  expect_match(
    htmltools::renderTags(
      datapond:::ui
    )$head,
    "datapond-copy-text",
    fixed = TRUE
  )
})

test_that("collapsing configuration allows the main panel to use full width", {
  html <- as.character(datapond:::ui)

  expect_match(html, 'id="main_panel"', fixed = TRUE)
  expect_match(html, "mainPanel.classList.add('col-sm-12')", fixed = TRUE)
  expect_match(html, "mainPanel.classList.add('col-sm-9')", fixed = TRUE)
})

test_that("new ponds default dataset creation to the dev folder", {
  expect_identical(
    datapond:::default_pond_config()$dataset_parent_dir,
    "dev"
  )
  expect_identical(
    datapond:::normalise_dataset_parent_dir(NULL),
    "dev"
  )
  expect_identical(
    datapond:::normalise_dataset_parent_dir("src"),
    "src"
  )
})

test_that("pond organisation selectors use client-side choices", {
  sources <- c(
    mod_licensing = paste(
      deparse(datapond:::mod_licensing_server),
      collapse = "\n"
    ),
    mod_data_access = paste(
      deparse(datapond:::mod_data_access_server),
      collapse = "\n"
    ),
    mod_dataset_org = paste(
      deparse(datapond:::mod_dataset_org_server),
      collapse = "\n"
    )
  )

  expect_match(
    sources[["mod_licensing"]],
    '"rights_holder_organisation_id",\\s*choices = choices,\\s*selected = current,\\s*server = FALSE'
  )
  expect_false(any(grepl("server = TRUE", sources, fixed = TRUE)))
})

test_that("form controls use their full layout-column width", {
  html <- as.character(datapond:::ui)

  expect_match(
    html,
    ".shiny-input-container:not(.shiny-input-container-inline)",
    fixed = TRUE
  )
})

test_that("select and selectize controls use their full layout-column width", {
  html <- as.character(datapond:::ui)

  expect_match(
    html,
    ".shiny-input-container .selectize-control",
    fixed = TRUE
  )

  select_html <- as.character(
    shiny::selectInput("layout_test", "Layout test", choices = NULL)
  )
  expect_match(
    select_html,
    'class="shiny-input-select"',
    fixed = TRUE
  )
})

test_that("navigation tabs use the shared earthy colour direction", {
  html <- as.character(datapond:::ui)

  expect_match(html, ".nav-tabs > li > a", fixed = TRUE)
  expect_match(html, "color: #3F5146 !important", fixed = TRUE)
  expect_match(html, "background-color: #D1DDC7", fixed = TRUE)
  expect_match(html, ".nav-tabs > li.active > a", fixed = TRUE)
  expect_match(html, "background-color: #526657 !important", fixed = TRUE)
})

test_that("the primary app header uses a warm off-white background", {
  html <- as.character(datapond:::ui)

  expect_match(html, "#main_panel > h2", fixed = TRUE)
  expect_match(
    html,
    "#main_panel > .tabbable > .nav-tabs",
    fixed = TRUE
  )
  expect_match(html, "background-color: #F1E9DA", fixed = TRUE)
  expect_match(html, "border: 1px solid #C3A35B", fixed = TRUE)
})

test_that("the app header displays the installed package version", {
  html <- as.character(datapond:::ui)
  expected <- paste0(
    "Data pond \u2014 v",
    as.character(utils::packageVersion("datapond"))
  )

  expect_match(html, expected, fixed = TRUE)
  expect_false(grepl("Data pond \u2014 v0.5", html, fixed = TRUE))
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

test_that("keyword vocabulary seeds a new pond without an active user", {
  pond <- tempfile("keyword-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)

  keyword_seed <- data.table::fread(
    datapond:::datapond_resource("vocab_keywords.csv")
  )

  expect_no_error(
    result <- datapond:::seed_keyword_vocab_if_empty(
      vocab_dt = keyword_seed,
      base_path = pond,
      user_id = NULL
    )
  )
  expect_gt(result$inserted, 0L)

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  expect_equal(
    DBI::dbGetQuery(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM keyword_vocab_tbl
      WHERE created_by_user_id IS NOT NULL
         OR updated_by_user_id IS NOT NULL
      "
    )$n,
    0L
  )
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
