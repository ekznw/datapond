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

test_that("data pond folder roots are concrete and OS appropriate", {
  provider_called <- FALSE
  volume_provider <- function() {
    provider_called <<- TRUE
    c(
      `System (C:)` = "C:/",
      `External (E:)` = "E:/"
    )
  }

  other_roots <- datapond:::data_pond_folder_roots(
    os_type = "unix",
    sysname = "Darwin",
    volume_provider = volume_provider
  )

  expect_identical(other_roots, c(home = "~"))
  expect_false(provider_called)

  mounted_drive <- tempfile("mounted-drive-")
  dir.create(mounted_drive)
  linux_roots <- datapond:::data_pond_folder_roots(
    os_type = "unix",
    sysname = "Linux",
    volume_provider = volume_provider,
    linux_mount_candidates = c(
      `/media` = mounted_drive,
      `/mnt` = paste0(mounted_drive, "-missing")
    )
  )

  expect_identical(
    unname(linux_roots),
    c("~", mounted_drive)
  )
  expect_false(provider_called)

  windows_roots <- datapond:::data_pond_folder_roots(
    os_type = "windows",
    volume_provider = volume_provider
  )

  expect_false(is.function(windows_roots))
  expect_true(provider_called)
  expect_identical(
    unname(windows_roots),
    c("~", "C:/", "E:/")
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
  expect_match(
    sources[["mod_dataset_org"]],
    "state\\$refresh_org"
  )
})

test_that("the dataset organisation selector refreshes after an addition", {
  pond <- tempfile("organisation-refresh-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)

  state <- shiny::reactiveValues(
    data_pond = pond,
    refresh_org = 0L
  )

  shiny::testServer(
    datapond:::mod_dataset_org_server,
    args = list(state = state),
    {
      expect_length(org_choices(), 0L)

      expect_true(datapond:::insert_organisation(
        list(
          full = "Refresh Organisation",
          short = "Refresh Org",
          code = "REFRESHORG",
          department = "Data management",
          address = "Test address"
        ),
        pond
      ))

      state$refresh_org <- state$refresh_org + 1L
      session$flushReact()

      expect_identical(
        unname(org_choices()),
        "1"
      )
      expect_match(
        names(org_choices()),
        "Data management",
        fixed = TRUE
      )
    }
  )
})

test_that("the Parties person table omits IDs and includes contact details", {
  expect_identical(
    datapond:::party_person_table_columns(),
    c(
      "name_first",
      "name_last",
      "organisation_name_short",
      "organisation_code",
      "department",
      "email",
      "phone_number",
      "is_user"
    )
  )
})

test_that("organisation choices expose departments while retaining IDs", {
  choices <- datapond:::organisation_select_choices(
    data.frame(
      id = c(7L, 8L),
      organisation_name_short = c("Test Org", "Other Org"),
      organisation_code = c("TEST", "TEST"),
      organisation_department = c("Spatial unit", "Data unit"),
      stringsAsFactors = FALSE
    )
  )

  expect_identical(
    choices,
    c(
      "Test Org (TEST) — Spatial unit" = "7",
      "Other Org (TEST) — Data unit" = "8"
    )
  )
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

test_that("legacy organisation codes become non-unique without breaking links", {
  pond <- tempfile("legacy-organisation-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE organisation_tbl (
      id INTEGER PRIMARY KEY,
      organisation_name_full TEXT,
      organisation_name_short TEXT,
      organisation_code TEXT UNIQUE NOT NULL,
      organisation_address TEXT
    )
    "
  )
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE persons_tbl (
      id INTEGER PRIMARY KEY,
      name_first TEXT,
      name_last TEXT,
      organisation_id INTEGER,
      FOREIGN KEY (organisation_id) REFERENCES organisation_tbl(id)
    )
    "
  )
  DBI::dbExecute(
    conn,
    "
    INSERT INTO organisation_tbl (
      id,
      organisation_name_full,
      organisation_name_short,
      organisation_code,
      organisation_address
    )
    VALUES (4, 'Test Organisation', 'Test Org', 'TESTORG', 'Test address')
    "
  )
  DBI::dbExecute(
    conn,
    "
    INSERT INTO persons_tbl (id, name_first, name_last, organisation_id)
    VALUES (9, 'Legacy', 'Person', 4)
    "
  )
  DBI::dbDisconnect(conn)

  datapond:::init_db(pond)

  expect_true(datapond:::insert_organisation(
    list(
      full = "Test Organisation",
      short = "Test Org",
      code = "TESTORG",
      department = "Second unit",
      address = "Test address"
    ),
    pond
  ))

  organisations <- datapond:::get_organisations(pond)
  expect_equal(sum(organisations$organisation_code == "TESTORG"), 2L)

  person <- datapond:::get_persons(pond)
  expect_equal(person$organisation_id[person$id == 9L], 4L)
})

test_that("stored party vocabularies seed each new pond idempotently", {
  organisation_seed <- datapond:::load_organisations_csv(
    datapond:::datapond_resource("vocab_organisations.csv")
  )
  person_seed <- data.table::fread(
    datapond:::datapond_resource("vocab_persons.csv")
  )

  expect_true("department" %in% names(organisation_seed))
  expect_true(all(
    c("phone_number", "organisation_department") %in%
      names(person_seed)
  ))

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

test_that("person seeds resolve duplicate codes by organisation department", {
  pond <- tempfile("department-person-seed-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)

  organisations <- data.table::data.table(
    full = c("Test Organisation", "Test Organisation"),
    short = c("Test Org", "Test Org"),
    department = c("Spatial unit", "Data unit"),
    address = c("Test address", "Test address"),
    code = c("TEST", "TEST")
  )
  datapond:::sync_organisations_to_db(
    organisations,
    pond
  )

  people <- data.table::data.table(
    name_first = "Seeded",
    name_last = "Person",
    email = "seeded.person@example.org",
    phone_number = "+27 33 555 0102",
    role_title = "Analyst",
    organisation_code = "TEST",
    organisation_department = "Data unit",
    is_user = 0L,
    is_admin = 0L
  )
  datapond:::sync_persons_to_db(
    people,
    pond
  )

  person <- datapond:::get_persons(pond)
  expect_equal(person$department, "Data unit")
  expect_equal(person$phone_number, "+27 33 555 0102")
})

test_that("organisation departments and person contacts are persisted", {
  pond <- tempfile("person-contact-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)

  expect_true(datapond:::insert_organisation(
    list(
      full = "Test Organisation",
      short = "Test Org",
      code = "TESTORG",
      department = "Spatial planning",
      address = "Test address"
    ),
    pond
  ))
  expect_true(datapond:::insert_organisation(
    list(
      full = "Test Organisation",
      short = "Test Org",
      code = "TESTORG",
      department = "Data science",
      address = "Test address"
    ),
    pond
  ))

  organisations <- datapond:::get_organisations(pond)
  expect_equal(sum(organisations$organisation_code == "TESTORG"), 2L)
  organisation_id <- organisations$id[
    organisations$organisation_department == "Spatial planning"
  ]
  person_id <- datapond:::insert_person(
    list(
      first = "First",
      last = "Person",
      email = "first.person@example.org",
      phone_number = "+27 33 555 0100",
      org_id = organisation_id
    ),
    pond
  )

  person <- datapond:::get_persons(pond)
  person <- person[person$id == person_id, , drop = FALSE]

  expect_equal(person$email, "first.person@example.org")
  expect_equal(person$department, "Spatial planning")
  expect_equal(person$phone_number, "+27 33 555 0100")

  expect_true(datapond:::update_organisation(
    organisation_id = organisation_id,
    data = list(
      full = "Test Organisation",
      short = "Test Org",
      code = "TESTORG",
      department = "Data management",
      address = "Test address"
    ),
    base_path = pond
  ))

  datapond:::update_person(
    person_id = person_id,
    data = list(
      first = "First",
      last = "Person",
      email = "updated.person@example.org",
      phone_number = "+27 33 555 0101",
      org_id = organisation_id
    ),
    base_path = pond
  )

  person <- datapond:::get_persons(pond)
  person <- person[person$id == person_id, , drop = FALSE]

  expect_equal(person$email, "updated.person@example.org")
  expect_equal(person$department, "Data management")
  expect_equal(person$phone_number, "+27 33 555 0101")
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
