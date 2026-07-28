test_that("unfrozen citation keys omit version and remain stable", {
  vals <- list(
    title = "Vegetation map",
    citation_date = "2026-07-28",
    keyword_object = "vegetation",
    version = "0.2.0",
    folder_locked = 0L
  )

  key <- datapond:::generate_citation_key(
    vals = vals,
    org_df = data.frame(),
    stable_rand_part = "a2b3"
  )

  expect_identical(
    key,
    "org_202607_vegetation_v_a2b3"
  )
  expect_false(grepl("_v0_2_0$", key))

  edited_vals <- vals
  edited_vals$title <- "A substantially revised title"
  edited_vals$version <- "0.3.0"

  expect_identical(
    datapond:::generate_citation_key(
      vals = edited_vals,
      org_df = data.frame(),
      existing_key = key
    ),
    key
  )
})


test_that("freezing adds only the current version suffix", {
  base_key <- "ekznw_202607_veget_vegm_a2b3"

  locked <- datapond:::generate_citation_key(
    vals = list(
      version = "1.2.3",
      folder_locked = 1L
    ),
    org_df = data.frame(),
    existing_key = base_key
  )

  expect_identical(
    locked,
    paste0(
      base_key,
      "_v1_2_3"
    )
  )

  expect_identical(
    datapond:::generate_citation_key(
      vals = list(
        version = "1.2.3",
        folder_locked = 0L
      ),
      org_df = data.frame(),
      existing_key = locked
    ),
    base_key
  )
})


test_that("BibLaTeX dataset entries preserve corporate authors", {
  record <- data.frame(
    citation_key = "ekznw_vegetation_a2b3",
    title = "KwaZulu-Natal vegetation & habitats",
    citation_date = "2026-07-28",
    version = "0.2.0",
    citation_author_type = "organisation",
    doi = "https://doi.org/10.1234/example",
    data_distribution_url =
      "https://example.org/data?id=10",
    abstract = "A test dataset.",
    stringsAsFactors = FALSE
  )

  orgs <- data.frame(
    organisation_name_full =
      "Ezemvelo KZN Wildlife",
    organisation_name_short =
      "EKZNW",
    organisation_address =
      "Pietermaritzburg",
    stringsAsFactors = FALSE
  )

  entry <- datapond:::format_biblatex_dataset(
    record = record,
    org_df = orgs
  )

  expect_match(
    entry,
    "@dataset{ekznw_vegetation_a2b3,",
    fixed = TRUE
  )
  expect_match(
    entry,
    "author = {{Ezemvelo KZN Wildlife}}",
    fixed = TRUE
  )
  expect_match(
    entry,
    "title = {{KwaZulu-Natal vegetation \\& habitats}}",
    fixed = TRUE
  )
  expect_match(
    entry,
    "doi = {10.1234/example}",
    fixed = TRUE
  )
})


test_that("pond bibliography is rebuilt and tracks frozen keys", {
  pond <- tempfile(
    "biblatex-pond-"
  )

  dir.create(
    pond,
    recursive = TRUE,
    showWarnings = FALSE
  )

  datapond:::init_db(
    pond
  )

  conn <- datapond:::get_conn(
    pond
  )

  datapond:::ensure_columns(
    conn = conn,
    table = "entry_tbl",
    fields = c(
      "citation_author_type"
    ),
    types = c(
      citation_author_type = "TEXT"
    )
  )

  DBI::dbExecute(
    conn,
    "
    INSERT INTO organisation_tbl (
      organisation_name_full,
      organisation_name_short,
      organisation_code,
      organisation_address
    )
    VALUES (?, ?, ?, ?)
    ",
    params = list(
      "Ezemvelo KZN Wildlife",
      "EKZNW",
      "EKZNW",
      "Pietermaritzburg"
    )
  )

  DBI::dbExecute(
    conn,
    "
    INSERT INTO entry_tbl (
      title,
      citation_date,
      version,
      citation_key,
      citation_author_type,
      lifecycle_status,
      folder_locked
    )
    VALUES (?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      "Test vegetation",
      "2026-07-28",
      "1.2.3",
      "ekznw_202607_veget_test_a2b3_v0_1_0",
      "organisation",
      "draft",
      0L
    )
  )

  dataset_id <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM entry_tbl LIMIT 1"
  )$id[1]

  organisation_id <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM organisation_tbl LIMIT 1"
  )$id[1]

  DBI::dbExecute(
    conn,
    "
    INSERT INTO dataset_organisation_tbl (
      dataset_id,
      organisation_id,
      role
    )
    VALUES (?, ?, ?)
    ",
    params = list(
      dataset_id,
      organisation_id,
      "owner"
    )
  )

  DBI::dbDisconnect(
    conn
  )

  result <- datapond:::write_pond_bibliography(
    pond
  )

  expect_identical(
    result$entries,
    1L
  )
  expect_true(
    file.exists(
      file.path(
        pond,
        "datapond.bib"
      )
    )
  )

  bibliography <- paste(
    readLines(
      result$path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )

  expect_match(
    bibliography,
    "@dataset{ekznw_202607_veget_test_a2b3,",
    fixed = TRUE
  )

  conn <- datapond:::get_conn(
    pond
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE entry_tbl
    SET
      lifecycle_status = 'released',
      folder_locked = 1
    WHERE id = ?
    ",
    params = list(
      dataset_id
    )
  )

  DBI::dbDisconnect(
    conn
  )

  result <- datapond:::write_pond_bibliography(
    pond
  )

  expect_identical(
    result$keys,
    "ekznw_202607_veget_test_a2b3_v1_2_3"
  )
  expect_true(
    file.exists(
      paste0(
        result$path,
        ".bak"
      )
    )
  )
})
