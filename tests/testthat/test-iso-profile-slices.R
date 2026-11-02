iso_profile_fixture <- function() {
  pond <- tempfile("iso-profile-slices-")
  dir.create(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  datapond:::ensure_columns(conn, "entry_tbl", "keyword_topic")
  datapond:::ensure_columns(conn, "entry_tbl", c("abstract", "metadata_creation_date"))
  DBI::dbExecute(conn, "
    INSERT INTO organisation_tbl (organisation_name_full, organisation_code)
    VALUES ('Metadata Office', 'META')
  ")
  organisation_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "
    INSERT INTO persons_tbl (name_first, name_last, email, organisation_id)
    VALUES ('Mina', 'Curator', 'mina@example.test', ?)
  ", params = list(organisation_id))
  person_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "
    INSERT INTO entry_tbl (uuid, title, citation_date, keyword_topic)
    VALUES ('resource-uuid', 'ISO profile test', '2024-06-30', 'ecology')
  ")
  dataset_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = 'Profile fixture abstract.',
    metadata_creation_date = '2024-06-01 10:00:00' WHERE id = ?", params = list(dataset_id))
  DBI::dbDisconnect(conn)
  datapond:::init_db(pond)
  list(
    path = pond, dataset_id = as.integer(dataset_id), person_id = as.integer(person_id),
    organisation_id = as.integer(organisation_id)
  )
}

test_that("Slice 1B migration adds distinct metadata identity and defaults idempotently", {
  fixture <- iso_profile_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::init_db(fixture$path)
  conn <- datapond:::get_conn(fixture$path)
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(fixture$dataset_id))
  DBI::dbDisconnect(conn)

  expect_true(nzchar(record$metadata_uuid))
  expect_false(identical(record$metadata_uuid, record$uuid))
  expect_identical(record$metadata_resource_scope_code, "dataset")
  expect_identical(record$metadata_language_code, "eng")
  expect_identical(record$metadata_character_encoding, "utf8")
})

test_that("legacy publication date is backfilled without inferring topic category", {
  fixture <- iso_profile_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  dates <- datapond:::get_dataset_citation_dates(fixture$dataset_id, fixture$path)
  topics <- datapond:::get_dataset_topic_categories(fixture$dataset_id, fixture$path)

  expect_equal(nrow(dates), 1L)
  expect_identical(dates$date_value, "2024-06-30")
  expect_identical(dates$date_type_code, "publication")
  expect_equal(nrow(topics), 0L)
})

test_that("Slice 1C categories and typed dates create update reload and delete", {
  fixture <- iso_profile_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::replace_dataset_topic_categories(
    fixture$dataset_id, c("biota", "environment"), fixture$path, fixture$person_id
  )
  additional <- data.frame(
    date_value = c("2020", "2025-02-14"),
    date_type_code = c("creation", "revision"),
    date_precision = c("year", "day"),
    date_note = c("Original survey year", "Quality revision"),
    stringsAsFactors = FALSE
  )
  datapond:::replace_dataset_citation_dates(
    fixture$dataset_id, additional, fixture$path, fixture$person_id,
    primary_publication_date = "2024-07-01"
  )

  expect_identical(
    datapond:::get_dataset_topic_categories(fixture$dataset_id, fixture$path)$topic_category_code,
    c("biota", "environment")
  )
  dates <- datapond:::get_dataset_citation_dates(fixture$dataset_id, fixture$path)
  expect_identical(dates$date_type_code, c("publication", "creation", "revision"))
  conn <- datapond:::get_conn(fixture$path)
  expect_identical(
    DBI::dbGetQuery(conn, "SELECT citation_date FROM entry_tbl WHERE id = ?", params = list(fixture$dataset_id))$citation_date,
    "2024-07-01"
  )
  DBI::dbDisconnect(conn)

  datapond:::replace_dataset_topic_categories(
    fixture$dataset_id, "environment", fixture$path, fixture$person_id
  )
  datapond:::replace_dataset_citation_dates(
    fixture$dataset_id, additional[1, , drop = FALSE], fixture$path,
    fixture$person_id, primary_publication_date = "2024-07-01"
  )
  expect_identical(
    datapond:::get_dataset_topic_categories(fixture$dataset_id, fixture$path)$topic_category_code,
    "environment"
  )
  expect_equal(nrow(datapond:::get_dataset_citation_dates(
    fixture$dataset_id, fixture$path
  )), 2L)
})

test_that("Slice 1D responsibilities remain separate from review history", {
  fixture <- iso_profile_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  responsibilities <- data.frame(
    person_id = c(fixture$person_id, NA_integer_),
    organisation_id = c(fixture$organisation_id, fixture$organisation_id),
    role_code = c("creator", "pointOfContact"),
    is_primary = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  datapond:::replace_metadata_responsibilities(
    fixture$dataset_id, responsibilities, fixture$path, fixture$person_id
  )
  saved <- datapond:::get_metadata_responsibilities(fixture$dataset_id, fixture$path)
  expect_setequal(saved$role_code, c("creator", "pointOfContact"))
  expect_equal(sum(saved$is_primary), 2L)

  conn <- datapond:::get_conn(fixture$path)
  expect_equal(DBI::dbGetQuery(
    conn, "SELECT COUNT(*) AS n FROM metadata_review_tbl WHERE dataset_id = ?",
    params = list(fixture$dataset_id)
  )$n, 0L)
  DBI::dbDisconnect(conn)

  datapond:::replace_metadata_responsibilities(
    fixture$dataset_id, saved[saved$role_code == "pointOfContact", ],
    fixture$path, fixture$person_id
  )
  expect_identical(
    datapond:::get_metadata_responsibilities(fixture$dataset_id, fixture$path)$role_code,
    "pointOfContact"
  )
})

test_that("scope controls topic and spatial completion requirements", {
  fixture <- iso_profile_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "
    UPDATE entry_tbl SET metadata_resource_scope_code = 'service' WHERE id = ?
  ", params = list(fixture$dataset_id))
  DBI::dbDisconnect(conn)
  status <- as.data.frame(datapond:::get_dataset_completion_status(
    fixture$dataset_id, fixture$path, NULL
  ))
  expect_identical(status$status[status$section == "ISO topic categories"], "not_applicable")
  expect_identical(status$status[status$section == "Spatial metadata"], "not_applicable")
})

test_that("profile export consumes identity, dates, topics, and responsibilities", {
  fixture <- iso_profile_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::save_iso_profile_sections(
    fixture$dataset_id,
    profile = list(
      metadata_uuid = "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee",
      metadata_resource_scope_code = "dataset",
      metadata_language_code = "eng",
      metadata_character_encoding = "utf8"
    ),
    topic_categories = c("biota", "environment"),
    citation_dates = data.frame(
      date_value = "2025-01-15", date_type_code = "revision",
      date_precision = "day", date_note = "Reviewed", stringsAsFactors = FALSE
    ),
    responsibilities = data.frame(
      person_id = fixture$person_id, organisation_id = fixture$organisation_id,
      role_code = "pointOfContact", is_primary = 1L
    ),
    primary_publication_date = "2024-07-01",
    base_path = fixture$path,
    user_person_id = fixture$person_id
  )
  datapond:::save_manual_spatial_extent(
    dataset_id = fixture$dataset_id,
    base_path = fixture$path,
    extent_label = "Export extent",
    crs_value = "EPSG:4326",
    xmin = 30, ymin = -30, xmax = 31, ymax = -29,
    source_note = "Test extent"
  )
  xml <- datapond:::build_iso_profile_discovery_xml(fixture$dataset_id, fixture$path)
  ns <- xml2::xml_ns(xml)
  expect_identical(
    xml2::xml_text(xml2::xml_find_first(xml, ".//mdb:metadataIdentifier//mcc:code", ns)),
    "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee"
  )
  expect_length(xml2::xml_find_all(xml, ".//mri:topicCategory", ns), 2L)
  expect_length(xml2::xml_find_all(xml, ".//mri:citation//cit:CI_Date", ns), 2L)
  expect_length(xml2::xml_find_all(xml, "/mdb:MD_Metadata/mdb:dateInfo", ns), 1L)
  expect_length(xml2::xml_find_all(xml, "/mdb:MD_Metadata/mdb:contact", ns), 1L)
})

test_that("profile, topic, date and responsibility editors expose their guidance", {
  profile_html <- as.character(datapond:::mod_metadata_profile_ui("profile"))
  topic_html <- as.character(datapond:::mod_topic_categories_ui("profile"))
  dates_html <- as.character(datapond:::mod_citation_dates_ui("profile"))
  responsibility_html <- as.character(datapond:::mod_metadata_responsibilities_ui("profile"))
  expect_match(profile_html, "Advanced metadata profile", fixed = TRUE)
  expect_match(profile_html, "Metadata-record UUID", fixed = TRUE)
  expect_match(topic_html, "Use keywords for more specific subjects", fixed = TRUE)
  expect_match(dates_html, "primary publication date", fixed = TRUE)
  expect_match(responsibility_html, "The active editor is the default metadata contact", fixed = TRUE)
})

test_that("the active editor supplies a default metadata point of contact", {
  rows <- datapond:::metadata_responsibilities_with_default(data.frame(), 42L)
  expect_identical(rows$person_id, 42L)
  expect_identical(rows$role_code, "pointOfContact")
  expect_identical(rows$is_primary, 1L)

  explicit <- data.frame(
    person_id = 9L,
    organisation_id = NA_integer_,
    role_code = "pointOfContact",
    is_primary = 1L
  )
  preserved <- datapond:::metadata_responsibilities_with_default(explicit, 42L)
  expect_equal(nrow(preserved), 1L)
  expect_identical(preserved$person_id, 9L)
})
