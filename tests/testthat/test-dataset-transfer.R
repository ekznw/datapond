transfer_test_pond <- function(prefix) {
  pond <- tempfile(prefix)
  dir.create(pond, recursive = TRUE)
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  pond
}


transfer_test_user <- function(pond, first, last, email, user_name, admin = 0L) {
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO organisation_tbl",
      "(organisation_name_full, organisation_code) VALUES (?, ?)"
    ),
    params = list(paste(first, "Organisation"), toupper(substr(first, 1L, 3L)))
  )
  organisation_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1]
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO persons_tbl",
      "(name_first, name_last, email, organisation_id)",
      "VALUES (?, ?, ?, ?)"
    ),
    params = list(first, last, email, organisation_id)
  )
  person_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1]
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO user_tbl (person_id, user_name, is_active, is_admin)",
      "VALUES (?, ?, 1, ?)"
    ),
    params = list(person_id, user_name, as.integer(admin))
  )
  list(
    user_id = as.integer(DBI::dbGetQuery(
      conn,
      "SELECT last_insert_rowid() AS id"
    )$id[1]),
    person_id = as.integer(person_id),
    organisation_id = as.integer(organisation_id)
  )
}


test_that("dataset bundles round-trip files, metadata and source provenance", {
  source_pond <- transfer_test_pond("transfer-source-")
  target_pond <- transfer_test_pond("transfer-target-")
  on.exit(unlink(c(source_pond, target_pond), recursive = TRUE, force = TRUE), add = TRUE)

  editor <- transfer_test_user(
    source_pond,
    "Alice",
    "Editor",
    "alice@example.test",
    "alice"
  )
  importer <- transfer_test_user(
    target_pond,
    "Paul",
    "Importer",
    "paul@example.test",
    "paul",
    admin = 1L
  )

  nested_parent <- file.path(source_pond, "src", "programme")
  dir.create(nested_parent, recursive = TRUE)
  folder <- datapond:::create_dataset_folder_structure(
    base_path = nested_parent,
    folder_key = "portable_test",
    uuid = "transfer-uuid-001",
    parent_dir = ".",
    data_pond_id = datapond:::ensure_data_pond_id(source_pond)
  )
  writeLines("name,value\nexample,1", file.path(folder$data_path, "example.csv"))

  source_conn <- datapond:::get_conn(source_pond)
  DBI::dbExecute(
    source_conn,
    paste(
      "INSERT INTO entry_tbl",
      "(uuid, title, dataset_path, data_path, data_pond_id,",
      "created_by_user_id, lifecycle_status, folder_key)",
      "VALUES (?, ?, ?, ?, ?, ?, 'draft', 'portable_test')"
    ),
    params = list(
      "transfer-uuid-001",
      "Portable test",
      folder$dataset_path,
      folder$data_path,
      datapond:::ensure_data_pond_id(source_pond),
      editor$user_id
    )
  )
  source_dataset_id <- as.integer(DBI::dbGetQuery(
    source_conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1])
  DBI::dbExecute(
    source_conn,
    paste(
      "INSERT INTO authority_tbl",
      "(dataset_id, person_id, author_order, role)",
      "VALUES (?, ?, 1, 'author')"
    ),
    params = list(source_dataset_id, editor$person_id)
  )
  DBI::dbExecute(
    source_conn,
    paste(
      "INSERT INTO data_file_tbl",
      "(dataset_id, data_file_rel_path_full, file_name, file_path, data_pond_id)",
      "VALUES (?, 'example.csv', 'example.csv', ?, ?)"
    ),
    params = list(
      source_dataset_id,
      file.path(folder$data_path, "example.csv"),
      datapond:::ensure_data_pond_id(source_pond)
    )
  )
  source_file_id <- as.integer(DBI::dbGetQuery(source_conn, "SELECT last_insert_rowid() AS id")$id[1])
  DBI::dbExecute(source_conn,
    "UPDATE data_file_tbl SET data_file_format = 'attribute_table' WHERE id = ?",
    params = list(source_file_id))
  DBI::dbExecute(source_conn, "INSERT INTO dataset_attribute_tbl
    (dataset_id, data_file_id, data_file_rel_path_full, data_file_name,
     data_field_name, data_field_desc, attribute_type, attribute_order)
    VALUES (?, ?, 'example.csv', 'example.csv', 'name', 'Example name', 'character', 1)",
    params = list(source_dataset_id, source_file_id))
  datapond:::ensure_columns(source_conn, "entry_tbl", "keyword_topic")
  # These fields only exist after a source form has used them. The target is a
  # fresh pond and must receive them without first opening the same forms.
  dynamic_metadata <- list(
    purpose = "Delineate conservation districts",
    citation = "Source organisation (2022). Conservation districts.",
    dataset_credit = "Source organisation",
    access_constraints = "Restricted to internal use",
    use_limitations = "Owner acknowledgement required",
    custom_resolution = 30.5
  )
  for (field in names(dynamic_metadata)) {
    DBI::dbExecute(source_conn, paste(
      "ALTER TABLE entry_tbl ADD COLUMN",
      DBI::dbQuoteIdentifier(source_conn, field),
      if (is.numeric(dynamic_metadata[[field]])) "REAL" else "TEXT"
    ))
    DBI::dbExecute(source_conn, paste0(
      "UPDATE entry_tbl SET ", DBI::dbQuoteIdentifier(source_conn, field),
      " = ? WHERE id = ?"
    ), params = list(dynamic_metadata[[field]], source_dataset_id))
  }
  DBI::dbExecute(source_conn, "UPDATE entry_tbl SET keyword_topic = 'portable data' WHERE id = ?",
                 params = list(source_dataset_id))
  DBI::dbExecute(source_conn, "UPDATE entry_tbl SET metadata_creation_date='2016-03-02',
    geographic_extent_name='KwaZulu-Natal, South Africa' WHERE id=?",
    params = list(source_dataset_id))
  DBI::dbExecute(source_conn, "INSERT INTO keyword_vocab_tbl
    (field_code, option_value, option_label) VALUES ('keyword_topic', 'portable data', 'Portable data')")
  DBI::dbDisconnect(source_conn)

  lineage_source_one <- datapond:::save_dataset_lineage_source(
    source_dataset_id,
    list(title = "Survey observations", description = "Primary field records"),
    source_pond,
    editor$person_id
  )
  lineage_source_two <- datapond:::save_dataset_lineage_source(
    source_dataset_id,
    list(title = "Study boundary", description = "Analysis extent"),
    source_pond,
    editor$person_id
  )
  writeLines("Method authority", file.path(folder$dataset_path, "docs", "method.pdf"))
  datapond:::scan_supporting_documents(
    source_dataset_id, folder$dataset_path, source_pond, editor$person_id
  )
  lineage_document_id <- datapond:::get_supporting_documents(
    source_dataset_id, source_pond
  )$id[1]
  datapond:::save_dataset_lineage_source(
    source_dataset_id,
    list(
      title = "Study boundary", description = "Analysis extent",
      supporting_document_id = lineage_document_id,
      value_origin_code = "supporting_document"
    ),
    source_pond,
    editor$person_id,
    lineage_source_two
  )
  lineage_step <- datapond:::save_dataset_lineage_process_step(
    source_dataset_id,
    list(description = "Clipped observations to the study boundary"),
    source_pond,
    editor$person_id
  )
  datapond:::set_dataset_lineage_step_sources(
    source_dataset_id,
    lineage_step,
    c(lineage_source_one, lineage_source_two),
    source_pond,
    editor$person_id
  )
  datapond:::save_iso_profile_sections(
    dataset_id = source_dataset_id,
    profile = list(
      metadata_uuid = "11111111-2222-4333-8444-555555555555",
      metadata_resource_scope_code = "dataset",
      metadata_language_code = "eng",
      metadata_character_encoding = "utf8",
      resource_language_code = "zul",
      resource_character_encoding = "utf8"
    ),
    topic_categories = c("biota", "environment"),
    citation_dates = data.frame(
      date_value = "2025-01-15", date_type_code = "revision",
      date_precision = "day", date_note = "Transfer fixture",
      stringsAsFactors = FALSE
    ),
    responsibilities = data.frame(
      person_id = editor$person_id,
      organisation_id = editor$organisation_id,
      role_code = "pointOfContact",
      is_primary = 1L
    ),
    primary_publication_date = "2024-01-01",
    base_path = source_pond,
    user_person_id = editor$person_id
  )
  datapond:::save_iso_wave2_sections(
    dataset_id = source_dataset_id,
    alternate_titles = c("Transfer short title", "Earlier transfer title"),
    online_resources = data.frame(
      resource_url = "https://example.test/transfer", resource_name = "Transfer download",
      resource_description = "Portable package", protocol = "HTTPS",
      function_code = "download", media_type = "application/zip", is_primary = 1L
    ),
    distribution_formats = data.frame(
      specification_title = "Comma-separated values", format_version = "RFC 4180",
      media_type = "text/csv", format_uri = "https://www.rfc-editor.org/rfc/rfc4180"
    ),
    quality_results = data.frame(
      scope_code = "dataset", data_file_id = NA_integer_, attribute_id = NA_integer_,
      measure_identifier = "TRANSFER-DQ-01", measure_name = "Transfer completeness",
      quality_element_code = "DQ_CompletenessOmission",
      measure_type_code = "completeness", evaluation_method = "Compared with source manifest",
      evaluation_date = "2025-01-16", result_type_code = "quantitative",
      result_value = "100", result_unit = "percent", conformance_specification = NA_character_,
      explanation = NA_character_, pass = NA_integer_
    ),
    temporal_extents = data.frame(
      extent_label = "Transfer period", start_value = "2020-01-01", end_value = "2020-12-31",
      start_precision = "day", end_precision = "day", start_indeterminate = NA_character_,
      end_indeterminate = NA_character_, description = "Fixture coverage"
    ),
    base_path = source_pond,
    user_person_id = editor$person_id
  )
  datapond:::save_dataset_release_rights(
    source_dataset_id,
    list(
      embargo_status = "Yes", embargo_reason = "Transfer review",
      embargo_start_date = "2025-01-01", embargo_end_date = "2025-12-31",
      release_addressee = "Import partner", release_statement = "Release after review.",
      dissemination_constraint_code = "approvedRecipients"
    ),
    source_pond,
    editor$person_id
  )
  datapond:::replace_dataset_vertical_extents(source_dataset_id, data.frame(
    extent_label = "Height", minimum_value = 5, maximum_value = 125, unit_symbol = "m",
    unit_uri = NA_character_, vertical_crs_name = "EGM2008", vertical_crs_identifier = "EPSG:3855",
    vertical_crs_uri = "https://epsg.io/3855", vertical_crs_type = "geoidal",
    description = "Transfer fixture height"), source_pond, editor$person_id)
  datapond:::save_dataset_feature_catalogue(source_dataset_id,
    list(title = "Portable test fields", version = "1", language_code = "eng"),
    source_pond, editor$person_id)
  datapond:::replace_party_addresses(source_pond, data.frame(
    delivery_point = "1 Transfer Road", city = "Pietermaritzburg",
    administrative_area = "KwaZulu-Natal", postal_code = "3201", country = "South Africa",
    electronic_mail_address = "alice@example.test", phone_number = NA_character_,
    online_resource = "https://example.test/office", is_primary = 1L),
    organisation_id = editor$organisation_id)
  datapond:::save_keyword_thesaurus(source_pond, list(
    field_code = "keyword_topic", citation_title = "Portable topics", edition = "1",
    edition_date = "2025-01-01", edition_date_type_code = "revision",
    thesaurus_uri = "https://example.test/topics", identifier = "PT",
    responsible_person_id = editor$person_id, responsible_organisation_id = NA_integer_,
    responsible_role_code = "publisher"))

  # Migrate then intentionally remove an old source before transfer. The
  # archival snapshot must not recreate it in the receiving pond.
  datapond:::save_dataset_lineage(source_dataset_id, list(
    lineage_statement = "Transfer lineage", lineage_status = "draft",
    source_description = "Intentionally removed legacy source", lineage_notes = "Transferred context"
  ), source_pond)
  migrated_lineage <- datapond:::get_dataset_lineage(source_dataset_id, source_pond)
  migrated_sources <- datapond:::get_dataset_lineage_sources(source_dataset_id, source_pond)
  removed_source <- migrated_sources$id[migrated_sources$value_origin_code == "legacy_backfill"]
  expect_length(removed_source, 1L)
  datapond:::delete_dataset_lineage_source(source_dataset_id, removed_source, source_pond)

  bundle <- datapond:::create_dataset_transfer_bundle(
    source_dataset_id,
    source_pond,
    editor$user_id
  )
  expect_true(file.exists(bundle$path))
  expect_identical(dirname(bundle$path), folder$dataset_path)

  manifest <- datapond:::read_dataset_transfer_manifest(bundle$path)
  expect_identical(manifest$source_parent_dir, "src")
  expect_identical(manifest$bundle_format_version, 5L)
  expect_identical(manifest$source_relative_parent, "programme")
  expect_identical(manifest$source_exported_by_user_id, editor$user_id)

  imported <- datapond:::import_dataset_transfer_bundle(
    bundle$path,
    target_pond,
    importer$user_id
  )
  record <- as.data.frame(datapond:::get_dataset_record(
    imported$dataset_id,
    target_pond
  ))
  provenance <- datapond:::get_dataset_import_provenance(
    imported$dataset_id,
    target_pond
  )

  expect_identical(record$uuid, "transfer-uuid-001")
  expect_identical(record$metadata_uuid, "11111111-2222-4333-8444-555555555555")
  expect_identical(record$resource_language_code, "zul")
  expect_identical(record$resource_character_encoding, "utf8")
  expect_identical(record$created_by_user_id, importer$user_id)
  expect_match(record$dataset_path, "/src/programme/", fixed = TRUE)
  expect_true(file.exists(file.path(record$data_path, "example.csv")))
  expect_equal(nrow(datapond:::get_dataset_authors(imported$dataset_id, target_pond)), 1L)
  imported_sources <- datapond:::get_dataset_lineage_sources(
    imported$dataset_id, target_pond
  )
  imported_steps <- datapond:::get_dataset_lineage_process_steps(
    imported$dataset_id, target_pond
  )
  imported_lineage <- datapond:::get_dataset_lineage(imported$dataset_id, target_pond)
  expect_identical(imported_lineage$legacy_lineage_migrated_at, migrated_lineage$legacy_lineage_migrated_at)
  expect_identical(imported_lineage$lineage_statement, migrated_lineage$lineage_statement)
  expect_identical(imported_lineage$source_description, "Intentionally removed legacy source")
  expect_identical(imported_sources$title, c("Survey observations", "Study boundary"))
  imported_documents <- datapond:::get_supporting_documents(
    imported$dataset_id, target_pond
  )
  expect_identical(
    imported_sources$supporting_document_id[2], imported_documents$id[1]
  )
  expect_equal(nrow(imported_steps), 1L)
  expect_setequal(
    strsplit(imported_steps$source_ids, ",", fixed = TRUE)[[1]],
    as.character(imported_sources$id)
  )
  expect_identical(
    datapond:::get_dataset_topic_categories(imported$dataset_id, target_pond)$topic_category_code,
    c("biota", "environment")
  )
  expect_identical(
    datapond:::get_dataset_citation_dates(imported$dataset_id, target_pond)$date_type_code,
    c("publication", "revision")
  )
  imported_responsibility <- datapond:::get_metadata_responsibilities(
    imported$dataset_id, target_pond
  )
  expect_identical(imported_responsibility$role_code, "pointOfContact")
  expect_identical(imported_responsibility$person_name, "Alice Editor")
  expect_identical(
    datapond:::get_dataset_alternate_titles(imported$dataset_id, target_pond)$alternate_title,
    c("Transfer short title", "Earlier transfer title")
  )
  expect_identical(
    datapond:::get_dataset_online_resources(imported$dataset_id, target_pond)$resource_url,
    "https://example.test/transfer"
  )
  expect_identical(
    datapond:::get_dataset_distribution_formats(imported$dataset_id, target_pond)$media_type,
    "text/csv"
  )
  expect_identical(
    datapond:::get_dataset_quality_results(imported$dataset_id, target_pond)$measure_identifier,
    "TRANSFER-DQ-01"
  )
  expect_identical(
    datapond:::get_dataset_quality_results(imported$dataset_id, target_pond)$quality_element_code,
    "DQ_CompletenessOmission"
  )
  expect_identical(
    datapond:::get_dataset_temporal_extents(imported$dataset_id, target_pond)$extent_label,
    "Transfer period"
  )
  expect_identical(
    datapond:::get_dataset_release_rights(imported$dataset_id, target_pond)$release_statement,
    "Release after review."
  )
  expect_identical(
    datapond:::get_dataset_vertical_extents(imported$dataset_id, target_pond)$extent_label,
    "Height"
  )
  expect_identical(
    datapond:::get_dataset_feature_catalogue(imported$dataset_id, target_pond)$title,
    "Portable test fields"
  )
  imported_org_id <- imported_responsibility$organisation_id[1]
  expect_identical(
    datapond:::get_party_addresses(target_pond, organisation_id = imported_org_id)$city,
    "Pietermaritzburg"
  )
  expect_identical(
    datapond:::get_keyword_thesauri(target_pond, "keyword_topic")$citation_title,
    "Portable topics"
  )
  target_conn <- datapond:::get_conn(target_pond)
  imported_keywords <- DBI::dbGetQuery(
    target_conn,
    "SELECT keyword_object, keyword_topic FROM entry_tbl WHERE id = ?",
    params = list(imported$dataset_id)
  )
  imported_entry <- DBI::dbGetQuery(target_conn,
    "SELECT * FROM entry_tbl WHERE id = ?", params = list(imported$dataset_id))
  expect_identical(imported_entry$metadata_creation_date, "2016-03-02")
  expect_identical(imported_entry$geographic_extent_name, "KwaZulu-Natal, South Africa")
  for (field in names(dynamic_metadata)) {
    expect_identical(imported_entry[[field]], dynamic_metadata[[field]], info = field)
  }
  DBI::dbDisconnect(target_conn)
  expect_identical(imported_keywords$keyword_object, "portable data")
  expect_true(is.na(imported_keywords$keyword_topic))
  expect_identical(provenance$source_data_pond_id, manifest$source_data_pond_id)
  expect_identical(provenance$source_created_by_user_id, editor$user_id)
  expect_identical(provenance$source_exported_by_user_id, editor$user_id)
  expect_identical(provenance$imported_by_user_id, importer$user_id)

  expect_error(
    datapond:::import_dataset_transfer_bundle(
      bundle$path,
      target_pond,
      importer$user_id
    ),
    "already exists"
  )
})


test_that("dataset transfer UI exposes drag-and-drop import and compression", {
  html <- as.character(datapond:::dataset_transfer_ui())
  expect_match(html, "Pond links", fixed = TRUE)
  expect_match(html, "Connected pond catalogues", fixed = TRUE)
  expect_match(html, "connected_dataset_table", fixed = TRUE)
  expect_match(html, "dataset_bundle_file", fixed = TRUE)
  expect_match(html, "Drag and drop a Data Pond ZIP here", fixed = TRUE)
  expect_match(html, "Compress selected dataset", fixed = TRUE)
  expect_match(html, "Import provenance", fixed = TRUE)
})

test_that("bundle compression settings are validated before export", {
  for (level in list(-1L, 10L, 1.5, NA_real_, Inf, "fast", c(1L, 2L))) {
    withr::local_options(datapond.bundle_compression_level = level)
    expect_error(
      datapond:::create_dataset_transfer_bundle(1L, tempfile("missing-pond-")),
      "must be an integer from 0 to 9", fixed = TRUE
    )
  }
})
