wave3_fixture <- function() {
  pond <- tempfile("iso-wave3-"); dir.create(pond); datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  datapond:::ensure_columns(conn, "entry_tbl", "keyword_topic")
  datapond:::ensure_columns(conn, "entry_tbl", c("abstract", "metadata_creation_date"))
  DBI::dbExecute(conn, "INSERT INTO organisation_tbl
    (organisation_name_full, organisation_code, organisation_department, organisation_address)
    VALUES ('Wave 3 Office', 'W3', 'Ecology unit', 'Legacy address, kept verbatim')")
  organisation_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO persons_tbl (name_first, name_last, email, organisation_id)
    VALUES ('Willow', 'Curator', 'willow@example.test', ?)", params = list(organisation_id))
  person_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO entry_tbl
    (uuid, title, citation_date, keyword_topic, metadata_resource_scope_code)
    VALUES ('wave3-resource', 'Wave 3 fixture', '2026-01-01', 'birds; wetlands', 'service')")
  dataset_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "UPDATE entry_tbl SET abstract = 'Wave 3 fixture abstract.',
    metadata_creation_date = '2025-12-01 10:00:00' WHERE id = ?", params = list(dataset_id))
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl
    (dataset_id, data_file_rel_path_full, file_name, data_file_format, active)
    VALUES (?, 'data/features.gpkg', 'features.gpkg', 'vector', 1)", params = list(dataset_id))
  vector_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl
    (dataset_id, data_file_rel_path_full, file_name, data_file_format, active)
    VALUES (?, 'data/elevation.tif', 'elevation.tif', 'raster', 1)", params = list(dataset_id))
  raster_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  add_attribute <- function(file_id, path, name, field, type) DBI::dbExecute(conn, "INSERT INTO dataset_attribute_tbl
    (dataset_id, data_file_id, data_file_rel_path_full, data_file_name, data_field_name,
     data_field_desc, data_field_units, attribute_type, attribute_order)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1)", params = list(dataset_id, file_id, path, name, field, paste(field, "description"), "m", type))
  add_attribute(vector_id, "data/features.gpkg", "features.gpkg", "species_name", "character")
  add_attribute(raster_id, "data/elevation.tif", "elevation.tif", "elevation_band", "float32")
  DBI::dbExecute(conn, "INSERT INTO keyword_vocab_tbl (field_code, option_value, option_label)
    VALUES ('keyword_topic', 'birds', 'Birds')")
  DBI::dbDisconnect(conn)
  datapond:::init_db(pond)
  list(path = pond, dataset_id = as.integer(dataset_id), person_id = as.integer(person_id),
       organisation_id = as.integer(organisation_id), vector_id = as.integer(vector_id), raster_id = as.integer(raster_id))
}

test_that("Wave 3 migration is idempotent and preserves legacy contact text", {
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::init_db(fixture$path); datapond:::init_db(fixture$path)
  orgs <- datapond:::get_organisations(fixture$path)
  addresses <- datapond:::get_party_addresses(fixture$path, organisation_id = fixture$organisation_id)
  expect_identical(orgs$organisation_address[orgs$id == fixture$organisation_id], "Legacy address, kept verbatim")
  expect_identical(orgs$organisational_unit_name[orgs$id == fixture$organisation_id], "Ecology unit")
  expect_equal(nrow(addresses), 1L)
  expect_identical(addresses$delivery_point, "Legacy address, kept verbatim")
})

test_that("vertical extents support replacement and strict validation", {
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  row <- data.frame(extent_label = "Elevation", minimum_value = 12, maximum_value = 1440,
    unit_symbol = "m", unit_uri = "http://qudt.org/vocab/unit/M", vertical_crs_name = "EGM2008 height",
    vertical_crs_identifier = "EPSG:3855", vertical_crs_uri = "https://epsg.io/3855",
    vertical_crs_type = "geoidal", description = "Terrain elevation")
  datapond:::replace_dataset_vertical_extents(fixture$dataset_id, row, fixture$path, fixture$person_id)
  saved <- datapond:::get_dataset_vertical_extents(fixture$dataset_id, fixture$path)
  expect_equal(saved$minimum_value, 12)
  row$minimum_value <- 2000
  expect_error(datapond:::replace_dataset_vertical_extents(fixture$dataset_id, row, fixture$path), "cannot exceed")
  expect_equal(nrow(datapond:::get_dataset_vertical_extents(fixture$dataset_id, fixture$path)), 1L)
  datapond:::replace_dataset_vertical_extents(fixture$dataset_id, data.frame(), fixture$path)
  expect_equal(nrow(datapond:::get_dataset_vertical_extents(fixture$dataset_id, fixture$path)), 0L)
})

test_that("thesaurus citations attach to their keyword groups", {
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  before <- as.data.frame(datapond:::get_dataset_completion_status(fixture$dataset_id, fixture$path, NULL))
  expect_identical(before$status[before$section == "Keyword thesauri"], "partial")
  id <- datapond:::save_keyword_thesaurus(fixture$path, list(
    field_code = "keyword_topic", citation_title = "Wetland topics", edition = "2026.1",
    edition_date = "2026-02-01", edition_date_type_code = "revision",
    thesaurus_uri = "https://example.test/vocabulary/topics", identifier = "WT",
    responsible_person_id = fixture$person_id, responsible_organisation_id = NA_integer_,
    responsible_role_code = "publisher"))
  expect_true(id > 0L)
  thesaurus <- datapond:::get_keyword_thesauri(fixture$path, "keyword_topic")
  expect_identical(thesaurus$citation_title, "Wetland topics")
  conn <- datapond:::get_conn(fixture$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  expect_identical(DBI::dbGetQuery(conn, "SELECT thesaurus_id FROM keyword_vocab_tbl")$thesaurus_id, id)
  after <- as.data.frame(datapond:::get_dataset_completion_status(fixture$dataset_id, fixture$path, NULL))
  expect_identical(after$status[after$section == "Keyword thesauri"], "complete")
})

test_that("custom keyword phrases export intact without a thesaurus citation", {
  fixture <- wave3_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE))
  datapond:::save_iso_profile_sections(fixture$dataset_id,
    profile = list(metadata_uuid = "33333333-4444-4555-8666-777777777777",
      metadata_resource_scope_code = "service", metadata_language_code = "eng", metadata_character_encoding = "utf8"),
    topic_categories = character(), citation_dates = data.frame(),
    responsibilities = data.frame(person_id = fixture$person_id, organisation_id = fixture$organisation_id,
      role_code = "pointOfContact", is_primary = 1L), primary_publication_date = "2026-01-01",
    base_path = fixture$path, user_person_id = fixture$person_id)
  datapond:::register_pending_keyword_values(list(keyword_object = "Forest, woodland"), fixture$path)
  datapond:::save_keyword_thesaurus(fixture$path, list(
    field_code = "keyword_object", citation_title = "Controlled birds"))
  datapond:::save_dataset_feature_catalogue(fixture$dataset_id,
    list(title = "Keyword export fixture", language_code = "eng"), fixture$path)
  conn <- datapond:::get_conn(fixture$path)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET keyword_object = 'birds; Forest, woodland'")
  DBI::dbDisconnect(conn)
  doc <- datapond:::build_iso_wave3_metadata_xml(fixture$dataset_id, fixture$path)
  ns <- xml2::xml_ns(doc)
  cited <- xml2::xml_find_all(doc, ".//mri:MD_Keywords[mri:thesaurusName]/mri:keyword/gco:CharacterString", ns)
  custom <- xml2::xml_find_all(doc, ".//mri:MD_Keywords[not(mri:thesaurusName)]/mri:keyword/gco:CharacterString", ns)
  expect_identical(xml2::xml_text(cited), "birds")
  expect_identical(xml2::xml_text(custom), "Forest, woodland")
})

test_that("ISO 19110 generation includes vector and tabular fields but excludes raster bands", {
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::save_dataset_feature_catalogue(fixture$dataset_id,
    list(title = "Wave 3 attributes", version = "1.0", version_date = "2026-03-01", language_code = "eng"),
    fixture$path, fixture$person_id)
  feature_rows <- datapond:::dataset_feature_catalogue_attributes(fixture$dataset_id, fixture$path, FALSE)
  coverage_rows <- datapond:::dataset_feature_catalogue_attributes(fixture$dataset_id, fixture$path, TRUE)
  expect_identical(feature_rows$data_field_name, "species_name")
  expect_identical(coverage_rows$data_field_name, "elevation_band")
  xml <- as.character(datapond:::build_iso_19110_feature_catalogue_xml(fixture$dataset_id, fixture$path))
  expect_match(xml, "species_name", fixed = TRUE)
  expect_false(grepl("elevation_band", xml, fixed = TRUE))
})

test_that("Wave 3 metadata composes vertical, thesaurus, catalogue link, and raster content", {
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::save_iso_profile_sections(fixture$dataset_id,
    profile = list(metadata_uuid = "33333333-4444-4555-8666-777777777777",
      metadata_resource_scope_code = "service", metadata_language_code = "eng", metadata_character_encoding = "utf8"),
    topic_categories = character(), citation_dates = data.frame(),
    responsibilities = data.frame(person_id = fixture$person_id, organisation_id = fixture$organisation_id,
      role_code = "pointOfContact", is_primary = 1L), primary_publication_date = "2026-01-01",
    base_path = fixture$path, user_person_id = fixture$person_id)
  datapond:::replace_dataset_vertical_extents(fixture$dataset_id, data.frame(
    extent_label = "Elevation", minimum_value = 12, maximum_value = 1440, unit_symbol = "m",
    unit_uri = NA_character_, vertical_crs_name = "EGM2008", vertical_crs_identifier = "EPSG:3855",
    vertical_crs_uri = "https://epsg.io/3855", vertical_crs_type = "geoidal", description = NA_character_), fixture$path)
  datapond:::save_keyword_thesaurus(fixture$path, list(field_code = "keyword_topic",
    citation_title = "Wetland topics", edition = "1", edition_date = "2026-01-01",
    edition_date_type_code = "revision", thesaurus_uri = "https://example.test/topics",
    responsible_person_id = NA_integer_, responsible_organisation_id = fixture$organisation_id,
    responsible_role_code = "publisher"))
  datapond:::save_dataset_feature_catalogue(fixture$dataset_id,
    list(title = "Wave 3 attributes", language_code = "eng"), fixture$path)
  datapond:::replace_party_addresses(fixture$path, data.frame(
    delivery_point = "1 Conservation Way", city = "Pietermaritzburg",
    administrative_area = "KwaZulu-Natal", postal_code = "3201", country = "South Africa",
    electronic_mail_address = "office@example.test", phone_number = "+27 33 000 0000",
    online_resource = "https://example.test/office", is_primary = 1L),
    organisation_id = fixture$organisation_id)
  text <- as.character(datapond:::build_iso_wave3_metadata_xml(fixture$dataset_id, fixture$path))
  expect_match(text, "EX_VerticalExtent", fixed = TRUE)
  expect_match(text, "Wetland topics", fixed = TRUE)
  expect_match(text, "MD_FeatureCatalogueDescription", fixed = TRUE)
  expect_match(text, "elevation_band", fixed = TRUE)
  expect_match(text, "Pietermaritzburg", fixed = TRUE)
  expect_false(grepl("species_name</gco:CharacterString>.*MD_SampleDimension", text))
  expect_match(text, paste("DataPond crosswalk", datapond:::current_iso_crosswalk_version()), fixed = TRUE)
  document <- xml2::read_xml(text)
  ns <- xml2::xml_ns(document)
  theme_keywords <- xml2::xml_text(xml2::xml_find_all(
    document,
    paste0(
      ".//mri:MD_Keywords[",
      "mri:type/mri:MD_KeywordTypeCode/@codeListValue='theme'",
      "]/mri:keyword/gco:CharacterString"
    ),
    ns
  ))
  expect_setequal(theme_keywords, c("birds", "wetlands"))
  output <- tempfile("wave3-export-"); dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  # This deliberately incomplete legacy candidate has invalid specialist
  # sections. The writer must refuse it before creating either document.
  expect_error(datapond:::write_iso_wave3_documents(fixture$dataset_id, fixture$path, output), "ISO export was not written")
  expect_length(list.files(output), 0L)
})

test_that("normalized addresses and organisational hierarchy remain editable", {
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::insert_organisation(list(full = "Child Office", short = "Child", code = "CHILD",
    department = "Science", address = "Legacy child address", organisational_unit_name = "Biodiversity data",
    parent_organisation_id = fixture$organisation_id, delivery_point = "1 Conservation Way", city = "Pietermaritzburg",
    administrative_area = "KwaZulu-Natal", postal_code = "3201", country = "South Africa",
    electronic_mail_address = "data@example.test"), fixture$path)
  child <- datapond:::get_organisations(fixture$path)
  child <- child[child$organisation_code == "CHILD", ]
  expect_identical(child$parent_organisation_id, fixture$organisation_id)
  expect_identical(child$organisational_unit_name, "Biodiversity data")
  address <- datapond:::get_party_addresses(fixture$path, organisation_id = child$id)
  expect_identical(address$city, "Pietermaritzburg")
  expect_identical(address$delivery_point, "1 Conservation Way")
  expect_error(datapond:::update_organisation(fixture$organisation_id, list(
    full = "Wave 3 Office", short = "W3", code = "W3", department = "Ecology unit",
    address = "Legacy address, kept verbatim", parent_organisation_id = child$id,
    organisational_unit_name = "Ecology unit"), fixture$path), "hierarchy cycle")
})

test_that("Wave 3 UI and staged server expose specialist editing", {
  local_trusted_access_fixture()

  expect_match(as.character(datapond:::mod_vertical_extents_ui("w3")), "Vertical extents", fixed = TRUE)
  expect_match(as.character(datapond:::mod_feature_catalogue_ui("w3")), "Raster band descriptions are handled separately", fixed = TRUE)
  fixture <- wave3_fixture(); on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)
  state <- shiny::reactiveValues(data_pond = fixture$path, current_dataset_id = fixture$dataset_id,
                                 refresh_datasets = 0L, refresh_attributes = 0L)
  shiny::testServer(datapond:::mod_iso_wave3_server, args = list(state = state), {
    session$flushReact(); load(fixture$dataset_id); session$flushReact()
    expect_equal(nrow(vertical()), 0L)
    expect_true(is.list(feature_catalogue()))
    expect_identical(
      feature_catalogue()$title,
      "Wave 3 fixture - feature catalogue"
    )
  })
})
