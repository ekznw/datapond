test_that("release summaries survive working edits and exclude removed ZIPs", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn))
  rows <- data.table::data.table(id = 1:3, version = c("3.0.0", "1.0.0", NA), stage = c("dev", "src", "arc"))
  expect_equal(pond_overview_release_metadata(conn, rows)$release_count, c(0L, 0L, 0L))
  DBI::dbWriteTable(conn, "dataset_release_tbl", data.frame(dataset_id = c(1L, 1L, 1L, 2L),
    version = c("1.0.0", "2.0.0", "0.5.0", "1.0.0"),
    state = c("frozen", "frozen", "removed", "removed"),
    frozen_at = c("2026-01-01", "2026-02-01", "2025-12-01", "2026-01-01")))
  result <- pond_overview_release_metadata(conn, rows)
  expect_equal(result$version, rows$version)
  expect_equal(result$release_count, c(2L, 0L, 0L))
  expect_equal(result$latest_release_version[1], "2.0.0")
  expect_equal(result$release_summary, c("2 frozen releases: 2.0.0, 1.0.0", "No frozen releases", "No frozen releases"))
  expect_equal(filter_pond_overview_data(result, "1.0.0 frozen")$id, 1L)
  html <- as.character(pond_overview_version_summary(as.list(result[1])))
  expect_match(html, "Current version:", fixed = TRUE)
  expect_match(html, "3.0.0", fixed = TRUE)
  expect_match(html, "2 frozen releases: 2.0.0, 1.0.0", fixed = TRUE)
})

test_that("map and inventory search controls share metadata matching and clear together", {
  rows <- data.table::data.table(id = 1:2, title = c("Survey", "Other"),
    organisation_search = c("National River Institute", NA),
    keyword_geo = c("Cathedral Peak", NA), authors = c("First Author, Hidden Coauthor", NA))
  rows[, .search_text := pond_overview_search_index(rows)]
  shiny::testServer(function(input, output, session) {
    query <- pond_overview_search_server(input, session)
    matches <- shiny::reactive(filter_pond_overview_data(rows, query()))
  }, {
    messages <- list()
    session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    session$setInputs(pond_dataset_search = "", pond_inventory_search = "")
    expect_equal(matches()$id, 1:2)
    session$setInputs(pond_inventory_search = "INSTITUTE peak")
    session$elapse(300)
    expect_equal(matches()$id, 1L)
    expect_identical(messages$pond_dataset_search$value, "INSTITUTE peak")
    # Browser echo of a mirrored value must not start another update cycle.
    messages <- list()
    session$setInputs(pond_dataset_search = "INSTITUTE peak")
    expect_length(messages, 0L)
    session$setInputs(pond_dataset_search = "Hidden Coauthor")
    session$elapse(300)
    expect_equal(matches()$id, 1L)
    expect_identical(messages$pond_inventory_search$value, "Hidden Coauthor")
    session$setInputs(pond_inventory_search = "no match")
    session$elapse(300)
    expect_equal(nrow(matches()), 0L)
    session$setInputs(pond_inventory_search = "")
    session$elapse(300)
    expect_equal(matches()$id, 1:2)
    expect_identical(messages$pond_dataset_search$value, "")
  })

  html <- xml2::read_html(as.character(pond_overview_ui()))
  controls <- xml2::xml_find_all(html, "//input[@id='pond_dataset_search' or @id='pond_inventory_search']")
  expect_length(controls, 2L)
  expect_length(unique(xml2::xml_attr(controls, "placeholder")), 1L)
  expect_false(grepl("f", pond_overview_responsive_table_options()$dom, fixed = TRUE))
})

test_that("overview discovery searches assigned metadata in current and connected ponds", {
  root <- tempfile("overview-search-")
  pond <- file.path(root, "pond")
  ensure_pond_structure(pond)
  init_db(pond)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  conn <- get_conn(pond)
  ensure_columns(conn, "entry_tbl", c("purpose", "citation", "keyword_object",
    "keyword_topic", "keyword_discipline", "keyword_geo"))
  DBI::dbWriteTable(conn, "entry_tbl", data.frame(
    id = 1:3, uuid = c("discovery-uuid", "unrelated-uuid", "hidden-uuid"),
    title = c("Survey", "Unrelated", "Hidden"),
    abstract = c("Estuary observations", NA, "Secret observation"),
    purpose = c("Restoration planning", NA, NA),
    citation = c("Published survey reference", NA, NA),
    citation_key = c("survey-reference-2026", NA, NA),
    citation_date = c("2026-10-01", NA, NA), version = c("2.3.4", NA, NA),
    keyword_object = c("survey_code; floodplains", NA, NA),
    keyword_topic = c("legacy habitat", NA, NA),
    keyword_discipline = c("hydrology; geomorphology", NA, NA),
    keyword_geo = c("Cathedral Peak; KwaZulu-Natal", NA, NA),
    folder_key = c("survey-folder", "other", "hidden"),
    dataset_path = file.path(pond, "src", c("survey-folder", "other", "hidden")),
    is_hidden = c(0L, 0L, 1L)), append = TRUE)
  DBI::dbWriteTable(conn, "organisation_tbl", data.frame(
    id = 1:2, organisation_name_short = c("NRI", "Unassigned"),
    organisation_name_full = c("National River Institute", "Unassigned organisation"),
    organisation_department = c("Catchment Assessment", NA),
    organisational_unit_name = c("Field Sciences", NA),
    organisation_code = c("ORG-123", "ORG-999")), append = TRUE)
  DBI::dbExecute(conn, "INSERT INTO dataset_organisation_tbl (dataset_id, organisation_id, role)
    VALUES (1, 1, 'custodian')")
  DBI::dbWriteTable(conn, "keyword_vocab_tbl", data.frame(
    field_code = c("keyword_object", "keyword_object", "keyword_geo"),
    option_value = c("survey_code", "unused_code", "survey_code"),
    option_label = c("Riparian condition", "Unassigned label", "Wrong vocabulary"),
    is_active = c(0L, 1L, 1L)), append = TRUE)
  DBI::dbDisconnect(conn)

  current <- build_pond_overview_data(pond, dataset_overview_schema())
  expect_equal(current$uuid[current$id == 1L], "discovery-uuid")
  expect_equal(current$citation_key[current$id == 1L], "survey-reference-2026")
  local <- read_connected_pond_inventory(pond)
  local[, pond_name := "Local collection"]
  database <- get_db_path(pond)
  before <- tools::md5sum(database)
  cache <- file.path(root, "cache")
  snapshot <- movement_snapshot(pond, file.path(cache, "catalogue-discovery"))
  movement_cache_commit(list(cache = cache, directory = snapshot$directory, remote = "team:pond"), snapshot$manifest)
  remote_database <- get_db_path(movement_cache_current(cache)$directory)
  remote_before <- tools::md5sum(remote_database)
  connected <- build_connected_overview_inventory(local, list(list(name = "Remote collection", cache = cache)))
  expect_length(connected$unavailable, 0L)
  rows <- merge_pond_overview_inventory(current, connected$data, "Current collection", pond)
  expect_equal(nrow(rows), 6L)
  # Search must use the cached index, including after local/remote projection.
  local_mocked_bindings(pond_overview_search_index = function(...) stop("Unexpected reindex"))
  terms <- c("NATIONAL RIVER INSTITUTE", "NRI", "Catchment Assessment", "Field Sciences", "ORG-123",
    "custodian", "survey_code", "Riparian condition", "floodplains", "legacy habitat",
    "hydrology", "geomorphology", "Cathedral Peak", "KwaZulu-Natal", "Estuary observations",
    "Restoration planning", "Published survey reference", "2026-10-01", "2.3.4",
    "discovery-uuid", "survey-reference-2026", "survey-folder", "institute geomorphology")
  for (term in terms) {
    matches <- filter_pond_overview_data(rows, term)
    expect_equal(nrow(matches), 3L, info = term)
    expect_true(all(matches$uuid == "discovery-uuid"), info = term)
  }
  for (term in c("Unassigned organisation", "Unassigned label", "Wrong vocabulary", "Secret observation")) {
    expect_equal(nrow(filter_pond_overview_data(rows, term)), 0L, info = term)
  }
  expect_identical(tools::md5sum(database), before)
  expect_identical(tools::md5sum(remote_database), remote_before)
})

test_that("overview discovery tolerates older catalogues and missing metadata", {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbWriteTable(conn, "entry_tbl", data.frame(id = 1:2, title = c("Old survey", NA),
    abstract = c("Historical observations", NA)))
  rows <- pond_overview_discovery_metadata(conn, data.table::data.table(id = 2:1, title = c(NA, "Old survey")))
  expect_equal(rows$id, 2:1)
  expect_true(all(is.na(rows$organisation_search)))
  expect_equal(filter_pond_overview_data(rows, "old survey")$id, 1L)
  expect_equal(filter_pond_overview_data(rows, "Historical observations")$id, 1L)
  expect_equal(nrow(filter_pond_overview_data(rows, "missing")), 0L)
  expect_equal(nrow(filter_pond_overview_data(rows, "  ")), 2L)
  expect_equal(nrow(pond_overview_discovery_metadata(conn, rows[0])), 0L)
  DBI::dbWriteTable(conn, "organisation_tbl", data.frame(id = 1L,
    organisation_name_full = "Legacy institute"))
  DBI::dbWriteTable(conn, "dataset_organisation_tbl", data.frame(dataset_id = 1L, organisation_id = 1L))
  rows <- pond_overview_discovery_metadata(conn, rows)
  expect_equal(filter_pond_overview_data(rows, "Legacy institute")$id, 1L)
})
