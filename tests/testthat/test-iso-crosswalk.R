test_that("the versioned ISO crosswalk is complete and internally consistent", {
  skip_if_not_installed("yaml")

  root <- system.file("extdata", "iso_crosswalk", package = "datapond")
  expect_true(dir.exists(root))

  current <- trimws(readLines(file.path(root, "CURRENT"), warn = FALSE)[1])
  version_dir <- file.path(root, current)
  manifest <- yaml::read_yaml(file.path(version_dir, "manifest.yml"))
  crosswalk <- yaml::read_yaml(file.path(version_dir, "crosswalk.yml"))
  gaps <- yaml::read_yaml(file.path(version_dir, "profile-gaps.yml"))

  expect_identical(paste0("v", manifest$crosswalk$version), current)
  expect_identical(manifest$crosswalk$status, "implementation-candidate")
  expect_gte(length(crosswalk$mappings), 70L)
  expect_gte(length(gaps$gaps), 1L)
  closed_wave_one_gaps <- c(
    "GAP-001", "GAP-002", "GAP-003", "GAP-004", "GAP-005",
    "GAP-008", "GAP-013"
  )
  expect_false(any(closed_wave_one_gaps %in% vapply(
    gaps$gaps, `[[`, character(1), "id"
  )))
  closed_wave_two_gaps <- c(
    "GAP-006", "GAP-007", "GAP-009", "GAP-011", "GAP-012", "GAP-017"
  )
  expect_false(any(closed_wave_two_gaps %in% vapply(
    gaps$gaps, `[[`, character(1), "id"
  )))
  closed_wave_three_gaps <- c("GAP-010", "GAP-014", "GAP-015", "GAP-016")
  expect_false(any(closed_wave_three_gaps %in% vapply(
    gaps$gaps, `[[`, character(1), "id"
  )))

  required <- unlist(manifest$mapping_contract$required_keys, use.names = FALSE)
  allowed_types <- unlist(
    manifest$mapping_contract$allowed_mapping_types,
    use.names = FALSE
  )
  allowed_statuses <- unlist(
    manifest$mapping_contract$allowed_statuses,
    use.names = FALSE
  )

  ids <- vapply(crosswalk$mappings, `[[`, character(1), "id")
  expect_identical(anyDuplicated(ids), 0L)

  for (mapping in crosswalk$mappings) {
    expect_true(all(required %in% names(mapping)), info = mapping$id)
    expect_true(mapping$mapping %in% allowed_types, info = mapping$id)
    expect_true(mapping$status %in% allowed_statuses, info = mapping$id)
    expect_true(nzchar(mapping$target), info = mapping$id)
  }

  gap_ids <- vapply(gaps$gaps, `[[`, character(1), "id")
  expect_identical(anyDuplicated(gap_ids), 0L)
})

test_that("every concrete crosswalk source exists in the effective database model", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("readxl")

  root <- system.file("extdata", "iso_crosswalk", package = "datapond")
  current <- trimws(readLines(file.path(root, "CURRENT"), warn = FALSE)[1])
  crosswalk <- yaml::read_yaml(
    file.path(root, current, "crosswalk.yml")
  )
  manifest <- yaml::read_yaml(
    file.path(root, current, "manifest.yml")
  )

  pond <- tempfile("iso-crosswalk-model-")
  dir.create(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  tables <- DBI::dbListTables(conn)
  expect_setequal(
    names(manifest$source_policy$table_defaults),
    tables
  )
  stored_sources <- unlist(lapply(tables, function(table) {
    fields <- DBI::dbGetQuery(
      conn,
      paste0("PRAGMA table_info(\"", table, "\")")
    )$name
    paste0(table, ".", fields)
  }), use.names = FALSE)

  schema <- readxl::read_excel(
    system.file("extdata", "metadata_schema.xlsx", package = "datapond"),
    sheet = "metadata_schema",
    skip = 5
  )
  legacy_entry_sources <- paste0(
    "entry_tbl.",
    schema$field_code[
      schema$tbl == "entry_tbl" &
        schema$in_app == "T" &
        !is.na(schema$field_code)
    ]
  )
  app_managed_entry_sources <- paste0(
    "entry_tbl.",
    datapond:::data_access_entry_fields()
  )
  current_schema <- data.table::rbindlist(datapond:::dataset_overview_schema()[["Dataset overview"]])
  dynamic_entry_sources <- paste0("entry_tbl.", current_schema$field)
  effective_sources <- unique(c(stored_sources, dynamic_entry_sources, legacy_entry_sources))
  effective_sources <- unique(c(effective_sources, app_managed_entry_sources))

  declared_sources <- unique(unlist(lapply(crosswalk$mappings, function(x) {
    unlist(x$source, use.names = FALSE)
  }), use.names = FALSE))
  concrete_sources <- declared_sources[
    grepl("^[A-Za-z][A-Za-z0-9_]*\\.[A-Za-z][A-Za-z0-9_]*$", declared_sources)
  ]

  expect_setequal(
    setdiff(concrete_sources, effective_sources),
    character()
  )
  expect_setequal(
    setdiff(dynamic_entry_sources, concrete_sources),
    character()
  )
  expect_setequal(
    setdiff(app_managed_entry_sources, concrete_sources),
    character()
  )

  policy <- datapond:::load_current_iso_crosswalk()$capture_policy
  expect_identical(policy$version, 1L)
  mapping_ids <- vapply(crosswalk$mappings, `[[`, character(1), "id")
  for (group in policy$groups) {
    expect_true(all(unlist(group$canonical) %in% effective_sources), info = group$id)
    expect_true(all(unlist(group$compatibility) %in% effective_sources), info = group$id)
    expect_true(all(unlist(group$mappings) %in% mapping_ids), info = group$id)
    expect_length(intersect(unlist(group$canonical), unlist(group$compatibility)), 0L)
  }
})
