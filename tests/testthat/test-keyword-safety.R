keyword_safety_fixture <- function() {
  pond <- tempfile("keyword-safety-")
  datapond:::init_db(pond)
  seed <- data.frame(field_code = "keyword_object", option_value = c("land_use", "fire"),
    option_label = c("Land Use", "Fire"), sort_order = 1:2)
  datapond:::merge_keyword_vocab_seed(seed, pond)
  list(pond = pond, seed = seed)
}

test_that("add-only imports preserve edits and deactivation", {
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  conn <- datapond:::get_conn(f$pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "UPDATE keyword_vocab_tbl SET is_active = 0, option_label = 'Local label'")
  before <- DBI::dbReadTable(conn, "keyword_vocab_tbl")
  result <- datapond:::merge_keyword_vocab_seed(f$seed, f$pond)
  expect_equal(result$updated, 0)
  expect_equal(DBI::dbReadTable(conn, "keyword_vocab_tbl"), before)
})

test_that("invalid and failed resets leave the entire vocabulary untouched", {
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  conn <- datapond:::get_conn(f$pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  before <- DBI::dbReadTable(conn, "keyword_vocab_tbl")
  expect_error(datapond:::reset_keyword_vocab_seed(data.frame(bad = 1), f$pond), "missing columns")
  expect_error(datapond:::reset_keyword_vocab_seed(f$seed[0, ], f$pond), "at least one")
  expect_equal(DBI::dbReadTable(conn, "keyword_vocab_tbl"), before)
  DBI::dbExecute(conn, paste(
    "CREATE TRIGGER fail_keyword_deactivation BEFORE UPDATE ON keyword_vocab_tbl",
    "WHEN NEW.is_active = 0 BEGIN SELECT RAISE(ABORT, 'injected failure'); END"
  ))
  changed <- f$seed[1, ]
  changed$option_label <- "Updated label"
  expect_error(datapond:::reset_keyword_vocab_seed(changed, f$pond), "injected failure")
  expect_equal(DBI::dbReadTable(conn, "keyword_vocab_tbl"), before)
})

test_that("previews match applied changes and reject stale snapshots", {
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  conn <- datapond:::get_conn(f$pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  datapond:::register_pending_keyword_values(list(keyword_object = "Custom, phrase"), f$pond)
  seed <- f$seed[1, ]
  seed$option_label <- "Land use updated"
  preview <- datapond:::preview_keyword_vocab_seed(seed, f$pond, "reset")
  expect_setequal(preview$plan$action, c("Update / reactivate", "Deactivate"))
  changed_seed <- seed
  changed_seed$option_label <- "Changed again"
  expect_error(datapond:::apply_keyword_vocab_seed(changed_seed, f$pond,
    mode = "reset", preview = preview), "Preview the changes again")
  result <- datapond:::apply_keyword_vocab_seed(seed, f$pond, mode = "reset", preview = preview)
  expect_equal(result$updated, 1)
  expect_equal(result$deactivated, 1)
  custom <- DBI::dbGetQuery(conn, "SELECT * FROM keyword_vocab_tbl WHERE source = 'dataset_custom'")
  expect_equal(custom$is_active, 1L)
  expect_equal(custom$approval_status, "pending")
  expect_error(datapond:::apply_keyword_vocab_seed(seed, f$pond,
    mode = "reset", preview = preview), "Preview the changes again")
})

test_that("CSV validation rejects ambiguous or unusable terms", {
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  check <- function(column, value, pattern) {
    seed <- f$seed
    seed[[column]][2] <- value
    expect_error(datapond:::prepare_keyword_vocab_seed(seed), pattern)
  }
  check("field_code", "unknown", "unsupported")
  check("option_value", "", "blank")
  check("option_value", "LAND_USE", "duplicate stored")
  check("option_label", "LAND USE", "duplicate display")
  check("option_value", "two; terms", "semicolons")
  check("sort_order", 1.5, "whole numbers")
  check("sort_order", NA, "whole numbers")
})

test_that("custom punctuation, order and label matching survive saving and hydration", {
  choices <- c("Land Use" = "land_use", "Fire" = "fire")
  input <- c("  Land Use  ", "Forest, woodland", "FIRE", "fire", "Other; Last")
  canonical <- datapond:::normalise_keyword_values(input, choices)
  expect_identical(canonical, c("land_use", "Forest, woodland", "fire", "Other", "Last"))
  stored <- paste(canonical, collapse = "; ")
  hydrated <- datapond:::merge_keyword_choices(choices, datapond:::split_keyword_values(stored))
  expect_identical(hydrated$selected, canonical)
  expect_identical(datapond:::extract_primary_keyword("Forest, woodland; Fire"), "forestwoodland")
  expect_identical(datapond:::first_folder_keyword("Forest, woodland; Fire"),
    datapond:::first_folder_keyword(c("Forest, woodland", "Fire")))
  # Generic multi-value fields keep their former comma semantics.
  expect_identical(datapond:::split_stored_multi_value("a,b"), c("a", "b"))
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  expect_equal(datapond:::register_pending_keyword_values(
    list(keyword_object = stored), f$pond), 3L)
  expect_equal(datapond:::register_pending_keyword_values(
    list(keyword_object = c("Land Use", "FOREST, WOODLAND")), f$pond), 0L)
  conn <- datapond:::get_conn(f$pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  datapond:::ensure_columns(conn, "entry_tbl", "keyword_object")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (uuid, keyword_object) VALUES ('round-trip', ?)", params = list(stored))
  DBI::dbDisconnect(conn)
  datapond:::init_db(f$pond)
  conn <- datapond:::get_conn(f$pond)
  reloaded <- DBI::dbGetQuery(conn, "SELECT keyword_object FROM entry_tbl")$keyword_object
  expect_identical(datapond:::split_keyword_values(reloaded), canonical)
})

keyword_safety_admin <- function(pond) {
  person <- datapond:::insert_person(list(first = "Keyword", last = "Reviewer", org_id = NA_integer_), pond)
  datapond:::insert_user(person, pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, "UPDATE user_tbl SET is_admin = 1")
  DBI::dbGetQuery(conn, "SELECT id FROM user_tbl")$id[1]
}

test_that("used term identities are protected and deactivation survives reset", {
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  user <- keyword_safety_admin(f$pond)
  conn <- datapond:::get_conn(f$pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  datapond:::ensure_columns(conn, "entry_tbl", "keyword_object")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (uuid, keyword_object) VALUES ('used', 'land_use; Forest, woodland')")
  vocabulary <- datapond:::get_keyword_vocab_admin(f$pond)
  id <- vocabulary$id[vocabulary$option_value == "land_use"]
  expect_equal(vocabulary$dataset_count[vocabulary$id == id], 1L)
  expect_error(datapond:::save_keyword_vocab_term(f$pond, user,
    "keyword_object", "LAND_USE"), "already exists")
  expect_error(datapond:::save_keyword_vocab_term(f$pond, user,
    "keyword_object", "missing", keyword_id = -1L), "no longer exists")
  expect_error(datapond:::save_keyword_vocab_term(f$pond, user,
    "keyword_object", "renamed", keyword_id = id), "used by datasets")
  expect_error(datapond:::save_keyword_vocab_term(f$pond, user,
    "keyword_geo", "land_use", keyword_id = id), "used by datasets")
  datapond:::save_keyword_vocab_term(f$pond, user, "keyword_object", "land_use", "Local label", keyword_id = id)
  datapond:::set_keyword_vocab_active(id, FALSE, user, f$pond)
  datapond:::reset_keyword_vocab_seed(f$seed, f$pond)
  row <- DBI::dbGetQuery(conn, "SELECT * FROM keyword_vocab_tbl WHERE id = ?", params = list(id))
  expect_equal(row$is_active, 0L)
  expect_equal(row$option_label, "Local label")
  expect_equal(row$source, "admin_override")
  expect_equal(DBI::dbGetQuery(conn, "SELECT keyword_object FROM entry_tbl")$keyword_object,
    "land_use; Forest, woodland")
})

test_that("CSV preview exposes label collisions without writing any changes", {
  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  datapond:::register_pending_keyword_values(list(keyword_object = "New label"), f$pond)
  extra <- data.frame(field_code = "keyword_object", option_value = "new_label", option_label = "New label", sort_order = 3L)
  seed <- rbind(f$seed, extra)
  preview <- datapond:::preview_keyword_vocab_seed(seed, f$pond)
  expect_true(any(startsWith(preview$plan$action, "Conflict")))
  expect_error(datapond:::apply_keyword_vocab_seed(seed, f$pond, preview = preview), "Resolve the conflicts")
  conn <- datapond:::get_conn(f$pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  expect_equal(DBI::dbReadTable(conn, "keyword_vocab_tbl"), preview$existing)
})

test_that("keyword administration keeps filtered row selection and CSV confirmation safe", {
  local_trusted_access_fixture()

  f <- keyword_safety_fixture()
  on.exit(unlink(f$pond, recursive = TRUE))
  user <- keyword_safety_admin(f$pond)
  datapond:::register_pending_keyword_values(list(keyword_object = "Forest, woodland"), f$pond, user)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(f$seed, path, row.names = FALSE)
  state <- shiny::reactiveValues(data_pond = f$pond, active_user_id = user,
    refresh_person = 0L, refresh_org = 0L, refresh_vocab = 0L)
  shiny::testServer(datapond:::mod_keyword_admin_server, args = list(state = state, vocab_path = path), {
    session$setInputs(field_filter = "", status_filter = "pending", active_only = FALSE,
      edit_field = "keyword_object", seed_mode = "add")
    expect_equal(nrow(filtered_vocab()), 1L)
    session$setInputs(vocab_table_rows_selected = 1L)
    custom_id <- selected_id()
    expect_equal(vocab_data()$option_value[vocab_data()$id == custom_id], "Forest, woodland")
    session$setInputs(option_value = "Forest, woodland", option_label = "Forest, woodland",
      sort_order = 100L, approval_status = "approved", is_active = TRUE, save_term = 1L)
    expect_equal(nrow(filtered_vocab()), 0L)
    expect_equal(vocab_data()$approval_status[vocab_data()$id == custom_id], "approved")
    session$setInputs(new_term = 1L)
    expect_null(selected_id())
    session$setInputs(seed_mode = "reset", preview_seed = 1L)
    expect_equal(seed_preview()$mode, "reset")
    snapshot <- seed_preview()$existing
    session$setInputs(confirm_seed = FALSE, apply_seed = 1L)
    expect_equal(seed_preview()$existing, snapshot)
    session$setInputs(confirm_seed = TRUE, apply_seed = 2L)
    expect_null(seed_preview())
    expect_true(grepl("CSV vocabulary maintenance", as.character(output$admin_body$html), fixed = TRUE))
  })
})
