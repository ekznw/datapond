# Tiny real release archives isolate retention from metadata-approval policy.
removal_fixture <- function() {
  root <- tempfile("release-removal-"); dir.create(root)
  pond <- file.path(root, "pond"); dir.create(pond)
  datapond:::ensure_pond_structure(pond); datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond); on.exit(DBI::dbDisconnect(conn))
  datapond:::ensure_columns(conn, "entry_tbl", c("citation", "abstract", "purpose"))
  DBI::dbExecute(conn, "INSERT INTO persons_tbl(name_first,name_last) VALUES ('Retention','Tester')")
  DBI::dbExecute(conn, "INSERT INTO user_tbl(person_id,user_name,is_active,is_admin) VALUES (1,'retention-tester',1,1)")
  folder <- file.path(pond, "src", "retention__test"); dir.create(file.path(folder, "data"), recursive = TRUE)
  writeLines("test raster bytes", file.path(folder, "data", "example.txt"))
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(uuid,title,version,citation,abstract,purpose,dataset_path,data_path,data_pond_id)
    VALUES ('retention-test','Retention test','1.0','Test citation','Fixture','Exercise removal',?,?,?)",
    params = list(folder, file.path(folder, "data"), datapond:::ensure_data_pond_id(pond, conn = conn)))
  f <- list(root = root, pond = pond, folder = folder, id = 1L, user = 1L)
  f$old <- removal_freeze(f, "1.0")
  f$new <- removal_freeze(f, "2.0")
  f
}

removal_freeze <- function(f, version) {
  testthat::local_mocked_bindings(release_metadata_gate = function(dataset_id, base_path, conn, population_profile) list(
    population_profile = population_profile, reviewer = "Fixture reviewer", reviewed_at = "2026-10-29",
    metadata_hash = datapond:::metadata_approval_hash(conn, dataset_id)),
    render_metadata_document = function(directory) {
      writeLines("Fixture PDF", file.path(directory, "metadata.pdf"))
      file.path(directory, "metadata.pdf")
    }, .package = "datapond")
  conn <- datapond:::get_conn(f$pond)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET version=? WHERE id=?", params = list(version, f$id))
  DBI::dbDisconnect(conn)
  datapond:::save_working_metadata_document(f$id, f$pond, f$user)
  plan <- datapond:::preview_dataset_release(f$id, f$pond, f$user)
  datapond:::create_dataset_release(f$id, f$pond, f$user, "Fixture freeze", plan)
}

removal_plan <- function(f) datapond:::preview_dataset_release_removal(f$old$release_id, f$pond, f$user)
removal_run <- function(f, plan) datapond:::remove_dataset_release(f$old$release_id, f$pond, f$user, "Superseded baseline", plan)

test_that("removing an old ZIP frees bytes while preserving audit, metadata and latest release", {
  f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  paths <- datapond:::verify_dataset_release(f$old$release_id, f$pond)
  before <- datapond:::release_inventory(f$folder)
  plan <- removal_plan(f)
  expect_equal(plan$bytes, file.info(paths$zip)$size)
  expect_identical(plan$retained_release_id, f$new$release_id)
  result <- removal_run(f, plan)
  expect_equal(result$bytes_removed, plan$bytes)
  expect_false(file.exists(paths$zip))
  expect_true(all(file.exists(c(paths$snapshot, paths$manifest, paths$source_manifest))))
  expect_identical(datapond:::release_inventory(f$folder), before)
  expect_silent(datapond:::verify_dataset_release(f$new$release_id, f$pond))
  history <- datapond:::dataset_release_history(f$id, f$pond)
  expect_equal(nrow(history), 2)
  expect_identical(history$state[history$release_id == f$old$release_id], "removed")
  expect_identical(jsonlite::read_json(file.path(paths$root, "release.json"))$state, "removed")
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  event <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_event_tbl WHERE action='remove_release_zip'")
  expect_equal(nrow(event), 1); expect_equal(event$user_id, f$user)
  expect_identical(event$note, "Superseded baseline")
  expect_error(datapond:::verify_dataset_release(f$old$release_id, f$pond), "removed")
  expect_error(datapond:::restore_dataset_release(f$old$release_id, f$pond, f$user, "Restore"), "removed")
  expect_error(removal_run(f, plan), "removed")
  expect_error(removal_freeze(f, "1.0"), "already frozen")
  expect_equal(nrow(DBI::dbGetQuery(conn, "PRAGMA foreign_key_check")), 0)
})

test_that("retention protects newest releases, current archive pointers and checkpoints", {
  f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  expect_error(datapond:::preview_dataset_release_removal(f$new$release_id, f$pond, f$user), "newest")
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path=?,lifecycle_status='archived' WHERE id=?",
    params = list(file.path(f$pond, f$old$relative_path), f$id))
  expect_error(removal_plan(f), "current archived dataset")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path=?,lifecycle_status='draft' WHERE id=?", params = list(f$folder, f$id))
  restored <- datapond:::restore_dataset_release(f$old$release_id, f$pond, f$user, "Checkpoint test")
  expect_true(file.exists(file.path(restored$checkpoint, "prior-working.zip")))
  expect_error(removal_plan(f), "checkpoint depends")
  expect_silent(datapond:::verify_dataset_release(f$old$release_id, f$pond))
})

test_that("removal requires an active administrator, a reason and the same preview", {
  f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  plan <- removal_plan(f)
  expect_error(datapond:::remove_dataset_release(f$old$release_id, f$pond, f$user, "", plan), "Record why")
  expect_error(datapond:::remove_dataset_release(f$old$release_id, f$pond, f$user, "Reason"), "preview changed")
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE user_tbl SET is_admin=0 WHERE id=1")
  expect_error(removal_run(f, plan), "administrator")
  DBI::dbExecute(conn, "UPDATE user_tbl SET is_admin=1,is_active=0 WHERE id=1")
  expect_error(removal_plan(f), "active application user")
  DBI::dbExecute(conn, "UPDATE user_tbl SET is_active=1 WHERE id=1")
  removal_freeze(f, "3.0")
  expect_error(removal_run(f, plan), "preview changed")
  expect_true(file.exists(plan$source))
})

test_that("corrupt retained archives and uncommitted removal cannot consume an old ZIP", {
  f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  plan <- removal_plan(f)
  local({
    testthat::local_mocked_bindings(release_event = function(...) stop("Interrupted transaction"), .package = "datapond")
    expect_error(removal_run(f, plan), "Interrupted transaction")
  })
  expect_true(file.exists(plan$source))
  expect_error(removal_plan(f), "needs recovery")
  expect_match(datapond:::recover_dataset_release_operations(f$id, f$pond, f$user), "Cancelled uncommitted")
  expect_silent(datapond:::verify_dataset_release(f$old$release_id, f$pond))
  cat("corrupt", file = file.path(f$pond, f$new$relative_path, "dataset.zip"), append = TRUE)
  expect_error(removal_plan(f), "integrity check")
  expect_true(file.exists(plan$source))
})

test_that("committed removal recovers both before and after unlink", {
  for (after in c(FALSE, TRUE)) {
    f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
    plan <- removal_plan(f)
    local({
      testthat::local_mocked_bindings(release_unlink_zip = function(path) {
        if (after) unlink(path)
        stop("Interrupted unlink")
      }, .package = "datapond")
      expect_error(removal_run(f, plan), "Interrupted unlink")
    })
    expect_identical(file.exists(plan$source), !after)
    expect_error(removal_plan(f), "needs recovery")
    expect_match(datapond:::recover_dataset_release_operations(f$id, f$pond, f$user), "Completed release ZIP removal")
    expect_false(file.exists(plan$source))
    expect_match(datapond:::recover_dataset_release_operations(f$id, f$pond, f$user), "No interrupted")
    expect_silent(datapond:::verify_dataset_release(f$new$release_id, f$pond))
  }
})

test_that("release removal refuses aliases and redirected release directories", {
  skip_on_os("windows")
  f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  zip <- file.path(f$pond, f$old$relative_path, "dataset.zip")
  expect_true(file.rename(zip, paste0(zip, ".saved")))
  expect_true(file.symlink(paste0(zip, ".saved"), zip))
  expect_error(removal_plan(f), "symbolic links")
  unlink(zip); expect_true(file.rename(paste0(zip, ".saved"), zip))
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET relative_path=? WHERE release_id=?",
    params = list(paste0(f$new$relative_path, "/."), f$old$release_id))
  expect_error(removal_plan(f), "registered arc/releases")
  expect_true(file.exists(zip))
})

test_that("removal UI requires explicit confirmation and invalidates changed selection", {
  local_trusted_access_fixture()
  f <- removal_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  state <- shiny::reactiveValues(data_pond = f$pond, current_dataset_id = f$id,
    active_user_id = f$user, refresh_datasets = 0L)
  shiny::testServer(datapond:::mod_dataset_releases_server, args = list(state = state), {
    session$flushReact()
    old_index <- match(f$old$release_id, history()$release_id)
    new_index <- match(f$new$release_id, history()$release_id)
    session$setInputs(history_rows_selected = old_index, notes = "Superseded")
    session$setInputs(preview_removal = 1)
    expect_false(is.null(removal()))
    session$setInputs(remove_release = 1)
    expect_match(result(), "Confirm permanent removal")
    expect_true(file.exists(removal()$preview$source))
    session$setInputs(history_rows_selected = new_index)
    expect_null(removal())
    session$setInputs(confirm_removal = TRUE, remove_release = 2)
    expect_match(result(), "Preview the selected release")
    session$setInputs(history_rows_selected = old_index)
    session$setInputs(preview_removal = 2)
    session$setInputs(confirm_removal = TRUE, remove_release = 3)
    expect_match(result(), "reclaimed")
    expect_null(removal())
    session$setInputs(history_rows_selected = match(f$old$release_id, history()$release_id))
    link <- xml2::xml_find_first(xml2::read_html(output$download_control$html), "//a")
    expect_identical(xml2::xml_attr(link, "aria-disabled"), "true")
  })
})
