missing_cleanup_fixture <- function(stage = "dev") {
  pond <- tempfile("missing-cleanup-")
  datapond:::ensure_pond_structure(pond)
  datapond:::init_db(pond)
  root <- file.path(pond, stage, "fixture")
  dir.create(file.path(root, "data"), recursive = TRUE)
  dir.create(file.path(root, "docs"))
  dir.create(file.path(root, "data", "present.gdb"))
  writeLines("present", file.path(root, "data", "present.csv"))
  writeLines("present", file.path(root, "docs", "present.txt"))
  conn <- datapond:::get_conn(pond); on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, "INSERT INTO persons_tbl(id,name_first,name_last) VALUES (1,'Cleanup','Tester')")
  DBI::dbExecute(conn, "INSERT INTO user_tbl(id,person_id,user_name,is_active) VALUES (1,1,'cleanup',1)")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(id,uuid,title,version,dataset_path,data_path)
    VALUES (1,'cleanup-fixture','Cleanup','1.0',?,?)", params = list(root, file.path(root, "data")))
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(id,title) VALUES (2,'Other dataset')")
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl(id,dataset_id,data_file_rel_path_full,active,scan_status)
    VALUES (1,1,'removed.csv',1,'present'), (2,1,'present.csv',0,'missing'),
      (3,1,'present.gdb',1,'present'), (4,1,'unselected.csv',0,'missing'), (5,2,'removed.csv',0,'missing')")
  DBI::dbExecute(conn, "INSERT INTO supporting_document_tbl(id,dataset_id,document_rel_path,active)
    VALUES (1,1,'docs/removed.txt',1), (2,1,'docs/present.txt',0), (3,2,'docs/removed.txt',0)")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_source_tbl(dataset_id,title,supporting_document_id,source_order)
    VALUES (1,'Removed source',1,1), (1,'Retained source',2,2)")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_process_step_tbl(id,dataset_id,description) VALUES (1,1,'Retained step')")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_step_source_tbl(dataset_id,process_step_id,source_id) VALUES (1,1,1), (1,1,2)")
  DBI::dbExecute(conn, "INSERT INTO dataset_attribute_tbl(id,dataset_id,data_file_id,data_file_rel_path_full,data_field_name)
    VALUES (1,1,1,'removed.csv','linked'), (2,1,NULL,'removed.csv','legacy'),
      (3,1,2,'present.csv','retained'), (4,2,5,'removed.csv','other dataset')")
  DBI::dbExecute(conn, "INSERT INTO data_file_spatial_metadata_tbl(dataset_id,data_file_id,data_file_rel_path_full,layer_key)
    VALUES (1,1,'removed.csv','linked'), (1,NULL,'removed.csv','legacy'), (1,2,'present.csv',''), (2,5,'removed.csv','')")
  DBI::dbExecute(conn, "INSERT INTO data_profile_run_tbl(dataset_id,data_file_id) VALUES (1,1), (1,2)")
  DBI::dbExecute(conn, "INSERT INTO dataset_quality_result_tbl(dataset_id,data_file_id,attribute_id,measure_identifier,
    measure_type_code,evaluation_method,result_type_code,sort_order)
    VALUES (1,1,NULL,'file','completeness','check','descriptive',1),
      (1,NULL,2,'attribute','completeness','check','descriptive',2),
      (1,2,3,'retained','completeness','check','descriptive',3)")
  DBI::dbExecute(conn, "UPDATE dataset_quality_result_tbl SET explanation='Fixture quality check'")
  DBI::dbExecute(conn, "INSERT INTO dataset_manual_spatial_extent_tbl(dataset_id,data_file_id,extent_scope,
    extent_label,crs_input,bbox_native,bbox_status,xmin,ymin,xmax,ymax,include_in_dataset_bbox)
    VALUES (1,1,'data_file','removed','EPSG:4326','0,0,1,1','valid',0,0,1,1,0),
      (1,NULL,'dataset','retained','EPSG:4326','0,0,1,1','valid',0,0,1,1,0)")
  list(pond = pond, root = root, id = 1L, user = 1L)
}

cleanup_preview <- function(f) datapond:::preview_missing_file_cleanup(f$id, f$pond, f$user)
cleanup_run <- function(f, p, keys = c("data_file_tbl:1", "supporting_document_tbl:1"))
  datapond:::remove_missing_file_metadata(f$id, f$pond, f$user, p, keys)

test_that("cleanup removes selected missing metadata and dependencies from working and transferable records", {
  for (stage in c("src", "dev")) {
    f <- missing_cleanup_fixture(stage); on.exit(unlink(f$pond, recursive = TRUE), add = TRUE)
    p <- cleanup_preview(f)
    expect_setequal(p$rows$key, c("data_file_tbl:1", "data_file_tbl:4", "supporting_document_tbl:1"))
    expect_equal(cleanup_run(f, p), 2L)
    conn <- datapond:::get_conn(f$pond)
    expect_setequal(DBI::dbReadTable(conn, "data_file_tbl")$id, 2:5)
    expect_setequal(DBI::dbReadTable(conn, "supporting_document_tbl")$id, 2:3)
    expect_setequal(DBI::dbReadTable(conn, "dataset_attribute_tbl")$id, 3:4)
    expect_equal(nrow(DBI::dbReadTable(conn, "data_file_spatial_metadata_tbl")), 2)
    expect_equal(nrow(DBI::dbReadTable(conn, "dataset_quality_result_tbl")), 1)
    expect_equal(nrow(DBI::dbReadTable(conn, "data_profile_run_tbl")), 1)
    expect_identical(DBI::dbReadTable(conn, "dataset_lineage_source_tbl")$title, "Retained source")
    expect_identical(DBI::dbReadTable(conn, "dataset_lineage_step_source_tbl")$source_id, 2L)
    expect_identical(DBI::dbReadTable(conn, "dataset_manual_spatial_extent_tbl")$extent_label, "retained")
    expect_equal(DBI::dbReadTable(conn, "dataset_spatial_summary_tbl")$included_source_count, 0)
    expect_equal(nrow(DBI::dbGetQuery(conn, "PRAGMA foreign_key_check")), 0)
    expect_identical(DBI::dbReadTable(conn, "metadata_review_tbl")$metadata_status, "needs_attention")
    # Freeze captures this payload for both its private snapshot and public bundle.
    payload <- datapond:::release_payload(conn, f$id, f$pond, f$user)
    expect_false("removed.csv" %in% payload$scoped$data_file_tbl$data_file_rel_path_full)
    expect_false("docs/removed.txt" %in% payload$scoped$supporting_document_tbl$document_rel_path)
    DBI::dbDisconnect(conn)
    expect_true(file.exists(file.path(f$root, "data", "present.csv")))
    expect_true(dir.exists(file.path(f$root, "data", "present.gdb")))
  }
})

test_that("cleanup rejects stale previews, restored files and unavailable roots", {
  f <- missing_cleanup_fixture(); on.exit(unlink(f$pond, recursive = TRUE))
  p <- cleanup_preview(f)
  expect_error(cleanup_run(f, p, "data_file_tbl:2"), "Select missing")
  expect_error(cleanup_run(f, p, character()), "Select missing")
  writeLines("restored", file.path(f$root, "data", "removed.csv"))
  expect_error(cleanup_run(f, p), "present again")
  unlink(file.path(f$root, "data", "removed.csv"))
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE data_file_tbl SET data_file_notes='new notes' WHERE id=1")
  expect_error(cleanup_run(f, p), "metadata changed")
  expect_error(datapond:::preview_missing_file_cleanup(1L, f$pond, NA_integer_), "active application user")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path=? WHERE id=1", params = list(paste0(f$root, "-offline")))
  expect_error(cleanup_preview(f), "unavailable")
  expect_equal(nrow(DBI::dbReadTable(conn, "data_file_tbl")), 5)
})

test_that("cleanup rolls back all deletions if a dependent update fails", {
  f <- missing_cleanup_fixture(); on.exit(unlink(f$pond, recursive = TRUE))
  p <- cleanup_preview(f)
  local_mocked_bindings(rebuild_dataset_spatial_summary_conn = function(...) stop("fixture failure"), .package = "datapond")
  expect_error(cleanup_run(f, p), "fixture failure")
  expect_identical(cleanup_preview(f), p)
})

test_that("cleanup controls save the selection and invalidate release previews", {
  local_trusted_access_fixture()
  f <- missing_cleanup_fixture(); on.exit(unlink(f$pond, recursive = TRUE))
  state <- shiny::reactiveValues(data_pond = f$pond, current_dataset_id = f$id,
    active_user_id = f$user, refresh_datasets = 0L)
  shiny::testServer(datapond:::missing_file_cleanup_server, args = list(state = state), {
    session$flushReact()
    session$setInputs(preview = 1)
    expect_equal(nrow(preview()$rows), 3)
    session$setInputs(keys = "supporting_document_tbl:1", remove = 1)
    expect_match(result(), "Removed 1")
    expect_null(preview())
    expect_equal(state$refresh_datasets, 1L)
    session$setInputs(preview = 2)
    expect_equal(nrow(preview()$rows), 2)
    state$current_dataset_id <- 2L
    session$flushReact()
    expect_null(preview())
  })
})

test_that("saving the quality editor cannot restore removed file references", {
  local_trusted_access_fixture()
  f <- missing_cleanup_fixture(); on.exit(unlink(f$pond, recursive = TRUE))
  state <- shiny::reactiveValues(data_pond = f$pond, current_dataset_id = f$id,
    active_user_id = f$user, refresh_datasets = 0L, file_metadata_cleanup_tick = 0L)
  shiny::testServer(datapond:::mod_iso_wave2_server, args = list(state = state), {
    session$flushReact()
    load(f$id)
    session$flushReact()
    expect_equal(nrow(quality()), 3)
    extra <- quality()[3, , drop = FALSE]
    extra$measure_identifier <- "Unsaved dataset result"
    extra$data_file_id <- extra$attribute_id <- NA_integer_
    quality(rbind(quality(), extra))
    cleanup_run(f, cleanup_preview(f))
    state$file_metadata_cleanup_tick <- 1L
    session$flushReact()
    expect_setequal(quality()$measure_identifier, c("retained", "Unsaved dataset result"))
    datapond:::replace_dataset_quality_results(f$id, quality(), f$pond, 1L)
    expect_setequal(datapond:::get_dataset_quality_results(f$id, f$pond)$measure_identifier,
      c("retained", "Unsaved dataset result"))
  })
})

test_that("cleanup revokes approval and refuses open review exchanges", {
  f <- missing_cleanup_fixture(); on.exit(unlink(f$pond, recursive = TRUE))
  datapond:::insert_metadata_review(f$id, f$pond, reviewer_id = 1L,
    update_reason = "Fixture approval", metadata_status = "approved")
  cleanup_run(f, cleanup_preview(f))
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  expect_equal(nrow(DBI::dbReadTable(conn, "metadata_approval_tbl")), 0)
  datapond:::review_ensure_schema(conn)
  DBI::dbExecute(conn, "INSERT INTO dataset_review_exchange_tbl(review_id,dataset_id,role,status,
    master_pond_id,dataset_uuid,request_bundle_id,baseline_hash,files_hash,assignee,issued_at)
    VALUES ('fixture',1,'master','issued','pond','uuid','bundle','hash','hash','Reviewer','2026-01-01')")
  expect_error(cleanup_preview(f), "active metadata review")
})

test_that("new frozen bundles and restoration snapshots contain cleaned inventories", {
  f <- missing_cleanup_fixture(); on.exit(unlink(f$pond, recursive = TRUE))
  # Keep this integration focused on the real archive and snapshot pipeline.
  local_mocked_bindings(
    release_metadata_gate = function(dataset_id, base_path, conn, population_profile) list(
      population_profile = population_profile, reviewer = "Fixture", reviewed_at = "2026-10-29",
      metadata_hash = datapond:::metadata_approval_hash(conn, dataset_id)),
    render_metadata_document = function(directory) {
      writeLines("Fixture PDF", file.path(directory, "metadata.pdf"))
      file.path(directory, "metadata.pdf")
    }, .package = "datapond")
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  datapond:::ensure_columns(conn, "entry_tbl", "citation")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET citation='Cleanup fixture citation' WHERE id=1")
  cleanup_run(f, cleanup_preview(f))
  datapond:::save_working_metadata_document(f$id, f$pond, f$user)
  plan <- datapond:::preview_dataset_release(f$id, f$pond, f$user)
  release <- datapond:::create_dataset_release(f$id, f$pond, f$user, "Cleaned missing records", plan)
  paths <- datapond:::verify_dataset_release(release$release_id, f$pond)
  snapshot <- datapond:::release_read_payload(paths$snapshot)
  expect_false("removed.csv" %in% snapshot$scoped$data_file_tbl$data_file_rel_path_full)
  expect_false("docs/removed.txt" %in% snapshot$scoped$supporting_document_tbl$document_rel_path)
  output <- tempfile("cleaned-release-"); dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  utils::unzip(paths$zip, exdir = output)
  databases <- list.files(output, "dataset_metadata.sqlite", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  expect_length(databases, 1L)
  bundled <- DBI::dbConnect(RSQLite::SQLite(), databases[1])
  on.exit(DBI::dbDisconnect(bundled), add = TRUE)
  expect_false("removed.csv" %in% DBI::dbReadTable(bundled, "data_file_tbl")$data_file_rel_path_full)
  expect_false("docs/removed.txt" %in% DBI::dbReadTable(bundled, "supporting_document_tbl")$document_rel_path)
})
