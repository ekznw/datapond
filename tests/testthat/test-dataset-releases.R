release_fixture <- function(prepare = TRUE) {
  root <- tempfile("release-test-"); dir.create(root)
  pond <- file.path(root, "pond"); dir.create(pond)
  datapond:::ensure_pond_structure(pond); datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond); on.exit(DBI::dbDisconnect(conn))
  datapond:::ensure_columns(conn, "entry_tbl", c("citation", "purpose", "abstract"))
  DBI::dbExecute(conn, "INSERT INTO persons_tbl(name_first,name_last) VALUES ('Release','Tester')")
  person <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO user_tbl(person_id,user_name,is_active,is_admin) VALUES (?,'release-tester',1,1)", params=list(person))
  user <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  folder <- file.path(pond, "dev", "example__release-fixture-uuid"); dir.create(file.path(folder, "data"), recursive=TRUE)
  writeLines(c("id,value", "1,20"), file.path(folder, "data", "measurements.csv"))
  writeLines(c("---", "title: Development", "---", "```{r}", "stop('do not execute')", "```"), file.path(folder, "data_dev_test.qmd"))
  datapond:::write_dataset_marker(folder, "release-fixture-uuid", datapond:::ensure_data_pond_id(pond, conn=conn))
  DBI::dbExecute(conn, "INSERT INTO entry_tbl(uuid,title,version,citation,abstract,purpose,dataset_path,data_path,data_pond_id,lifecycle_status,folder_locked)
    VALUES ('release-fixture-uuid','Release example','1.0.0','Example (2026). Release example.','Synthetic lifecycle fixture.','Test frozen versions.',?,?,?,'draft',0)",
    params=list(folder,file.path(folder,"data"),datapond:::ensure_data_pond_id(pond,conn=conn)))
  id <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn,"INSERT INTO data_file_tbl(dataset_id,data_file_rel_path_full,file_name,data_file_format,active) VALUES (?,'measurements.csv','measurements.csv','tabular',1)",params=list(id))
  file_id <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn,"INSERT INTO dataset_attribute_tbl(dataset_id,data_file_id,data_field_name,data_field_desc) VALUES (?,?,'value','Measured value')",params=list(id,file_id))
  f <- list(root=root,pond=pond,id=as.integer(id),user=as.integer(user),folder=folder)
  if (prepare) release_prepare(f)
  f
}

release_prepare <- function(f, download = NULL) {
  testthat::local_mocked_bindings(render_metadata_document = function(directory) {
    writeLines("Prepared PDF fixture", file.path(directory, "metadata.pdf"))
    file.path(directory, "metadata.pdf")
  }, .package = "datapond")
  datapond:::save_working_metadata_document(f$id, f$pond, f$user, download = download)
}

release_test_renderer <- function(.env=parent.frame()) {
  force(.env)
  testthat::local_mocked_bindings(render_metadata_document=function(...) stop("Freezing must not render a PDF"),
    .package="datapond", .env=.env)
}

release_freeze <- function(f, close=FALSE) {
  p <- datapond:::preview_dataset_release(f$id,f$pond,f$user)
  datapond:::create_dataset_release(f$id,f$pond,f$user,"Verified test version",p,close_working=close)
}

test_that("frozen releases preserve working data and use one unpacked consumer cache", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  before <- datapond:::release_inventory(f$folder)
  row <- release_freeze(f)
  expect_identical(datapond:::release_inventory(f$folder),before)
  expect_equal(nrow(datapond:::dataset_release_history(f$id,f$pond)),1)
  expect_silent(datapond:::verify_dataset_release(row$release_id,f$pond))
  expect_error(release_freeze(f),"already frozen")
  cache <- datapond:::cache_dataset_release(row$release_id,f$pond,file.path(f$root,"cache"))
  expect_true(file.exists(file.path(cache,"release","dataset","data","measurements.csv")))
  expect_true(file.exists(file.path(cache,"receipt.json")))
  expect_false(file.exists(file.path(cache,"download.zip")))
  expect_silent(datapond:::verify_cached_dataset_release(cache))
  expect_error(datapond:::cache_dataset_release(row$release_id,f$pond,file.path(f$root,"cache")),"already cached")
  cache2 <- datapond:::cache_dataset_release(row$release_id,f$pond,file.path(f$root,"keep-cache"),keep_zip=TRUE)
  expect_true(file.exists(file.path(cache2,"download.zip")))
  expect_silent(datapond:::verify_cached_dataset_release(cache2))
  cat("damaged",file=file.path(cache2,"download.zip"),append=TRUE)
  expect_error(datapond:::verify_cached_dataset_release(cache2),"Retained cache ZIP")
  payload <- datapond:::collect_dataset_transfer_payload(f$id,f$pond,f$user)
  expect_false(any(datapond:::release_control_tables() %in% names(payload$scoped)))
  expect_error(datapond:::hard_delete_dataset_record(f$id,f$user,f$pond),"frozen releases")
})

test_that("changed previews and damaged archives are refused", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  p <- datapond:::preview_dataset_release(f$id,f$pond,f$user)
  writeLines("changed",file.path(f$folder,"data","measurements.csv"))
  expect_error(datapond:::create_dataset_release(f$id,f$pond,f$user,"Test",p),"changed since preview")
  row <- release_freeze(f)
  path <- file.path(f$pond,row$relative_path,"dataset.zip")
  cat("damage",file=path,append=TRUE)
  expect_error(datapond:::verify_dataset_release(row$release_id,f$pond),"integrity check")
  expect_error(datapond:::archive_dataset_release(row$release_id,f$pond,f$user,"Close"),"integrity check")
  expect_true(dir.exists(f$folder))
})

test_that("archive-and-close can restore the same dataset identity and metadata", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  row <- release_freeze(f,TRUE)
  expect_false(dir.exists(f$folder))
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  archived <- datapond:::release_entry(conn,f$id)
  expect_identical(archived$lifecycle_status,"archived")
  expect_identical(datapond:::reconcile_dataset_locations(f$pond,f$id)$location_status,"frozen_release")
  expect_error(datapond:::set_dataset_lifecycle(f$id,f$pond,"draft"),"Restore as draft")
  restored <- datapond:::restore_dataset_release(row$release_id,f$pond,f$user,"Continue from the archived version")
  entry <- datapond:::release_entry(conn,f$id)
  expect_identical(entry$uuid,archived$uuid)
  expect_identical(entry$lifecycle_status,"draft")
  expect_identical(entry$folder_locked,0L)
  expect_identical(readLines(file.path(restored$dataset_path,"data","measurements.csv")),c("id,value","1,20"))
  expect_identical(datapond:::reconcile_dataset_locations(f$pond,f$id)$location_status,"ok")
  expect_equal(nrow(DBI::dbGetQuery(conn,"PRAGMA foreign_key_check")),0)
  expect_equal(nrow(DBI::dbReadTable(conn,"entry_tbl")),1)
})

test_that("restore checkpoints newer drafts and remaps linked metadata", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  row <- release_freeze(f)
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  DBI::dbExecute(conn,"UPDATE entry_tbl SET title='New working title',version='1.1.0'")
  writeLines("newer draft bytes",file.path(f$folder,"data","measurements.csv"))
  restored <- datapond:::restore_dataset_release(row$release_id,f$pond,f$user,"Return to the verified baseline")
  expect_identical(datapond:::release_entry(conn,f$id)$title,"Release example")
  expect_true(file.exists(file.path(restored$checkpoint,"prior-working.zip")))
  expect_false(dir.exists(file.path(restored$checkpoint,"prior-working")))
  prior <- datapond:::release_read_payload(file.path(restored$checkpoint,"prior-metadata.sqlite"))
  expect_identical(prior$entry_tbl$title,"New working title")
  attrs <- DBI::dbReadTable(conn,"dataset_attribute_tbl"); files <- DBI::dbReadTable(conn,"data_file_tbl")
  expect_true(all(attrs$data_file_id %in% files$id))
  expect_equal(nrow(DBI::dbGetQuery(conn,"PRAGMA foreign_key_check")),0)
  checkpoint_id <- basename(restored$checkpoint)
  rolled_back <- datapond:::restore_dataset_checkpoint(checkpoint_id,f$pond,f$user,"Recover the newer draft")
  expect_identical(datapond:::release_entry(conn,f$id)$title,"New working title")
  expect_identical(readLines(file.path(rolled_back$dataset_path,"data","measurements.csv")),"newer draft bytes")
  expect_true(file.exists(file.path(rolled_back$checkpoint,"prior-working.zip")))
})

test_that("interrupted release registration can be recovered without rebuilding bytes", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  local({
    testthat::local_mocked_bindings(release_event=function(...) stop("Commit interrupted"),.package="datapond")
    expect_error(release_freeze(f),"Commit interrupted")
  })
  expect_equal(nrow(datapond:::dataset_release_history(f$id,f$pond)),0)
  expect_error(release_freeze(f),"interrupted freeze needs recovery")
  expect_match(datapond:::recover_dataset_release_operations(f$id,f$pond,f$user),"Recovered frozen release")
  row <- datapond:::dataset_release_history(f$id,f$pond)
  expect_equal(nrow(row),1)
  expect_silent(datapond:::verify_dataset_release(row$release_id,f$pond))
  expect_true(dir.exists(f$folder))
})

test_that("recovery repairs the filesystem gap after a rolled back archive transaction", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  writeLines("{}",file.path(f$folder,"operation.json"))
  writeLines("{}",file.path(f$folder,"freeze-operation.json"))
  row <- release_freeze(f)
  before <- datapond:::release_inventory(f$folder)
  local({
    testthat::local_mocked_bindings(release_before_commit=function() stop("Interrupted before commit"),
      release_rollback_files=function(op) invisible(op),.package="datapond")
    expect_error(datapond:::archive_dataset_release(row$release_id,f$pond,f$user,"Close"),"Interrupted")
  })
  expect_false(dir.exists(f$folder))
  expect_error(datapond:::restore_dataset_release(row$release_id,f$pond,f$user,"Restore"),"needs recovery")
  expect_match(datapond:::recover_dataset_release_operations(f$id,f$pond,f$user),"Rolled back interrupted close")
  expect_identical(datapond:::release_inventory(f$folder),before)
})

test_that("committed archive cleanup is resumable", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  row <- release_freeze(f)
  local({
    testthat::local_mocked_bindings(release_cleanup_operation=function(...) stop("Interrupted cleanup"),.package="datapond")
    expect_error(datapond:::archive_dataset_release(row$release_id,f$pond,f$user,"Close"),"Interrupted cleanup")
  })
  expect_false(dir.exists(f$folder))
  expect_match(datapond:::recover_dataset_release_operations(f$id,f$pond,f$user),"Completed cleanup for close")
  expect_false(dir.exists(f$folder))
  expect_silent(datapond:::verify_dataset_release(row$release_id,f$pond))
})

test_that("metadata and files changing during packaging cancel the freeze", {
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  testthat::local_mocked_bindings(release_write_documents=function(...) {
    writeLines("changed during render",file.path(f$folder,"data","measurements.csv"))
  },.package="datapond")
  expect_error(release_freeze(f),"changed while freezing")
  expect_equal(nrow(datapond:::dataset_release_history(f$id,f$pond)),0)
  expect_identical(readLines(file.path(f$folder,"data","measurements.csv")),"changed during render")
})

test_that("failed close and restore put the previous working folder back", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  row <- release_freeze(f)
  before <- datapond:::release_inventory(f$folder)
  testthat::local_mocked_bindings(release_before_commit=function() stop("Injected failure"),.package="datapond")
  expect_error(datapond:::archive_dataset_release(row$release_id,f$pond,f$user,"Close"),"Injected failure")
  expect_identical(datapond:::release_inventory(f$folder),before)
  expect_error(datapond:::restore_dataset_release(row$release_id,f$pond,f$user,"Restore"),"Injected failure")
  expect_identical(datapond:::release_inventory(f$folder),before)
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  expect_identical(datapond:::release_entry(conn,f$id)$dataset_path,f$folder)
  expect_identical(datapond:::release_entry(conn,f$id)$lifecycle_status,"draft")
})

test_that("symlinks cannot introduce data outside the frozen dataset", {
  skip_on_os("windows")
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  outside <- file.path(f$root,"outside.txt"); writeLines("private",outside)
  expect_true(file.symlink(outside,file.path(f$folder,"link.txt")))
  expect_error(datapond:::preview_dataset_release(f$id,f$pond,f$user),"Symbolic links")
})

test_that("recovery preserves late edits instead of removing retired files", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  row <- release_freeze(f)
  local({
    testthat::local_mocked_bindings(release_cleanup_operation=function(...) stop("Interrupted cleanup"),.package="datapond")
    expect_error(datapond:::archive_dataset_release(row$release_id,f$pond,f$user,"Close"),"Interrupted cleanup")
  })
  journal <- list.files(datapond:::release_private(f$pond,"operations"),pattern="^operation.json$",recursive=TRUE,full.names=TRUE)
  op <- jsonlite::read_json(journal)
  late <- file.path(op$backup,"late-edit.txt"); writeLines("keep this work",late)
  expect_error(datapond:::recover_dataset_release_operations(f$id,f$pond,f$user),"retired working folder changed")
  expect_identical(readLines(late),"keep this work")
})

test_that("damaged checkpoints and edited caches are detected", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  row <- release_freeze(f)
  restored <- datapond:::restore_dataset_release(row$release_id,f$pond,f$user,"Test restore")
  cat("damaged",file=file.path(restored$checkpoint,"prior-metadata.sqlite"),append=TRUE)
  expect_error(datapond:::restore_dataset_checkpoint(basename(restored$checkpoint),f$pond,f$user,"Undo"),"integrity check")
  expect_true(dir.exists(restored$dataset_path))
  cache <- datapond:::cache_dataset_release(row$release_id,f$pond,file.path(f$root,"cache"))
  writeLines("edited",file.path(cache,"release","dataset","data","measurements.csv"))
  expect_error(datapond:::verify_cached_dataset_release(cache),"files differ")
})

test_that("ZIP traversal is rejected without creating files outside extraction", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root,recursive=TRUE),add=TRUE)
  archive <- file.path(root,"unsafe.zip"); manifest <- file.path(root,"files.json")
  python <- Sys.which("python3"); skip_if(!nzchar(python))
  processx::run(python,c("-c", "import sys,zipfile; z=zipfile.ZipFile(sys.argv[1],'w'); z.writestr('../escape.txt',b'bad'); z.close()",archive))
  datapond:::release_json(list(list(path="../escape.txt",size=3L,sha256=digest::digest("bad",serialize=FALSE,algo="sha256"))),manifest)
  expect_error(datapond:::release_archive_tool("extract",archive,manifest,file.path(root,"extracted")),"Unsafe release path")
  expect_false(file.exists(file.path(root,"escape.txt")))
  expect_false(dir.exists(file.path(root,"extracted")))
})

test_that("recovery will not interrupt a live lifecycle owner", {
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  lock <- datapond:::release_lock(f$pond,f$id)
  expect_error(datapond:::recover_dataset_release_operations(f$id,f$pond,f$user),"may still be running")
  expect_true(dir.exists(lock))
})

test_that("freeze refuses an external data folder and restore keeps a custom data folder", {
  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  DBI::dbExecute(conn,"UPDATE entry_tbl SET data_path=?",params=list(f$root))
  expect_error(datapond:::preview_dataset_release(f$id,f$pond,f$user),"data folder must be inside")
  expect_true(file.rename(file.path(f$folder,"data"),file.path(f$folder,"spatial")))
  DBI::dbExecute(conn,"UPDATE entry_tbl SET data_path=?",params=list(file.path(f$folder,"spatial")))
  row <- release_freeze(f)
  restored <- datapond:::restore_dataset_release(row$release_id,f$pond,f$user,"Restore custom data folder")
  expect_identical(datapond:::release_entry(conn,f$id)$data_path,file.path(restored$dataset_path,"spatial"))
  expect_true(file.exists(file.path(restored$dataset_path,"spatial","measurements.csv")))
})

test_that("requested ISO export is packaged and validation failure aborts freezing", {
  testthat::local_mocked_bindings(
    render_metadata_document=function(directory) writeLines("Test PDF",file.path(directory,"metadata.pdf")),
    write_iso_wave3_documents=function(dataset_id,base_path,output_dir,validation_policy=c("pinned","datapond")) {
      expect_identical(validation_policy,"datapond")
      path <- file.path(output_dir,"metadata.xml"); writeLines("<fixture/>",path); path
    },.package="datapond")
  f <- release_fixture(); on.exit(unlink(f$root,recursive=TRUE),add=TRUE)
  p <- datapond:::preview_dataset_release(f$id,f$pond,f$user)
  row <- datapond:::create_dataset_release(f$id,f$pond,f$user,"With ISO",p,iso_policy="datapond")
  verified <- datapond:::verify_dataset_release(row$release_id,f$pond)
  members <- jsonlite::read_json(verified$manifest)
  expect_true("metadata.xml" %in% vapply(members,`[[`,character(1),"path"))
  f2 <- release_fixture(); on.exit(unlink(f2$root,recursive=TRUE),add=TRUE)
  local({
    testthat::local_mocked_bindings(write_iso_wave3_documents=function(...) stop("ISO validation failed"),.package="datapond")
    p2 <- datapond:::preview_dataset_release(f2$id,f2$pond,f2$user)
    expect_error(datapond:::create_dataset_release(f2$id,f2$pond,f2$user,"With ISO",p2,iso_policy="pinned"),"ISO validation failed")
  })
  expect_equal(nrow(datapond:::dataset_release_history(f2$id,f2$pond)),0)
  expect_true(file.exists(file.path(f2$folder,"data","measurements.csv")))
})

test_that("explicit document preparation supplies identical download, working and frozen copies", {
  release_test_renderer()
  f <- release_fixture(FALSE); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  expect_error(release_freeze(f), "Save and download metadata.pdf")
  download <- file.path(f$root, "browser.pdf")
  release_prepare(f, download)
  expected <- datapond:::release_sha(download)
  expect_identical(datapond:::release_sha(file.path(f$folder, "metadata.pdf")), expected)
  expect_identical(readLines(file.path(f$folder, "citation.txt")), "Example (2026). Release example.")
  expect_false(file.exists(file.path(f$folder, "metadata.qmd")))
  row <- release_freeze(f)
  extracted <- file.path(f$root, "extracted")
  datapond:::verify_dataset_release(row$release_id, f$pond, extract_to = extracted)
  for (path in c("metadata.pdf", "dataset/metadata.pdf"))
    expect_identical(datapond:::release_sha(file.path(extracted, path)), expected)
  for (path in c("citation.txt", "metadata.qmd", "ekznw-theme/ekznw.yml"))
    expect_identical(datapond:::release_sha(file.path(extracted, path)),
      datapond:::release_sha(file.path(datapond:::metadata_document_directory(f$pond, f$id), path)))
  expect_false(file.exists(file.path(extracted, "prepared.json")))
  expect_silent(datapond:::archive_dataset_release(row$release_id, f$pond, f$user, "Reviewed documents"))
  expect_error(release_prepare(f), "editable dataset|draft|unavailable")
  expect_silent(datapond:::verify_dataset_release(row$release_id, f$pond))
})

test_that("citation-only downloads cannot certify a missing or stale PDF", {
  f <- release_fixture(FALSE); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  download <- file.path(f$root, "browser.txt")
  release_test_renderer()
  datapond:::save_working_metadata_document(f$id, f$pond, f$user, pdf = FALSE, download = download)
  expect_identical(readLines(download), readLines(file.path(f$folder, "citation.txt")))
  expect_error(release_freeze(f), "Save and download metadata.pdf")
  release_prepare(f)
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET citation='Corrected citation'")
  datapond:::save_working_metadata_document(f$id, f$pond, f$user, pdf = FALSE, download = download)
  expect_identical(readLines(download), "Corrected citation")
  expect_error(release_freeze(f), "out of date")
  release_prepare(f)
  expect_silent(datapond:::preview_dataset_release(f$id, f$pond, f$user))
  writeLines("Externally edited PDF", file.path(f$folder, "metadata.pdf"))
  expect_error(release_freeze(f), "missing, changed or out of date")
  release_prepare(f)
  writeLines("Externally edited source", file.path(datapond:::metadata_document_directory(f$pond, f$id), "metadata.qmd"))
  expect_error(release_freeze(f), "missing, changed or out of date")
})

test_that("failed rendering or delivery preserves all previously prepared documents", {
  f <- release_fixture(); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  before <- datapond:::release_inventory(f$folder)
  private <- datapond:::metadata_document_directory(f$pond, f$id)
  cached <- datapond:::release_inventory(private)
  local({
    testthat::local_mocked_bindings(render_metadata_document = function(...) stop("Renderer unavailable"), .package = "datapond")
    expect_error(datapond:::save_working_metadata_document(f$id, f$pond, f$user), "Renderer unavailable")
  })
  expect_warning(expect_error(release_prepare(f, file.path(f$root, "missing", "browser.pdf")),
    "Could not deliver"), "cannot create file")
  expect_identical(datapond:::release_inventory(f$folder), before)
  expect_identical(datapond:::release_inventory(private), cached)
  expect_length(list.files(dirname(private), pattern = "^\\.prepare-", all.files = TRUE), 0)
  expect_silent(datapond:::preview_dataset_release(f$id, f$pond, f$user))
})

test_that("document publication rolls back partial replacements", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  targets <- file.path(root, c("metadata.pdf", "citation.txt"))
  sources <- file.path(root, c("new.pdf", "missing.txt"))
  writeLines("Old PDF", targets[1]); writeLines("Old citation", targets[2]); writeLines("New PDF", sources[1])
  expect_warning(expect_error(datapond:::metadata_document_publish(sources, targets), "Could not save"), "cannot rename")
  expect_identical(readLines(targets[1]), "Old PDF")
  expect_identical(readLines(targets[2]), "Old citation")
})

test_that("working document writes require an active user and an unlocked ordinary destination", {
  f <- release_fixture(); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  expect_error(datapond:::save_working_metadata_document(f$id, f$pond, NA_integer_, pdf = FALSE), "active application user")
  conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET folder_locked=1")
  expect_error(datapond:::save_working_metadata_document(f$id, f$pond, f$user, pdf = FALSE), "Unlock")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET folder_locked=0")
  skip_on_os("windows")
  path <- file.path(f$folder, "citation.txt"); unlink(path)
  target <- file.path(f$root, "outside.txt"); writeLines("Keep this", target)
  expect_true(file.symlink(target, path))
  expect_error(datapond:::save_working_metadata_document(f$id, f$pond, f$user, pdf = FALSE), "symbolic links")
  expect_identical(readLines(target), "Keep this")
})

test_that("metadata edited during PDF preparation leaves previous files untouched", {
  f <- release_fixture(); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  before <- datapond:::release_inventory(f$folder)
  testthat::local_mocked_bindings(render_metadata_document = function(directory) {
    conn <- datapond:::get_conn(f$pond); on.exit(DBI::dbDisconnect(conn))
    DBI::dbExecute(conn, "UPDATE entry_tbl SET title='Changed while preparing'")
    writeLines("New PDF", file.path(directory, "metadata.pdf"))
  }, .package = "datapond")
  expect_error(datapond:::save_working_metadata_document(f$id, f$pond, f$user), "changed during preparation")
  expect_identical(datapond:::release_inventory(f$folder), before)
  expect_error(release_freeze(f), "out of date")
})

test_that("release UI requires review confirmation and clears it on a new preview", {
  local_trusted_access_fixture()

  release_test_renderer()
  f <- release_fixture(); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  state <- shiny::reactiveValues(data_pond = f$pond, current_dataset_id = f$id,
    active_user_id = f$user, refresh_datasets = 0L)
  shiny::testServer(datapond:::mod_dataset_releases_server, args = list(state = state), {
    session$setInputs(preview = 1, notes = "Reviewed baseline", iso_policy = "none")
    session$setInputs(freeze = 1)
    expect_match(result(), "review confirmation")
    expect_equal(nrow(datapond:::dataset_release_history(f$id, f$pond)), 0)
    session$setInputs(documents_reviewed = TRUE)
    session$setInputs(preview = 2)
    session$setInputs(freeze = 2)
    expect_match(result(), "review confirmation")
    session$setInputs(documents_reviewed = FALSE)
    session$setInputs(documents_reviewed = TRUE)
    session$setInputs(freeze = 3)
    expect_equal(nrow(datapond:::dataset_release_history(f$id, f$pond)), 1)
    expect_null(preview())
  })
})

test_that("Shiny document downloads save the same bytes into the working folder", {
  local_trusted_access_fixture()

  f <- release_fixture(FALSE); on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(render_metadata_document = function(directory) {
    writeLines("Downloaded PDF fixture", file.path(directory, "metadata.pdf"))
  }, .package = "datapond")
  state <- shiny::reactiveValues(data_pond = f$pond, current_dataset_id = f$id, active_user_id = f$user)
  shiny::testServer(datapond:::mod_metadata_document_server, args = list(state = state), {
    download <- output$pdf
    expect_identical(datapond:::release_sha(download), datapond:::release_sha(file.path(f$folder, "metadata.pdf")))
    citation <- output$citation
    expect_identical(readLines(citation), readLines(file.path(f$folder, "citation.txt")))
  })
  expect_silent(datapond:::preview_dataset_release(f$id, f$pond, f$user))
})
