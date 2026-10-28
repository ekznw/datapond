review_test_fixture <- function() {
  root <- tempfile("review-cycle-"); dir.create(root)
  ponds <- file.path(root, c("master","reviewer","stage"))
  users <- lapply(seq_along(ponds), function(i) {
    pond <- ponds[i]; dir.create(pond); datapond:::ensure_pond_structure(pond); datapond:::init_db(pond)
    conn <- datapond:::get_conn(pond); on.exit(DBI::dbDisconnect(conn),add=TRUE)
    DBI::dbExecute(conn,"INSERT INTO persons_tbl(name_first,name_last,email) VALUES(?,?,?)",params=list(paste0("User",i),"Editor",paste0("u",i,"@example.test")))
    person <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
    DBI::dbExecute(conn,"INSERT INTO user_tbl(person_id,user_name,is_active,is_admin) VALUES(?,?,1,?)",params=list(person,paste0("editor",i),as.integer(i==1)))
    list(id=DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1],person=person)
  })
  folder <- datapond:::create_dataset_folder_structure(file.path(ponds[1],"dev"),"review_test","review-test-uuid",parent_dir=".",data_pond_id=datapond:::ensure_data_pond_id(ponds[1]))
  writeLines("original bytes",file.path(folder$data_path,"test.txt"))
  id <- datapond:::save_entry_dynamic("entry_tbl",list(uuid="review-test-uuid",title="Original title",abstract="Original abstract",purpose="Original purpose",metadata_uuid="metadata-review-test",dataset_path=folder$dataset_path,data_path=folder$data_path,data_pond_id=datapond:::ensure_data_pond_id(ponds[1]),created_by_user_id=users[[1]]$id,lifecycle_status="draft",folder_key="review_test"),ponds[1])
  datapond:::insert_metadata_review(id,ponds[1],reviewer_id=users[[1]]$person,reviewer_name="User1 Editor",update_reason="Initial record",metadata_status="draft")
  list(root=root,master=ponds[1],reviewer=ponds[2],stage=ponds[3],users=users,id=id,folder=folder)
}
review_test_return <- function(f) {
  request <- datapond:::issue_dataset_review(f$id,f$master,f$users[[1]]$id,"User2 Editor")
  imported <- datapond:::import_dataset_transfer_bundle(request$path,f$reviewer,f$users[[2]]$id)
  datapond:::save_entry_dynamic("entry_tbl",list(title="Reviewed title",purpose="Reviewed purpose"),f$reviewer,id=imported$dataset_id)
  datapond:::insert_metadata_review(imported$dataset_id,f$reviewer,reviewer_id=f$users[[2]]$person,reviewer_name="User2 Editor",update_reason="Checked description",metadata_status="reviewed")
  returned <- datapond:::create_dataset_review_return(imported$dataset_id,f$reviewer,f$users[[2]]$id)
  list(request=request,imported=imported,returned=returned)
}

test_that("metadata review goes out, returns and updates the existing master atomically", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  expect_length(datapond:::dataset_transfer_archive_index(r$request$path)$names,1L)
  expect_false(file.exists(file.path(r$imported$dataset_path,"data","test.txt")))
  expect_error(datapond:::issue_dataset_review(f$id,f$master,f$users[[1]]$id,"Another colleague"),"active review")
  expect_error(datapond:::import_dataset_transfer_bundle(r$returned$path,f$master,f$users[[1]]$id),"Review returned bundle")
  check <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  expect_true(check$can_accept)
  expect_true(any(check$changes$Change == "Title"))
  expect_true(any(grepl("Reviewed title",check$changes$After,fixed=TRUE)))
  accepted <- datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accepted the colleague's corrections",check$return_hash)
  expect_identical(accepted$dataset_id,as.integer(f$id))
  expect_identical(accepted$dataset_uuid,"review-test-uuid")
  expect_identical(readLines(file.path(f$folder$data_path,"test.txt")),"original bytes")
  expect_true(file.exists(accepted$backup_path))
  conn <- datapond:::get_conn(f$master); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  entry <- DBI::dbGetQuery(conn,"SELECT * FROM entry_tbl WHERE id=?",params=list(f$id))
  expect_identical(entry$title,"Reviewed title")
  expect_identical(entry$purpose,"Reviewed purpose")
  expect_identical(entry$created_by_user_id,as.integer(f$users[[1]]$id))
  expect_identical(entry$dataset_path,f$folder$dataset_path)
  expect_equal(nrow(DBI::dbReadTable(conn,"metadata_review_tbl")),3)
  expect_equal(nrow(DBI::dbReadTable(conn,"dataset_import_tbl")),2)
  expect_identical(datapond:::review_requests(f$master,f$id)$status,"accepted")
  expect_error(datapond:::inspect_dataset_review_return(r$returned$path,f$master),"not awaiting acceptance")
  previous <- DBI::dbConnect(RSQLite::SQLite(),accepted$backup_path)
  expect_identical(DBI::dbReadTable(previous,"entry_tbl")$title,"Original title")
  DBI::dbDisconnect(previous)
})

test_that("review requests can carry the dataset files while returns update only metadata", {
  f <- review_test_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  files <- c("docs/method notes.txt", "README.txt", ".reference", "styles/theme/style.css")
  for (name in files) {
    path <- file.path(f$folder$dataset_path, name)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(paste("Reference:", name), path)
  }
  datapond:::scan_supporting_documents(f$id, f$folder$dataset_path, f$master, f$users[[1]]$person)
  old_zip <- "previous__datapond_old.zip"
  writeLines("Old transport archive", file.path(f$folder$dataset_path, old_zip))
  request <- datapond:::issue_dataset_review(f$id, f$master, f$users[[1]]$id,
    "User2 Editor", include_files = TRUE)
  archive <- datapond:::dataset_transfer_archive_index(request$path)
  expected <- c(files, "data/test.txt")
  expect_true(all(file.path(archive$root, expected) %in% archive$names))
  expect_false(file.path(archive$root, old_zip) %in% archive$names)
  manifest <- datapond:::read_dataset_transfer_manifest(request$path)
  expect_identical(manifest$review_include_files, 1L)
  # Old metadata-only requests cannot silently acquire a file payload.
  manifest$review_include_files <- NULL
  expect_error(datapond:::review_check_archive(archive, manifest), "only its metadata")
  imported <- datapond:::import_dataset_transfer_bundle(request$path, f$reviewer, f$users[[2]]$id)
  expect_identical(unname(tools::md5sum(file.path(imported$dataset_path, expected))),
    unname(tools::md5sum(file.path(f$folder$dataset_path, expected))))
  docs <- datapond:::get_supporting_documents(imported$dataset_id, f$reviewer)
  method <- docs[docs$document_rel_path == "docs/method notes.txt", , drop = FALSE]
  expect_equal(nrow(method), 1L)
  expect_identical(method$document_path, file.path(imported$dataset_path, "docs/method notes.txt"))
  expect_true(file.exists(method$document_path))
  datapond:::save_entry_dynamic("entry_tbl", list(title = "Checked against supporting documents"),
    f$reviewer, id = imported$dataset_id)
  writeLines("Local file change", file.path(imported$dataset_path, "data/test.txt"))
  returned <- datapond:::create_dataset_review_return(imported$dataset_id, f$reviewer, f$users[[2]]$id)
  expect_length(datapond:::dataset_transfer_archive_index(returned$path)$names, 1L)
  check <- datapond:::inspect_dataset_review_return(returned$path, f$master)
  expect_true(check$can_accept)
  datapond:::accept_dataset_review_return(returned$path, f$master, f$users[[1]]$id,
    "Checked supporting documents", check$return_hash)
  expect_identical(datapond:::get_dataset_record(f$id, f$master)$title, "Checked against supporting documents")
  expect_identical(readLines(file.path(f$folder$data_path, "test.txt")), "original bytes")
  manifest$review_kind <- "return"
  manifest$review_include_files <- 1L
  expect_error(datapond:::review_check_archive(archive, manifest), "only its metadata")
})

test_that("file changes during request packaging leave no active review", {
  f <- review_test_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  original <- datapond:::write_dataset_transfer_database
  local_mocked_bindings(write_dataset_transfer_database = function(...) {
    original(...)
    writeLines("Changed during packaging", file.path(f$folder$data_path, "test.txt"))
  }, .package = "datapond")
  expect_error(datapond:::issue_dataset_review(f$id, f$master, f$users[[1]]$id,
    "User2 Editor", include_files = TRUE), "files changed")
  expect_equal(nrow(datapond:::review_requests(f$master)), 0L)
  expect_length(list.files(file.path(f$master, ".metadata_pond", "reviews")), 0L)
})

test_that("changed masters and files cannot be overwritten by a returned review", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  preview <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  datapond:::save_entry_dynamic("entry_tbl",list(title="Master changed independently"),f$master,id=f$id)
  conflict <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  expect_false(conflict$can_accept)
  expect_match(conflict$conflicts[1],"metadata changed")
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accept",preview$return_hash),"Master metadata changed")
  datapond:::save_entry_dynamic("entry_tbl",list(title="Original title"),f$master,id=f$id)
  writeLines("changed file",file.path(f$folder$data_path,"test.txt"))
  expect_false(datapond:::inspect_dataset_review_return(r$returned$path,f$master)$can_accept)
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accept",preview$return_hash),"Master files changed")
})

test_that("failed acceptance rolls back metadata and leaves the review pending", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  preview <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  testthat::local_mocked_bindings(review_finish_accept=function(...) stop("Injected failure before commit"),.package="datapond")
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accept",preview$return_hash),"Injected failure")
  conn <- datapond:::get_conn(f$master); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  expect_identical(DBI::dbReadTable(conn,"entry_tbl")$title,"Original title")
  expect_equal(nrow(DBI::dbReadTable(conn,"metadata_review_tbl")),1)
  expect_equal(nrow(DBI::dbReadTable(conn,"dataset_import_tbl")),0)
  expect_identical(datapond:::review_requests(f$master,f$id)$status,"issued")
  expect_identical(readLines(file.path(f$folder$data_path,"test.txt")),"original bytes")
})

test_that("returns require an administrator, an inspected bundle and a decision", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  preview <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"",preview$return_hash),"decision")
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accept","incorrect hash"),"changed after inspection")
  conn <- datapond:::get_conn(f$master)
  DBI::dbExecute(conn,"UPDATE user_tbl SET is_admin=0")
  DBI::dbDisconnect(conn)
  # Role changes also invalidate the strict baseline; direct authority check must reject.
  conn <- datapond:::get_conn(f$master)
  expect_error(datapond:::review_require_user(conn,f$users[[1]]$id,admin=TRUE),"administrator")
  DBI::dbDisconnect(conn)
})

test_that("cancellation prevents later acceptance and permits a fresh request", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  datapond:::cancel_dataset_review(r$request$review_id,f$master,f$users[[1]]$id,"Scope changed")
  expect_error(datapond:::inspect_dataset_review_return(r$returned$path,f$master),"not awaiting acceptance")
  expect_type(datapond:::issue_dataset_review(f$id,f$master,f$users[[1]]$id,"New colleague")$path,"character")
})

test_that("linked metadata replacements retain master file IDs and remap lineage links", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  conn <- datapond:::get_conn(f$master)
  DBI::dbExecute(conn,"INSERT INTO data_file_tbl(dataset_id,data_file_rel_path_full,file_name,file_path,data_pond_id) VALUES(?, 'test.txt','test.txt',?,?)",params=list(f$id,file.path(f$folder$data_path,"test.txt"),datapond:::ensure_data_pond_id(f$master)))
  file_id <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn,"INSERT INTO dataset_attribute_tbl(dataset_id,data_file_id,data_file_rel_path_full,data_file_name,data_field_name,data_field_desc,attribute_type,attribute_order) VALUES(?,?,'test.txt','test.txt','value','Value','character',1)",params=list(f$id,file_id))
  attr_id <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn,"INSERT INTO dataset_quality_result_tbl(dataset_id,data_file_id,attribute_id,measure_identifier,measure_type_code,evaluation_method,result_type_code,explanation) VALUES(?,?,?,'check','completeness','Inspection','descriptive','Initial assessment')",params=list(f$id,file_id,attr_id))
  DBI::dbDisconnect(conn)
  writeLines("Method",file.path(f$folder$dataset_path,"docs","method.txt"))
  datapond:::scan_supporting_documents(f$id,f$folder$dataset_path,f$master,f$users[[1]]$person)
  doc <- datapond:::get_supporting_documents(f$id,f$master)$id[1]
  source <- datapond:::save_dataset_lineage_source(f$id,list(title="Source",description="Before",supporting_document_id=doc,value_origin_code="supporting_document"),f$master,f$users[[1]]$person)
  step <- datapond:::save_dataset_lineage_process_step(f$id,list(description="Original step"),f$master,f$users[[1]]$person)
  datapond:::set_dataset_lineage_step_sources(f$id,step,source,f$master,f$users[[1]]$person)
  r <- review_test_return(f)
  conn <- datapond:::get_conn(f$reviewer)
  DBI::dbExecute(conn,"UPDATE dataset_lineage_source_tbl SET description='Clarified source'")
  DBI::dbExecute(conn,"UPDATE dataset_lineage_process_step_tbl SET description='Clarified step'")
  DBI::dbExecute(conn,"UPDATE dataset_quality_result_tbl SET explanation='Reviewed assessment'")
  template <- DBI::dbGetQuery(conn,"SELECT * FROM license_template_tbl WHERE license_identifier='CC-BY-4.0'")
  DBI::dbExecute(conn,"INSERT INTO dataset_license_tbl(dataset_id,license_template_id,license_identifier,license_name,access_level_code) VALUES(?,?,?,'Creative Commons Attribution 4.0','public')",params=list(r$imported$dataset_id,template$id[1],"CC-BY-4.0"))
  DBI::dbDisconnect(conn)
  returned <- datapond:::create_dataset_review_return(r$imported$dataset_id,f$reviewer,f$users[[2]]$id)
  check <- datapond:::inspect_dataset_review_return(returned$path,f$master)
  expect_true(check$can_accept)
  datapond:::accept_dataset_review_return(returned$path,f$master,f$users[[1]]$id,"Checked all linked metadata",check$return_hash)
  conn <- datapond:::get_conn(f$master); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  expect_identical(DBI::dbReadTable(conn,"data_file_tbl")$id,as.integer(file_id))
  expect_identical(DBI::dbReadTable(conn,"dataset_attribute_tbl")$id,as.integer(attr_id))
  expect_identical(DBI::dbReadTable(conn,"supporting_document_tbl")$id,as.integer(doc))
  sources <- DBI::dbReadTable(conn,"dataset_lineage_source_tbl")
  steps <- DBI::dbReadTable(conn,"dataset_lineage_process_step_tbl")
  links <- DBI::dbReadTable(conn,"dataset_lineage_step_source_tbl")
  expect_identical(sources$description,"Clarified source")
  expect_identical(sources$supporting_document_id,as.integer(doc))
  expect_identical(steps$description,"Clarified step")
  expect_identical(links$source_id,sources$id)
  expect_identical(links$process_step_id,steps$id)
  quality <- DBI::dbReadTable(conn,"dataset_quality_result_tbl")
  expect_identical(quality$explanation,"Reviewed assessment")
  expect_identical(quality$data_file_id,as.integer(file_id))
  expect_identical(quality$attribute_id,as.integer(attr_id))
  expect_identical(DBI::dbReadTable(conn,"dataset_license_tbl")$license_identifier,"CC-BY-4.0")
  expect_equal(nrow(DBI::dbGetQuery(conn,"PRAGMA foreign_key_check")),0L)
})

test_that("modified history, extra files and changed protected metadata are rejected", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  payload <- datapond:::review_read_bundle(r$returned$path)
  payload$scoped$metadata_review_tbl$update_reason[1] <- "Rewritten history"
  altered <- datapond:::review_write_bundle(payload,file.path(f$root,"altered"),"return")
  expect_error(datapond:::inspect_dataset_review_return(altered,f$master),"review history")
  payload <- datapond:::review_read_bundle(r$returned$path)
  payload$entry_tbl$lifecycle_status <- "released"
  altered <- datapond:::review_write_bundle(payload,file.path(f$root,"identity"),"return")
  expect_error(datapond:::inspect_dataset_review_return(altered,f$master),"identity, lifecycle")
  payload <- datapond:::review_read_bundle(r$returned$path)
  payload$manifest$baseline_hash <- "wrong"
  altered <- datapond:::review_write_bundle(payload,file.path(f$root,"baseline"),"return")
  expect_error(datapond:::inspect_dataset_review_return(altered,f$master),"registered request")
  stage <- file.path(f$root,"extra");dir.create(stage)
  writeLines("not allowed",file.path(stage,"extra.txt"))
  old <- setwd(stage); on.exit(setwd(old),add=TRUE)
  utils::zip(altered,"extra.txt",flags="-q")
  expect_error(datapond:::inspect_dataset_review_return(altered,f$master),"top-level|only its metadata")
})

test_that("app review module previews returns and accepts only after inspection", {
  local_trusted_access_fixture()

  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  state <- shiny::reactiveValues(data_pond=f$master,current_dataset_id=f$id,active_user_id=f$users[[1]]$id)
  accepted <- 0L
  shiny::testServer(datapond:::mod_dataset_review_server,args=list(state=state,on_accepted=function() accepted <<- accepted+1L),{
    session$setInputs(inspect=0L,accept=0L)
    session$setInputs(bundle=data.frame(name=basename(r$returned$path),size=file.info(r$returned$path)$size,type="application/zip",datapath=r$returned$path))
    expect_null(assessment())
    session$setInputs(inspect=1L)
    expect_true(assessment()$can_accept)
    session$setInputs(decision="Reviewed and accepted",accept=1L)
    expect_null(assessment())
  })
  expect_equal(accepted,1L)
  html <- as.character(datapond:::mod_dataset_review_ui("review"))
  expect_match(html,"Inspect return",fixed=TRUE)
  expect_match(html,"metadata only",fixed=TRUE)
})

test_that("the review form passes the selected file option to request creation", {
  local_trusted_access_fixture()
  f <- review_test_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  state <- shiny::reactiveValues(data_pond = f$master, current_dataset_id = f$id,
    active_user_id = f$users[[1]]$id)
  options <- logical()
  local_mocked_bindings(issue_dataset_review = function(dataset_id, base_path, user_id,
      assignee, include_files = FALSE) {
    options <<- c(options, include_files)
    list(path = file.path(f$root, "request.zip"))
  }, .package = "datapond")
  shiny::testServer(datapond:::mod_dataset_review_server, args = list(state = state), {
    session$setInputs(issue = 0L, assignee = "Colleague", include_files = TRUE)
    session$setInputs(issue = 1L)
    session$setInputs(include_files = FALSE, issue = 2L)
  })
  expect_identical(options, c(TRUE, FALSE))
  html <- xml2::read_html(as.character(datapond:::mod_dataset_review_ui("review")))
  checkbox <- xml2::xml_find_all(html, "//input[@id='review-include_files' and @checked]")
  expect_length(checkbox, 1L)
})

test_that("an ordinary second transfer retains the first import's provenance", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  first <- datapond:::create_dataset_transfer_bundle(f$id,f$master,f$users[[1]]$id)
  imported <- datapond:::import_dataset_transfer_bundle(first$path,f$reviewer,f$users[[2]]$id)
  second <- datapond:::create_dataset_transfer_bundle(imported$dataset_id,f$reviewer,f$users[[2]]$id)
  next_import <- datapond:::import_dataset_transfer_bundle(second$path,f$stage,f$users[[3]]$id)
  history <- datapond:::get_dataset_import_provenance(next_import$dataset_id,f$stage)
  expect_equal(nrow(history),2L)
  expect_setequal(history$bundle_id,c(first$bundle_id,second$bundle_id))
  expect_true(all(history$source_dataset_uuid=="review-test-uuid"))
})

test_that("administrator enforcement and late baseline changes are rechecked at acceptance", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  r <- review_test_return(f)
  preview <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  conn <- datapond:::get_conn(f$master)
  DBI::dbExecute(conn,"INSERT INTO persons_tbl(name_first,name_last,email) VALUES('Other','Editor','other@example.test')")
  person <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn,"INSERT INTO user_tbl(person_id,user_name,is_active,is_admin) VALUES(?,'other',1,0)",params=list(person))
  user <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbDisconnect(conn)
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,user,"Accept",preview$return_hash),"administrator")
  original <- datapond:::review_begin_accept
  testthat::local_mocked_bindings(review_begin_accept=function(conn,assessment,pond) {
    DBI::dbExecute(conn,"UPDATE entry_tbl SET purpose='Late change' WHERE id=?",params=list(assessment$dataset_id))
    original(conn,assessment,pond)
  },.package="datapond")
  expect_error(datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accept",preview$return_hash),"Master metadata changed")
  expect_identical(datapond:::get_dataset_record(f$id,f$master)$title,"Original title")
})

test_that("file inventory changes are rejected and unrelated master records are preserved", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  other_id <- datapond:::save_entry_dynamic("entry_tbl",list(uuid="unrelated",title="Unrelated",purpose="Keep this"),f$master)
  r <- review_test_return(f)
  payload <- datapond:::review_read_bundle(r$returned$path)
  payload$scoped$data_file_tbl <- data.frame(id=99L,dataset_id=r$imported$dataset_id,file_name="new.txt")
  changed <- datapond:::review_write_bundle(payload,file.path(f$root,"inventory-change"),"return")
  expect_error(datapond:::inspect_dataset_review_return(changed,f$master),"file inventories")
  preview <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Accept",preview$return_hash)
  expect_identical(datapond:::get_dataset_record(other_id,f$master)$purpose,"Keep this")
})

test_that("distributor references are remapped and dangling references cannot be accepted", {
  f <- review_test_fixture(); on.exit(unlink(f$root,recursive=TRUE))
  conn <- datapond:::get_conn(f$master)
  DBI::dbExecute(conn,"INSERT INTO organisation_tbl(organisation_name_full,organisation_code) VALUES('Review distributor','RVD')")
  org <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  datapond:::ensure_columns(conn,"entry_tbl","data_distributor_organisation_id",types=c(data_distributor_organisation_id="INTEGER"))
  DBI::dbExecute(conn,"UPDATE entry_tbl SET data_distributor_organisation_id=? WHERE id=?",params=list(org,f$id))
  DBI::dbDisconnect(conn)
  # Force different source and destination organisation IDs.
  conn <- datapond:::get_conn(f$reviewer)
  DBI::dbExecute(conn,"INSERT INTO organisation_tbl(organisation_name_full,organisation_code) VALUES('Local organisation','LOC')")
  DBI::dbDisconnect(conn)
  r <- review_test_return(f)
  check <- datapond:::inspect_dataset_review_return(r$returned$path,f$master)
  expect_true(check$can_accept)
  datapond:::accept_dataset_review_return(r$returned$path,f$master,f$users[[1]]$id,"Checked distributor",check$return_hash)
  expect_identical(datapond:::get_dataset_record(f$id,f$master)$data_distributor_organisation_id,as.integer(org))
  payload <- datapond:::review_read_bundle(r$returned$path)
  payload$entry_tbl$data_distributor_organisation_id <- 999999L
  conn <- datapond:::get_conn(f$master); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  expect_error(datapond:::review_validate_metadata(check$baseline,payload,conn),"Unresolved")
})
