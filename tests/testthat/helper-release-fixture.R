release_fixture <- function(prepare = TRUE) {
  root <- tempfile("release-test-"); dir.create(root)
  pond <- file.path(root, "pond"); dir.create(pond)
  datapond:::ensure_pond_structure(pond); datapond:::init_db(pond)
  datapond:::ensure_entry_data_access_columns(pond)
  conn <- datapond:::get_conn(pond); on.exit(DBI::dbDisconnect(conn))
  datapond:::ensure_columns(conn, "entry_tbl", c("citation", "purpose", "abstract", "keyword_object"))
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
  DBI::dbExecute(conn, "UPDATE entry_tbl SET keyword_object='observations',
    metadata_uuid='metadata-release-fixture', metadata_resource_scope_code='dataset',
    metadata_language_code='eng', metadata_character_encoding='utf8', geographic_extent_name='Test area',
    access_constraints='Internal', use_limitations='Test only', embargo_status='No' WHERE id=?", params=list(id))
  DBI::dbExecute(conn, "INSERT INTO dataset_topic_category_tbl(dataset_id,topic_category_code) VALUES (?,'environment')", params=list(id))
  DBI::dbExecute(conn, "INSERT INTO dataset_citation_date_tbl(dataset_id,date_value,date_type_code,date_precision)
    VALUES (?,'2026-01-01','publication','day')", params=list(id))
  DBI::dbExecute(conn, "INSERT INTO authority_tbl(dataset_id,person_id,author_order,role) VALUES (?,?,1,'author')", params=list(id,person))
  DBI::dbExecute(conn, "INSERT INTO poc_tbl(dataset_id,person_id) VALUES (?,?)", params=list(id,person))
  DBI::dbExecute(conn, "INSERT INTO metadata_responsibility_tbl(dataset_id,person_id,role_code,is_primary)
    VALUES (?,?,'pointOfContact',1)", params=list(id,person))
  DBI::dbExecute(conn, "INSERT INTO organisation_tbl(organisation_name_full,organisation_code) VALUES ('Test organisation','TEST')")
  org <- DBI::dbGetQuery(conn,"SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO dataset_organisation_tbl(dataset_id,organisation_id,role) VALUES (?,?,'owner')", params=list(id,org))
  DBI::dbExecute(conn, "INSERT INTO dataset_license_tbl(dataset_id,rights_basis_code,license_holder,access_level_code,
    redistribution_permission_code,license_identifier,sensitivity_handling_code)
    VALUES (?,'rights_unknown','Test organisation','not_distributable','prohibited','EKZNW-INTERNAL','none')", params=list(id))
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl(dataset_id,lineage_statement,lineage_source_mode)
    VALUES (?,'Synthetic observations.','form')", params=list(id))
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_process_step_tbl(dataset_id,description) VALUES (?,'Generated test measurements.')", params=list(id))
  f <- list(root=root,pond=pond,id=as.integer(id),user=as.integer(user),folder=folder)
  # Exercise the same lazy legacy normalisation as the status page before review.
  datapond:::get_dataset_completion_status(id,pond,datapond:::dataset_overview_schema(),"full")
  release_approve(f)
  if (prepare) release_prepare(f)
  f
}

release_approve <- function(f) {
  datapond:::insert_metadata_review(f$id, f$pond, reviewer_id=1L,
    update_reason="Approved saved release metadata", metadata_status="approved")
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

