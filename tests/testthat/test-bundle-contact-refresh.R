test_that("bundle import immediately updates shared contact selectors without reopening the pond", {
  root <- tempfile("bundle-contact-refresh-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  local_mocked_bindings(app_state_dir = function() file.path(root, "app"), .package = "datapond")
  source <- file.path(root, "source")
  target <- file.path(root, "target")
  for (pond in c(source, target)) {
    datapond:::ensure_pond_structure(pond)
    datapond:::init_db(pond)
  }
  password <- "bundle contact refresh test passphrase"
  code <- datapond::setup_pond_access(target, "owner", first_name = "Target", last_name = "Owner")
  datapond:::access_activate(target, "owner", code, password)
  folder <- datapond:::create_dataset_folder_structure(
    base_path = file.path(source, "src"), folder_key = "contact_fixture",
    uuid = "bundle-contact-fixture", parent_dir = ".",
    data_pond_id = datapond:::ensure_data_pond_id(source))
  conn <- datapond:::get_conn(source)
  DBI::dbExecute(conn, "INSERT INTO organisation_tbl (id,organisation_name_full,organisation_name_short,organisation_code)
    VALUES (900,'Imported Climate Institute','Imported Climate Institute','ICI')")
  DBI::dbExecute(conn, "INSERT INTO persons_tbl (id,name_first,name_last,email,organisation_id)
    VALUES (900,'Imported','Scientist','scientist@example.test',900)")
  DBI::dbExecute(conn, "INSERT INTO user_tbl (id,person_id,user_name,is_active) VALUES (900,900,'source-scientist',1)")
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (id,uuid,title,dataset_path,data_path,data_pond_id,created_by_user_id)
    VALUES (1,'bundle-contact-fixture','Climate contact fixture',?,?,?,900)",
    params = list(folder$dataset_path, folder$data_path, datapond:::ensure_data_pond_id(source)))
  DBI::dbExecute(conn, "INSERT INTO dataset_organisation_tbl (dataset_id,organisation_id,role) VALUES (1,900,'originator')")
  DBI::dbExecute(conn, "INSERT INTO poc_tbl (dataset_id,person_id,organisation_id,role) VALUES (1,900,900,'pointOfContact')")
  DBI::dbDisconnect(conn)
  bundle <- datapond:::create_dataset_transfer_bundle(1L, source, 900L)
  withr::local_options(datapond.data_pond = target)

  shiny::testServer(datapond:::server, {
    # Capture the actual dropdown updates sent by the party and dataset modules.
    messages <- new.env(parent = emptyenv())
    session$sendInputMessage <- function(inputId, message) {
      # MockShinySession forwards module messages without namespacing them.
      inputId <- shiny::getDefaultReactiveDomain()$ns(inputId)
      previous <- messages[[inputId]]
      if (is.null(previous)) previous <- list()
      messages[[inputId]] <- utils::modifyList(previous, message)
    }
    html <- xml2::read_html(as.character(datapond:::ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$access_login <- 0L
    do.call(session$setInputs, defaults)
    session$setInputs(access_username = "owner", access_password = password, access_login = 1L)
    session$setInputs(access_workspace_ready = auth$workspace_nonce)
    session$flushReact()
    expect_true(auth$authenticated)
    options_for <- function(id) paste(messages[[id]]$options, collapse = "")
    expect_false(grepl("Imported Climate Institute", options_for("parties-org_select"), fixed = TRUE))
    expect_false(grepl("Imported Scientist", options_for("parties-person_edit_select"), fixed = TRUE))
    session$setInputs(dataset_bundle_file = data.frame(name = basename(bundle$path),
      size = file.info(bundle$path)$size, type = "application/zip", datapath = bundle$path))
    expect_true(uploaded_bundle_manifest()$ok)
    session$setInputs(import_dataset_bundle = 1L)
    session$flushReact()

    orgs <- datapond:::get_organisations(target)
    people <- datapond:::get_persons(target)
    organisation <- orgs[orgs$organisation_code == "ICI", , drop = FALSE]
    person <- people[people$email == "scientist@example.test" & !is.na(people$email), , drop = FALSE]
    expect_equal(nrow(organisation), 1L)
    expect_equal(nrow(person), 1L)
    expect_equal(person$organisation_id, organisation$id)
    expect_identical(state$current_dataset_uuid, "bundle-contact-fixture")
    expect_match(options_for("parties-org_select"), "Imported Climate Institute", fixed = TRUE)
    expect_match(options_for("orgs-org_select"), "Imported Climate Institute", fixed = TRUE)
    expect_match(options_for("parties-person_edit_select"), "Imported Scientist", fixed = TRUE)
    expect_match(options_for("poc-poc_person"), "Imported Scientist", fixed = TRUE)
    conn <- datapond:::get_conn(target)
    links <- DBI::dbGetQuery(conn, "SELECT person_id,organisation_id FROM poc_tbl WHERE dataset_id=?",
      params = list(state$current_dataset_id))
    DBI::dbDisconnect(conn)
    expect_equal(links$person_id, person$id)
    expect_equal(links$organisation_id, organisation$id)

    # Rejected duplicate imports neither add contacts nor invalidate their lists.
    before <- c(state$refresh_org, state$refresh_person, state$refresh_vocab)
    session$setInputs(import_dataset_bundle = 2L)
    expect_equal(c(state$refresh_org, state$refresh_person, state$refresh_vocab), before)
    expect_equal(nrow(datapond:::get_organisations(target)), nrow(orgs))
    expect_equal(nrow(datapond:::get_persons(target)), nrow(people))
  })
})
