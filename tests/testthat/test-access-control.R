access_fixture <- function() {
  root <- tempfile("access-test-"); dir.create(root)
  pond <- file.path(root, "pond")
  datapond:::ensure_pond_structure(pond); datapond:::init_db(pond)
  list(root = root, pond = pond)
}

test_that("bootstrap, registration, activation and login preserve attribution", {
  f <- access_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(f$root, "app"), .package = "datapond")
  pond <- f$pond
  code <- setup_pond_access(pond, "owner", first_name = "Pond", last_name = "Owner")
  expect_length(code, 1L)
  expect_error(setup_pond_access(pond, "other", first_name = "Other", last_name = "Owner"), "already configured")
  expect_error(datapond:::access_login(pond, "owner", "long enough password"), "Sign-in failed")
  datapond:::access_activate(pond, "owner", code, "a long unique pond passphrase")
  expect_error(datapond:::access_activate(pond, "owner", code, "another long pond passphrase"), "Invalid or expired")
  token <- datapond:::access_login(pond, "OWNER", "a long unique pond passphrase")
  expect_identical(datapond:::access_session(pond, token, "admin")$role, "admin")
  expect_null(datapond:::access_session(pond, "invented token", "admin"))
  datapond:::access_request(pond, "colleague", "New", "Colleague", organisation = "Field team")
  rows <- datapond:::access_accounts(pond, token)
  request <- rows$id[rows$username == "colleague"]
  expect_error(datapond:::access_login(pond, "colleague", "colleague passphrase"), "Sign-in failed")
  # A pending registration does not create or claim an attribution identity.
  expect_equal(nrow(datapond:::get_users(pond)), 1L)
  activation <- datapond:::access_approve(pond, token, request, "viewer")
  datapond:::access_activate(pond, "colleague", activation, "colleague long passphrase")
  viewer <- datapond:::access_login(pond, "colleague", "colleague long passphrase")
  expect_identical(datapond:::access_session(pond, viewer)$role, "viewer")
  expect_null(datapond:::access_session(pond, viewer, "edit"))
  expect_null(datapond:::access_session(pond, viewer, "download"))
  expect_error(datapond:::access_accounts(pond, viewer), "administrator")
  expect_error(datapond:::access_update_account(pond, viewer, request, "admin", "active"), "administrator")
  expect_error(datapond:::access_approve(pond, token, request), "pending")
  datapond:::access_update_account(pond, token, request, "editor", "active")
  expect_null(datapond:::access_session(pond, viewer))
  editor <- datapond:::access_login(pond, "colleague", "colleague long passphrase")
  expect_identical(datapond:::access_session(pond, editor, "edit")$role, "editor")
  expect_null(datapond:::access_session(pond, editor, "admin"))
  recovery <- datapond:::access_recovery(pond, token, request)
  expect_null(datapond:::access_session(pond, editor))
  datapond:::access_activate(pond, "colleague", recovery, "new colleague passphrase")
  expect_error(datapond:::access_login(pond, "colleague", "colleague long passphrase"), "Sign-in failed")
  expect_error(datapond:::access_activate(pond, "colleague", recovery, "yet another passphrase"), "Invalid or expired")
  editor <- datapond:::access_login(pond, "colleague", "new colleague passphrase")
  datapond:::access_update_account(pond, token, request, "editor", "disabled")
  expect_null(datapond:::access_session(pond, editor))
  expect_error(datapond:::access_login(pond, "colleague", "new colleague passphrase"), "Sign-in failed")
  expect_error(datapond:::access_update_account(pond, token, 1L, "viewer", "active"), "at least one")
  expect_error(datapond:::access_update_account(pond, token, 1L, "admin", "disabled"), "at least one")
  # A rejected request and a disabled invitation can be reopened deliberately.
  datapond:::access_request(pond, "invited", "Invited", "Person")
  datapond:::access_update_account(pond, token, 3L, "viewer", "rejected")
  datapond:::access_update_account(pond, token, 3L, "viewer", "pending")
  invisible(datapond:::access_approve(pond, token, 3L, "viewer"))
  datapond:::access_update_account(pond, token, 3L, "viewer", "disabled")
  expect_error(datapond:::access_update_account(pond, token, 3L, "viewer", "active"), "activate")
  datapond:::access_update_account(pond, token, 3L, "viewer", "invited")
  invitation <- datapond:::access_recovery(pond, token, 3L)
  expect_silent(datapond:::access_activate(pond, "invited", invitation, "an invited long passphrase"))
  person <- datapond:::insert_person(list(first="Known", last="Person", email="known@example.test", org_id=NA_integer_), pond)
  datapond:::insert_user(person, pond)
  existing_user <- datapond:::get_users(pond)$id[datapond:::get_users(pond)$person_id == person]
  datapond:::access_request(pond, "known", "Known", "Person", "known@example.test")
  expect_error(datapond:::access_approve(pond, token, 4L), "Verify and select")
  expect_length(datapond:::access_approve(pond, token, 4L, person_id=person), 1L)
  expect_equal(datapond:::access_accounts(pond, token)$user_id[4], existing_user)
  conn <- datapond:::access_connect(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  accounts <- DBI::dbReadTable(conn, "accounts")
  expect_false(any(grepl("passphrase", accounts$password_hash)))
  expect_false(token %in% DBI::dbReadTable(conn, "sessions")$token_hash)
  expect_true(all(c("approved", "account_changed", "password_set", "login_failed") %in% DBI::dbReadTable(conn, "audit")$event))
  expect_false(startsWith(datapond:::access_db_path(pond), paste0(pond, "/")))
  meta <- datapond:::get_conn(pond); on.exit(DBI::dbDisconnect(meta), add = TRUE, after = FALSE)
  expect_false(any(c("accounts", "sessions", "tokens") %in% DBI::dbListTables(meta)))
})

test_that("expiry, throttling, pond isolation and console recovery fail closed", {
  f <- access_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(f$root, "app"), .package = "datapond")
  code <- setup_pond_access(f$pond, "owner", first_name = "Pond", last_name = "Owner")
  conn <- datapond:::access_connect(f$pond); on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "UPDATE tokens SET expires_at=0")
  expect_error(datapond:::access_activate(f$pond, "owner", code, "a long unique passphrase"), "Invalid or expired")
  code <- recover_pond_access(f$pond, "owner")
  datapond:::access_activate(f$pond, "owner", code, "a long unique passphrase")
  for (i in 1:5) expect_error(datapond:::access_login(f$pond, "owner", "wrong"), "Sign-in failed")
  expect_error(datapond:::access_login(f$pond, "owner", "a long unique passphrase"), "Too many")
  code <- recover_pond_access(f$pond, "owner")
  datapond:::access_activate(f$pond, "owner", code, "a new long unique passphrase")
  token <- datapond:::access_login(f$pond, "owner", "a new long unique passphrase")
  other <- file.path(f$root, "other"); datapond:::init_db(other)
  expect_null(datapond:::access_session(other, token))
  DBI::dbExecute(conn, "UPDATE sessions SET last_seen=0")
  expect_null(datapond:::access_session(f$pond, token))
  token <- datapond:::access_login(f$pond, "owner", "a new long unique passphrase")
  DBI::dbExecute(conn, "UPDATE sessions SET created_at=0")
  expect_null(datapond:::access_session(f$pond, token))
  token <- datapond:::access_login(f$pond, "owner", "a new long unique passphrase")
  datapond:::access_logout(f$pond, token)
  expect_null(datapond:::access_session(f$pond, token))
})

test_that("cached HTTP data is bound to its login and revoked immediately", {
  f <- access_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(f$root, "app"), .package = "datapond")
  code <- setup_pond_access(f$pond, "owner", first_name = "Pond", last_name = "Owner")
  datapond:::access_activate(f$pond, "owner", code, "a long owner passphrase")
  token <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
  auth <- shiny::reactiveValues(pond=f$pond, token=token, ready=TRUE, authenticated=TRUE)
  calls <- 0L
  endpoint <- datapond:::access_http_filter(function(...) { calls <<- calls + 1L; shiny::httpResponse(content="private data") },
    auth, bind_identity=TRUE)
  expect_equal(endpoint()$status, 200L)
  datapond:::access_logout(f$pond, token)
  expect_equal(endpoint()$status, 403L)
  expect_equal(calls, 1L)
  replacement <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
  auth$token <- replacement
  expect_equal(endpoint()$status, 403L)
  expect_equal(calls, 1L)
})

test_that("browser gates reject forged identities and viewer/admin mutations", {
  f <- access_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(f$root, "app"), .package = "datapond")
  code <- setup_pond_access(f$pond, "owner", first_name = "Pond", last_name = "Owner")
  datapond:::access_activate(f$pond, "owner", code, "a long owner passphrase")
  owner <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
  datapond:::access_request(f$pond, "viewer", "View", "Only")
  code <- datapond:::access_approve(f$pond, owner, 2L, "viewer")
  datapond:::access_activate(f$pond, "viewer", code, "a long viewer passphrase")
  viewer <- datapond:::access_login(f$pond, "viewer", "a long viewer passphrase")
  state <- shiny::reactiveValues(data_pond=f$pond, active_user_id=1L, refresh_person=0L, refresh_org=0L)
  shiny::testServer(function(input, output, session) {
    session$userData$datapond_access <- shiny::reactiveValues(pond=f$pond, token=NULL, authenticated=FALSE, ready=TRUE, tick=0L)
    datapond:::mod_parties_server("parties", state)
  }, {
    auth <- session$userData$datapond_access
    expect_false(datapond:::access_allowed("read"))
    expect_false(datapond:::require_active_user_for_write(state))
    auth$token <- viewer; auth$authenticated <- TRUE
    expect_true(datapond:::access_allowed("read"))
    expect_false(datapond:::access_allowed("edit"))
    expect_false(datapond:::is_user_admin(1L, f$pond))
    session$setInputs(`parties-add_person`=0, `parties-first`="Forged", `parties-last`="Person")
    before <- nrow(datapond:::get_persons(f$pond))
    session$setInputs(`parties-add_person`=1)
    expect_equal(nrow(datapond:::get_persons(f$pond)), before)
    expect_error(setup_pond_access(f$pond, "hijack"), "local R console")
    auth$token <- owner
    expect_true(datapond:::access_allowed("admin"))
    expect_true(datapond:::require_active_user_for_write(state))
    state$active_user_id <- 999L
    expect_false(datapond:::require_active_user_for_write(state))
    datapond:::access_logout(f$pond, owner)
    expect_false(datapond:::access_allowed("read"))
  })
})

test_that("active Flow work prevents idle expiry without bypassing session revocation", {
  f <- access_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  clock <- new.env(); clock$now <- as.numeric(Sys.time()); clock$polls <- 0L
  local_mocked_bindings(
    app_state_dir = function() file.path(f$root, "app"),
    access_now = function() clock$now,
    movement_remotes = function() data.frame(name = character(), type = character()),
    movement_job_poll = function(job) { clock$polls <- clock$polls + 1L; job$status },
    movement_job_log = function(...) "Test transfer running",
    .package = "datapond"
  )
  code <- setup_pond_access(f$pond, "owner", first_name = "Pond", last_name = "Owner")
  datapond:::access_activate(f$pond, "owner", code, "a long owner passphrase")
  token <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
  shiny::testServer(datapond:::mod_data_movement_server, args = list(
    pond = function() f$pond, dataset_path = function() NULL,
    state_directory = file.path(f$root, "app")
  ), {
    auth <- shiny::reactiveValues(pond = f$pond, token = token, ready = TRUE, authenticated = TRUE)
    session$userData$datapond_access <- auth
    session$flushReact()
    new_job <- function() list2env(list(status = "running", phase = "running", steps = list(),
      ticket = NULL, process = NULL), parent = emptyenv())
    advance <- function(seconds) {
      clock$now <- clock$now + seconds
      session$elapse(600)
      session$flushReact()
    }
    state$job <- new_job()
    session$flushReact()
    for (i in seq_len(31L)) advance(60)
    expect_false(is.null(datapond:::access_session(f$pond, token, "admin")))
    expect_equal(datapond:::access_session(f$pond, token)$last_seen, clock$now)
    # Sub-minute progress polling does not repeatedly write the session store.
    previous <- clock$now
    advance(10)
    expect_equal(datapond:::access_session(f$pond, token)$last_seen, previous)
    # Queued work also remains authorised, including a wait for cloud backoff.
    state$job$phase <- "queued"
    advance(60)
    expect_equal(datapond:::access_session(f$pond, token)$last_seen, clock$now)
    state$job$status <- "complete"
    advance(1801)
    expect_null(datapond:::access_session(f$pond, token))

    auth$token <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
    state$job <- new_job(); state$private <- TRUE
    session$flushReact()
    advance(1801)
    expect_null(datapond:::access_session(f$pond, auth$token))

    auth$token <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
    state$private <- FALSE; state$job <- new_job()
    session$flushReact()
    conn <- datapond:::access_connect(f$pond)
    DBI::dbExecute(conn, "UPDATE sessions SET created_at=?", params = list(clock$now - 43201))
    polls <- clock$polls
    advance(60)
    expect_null(datapond:::access_session(f$pond, auth$token))
    expect_equal(clock$polls, polls)
    DBI::dbDisconnect(conn)

    auth$token <- datapond:::access_login(f$pond, "owner", "a long owner passphrase")
    session$flushReact()
    datapond:::access_logout(f$pond, auth$token)
    polls <- clock$polls
    advance(60)
    expect_null(datapond:::access_session(f$pond, auth$token))
    expect_equal(clock$polls, polls)
    # The sign-out/revocation hook still cancels the session's job.
    for (cleanup in session$userData$datapond_access_cleanup) cleanup()
    expect_identical(state$job$status, "cancelled")
  })
})

test_that("the full app starts locked and authenticates only through login", {
  f <- access_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  local_mocked_bindings(app_state_dir = function() file.path(f$root, "app"), .package = "datapond")
  withr::local_options(datapond.data_pond = f$pond)
  code <- setup_pond_access(f$pond, "owner", first_name = "Pond", last_name = "Owner")
  datapond:::access_activate(f$pond, "owner", code, "a long owner passphrase")
  shiny::testServer(datapond:::server, {
    expect_null(state$active_user_id)
    expect_false(auth$authenticated)
    session$setInputs(active_user="1", required_active_user="1", confirm_active_user_selection=1L)
    expect_null(state$active_user_id)
    expect_false(datapond:::access_allowed("read"))
    session$setInputs(access_login=0L)
    session$setInputs(access_username="owner", access_password="a long owner passphrase", access_login=1L)
    expect_true(auth$authenticated)
    expect_equal(state$active_user_id, 1L)
    # The browser binds the new workspace controls together before access opens.
    html <- xml2::read_html(as.character(datapond:::ui))
    selects <- xml2::xml_find_all(html, "//select")
    defaults <- stats::setNames(lapply(selects, function(node) {
      selected <- xml2::xml_find_all(node, "./option[@selected]")
      if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
      if (!length(selected)) return("")
      xml2::xml_attr(selected, "value")
    }), xml2::xml_attr(selects, "id"))
    defaults$access_workspace_ready <- auth$workspace_nonce
    do.call(session$setInputs, defaults)
    expect_true(datapond:::access_allowed("admin"))
    expect_true(datapond:::is_user_admin(1L, f$pond))
    expect_error(datapond:::set_user_status(1L, FALSE, f$pond), "linked to a login account")
    # Legacy dropdown messages can no longer change identity.
    session$setInputs(active_user="999", required_active_user="999", confirm_active_user_selection=2L)
    expect_equal(state$active_user_id, 1L)
    auth$invalidate()
    expect_false(auth$authenticated)
    expect_null(state$active_user_id)
    expect_false(datapond:::access_allowed("read"))
    chooser <- datapond:::access_directory_input(shiny::reactiveValues(path="/private"))
    expect_error(chooser[["path"]], class = "shiny.silent.error")
    # Choosing another existing pond before login must not migrate its database.
    other <- file.path(f$root, "unconfigured")
    datapond:::init_db(other)
    before <- unname(tools::md5sum(datapond:::get_db_path(other)))
    switch_data_pond(other)
    expect_identical(unname(tools::md5sum(datapond:::get_db_path(other))), before)
    expect_false(auth$authenticated)
    expect_identical(state$data_pond, other)
  })
})

test_that("every workspace observer, renderer and download has an access gate", {
  root <- datapond_test_source_root()
  files <- list.files(file.path(root, "R"), "[.]R$", full.names = TRUE)
  files <- files[!basename(files) %in% "mod_access.R"]
  public_events <- c("input$retry_pond_startup", "input$recovery_known_pond",
    "input$recovery_pond_folder", "input$open_recovery_pond", "input$exit_datapond")
  name <- function(x) {
    if (is.symbol(x)) return(as.character(x))
    if (is.call(x) && identical(x[[1]], as.name("::"))) return(as.character(x[[3]]))
    ""
  }
  gated <- function(x) is.call(x) && identical(x[[1]], as.name("{")) &&
    length(x) >= 2L && is.call(x[[2]]) && name(x[[2]][[1]]) == "access_require"
  checked <- 0L
  walk <- function(x, file) {
    if (missing(x)) return()
    if (!is.call(x) && !is.expression(x)) return()
    if (is.call(x)) {
      kind <- name(x[[1]])
      if (kind %in% c("observe", "observeEvent") ||
          grepl("^render(UI|Text|Print|Table|DT|DataTable|Plot|Leaflet|Image)$", kind)) {
        args <- as.list(x)[-1]
        arg <- args[[if (kind == "observeEvent") 2L else 1L]]
        public <- kind == "observeEvent" && basename(file) == "zzz_app_main.R" &&
          paste(deparse(args[[1]]), collapse = "") %in% public_events
        if (!public) {
          expect_true(gated(arg), info = paste(basename(file), kind, substr(paste(deparse(x), collapse=" "), 1, 100)))
          checked <<- checked + 1L
        }
      }
      if (kind == "downloadHandler") for (arg in as.list(x)[-1]) {
        if (is.call(arg) && identical(arg[[1]], as.name("function"))) {
          expect_true(gated(arg[[3]]), info = paste(basename(file), "download"))
          checked <<- checked + 1L
        }
      }
    }
    for (item in as.list(x)) if (!missing(item) && !is.symbol(item)) walk(item, file)
  }
  for (file in files) walk(parse(file), file)
  expect_gt(checked, 400L)
})
