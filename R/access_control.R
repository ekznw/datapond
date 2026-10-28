# Local account security. Never store credentials in metadata.db: that database
# is deliberately portable. This store belongs to the installation/OS account.
access_path <- function(pond) {
  normalizePath(path.expand(pond), winslash = "/", mustWork = TRUE)
}

access_db_path <- function(pond) {
  file.path(app_state_dir(), "security",
    digest::digest(access_path(pond), algo = "sha256", serialize = FALSE), "access.sqlite")
}

access_connect <- function(pond, create = FALSE) {
  path <- access_db_path(pond)
  if (!file.exists(path) && !create) stop("Access has not been set up for this pond.", call. = FALSE)
  if (create) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE, mode = "0700")
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000")
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  Sys.chmod(path, "0600")
  conn
}

access_initialise <- function(pond) {
  conn <- access_connect(pond, create = TRUE)
  on.exit(DBI::dbDisconnect(conn))
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS accounts (
    id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE,
    first_name TEXT NOT NULL, last_name TEXT NOT NULL, email TEXT NOT NULL DEFAULT '',
    organisation TEXT NOT NULL DEFAULT '', user_id INTEGER UNIQUE,
    role TEXT NOT NULL DEFAULT 'viewer' CHECK(role IN ('viewer','editor','admin')),
    status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','invited','active','disabled','rejected')),
    password_hash TEXT, created_at REAL NOT NULL)")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS tokens (
    account_id INTEGER PRIMARY KEY REFERENCES accounts(id), token_hash TEXT NOT NULL,
    expires_at REAL NOT NULL)")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS sessions (
    token_hash TEXT PRIMARY KEY, account_id INTEGER NOT NULL REFERENCES accounts(id),
    created_at REAL NOT NULL, last_seen REAL NOT NULL)")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS attempts (
    scope TEXT PRIMARY KEY, failures INTEGER NOT NULL, window_start REAL NOT NULL)")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS audit (
    id INTEGER PRIMARY KEY, happened_at REAL NOT NULL, actor_id INTEGER,
    event TEXT NOT NULL, target_id INTEGER, detail TEXT NOT NULL DEFAULT '')")
  invisible(TRUE)
}

access_now <- function() as.numeric(Sys.time())
access_secret <- function() sodium::bin2hex(sodium::random(32))
access_digest <- function(value) digest::digest(value, algo = "sha256", serialize = FALSE)
access_text <- function(value, label, max = 200L, required = TRUE) {
  if (length(value) != 1L || is.na(value)) stop(paste(label, "is required."), call. = FALSE)
  value <- trimws(as.character(value))
  if ((required && !nzchar(value)) || nchar(value) > max) stop(paste("Invalid", label), call. = FALSE)
  value
}
access_username <- function(value) {
  value <- tolower(access_text(value, "username", 100L))
  if (!grepl("^[a-z0-9][a-z0-9._@+-]{2,99}$", value))
    stop("Use 3\u2013100 letters, digits, or . _ @ + - for the username.", call. = FALSE)
  value
}
access_password <- function(password) {
  if (length(password) != 1L || is.na(password) || nchar(password) < 15L || nchar(password, type = "bytes") > 1024L)
    stop("Use a passphrase of at least 15 characters (at most 1024 bytes).", call. = FALSE)
  if (tolower(password) %in% c("passwordpassword", "123456789012345", "password123456789", "abcdefghijklmnop"))
    stop("Choose a less predictable passphrase.", call. = FALSE)
  sodium::password_store(enc2utf8(password))
}
access_verify <- function(hash, password) {
  if (length(password) != 1L || is.na(password) || nchar(password, type = "bytes") > 1024L) return(FALSE)
  tryCatch(isTRUE(sodium::password_verify(hash, enc2utf8(password))), error = function(e) FALSE)
}
access_audit <- function(conn, event, actor = NA_integer_, target = NA_integer_, detail = "") {
  DBI::dbExecute(conn, "INSERT INTO audit (happened_at, actor_id, event, target_id, detail) VALUES (?, ?, ?, ?, ?)",
    params = list(access_now(), actor, event, target, detail))
}

# BEGIN IMMEDIATE serialises last-administrator, token consumption, and approval
# decisions across independent browser sessions and R processes.
access_transaction <- function(conn, code) {
  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  done <- FALSE
  on.exit(if (!done) DBI::dbRollback(conn))
  result <- force(code)
  DBI::dbCommit(conn)
  done <- TRUE
  result
}

access_throttle <- function(conn, scope, limit = 5L) {
  row <- DBI::dbGetQuery(conn, "SELECT * FROM attempts WHERE scope = ?", params = list(scope))
  if (nrow(row) && row$window_start[1] > access_now() - 900 && row$failures[1] >= limit)
    stop("Too many attempts. Wait 15 minutes and try again.", call. = FALSE)
}
access_attempt <- function(conn, scope) {
  DBI::dbExecute(conn, "INSERT INTO attempts VALUES (?, 1, ?)
    ON CONFLICT(scope) DO UPDATE SET
    failures = CASE WHEN window_start < ? THEN 1 ELSE failures + 1 END,
    window_start = CASE WHEN window_start < ? THEN excluded.window_start ELSE window_start END",
    params = list(scope, access_now(), access_now() - 900, access_now() - 900))
}

access_request <- function(pond, username, first_name, last_name, email = "", organisation = "") {
  username <- access_username(username)
  first_name <- access_text(first_name, "first name")
  last_name <- access_text(last_name, "last name")
  email <- access_text(email, "email", required = FALSE)
  organisation <- access_text(organisation, "organisation", required = FALSE)
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  access_transaction(conn, {
    if (DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM accounts WHERE role='admin' AND status IN ('active','invited')")$n == 0L)
      stop("Ask the installation owner to set up the first administrator.")
    access_throttle(conn, "registration", limit = 10L)
    access_attempt(conn, "registration")
    if (DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM accounts WHERE status='pending'")$n >= 100L)
      stop("The request queue is full. Contact the pond administrator.", call. = FALSE)
    changed <- DBI::dbExecute(conn, "INSERT OR IGNORE INTO accounts
      (username, first_name, last_name, email, organisation, created_at) VALUES (?, ?, ?, ?, ?, ?)",
      params = list(username, first_name, last_name, email, organisation, access_now()))
    if (changed == 1L) access_audit(conn, "access_requested", target = DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id)
  })
  # Do not disclose whether a username is already registered.
  invisible(TRUE)
}

access_session <- function(pond, token, permission = "read", touch = FALSE, conn = NULL) {
  tryCatch(access_session_impl(pond, token, permission, touch, conn), error = function(e) NULL)
}

access_session_impl <- function(pond, token, permission = "read", touch = FALSE, conn = NULL) {
  if (length(token) != 1L || is.na(token) || !nzchar(token)) return(NULL)
  if (is.null(conn)) {
    conn <- tryCatch(access_connect(pond), error = function(e) NULL)
    if (is.null(conn)) return(NULL)
    on.exit(DBI::dbDisconnect(conn))
  }
  row <- DBI::dbGetQuery(conn, "SELECT a.id, a.username, a.user_id, a.role, a.first_name, a.last_name,
    s.created_at, s.last_seen FROM sessions s JOIN accounts a ON a.id=s.account_id
    WHERE s.token_hash=? AND a.status='active' AND s.last_seen>? AND s.created_at>?",
    params = list(access_digest(token), access_now() - 1800, access_now() - 43200))
  if (nrow(row) != 1L) return(NULL)
  roles <- switch(permission, read = c("viewer", "editor", "admin"), edit = c("editor", "admin"),
    admin = "admin", download = c("editor", "admin"), stop("Unknown access permission."))
  if (!row$role[1] %in% roles) return(NULL)
  # Parties deactivation or deletion also revokes the effective identity.
  meta <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(pond), flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(meta), add = TRUE)
  user <- DBI::dbGetQuery(meta, "SELECT id FROM user_tbl WHERE id=? AND is_active=1", params = list(row$user_id[1]))
  if (nrow(user) != 1L) return(NULL)
  if (touch) DBI::dbExecute(conn, "UPDATE sessions SET last_seen=? WHERE token_hash=?",
    params = list(access_now(), access_digest(token)))
  row
}

access_login <- function(pond, username, password) {
  username <- access_username(username)
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  scope <- paste0("login:", username)
  access_throttle(conn, scope)
  row <- DBI::dbGetQuery(conn, "SELECT * FROM accounts WHERE username=?", params = list(username))
  # Same costly verification for unknown, pending and disabled accounts.
  hash <- if (nrow(row) && !is.na(row$password_hash[1])) row$password_hash[1] else access_dummy_hash()
  valid <- access_verify(hash, password)
  if (!valid || nrow(row) != 1L || row$status[1] != "active") {
    access_attempt(conn, scope); access_audit(conn, "login_failed")
    stop("Sign-in failed. Check your credentials or contact the administrator.", call. = FALSE)
  }
  token <- access_secret()
  access_transaction(conn, {
    DBI::dbExecute(conn, "DELETE FROM sessions WHERE last_seen<? OR created_at<?",
      params = list(access_now() - 1800, access_now() - 43200))
    DBI::dbExecute(conn, "INSERT INTO sessions VALUES (?, ?, ?, ?)",
      params = list(access_digest(token), row$id[1], access_now(), access_now()))
    DBI::dbExecute(conn, "DELETE FROM attempts WHERE scope=?", params = list(scope))
    access_audit(conn, "login", row$id[1], row$id[1])
  })
  if (is.null(access_session(pond, token))) {
    access_logout(pond, token)
    stop("Sign-in failed. Contact the administrator.", call. = FALSE)
  }
  token
}
access_dummy_hash <- local({
  value <- NULL
  function() {
    if (is.null(value)) value <<- sodium::password_store(access_secret())
    value
  }
})
access_logout <- function(pond, token) {
  if (is.null(token)) return(invisible(NULL))
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  actor <- DBI::dbGetQuery(conn, "SELECT account_id FROM sessions WHERE token_hash=?", params = list(access_digest(token)))
  DBI::dbExecute(conn, "DELETE FROM sessions WHERE token_hash=?", params = list(access_digest(token)))
  if (nrow(actor)) access_audit(conn, "logout", actor$account_id[1])
  invisible(NULL)
}

access_issue_token <- function(conn, account_id, actor = NA_integer_) {
  token <- access_secret()
  DBI::dbExecute(conn, "INSERT INTO tokens VALUES (?, ?, ?) ON CONFLICT(account_id)
    DO UPDATE SET token_hash=excluded.token_hash, expires_at=excluded.expires_at",
    params = list(account_id, access_digest(token), access_now() + 86400))
  DBI::dbExecute(conn, "DELETE FROM sessions WHERE account_id=?", params = list(account_id))
  access_audit(conn, "activation_or_recovery_issued", actor, account_id)
  token
}

access_activate <- function(pond, username, token, password) {
  username <- access_username(username)
  token <- access_text(token, "activation or recovery code", 128L)
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  scope <- paste0("activate:", username)
  access_throttle(conn, scope)
  # Count failures outside the consuming transaction so rollback cannot erase them.
  access_attempt(conn, scope)
  hash <- access_password(password)
  access_transaction(conn, {
    row <- DBI::dbGetQuery(conn, "SELECT a.id FROM accounts a JOIN tokens t ON a.id=t.account_id
      WHERE a.username=? AND a.status IN ('invited','active') AND t.token_hash=? AND t.expires_at>?",
      params = list(username, access_digest(token), access_now()))
    if (nrow(row) != 1L) stop("Invalid or expired code. Contact the administrator.", call. = FALSE)
    DBI::dbExecute(conn, "UPDATE accounts SET password_hash=?, status='active' WHERE id=?", params = list(hash, row$id[1]))
    DBI::dbExecute(conn, "DELETE FROM tokens WHERE account_id=?", params = list(row$id[1]))
    DBI::dbExecute(conn, "DELETE FROM sessions WHERE account_id=?", params = list(row$id[1]))
    DBI::dbExecute(conn, "DELETE FROM attempts WHERE scope IN (?, ?)", params = list(scope, paste0("login:", username)))
    access_audit(conn, "password_set", row$id[1], row$id[1])
  })
  invisible(TRUE)
}

access_admin <- function(conn, pond, token) {
  actor <- access_session(pond, token, "admin", conn = conn)
  if (is.null(actor)) stop("An authenticated pond administrator is required.", call. = FALSE)
  actor$id[1]
}
access_accounts <- function(pond, token) {
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  access_admin(conn, pond, token)
  DBI::dbGetQuery(conn, "SELECT id, username, first_name, last_name, email, organisation, user_id, role, status FROM accounts ORDER BY id")
}

access_link_person <- function(pond, row, person_id = NULL) {
  meta <- get_conn(pond); on.exit(DBI::dbDisconnect(meta))
  DBI::dbWithTransaction(meta, {
    if (is.null(person_id) || !length(person_id) || is.na(person_id) || !nzchar(as.character(person_id))) {
      if (nzchar(trimws(row$email[1])) && nrow(DBI::dbGetQuery(meta,
          "SELECT id FROM persons_tbl WHERE LOWER(TRIM(email))=LOWER(TRIM(?))",
          params = list(row$email[1]))) > 0L)
        stop("A metadata person already has this email. Verify and select that person, or recover their existing account.", call. = FALSE)
      DBI::dbExecute(meta, "INSERT INTO persons_tbl (name_first,name_last,email) VALUES (?,?,?)",
        params = list(row$first_name[1], row$last_name[1], row$email[1]))
      person_id <- DBI::dbGetQuery(meta, "SELECT last_insert_rowid() AS id")$id
    } else {
      person_id <- as.integer(person_id)
      if (nrow(DBI::dbGetQuery(meta, "SELECT id FROM persons_tbl WHERE id=?", params = list(person_id))) != 1L)
        stop("Select an existing person or create a new person.")
    }
    DBI::dbExecute(meta, "INSERT OR IGNORE INTO user_tbl (person_id,user_name,is_active,is_admin) VALUES (?,?,1,0)",
      params = list(person_id, paste0("user_", person_id)))
    DBI::dbExecute(meta, "UPDATE user_tbl SET is_active=1 WHERE person_id=?", params = list(person_id))
    DBI::dbGetQuery(meta, "SELECT id FROM user_tbl WHERE person_id=?", params = list(person_id))$id[1]
  })
}

access_approve <- function(pond, token, account_id, role = "viewer", person_id = NULL) {
  role <- match.arg(role, c("viewer", "editor", "admin"))
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  access_transaction(conn, {
    actor <- access_admin(conn, pond, token)
    row <- DBI::dbGetQuery(conn, "SELECT * FROM accounts WHERE id=? AND status='pending'", params = list(as.integer(account_id)))
    if (nrow(row) != 1L) stop("Select a pending request.")
    # Never merge a request with a seeded identity merely because names match.
    user_id <- access_link_person(pond, row, person_id)
    if (nrow(DBI::dbGetQuery(conn, "SELECT id FROM accounts WHERE user_id=?", params = list(user_id))))
      stop("This person already has an account. Use account recovery instead.")
    DBI::dbExecute(conn, "UPDATE accounts SET user_id=?, role=?, status='invited' WHERE id=?",
      params = list(user_id, role, row$id[1]))
    access_audit(conn, "approved", actor, row$id[1], role)
    access_issue_token(conn, row$id[1], actor)
  })
}

access_update_account <- function(pond, token, account_id, role, status) {
  role <- match.arg(role, c("viewer", "editor", "admin"))
  status <- match.arg(status, c("active", "disabled", "rejected", "pending", "invited"))
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  access_transaction(conn, {
    actor <- access_admin(conn, pond, token)
    row <- DBI::dbGetQuery(conn, "SELECT * FROM accounts WHERE id=?", params = list(as.integer(account_id)))
    if (nrow(row) != 1L) stop("Select an account.")
    if (status == "active" && (is.na(row$password_hash[1]) || is.na(row$user_id[1])))
      stop("Approve and activate this account before enabling it.")
    if (status == "invited" && is.na(row$user_id[1])) stop("Approve this request before inviting it.")
    if (status == "pending" && !is.na(row$user_id[1])) stop("This account is already approved. Use disabled, active or awaiting activation.")
    if (row$role[1] == "admin" && row$status[1] %in% c("active", "invited") &&
        (role != "admin" || status != "active")) {
      others <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM accounts WHERE role='admin' AND status IN ('active','invited') AND id<>?",
        params = list(row$id[1]))$n
      if (others == 0L) stop("Keep at least one active or invited administrator.")
    }
    DBI::dbExecute(conn, "UPDATE accounts SET role=?, status=? WHERE id=?", params = list(role, status, row$id[1]))
    DBI::dbExecute(conn, "DELETE FROM sessions WHERE account_id=?", params = list(row$id[1]))
    DBI::dbExecute(conn, "DELETE FROM tokens WHERE account_id=?", params = list(row$id[1]))
    access_audit(conn, "account_changed", actor, row$id[1], paste(role, status))
  })
  invisible(TRUE)
}

access_recovery <- function(pond, token, account_id) {
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  access_transaction(conn, {
    actor <- access_admin(conn, pond, token)
    row <- DBI::dbGetQuery(conn, "SELECT id FROM accounts WHERE id=? AND status IN ('active','invited')", params = list(as.integer(account_id)))
    if (nrow(row) != 1L) stop("Select an active or invited account.")
    if (identical(as.integer(actor), as.integer(account_id)))
      stop("Ask another administrator to recover your account, or use recover_pond_access() in the local R console.")
    access_issue_token(conn, row$id[1], actor)
  })
}

#' Set up the first local DataPond administrator
#' @param data_pond Existing pond path.
#' @param username Administrator's login name.
#' @param person_id Existing metadata person ID, or NULL for a new person.
#' @param first_name,last_name Name for a new person.
#' @param email Optional contact email.
#' @return A single-use activation code, valid for 24 hours. Keep it private.
#' @export
setup_pond_access <- function(data_pond, username, person_id = NULL, first_name = NULL, last_name = NULL, email = "") {
  if (!is.null(shiny::getDefaultReactiveDomain())) stop("Run setup from the local R console, outside a Shiny session.")
  pond <- validate_existing_data_pond(data_pond)
  init_db(pond)
  access_initialise(pond)
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn))
  username <- access_username(username)
  if (!is.null(person_id)) {
    people <- get_persons(pond)
    person <- people[people$id == as.integer(person_id), , drop = FALSE]
    if (nrow(person) != 1L) stop("Unknown person ID.")
    first_name <- person$name_first[1]; last_name <- person$name_last[1]
    email <- if (is.na(person$email[1])) "" else person$email[1]
  }
  first_name <- access_text(first_name, "first name"); last_name <- access_text(last_name, "last name")
  access_transaction(conn, {
    if (DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM accounts")$n > 0L)
      stop("Access is already configured. Use administrator approval or recover_pond_access().")
    user_id <- access_link_person(pond, data.frame(first_name, last_name, email), person_id)
    DBI::dbExecute(conn, "INSERT INTO accounts (username,first_name,last_name,email,user_id,role,status,created_at)
      VALUES (?,?,?,?,?,'admin','invited',?)", params = list(username,first_name,last_name,email,user_id,access_now()))
    id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id
    access_audit(conn, "console_bootstrap", target = id)
    access_issue_token(conn, id)
  })
}

#' Recover a local DataPond account from the trusted R console
#' @param data_pond Existing pond path.
#' @param username Active or invited account to recover.
#' @return A single-use recovery code, valid for 24 hours. Keep it private.
#' @export
recover_pond_access <- function(data_pond, username) {
  if (!is.null(shiny::getDefaultReactiveDomain())) stop("Run recovery from the local R console, outside a Shiny session.")
  conn <- access_connect(data_pond); on.exit(DBI::dbDisconnect(conn))
  access_transaction(conn, {
    row <- DBI::dbGetQuery(conn, "SELECT id FROM accounts WHERE username=? AND status IN ('active','invited')",
      params = list(access_username(username)))
    if (nrow(row) != 1L) stop("No active or invited account has that username.")
    access_audit(conn, "console_recovery", target = row$id[1])
    access_issue_token(conn, row$id[1])
  })
}

# The trusted local R API remains available for scripts. Every browser callback
# must pass this gate; a Shiny domain with no security context fails closed.
access_context <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
  session$userData$datapond_access
}

access_assert_unlinked_person <- function(pond, person_id) {
  if (!file.exists(access_db_path(pond))) return(invisible(TRUE))
  meta <- get_conn(pond); on.exit(DBI::dbDisconnect(meta))
  users <- DBI::dbGetQuery(meta, "SELECT id FROM user_tbl WHERE person_id=?", params = list(as.integer(person_id)))
  if (!nrow(users)) return(invisible(TRUE))
  conn <- access_connect(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  linked <- DBI::dbGetQuery(conn, "SELECT id FROM accounts WHERE user_id=?", params = list(users$id[1]))
  if (nrow(linked)) stop("This person is linked to a login account. Manage access through Manage accounts; account-linked people cannot be merged or deactivated in Parties.", call. = FALSE)
  invisible(TRUE)
}
access_allowed <- function(permission = "read", pond = NULL, touch = FALSE) {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) return(TRUE)
  auth <- access_context(session)
  if (is.null(auth)) return(FALSE)
  if (!isTRUE(auth$authenticated) || !isTRUE(auth$ready)) return(FALSE)
  path <- shiny::isolate(auth$pond)
  if (!is.null(pond) && !identical(access_path(pond), access_path(path))) return(FALSE)
  !is.null(access_session(path, auth$token, permission, touch = touch))
}
access_require <- function(permission = "read", pond = NULL, touch = FALSE) {
  shiny::req(access_allowed(permission, pond, touch))
  invisible(TRUE)
}

# Third-party file choosers register their own observers. Guard their input
# reads too, including forged refresh/new-directory messages after sign-out.
access_directory_input <- function(input, permission = "admin") {
  structure(list(input = input, permission = permission), class = "datapond_access_input")
}

#' @export
`[[.datapond_access_input` <- function(x, i, ...) {
  raw <- unclass(x)
  access_require(raw$permission, touch = TRUE)
  raw$input[[i]]
}

# DT and Shiny serve cached tables/images through session HTTP routes, outside
# reactive renderers. Revalidate those routes, and bind table caches to the
# identity and pond which created them so old URLs cannot survive a login switch.
access_http_filter <- function(handler, auth, permission = "read", bind_identity = FALSE) {
  original <- shiny::isolate(list(pond = auth$pond, token = auth$token))
  function(...) {
    current <- shiny::isolate(list(pond = auth$pond, token = auth$token,
      ready = auth$ready, authenticated = auth$authenticated))
    valid <- isTRUE(current$ready) && isTRUE(current$authenticated) &&
      (!bind_identity || (identical(current$pond, original$pond) && identical(current$token, original$token))) &&
      !is.null(access_session(current$pond, current$token, permission))
    if (!valid) return(shiny::httpResponse(403L, "text/plain", "Sign in with the required access.",
      headers = list(`Cache-Control` = "no-store", `X-DataPond-Access` = "denied")))
    handler(...)
  }
}

access_replace_session_method <- function(session, name, value) {
  # Shiny locks each R6 session method. Change only this session's entry point,
  # retaining the original implementation inside the authentication wrapper.
  locked <- bindingIsLocked(name, session)
  if (locked) unlockBinding(name, session)
  on.exit(if (locked) lockBinding(name, session))
  assign(name, value, envir = session)
  invisible(NULL)
}
