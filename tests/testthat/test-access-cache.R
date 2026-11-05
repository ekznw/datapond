test_that("published caches use current administrator access without a cache login", {
  root <- tempfile("cache-access-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  local_mocked_bindings(app_state_dir = function() file.path(root, "settings"), .package = "datapond")
  pond <- file.path(root, "working")
  init_db(pond)
  conn <- get_conn(pond)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (uuid, title) VALUES ('cache-test', 'Published dataset')")
  DBI::dbDisconnect(conn)
  snapshot <- movement_snapshot(pond)
  on.exit(unlink(snapshot$directory, recursive = TRUE), add = TRUE)
  prepared <- movement_cache_prepare(file.path(root, "publication"), file.path(root, "cache"))
  files <- list.files(snapshot$directory, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  expect_true(all(file.copy(files, prepared$directory, recursive = TRUE)))
  movement_cache_commit(prepared, snapshot$manifest)
  current <- movement_cache_current(prepared$cache)
  before <- unname(tools::md5sum(get_db_path(current$directory)))
  expect_false(file.exists(access_db_path(current$directory)))

  password <- "cache access test passphrase"
  code <- setup_pond_access(pond, "owner", first_name = "Pond", last_name = "Owner")
  access_activate(pond, "owner", code, password)
  admin <- access_login(pond, "owner", password)
  access_request(pond, "reader", "Cache", "Reader")
  request <- access_accounts(pond, admin)
  code <- access_approve(pond, admin, request$id[request$username == "reader"], "viewer")
  access_activate(pond, "reader", code, password)
  viewer <- access_login(pond, "reader", password)
  other <- file.path(root, "other-working")
  init_db(other)

  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  auth <- shiny::reactiveValues(pond = pond, token = admin, authenticated = TRUE,
    ready = TRUE, connected_tokens = list())
  session$userData$datapond_access <- auth
  shiny::withReactiveDomain(session, shiny::isolate({
    expect_identical(movement_cache_inventory(prepared$cache)$title, "Published dataset")
    expect_length(auth$connected_tokens, 0L)
    # Another working pond retains its separate authentication boundary.
    expect_error(read_connected_pond_inventory(current$directory), "Sign-in required")
    expect_error(connected_pond_sign_in(current$directory, "owner", password), "read-only remote cache.*Flow")
    expect_error(connected_pond_sign_in(other, "owner", password), "Switch to selected pond")
    expect_false(file.exists(access_db_path(other)))
    expect_false(file.exists(access_db_path(current$directory)))
    expect_identical(unname(tools::md5sum(get_db_path(current$directory))), before)
    auth$token <- viewer
    expect_error(movement_cache_inventory(prepared$cache), class = "shiny.silent.error")
    auth$token <- admin
    access_logout(pond, admin)
    expect_error(movement_cache_inventory(prepared$cache), class = "shiny.silent.error")
  }))
})

