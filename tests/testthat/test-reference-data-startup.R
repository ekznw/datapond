test_that("startup party seeds preserve local edits and user permissions", {
  pond <- tempfile("party-preservation-")
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  datapond:::seed_pond_parties(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "UPDATE organisation_tbl SET organisation_address = 'Local address'")
  DBI::dbExecute(conn, "UPDATE persons_tbl SET role_title = 'Local role'")
  DBI::dbExecute(conn, "UPDATE user_tbl SET is_admin = 0, is_active = 0")
  # Simulate an older pond with no first-use marker as well as a marked pond.
  for (old_pond in c(FALSE, TRUE)) {
    if (old_pond) DBI::dbExecute(conn, "DELETE FROM app_config_tbl WHERE config_key = 'party_seed_initialised'")
    tables <- c("organisation_tbl", "persons_tbl", "user_tbl")
    before <- lapply(tables, function(t) DBI::dbReadTable(conn, t))
    result <- datapond:::seed_pond_parties(pond)
    expect_identical(result, list(organisations = 0L, persons = 0L))
    expect_equal(lapply(tables, function(t) DBI::dbReadTable(conn, t)), before)
  }
  expect_equal(DBI::dbGetQuery(conn, paste(
    "SELECT COUNT(*) AS n FROM app_config_tbl",
    "WHERE config_key = 'party_seed_initialised'"
  ))$n, 1L)
})

test_that("startup does not resurrect deleted parties", {
  pond <- tempfile("party-deletions-")
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  datapond:::seed_pond_parties(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  for (table in c("user_tbl", "persons_tbl", "organisation_tbl")) {
    DBI::dbExecute(conn, paste("DELETE FROM", table))
  }
  datapond:::seed_pond_parties(pond)
  for (table in c("user_tbl", "persons_tbl", "organisation_tbl")) {
    expect_equal(nrow(DBI::dbReadTable(conn, table)), 0L)
  }
})

test_that("failed initial seeding rolls back parties and its marker", {
  pond <- tempfile("party-rollback-")
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  testthat::local_mocked_bindings(
    sync_persons_to_db = function(...) stop("Injected person seed failure"),
    .package = "datapond"
  )
  expect_error(datapond:::seed_pond_parties(pond), "Injected person seed failure")
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  for (table in c("organisation_tbl", "persons_tbl", "user_tbl", "app_config_tbl")) {
    expect_equal(nrow(DBI::dbReadTable(conn, table)), 0L)
  }
})
