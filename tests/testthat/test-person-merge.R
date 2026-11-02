person_merge_fixture <- function() {
  pond <- tempfile("person-merge-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)
  pond
}

test_that("merging duplicate people preserves person and user references", {
  pond <- person_merge_fixture()
  duplicate_id <- datapond:::insert_person(
    list(first = "Jon", last = "Smyth", org_id = NA_integer_),
    pond
  )
  correct_id <- datapond:::insert_person(
    list(first = "John", last = "Smith", org_id = NA_integer_),
    pond
  )
  datapond:::insert_user(duplicate_id, pond)
  datapond:::insert_user(correct_id, pond)

  conn <- datapond:::get_conn(pond)
  users <- DBI::dbGetQuery(
    conn,
    "SELECT id, person_id FROM user_tbl WHERE person_id IN (?, ?)",
    params = list(duplicate_id, correct_id)
  )
  duplicate_user_id <- users$id[users$person_id == duplicate_id]
  correct_user_id <- users$id[users$person_id == correct_id]
  DBI::dbExecute(
    conn,
    "UPDATE user_tbl SET is_admin = 1 WHERE id = ?",
    params = list(duplicate_user_id)
  )
  DBI::dbExecute(conn, paste(
    "CREATE TABLE person_merge_link_test (",
    "id INTEGER PRIMARY KEY, person_id INTEGER,",
    "FOREIGN KEY (person_id) REFERENCES persons_tbl(id))"
  ))
  DBI::dbExecute(conn, paste(
    "CREATE TABLE user_merge_link_test (",
    "id INTEGER PRIMARY KEY, updated_by_user_id INTEGER,",
    "FOREIGN KEY (updated_by_user_id) REFERENCES user_tbl(id))"
  ))
  DBI::dbExecute(conn, paste(
    "CREATE TABLE legacy_person_link_test (",
    "id INTEGER PRIMARY KEY, reviewer_id INTEGER)"
  ))
  DBI::dbExecute(
    conn,
    "INSERT INTO person_merge_link_test (person_id) VALUES (?)",
    params = list(duplicate_id)
  )
  DBI::dbExecute(
    conn,
    "INSERT INTO user_merge_link_test (updated_by_user_id) VALUES (?)",
    params = list(duplicate_user_id)
  )
  DBI::dbExecute(
    conn,
    "INSERT INTO legacy_person_link_test (reviewer_id) VALUES (?)",
    params = list(duplicate_id)
  )
  DBI::dbDisconnect(conn)

  merged <- datapond:::merge_person_records(
    duplicate_id,
    correct_id,
    pond
  )

  expect_identical(merged$target_user_id, as.integer(correct_user_id))
  expect_identical(merged$person_links_moved, 2L)
  expect_identical(merged$user_links_moved, 1L)

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT id FROM persons_tbl WHERE id = ?",
      params = list(duplicate_id))$id,
    integer()
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT person_id FROM person_merge_link_test")$person_id,
    correct_id
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT reviewer_id FROM legacy_person_link_test")$reviewer_id,
    correct_id
  )
  expect_equal(
    DBI::dbGetQuery(conn, "SELECT updated_by_user_id FROM user_merge_link_test")$updated_by_user_id,
    correct_user_id
  )
  kept_user <- DBI::dbGetQuery(
    conn,
    "SELECT person_id, is_admin FROM user_tbl WHERE id = ?",
    params = list(correct_user_id)
  )
  expect_equal(kept_user$person_id, correct_id)
  expect_equal(kept_user$is_admin, 1L)
})

test_that("a duplicate's sole user account moves to the retained person", {
  pond <- person_merge_fixture()
  duplicate_id <- datapond:::insert_person(
    list(first = "Wrong", last = "Name", org_id = NA_integer_),
    pond
  )
  correct_id <- datapond:::insert_person(
    list(first = "Right", last = "Name", org_id = NA_integer_),
    pond
  )
  datapond:::insert_user(duplicate_id, pond)
  before <- datapond:::get_users(pond)
  user_id <- before$id[before$person_id == duplicate_id]

  merged <- datapond:::merge_person_records(
    duplicate_id,
    correct_id,
    pond
  )
  after <- datapond:::get_users(pond)

  expect_identical(merged$source_user_id, as.integer(user_id))
  expect_identical(merged$target_user_id, as.integer(user_id))
  expect_false(duplicate_id %in% after$person_id)
  expect_true(correct_id %in% after$person_id)
})

test_that("person merge controls require an explicit confirmation", {
  html <- as.character(datapond:::mod_parties_ui("parties"))
  server_source <- paste(
    deparse(datapond:::mod_parties_server),
    collapse = "\n"
  )

  expect_match(html, "Merge duplicate person", fixed = TRUE)
  expect_match(html, "parties-person_merge_source", fixed = TRUE)
  expect_match(html, "parties-person_merge_target", fixed = TRUE)
  expect_match(server_source, "confirm_person_merge", fixed = TRUE)
  expect_match(server_source, "merge_person_records", fixed = TRUE)
})
