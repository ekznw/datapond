organisation_management_fixture <- function() {
  pond <- tempfile("organisation-management-pond-")
  dir.create(pond, recursive = TRUE, showWarnings = FALSE)
  datapond:::init_db(pond)
  pond
}

add_test_organisation <- function(pond, short, department, address) {
  datapond:::insert_organisation(
    list(
      full = paste(short, "Full Name"),
      short = short,
      code = gsub("[^A-Za-z0-9]", "", toupper(short)),
      department = department,
      address = address
    ),
    pond
  )
  organisations <- datapond:::get_organisations(pond)
  organisations$id[
    organisations$organisation_name_short == short &
      organisations$organisation_department == department
  ][1]
}

test_that("organisation merge transfers current and legacy references", {
  pond <- organisation_management_fixture()
  duplicate_id <- add_test_organisation(
    pond, "Wrong Org", "Wrong unit", "1 Wrong Road"
  )
  correct_id <- add_test_organisation(
    pond, "Right Org", "Right unit", "2 Right Road"
  )
  person_id <- datapond:::insert_person(
    list(first = "Linked", last = "Person", org_id = duplicate_id),
    pond
  )

  conn <- datapond:::get_conn(pond)
  DBI::dbExecute(
    conn,
    "INSERT INTO entry_tbl (id, uuid, title) VALUES (1, 'org-merge', 'Test')"
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO dataset_organisation_tbl",
      "(dataset_id, organisation_id, role) VALUES (1, ?, 'owner')"
    ),
    params = list(correct_id)
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO dataset_organisation_tbl",
      "(dataset_id, organisation_id, role) VALUES (1, ?, 'owner')"
    ),
    params = list(duplicate_id)
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO dataset_organisation_tbl",
      "(dataset_id, organisation_id, role) VALUES (1, ?, 'custodian')"
    ),
    params = list(duplicate_id)
  )
  DBI::dbExecute(conn, paste(
    "CREATE TABLE legacy_organisation_link_test (",
    "id INTEGER PRIMARY KEY, rights_holder_organisation_id INTEGER)"
  ))
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO legacy_organisation_link_test",
      "(rights_holder_organisation_id) VALUES (?)"
    ),
    params = list(duplicate_id)
  )
  DBI::dbDisconnect(conn)

  merged <- datapond:::merge_organisation_records(
    duplicate_id,
    correct_id,
    pond
  )

  expect_identical(merged$links_moved, 3L)
  expect_identical(merged$duplicate_links_removed, 1L)

  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(
      conn,
      "SELECT organisation_id FROM persons_tbl WHERE id = ?",
      params = list(person_id)
    )$organisation_id,
    correct_id
  )
  expect_equal(
    DBI::dbGetQuery(
      conn,
      "SELECT rights_holder_organisation_id FROM legacy_organisation_link_test"
    )$rights_holder_organisation_id,
    correct_id
  )
  links <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT organisation_id, role FROM dataset_organisation_tbl",
      "ORDER BY role"
    )
  )
  expect_equal(links$organisation_id, rep(correct_id, 2L))
  expect_equal(links$role, c("custodian", "owner"))
  expect_equal(
    DBI::dbGetQuery(
      conn,
      "SELECT id FROM organisation_tbl WHERE id = ?",
      params = list(duplicate_id)
    )$id,
    integer()
  )
})

test_that("only unused organisations can be removed", {
  pond <- organisation_management_fixture()
  used_id <- add_test_organisation(
    pond, "Used Org", "Data", "3 Used Road"
  )
  unused_id <- add_test_organisation(
    pond, "Unused Org", "Archive", "4 Unused Road"
  )
  datapond:::insert_person(
    list(first = "Organisation", last = "User", org_id = used_id),
    pond
  )

  usage <- datapond:::get_organisation_usage(used_id, pond)
  expect_equal(sum(usage$records), 1L)
  expect_error(
    datapond:::remove_organisation_record(used_id, pond),
    "still used"
  )
  expect_true(used_id %in% datapond:::get_organisations(pond)$id)

  expect_true(datapond:::remove_organisation_record(unused_id, pond))
  expect_false(unused_id %in% datapond:::get_organisations(pond)$id)
})

test_that("organisation management UI confirms destructive actions", {
  html <- as.character(datapond:::mod_parties_ui("parties"))
  server_source <- paste(
    deparse(datapond:::mod_parties_server),
    collapse = "\n"
  )

  expect_match(html, "parties-org_address", fixed = TRUE)
  expect_match(html, "Merge or remove organisation", fixed = TRUE)
  expect_match(html, "parties-org_merge_source", fixed = TRUE)
  expect_match(html, "parties-org_merge_target", fixed = TRUE)
  expect_match(html, "parties-org_remove_select", fixed = TRUE)
  expect_match(server_source, "confirm_org_merge", fixed = TRUE)
  expect_match(server_source, "confirm_org_remove", fixed = TRUE)
  expect_match(server_source, "get_organisation_usage", fixed = TRUE)
})
