test_that("organisation editor adds roles and removes the selected relationship", {
  orgs <- data.frame(id = 1:3, organisation_name_full = c("Alpha", "Beta", "Gamma"),
    organisation_name_short = c("A", "B", "G"), organisation_code = c("AA", "BB", "GG"),
    organisation_department = "Research", organisation_address = "Example address")
  testthat::local_mocked_bindings(access_require = function(...) invisible(TRUE),
    get_organisations = function(...) orgs, .package = "datapond")
  state <- shiny::reactiveValues(data_pond = tempdir(), refresh_org = 0)
  shiny::testServer(mod_dataset_org_server, args = list(state = state), {
    session$setInputs(org_select = "1", org_role = "owner", add_org = 1L)
    session$setInputs(org_select = "2", add_org = 2L)
    session$setInputs(org_select = "3", add_org = 3L)
    session$setInputs(org_table_rows_selected = 2L, remove_org = 1L)
    expect_identical(org_list()$id, c(1L, 3L))
    session$setInputs(org_table_rows_selected = integer(), remove_org = 2L)
    session$setInputs(org_table_rows_selected = 99L, remove_org = 3L)
    expect_identical(org_list()$id, c(1L, 3L))
    session$setInputs(org_select = "1", add_org = 4L)
    expect_equal(nrow(org_list()), 2L)
    session$setInputs(org_role = "custodian", add_org = 5L)
    expect_identical(org_list()$role, c("owner", "owner", "custodian"))
    session$setInputs(org_table_rows_selected = 1L, remove_org = 4L)
    expect_identical(org_list()$id, c(3L, 1L))
    expect_identical(org_list()$role, c("owner", "custodian"))
    session$setInputs(org_select = "999", add_org = 6L)
    expect_equal(nrow(org_list()), 2L)
    clear_orgs()
    expect_equal(nrow(org_list()), 0L)
  })
})

test_that("author controls preserve citation order and reject stale selections", {
  people <- data.frame(id = 1:3, name_first = c("Alice", "Bob", "Carol"), name_last = "Example")
  testthat::local_mocked_bindings(access_require = function(...) invisible(TRUE),
    get_persons = function(...) people,
    get_persons_by_ids = function(person_ids, ...) people[people$id %in% person_ids, ],
    .package = "datapond")
  state <- shiny::reactiveValues(data_pond = tempdir(), refresh_person = 0)
  shiny::testServer(mod_authors_server, args = list(state = state), {
    session$setInputs(author_select = "1", add_author = 1L)
    session$setInputs(author_select = "2", add_author = 2L)
    session$setInputs(author_select = "3", add_author = 3L)
    session$setInputs(add_author = 4L)
    expect_identical(author_list()$id, 1:3)
    session$setInputs(author_table_rows_selected = 2L, move_up = 1L)
    expect_identical(author_list()$id, c(2L, 1L, 3L))
    session$setInputs(author_table_rows_selected = 1L, move_down = 1L)
    expect_identical(author_list()$id, 1:3)
    session$setInputs(author_table_rows_selected = 99L, move_up = 2L, remove_author = 1L)
    expect_identical(author_list()$id, 1:3)
    session$setInputs(author_table_rows_selected = 2L, remove_author = 2L)
    expect_identical(author_list()$id, c(1L, 3L))
    expect_identical(author_list()$author_order, 1:2)
    clear_authors()
    expect_equal(nrow(author_list()), 0L)
  })
})
