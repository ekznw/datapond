author_editor_fixture <- function() {
  data.frame(
    id = c(11L, 22L, 33L),
    name_first = c("Alice", "Bongani", "Carol"),
    name_last = c("Alpha", "Beta", "Gamma"),
    stringsAsFactors = FALSE
  )
}


test_that("author editor row movement preserves sequential citation order", {
  authors <- datapond:::normalise_author_editor_table(author_editor_fixture())
  moved_up <- datapond:::move_author_editor_row(authors, 2L, "up")
  expect_identical(moved_up$id, c(22L, 11L, 33L))
  expect_identical(moved_up$author_order, 1:3)

  moved_down <- datapond:::move_author_editor_row(moved_up, 2L, "down")
  expect_identical(moved_down$id, c(22L, 33L, 11L))
  expect_identical(moved_down$author_order, 1:3)

  expect_identical(
    datapond:::move_author_editor_row(authors, 1L, "up"),
    authors
  )
  expect_identical(
    datapond:::move_author_editor_row(authors, integer(), "down"),
    authors
  )
})


test_that("author editor removes the selected row and renumbers order", {
  authors <- datapond:::normalise_author_editor_table(author_editor_fixture())
  removed <- datapond:::remove_author_editor_row(authors, 2L)
  expect_identical(removed$id, c(11L, 33L))
  expect_identical(removed$author_order, 1:2)
  expect_identical(
    datapond:::remove_author_editor_row(authors, integer()),
    authors
  )
})


test_that("author UI uses selectable DataTable controls", {
  html <- as.character(datapond:::mod_authors_ui("authors"))
  server_source <- paste(deparse(datapond:::mod_authors_server), collapse = "\n")
  expect_match(html, "authors-author_table", fixed = TRUE)
  expect_match(html, "datatables", ignore.case = TRUE)
  expect_match(html, "Move selected up", fixed = TRUE)
  expect_match(html, "Move selected down", fixed = TRUE)
  expect_match(html, "Remove selected", fixed = TRUE)
  expect_match(server_source, "input$author_table_rows_selected", fixed = TRUE)
})


test_that("organisation role dropdown explains each responsibility", {
  choices <- datapond:::dataset_organisation_role_choices()
  expect_identical(
    unname(choices),
    c("owner", "custodian", "publisher", "distributor")
  )
  expect_match(names(choices)[1], "accountable for the dataset", fixed = TRUE)
  expect_match(names(choices)[2], "maintains and safeguards", fixed = TRUE)
  expect_match(names(choices)[3], "formally issues or releases", fixed = TRUE)
  expect_match(names(choices)[4], "provides access", fixed = TRUE)
  expect_match(
    datapond:::dataset_organisation_role_descriptor(),
    "day-to-day maintenance",
    fixed = TRUE
  )
})
