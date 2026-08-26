test_that("attribute table and editor use independently scrolling panes", {
  html <- as.character(
    datapond:::data_files_attributes_ui(
      shiny::NS("attributes")
    )
  )

  expect_match(
    html,
    "datapond-attribute-workspace",
    fixed = TRUE
  )
  expect_match(
    html,
    "datapond-attribute-table-pane",
    fixed = TRUE
  )
  expect_match(
    html,
    "datapond-attribute-editor-pane",
    fixed = TRUE
  )
  expect_match(
    html,
    "overflow: auto",
    fixed = TRUE
  )
  expect_match(
    html,
    "grid-template-columns: minmax(0, 7fr) minmax(320px, 5fr)",
    fixed = TRUE
  )
  expect_match(
    html,
    "@media (max-width: 991px)",
    fixed = TRUE
  )
  expect_match(
    html,
    'id="attributes-data_field_alias"',
    fixed = TRUE
  )
  expect_match(
    html,
    'id="attributes-attribute_table"',
    fixed = TRUE
  )
})
