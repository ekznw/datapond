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
  expect_match(
    html,
    ".datapond-attribute-table-pane table.dataTable",
    fixed = TRUE
  )
  expect_match(
    html,
    "font-size: 11px",
    fixed = TRUE
  )
  expect_match(
    html,
    "white-space: nowrap",
    fixed = TRUE
  )
})


test_that("data-file tables present database fields as readable headings", {
  expect_identical(
    datapond:::friendly_field_label(
      c(
        "data_file_rel_path_full",
        "data_field_name",
        "profile_updated_at"
      )
    ),
    c(
      "Relative path",
      "Field name",
      "Profile updated"
    )
  )

  server_source <- paste(
    deparse(body(datapond:::mod_data_files_server)),
    collapse = "\n"
  )
  expect_match(
    server_source,
    "friendly_field_label",
    fixed = TRUE
  )
})


test_that("opening attributes asks the application to collapse side panels", {
  server_source <- paste(
    deparse(body(datapond:::mod_data_files_server)),
    collapse = "\n"
  )
  expect_match(
    server_source,
    "hideDatapondSidePanels",
    fixed = TRUE
  )
  expect_match(
    server_source,
    'identical(input$data_files_section, "attributes")',
    fixed = TRUE
  )

  app_html <- as.character(datapond:::ui)
  expect_match(
    app_html,
    "function hideDatapondSidePanels()",
    fixed = TRUE
  )
  expect_match(
    app_html,
    "Shiny.addCustomMessageHandler('hideDatapondSidePanels'",
    fixed = TRUE
  )

  app_server_source <- paste(
    deparse(body(datapond:::server)),
    collapse = "\n"
  )
  expect_match(
    app_server_source,
    'input[["data_files-data_files_section"]]',
    fixed = TRUE
  )
})
