test_that("dataset workflow panel exposes identity, actions and ordered shortcuts", {
  html <- as.character(datapond:::dataset_workflow_panel_ui())

  for (id in c(
    "dataset_guide_folder",
    "dataset_guide_citation",
    "guide_start_new_dataset",
    "guide_create_dataset_folder",
    "guide_save_dataset",
    "guide_open_dataset_folder",
    "guide_tab_dataset_intro",
    "guide_tab_licensing",
    "guide_tab_data_files",
    "guide_tab_spatial",
    "guide_tab_supporting",
    "guide_tab_lineage",
    "guide_tab_review",
    "guide_tab_status"
  )) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }

  expect_match(html, "Name the folder", fixed = TRUE)
  expect_match(html, "Use these shortcuts in order", fixed = TRUE)
  expect_match(html, "datapond-requires-active-user", fixed = TRUE)
})


test_that("workflow folder summary distinguishes proposed and created folders", {
  empty <- datapond:::dataset_workflow_folder_summary()
  expect_identical(empty$name, "Not named yet")
  expect_false(empty$created)

  proposed <- datapond:::dataset_workflow_folder_summary(
    folder_key = "species_observations__202608",
    dataset_uuid = "20260828_abc123"
  )
  expect_identical(
    proposed$name,
    "species_observations__202608__20260828_abc123"
  )
  expect_identical(proposed$label, "Proposed folder name")
  expect_false(proposed$created)

  created_path <- file.path(
    tempdir(),
    "dev",
    "species_observations__202608__20260828_abc123"
  )
  created <- datapond:::dataset_workflow_folder_summary(
    dataset_path = created_path,
    folder_key = "ignored"
  )
  expect_identical(created$name, basename(created_path))
  expect_identical(created$detail, created_path)
  expect_true(created$created)
})


test_that("main application provides collapsible guide and stable workflow tabs", {
  html <- as.character(datapond:::ui)

  expect_match(html, 'id="dataset_guide_panel"', fixed = TRUE)
  expect_match(html, 'id="toggle_dataset_guide"', fixed = TRUE)
  expect_match(html, 'id="main_dataset_tabs"', fixed = TRUE)
  expect_match(html, 'id="dataset_edit_sections"', fixed = TRUE)
  expect_match(html, 'class="nav nav-hidden shiny-tab-input"', fixed = TRUE)
  expect_match(html, "Create / edit dataset", fixed = TRUE)
  expect_match(html, 'data-value="dataset_intro"', fixed = TRUE)
  expect_match(html, 'data-value="licensing_access"', fixed = TRUE)
  expect_match(html, 'data-value="dataset_status"', fixed = TRUE)
  expect_match(html, "toggleDatasetGuide", fixed = TRUE)
  expect_match(html, "showDatasetGuide", fixed = TRUE)
  expect_match(
    htmltools::renderTags(datapond:::ui)$head,
    "datapond-click-input",
    fixed = TRUE
  )

  server_source <- paste(deparse(body(datapond:::server)), collapse = "\n")
  expect_match(server_source, "workflow_tab_targets", fixed = TRUE)
  expect_match(server_source, "guide_create_dataset_folder", fixed = TRUE)
  expect_match(server_source, "main_dataset_tabs", fixed = TRUE)
  expect_match(server_source, "dataset_edit_sections", fixed = TRUE)
})


test_that("top navigation is concise and admin tabs are conditional", {
  html <- as.character(datapond:::ui)
  lines <- strsplit(html, "\n", fixed = TRUE)[[1]]
  nav_start <- grep('id="main_dataset_tabs"', lines, fixed = TRUE)[1]
  nav_end <- nav_start - 1L + grep(
    "</ul>",
    lines[nav_start:length(lines)],
    fixed = TRUE
  )[1]
  top_nav <- paste(lines[nav_start:nav_end], collapse = "\n")

  expect_match(top_nav, 'data-value="dataset_workspace"', fixed = TRUE)
  expect_match(top_nav, 'data-value="dataset_status"', fixed = TRUE)
  expect_false(grepl('data-value="dataset_intro"', top_nav, fixed = TRUE))
  expect_false(grepl('data-value="metadata_review"', top_nav, fixed = TRUE))
  expect_false(grepl('data-value="parties"', top_nav, fixed = TRUE))
  expect_false(grepl('data-value="keyword_admin"', top_nav, fixed = TRUE))

  workspace_position <- regexpr(
    'data-value="dataset_workspace"', top_nav, fixed = TRUE
  )[1]
  status_position <- regexpr(
    'data-value="dataset_status"', top_nav, fixed = TRUE
  )[1]
  expect_lt(workspace_position, status_position)

  parties_html <- as.character(datapond:::dataset_admin_parties_tab())
  keywords_html <- as.character(datapond:::dataset_admin_keywords_tab())
  expect_match(parties_html, 'data-value="parties"', fixed = TRUE)
  expect_match(keywords_html, 'data-value="keyword_admin"', fixed = TRUE)

  server_source <- paste(deparse(body(datapond:::server)), collapse = "\n")
  expect_match(server_source, "active_user_is_admin", fixed = TRUE)
  expect_match(server_source, "insertTab", fixed = TRUE)
  expect_match(server_source, "removeTab", fixed = TRUE)
})


test_that("guide step numbers use a centred circular badge", {
  html <- as.character(datapond:::ui)
  expect_match(html, "flex: 0 0 28px", fixed = TRUE)
  expect_match(html, "width: 28px", fixed = TRUE)
  expect_match(html, "height: 28px", fixed = TRUE)
  expect_match(html, "border-radius: 999px", fixed = TRUE)
  expect_match(html, "flex-direction: column", fixed = TRUE)
  expect_match(html, "align-items: center", fixed = TRUE)
  expect_match(html, "justify-content: center", fixed = TRUE)
  expect_match(html, "margin: 0 auto 1px auto", fixed = TRUE)
  expect_match(html, "text-align: center", fixed = TRUE)
  expect_match(html, "line-height: 1", fixed = TRUE)
})
