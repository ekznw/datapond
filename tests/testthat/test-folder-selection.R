test_that("folder selectors share state without repeated saves", {
  shiny::testServer(function(input, output, session) {
    state <- shiny::reactiveValues(dataset_parent_dir = "src")
    log <- new.env()
    log$saved <- character()
    log$sent <- list()
    session$sendInputMessage <- function(inputId, message) {
      log$sent[[inputId]] <- message$value
    }
    dataset_folder_selection_server(input, session, state, function(parent) {
      log$saved <- c(log$saved, parent)
      state$dataset_parent_dir <- parent
    })
  }, {
    session$flushReact()
    expect_identical(log$sent$dataset_parent_dir, "src")
    expect_identical(log$sent$guide_dataset_parent_dir, "src")
    # Delayed insertion after sign-in sends the HTML's dev default first.
    session$setInputs(access_workspace_ready = "first-login",
      dataset_parent_dir = "dev", guide_dataset_parent_dir = "dev")
    expect_identical(state$dataset_parent_dir, "src")
    expect_identical(log$sent$dataset_parent_dir, "src")
    expect_identical(log$sent$guide_dataset_parent_dir, "src")
    session$setInputs(dataset_parent_dir = "src", guide_dataset_parent_dir = "src")
    expect_length(log$saved, 0L)
    session$setInputs(guide_dataset_parent_dir = "dev")
    expect_identical(state$dataset_parent_dir, "dev")
    expect_identical(log$sent$dataset_parent_dir, "dev")
    session$setInputs(dataset_parent_dir = "dev")
    expect_identical(log$saved, "dev")
    session$setInputs(dataset_parent_dir = "src")
    expect_identical(state$dataset_parent_dir, "src")
    expect_identical(log$sent$guide_dataset_parent_dir, "src")
    session$setInputs(guide_dataset_parent_dir = "src")
    expect_identical(log$saved, c("dev", "src"))
    # A pond switch refreshes both controls without saving the previous pond's default.
    state$dataset_parent_dir <- "dev"
    session$flushReact()
    expect_identical(log$sent$dataset_parent_dir, "dev")
    expect_identical(log$sent$guide_dataset_parent_dir, "dev")
    session$setInputs(dataset_parent_dir = "dev", guide_dataset_parent_dir = "dev")
    expect_identical(log$saved, c("dev", "src"))
    state$dataset_parent_dir <- "src"
    session$setInputs(access_workspace_ready = "second-login",
      dataset_parent_dir = "dev", guide_dataset_parent_dir = "dev")
    expect_identical(state$dataset_parent_dir, "src")
    expect_identical(log$sent$dataset_parent_dir, "src")
    expect_identical(log$sent$guide_dataset_parent_dir, "src")
    expect_identical(log$saved, c("dev", "src"))
  })
})

test_that("configuration groups folder controls and keeps navigation compact", {
  config <- xml2::read_html(as.character(config_panel_ui()))
  lifecycle <- xml2::xml_find_first(config, "//details[summary='Lifecycle folder status']")
  expect_length(xml2::xml_find_all(lifecycle, ".//select[@id='dataset_parent_dir']"), 1L)
  expect_length(xml2::xml_find_all(lifecycle, ".//div[@class='lifecycle-folder-card']"), 3L)
  expect_length(xml2::xml_find_all(lifecycle, ".//i[contains(@class, 'fa-folder')]"), 3L)
  expect_length(xml2::xml_find_all(config, "//h4[.='User guide' or .='New dataset defaults']"), 0L)
  expect_length(xml2::xml_find_all(config, "//summary[.='Help']"), 0L)
  expect_length(xml2::xml_find_all(config,
    "//*[@id='current_pond_name']/parent::div/following-sibling::div[1]//*[@id='current_path']"), 1L)
  expect_length(xml2::xml_find_all(config,
    "//details[summary='Document tools']/following-sibling::div[1]/h4[.='Display & sound']"), 1L)
  app <- xml2::read_html(as.character(datapond:::ui))
  gear <- xml2::xml_find_first(app, "//*[@id='toggle_config']")
  expect_identical(trimws(xml2::xml_text(gear)), "")
  expect_length(xml2::xml_find_all(gear, ".//i[contains(@class, 'fa-gear')]"), 1L)
  guide <- xml2::xml_find_first(app, "//*[@id='open_user_guide']")
  expect_identical(trimws(xml2::xml_text(guide)), "?")
  expect_identical(xml2::xml_attr(guide, "href"), "datapond-guide/index.html")
  expect_identical(xml2::xml_attr(guide, "aria-label"), "Open user guide")
  expect_length(xml2::xml_find_all(app,
    "//*[@id='guide_dataset_parent_dir']/ancestor::div[contains(@class,'dataset-guide-primary-actions')]//*[@id='guide_create_dataset_folder']"), 1L)
})
