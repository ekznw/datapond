test_that("population modes share save and folder requirements without inventing metadata", {
  schema <- datapond:::dataset_overview_schema()
  citation <- schema[["Dataset overview"]][["Citation"]]
  about <- schema[["Dataset overview"]][["About"]]
  values <- list(version = "0.1.0", title = "Survey", citation_date = as.Date("2024-01-02"), abstract = "Survey observations.", keyword_object = "biota")
  organisations <- data.frame(code = "EXAMPLE")
  validate <- function(profile, x = values, scope = "dataset", topics = character()) {
    datapond:::validate_metadata_population(x, citation, about, profile, scope, topics, organisations)
  }
  expect_true(validate("rough")$valid)
  for (profile in c("rough", "full")) {
    for (version in list(NULL, "", " ", "unfinished", "1.2")) {
      incomplete <- values
      incomplete$purpose <- "Monitor change."
      incomplete$version <- version
      expect_false(validate(profile, incomplete, topics = "biota")$valid)
    }
  }
  full <- validate("full")
  expect_false(full$valid)
  expect_length(full$missing, 2L) # purpose, ISO topic
  for (field in c("title", "citation_date", "version", "abstract", "keyword_object")) {
    incomplete <- values
    incomplete[[field]] <- " "
    expect_false(validate("rough", incomplete)$valid, info = field)
  }
  for (bad_date in c("2024-02-30", "not a date", "0000-01-01")) {
    invalid <- values
    invalid$citation_date <- bad_date
    expect_false(validate("rough", invalid)$valid)
  }
  for (bad_code in c("", "---", NA_character_)) {
    organisations$code <- bad_code
    expect_false(validate("rough")$valid)
  }
  organisations <- data.frame(code = c("", "SECOND"))
  expect_false(validate("rough")$valid) # a later code cannot repair the first folder component
  organisations <- data.frame(code = "EXAMPLE")
  invalid <- values
  invalid$keyword_object <- "---"
  expect_false(validate("rough", invalid)$valid)
  values$purpose <- "Monitor change."
  values$version <- "1.0.0"
  values$citation_date <- as.Date("2024-01-02")
  expect_true(validate("full", topics = "biota")$valid)
  expect_true(validate("full", scope = "nonGeographicDataset")$valid)
  values$version <- "unfinished"
  expect_false(validate("rough")$valid)
  values$version <- "0.1.0"
  values$temporal_coverage_start_date <- "2024-12-01"
  values$temporal_coverage_end_date <- "2024-01-01"
  expect_false(validate("rough")$valid)
})

test_that("mode changes hide controls in place and publication starts blank", {
  schema <- datapond:::dataset_overview_schema()
  html <- xml2::read_html(as.character(datapond:::generate_section(
    schema[["Dataset overview"]][["Citation"]], identity, function(x) NULL,
    population_visibility = TRUE
  )))
  version <- xml2::xml_find_first(html, "//input[@id='version']")
  expect_identical(xml2::xml_attr(version, "value"), "0.1.0")
  expect_length(xml2::xml_find_all(version, "ancestor::*[@data-display-if]"), 0L)
  version_label <- xml2::xml_find_first(html, "//label[@for='version']")
  expect_match(xml2::xml_text(version_label), "Mandatory", fixed = TRUE)
  expect_match(xml2::xml_text(version_label), "create or save", fixed = TRUE)
  title <- xml2::xml_find_first(html, "//*[@id='title']")
  expect_length(xml2::xml_find_all(title, "ancestor::*[@data-display-if]"), 0L)
  date <- xml2::xml_find_first(html, "//*[@id='citation_date']//input")
  expect_identical(xml2::xml_attr(date, "data-initial-date"), "")
  expect_length(xml2::xml_find_all(date, "ancestor::*[@data-display-if]"), 0L)

  app <- xml2::read_html(as.character(datapond:::ui))
  expect_length(xml2::xml_find_all(app, "//*[@id='population_profile']"), 1L)
  expect_length(xml2::xml_find_all(app, "//*[@id='config_panel']//*[@id='population_profile']"), 1L)
  expect_length(xml2::xml_find_all(app, "//*[@id='main_panel']//*[@id='population_profile']"), 0L)
  choices <- xml2::xml_find_all(app, "//input[@name='population_profile']")
  expect_identical(xml2::xml_attr(choices, "value"), c("rough", "full"))
  expect_identical(xml2::xml_attr(choices, "type"), c("radio", "radio"))
  expect_identical(xml2::xml_attr(choices[1], "checked"), "checked")
  labels <- xml2::xml_find_all(app, "//*[@id='population_profile']//*[@class='population-name']")
  expect_identical(
    xml2::xml_text(labels),
    c("Essential — Prickly Porcupine", "Full — Armoured Pangolin")
  )
  expect_length(xml2::xml_find_all(app, "//*[@id='population_profile']//*[name()='svg']"), 2L)
  org <- xml2::xml_find_first(app, "//*[@id='orgs-org_select']")
  expect_length(xml2::xml_find_all(org, "ancestor::*[@data-display-if]"), 0L)
  authors <- xml2::xml_find_all(app, "//*[@id='authors-author_select']")
  expect_length(authors, 1L)
  expect_length(xml2::xml_find_all(authors, "ancestor::*[@data-display-if]"), 0L)
  for (id in c("iso_profile-topic_categories")) {
    # The repeatable editors stay mounted inside visibility wrappers.
    node <- xml2::xml_find_all(app, paste0("//*[@id='", id, "']"))
    expect_length(node, 1L)
    expect_gt(length(xml2::xml_find_all(node, "ancestor::*[@data-display-if]")), 0L)
  }
})

test_that("hydrating an undated draft clears a previously populated date", {
  messages <- list()
  session <- list(sendInputMessage = function(id, message) {
    messages[[id]] <<- message
  })
  datapond:::hydrate_one_input(session, "citation_date", "date", NA_character_)
  expect_true("value" %in% names(messages$citation_date))
  expect_null(messages$citation_date$value)
})

for (create_action in c("save", "new_dataset")) {
  test_that(paste("minimal drafts save, reload and retain detail through", create_action), {
  local_trusted_access_fixture()

    pond <- tempfile("population-entry-pond-")
    config_dir <- tempfile("population-entry-config-")
    dir.create(pond)
    on.exit(unlink(c(pond, config_dir), recursive = TRUE), add = TRUE)
    old <- options(datapond.data_pond = pond)
    on.exit(options(old), add = TRUE)
    testthat::local_mocked_bindings(
      app_state_dir = function() config_dir, .package = "datapond"
    )
    startup_session <- shiny::MockShinySession$new()
    register_flushed <- startup_session$onFlushed
    unlockBinding("onFlushed", startup_session)
    startup_session$onFlushed <- function(fun, once = TRUE) {
      # A real session invokes flush callbacks outside a reactive consumer.
      # testServer's surrounding isolate otherwise conceals invalid reads.
      register_flushed(function() shiny::maskReactiveContext(fun()), once = once)
    }
    lockBinding("onFlushed", startup_session)
    shiny::testServer(datapond:::server, session = startup_session, {
      # testServer has no browser input bindings. Supply the initial selections
      # from the real UI, including blank organisation/licence selections.
      html <- xml2::read_html(as.character(datapond:::ui))
      selects <- xml2::xml_find_all(html, "//select")
      defaults <- stats::setNames(lapply(selects, function(node) {
        selected <- xml2::xml_find_all(node, "./option[@selected]")
        if (!length(selected)) selected <- xml2::xml_find_all(node, "./option[1]")
        if (!length(selected)) return("")
        xml2::xml_attr(selected, "value")
      }), xml2::xml_attr(selects, "id"))
      defaults$save <- 0L
      defaults$new_dataset <- 0L
      defaults$clear_dataset_form <- 0L
      do.call(session$setInputs, defaults)
      discovery <- xml2::read_html(output$about_discovery_fields$html)
      extra_keywords <- xml2::xml_find_all(discovery, "//details[summary='Additional keywords']")
      expect_length(extra_keywords, 1L)
      for (field in c("keyword_discipline", "keyword_geo")) {
        expect_length(xml2::xml_find_all(extra_keywords, sprintf(".//select[@id='%s']", session$ns(field))), 1L)
      }
      optional <- xml2::read_html(output$about_optional_fields$html)
      expect_length(xml2::xml_find_all(optional, "//select[contains(@id, 'keyword_')]"), 0L)
      expect_length(xml2::xml_find_all(optional, sprintf("//*[@id='%s']", session$ns("update_frequency"))), 1L)
      conn <- datapond:::get_conn(pond)
      DBI::dbExecute(conn, "INSERT INTO persons_tbl (name_first, name_last) VALUES ('Population', 'Tester')")
      person <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() id")$id[1]
      DBI::dbExecute(conn, "INSERT INTO user_tbl (person_id, user_name) VALUES (?, 'populationtester')", params = list(person))
      user <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() id")$id[1]
      DBI::dbDisconnect(conn)
      state$refresh_person <- state$refresh_person + 1L
      state$active_user_id <- user
      session$setInputs(
        metadata_reviewer_user = as.character(user),
        population_profile = "full", version = "0.1.0", title = "Minimal observations",
        abstract = "Observed species.", keyword_object = "biota"
      )
      do.call(session$setInputs, stats::setNames(list(1L), create_action))
      expect_null(state$current_dataset_id)
      expect_equal(nrow(datapond:::get_datasets(pond)), 0L)
      expect_length(list.dirs(file.path(pond, "dev"), recursive = FALSE), 0L)

      session$setInputs(population_profile = "rough")
      do.call(session$setInputs, stats::setNames(list(2L), create_action))
      expect_null(state$current_dataset_id)
      expect_length(list.dirs(file.path(pond, "dev"), recursive = FALSE), 0L)
      session$setInputs(citation_date = as.Date("2024-02-03"))
      conn <- datapond:::get_conn(pond)
      DBI::dbExecute(conn, "INSERT INTO organisation_tbl (organisation_name_full, organisation_name_short, organisation_code) VALUES ('Example Organisation', 'Example', 'EXAMPLE')")
      organisation_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() id")$id[1]
      DBI::dbDisconnect(conn)
      state$refresh_org <- state$refresh_org + 1L
      # Exercise the actual Add action, rather than just the selected dropdown.
      session$setInputs(`orgs-org_select` = as.character(organisation_id), `orgs-org_role` = "custodian")
      do.call(session$setInputs, stats::setNames(list(3L), create_action))
      expect_null(state$current_dataset_id)
      session$setInputs(`orgs-add_org` = 1L)
      do.call(session$setInputs, stats::setNames(list(4L), create_action))
      dataset_id <- state$current_dataset_id
      expect_false(is.null(dataset_id))
      record <- datapond:::get_dataset_record(dataset_id, pond)
      expect_identical(record$title[1], "Minimal observations")
      expect_identical(record$version[1], "0.1.0")
      expect_identical(record$citation_date[1], "2024-02-03")
      expect_identical(record$folder_key[1], "biota_2024_example")
      expect_identical(basename(record$dataset_path[1]), paste0("biota_2024_example__", record$uuid[1]))
      expect_match(record$citation_key[1], "^example_202402_biota_mo_[a-z0-9]{4}$")
      expect_true(is.na(record$purpose[1]))
      expect_identical(record$lifecycle_status[1], "draft")
      expect_true(dir.exists(record$dataset_path[1]))
      expect_equal(nrow(datapond:::get_dataset_citation_dates(dataset_id, pond)), 1L)
      expect_false(datapond:::validate_iso_profile_export(dataset_id, pond)$valid)
      rough_status <- dataset_status_mod$status()
      expect_identical(rough_status$status[rough_status$section == "About"], "complete")
      session$setInputs(population_profile = "full")
      full_status <- dataset_status_mod$status()
      expect_identical(full_status$status[full_status$section == "About"], "partial")
      expect_equal(datapond:::get_dataset_record(dataset_id, pond), record)

      session$setInputs(
        purpose = "Monitor change.", version = "1.2.3", citation_date = as.Date("2025-03-04"),
        `iso_profile-topic_categories` = "biota", metadata_update_reason = "Added detail.",
        metadata_update_type = "metadata_correction", metadata_section = "about"
      )
      iso_wave2_mod$alternate_titles(data.frame(alternate_title = "Field survey"))
      session$setInputs(save = 5L)
      detailed <- datapond:::get_dataset_record(dataset_id, pond)
      expect_identical(detailed$purpose[1], "Monitor change.")
      expect_identical(detailed$version[1], "1.2.3")
      expect_identical(detailed$citation_key, record$citation_key)
      # Draft metadata can update the proposed key, but does not rename the
      # physical folder or regenerate an established citation key.
      expect_identical(detailed$folder_key[1], "biota_2025_example")
      expect_identical(detailed$dataset_path, record$dataset_path)
      expect_equal(nrow(datapond:::get_dataset_alternate_titles(dataset_id, pond)), 1L)
      session$setInputs(population_profile = "rough", abstract = "Updated observations.")
      expect_identical(input$purpose, "Monitor change.")
      expect_identical(iso_wave2_mod$alternate_titles()$alternate_title, "Field survey")
      session$setInputs(save = 6L)
      retained <- datapond:::get_dataset_record(dataset_id, pond)
      expect_identical(retained$abstract[1], "Updated observations.")
      for (field in c("purpose", "version", "citation_date", "uuid", "citation_key", "dataset_path")) {
        expect_identical(retained[[field]], detailed[[field]], info = field)
      }
      expect_identical(datapond:::get_dataset_topic_categories(dataset_id, pond)$topic_category_code, "biota")
      expect_identical(datapond:::get_dataset_alternate_titles(dataset_id, pond)$alternate_title, "Field survey")

      # An existing record cannot lose its version on save in either mode.
      for (profile in c("rough", "full")) {
        session$setInputs(population_profile = profile, version = "")
        session$setInputs(save = if (profile == "rough") 7L else 8L)
        expect_identical(datapond:::get_dataset_record(dataset_id, pond)$version, retained$version)
      }
      session$setInputs(version = "1.2.3")
      session$setInputs(clear_dataset_form = 1L)
      expect_equal(nrow(iso_wave2_mod$alternate_titles()), 0L)
      session$setInputs(dataset_select = as.character(dataset_id))
      expect_identical(state$current_dataset_id, dataset_id)
      expect_identical(iso_wave2_mod$alternate_titles()$alternate_title, "Field survey")
      # The scalar hydration queue is sent to the browser; persisted fields
      # and the repeatable editors are restored by the actual selection path.
      expect_identical(datapond:::get_dataset_record(state$current_dataset_id, pond)$purpose[1], "Monitor change.")
    })
  })
}
