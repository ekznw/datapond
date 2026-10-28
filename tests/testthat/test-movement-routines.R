routine_test_profiles <- function(root) {
  lapply(seq_len(3), function(i) {
    source <- file.path(root, paste0("source", i))
    dir.create(source)
    writeLines(paste("component", i), file.path(source, "data.txt"))
    list(name = paste("Component", i), src = source,
         dest = file.path(root, paste0("dest", i)), excludes = character())
  })
}

test_that("legacy components gain identities and retain their operation on export/import", {
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  profiles <- routine_test_profiles(root)
  profiles[[2]]$mode <- "sync"
  path <- file.path(root, "syncs.json")
  movement_json_write(list(syncs = profiles), path)
  imported <- movement_read_profiles(path)
  expect_length(unique(vapply(imported, `[[`, character(1), "id")), 3L)
  expect_equal(vapply(imported, `[[`, character(1), "mode"), c("copy", "sync", "copy"))
  movement_json_write(list(syncs = imported), path)
  again <- movement_read_profiles(path)
  expect_equal(lapply(again, function(x) x[names(x) != "id"]), lapply(imported, function(x) x[names(x) != "id"]))
  expect_false(identical(again[[1]]$id, imported[[1]]$id))
  expect_error(movement_routine_plan(list()), "Select at least one")
  profiles[[2]]$dest <- profiles[[1]]$dest
  expect_error(movement_routine_plan(profiles[1:2]), "overlapping")
  profiles[[2]]$dest <- file.path(root, "another-destination")
  profiles[[2]]$src <- profiles[[1]]$dest
  expect_error(movement_routine_plan(profiles[1:2]), "overlapping")
})

test_that("Shiny can edit, rename, duplicate and remove components by identity", {
  local_trusted_access_fixture()

  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  profiles <- routine_test_profiles(root)
  settings <- file.path(root, "settings"); dir.create(settings)
  saveRDS(profiles, file.path(settings, "movement-routines.rds"))
  testthat::local_mocked_bindings(movement_remotes = function() data.frame(name = character(), type = character()))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() NULL, state_directory = settings
  ), {
    session$setInputs(routine_components_rows_selected = 2L, edit_routine = 1L)
    original_id <- state$routines[[2]]$id
    expect_identical(state$editing_id, original_id)
    # Send the field values that a real browser echoes after loading the editor.
    session$setInputs(routine_name = "Renamed second", source = profiles[[2]]$src,
      destination = profiles[[2]]$dest, excludes = "*.tmp\n**/.git/**", mode = "sync", save_routine = 1L)
    expect_length(state$routines, 3L)
    expect_identical(state$routines[[2]]$id, original_id)
    expect_identical(state$routines[[2]]$name, "Renamed second")
    expect_identical(state$routines[[2]]$mode, "sync")
    expect_equal(state$routines[[2]]$excludes, c("*.tmp", "**/.git/**"))
    expect_length(state$selected_routine_ids, 0L)
    expect_equal(readRDS(routines_file), state$routines)

    session$setInputs(duplicate_routine = 1L)
    expect_length(state$routines, 4L)
    expect_identical(state$routines[[4]]$name, "Renamed second (copy)")
    expect_false(identical(state$routines[[4]]$id, original_id))
    expect_identical(state$routines[[4]]$mode, "sync")
    session$setInputs(routine_components_rows_selected = c(2L, 4L), delete_routines = 1L)
    expect_length(state$routines, 4L) # Nothing is removed before confirmation.
    session$setInputs(confirm_delete_routines = 1L)
    expect_equal(vapply(state$routines, `[[`, character(1), "name"), c("Component 1", "Component 3"))
    expect_identical(state$editing_id, "")
    expect_true(file.exists(file.path(profiles[[2]]$src, "data.txt")))
    expect_equal(readRDS(routines_file), state$routines)
    session$setInputs(select_all_routines = 1L)
    expect_length(state$selected_routine_ids, 2L)
    session$setInputs(clear_routine_selection = 1L)
    expect_length(state$selected_routine_ids, 0L)
    session$setInputs(new_routine = 1L)
    expect_identical(state$editing_id, "")
  })
})

test_that("only selected JSON components run and selection edits require a new preview", {
  local_trusted_access_fixture()

  skip_if(!nzchar(Sys.which("rclone")))
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  profiles <- routine_test_profiles(root)
  # The unselected mirror must never delete its destination's extra files.
  profiles[[2]]$mode <- "sync"
  dir.create(profiles[[2]]$dest)
  extra <- file.path(profiles[[2]]$dest, "keep.txt"); writeLines("keep", extra)
  path <- file.path(root, "syncs.json")
  movement_json_write(list(syncs = profiles), path)
  testthat::local_mocked_bindings(movement_remotes = function() data.frame(name = character(), type = character()))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() NULL, state_directory = file.path(root, "settings")
  ), {
    finish <- function() {
      for (i in seq_len(120)) {
        if (!is.null(state$job)) state$job$process$wait(100)
        session$elapse(600)
        if (!is.null(state$job) && state$job$status != "running") break
      }
    }
    session$setInputs(import = list(datapath = path))
    expect_length(state$routines, 3L)
    expect_null(state$job)
    session$setInputs(preview_routines = 1L)
    expect_match(state$status, "Select at least one")
    session$setInputs(routine_components_rows_selected = c(1L, 3L), preview_routines = 2L)
    finish()
    expect_match(state$status, "Preview complete for 2")
    expect_identical(output$mirror_ack_required, "false")
    expect_identical(state$preview$plan$summary$Component, c("Component 1", "Component 3"))
    expect_length(state$preview$steps, 2L)
    expect_false(dir.exists(profiles[[1]]$dest))
    expect_false(dir.exists(profiles[[3]]$dest))
    session$setInputs(routine_components_rows_selected = 2L, run = 1L)
    expect_match(state$status, "Settings changed")
    expect_true(file.exists(extra))
    session$setInputs(routine_components_rows_selected = c(1L, 3L), preview_routines = 3L)
    finish()
    session$setInputs(run = 2L)
    finish()
    expect_match(state$status, "2 selected flows completed successfully")
    expect_true(file.exists(file.path(profiles[[1]]$dest, "data.txt")))
    expect_true(file.exists(file.path(profiles[[3]]$dest, "data.txt")))
    expect_false(file.exists(file.path(profiles[[2]]$dest, "data.txt")))
    expect_true(file.exists(extra))
    expect_identical(state$job$labels, c("Component 1", "Component 3"))

    # One component can also be run alone; mirror acknowledgement still applies.
    session$setInputs(routine_components_rows_selected = 2L, preview_routines = 4L)
    finish()
    expect_identical(output$mirror_ack_required, "true")
    session$setInputs(delete_ack = FALSE, run = 3L)
    expect_match(state$status, "Acknowledge")
    expect_true(file.exists(extra))
    session$setInputs(delete_ack = TRUE, run = 4L)
    finish()
    expect_match(state$status, "1 selected flow completed successfully")
    expect_identical(output$mirror_ack_required, "false")
    expect_false(file.exists(extra))
    expect_true(file.exists(file.path(profiles[[2]]$dest, "data.txt")))
  })
})

test_that("routine UI separates saved batch selection from the editable transfer", {
  html <- as.character(mod_data_movement_ui("flow"))
  for (label in c("Preview selected flows", "Edit selected flow", "Save flow", "Duplicate flow", "Remove selected flows", "Export flows")) {
    expect_match(html, label, fixed = TRUE)
  }
  expect_match(html, 'id="flow-routine_components"', fixed = TRUE)
  expect_match(html, 'id="flow-transfer_plan"', fixed = TRUE)
})

test_that("Flow separates configuration, flow design and execution without duplicate inputs", {
  html <- xml2::read_html(as.character(mod_data_movement_ui("flow")))
  pane <- function(value) xml2::xml_find_first(html, paste0("//div[@data-value='", value, "']"))
  tabs <- xml2::xml_find_all(html, "//ul[@id='flow-flow_tabs']/li/a")
  expect_identical(xml2::xml_attr(tabs, "data-value"), c("remote", "run", "config", "design"))
  expect_identical(trimws(gsub("[[:space:]]+", " ", xml2::xml_text(tabs))), c("Read remote pond", "Run Flows", "Define connections", "Design flow"))
  expect_length(xml2::xml_find_all(tabs[c(1, 3)], ".//br"), 2L)
  expect_identical(xml2::xml_text(xml2::xml_find_all(html, "//ol[@class='flow-navigation']/li/strong")),
    c("1. Read remote pond", "2. Run Flows", "3. Define connections", "4. Design flow"))
  expect_length(xml2::xml_find_all(pane("config"), ".//*[@id='flow-save_location']"), 1L)
  expect_length(xml2::xml_find_all(pane("design"), ".//*[@id='flow-save_routine']"), 1L)
  expect_length(xml2::xml_find_all(pane("run"), ".//*[@id='flow-run']"), 1L)
  expect_length(xml2::xml_find_all(pane("run"), ".//*[@id='flow-routine_components']"), 1L)
  ids <- xml2::xml_attr(xml2::xml_find_all(html, "//*[@id]"), "id")
  expect_false(anyDuplicated(ids) > 0L)
})

test_that("named local and remote locations persist and populate flow paths", {
  local_trusted_access_fixture()

  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  settings <- file.path(root, "settings")
  testthat::local_mocked_bindings(movement_remotes = function() data.frame(name = "team", type = "onedrive"))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() NULL, state_directory = settings
  ), {
    messages <- list()
    root_session <- session$rootScope()
    root_session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    session$flushReact()
    # Configured connections are usable without creating a folder bookmark.
    expect_match(messages$destination_location$options, "Configured remotes", fixed = TRUE)
    expect_match(messages$source_location$options, 'value="remote:team:"', fixed = TRUE)
    session$setInputs(destination_location = "remote:team:")
    expect_identical(messages$destination$value, "team:")
    session$setInputs(source_location = "remote:team:")
    expect_identical(messages$source$value, "team:")
    expect_length(state$locations, 0L)
    expect_false(file.exists(locations_file))
    # Discovery must not overwrite a subfolder typed by the user.
    session$setInputs(destination = "team:publication", discover = 1L)
    expect_identical(input$destination, "team:publication")
    expect_identical(messages$destination$value, "team:")
    session$setInputs(saved_location = "", location_name = "Local work", location_path = root, save_location = 1L)
    expect_length(state$locations, 1L)
    local_id <- state$locations[[1]]$id
    expect_equal(state$locations[[1]]$path, normalizePath(root))
    expect_equal(messages$saved_location$value, local_id)
    session$setInputs(source_location = local_id)
    expect_identical(messages$source$value, normalizePath(root))

    session$setInputs(saved_location = "", location_name = "Team library", location_path = "team:shared", save_location = 2L)
    expect_length(state$locations, 2L)
    remote_id <- state$locations[[2]]$id
    session$setInputs(destination_location = remote_id)
    expect_identical(messages$destination$value, "team:shared")
    session$setInputs(saved_location = remote_id, location_name = "Team archive", location_path = "team:archive", save_location = 3L)
    expect_length(state$locations, 2L)
    expect_identical(state$locations[[2]]$id, remote_id)
    expect_identical(state$locations[[2]]$path, "team:archive")
    expect_identical(readRDS(locations_file), state$locations)

    session$setInputs(saved_location = "", location_name = "Missing remote", location_path = "missing:folder", save_location = 4L)
    expect_match(state$status, "Connect this remote first")
    expect_length(state$locations, 2L)
    session$setInputs(location_name = "Local work", location_path = root, save_location = 5L)
    expect_match(state$status, "already has that name")
    expect_length(state$locations, 2L)

    # Removing a bookmark must leave saved task paths and files intact.
    session$setInputs(source = root, destination = "team:archive", routine_name = "Daily copy", mode = "copy", excludes = "", save_routine = 1L)
    session$setInputs(saved_location = remote_id, remove_location = 1L)
    expect_length(state$locations, 1L)
    expect_identical(state$routines[[1]]$dest, "team:archive")
    expect_true(dir.exists(root))
  })
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() NULL, state_directory = settings
  ), {
    session$flushReact()
    expect_length(state$locations, 1L)
    expect_identical(state$locations[[1]]$name, "Local work")
    expect_length(state$routines, 1L)
  })
})

test_that("configuration can be hidden and restored while retaining setup and editor values", {
  local_trusted_access_fixture()

  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  testthat::local_mocked_bindings(movement_remotes = function() data.frame(name = character(), type = character()))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() NULL, state_directory = file.path(root, "settings")
  ), {
    messages <- list(); visibility <- list()
    root_session <- session$rootScope()
    root_session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
    root_session$sendChangeTabVisibility <- function(inputId, target, type) visibility[[target]] <<- type
    session$setInputs(location_name = "Laptop", location_path = root, save_location = 1L,
      flow_tabs = "config", show_config = TRUE, source = root)
    session$setInputs(show_config = FALSE)
    session$flushReact()
    expect_identical(visibility$config, "hide")
    expect_identical(messages$flow_tabs$value, "run")
    session$setInputs(show_config = TRUE)
    session$flushReact()
    expect_identical(visibility$config, "show")
    expect_length(state$locations, 1L)
    expect_identical(input$source, root)
    session$setInputs(design_tasks = 1L)
    expect_identical(messages$flow_tabs$value, "design")
    session$setInputs(go_run = 1L)
    expect_identical(messages$flow_tabs$value, "run")
  })
})

test_that("Flow validates required fields for each action before starting work", {
  local_trusted_access_fixture()

  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  testthat::local_mocked_bindings(movement_remotes = function() data.frame(name = character(), type = character()))
  shiny::testServer(mod_data_movement_server, args = list(
    pond = function() root, dataset_path = function() NULL, state_directory = file.path(root, "settings")
  ), {
    session$flushReact()
    session$setInputs(routine_name = "", source = "  ", destination = "", mode = "copy", save_routine = 1L)
    expect_match(state$status, "Required for saving a flow: Flow name, Flow from \u2014 source, Flow to \u2014 destination", fixed = TRUE)
    expect_length(state$routines, 0L)
    expect_false(file.exists(routines_file))
    session$setInputs(preview_transfer = 1L)
    expect_match(state$status, "Required for previewing a flow: Flow from \u2014 source, Flow to \u2014 destination", fixed = TRUE)
    expect_null(state$job)
    session$setInputs(preview_publish = 1L)
    expect_match(state$status, "Required for publishing a pond: Publication destination", fixed = TRUE)
    session$setInputs(location_name = "", location_path = "", save_location = 1L)
    expect_match(state$status, "Required for saving a location: Location name, Local folder or remote folder", fixed = TRUE)
    expect_length(state$locations, 0L)
    expect_false(file.exists(locations_file))
    session$setInputs(remote = "", cache = "", refresh_cache = 1L)
    expect_match(state$status, "Required for connecting or refreshing a remote pond: Remote pond folder, Local pond cache", fixed = TRUE)
    session$setInputs(connection_name = "", connect_library = 1L)
    expect_match(state$status, "Required for connecting a remote library", fixed = TRUE)
    expect_null(state$job)

    # A location is independent of the incomplete remote/task forms.
    session$setInputs(location_name = "Laptop", location_path = root, save_location = 2L)
    expect_length(state$locations, 1L)
    expect_true(file.exists(locations_file))
    # Exclusions are optional when saving an otherwise complete task.
    session$setInputs(routine_name = "Copy work", source = root,
      destination = paste0(root, "-destination"), mode = "copy", excludes = "", save_routine = 2L)
    expect_length(state$routines, 1L)
    expect_length(state$routines[[1]]$excludes, 0L)
  })
})
