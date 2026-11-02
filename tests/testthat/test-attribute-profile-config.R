test_that("profiling configuration persists per pond and rejects stale pond input", {
  root <- tempfile("profile-config-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  first <- file.path(root, "first")
  second <- file.path(root, "second")
  datapond:::init_db(first)
  datapond:::init_db(second)
  config <- datapond:::read_pond_config(first)
  config$attribute_profile$max_example_values <- 3L
  datapond:::write_pond_config(first, config)
  permissions <- new.env()
  permissions$allowed <- TRUE
  local_mocked_bindings(access_require = function(permission, ...) {
    expect_identical(permission, "edit")
    shiny::req(permissions$allowed)
  }, .package = "datapond")
  state <- shiny::reactiveValues(data_pond = first, pond_config = config)
  shiny::testServer(datapond:::mod_attribute_profile_config_server,
    args = list(state = state), {
      session$flushReact()
      expect_match(output$setting$html, "Enable attribute profiling", fixed = TRUE)
      session$setInputs(pond = first, enabled = TRUE, save = 1)
      expect_true(datapond:::read_pond_config(first)$attribute_profile$enabled)
      expect_true(state$pond_config$attribute_profile$enabled)
      expect_identical(state$pond_config$attribute_profile$max_example_values, 3L)

      state$data_pond <- second
      state$pond_config <- datapond:::read_pond_config(second)
      session$flushReact()
      expect_false(grepl('checked="checked"', output$setting$html, fixed = TRUE))
      session$setInputs(save = 2)
      expect_false(datapond:::read_pond_config(second)$attribute_profile$enabled)
      session$setInputs(pond = second, enabled = TRUE, save = 3)
      expect_true(datapond:::read_pond_config(second)$attribute_profile$enabled)
      permissions$allowed <- FALSE
      session$setInputs(enabled = FALSE, save = 4)
      expect_true(datapond:::read_pond_config(second)$attribute_profile$enabled)
      permissions$allowed <- TRUE
      session$setInputs(save = 5)
      expect_false(datapond:::read_pond_config(second)$attribute_profile$enabled)
      expect_true(datapond:::read_pond_config(first)$attribute_profile$enabled)
    })
})
