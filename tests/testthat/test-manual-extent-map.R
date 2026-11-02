test_that("map clicks reject malformed coordinates and wrap world copies", {
  point <- datapond:::manual_extent_map_point
  expect_null(point(NULL))
  expect_null(point(list(lng = NA_real_, lat = 0)))
  expect_null(point(list(lng = c(1, 2), lat = 0)))
  expect_null(point(list(lng = 30, lat = 91)))
  expect_null(point(list(lng = "30", lat = -29)))
  expect_equal(point(list(lng = 391, lat = -29)), list(lng = 31, lat = -29))
})

test_that("two map corners fill the existing form only during selection", {
  local_trusted_access_fixture()

  shiny::testServer(function(input, output, session) {
    messages <- new.env(parent = emptyenv())
    session$sendInputMessage <- function(inputId, message) {
      messages[[inputId]] <- message
    }
    dataset <- shiny::reactiveVal(1L)
    selector <- datapond:::register_manual_extent_map_selector(
      input, output, session, reset_key = dataset
    )
  }, {
    session$flushReact()
    session$setInputs(manual_extent_map_click = list(lng = 31, lat = -29))
    expect_null(selector$first_corner())
    expect_length(messages, 0)

    session$setInputs(manual_extent_map_select = 1)
    session$setInputs(manual_extent_map_click = list(lng = 31, lat = -29))
    expect_equal(selector$first_corner(), list(lng = 31, lat = -29))
    expect_length(messages, 0)

    # A zero-width rectangle must not change the form or finish selection.
    session$setInputs(manual_extent_map_click = list(lng = 31, lat = -30))
    expect_true(selector$selecting())
    expect_length(messages, 0)

    # Corner order does not matter; map coordinates always have a matching CRS.
    session$setInputs(manual_extent_map_click = list(lng = 30, lat = -30))
    expect_false(selector$selecting())
    expect_null(selector$first_corner())
    expect_identical(messages$manual_extent_crs$value, "EPSG:4326")
    expect_equal(as.numeric(messages$manual_extent_xmin$value), 30)
    expect_equal(as.numeric(messages$manual_extent_ymin$value), -30)
    expect_equal(as.numeric(messages$manual_extent_xmax$value), 31)
    expect_equal(as.numeric(messages$manual_extent_ymax$value), -29)

    session$setInputs(manual_extent_map_select = 2)
    session$setInputs(manual_extent_map_click = list(lng = 25, lat = -25))
    session$setInputs(manual_extent_map_cancel = 1)
    expect_false(selector$selecting())
    expect_null(selector$first_corner())
    expect_equal(as.numeric(messages$manual_extent_xmin$value), 30)

    session$setInputs(manual_extent_map_select = 3)
    session$setInputs(manual_extent_map_click = list(lng = 25, lat = -25))
    session$setInputs(manual_extent_select = "another-record")
    expect_false(selector$selecting())
    expect_null(selector$first_corner())

    session$setInputs(manual_extent_map_select = 4)
    session$setInputs(manual_extent_map_click = list(lng = 25, lat = -25))
    session$setInputs(manual_extent_xmin = 24)
    expect_false(selector$selecting())
    expect_null(selector$first_corner())

    session$setInputs(manual_extent_map_select = 5)
    session$setInputs(manual_extent_map_click = list(lng = 25, lat = -25))
    dataset(2L)
    session$flushReact()
    expect_false(selector$selecting())
    expect_null(selector$first_corner())
  })
})
