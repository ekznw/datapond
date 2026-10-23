test_that("overview controls work without an external schema resource", {
  testthat::local_mocked_bindings(
    datapond_resource = function(...) stop("Unexpected resource lookup"),
    .package = "datapond"
  )
  schema <- datapond:::dataset_overview_schema()
  fields <- data.table::rbindlist(schema[["Dataset overview"]])
  html <- xml2::read_html(as.character(datapond:::generate_section(
    fields, identity, function(field) NULL
  )))
  expect_length(xml2::xml_find_all(html, "//textarea[@id='purpose']"), 1L)
  expect_length(xml2::xml_find_all(html, "//select[@id='keyword_object']"), 1L)
  for (field in c("keyword_topic", "temporal_coverage_start_date", "temporal_coverage_end_date")) {
    expect_length(xml2::xml_find_all(html, paste0("//*[@id='", field, "']")), 0L)
  }
})

test_that("editing one session's schema cannot change another session's controls", {
  schema <- datapond:::dataset_overview_schema()
  citation <- schema[["Dataset overview"]][["Citation"]]
  original <- data.table::copy(citation)
  citation[, label := "Changed locally"]
  expect_equal(
    datapond:::dataset_overview_schema()[["Dataset overview"]][["Citation"]],
    original
  )
})
