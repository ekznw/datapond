test_that("optional metadata uses collapsed progressive disclosure", {
  html <- as.character(datapond:::metadata_optional_details(
    "Optional example",
    shiny::textInput("example", "Example")
  ))
  expect_match(html, "<details", fixed = TRUE)
  expect_match(html, "datapond-optional-details", fixed = TRUE)
  expect_match(html, "Optional example", fixed = TRUE)
  expect_false(grepl(" open=", html, fixed = TRUE))
})

test_that("specialist editors appear once in their relevant workflow tabs", {
  html <- as.character(datapond:::ui)
  expect_match(html, "Essential metadata", fixed = TRUE)
  expect_match(html, "Optional metadata", fixed = TRUE)
  expect_false(grepl("enable_specialist_iso", html, fixed = TRUE))
  expect_false(grepl("open_iso_quality", html, fixed = TRUE))
  expect_false(grepl("open_iso_specialist", html, fixed = TRUE))
  expect_match(html, "about_optional_fields", fixed = TRUE)
  doc <- xml2::read_html(html)
  intro <- xml2::xml_find_first(doc, "//div[@data-value='dataset_intro']")
  expect_length(xml2::xml_find_all(intro, ".//div[@class='datapond-specialist-metadata']"), 0L)
  expect_false(grepl("Keyword thesauri", xml2::xml_text(intro), fixed = TRUE))
  panels <- xml2::xml_find_all(doc, "//div[@class='datapond-specialist-metadata']/details")
  expect_length(panels, 3L)
  expect_true(all(is.na(xml2::xml_attr(panels, "open"))))
  # Moving the editors must not leave duplicate Shiny bindings in hidden tabs.
  destinations <- list(
    "iso_wave2-quality_template" = c("data_files", "quality"),
    "iso_wave3-catalogue_title" = c("data_files", "attributes"),
    "iso_wave3-vertical_label" = c("spatial_metadata", "dataset_extent"),
    "iso_profile-responsibility_person" = "metadata_review",
    "iso_profile-metadata_uuid" = "metadata_review"
  )
  for (id in names(destinations)) {
    field <- xml2::xml_find_all(doc, sprintf("//*[@id='%s']", id))
    expect_length(field, 1L)
    parents <- xml2::xml_attr(xml2::xml_find_all(field, "ancestor::div[@data-value]"), "data-value")
    expect_true(all(destinations[[id]] %in% parents), info = id)
    expect_false("dataset_intro" %in% parents, info = id)
  }
  controls <- xml2::xml_find_all(panels, ".//input | .//select | .//textarea")
  help_ids <- xml2::xml_attr(controls, "aria-describedby")
  expect_false(anyNA(help_ids))
  for (id in unique(help_ids)) {
    help <- xml2::xml_find_all(doc, sprintf("//*[@id='%s']", id))
    expect_length(help, 1L)
    expect_gt(nchar(xml2::xml_text(help)), 30L)
  }
})

test_that("ISO guidance targets fields in their new workflow tabs", {
  doc <- xml2::read_html(as.character(datapond:::ui))
  cases <- c(
    "Missing metadata encoding" = "metadata_review",
    "Missing metadata creation date" = "metadata_review",
    "Missing area covered" = "metadata_review",
    "Missing dataset language" = "metadata_review",
    "Missing feature catalogue" = "data_files",
    "Missing quality measure" = "data_files",
    "Missing vertical extent" = "spatial_metadata"
  )
  for (message in names(cases)) {
    guidance <- datapond:::iso_export_guidance(message)
    expect_identical(guidance$section, unname(cases[[message]]))
    expect_length(xml2::xml_find_all(doc, sprintf(
      "//div[@data-value='%s']//*[@id='%s']", guidance$section, guidance$field
    )), 1L)
  }
  admin <- as.character(datapond:::keyword_admin_body_ui(shiny::NS("keywords")))
  expect_match(admin, "Thesaurus citation", fixed = TRUE)
  expect_match(admin, "settings apply across the pond", fixed = TRUE)
})

test_that("DataPond-only scale range is collapsed behind the ISO scale", {
  html <- as.character(
    datapond:::data_files_spatial_overview_ui(shiny::NS("spatial"))
  )
  expect_match(html, "ISO representative fraction denominator", fixed = TRUE)
  expect_match(html, "Optional: creation or recommended-use scale range and notes", fixed = TRUE)
  expect_match(html, "datapond-optional-details", fixed = TRUE)
})

test_that("completion status separates essentials from optional and workflow", {
  sections <- c(
    "Citation",
    "ISO data quality",
    "Feature catalogue",
    "Data files",
    "Metadata review"
  )
  expect_identical(
    datapond:::dataset_status_requirement_level(sections),
    c("essential", "optional", "optional", "workflow", "workflow")
  )
  html <- as.character(datapond:::mod_dataset_status_ui("status"))
  expect_match(html, "Essential metadata only", fixed = TRUE)
  expect_match(html, "Everything, including workflow", fixed = TRUE)
  expect_identical(
    datapond:::dataset_status_requirement_level("Spatial metadata"),
    "optional"
  )
})
