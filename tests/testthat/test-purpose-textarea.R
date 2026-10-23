test_that("dataset purpose renders as a multiline text entry", {
  fields <- data.table::data.table(
    field = "purpose",
    label = "Purpose",
    type = "text",
    description = "Describe why the dataset was created.",
    options = NA_character_
  )

  html <- as.character(
    datapond:::generate_section(
      fields,
      ns = identity,
      get_choices_fn = function(field_code) NULL
    )
  )

  expect_match(html, '<textarea id="purpose"', fixed = TRUE)
  expect_match(html, 'rows="5"', fixed = TRUE)
  expect_match(
    html,
    "Describe why the dataset was originally created.",
    fixed = TRUE
  )
})

test_that("purpose uses textarea hydration despite its legacy schema type", {
  expect_true(datapond:::dataset_field_uses_textarea("purpose", "text"))
  expect_true(datapond:::dataset_field_uses_textarea("notes", "textarea"))
  expect_false(datapond:::dataset_field_uses_textarea("edition", "text"))
})
