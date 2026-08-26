test_that("active user identifiers are validated as scalar values", {
  expect_false(datapond:::has_active_user_id(NULL))
  expect_false(datapond:::has_active_user_id(integer()))
  expect_false(datapond:::has_active_user_id(NA_integer_))
  expect_false(datapond:::has_active_user_id(""))
  expect_false(datapond:::has_active_user_id("  "))
  expect_false(datapond:::has_active_user_id(c(1L, 2L)))

  expect_true(datapond:::has_active_user_id(1L))
  expect_true(datapond:::has_active_user_id("2"))
})

test_that("the application exposes the active-user write lock", {
  rendered <- htmltools::renderTags(datapond:::ui)
  body <- as.character(datapond:::ui)

  expect_match(
    rendered$head,
    "datapond-active-user-write-lock",
    fixed = TRUE
  )
  expect_match(
    rendered$head,
    "datapond-write-locked",
    fixed = TRUE
  )
  expect_match(body, "active_user_write_status", fixed = TRUE)

  locked_controls <- gregexpr(
    "datapond-requires-active-user",
    body,
    fixed = TRUE
  )[[1L]]
  expect_gte(sum(locked_controls > 0L), 20L)
})
