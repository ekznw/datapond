test_that("legacy sharing authority requires review even when other sharing fields are filled", {
  pond <- tempfile("sharing-review-status-")
  datapond:::init_db(pond)
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)
  DBI::dbExecute(conn, "INSERT INTO entry_tbl (id, title) VALUES (1, 'Sharing review')")
  DBI::dbExecute(conn, "INSERT INTO dataset_license_tbl
    (dataset_id, rights_basis_code, license_holder, access_level_code,
     redistribution_permission_code, license_identifier, sensitivity_handling_code)
    VALUES (1, 'review_required', 'Example provider', 'not_distributable',
            'prohibited', 'EKZNW-INTERNAL', 'none')")
  assess <- function() {
    result <- as.data.frame(datapond:::get_dataset_completion_status(1L, pond, NULL))
    result[result$section == "Licensing and sharing", , drop = FALSE]
  }
  pending <- assess()
  expect_identical(pending$status, "partial")
  expect_equal(pending$required_missing, 1L)
  expect_identical(pending$missing_fields, "Sharing authority")
  expect_match(pending$next_action, "Who controls the sharing rights?", fixed = TRUE)
  # Recording reviewed uncertainty completes the assessment without granting sharing permission.
  DBI::dbExecute(conn, "UPDATE dataset_license_tbl SET rights_basis_code = 'rights_unknown' WHERE dataset_id = 1")
  reviewed <- assess()
  expect_identical(reviewed$status, "complete")
  expect_equal(reviewed$required_missing, 0L)
  saved <- DBI::dbGetQuery(conn, "SELECT access_level_code, redistribution_permission_code FROM dataset_license_tbl WHERE dataset_id = 1")
  expect_identical(saved$access_level_code, "not_distributable")
  expect_identical(saved$redistribution_permission_code, "prohibited")
})
