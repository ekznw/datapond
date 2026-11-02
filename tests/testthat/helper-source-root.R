# R CMD check runs copied tests outside the source tree. Its unpacked source
# remains available under 00_pkg_src for evidence checks and subprocess fixtures.
datapond_test_source_root <- function() {
  roots <- c(
    testthat::test_path("..", ".."),
    testthat::test_path("..", "..", "00_pkg_src", "datapond")
  )
  found <- roots[file.exists(file.path(roots, "DESCRIPTION")) &
                   dir.exists(file.path(roots, "R")) &
                   dir.exists(file.path(roots, "tests"))]
  if (!length(found)) stop("The DataPond source tree is required for this test.")
  normalizePath(found[1], mustWork = TRUE)
}
