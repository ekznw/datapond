# Apply project exclusions before R CMD build makes its initial source copy.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
source <- normalizePath(args[[1]], mustWork = TRUE)
destination <- args[[2]]
artifacts <- normalizePath(args[[3]], mustWork = TRUE)
patterns <- readLines(file.path(source, ".Rbuildignore"), warn = FALSE)
patterns <- patterns[nzchar(patterns)]

copy_directory <- function(relative = "") {
  entries <- list.files(file.path(source, relative), all.files = TRUE,
                        no.. = TRUE)
  for (entry in entries) {
    path <- if (nzchar(relative)) file.path(relative, entry) else entry
    if (any(vapply(patterns, function(pattern) {
      grepl(pattern, path, perl = TRUE, ignore.case = TRUE)
    }, logical(1)))) next
    from <- file.path(source, path)
    # Also exclude a caller-supplied output directory inside the source tree.
    if (normalizePath(from, mustWork = TRUE) == artifacts) next
    to <- file.path(destination, path)
    if (dir.exists(from)) {
      dir.create(to, recursive = TRUE, showWarnings = FALSE)
      copy_directory(path)
    } else if (!file.copy(from, to, copy.mode = TRUE, copy.date = TRUE)) {
      stop("Could not stage package file: ", path)
    }
  }
}
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
copy_directory()
