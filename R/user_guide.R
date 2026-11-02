#' Open the DataPond user guide
#'
#' Opens the bundled, self-contained HTML guide in the default browser. Reading
#' the guide does not require Quarto, an internet connection or a running app.
#'
#' @param browse Whether to open the guide in a browser. Use `FALSE` to obtain
#'   its path without opening it.
#' @return The normalized path to the offline HTML guide, invisibly.
#' @export
open_user_guide <- function(browse = interactive()) {
  if (!is.logical(browse) || length(browse) != 1L || is.na(browse)) {
    stop("browse must be TRUE or FALSE.", call. = FALSE)
  }
  path <- system.file("guide", "offline.html", package = "datapond")
  if (!nzchar(path)) {
    stop("The bundled guide is missing. Reinstall DataPond or, in a source checkout, run Rscript scripts/build_user_guide.R.", call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (browse) utils::browseURL(path)
  invisible(path)
}
