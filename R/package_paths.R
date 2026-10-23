#' Locate a bundled Data Pond resource
#'
#' @param ... Path components below `inst/extdata`.
#' @return A normalized path to the requested installed resource.
#' @keywords internal
datapond_resource <- function(...) {
  path <- system.file("extdata", ..., package = "datapond")

  if (!nzchar(path)) {
    stop(
      "Bundled Data Pond resource was not found: ",
      file.path(...),
      call. = FALSE
    )
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Build a cache-safe URL for a bundled browser asset
#'
#' @param file File name below `inst/www`.
#' @return A resource-path URL carrying a fingerprint of the file contents.
#' @keywords internal
datapond_asset_url <- function(file) {
  path <- system.file("www", file, package = "datapond", mustWork = TRUE)
  fingerprint <- unname(tools::md5sum(path))

  paste0(
    "datapond-assets/",
    file,
    "?v=",
    substr(fingerprint, 1L, 12L)
  )
}

#' Run Data Pond
#'
#' Starts the offline-first Data Pond Shiny application. Dataset files,
#' pond configuration, and the SQLite metadata database remain in the selected
#' local data pond. The startup dialogs offer Exit DataPond to stop this local
#' application and return control to R; the browser tab can then be closed.
#'
#' @param launch.browser Passed to [shiny::runApp()].
#' @param host Host interface on which to listen.
#' @param port Optional TCP port. By default Shiny selects an available port.
#' @param data_pond Optional path to the data pond to open. When `NULL`, Data
#'   Pond uses the last selected pond and then its normal fallback.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()], invisibly.
#' @md
#' @export
run_data_pond <- function(
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port"),
  data_pond = NULL,
  ...
) {
  old_options <- options(
    datapond.data_pond = data_pond,
    datapond.stop_on_exit = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  shiny::runApp(
    datapond_app(),
    launch.browser = launch.browser,
    host = host,
    port = port,
    ...
  )
}
