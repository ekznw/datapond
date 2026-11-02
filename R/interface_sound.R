#' Play the Data Pond milestone sound
#'
#' Sends a browser message that plays the bundled African Finfoot recording.
#' The browser owns the short playback window and fade so the original licensed
#' recording remains unmodified on disk.
#'
#' @param session Active Shiny session.
#' @param reason Short machine-readable description of the milestone.
#' @return `TRUE`, invisibly.
#' @keywords internal
play_finfoot_cue <- function(session, reason = "milestone") {
  session$sendCustomMessage(
    type = "datapond-play-finfoot",
    message = list(reason = as.character(reason)[1])
  )

  invisible(TRUE)
}
