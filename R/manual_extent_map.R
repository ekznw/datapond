# A map selection edits the existing form; persistence and write permissions
# remain with the manual extent Save action.
manual_extent_map_point <- function(click) {
  if (!is.list(click)) return(NULL)
  valid_number <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x)
  }
  if (!valid_number(click$lng) || !valid_number(click$lat) ||
      abs(click$lat) > 90) return(NULL)

  # Leaflet can report longitudes outside the world after horizontal panning.
  lng <- if (abs(click$lng) <= 180) click$lng else (click$lng + 180) %% 360 - 180
  list(lng = lng, lat = click$lat)
}

register_manual_extent_map_selector <- function(input, output, session, reset_key = NULL) {
  selecting <- shiny::reactiveVal(FALSE)
  first_corner <- shiny::reactiveVal(NULL)
  map_proxy <- function() leaflet::leafletProxy("manual_extent_map", session = session)
  clear_corner <- function() {
    first_corner(NULL)
    leaflet::clearGroup(map_proxy(), "manual_extent_selection")
  }
  cancel_selection <- function() {
    selecting(FALSE)
    clear_corner()
  }

  shiny::observeEvent(input$manual_extent_map_select, {
    access_require("edit", touch = TRUE)
    clear_corner()
    selecting(TRUE)
  })
  shiny::observeEvent(input$manual_extent_map_cancel, { access_require("edit", touch = TRUE); cancel_selection() })
  if (!is.null(reset_key)) {
    shiny::observeEvent(reset_key(), { access_require("read", touch = TRUE); cancel_selection() }, ignoreInit = TRUE)
  }

  # Switching records, clearing the form or typing coordinates cancels an
  # unfinished selection, so its first corner cannot leak into another extent.
  shiny::observeEvent(list(
    input$manual_extent_select, input$manual_extent_new,
    input$manual_extent_crs, input$manual_extent_xmin, input$manual_extent_ymin,
    input$manual_extent_xmax, input$manual_extent_ymax
  ), { access_require("read", touch = TRUE); cancel_selection() }, ignoreInit = TRUE)

  output$manual_extent_map_status <- shiny::renderUI({
    access_require("read")
    message <- if (!selecting()) {
      "Map navigation is active. Choose Select extent on map to start a new rectangle."
    } else if (is.null(first_corner())) {
      "Click the first corner of your extent."
    } else {
      "Click the opposite corner to finish, or cancel to keep the current coordinates."
    }
    shiny::tags$p(role = "status", style = "margin-top:8px;", message)
  })

  shiny::observeEvent(input$manual_extent_map_click, {
    access_require("edit", touch = TRUE)
    if (!selecting()) return()
    corner <- manual_extent_map_point(input$manual_extent_map_click)
    if (is.null(corner)) return()
    first <- first_corner()
    if (is.null(first)) {
      first_corner(corner)
      leaflet::addCircleMarkers(
        map_proxy(), lng = corner$lng, lat = corner$lat,
        group = "manual_extent_selection", radius = 6,
        color = "#526657", fillOpacity = 1,
        label = "First corner: click the opposite corner"
      )
      return()
    }

    coords <- c(
      xmin = min(first$lng, corner$lng), ymin = min(first$lat, corner$lat),
      xmax = max(first$lng, corner$lng), ymax = max(first$lat, corner$lat)
    )
    if (coords[["xmin"]] == coords[["xmax"]] ||
        coords[["ymin"]] == coords[["ymax"]]) {
      shiny::showNotification(
        "Choose an opposite corner with a different longitude and latitude.",
        type = "warning", session = session
      )
      return()
    }

    shiny::updateTextInput(session, "manual_extent_crs", value = "EPSG:4326")
    for (field in names(coords)) {
      shiny::updateNumericInput(session, paste0("manual_extent_", field), value = coords[[field]])
    }
    cancel_selection()
  })

  invisible(list(selecting = selecting, first_corner = first_corner))
}
