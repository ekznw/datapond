mod_attribute_profile_config_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("setting")),
    shiny::actionButton(ns("save"), "Save profiling setting"),
    shiny::helpText(paste(
      "Saved for the active pond. Requires editor or administrator access.",
      "In Data files, select a saved file, enable and save",
      "Include in attribute profiling in File review, then use",
      "Profile selected file attributes / raster layers."
    ))
  )
}

mod_attribute_profile_config_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$setting <- shiny::renderUI({
      shiny::req(state$data_pond)
      shiny::tagList(
        shiny::checkboxInput(session$ns("enabled"), "Enable attribute profiling",
          value = isTRUE(state$pond_config$attribute_profile$enabled)),
        # Bind the rendered setting to its pond so a pending browser event
        # cannot save the previous pond's value after a switch.
        shiny::tags$div(style = "display:none;",
          shiny::textInput(session$ns("pond"), NULL, value = state$data_pond))
      )
    })

    shiny::observeEvent(input$save, {
      access_require("edit", touch = TRUE)
      shiny::req(identical(input$pond, state$data_pond), !is.null(input$enabled))
      tryCatch({
        config <- read_pond_config(state$data_pond)
        config$attribute_profile$enabled <- isTRUE(input$enabled)
        write_pond_config(state$data_pond, config)
        state$pond_config <- config
        shiny::showNotification("Attribute profiling setting saved.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Could not save profiling setting:", conditionMessage(e)),
          type = "error")
      })
    }, ignoreInit = TRUE)
  })
}
