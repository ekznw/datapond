dataset_folder_selection_ui <- function(id) {
  shiny::selectInput(
    id, "Create new folders in",
    choices = c("dev/ \u2014 Development" = "dev", "src/ \u2014 Source / reference" = "src"),
    selected = "dev", selectize = FALSE
  )
}

# Both selectors edit the same pond default. Creation reads the shared state,
# so it does not wait for the other selector's browser update to arrive.
dataset_folder_selection_server <- function(input, session, state, on_change) {
  ids <- c("dataset_parent_dir", "guide_dataset_parent_dir")
  initialized <- stats::setNames(rep(FALSE, length(ids)), ids)
  # The workspace is inserted after sign-in. Its HTML starts at dev; that
  # initial binding must not overwrite the pond's saved default (or a new pond
  # selected during a later sign-in in the same session).
  shiny::observeEvent(input$access_workspace_ready, {
    initialized[] <<- FALSE
    for (id in ids) {
      shiny::updateSelectInput(session, id, selected = state$dataset_parent_dir)
    }
  }, priority = 100)
  lapply(ids, function(id) {
    shiny::observeEvent(input[[id]], {
      if (!initialized[[id]]) {
        initialized[[id]] <<- TRUE
        shiny::updateSelectInput(session, id, selected = state$dataset_parent_dir)
        return()
      }
      parent <- normalise_dataset_parent_dir(input[[id]])
      if (!identical(parent, state$dataset_parent_dir)) on_change(parent)
    }, priority = 10)
  })
  shiny::observe({
    parent <- normalise_dataset_parent_dir(state$dataset_parent_dir)
    for (id in ids) {
      if (!identical(shiny::isolate(input[[id]]), parent)) {
        shiny::updateSelectInput(session, id, selected = parent)
      }
    }
  })
  invisible(NULL)
}

lifecycle_folder_help_ui <- function() {
  card <- function(folder, title, text) {
    shiny::tags$div(class = "lifecycle-folder-card",
      shiny::tags$strong(shiny::icon("folder"), paste(folder, title)),
      shiny::tags$p(text))
  }
  shiny::tagList(
    shiny::tags$div(class = "lifecycle-folder-cards",
      card("src/", "Source", "Received or reference data, kept close to its original form."),
      card("dev/", "Development", "Work in progress: processing, analysis and draft outputs. The default for new folders."),
      card("arc/", "Archive", "Released, frozen or superseded versions. Move datasets here through Lifecycle.")),
    shiny::tags$details(class = "lifecycle-metadata-help",
      shiny::tags$summary("Metadata workflow"),
      shiny::tags$p("Sign in \u2192 create or select a dataset \u2192 complete its metadata \u2192 scan files \u2192 add documents and lineage \u2192 check Dataset status."),
      shiny::tags$p("Save dataset records your edits in metadata update history."))
  )
}
