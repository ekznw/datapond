mod_metadata_document_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(
    shiny::tags$summary("Human-readable metadata"),
    shiny::p(class = "help-block", style = "font-size:12px;",
      "Save edits, then save and download the PDF. This also places metadata.pdf and citation.txt in the working dataset folder root.",
      "Open the PDF and inspect the complete dataset folder before freezing. Freezing and archiving preserve these copies without regenerating them.",
      "Development documents are listed under lineage; software and package details remain in those documents."),
    shiny::p(class = "help-block", style = "font-size:12px;",
      "Preparing documents requires Quarto, XeLaTeX and Python 3.9+. Use Configuration \u2192 Document tools to check this computer, or run datapond::setup_document_tools() in R. Missing metadata is shown as 'Not recorded'. Saving replaces previous working copies; frozen releases remain unchanged."),
    shiny::downloadButton(ns("pdf"), "Save PDF to dataset folder & download", class = "datapond-requires-active-user"),
    shiny::downloadButton(ns("citation"), "Save citation to dataset folder & download", class = "datapond-requires-active-user")
  )
}

mod_metadata_document_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    save_document <- function(file, pdf) {
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id))
        stop("Select and save a dataset before downloading metadata.", call. = FALSE)
      path <- save_working_metadata_document(state$current_dataset_id, state$data_pond,
        state$active_user_id, pdf = pdf, download = file)
      shiny::showNotification(paste("Saved to", dirname(path),
        if (pdf) "\u2014 open the downloaded PDF and review the dataset folder before freezing." else "\u2014 citation.txt saved."),
        type = "message", duration = 10)
    }
    protect <- function(expr) tryCatch(force(expr), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      stop(e)
    })
    output$pdf <- shiny::downloadHandler(
      filename = function() { access_require("download"); "metadata.pdf" }, contentType = "application/pdf",
      content = function(file) {
        access_require("download")
        protect(shiny::withProgress(message = "Preparing and saving dataset metadata", value = 0.3,
          save_document(file, TRUE)))
      })
    output$citation <- shiny::downloadHandler(
      filename = function() { access_require("download"); "citation.txt" }, contentType = "text/plain; charset=utf-8",
      content = function(file) { access_require("download"); protect(save_document(file, FALSE)) })
  })
}
