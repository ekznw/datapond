mod_lineage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Dataset lineage"),
    tags$small(
      style = "color:#666; display:block;",
      "Use this section to maintain the current lineage statement for the selected dataset. Supporting documents flagged for lineage are shown below."
    ),
    br(),
    textAreaInput(
      ns("lineage_statement"),
      "Lineage statement",
      placeholder = "Summarise where the dataset came from and how it was produced."
    ),
    textAreaInput(
      ns("source_description"),
      "Source description",
      placeholder = "Describe source datasets, providers, dates, versions, or original materials."
    ),
    textAreaInput(
      ns("source_datasets_text"),
      "Source datasets / inputs",
      placeholder = "List source datasets or inputs used to create or maintain this dataset."
    ),
    textAreaInput(
      ns("process_description"),
      "Process description",
      placeholder = "Describe cleaning, transformation, extraction, modelling, validation, or other processing steps."
    ),
    textAreaInput(
      ns("processing_tools"),
      "Processing tools"
    ),
    textAreaInput(
      ns("processing_scripts"),
      "Processing scripts / notebooks"
    ),
    textAreaInput(
      ns("output_description"),
      "Output description",
      placeholder = "Describe the resulting dataset or outputs."
    ),
    textAreaInput(
      ns("lineage_notes"),
      "Lineage notes"
    ),
    selectInput(
      ns("lineage_status"),
      "Lineage status",
      choices = c(
        "draft",
        "reviewed",
        "approved",
        "needs_attention"
      ),
      selected = "draft"
    ),
    actionButton(ns("save_lineage"), "Save lineage"),
    br(),
    br(),
    h4("Current saved lineage"),
    tableOutput(ns("lineage_preview")),
    h4("Documents supporting lineage"),
    tableOutput(ns("lineage_docs_table"))
  )
}

mod_lineage_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    current_lineage <- reactive({
      state$refresh_lineage
      if (is.null(state$current_dataset_id) ||
          length(state$current_dataset_id) == 0 ||
          is.na(state$current_dataset_id)) {
        return(NULL)
      }
      get_dataset_lineage(
        state$current_dataset_id,
        state$data_pond
      )
    })

    lineage_docs <- reactive({
      state$refresh_supporting_docs
      if (is.null(state$current_dataset_id) ||
          length(state$current_dataset_id) == 0 ||
          is.na(state$current_dataset_id)) {
        return(data.frame(
          message = "No dataset selected."
        ))
      }
      docs <- get_lineage_supporting_documents(
        state$current_dataset_id,
        state$data_pond
      )
      if (nrow(docs) == 0) {
        return(data.frame(
          message = "No supporting documents flagged for lineage."
        ))
      }
      docs
    })

    observeEvent(state$current_dataset_id, {
      x <- current_lineage()
      if (is.null(x) || nrow(x) == 0) {
        updateTextAreaInput(session, "lineage_statement", value = "")
        updateTextAreaInput(session, "source_description", value = "")
        updateTextAreaInput(session, "source_datasets_text", value = "")
        updateTextAreaInput(session, "process_description", value = "")
        updateTextInput(session, "processing_tools", value = "")
        updateTextAreaInput(session, "processing_scripts", value = "")
        updateTextAreaInput(session, "output_description", value = "")
        updateTextAreaInput(session, "lineage_notes", value = "")
        updateSelectInput(session, "lineage_status", selected = "draft")
        return()
      }
      updateTextAreaInput(session, "lineage_statement", value = x$lineage_statement[1] %||% "")
      updateTextAreaInput(session, "source_description", value = x$source_description[1] %||% "")
      updateTextAreaInput(session, "source_datasets_text", value = x$source_datasets_text[1] %||% "")
      updateTextAreaInput(session, "process_description", value = x$process_description[1] %||% "")
      updateTextInput(session, "processing_tools", value = x$processing_tools[1] %||% "")
      updateTextAreaInput(session, "processing_scripts", value = x$processing_scripts[1] %||% "")
      updateTextAreaInput(session, "output_description", value = x$output_description[1] %||% "")
      updateTextAreaInput(session, "lineage_notes", value = x$lineage_notes[1] %||% "")
      updateSelectInput(session, "lineage_status", selected = x$lineage_status[1] %||% "draft")
    }, ignoreInit = FALSE)

    observeEvent(input$save_lineage, {
      req(state$current_dataset_id)

      if (is.null(input$lineage_statement) ||
          length(input$lineage_statement) == 0 ||
          trimws(input$lineage_statement) == "") {
        showNotification(
          "Please provide a lineage statement before saving.",
          type = "error"
        )
        return()
      }

      user_person_id <- get_active_user_person_id(
        state$active_user_id,
        state$data_pond
      )

      save_dataset_lineage(
        dataset_id = state$current_dataset_id,
        data = list(
          lineage_statement = input$lineage_statement,
          source_description = input$source_description,
          source_datasets_text = input$source_datasets_text,
          process_description = input$process_description,
          processing_tools = input$processing_tools,
          processing_scripts = input$processing_scripts,
          output_description = input$output_description,
          lineage_notes = input$lineage_notes,
          lineage_status = input$lineage_status
        ),
        base_path = state$data_pond,
        user_person_id = user_person_id
      )

      insert_metadata_review(
        dataset_id = state$current_dataset_id,
        base_path = state$data_pond,
        reviewer_id = user_person_id,
        reviewer_name = NA_character_,
        reviewer_role = NA_character_,
        update_type = "other",
        update_reason = "Updated dataset lineage information.",
        update_summary = paste0(
          "Lineage status set to ",
          input$lineage_status
        ),
        metadata_section = "lineage",
        metadata_status = input$lineage_status %||% "reviewed",
        source_of_change = "lineage_module",
        app_version = "1.0.20",
        notes = NA_character_
      )

      state$refresh_lineage <- state$refresh_lineage + 1
      state$refresh_metadata_reviews <- state$refresh_metadata_reviews + 1

      showNotification("Lineage saved.", type = "message")
    })

    output$lineage_preview <- renderTable({
      x <- current_lineage()
      if (is.null(x) || nrow(x) == 0) {
        return(NULL)
      }
      x[, c(
        "lineage_status",
        "lineage_statement",
        "updated_at"
      ), drop = FALSE]
    }, striped = TRUE)

    output$lineage_docs_table <- renderTable({
      lineage_docs()
    }, striped = TRUE)

    # clear the lineage forms
    clear_lineage_inputs <- function() {
      updateTextAreaInput(
        session,
        "lineage_statement",
        value = ""
      )

      updateTextAreaInput(
        session,
        "source_description",
        value = ""
      )

      updateTextAreaInput(
        session,
        "source_datasets_text",
        value = ""
      )

      updateTextAreaInput(
        session,
        "process_description",
        value = ""
      )

      updateTextAreaInput(
        session,
        "processing_tools",
        value = ""
      )

      updateTextAreaInput(
        session,
        "processing_scripts",
        value = ""
      )

      updateTextAreaInput(
        session,
        "output_description",
        value = ""
      )

      updateTextAreaInput(
        session,
        "lineage_notes",
        value = ""
      )

      updateSelectInput(
        session,
        "lineage_status",
        selected = "draft"
      )

      invisible(TRUE)
    }

    list(
      clear = clear_lineage_inputs
    )
  })
}