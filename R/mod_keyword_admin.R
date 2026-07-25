mod_keyword_admin_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Keyword administration"),
    shiny::uiOutput(ns("admin_body"))
  )
}


mod_keyword_admin_server <- function(
  id,
  state,
  vocab_path = datapond_resource("vocab_keywords.csv")
) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_id <- shiny::reactiveVal(NULL)
    local_refresh <- shiny::reactiveVal(0L)

    current_user_is_admin <- shiny::reactive({
      state$refresh_person

      is_user_admin(
        user_id = state$active_user_id,
        base_path = state$data_pond
      )
    })

    output$admin_body <- shiny::renderUI({
      if (!isTRUE(current_user_is_admin())) {
        return(
          shiny::tags$div(
            class = "alert alert-warning",
            paste(
              "Administrator access is required to edit or reseed",
              "the controlled keyword vocabulary."
            )
          )
        )
      }

      ns <- session$ns

      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::selectInput(
              ns("field_filter"),
              "Vocabulary field",
              choices = c(
                "All fields" = "",
                "Object" = "keyword_object",
                "Discipline" = "keyword_discipline",
                "Topic" = "keyword_topic",
                "Geography" = "keyword_geo"
              )
            )
          ),
          shiny::column(
            width = 8,
            shiny::tags$p(
              style = "margin-top:28px; color:#666;",
              paste(
                "Pending terms were entered in dataset metadata but",
                "have not yet been approved for general use."
              )
            )
          )
        ),

        DT::DTOutput(ns("vocab_table")),
        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::selectInput(
              ns("edit_field"),
              "Field",
              choices = c(
                "Object" = "keyword_object",
                "Discipline" = "keyword_discipline",
                "Topic" = "keyword_topic",
                "Geography" = "keyword_geo"
              )
            ),
            shiny::textInput(ns("option_value"), "Stored value"),
            shiny::textInput(ns("option_label"), "Display label")
          ),
          shiny::column(
            width = 4,
            shiny::numericInput(
              ns("sort_order"),
              "Sort order",
              value = 100L,
              min = 0L,
              step = 1L
            ),
            shiny::selectInput(
              ns("approval_status"),
              "Approval status",
              choices = c("approved", "pending", "rejected")
            ),
            shiny::checkboxInput(
              ns("is_active"),
              "Active",
              value = TRUE
            )
          ),
          shiny::column(
            width = 4,
            shiny::br(),
            shiny::actionButton(ns("new_term"), "New term"),
            shiny::actionButton(ns("save_term"), "Save term"),
            shiny::actionButton(ns("deactivate_term"), "Deactivate")
          )
        ),

        shiny::hr(),
        shiny::h4("CSV seed management"),

        shiny::actionButton(
          ns("merge_seed"),
          "Merge missing terms from CSV"
        ),

        shiny::checkboxInput(
          ns("confirm_reset"),
          paste(
            "Confirm reset of CSV-managed terms.",
            "Administrator-created terms will be preserved."
          ),
          value = FALSE
        ),

        shiny::actionButton(
          ns("reset_seed"),
          "Reset CSV-managed terms"
        )
      )
    })

    vocab_data <- shiny::reactive({
      local_refresh()
      state$refresh_vocab

      shiny::req(current_user_is_admin())

      field <- input$field_filter %||% ""

      get_keyword_vocab_admin(
        base_path = state$data_pond,
        field_code = if (nzchar(field)) field else NULL
      )
    })

    output$vocab_table <- DT::renderDT({
      df <- vocab_data()

      DT::datatable(
        df,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        options = list(
          pageLength = 15L,
          scrollX = TRUE
        )
      )
    })

    shiny::observeEvent(input$vocab_table_rows_selected, {
      row_number <- input$vocab_table_rows_selected
      df <- vocab_data()

      if (length(row_number) == 0L ||
          row_number < 1L ||
          row_number > nrow(df)) {
        return()
      }

      row <- df[row_number, , drop = FALSE]
      selected_id(as.integer(row$id[1]))

      shiny::updateSelectInput(
        session,
        "edit_field",
        selected = row$field_code[1]
      )

      shiny::updateTextInput(
        session,
        "option_value",
        value = row$option_value[1] %||% ""
      )

      shiny::updateTextInput(
        session,
        "option_label",
        value = row$option_label[1] %||% ""
      )

      shiny::updateNumericInput(
        session,
        "sort_order",
        value = row$sort_order[1] %||% 100L
      )

      shiny::updateSelectInput(
        session,
        "approval_status",
        selected = row$approval_status[1] %||% "pending"
      )

      shiny::updateCheckboxInput(
        session,
        "is_active",
        value = isTRUE(as.integer(row$is_active[1]) == 1L)
      )
    })

    clear_editor <- function() {
      selected_id(NULL)

      shiny::updateTextInput(session, "option_value", value = "")
      shiny::updateTextInput(session, "option_label", value = "")
      shiny::updateNumericInput(session, "sort_order", value = 100L)

      shiny::updateSelectInput(
        session,
        "approval_status",
        selected = "approved"
      )

      shiny::updateCheckboxInput(
        session,
        "is_active",
        value = TRUE
      )
    }

    refresh_vocab <- function() {
      local_refresh(local_refresh() + 1L)
      state$refresh_vocab <- state$refresh_vocab + 1L
    }

    shiny::observeEvent(input$new_term, {
      shiny::req(current_user_is_admin())
      clear_editor()
    })

    shiny::observeEvent(input$save_term, {
      shiny::req(current_user_is_admin())

      tryCatch({
        saved_id <- save_keyword_vocab_term(
          base_path = state$data_pond,
          user_id = state$active_user_id,
          field_code = input$edit_field,
          option_value = input$option_value,
          option_label = input$option_label,
          sort_order = input$sort_order,
          is_active = input$is_active,
          approval_status = input$approval_status,
          keyword_id = selected_id()
        )

        selected_id(saved_id)
        refresh_vocab()

        shiny::showNotification(
          "Keyword term saved.",
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          conditionMessage(e),
          type = "error"
        )
      })
    })

    shiny::observeEvent(input$deactivate_term, {
      shiny::req(current_user_is_admin(), selected_id())

      set_keyword_vocab_active(
        keyword_id = selected_id(),
        is_active = FALSE,
        user_id = state$active_user_id,
        base_path = state$data_pond
      )

      refresh_vocab()

      shiny::showNotification(
        "Keyword term deactivated.",
        type = "message"
      )
    })

    shiny::observeEvent(input$merge_seed, {
      shiny::req(current_user_is_admin())

      result <- merge_keyword_vocab_seed(
        vocab_dt = load_vocab(vocab_path),
        base_path = state$data_pond,
        user_id = state$active_user_id
      )

      refresh_vocab()

      shiny::showNotification(
        paste0(
          "CSV merge complete: ",
          result$inserted,
          " inserted, ",
          result$updated,
          " updated, ",
          result$preserved,
          " administrator-managed terms preserved."
        ),
        type = "message",
        duration = 8
      )
    })

    shiny::observeEvent(input$reset_seed, {
      shiny::req(current_user_is_admin())

      if (!isTRUE(input$confirm_reset)) {
        shiny::showNotification(
          "Select the confirmation checkbox before resetting.",
          type = "warning"
        )
        return()
      }

      result <- reset_keyword_vocab_seed(
        vocab_dt = load_vocab(vocab_path),
        base_path = state$data_pond,
        user_id = state$active_user_id
      )

      refresh_vocab()

      shiny::updateCheckboxInput(
        session,
        "confirm_reset",
        value = FALSE
      )

      shiny::showNotification(
        paste0(
          "CSV-managed terms reset: ",
          result$inserted,
          " inserted and ",
          result$updated,
          " reactivated or updated."
        ),
        type = "message",
        duration = 8
      )
    })
  })
}
