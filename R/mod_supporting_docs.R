mod_supporting_documents_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Supporting documents"),
    tags$small(
      style = "color:#666; display:block;",
      "Scans the dataset folder outside data/. Documents in docs/, scripts/ and the dataset root can be listed and classified.",
      "Supporting documents are optional unless chosen as the authoritative lineage source; select those documents in Lineage."
    ),
    br(),
    actionButton(
      ns("scan_docs"),
      "Scan supporting documents",
      class = "datapond-requires-active-user"
    ),
    tags$p(class = "help-block supporting-doc-scroll-help",
      "The document list and review form scroll separately. Scroll the table sideways for more columns. Long text is previewed in the table; select a document to read or edit its full notes."),
    tags$div(class = "supporting-doc-workspace",
      tags$section(class = "supporting-doc-pane supporting-doc-table-pane",
        tabindex = "0", `aria-label` = "Supporting document list",
        h4("Detected supporting documents"),
        DT::DTOutput(ns("tbl_documents")),
        actionButton(ns("accept_default_types"), "Accept all default document types",
          class = "datapond-requires-active-user"),
        tags$small(class = "help-block",
          "Marks all active, automatically classified documents as reviewed. Notes and individual overrides can still be saved in the review form.")
      ),
      tags$section(class = "supporting-doc-pane supporting-doc-editor-pane",
        tabindex = "0", `aria-label` = "Supporting document review form",
        h4("Edit document classification"),
        selectInput(ns("doc_select"), metadata_field_label("Select document", "Conditional", "Select a document before saving its classification or notes."), choices = NULL),
        selectInput(ns("document_type_code"),
          metadata_field_label("Document type", "Conditional", "Choose or accept the document type when saving its classification."),
          choices = supporting_document_type_choices),
        textInput(ns("document_type_user_text"), metadata_field_label("Document type note / override reason", "Conditional", "Required when changing the automatically assigned document type.")),
        checkboxInput(ns("include_in_lineage"), "Include in lineage", value = FALSE),
        checkboxInput(ns("include_in_metadata"), "Include in metadata record", value = TRUE),
        textAreaInput(ns("notes"), "Notes", width = "100%", rows = 8),
        tags$div(class = "supporting-doc-editor-actions",
          actionButton(ns("save_doc_edit"), "Save document classification & notes",
            class = "datapond-requires-active-user"))
      )
    )
  )
}

mod_supporting_documents_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {

    scan_summary <- reactiveVal(NULL)

    # helper functions ----
    prepare_supporting_documents_table <- function(docs) {
      docs <- data.table::as.data.table(
        data.table::copy(docs)
      )

      required_columns <- list(
        id = NA_integer_,
        document_name = NA_character_,
        document_rel_path = NA_character_,
        document_type_auto_code = NA_character_,
        document_type_code = NA_character_,
        document_type_label = NA_character_,
        document_type_auto_text = NA_character_,
        document_type_user_set = 0L,
        document_type_user_text = NA_character_,
        include_in_lineage = 0L,
        include_in_metadata = 0L,
        notes = NA_character_,
        active = 1L,
        classified_by_name = NA_character_,
        reviewed_at = NA_character_,
        reviewed_by_name = NA_character_
      )

      for (column_name in names(required_columns)) {
        if (!column_name %in% names(docs)) {
          docs[, (column_name) := required_columns[[column_name]]]
        }
      }

      clean_text <- function(x) {
        x <- as.character(x)

        x[
          is.na(x) |
            trimws(x) == ""
        ] <- ""

        x
      }

      docs[
        ,
        document_type_label :=
          clean_text(document_type_label)
      ]

      docs[
        ,
        document_type_auto_text :=
          clean_text(document_type_auto_text)
      ]

      docs[
        ,
        document_type_user_text :=
          clean_text(document_type_user_text)
      ]

      docs[
        ,
        notes :=
          clean_text(notes)
      ]

      docs[
        ,
        classified_by_name :=
          clean_text(classified_by_name)
      ]

      docs[
        ,
        reviewed_by_name :=
          clean_text(reviewed_by_name)
      ]

      docs[
        ,
        review_status :=
          data.table::fifelse(
            !is.na(reviewed_at) &
              nzchar(trimws(as.character(reviewed_at))),
            "Reviewed",
            "Needs review"
          )
      ]

      docs[
        ,
        classification_source :=
          data.table::fifelse(
            as.integer(document_type_user_set) == 1L,
            "User override",
            "Automatic"
          )
      ]

      docs[
        ,
        override_reason :=
          data.table::fifelse(
            as.integer(document_type_user_set) == 1L,
            document_type_user_text,
            ""
          )
      ]

      docs[
        ,
        include_in_lineage_label :=
          data.table::fifelse(
            as.integer(include_in_lineage) == 1L,
            "Yes",
            "No"
          )
      ]

      docs[
        ,
        include_in_metadata_label :=
          data.table::fifelse(
            as.integer(include_in_metadata) == 1L,
            "Yes",
            "No"
          )
      ]

      docs[
        ,
        active_label :=
          data.table::fifelse(
            as.integer(active) == 1L,
            "Active",
            "Missing"
          )
      ]

      docs[
        ,
        .(
          id,
          document_name,
          document_rel_path,
          document_type = document_type_label,
          automatic_classification =
            document_type_auto_text,
          review_status,
          classification_source,
          override_reason,
          notes,
          include_in_lineage =
            include_in_lineage_label,
          include_in_metadata =
            include_in_metadata_label,
          status = active_label,
          reviewed_by =
            reviewed_by_name
        )
      ]
    }

    observeEvent(input$scan_docs, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "scanning supporting documents"
      )) {
        return()
      }

      if (is.null(state$current_dataset_id) ||
          length(state$current_dataset_id) == 0 ||
          is.na(state$current_dataset_id)) {
        showNotification(
          "Please save the dataset record before scanning supporting documents.",
          type = "warning"
        )
        scan_summary(data.frame(
          message = "No saved dataset record is selected. Save the dataset before scanning supporting documents."
        ))
        return()
      }
      dataset_path <- state$dataset_path %||% state$current_dataset_folder
      if ((is.null(dataset_path) ||
          length(dataset_path) == 0 ||
          is.na(dataset_path) ||
          dataset_path == "") &&
          !is.null(state$current_dataset_id) &&
          length(state$current_dataset_id) > 0 &&
          !is.na(state$current_dataset_id)) {
        rec <- get_dataset_record(state$current_dataset_id, state$data_pond)
        if (!is.null(rec) &&
            !is.null(rec$dataset_path) &&
            length(rec$dataset_path) > 0 &&
            !is.na(rec$dataset_path) &&
            rec$dataset_path != "") {
          dataset_path <- rec$dataset_path
          state$dataset_path <- rec$dataset_path
          state$current_dataset_folder <- rec$dataset_path
          if (!is.null(rec$data_path) &&
              length(rec$data_path) > 0 &&
              !is.na(rec$data_path) &&
              rec$data_path != "") {
            state$data_path <- rec$data_path
          } else {
            state$data_path <- file.path(rec$dataset_path, "data")
          }
        }
      }
      if (is.null(dataset_path) ||
          length(dataset_path) == 0 ||
          is.na(dataset_path) ||
          dataset_path == "") {
        showNotification(
          "No dataset folder is selected or saved for this dataset.",
          type = "warning"
        )
        scan_summary(data.frame(
          message = "No dataset folder path is available. Save or reload the dataset record first."
        ))
        return()
      }
      dataset_path <- normalizePath(dataset_path, mustWork = FALSE, winslash = "/")
      if (!dir.exists(dataset_path)) {
        showNotification(
          paste("Dataset folder does not exist:", dataset_path),
          type = "warning"
        )
        scan_summary(data.frame(
          message = paste("Dataset folder does not exist:", dataset_path)
        ))
        return()
      }
      users <- get_users(state$data_pond)
      user <- users[
        as.character(users$id) == as.character(state$active_user_id),
        ,
        drop = FALSE
      ]
      user_person_id <- if (nrow(user) > 0) {
        as.integer(user$person_id[1])
      } else {
        NA_integer_
      }
      res <- scan_supporting_documents(
        dataset_id = state$current_dataset_id,
        dataset_path = dataset_path,
        base_path = state$data_pond,
        user_person_id = user_person_id
      )
      scan_summary(res)
      state$refresh_supporting_docs <- state$refresh_supporting_docs + 1L
      if ("message" %in% names(res)) {
        showNotification(
          res$message[1],
          type = "warning"
        )
      } else {
        showNotification(
          "Supporting documents scanned.",
          type = "message"
        )
      }
    })

    observeEvent(input$accept_default_types, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "reviewing supporting-document defaults"
      )) {
        return()
      }

      req(state$current_dataset_id)
      req(state$data_pond)

      users <- get_users(state$data_pond)
      user <- users[
        as.character(users$id) == as.character(state$active_user_id),
        ,
        drop = FALSE
      ]
      user_person_id <- if (nrow(user) > 0L) {
        as.integer(user$person_id[1])
      } else {
        NA_integer_
      }

      reviewed <- accept_supporting_document_defaults(
        dataset_id = state$current_dataset_id,
        base_path = state$data_pond,
        user_person_id = user_person_id
      )

      state$refresh_supporting_docs <-
        state$refresh_supporting_docs + 1L

      showNotification(
        if (reviewed == 0L) {
          "All default document types were already reviewed."
        } else if (reviewed == 1L) {
          "Accepted the default type for 1 supporting document."
        } else {
          paste(
            "Accepted the default types for",
            reviewed,
            "supporting documents."
          )
        },
        type = "message"
      )
    })

    observe({
      access_require("read")
      df <- docs()
      if (is.null(df) || nrow(df) == 0) {
        updateSelectInput(session, "doc_select", choices = character())
        return()
      }
      choices <- setNames(
        as.character(df$id),
        df$document_rel_path
      )
      updateSelectInput(
        session,
        "doc_select",
        choices = choices,
        selected = input$doc_select
      )
    })
    observeEvent(input$doc_select, {
      access_require("edit", touch = TRUE)
      row <- selected_document()

      if (
        is.null(row) ||
        nrow(row) == 0
      ) {
        return()
      }

      selected_type <-
        row$document_type_code[1]

      if (
        is.null(selected_type) ||
        length(selected_type) == 0 ||
        is.na(selected_type) ||
        selected_type == ""
      ) {
        selected_type <- "other"
      }

      updateSelectInput(
        session,
        "document_type_code",
        selected = selected_type
      )

      updateTextInput(
        session,
        "document_type_user_text",
        value = if (
          isTRUE(
            as.integer(
              row$document_type_user_set[1]
            ) == 1L
          )
        ) {
          row$document_type_user_text[1] %||% ""
        } else {
          ""
        }
      )

      updateCheckboxInput(
        session,
        "include_in_lineage",
        value = isTRUE(
          as.integer(
            row$include_in_lineage[1]
          ) == 1L
        )
      )

      updateCheckboxInput(
        session,
        "include_in_metadata",
        value = isTRUE(
          as.integer(
            row$include_in_metadata[1]
          ) == 1L
        )
      )

      updateTextAreaInput(
        session,
        "notes",
        value = row$notes[1] %||% ""
      )
    }, ignoreInit = TRUE)

    observeEvent(input$save_doc_edit, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "saving supporting-document metadata"
      )) {
        return()
      }

      req(input$doc_select)
      req(state$data_pond)

      selected_doc <- selected_document()

      if (
        is.null(selected_doc) ||
        nrow(selected_doc) == 0
      ) {
        showNotification(
          "Please select a supporting document.",
          type = "warning"
        )
        return()
      }

      selected_type <- trimws(
        as.character(
          input$document_type_code %||% ""
        )
      )

      if (!nzchar(selected_type)) {
        showNotification(
          "Please select a document type.",
          type = "error"
        )
        return()
      }

      automatic_type <-
        selected_doc$document_type_auto_code[1]

      # Legacy-row fallback.
      if (
        is.null(automatic_type) ||
        length(automatic_type) == 0 ||
        is.na(automatic_type) ||
        automatic_type == ""
      ) {
        automatic_type <-
          selected_doc$document_type_code[1]
      }

      automatic_type <- trimws(
        as.character(
          automatic_type %||% ""
        )
      )

      classification_changed <- !identical(
        selected_type,
        automatic_type
      )

      override_reason <- trimws(
        as.character(
          input$document_type_user_text %||% ""
        )
      )

      if (
        isTRUE(classification_changed) &&
        !nzchar(override_reason)
      ) {
        showNotification(
          paste(
            "Please provide an override reason when",
            "changing the automatically assigned document type."
          ),
          type = "error"
        )
        return()
      }

      # Do not retain an override reason when the selected type
      # matches the automatic classification.
      override_reason_to_save <- if (
        isTRUE(classification_changed)
      ) {
        override_reason
      } else {
        NA_character_
      }

      users <- get_users(state$data_pond)

      user <- users[
        as.character(users$id) ==
          as.character(state$active_user_id),
        ,
        drop = FALSE
      ]

      user_person_id <- if (nrow(user) > 0) {
        as.integer(user$person_id[1])
      } else {
        NA_integer_
      }

      update_supporting_document(
        doc_id = as.integer(input$doc_select),
        data = list(
          document_type_code = selected_type,
          document_type_label =
            supporting_document_type_label(
              selected_type
            ),
          document_type_user_set =
            classification_changed,
          document_type_user_text =
            override_reason_to_save,
          include_in_lineage =
            isTRUE(input$include_in_lineage),
          include_in_metadata =
            isTRUE(input$include_in_metadata),
          notes = trimws(
            as.character(
              input$notes %||% ""
            )
          )
        ),
        base_path = state$data_pond,
        user_person_id = user_person_id
      )

      state$refresh_supporting_docs <-
        state$refresh_supporting_docs + 1L

      showNotification(
        "Supporting document classification saved.",
        type = "message"
      )
    })

    output$scan_summary <- DT::renderDT({
      access_require("read")
      datapond_summary_table(scan_summary())
    })

    selected_document <- reactive({
      req(state$current_dataset_id)
      req(state$data_pond)

      selected_id <- input$doc_select

      if (
        is.null(selected_id) ||
        length(selected_id) == 0 ||
        selected_id == ""
      ) {
        return(NULL)
      }

      df <- docs()

      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      row <- df[
        as.character(df$id) ==
          as.character(selected_id),
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(NULL)
      }

      row[1, , drop = FALSE]
    })
    docs <- reactive({
      state$refresh_supporting_docs
      if (is.null(state$current_dataset_id) ||
          length(state$current_dataset_id) == 0 ||
          is.na(state$current_dataset_id)) {
        return(NULL)
      }
      get_supporting_documents(
        state$current_dataset_id,
        state$data_pond,
        active_only = FALSE
      )
    })

    # render the supporting documents table
    output$tbl_documents <- DT::renderDT({
      access_require("read")
      req(state$current_dataset_id)
      req(state$data_pond)

      docs <- docs()

      if (is.null(docs) || nrow(docs) == 0) {
        return(
          datapond_datatable(
            data.frame(
              message = paste(
                "No supporting documents have been scanned",
                "for the selected dataset."
              )
            ),
            rownames = FALSE,
            selection = "none",
            options = list(
              dom = "t",
              ordering = FALSE
            )
          )
        )
      }

      display_docs <- prepare_supporting_documents_table(
        docs
      )

      tbl <- datapond_datatable(
        display_docs,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        escape = TRUE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(visible = FALSE, targets = 0),
            list(className = "supporting-doc-path", width = "200px", targets = c(1, 2)),
            list(className = "supporting-doc-label", width = "150px", targets = c(3, 12)),
            list(className = "supporting-doc-description", width = "240px", targets = c(4, 7)),
            list(className = "supporting-doc-notes", width = "360px", targets = 8),
            list(className = "supporting-doc-review", width = "110px", targets = c(5, 6)),
            list(className = "supporting-doc-status", width = "80px", targets = c(9, 10, 11)),
            list(targets = c(1, 2, 4, 7, 8), render = DT::JS(
              "function(data, type) {",
              "  if (type !== 'display') return data;",
              "  var cell = document.createElement('div');",
              "  cell.className = 'supporting-doc-cell';",
              # DT escape=TRUE escapes text before this display renderer runs.
              "  cell.innerHTML = data == null ? '' : data;",
              "  cell.title = cell.textContent;",
              "  return cell.outerHTML;",
              "}"
            ))
          )
        ),
        colnames = c(
          "ID",
          "Document",
          "Relative path",
          "Document type",
          "Automatic classification",
          "Review",
          "Classification",
          "Override reason",
          "Notes",
          "Lineage",
          "Metadata",
          "Status",
          "Reviewed by"
        )
      )

      tbl <- DT::formatStyle(
        tbl,
        columns = "review_status",
        backgroundColor = DT::styleEqual(
          c(
            "Reviewed",
            "Needs review"
          ),
          c(
            "#D1DDC7",
            "#E9DFB5"
          )
        )
      )

      tbl <- DT::formatStyle(
        tbl,
        columns = "classification_source",
        backgroundColor = DT::styleEqual(
          c(
            "Automatic",
            "User override"
          ),
          c(
            "#F8F4EA",
            "#E9DFB5"
          )
        )
      )

      tbl <- DT::formatStyle(
        tbl,
        columns = "status",
        backgroundColor = DT::styleEqual(
          c(
            "Active",
            "Missing"
          ),
          c(
            "#D1DDC7",
            "#E4D6B8"
          )
        )
      )

      tbl
    })

    observeEvent(
      input$tbl_documents_rows_selected,
      {
        access_require("read", touch = TRUE)
        selected_row <-
          input$tbl_documents_rows_selected

        if (
          is.null(selected_row) ||
          length(selected_row) == 0
        ) {
          return()
        }

        df <- docs()

        if (is.null(df) || nrow(df) == 0) {
          return()
        }

        display_df <-
          prepare_supporting_documents_table(df)

        if (
          selected_row < 1L ||
          selected_row > nrow(display_df)
        ) {
          return()
        }

        document_id <-
          display_df$id[selected_row]

        # Make the dropdown and table selection agree.
        updateSelectInput(
          session,
          "doc_select",
          selected = as.character(document_id)
        )
      }
    )

    # clear the supporting docs form
    clear_supporting_documents <- function() {
      scan_summary(NULL)

      updateSelectInput(
        session,
        "doc_select",
        choices = character(),
        selected = character(0)
      )

      updateSelectInput(
        session,
        "document_type_code",
        selected = "other"
      )

      updateTextInput(
        session,
        "document_type_user_text",
        value = ""
      )

      updateCheckboxInput(
        session,
        "include_in_lineage",
        value = FALSE
      )

      updateCheckboxInput(
        session,
        "include_in_metadata",
        value = TRUE
      )

      updateTextAreaInput(
        session,
        "notes",
        value = ""
      )

      # Clear any selected row in the DT table.
      try(
        DT::selectRows(
          DT::dataTableProxy(
            "tbl_documents",
            session = session
          ),
          NULL
        ),
        silent = TRUE
      )

      invisible(TRUE)
    }
    list(
      clear = clear_supporting_documents,
      documents = docs,
      selected_document = selected_document
    )
  })
}
