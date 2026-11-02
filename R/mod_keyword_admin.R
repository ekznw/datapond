mod_keyword_admin_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3("Keyword administration"),
    shiny::uiOutput(ns("admin_body"))
  )
}

keyword_admin_field_choices <- function(include_all = FALSE) {
  choices <- c(
    "Theme / subject" = "keyword_object",
    "Knowledge domain" = "keyword_discipline",
    "Spatial context" = "keyword_geo"
  )
  if (isTRUE(include_all)) c("All fields" = "", choices) else choices
}

keyword_admin_display <- function(df) {
  data.frame(
    keyword = df$option_label,
    keyword_group = names(keyword_admin_field_choices())[match(df$field_code, keyword_admin_field_choices())],
    approval_status = tools::toTitleCase(df$approval_status),
    active = ifelse(df$is_active == 1L, "Yes", "No"),
    source = unname(c(seed_csv = "Starter CSV", admin_override = "Administrator override",
      admin_created = "Administrator", dataset_custom = "Custom keyword")[df$source]),
    dataset_count = df$dataset_count,
    stringsAsFactors = FALSE
  )
}

keyword_admin_body_ui <- function(ns) {
  palette <- app_earth_palette()
  shiny::tagList(
    shiny::p(class = "text-muted", paste(
      "Custom keywords can be used and saved on any dataset.",
      "Approval makes a term available as a shared suggestion; rejection or deactivation does not remove it from saved datasets."
    )),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("field_filter"), "Keyword group",
        choices = keyword_admin_field_choices(include_all = TRUE))),
      shiny::column(4, shiny::selectInput(ns("status_filter"), "Review status",
        choices = c("All statuses" = "", "Pending" = "pending", "Approved" = "approved", "Rejected" = "rejected"))),
      shiny::column(4, shiny::checkboxInput(ns("active_only"), "Show active terms only", FALSE))
    ),
    shiny::p(class = "text-muted", shiny::textOutput(ns("review_summary"), inline = TRUE)),
    DT::DTOutput(ns("vocab_table")),
    shiny::p(class = "text-muted", "Select a row to review or edit a keyword. Search matches the visible columns."),
    shiny::tags$div(class = "well",
      style = paste0("background:", palette[["cream"]], ";border-color:", palette[["stone"]], ";"),
      shiny::h4(shiny::textOutput(ns("editor_heading"), inline = TRUE)),
      shiny::fluidRow(
        shiny::column(6,
          shiny::textInput(ns("option_label"), metadata_field_label("Keyword label", "Mandatory", "Required when saving a keyword.")),
          shiny::selectInput(ns("edit_field"), metadata_field_label("Keyword group", "Mandatory", "Choose the group for the keyword or thesaurus citation being saved."), choices = keyword_admin_field_choices())),
        shiny::column(6,
          shiny::selectInput(ns("approval_status"), "Review status",
            choices = c("Approved" = "approved", "Pending" = "pending", "Rejected" = "rejected")),
          shiny::checkboxInput(ns("is_active"), "Active", TRUE))
      ),
      metadata_optional_details("Stored value, ordering and history",
        shiny::textInput(ns("option_value"), "Stored value (leave blank for a new term to use its label)"),
        shiny::helpText("Stored values and groups cannot be changed while referenced by datasets. Commas are allowed; semicolons separate terms."),
        shiny::numericInput(ns("sort_order"), "Suggestion order", 100L, min = 0L, step = 1L),
        shiny::textOutput(ns("term_audit"))),
      shiny::tags$div(class = "datapond-panel-toolbar",
        shiny::actionButton(ns("save_term"), "Save keyword", class = "btn-primary datapond-requires-active-user"),
        shiny::actionButton(ns("new_term"), "New keyword"),
        shiny::actionButton(ns("deactivate_term"), "Deactivate selected", class = "datapond-requires-active-user"))
    ),
    metadata_optional_details("Thesaurus citation",
      shiny::tags$p(class = "datapond-keyword-source-help",
        style = "font-size:12px; line-height:1.45;",
        "Name the source vocabulary for approved terms in the selected keyword group. These settings apply across the pond. Custom keywords do not need a thesaurus citation."),
      shiny::textInput(ns("thesaurus_title"), metadata_field_label("Citation title", "Conditional", "Required when saving a thesaurus citation.")),
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(ns("thesaurus_edition"), "Edition / version")),
        shiny::column(4, shiny::dateInput(ns("thesaurus_date"), "Edition date", value = NULL, format = "yyyy-mm-dd")),
        shiny::column(4, shiny::textInput(ns("thesaurus_identifier"), "Identifier"))),
      shiny::textInput(ns("thesaurus_uri"), "Vocabulary URI"),
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("thesaurus_person"), "Responsible person", c("None" = ""))),
        shiny::column(4, shiny::selectInput(ns("thesaurus_org"), "Responsible organisation", c("None" = ""))),
        shiny::column(4, shiny::selectInput(ns("thesaurus_role"), "Role", c("publisher", "custodian", "owner", "pointOfContact")))),
      shiny::actionButton(ns("save_thesaurus"), "Save citation", class = "btn-primary datapond-requires-active-user")),
    metadata_optional_details("CSV vocabulary maintenance",
      shiny::helpText("The packaged CSV supplies starter terms. The pond database stores local changes and custom keywords. Existing datasets are never rewritten by CSV updates."),
      shiny::selectInput(ns("seed_mode"), "Operation", c(
        "Add missing terms only" = "add",
        "Update CSV terms and add missing terms" = "update",
        "Reset CSV terms to the packaged vocabulary" = "reset")),
      shiny::helpText("Update refreshes CSV-managed labels and ordering and can reactivate CSV-managed terms. Reset also deactivates CSV terms no longer listed. Administrator overrides and custom terms are preserved."),
      shiny::actionButton(ns("preview_seed"), "Preview CSV changes"),
      shiny::p(shiny::textOutput(ns("seed_summary"), inline = TRUE)),
      DT::DTOutput(ns("seed_preview")),
      shiny::checkboxInput(ns("confirm_seed"), metadata_field_label("I have reviewed these changes", "Conditional", "Required before applying the previewed vocabulary changes."), FALSE),
      shiny::actionButton(ns("apply_seed"), "Apply reviewed changes", class = "btn-primary datapond-requires-active-user"))
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
      access_require("read")
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

      keyword_admin_body_ui(session$ns)
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

    filtered_vocab <- shiny::reactive({
      df <- vocab_data()
      status <- input$status_filter %||% ""
      if (nzchar(status)) df <- df[df$approval_status == status, , drop = FALSE]
      if (isTRUE(input$active_only)) df <- df[df$is_active == 1L, , drop = FALSE]
      df
    })

    output$review_summary <- shiny::renderText({
      access_require("read")
      df <- vocab_data()
      paste(sum(df$approval_status == "pending"), "pending review \u00b7",
        sum(df$is_active == 1L & df$approval_status == "approved"), "shared suggestions \u00b7",
        nrow(df), "terms in this group")
    })
    output$vocab_table <- DT::renderDT({
      access_require("read")
      clean_dt_widget(keyword_admin_display(filtered_vocab()), selection = "single",
        filter = "none", buttons = FALSE, page_length = 10L)
    })
    output$editor_heading <- shiny::renderText({
      access_require("read")
      df <- vocab_data()
      selected <- df[df$id %in% selected_id(), , drop = FALSE]
      if (nrow(selected)) paste("Edit keyword:", selected$option_label[1]) else "New keyword"
    })
    output$term_audit <- shiny::renderText({
      access_require("read")
      df <- vocab_data()
      row <- df[df$id %in% selected_id(), , drop = FALSE]
      if (!nrow(row)) return("Select a term to view its history.")
      paste("Created:", row$created_at[1], "\u00b7 Last updated:", row$updated_at[1],
        "\u00b7 Used by", row$dataset_count[1], "dataset(s).")
    })

    shiny::observeEvent(input$vocab_table_rows_selected, {
      access_require("admin", touch = TRUE)
      row_number <- input$vocab_table_rows_selected
      df <- filtered_vocab()

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
      DT::selectRows(DT::dataTableProxy("vocab_table", session = session), NULL)

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

    shiny::observe({
      access_require("read")
      state$refresh_person; state$refresh_org
      people <- get_persons(state$data_pond); orgs <- get_organisations(state$data_pond)
      person_labels <- trimws(paste(people$name_first, people$name_last))
      org_labels <- ifelse(!is.na(orgs$organisation_name_short) & nzchar(orgs$organisation_name_short),
                           orgs$organisation_name_short, orgs$organisation_name_full)
      shiny::updateSelectInput(session, "thesaurus_person", choices = c("None" = "", stats::setNames(as.character(people$id), person_labels)))
      shiny::updateSelectInput(session, "thesaurus_org", choices = c("None" = "", stats::setNames(as.character(orgs$id), org_labels)))
    })

    load_thesaurus <- function(field) {
      rows <- get_keyword_thesauri(state$data_pond, field)
      value <- function(name, default = "") if (nrow(rows) && !is.na(rows[[name]][1])) as.character(rows[[name]][1]) else default
      shiny::updateTextInput(session, "thesaurus_title", value = value("citation_title"))
      shiny::updateTextInput(session, "thesaurus_edition", value = value("edition"))
      shiny::updateDateInput(session, "thesaurus_date", value = if (nrow(rows) && !is.na(rows$edition_date[1])) as.Date(rows$edition_date[1]) else NULL)
      shiny::updateTextInput(session, "thesaurus_identifier", value = value("identifier"))
      shiny::updateTextInput(session, "thesaurus_uri", value = value("thesaurus_uri"))
      shiny::updateSelectInput(session, "thesaurus_person", selected = value("responsible_person_id"))
      shiny::updateSelectInput(session, "thesaurus_org", selected = value("responsible_organisation_id"))
      shiny::updateSelectInput(session, "thesaurus_role", selected = value("responsible_role_code", "publisher"))
    }
    shiny::observeEvent(input$edit_field, { access_require("admin", touch = TRUE); load_thesaurus(input$edit_field) }, ignoreInit = FALSE)
    shiny::observeEvent(input$save_thesaurus, {
      access_require("admin", touch = TRUE)
      if (!require_active_user_for_write(state, "saving a thesaurus citation")) return()
      shiny::req(current_user_is_admin())
      tryCatch({
        save_keyword_thesaurus(state$data_pond, list(
          field_code = input$edit_field, citation_title = input$thesaurus_title,
          edition = input$thesaurus_edition,
          edition_date = if (is.null(input$thesaurus_date) || is.na(input$thesaurus_date)) NA_character_ else format(input$thesaurus_date, "%Y-%m-%d"),
          edition_date_type_code = "revision", thesaurus_uri = input$thesaurus_uri,
          identifier = input$thesaurus_identifier, responsible_person_id = input$thesaurus_person,
          responsible_organisation_id = input$thesaurus_org, responsible_role_code = input$thesaurus_role
        ))
        refresh_vocab(); shiny::showNotification("Thesaurus citation saved.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })

    shiny::observeEvent(input$new_term, {
      access_require("admin", touch = TRUE)
      shiny::req(current_user_is_admin())
      clear_editor()
    })

    shiny::observeEvent(input$save_term, {
      access_require("admin", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "saving a keyword term"
      )) {
        return()
      }

      shiny::req(current_user_is_admin())

      tryCatch({
        saved_id <- save_keyword_vocab_term(
          base_path = state$data_pond,
          user_id = state$active_user_id,
          field_code = input$edit_field,
          option_value = if (nzchar(trimws(input$option_value %||% ""))) input$option_value else input$option_label,
          option_label = input$option_label,
          sort_order = input$sort_order,
          is_active = input$is_active,
          approval_status = input$approval_status,
          keyword_id = selected_id()
        )

        selected_id(saved_id)
        saved <- get_keyword_vocab_admin(state$data_pond)
        shiny::updateTextInput(session, "option_value", value = saved$option_value[match(saved_id, saved$id)])
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
      access_require("admin", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "deactivating a keyword term"
      )) {
        return()
      }

      shiny::req(current_user_is_admin())
      if (is.null(selected_id())) {
        shiny::showNotification("Select a keyword to deactivate first.", type = "warning")
        return()
      }

      tryCatch({
        set_keyword_vocab_active(
          keyword_id = selected_id(),
          is_active = FALSE,
          user_id = state$active_user_id,
          base_path = state$data_pond
        )
        shiny::updateCheckboxInput(session, "is_active", value = FALSE)
        refresh_vocab()

        shiny::showNotification(
          "Keyword term deactivated.",
          type = "message"
        )
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })

    seed_preview <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(state$data_pond, state$active_user_id, input$field_filter, input$status_filter, input$active_only), {
      access_require("admin", touch = TRUE)
      clear_editor()
      seed_preview(NULL)
      if (nzchar(input$field_filter %||% "")) {
        shiny::updateSelectInput(session, "edit_field", selected = input$field_filter)
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(state$data_pond, {
      access_require("admin", touch = TRUE)
      if (isTRUE(current_user_is_admin()) && !is.null(input$edit_field)) load_thesaurus(input$edit_field)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$seed_mode, { access_require("admin", touch = TRUE); seed_preview(NULL) }, ignoreInit = TRUE)

    output$seed_preview <- DT::renderDT({
      access_require("read")
      preview <- seed_preview()
      if (is.null(preview)) return(clean_dt_widget(
        data.frame(message = "Preview the CSV to review changes before applying them."), buttons = FALSE))
      df <- preview$plan[c("action", "field_code", "option_value", "previous_label", "option_label", "previous_order", "sort_order")]
      df$field_code <- names(keyword_admin_field_choices())[match(df$field_code, keyword_admin_field_choices())]
      clean_dt_widget(df, buttons = FALSE, filter = "none", page_length = 10L)
    })
    output$seed_summary <- shiny::renderText({
      access_require("read")
      preview <- seed_preview()
      if (is.null(preview)) return("")
      counts <- table(preview$plan$action)
      paste(paste(names(counts), as.integer(counts), sep = ": "), collapse = " \u00b7 ")
    })
    shiny::observeEvent(input$preview_seed, {
      access_require("admin", touch = TRUE)
      shiny::req(current_user_is_admin())
      seed_preview(NULL)
      shiny::updateCheckboxInput(session, "confirm_seed", value = FALSE)
      tryCatch({
        seed_preview(preview_keyword_vocab_seed(load_vocab(vocab_path), state$data_pond,
          mode = input$seed_mode))
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })
    shiny::observeEvent(input$apply_seed, {
      access_require("admin", touch = TRUE)
      if (!require_active_user_for_write(state, "applying keyword CSV changes")) return()
      shiny::req(current_user_is_admin())
      preview <- seed_preview()
      if (is.null(preview) || !isTRUE(input$confirm_seed)) {
        shiny::showNotification("Preview the changes and select the confirmation checkbox first.", type = "warning")
        return()
      }
      tryCatch({
        result <- apply_keyword_vocab_seed(load_vocab(vocab_path), state$data_pond,
          state$active_user_id, mode = input$seed_mode, preview = preview)
        seed_preview(NULL)
        shiny::updateCheckboxInput(session, "confirm_seed", value = FALSE)
        refresh_vocab()
        clear_editor()
        shiny::showNotification(paste(result$inserted, "added,", result$updated, "updated,",
          result$deactivated, "deactivated;", result$preserved, "preserved or unchanged."), type = "message")
      }, error = function(e) {
        seed_preview(NULL)
        shiny::updateCheckboxInput(session, "confirm_seed", value = FALSE)
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })
  })
}
