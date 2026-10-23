lineage_summary_edit_values <- function(data, saved) {
  # Preserve archival snapshots independently of the editable narrative.
  for (field in c("source_description", "source_datasets_text", "process_description",
                  "processing_tools", "processing_scripts", "output_description", "lineage_notes")) {
    data[[field]] <- if (!is.null(saved) && nrow(saved) > 0L && field %in% names(saved)) saved[[field]][1] else ""
  }
  data
}

lineage_field_label <- function(label, requirement, guidance) {
  shiny::tagList(
    label, " ", flow_requirement_marker(requirement),
    shiny::tags$small(class = "lineage-field-help", guidance)
  )
}

lineage_origin_display <- function(values) {
  labels <- humanise_code_value(values)
  labels[!is.na(values) & values == "legacy_backfill"] <- "Migrated from earlier fields"
  labels
}

mod_lineage_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "datapond-lineage",
    tags$style(HTML("
      .datapond-lineage .flow-field-requirement {
        display: inline-block; font-size: 14px; font-weight: 600;
        line-height: 1; vertical-align: baseline;
      }
      .datapond-lineage .flow-field-requirement.mandatory { color: #7A2525; }
      .datapond-lineage .flow-field-requirement.conditional { color: #65512E; }
      .datapond-lineage .lineage-field-help {
        display: block; color: #6B6B6B; font-size: 12px;
        font-weight: normal; line-height: 1.4; margin-top: 3px;
      }
    ")),
    h3("Dataset lineage"),
    tags$small(
      style = "color:#666; display:block;",
      paste(
        "Record a short lineage summary here. The detailed lineage can live",
        "in structured fields or in any supporting document."
      )
    ),
    br(),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('datapond-lineage-authority-open', function(message) {
        var panel = document.getElementById(message.id);
        if (panel) panel.open = message.open === true;
      });
    ")),
    tags$details(
      id = ns("lineage_authority_details"),
      class = "datapond-optional-details",
      open = "open",
      tags$summary("Lineage authority and supporting documents"),
      tags$div(style = "padding-top:12px;",
        radioButtons(
          ns("lineage_source_mode"),
          lineage_field_label("Where is the authoritative lineage detail?", "Mandatory",
            "Choose the authority when saving lineage. Structured fields is selected by default; a short summary is sufficient in Prickly Porcupine mode."),
          choices = c(
            "Supporting document(s)" = "supporting_documents",
            "Structured fields in this form" = "form"
          ),
          selected = "form"
        ),
        uiOutput(ns("lineage_document_picker")),
        actionButton(
          ns("open_supporting_documents"),
          "Scan or manage supporting documents"
        ),
        mod_data_dev_template_ui(ns("data_dev_template")),
        tags$small(
          style = "color:#666; display:block; margin-top:5px;",
          paste(
            "Selected documents are flagged for lineage when this record is saved.",
            "PDFs, reports, office documents, notebooks, QMDs and other scanned",
            "supporting records can all be authoritative."
          )
        )
      )
    ),
    br(),
    textAreaInput(
      ns("lineage_statement"),
      lineage_field_label("Lineage summary", "Mandatory",
        "Required to save lineage in both modes, including when a document holds the detail."),
      placeholder = paste(
        "Summarise where the dataset came from and",
        "how it was produced. Include any additional context here; linked documents can contain the full detail."
      )
    ),
    tags$hr(),
    metadata_optional_details(
      lineage_field_label("Detailed lineage sources and process steps", "Conditional",
        "Armoured Pangolin completion with structured-field authority needs at least one source or process step. Prickly Porcupine needs only the summary. Document-backed detail does not need to be repeated here."),
      h4("Ordered lineage sources"),
      tags$p(class = "lineage-field-help",
        "Start with the inputs: the datasets, surveys, maps or reports used to produce your dataset."),
      tags$ol(class = "lineage-field-help",
        tags$li("Describe one input below. Give it a source title, describe it, or select a document that identifies it. Fill in other details only when useful and known."),
        tags$li("Click Add source to save it as a row in the table. Repeat for each additional input."),
        tags$li("To correct an entry, select its row, edit the fields and click Update source. Clear source form starts a fresh entry; Move up/down changes the display order.")
      ),
      DT::DTOutput(ns("lineage_source_table")),
      fluidRow(
        column(6, textInput(ns("source_title"), lineage_field_label("Source title", "Conditional",
          "Name the input dataset, map or report, including its edition if useful. To save a source, provide a title, description or authoritative supporting document."))),
        column(6, uiOutput(ns("source_document_input")))
      ),
      textAreaInput(ns("source_detail_description"), lineage_field_label("Source description", "Conditional",
        "Explain what the input contains, who supplied it and which part you used. For example: District field-team vegetation plots surveyed in 2024; species observations used to classify habitats. Describe processing actions in the process steps below. Required only if neither a title nor a document is provided."),
        placeholder = "e.g. Vegetation plots supplied by the district field team, surveyed in 2024; species observations used for habitat classification.", rows = 3),
      fluidRow(
        column(4, textInput(ns("source_identifier"), lineage_field_label("Identifier", "Optional",
          "Enter an existing DOI, catalogue number or other stable identifier for the source; leave blank if none is known."))),
        column(4, textInput(ns("source_identifier_code_space"), lineage_field_label("Identifier code space", "Optional",
          "Name the system or organisation that issued the identifier, such as DOI or a provider catalogue."))),
        column(4, textInput(ns("source_uri"), lineage_field_label("Source URI", "Optional",
          "Link to the source dataset or its catalogue page, preferably using a persistent web address.")))
      ),
      fluidRow(
        column(4, textInput(ns("source_citation_date"), lineage_field_label("Citation date", "Optional",
          "Enter the date of the source edition you used, then choose what that date represents. Leave blank if unknown."), placeholder = "YYYY-MM-DD")),
        column(4, selectInput(
          ns("source_citation_date_type"), lineage_field_label("Citation date type", "Optional",
          "Indicate whether the citation date records creation, publication or revision of the source."),
          choices = c(stats::setNames("", ""), "Creation" = "creation", "Publication" = "publication", "Revision" = "revision")
        )),
        column(4, selectInput(
          ns("source_origin"), lineage_field_label("Value origin", "Optional",
          "How was this metadata captured? Leave blank for automatic selection on save: User entry, or Supporting document if linked. A blank choice keeps the recorded origin when editing an existing source."),
          choices = c(stats::setNames("", ""),
            "User entry" = "user", "Supporting document" = "supporting_document",
            "Extracted metadata" = "extracted_metadata", "Legacy backfill" = "legacy_backfill"
          ), selected = "", selectize = FALSE
        ))
      ),
      fluidRow(
        column(3, textInput(ns("source_temporal_start"), lineage_field_label("Temporal start", "Optional",
          "Earliest date covered by the source observations. This describes the source data, rather than its publication date."), placeholder = "YYYY-MM-DD")),
        column(3, textInput(ns("source_temporal_end"), lineage_field_label("Temporal end", "Optional",
          "Latest date covered by the source observations. Leave blank if the end is unknown or the series is ongoing."), placeholder = "YYYY-MM-DD")),
        column(3, textInput(ns("source_scale"), lineage_field_label("Equivalent scale denominator", "Optional",
          "Enter only the denominator of the source map scale: 50000 for 1:50,000. Leave blank if unknown or inapplicable."), placeholder = "e.g. 50000")),
        column(3, textInput(ns("source_resolution"), lineage_field_label("Resolution (value and unit)", "Optional",
          "Enter the source spatial resolution with its unit, such as 30 m for raster cell size. Use a known value."), placeholder = "30 m"))
      ),
      fluidRow(
        column(4, uiOutput(ns("source_person_input"))),
        column(4, uiOutput(ns("source_organisation_input"))),
        column(4, selectInput(
          ns("source_role"), lineage_field_label("Responsible-party role", "Optional",
          "Describe the selected person or organisation\u2019s relationship to the source, such as originator, custodian or publisher."),
          choices = c(stats::setNames("", ""), stats::setNames(
            lineage_responsible_role_codes(), lineage_responsible_role_codes()
          ))
        ))
      ),
      actionButton(ns("save_source"), "Add source", class = "datapond-requires-active-user"),
      actionButton(ns("clear_source"), "Clear source form"),
      actionButton(ns("move_source_up"), "Move up"),
      actionButton(ns("move_source_down"), "Move down"),
      actionButton(ns("delete_source"), "Delete source", class = "datapond-requires-active-user"),
      tags$hr(),
      h4("Ordered process steps"),
      tags$p(class = "lineage-field-help",
        "Then describe what you did to the inputs, in the order it happened. One row represents one meaningful action."),
      tags$ol(class = "lineage-field-help",
        tags$li("Write the first action in Process-step description, including important settings. For example: Removed duplicate survey records using the sample ID, keeping the most recent observation."),
        tags$li("Where useful, select Sources used by this step and add the software, script, date or output. Leave unknown or unnecessary details blank."),
        tags$li("Click Add process step. Repeat for the next action, such as converting coordinates, clipping to the study area or classifying habitats."),
        tags$li("Select a saved row to edit it and click Update process step. Use Move up/down to correct the sequence, or Clear step form to start a new entry.")
      ),
      tags$p(class = "lineage-field-help",
        "Use Add source and Add process step to save these rows. Save lineage saves the summary, authority choice and selected documents; it does not add unfinished source or step entries."),
      DT::DTOutput(ns("lineage_process_table")),
      textAreaInput(ns("step_description"), lineage_field_label("Process-step description", "Conditional",
        "Say what changed and how: the operation, the input and any settings needed to understand it. Required when adding or updating a step."),
        placeholder = "e.g. Clipped the cleaned vegetation observations to the reserve boundary and excluded records outside the reserve.", rows = 3),
      fluidRow(
        column(6, textAreaInput(ns("step_rationale"), lineage_field_label("Rationale", "Optional",
          "Explain why this step was needed or why the method was chosen. Put the actions themselves in the step description."))),
        column(6, textInput(ns("step_datetime"), lineage_field_label("Date/time", "Optional",
          "Record when this processing step occurred, if known. Include time and timezone when available, for example 2024-02-03T10:30:00+02:00."), placeholder = "YYYY-MM-DD or ISO date/time"))
      ),
      fluidRow(
        column(4, textInput(ns("step_tool"), lineage_field_label("Tool / software reference", "Optional",
          "Name the software or tool used and its version where known, such as R 4.5 or QGIS 3.40."))),
        column(4, textInput(ns("step_script"), lineage_field_label("Script / notebook reference", "Optional",
          "Identify the script or notebook used. Prefer a path relative to the dataset folder, such as scripts/process.R, or a public web URL."))),
        column(4, textInput(ns("step_output"), lineage_field_label("Output description", "Optional",
          "Describe what this step produced, such as a cleaned table, clipped layer or derived classification.")))
      ),
      fluidRow(
        column(4, uiOutput(ns("step_person_input"))),
        column(4, uiOutput(ns("step_organisation_input"))),
        column(4, selectInput(
          ns("step_role"), lineage_field_label("Processor role", "Optional",
          "Describe the selected person or organisation\u2019s role in this step. Processor is selected by default."),
          choices = c(stats::setNames("", ""), stats::setNames(
            lineage_responsible_role_codes(), lineage_responsible_role_codes()
          )), selected = "processor"
        ))
      ),
      selectizeInput(
        ns("step_source_ids"), lineage_field_label("Sources used by this step", "Optional",
          "Select the source rows this step used. Add them under Ordered lineage sources first; select more than one when combining inputs."),
        choices = NULL, multiple = TRUE
      ),
      selectInput(
        ns("step_origin"), lineage_field_label("Value origin", "Optional",
          "How was this metadata captured? Leave blank to use User entry for a new step. A blank choice keeps the recorded origin when editing an existing step."),
        choices = c(stats::setNames("", ""),
          "User entry" = "user", "Supporting document" = "supporting_document",
          "Extracted metadata" = "extracted_metadata", "Legacy backfill" = "legacy_backfill"
        ), selected = "", selectize = FALSE
      ),
      actionButton(ns("save_step"), "Add process step", class = "datapond-requires-active-user"),
      actionButton(ns("clear_step"), "Clear step form"),
      actionButton(ns("move_step_up"), "Move up"),
      actionButton(ns("move_step_down"), "Move down"),
      actionButton(ns("delete_step"), "Delete process step", class = "datapond-requires-active-user")
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
    actionButton(
      ns("save_lineage"),
      "Save lineage",
      class = "datapond-requires-active-user"
    ),
    br(),
    br(),
    h4("Current saved lineage"),
    tags$p(class = "lineage-field-help",
      "The latest saved authority, status and summary are shown below. Unsaved form edits appear here after Save lineage. This is not a revision history; detailed sources, process steps and supporting documents are listed separately."),
    tableOutput(ns("lineage_preview")),
    h4("Documents supporting lineage"),
    tableOutput(ns("lineage_docs_table"))
  )
}

mod_lineage_server <- function(
  id,
  state,
  open_supporting_documents = NULL
) {
  moduleServer(id, function(input, output, session) {
    mod_data_dev_template_server("data_dev_template", state)
    set_authority_expanded <- function(expanded) {
      session$sendCustomMessage("datapond-lineage-authority-open", list(
        id = session$ns("lineage_authority_details"), open = isTRUE(expanded)
      ))
    }

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

    supporting_docs <- reactive({
      state$refresh_supporting_docs
      if (is.null(state$current_dataset_id) ||
          length(state$current_dataset_id) == 0 ||
          is.na(state$current_dataset_id)) {
        return(data.frame())
      }
      get_supporting_documents(
        state$current_dataset_id,
        state$data_pond
      )
    })

    lineage_sources <- reactive({
      state$refresh_lineage
      if (is.null(state$current_dataset_id) || is.na(state$current_dataset_id)) {
        return(data.frame())
      }
      get_dataset_lineage_sources(state$current_dataset_id, state$data_pond)
    })

    lineage_steps <- reactive({
      state$refresh_lineage
      if (is.null(state$current_dataset_id) || is.na(state$current_dataset_id)) {
        return(data.frame())
      }
      get_dataset_lineage_process_steps(state$current_dataset_id, state$data_pond)
    })

    lineage_person_choices <- reactive({
      state$refresh_person
      people <- get_persons(state$data_pond)
      if (nrow(people) == 0L) return(character())
      first <- as.character(people$name_first)
      last <- as.character(people$name_last)
      first[is.na(first)] <- ""
      last[is.na(last)] <- ""
      labels <- trimws(paste(first, last))
      labels[!nzchar(labels)] <- paste("Person", people$id[!nzchar(labels)])
      stats::setNames(as.character(people$id), labels)
    })

    lineage_organisation_choices <- reactive({
      state$refresh_org
      organisation_select_choices(get_organisations(state$data_pond))
    })

    optional_choices <- function(x) c("None" = "", x)

    output$source_document_input <- renderUI({
      access_require("read")
      docs <- supporting_docs()
      choices <- if (nrow(docs) == 0L) character() else stats::setNames(
        as.character(docs$id), as.character(docs$document_rel_path)
      )
      selectInput(
        session$ns("source_document_id"),
        lineage_field_label("Authoritative supporting document", "Conditional",
          "Select a registered document that describes this source; its contents need not be copied here. Required only if neither a title nor a description is provided."),
        choices = optional_choices(choices)
      )
    })
    output$source_person_input <- renderUI({
      access_require("read")
      selectInput(
        session$ns("source_person_id"), lineage_field_label("Responsible person", "Optional",
          "Select a saved person responsible for the source. People are maintained in Parties; choose None if not known."),
        choices = optional_choices(lineage_person_choices())
      )
    })
    output$source_organisation_input <- renderUI({
      access_require("read")
      selectInput(
        session$ns("source_organisation_id"), lineage_field_label("Responsible organisation", "Optional",
          "Select the organisation responsible for the source, such as its producer or custodian. Choose None if not known."),
        choices = optional_choices(lineage_organisation_choices())
      )
    })
    output$step_person_input <- renderUI({
      access_require("read")
      selectInput(
        session$ns("step_person_id"), lineage_field_label("Processor person", "Optional",
          "Select the saved person who performed or was responsible for this step. Choose None if not known."),
        choices = optional_choices(lineage_person_choices())
      )
    })
    output$step_organisation_input <- renderUI({
      access_require("read")
      selectInput(
        session$ns("step_organisation_id"), lineage_field_label("Processor organisation", "Optional",
          "Select the organisation that performed or was responsible for this step. Choose None if not known."),
        choices = optional_choices(lineage_organisation_choices())
      )
    })

    observe({
      access_require("read")
      sources <- lineage_sources()
      labels <- if (nrow(sources) == 0L) character() else {
        title <- as.character(sources$title)
        missing <- is.na(title) | !nzchar(trimws(title))
        title[missing] <- as.character(sources$supporting_document_path[missing])
        title[is.na(title) | !nzchar(trimws(title))] <- paste(
          "Source", sources$source_order[is.na(title) | !nzchar(trimws(title))]
        )
        stats::setNames(as.character(sources$id), title)
      }
      current <- isolate(input$step_source_ids) %||% character()
      updateSelectizeInput(
        session, "step_source_ids", choices = labels,
        selected = intersect(as.character(current), unname(labels)), server = FALSE
      )
    })

    output$lineage_source_table <- DT::renderDT({
      access_require("read")
      x <- lineage_sources()
      display <- if (nrow(x) == 0L) {
        data.frame(Order = integer(), Source = character(), Authority = character(), `Value origin` = character(), check.names = FALSE)
      } else {
        source_label <- as.character(x$title)
        missing <- is.na(source_label) | !nzchar(trimws(source_label))
        source_label[missing] <- as.character(x$description[missing])
        data.frame(
          Order = x$source_order,
          Source = source_label,
          Authority = ifelse(is.na(x$supporting_document_path), "Structured record", x$supporting_document_path),
          `Value origin` = lineage_origin_display(x$value_origin_code),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
      DT::datatable(display, rownames = FALSE, selection = "single", options = list(
        dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE
      ), class = "compact stripe hover")
    })

    output$lineage_process_table <- DT::renderDT({
      access_require("read")
      x <- lineage_steps()
      display <- if (nrow(x) == 0L) {
        data.frame(Order = integer(), Description = character(), `Source IDs` = character(), `Value origin` = character(), check.names = FALSE)
      } else data.frame(
        Order = x$step_order,
        Description = x$description,
        `Source IDs` = ifelse(is.na(x$source_ids), "", x$source_ids),
        `Value origin` = lineage_origin_display(x$value_origin_code),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, rownames = FALSE, selection = "single", options = list(
        dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE
      ), class = "compact stripe hover")
    })

    clear_source_editor <- function() {
      updateTextInput(session, "source_title", value = "")
      updateTextAreaInput(session, "source_detail_description", value = "")
      for (id in c(
        "source_identifier", "source_identifier_code_space", "source_uri",
        "source_citation_date", "source_temporal_start", "source_temporal_end",
        "source_resolution"
      )) updateTextInput(session, id, value = "")
      updateTextInput(session, "source_scale", value = "")
      updateSelectInput(session, "source_citation_date_type", selected = "")
      updateSelectInput(session, "source_origin", selected = "")
      updateSelectInput(session, "source_document_id", selected = "")
      updateSelectInput(session, "source_person_id", selected = "")
      updateSelectInput(session, "source_organisation_id", selected = "")
      updateSelectInput(session, "source_role", selected = "")
      updateActionButton(session, "save_source", label = "Add source")
      DT::selectRows(DT::dataTableProxy("lineage_source_table", session = session), NULL)
      invisible(TRUE)
    }

    clear_step_editor <- function() {
      updateTextAreaInput(session, "step_description", value = "")
      updateTextAreaInput(session, "step_rationale", value = "")
      for (id in c("step_datetime", "step_tool", "step_script", "step_output")) {
        updateTextInput(session, id, value = "")
      }
      updateSelectInput(session, "step_person_id", selected = "")
      updateSelectInput(session, "step_organisation_id", selected = "")
      updateSelectInput(session, "step_role", selected = "processor")
      updateSelectInput(session, "step_origin", selected = "")
      updateSelectizeInput(session, "step_source_ids", selected = character())
      updateActionButton(session, "save_step", label = "Add process step")
      DT::selectRows(DT::dataTableProxy("lineage_process_table", session = session), NULL)
      invisible(TRUE)
    }

    observeEvent(input$lineage_source_table_rows_selected, {
      access_require("read", touch = TRUE)
      row <- input$lineage_source_table_rows_selected
      sources <- lineage_sources()
      if (length(row) != 1L || row > nrow(sources)) return()
      x <- sources[row, , drop = FALSE]
      updateTextInput(session, "source_title", value = x$title[1] %||% "")
      updateTextAreaInput(session, "source_detail_description", value = x$description[1] %||% "")
      updateTextInput(session, "source_identifier", value = x$identifier[1] %||% "")
      updateTextInput(session, "source_identifier_code_space", value = x$identifier_code_space[1] %||% "")
      updateTextInput(session, "source_uri", value = x$source_uri[1] %||% "")
      updateTextInput(session, "source_citation_date", value = x$citation_date[1] %||% "")
      updateSelectInput(session, "source_citation_date_type", selected = x$citation_date_type_code[1] %||% "")
      updateTextInput(session, "source_temporal_start", value = x$temporal_start[1] %||% "")
      updateTextInput(session, "source_temporal_end", value = x$temporal_end[1] %||% "")
      updateTextInput(
        session, "source_scale",
        value = if (is.na(x$equivalent_scale_denominator[1])) "" else as.character(x$equivalent_scale_denominator[1])
      )
      resolution <- if (is.na(x$spatial_resolution_value[1])) "" else trimws(paste(
        x$spatial_resolution_value[1], x$spatial_resolution_unit[1] %||% ""
      ))
      updateTextInput(session, "source_resolution", value = resolution)
      updateSelectInput(session, "source_document_id", selected = x$supporting_document_id[1] %||% "")
      updateSelectInput(session, "source_person_id", selected = x$responsible_person_id[1] %||% "")
      updateSelectInput(session, "source_organisation_id", selected = x$responsible_organisation_id[1] %||% "")
      updateSelectInput(session, "source_role", selected = x$role_code[1] %||% "")
      updateSelectInput(session, "source_origin", selected = x$value_origin_code[1] %||% "user")
      updateActionButton(session, "save_source", label = "Update source")
    })

    observeEvent(input$lineage_process_table_rows_selected, {
      access_require("read", touch = TRUE)
      row <- input$lineage_process_table_rows_selected
      steps <- lineage_steps()
      if (length(row) != 1L || row > nrow(steps)) return()
      x <- steps[row, , drop = FALSE]
      updateTextAreaInput(session, "step_description", value = x$description[1] %||% "")
      updateTextAreaInput(session, "step_rationale", value = x$rationale[1] %||% "")
      updateTextInput(session, "step_datetime", value = x$step_datetime[1] %||% "")
      updateTextInput(session, "step_tool", value = x$tool_software_reference[1] %||% "")
      updateTextInput(session, "step_script", value = x$script_reference[1] %||% "")
      updateTextInput(session, "step_output", value = x$output_description[1] %||% "")
      updateSelectInput(session, "step_person_id", selected = x$processor_person_id[1] %||% "")
      updateSelectInput(session, "step_organisation_id", selected = x$processor_organisation_id[1] %||% "")
      updateSelectInput(session, "step_role", selected = x$role_code[1] %||% "processor")
      updateSelectInput(session, "step_origin", selected = x$value_origin_code[1] %||% "user")
      selected_sources <- if (is.na(x$source_ids[1])) character() else strsplit(x$source_ids[1], ",", fixed = TRUE)[[1]]
      updateSelectizeInput(session, "step_source_ids", selected = selected_sources)
      updateActionButton(session, "save_step", label = "Update process step")
    })

    observeEvent(input$clear_source, { access_require("edit", touch = TRUE); clear_source_editor() })
    observeEvent(input$clear_step, { access_require("edit", touch = TRUE); clear_step_editor() })

    parse_resolution <- function(value) {
      value <- trimws(as.character(value %||% ""))
      if (!nzchar(value)) return(list(value = NA_real_, unit = NA_character_))
      match <- regexec("^([0-9]+(?:\\.[0-9]+)?)\\s*(.*)$", value)
      parts <- regmatches(value, match)[[1]]
      if (length(parts) == 0L) stop("Resolution must look like '30 m'.")
      list(value = as.numeric(parts[2]), unit = if (nzchar(parts[3])) parts[3] else NA_character_)
    }

    observeEvent(input$save_source, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(state, "saving a lineage source")) return()
      req(state$current_dataset_id)
      selected <- input$lineage_source_table_rows_selected
      sources <- lineage_sources()
      source_id <- if (length(selected) == 1L && selected <= nrow(sources)) sources$id[selected] else NULL
      user_person_id <- get_active_user_person_id(state$active_user_id, state$data_pond)
      tryCatch({
        resolution <- parse_resolution(input$source_resolution)
        save_dataset_lineage_source(
          state$current_dataset_id,
          list(
            title = input$source_title, description = input$source_detail_description,
            identifier = input$source_identifier, identifier_code_space = input$source_identifier_code_space,
            source_uri = input$source_uri, citation_date = input$source_citation_date,
            citation_date_type_code = input$source_citation_date_type,
            temporal_start = input$source_temporal_start, temporal_end = input$source_temporal_end,
            equivalent_scale_denominator = input$source_scale,
            spatial_resolution_value = resolution$value, spatial_resolution_unit = resolution$unit,
            responsible_person_id = input$source_person_id,
            responsible_organisation_id = input$source_organisation_id,
            role_code = input$source_role, supporting_document_id = input$source_document_id,
            value_origin_code = input$source_origin
          ),
          state$data_pond, user_person_id, source_id
        )
        state$refresh_lineage <- state$refresh_lineage + 1L
        clear_source_editor()
        showNotification("Lineage source saved.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    observeEvent(input$save_step, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(state, "saving a lineage process step")) return()
      req(state$current_dataset_id)
      selected <- input$lineage_process_table_rows_selected
      steps <- lineage_steps()
      step_id <- if (length(selected) == 1L && selected <= nrow(steps)) steps$id[selected] else NULL
      user_person_id <- get_active_user_person_id(state$active_user_id, state$data_pond)
      tryCatch({
        step_id <- save_dataset_lineage_process_step(
          state$current_dataset_id,
          list(
            description = input$step_description, rationale = input$step_rationale,
            step_datetime = input$step_datetime, processor_person_id = input$step_person_id,
            processor_organisation_id = input$step_organisation_id, role_code = input$step_role,
            tool_software_reference = input$step_tool, script_reference = input$step_script,
            output_description = input$step_output, value_origin_code = input$step_origin
          ),
          state$data_pond, user_person_id, step_id
        )
        set_dataset_lineage_step_sources(
          state$current_dataset_id, step_id, input$step_source_ids,
          state$data_pond, user_person_id
        )
        state$refresh_lineage <- state$refresh_lineage + 1L
        clear_step_editor()
        showNotification("Lineage process step saved.", type = "message")
      }, error = function(e) showNotification(conditionMessage(e), type = "error"))
    })

    lineage_selected_id <- function(type) {
      if (type == "source") {
        row <- input$lineage_source_table_rows_selected
        x <- lineage_sources()
      } else {
        row <- input$lineage_process_table_rows_selected
        x <- lineage_steps()
      }
      if (length(row) != 1L || row > nrow(x)) return(NA_integer_)
      as.integer(x$id[row])
    }

    observeEvent(input$delete_source, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(state, "deleting a lineage source")) return()
      id <- lineage_selected_id("source")
      if (is.na(id)) return()
      delete_dataset_lineage_source(state$current_dataset_id, id, state$data_pond)
      state$refresh_lineage <- state$refresh_lineage + 1L
      clear_source_editor()
    })
    observeEvent(input$delete_step, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(state, "deleting a lineage process step")) return()
      id <- lineage_selected_id("process_step")
      if (is.na(id)) return()
      delete_dataset_lineage_process_step(state$current_dataset_id, id, state$data_pond)
      state$refresh_lineage <- state$refresh_lineage + 1L
      clear_step_editor()
    })

    for (spec in list(
      list(input = "move_source_up", type = "source", direction = "up"),
      list(input = "move_source_down", type = "source", direction = "down"),
      list(input = "move_step_up", type = "process_step", direction = "up"),
      list(input = "move_step_down", type = "process_step", direction = "down")
    )) local({
      current_spec <- spec
      observeEvent(input[[current_spec$input]], {
        access_require("read", touch = TRUE)
        if (!require_active_user_for_write(state, "reordering lineage records")) return()
        id <- lineage_selected_id(current_spec$type)
        if (is.na(id)) return()
        move_dataset_lineage_record(
          state$current_dataset_id, id, current_spec$direction,
          current_spec$type, state$data_pond
        )
        state$refresh_lineage <- state$refresh_lineage + 1L
      })
    })

    output$lineage_document_picker <- renderUI({
      access_require("read")
      docs <- supporting_docs()

      if (nrow(docs) == 0L) {
        return(tags$div(
          class = "alert alert-info",
          lineage_field_label("Supporting documents containing lineage", "Conditional",
            "Select at least one document before saving when Supporting document(s) is the chosen authority, in either population mode."),
          "No supporting documents have been scanned for this dataset. Scan them to select a document; this is optional with structured-field authority."
        ))
      }

      document_types <- as.character(docs$document_type_label)
      document_types[
        is.na(document_types) | trimws(document_types) == ""
      ] <- "Supporting document"
      labels <- paste0(
        docs$document_rel_path,
        " - ",
        document_types
      )
      choices <- stats::setNames(as.character(docs$id), labels)
      selected <- as.character(
        docs$id[as.integer(docs$include_in_lineage) == 1L]
      )

      checkboxGroupInput(
        session$ns("lineage_document_ids"),
        lineage_field_label("Supporting documents containing lineage", "Conditional",
          "Select at least one document before saving when Supporting document(s) is the chosen authority, in either population mode."),
        choices = choices,
        selected = selected
      )
    })

    observeEvent(input$open_supporting_documents, {
      access_require("edit", touch = TRUE)
      if (is.function(open_supporting_documents)) {
        open_supporting_documents()
      }
    })

    observeEvent(state$current_dataset_id, {
      access_require("read", touch = TRUE)
      clear_source_editor()
      clear_step_editor()
      x <- current_lineage()
      set_authority_expanded(is.null(x) || nrow(x) == 0L)
      if (is.null(x) || nrow(x) == 0) {
        updateTextAreaInput(session, "lineage_statement", value = "")
        updateRadioButtons(
          session,
          "lineage_source_mode",
          selected = "form"
        )
        updateSelectInput(session, "lineage_status", selected = "draft")
        return()
      }
      updateTextAreaInput(session, "lineage_statement", value = x$lineage_statement[1] %||% "")
      updateRadioButtons(
        session,
        "lineage_source_mode",
        selected = x$lineage_source_mode[1] %||% "form"
      )
      updateSelectInput(session, "lineage_status", selected = x$lineage_status[1] %||% "draft")
    }, ignoreInit = FALSE)

    observeEvent(input$save_lineage, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "saving dataset lineage"
      )) {
        return()
      }

      lineage_source_mode <- input$lineage_source_mode %||% "form"
      lineage_document_ids <- input$lineage_document_ids %||% integer()

      if (identical(lineage_source_mode, "supporting_documents") &&
          length(lineage_document_ids) == 0L) {
        set_authority_expanded(TRUE)
        showNotification(
          paste(
            "Select at least one authoritative supporting document,",
            "or choose the structured-fields option."
          ),
          type = "error"
        )
        return()
      }

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
        data = lineage_summary_edit_values(list(
          lineage_source_mode = lineage_source_mode,
          lineage_statement = input$lineage_statement,
          lineage_status = input$lineage_status
        ), get_dataset_lineage(state$current_dataset_id, state$data_pond)),
        base_path = state$data_pond,
        user_person_id = user_person_id
      )

      set_lineage_supporting_documents(
        dataset_id = state$current_dataset_id,
        document_ids = lineage_document_ids,
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
        app_version = as.character(utils::packageVersion("datapond")),
        notes = NA_character_
      )

      state$refresh_lineage <- state$refresh_lineage + 1
      state$refresh_supporting_docs <-
        state$refresh_supporting_docs + 1L
      state$refresh_metadata_reviews <- state$refresh_metadata_reviews + 1

      set_authority_expanded(FALSE)
      showNotification("Lineage saved.", type = "message")
    })

    output$lineage_preview <- renderTable({
      access_require("read")
      x <- current_lineage()
      if (is.null(x) || nrow(x) == 0) {
        return(NULL)
      }
      display <- x[, c(
        "lineage_source_mode",
        "lineage_status",
        "lineage_statement",
        "updated_at"
      ), drop = FALSE]
      authority_labels <- c(form = "Structured fields", supporting_documents = "Supporting documents")
      display$lineage_source_mode <- unname(authority_labels[display$lineage_source_mode])
      display$lineage_status <- humanise_code_value(display$lineage_status)
      names(display) <- friendly_field_label(names(display))
      names(display)[names(display) == "Updated"] <- "Last updated"
      display
    }, striped = TRUE, rownames = FALSE)

    output$lineage_docs_table <- renderTable({
      access_require("read")
      display <- lineage_docs()
      names(display) <- friendly_field_label(names(display))
      names(display)[names(display) == "ID"] <- "Document ID"
      display
    }, striped = TRUE, rownames = FALSE)

    # clear the lineage forms
    clear_lineage_inputs <- function() {
      set_authority_expanded(TRUE)
      updateTextAreaInput(
        session,
        "lineage_statement",
        value = ""
      )

      updateRadioButtons(
        session,
        "lineage_source_mode",
        selected = "form"
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
