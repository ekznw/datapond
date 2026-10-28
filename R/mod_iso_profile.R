metadata_uuid_input <- function(input_id) {
  htmltools::tagQuery(
    shiny::textInput(input_id, metadata_field_label("Metadata-record UUID", "Optional", "Generated automatically; no entry is needed."))
  )$find("input")$addAttrs(readonly = "readonly")$allTags()
}

mod_metadata_profile_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(
    class = "metadata-profile-details",
    shiny::tags$summary("Advanced metadata profile"),
    shiny::tags$div(
      style = "padding-top:12px;",
      metadata_uuid_input(ns("metadata_uuid")),
      shiny::selectInput(
        ns("metadata_scope"), metadata_field_label("Metadata resource scope", "Mandatory", "Identifies the resource being described; Geographic dataset is selected by default."),
        choices = c("Geographic dataset (including tables about places)" = "dataset",
          "Dataset series" = "series", "Service" = "service", "Software" = "software",
          "Model" = "model", "Nongeographic dataset" = "nonGeographicDataset"),
        selected = "dataset"
      ),
      shiny::textInput(ns("geographic_extent_name"), metadata_field_label("Area covered", "Conditional", "Needed for geographic ISO export when no bounding box is available. Coordinates are entered in Spatial metadata."),
        placeholder = "e.g. KwaZulu-Natal, South Africa"),
      shiny::helpText("Name the area if coordinates are unavailable, including for tables about places. Place keywords are not copied here."),
      shiny::textInput(ns("metadata_creation_date"), metadata_field_label("Metadata-record creation date", "Optional", "Set automatically on first save; enter a known historical date only when needed."),
        placeholder = "Automatic on first save; historical dates may use YYYY-MM-DD"),
      shiny::helpText("Set automatically on first save. For older records, use the known creation date at its original precision, not the publication or review date."),
      shiny::selectInput(
        ns("metadata_language"), metadata_field_label("Metadata language", "Mandatory", "Required for complete metadata; English is selected by default."),
        choices = c("English" = "eng", "Afrikaans" = "afr", "isiZulu" = "zul"),
        selected = "eng"
      ),
      shiny::selectInput(
        ns("resource_language"), metadata_field_label("Dataset language", "Conditional", "Required when exporting the data dictionary."),
        choices = c("Not recorded" = "", "English" = "eng", "Afrikaans" = "afr", "isiZulu" = "zul", "Undetermined" = "und"),
        selected = ""
      ),
      shiny::helpText("Language used within the data. Needed when exporting its data dictionary."),
      shiny::selectInput(ns("resource_encoding"), "Dataset character encoding",
        choices = c("Not recorded" = "", "UTF-8" = "utf8", "UTF-16" = "utf16"), selected = ""),
      shiny::selectInput(
        ns("metadata_encoding"), metadata_field_label("Metadata character encoding", "Mandatory", "Required for complete metadata; UTF-8 is selected by default."),
        choices = c("UTF-8" = "utf8", "UTF-16" = "utf16"),
        selected = "utf8"
      ),
      shiny::tags$small(
        class = "text-muted",
        "Geographic datasets need a bounding box or named area for ISO export. Coordinate-based extents need a CRS; nongeographic datasets need neither. Draft saving remains available."
      )
    )
  )
}

mod_topic_categories_ui <- function(id) {
  ns <- shiny::NS(id)
  labels <- c(
    farming = "Farming", biota = "Biota", boundaries = "Boundaries",
    climatologyMeteorologyAtmosphere = "Climatology / meteorology / atmosphere",
    economy = "Economy", elevation = "Elevation", environment = "Environment",
    geoscientificInformation = "Geoscientific information", health = "Health",
    imageryBaseMapsEarthCover = "Imagery / base maps / earth cover",
    intelligenceMilitary = "Intelligence / military", inlandWaters = "Inland waters",
    location = "Location", oceans = "Oceans", planningCadastre = "Planning / cadastre",
    society = "Society", structure = "Structure", transportation = "Transportation",
    utilitiesCommunication = "Utilities / communication"
  )
  shiny::tagList(
    shiny::h4("ISO topic categories"),
    shiny::selectizeInput(
      ns("topic_categories"),
      metadata_field_label("Select one or more controlled discovery categories", "Conditional", "Required to save in Full metadata mode when resource scope is Geographic dataset."),
      choices = stats::setNames(names(labels), unname(labels)),
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        placeholder = "Choose the closest ISO category..."
      )
    ),
    shiny::tags$small(
      class = "text-muted",
      "Broad, standard categories for discovery. Use keywords for more specific subjects."
    ),
    shiny::tags$br(), shiny::tags$br()
  )
}

mod_citation_dates_ui <- function(id, heading = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (isTRUE(heading)) shiny::h4("Additional typed citation dates"),
    shiny::tags$p(
      class = "text-muted",
      "Add creation, revision or extra publication dates. The main Citation date remains the primary publication date. Skip this section if there are no other known dates."
    ),
    DT::DTOutput(ns("citation_dates_table")),
    metadata_field_help(
      shiny::textInput(ns("date_value"), metadata_field_label("Date", "Conditional", "Required when adding an additional citation date."), placeholder = "YYYY, YYYY-MM, or YYYY-MM-DD"),
      ns("date_value"),
      "Enter when the selected dataset event happened. Use only the detail you know, then match the Precision setting below. This is not the creation date of the metadata record."
    ),
    shiny::fluidRow(
      shiny::column(6, metadata_field_help(
        shiny::selectInput(
          ns("date_type"), metadata_field_label("Date type", "Conditional", "Required when adding an additional citation date; Creation is selected by default."),
          choices = c("Creation" = "creation", "Revision" = "revision", "Publication" = "publication")
        ),
        ns("date_type"),
        "Creation means the dataset was first produced; Revision means it was updated; Publication means it was released or made available. Choose the event this date describes."
      )),
      shiny::column(6, metadata_field_help(
        shiny::selectInput(
          ns("date_precision"), metadata_field_label("Precision", "Conditional", "Match the precision of the date being added."),
          choices = c("Day" = "day", "Month" = "month", "Year" = "year", "Date and time" = "dateTime")
        ),
        ns("date_precision"),
        "Match the recorded detail: Year for 2024, Month for 2024-06, Day for 2024-06-18, or Date and time for 2024-06-18T09:30:00. Do not invent missing detail."
      ))
    ),
    metadata_field_help(
      shiny::textInput(ns("date_note"), "Date note"),
      ns("date_note"),
      "Optional context or source, e.g. 'Revision date from the survey report'. Leave blank if the date needs no explanation."
    ),
    shiny::helpText("Choose Add date to add an entry. Select a row to edit or remove it; Clear date form starts another entry. Move up / down changes the list order. Save dataset to keep your changes."),
    shiny::actionButton(ns("add_date"), "Add date"),
    shiny::actionButton(ns("clear_date"), "Clear date form"),
    shiny::actionButton(ns("move_date_up"), "Move up"),
    shiny::actionButton(ns("move_date_down"), "Move down"),
    shiny::actionButton(ns("remove_date"), "Remove date")
  )
}

mod_metadata_responsibilities_ui <- function(id, heading = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (isTRUE(heading)) shiny::h4("Metadata responsibilities"),
    shiny::tags$p(
      class = "text-muted",
      paste(
        "The active editor is the default metadata contact when no primary contact is assigned.",
        "Add a person, an organisation or both to name other responsible parties."
      )
    ),
    DT::DTOutput(ns("responsibility_table")),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput(ns("responsibility_person"), metadata_field_label("Person", "Conditional", "When adding a responsibility, select a person, an organisation, or both."), choices = NULL)),
      shiny::column(4, shiny::selectInput(ns("responsibility_organisation"), metadata_field_label("Organisation", "Conditional", "Required when adding a responsibility without a person."), choices = NULL)),
      shiny::column(4, shiny::selectInput(
        ns("responsibility_role"), metadata_field_label("Responsibility", "Conditional", "Choose the responsibility when adding an entry."),
        choices = c(
          "Metadata creator" = "creator", "Metadata maintainer" = "maintainer",
          "Metadata point of contact" = "pointOfContact"
        )
      ))
    ),
    shiny::checkboxInput(ns("responsibility_primary"), "Primary for this responsibility", FALSE),
    shiny::helpText("Primary means the main person or organisation for this role. Choose Add responsibility, then Save dataset to keep it."),
    shiny::actionButton(ns("add_responsibility"), "Add responsibility"),
    shiny::actionButton(ns("clear_responsibility"), "Clear responsibility form"),
    shiny::actionButton(ns("remove_responsibility"), "Remove responsibility")
  )
}

empty_iso_citation_dates <- function() data.frame(
  date_value = character(), date_type_code = character(),
  date_precision = character(), date_note = character(), stringsAsFactors = FALSE
)

empty_metadata_responsibilities <- function() data.frame(
  person_id = integer(), organisation_id = integer(), role_code = character(),
  is_primary = integer(), person_name = character(), organisation_name = character(),
  stringsAsFactors = FALSE
)

move_iso_editor_row <- function(rows, selected, direction) {
  selected <- suppressWarnings(as.integer(selected[1]))
  if (length(selected) == 0L || is.na(selected) || nrow(rows) < 2L) return(rows)
  target <- selected + if (direction == "up") -1L else 1L
  if (target < 1L || target > nrow(rows)) return(rows)
  rows[c(selected, target), ] <- rows[c(target, selected), ]
  rownames(rows) <- NULL
  rows
}

mod_iso_profile_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    citation_dates <- shiny::reactiveVal(empty_iso_citation_dates())
    responsibilities <- shiny::reactiveVal(empty_metadata_responsibilities())
    loaded_record <- shiny::reactiveVal(FALSE)

    people_choices <- shiny::reactive({
      state$refresh_person
      people <- get_persons(state$data_pond)
      if (nrow(people) == 0L) return(character())
      first <- as.character(people$name_first); first[is.na(first)] <- ""
      last <- as.character(people$name_last); last[is.na(last)] <- ""
      labels <- trimws(paste(first, last))
      labels[!nzchar(labels)] <- paste("Person", people$id[!nzchar(labels)])
      stats::setNames(as.character(people$id), labels)
    })
    organisation_choices <- shiny::reactive({
      state$refresh_org
      organisation_select_choices(get_organisations(state$data_pond))
    })
    shiny::observe({
      access_require("read")
      shiny::updateSelectInput(
        session, "responsibility_person",
        choices = c("No person" = "", people_choices())
      )
      shiny::updateSelectInput(
        session, "responsibility_organisation",
        choices = c("No organisation" = "", organisation_choices())
      )
    })

    output$citation_dates_table <- DT::renderDT({
      access_require("read")
      rows <- citation_dates()
      display <- if (nrow(rows) == 0L) data.frame(
        Date = character(), Type = character(), Precision = character(), Note = character()
      ) else data.frame(
        Date = rows$date_value, Type = rows$date_type_code,
        Precision = rows$date_precision, Note = rows$date_note
      )
      datapond_datatable(display, rownames = FALSE, selection = "single", options = list(
        dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE
      ), class = "compact stripe hover")
    })
    output$responsibility_table <- DT::renderDT({
      access_require("read")
      rows <- responsibilities()
      display <- if (nrow(rows) == 0L) data.frame(
        Person = character(), Organisation = character(), Responsibility = character(), Primary = character()
      ) else data.frame(
        Person = rows$person_name, Organisation = rows$organisation_name,
        Responsibility = rows$role_code, Primary = ifelse(rows$is_primary == 1L, "Yes", "No")
      )
      datapond_datatable(display, rownames = FALSE, selection = "single", options = list(
        dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE
      ), class = "compact stripe hover")
    })

    clear_date_form <- function() {
      shiny::updateTextInput(session, "date_value", value = "")
      shiny::updateSelectInput(session, "date_type", selected = "creation")
      shiny::updateSelectInput(session, "date_precision", selected = "day")
      shiny::updateTextInput(session, "date_note", value = "")
      shiny::updateActionButton(session, "add_date", label = "Add date")
      DT::selectRows(DT::dataTableProxy("citation_dates_table", session = session), NULL)
    }
    clear_responsibility_form <- function() {
      shiny::updateSelectInput(session, "responsibility_person", selected = "")
      shiny::updateSelectInput(session, "responsibility_organisation", selected = "")
      shiny::updateSelectInput(session, "responsibility_role", selected = "creator")
      shiny::updateCheckboxInput(session, "responsibility_primary", value = FALSE)
      shiny::updateActionButton(session, "add_responsibility", label = "Add responsibility")
      DT::selectRows(DT::dataTableProxy("responsibility_table", session = session), NULL)
    }

    shiny::observeEvent(input$citation_dates_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$citation_dates_table_rows_selected
      rows <- citation_dates()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateTextInput(session, "date_value", value = row$date_value)
      shiny::updateSelectInput(session, "date_type", selected = row$date_type_code)
      shiny::updateSelectInput(session, "date_precision", selected = row$date_precision)
      shiny::updateTextInput(session, "date_note", value = row$date_note %||% "")
      shiny::updateActionButton(session, "add_date", label = "Update date")
    })
    shiny::observeEvent(input$add_date, {
      access_require("edit", touch = TRUE)
      checked <- tryCatch(
        validate_iso_date_value(input$date_value, input$date_precision),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(checked)) return()
      row <- data.frame(
        date_value = checked$value, date_type_code = input$date_type,
        date_precision = checked$precision, date_note = input$date_note %||% "",
        stringsAsFactors = FALSE
      )
      rows <- citation_dates()
      selected <- input$citation_dates_table_rows_selected
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      citation_dates(rows)
      clear_date_form()
    })
    shiny::observeEvent(input$clear_date, { access_require("edit", touch = TRUE); clear_date_form() })
    shiny::observeEvent(input$remove_date, {
      access_require("edit", touch = TRUE)
      selected <- input$citation_dates_table_rows_selected
      rows <- citation_dates()
      if (length(selected) == 1L && selected <= nrow(rows)) citation_dates(rows[-selected, , drop = FALSE])
      clear_date_form()
    })
    shiny::observeEvent(input$move_date_up, {
      access_require("edit", touch = TRUE)
      citation_dates(move_iso_editor_row(citation_dates(), input$citation_dates_table_rows_selected, "up"))
    })
    shiny::observeEvent(input$move_date_down, {
      access_require("edit", touch = TRUE)
      citation_dates(move_iso_editor_row(citation_dates(), input$citation_dates_table_rows_selected, "down"))
    })

    shiny::observeEvent(input$responsibility_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$responsibility_table_rows_selected
      rows <- responsibilities()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateSelectInput(session, "responsibility_person", selected = row$person_id %||% "")
      shiny::updateSelectInput(session, "responsibility_organisation", selected = row$organisation_id %||% "")
      shiny::updateSelectInput(session, "responsibility_role", selected = row$role_code)
      shiny::updateCheckboxInput(session, "responsibility_primary", value = row$is_primary == 1L)
      shiny::updateActionButton(session, "add_responsibility", label = "Update responsibility")
    })
    shiny::observeEvent(input$add_responsibility, {
      access_require("edit", touch = TRUE)
      person_id <- iso_optional_integer(input$responsibility_person)
      organisation_id <- iso_optional_integer(input$responsibility_organisation)
      if (is.na(person_id) && is.na(organisation_id)) {
        shiny::showNotification("Select a person, an organisation, or both.", type = "error")
        return()
      }
      people <- people_choices(); organisations <- organisation_choices()
      row <- data.frame(
        person_id = person_id, organisation_id = organisation_id,
        role_code = input$responsibility_role,
        is_primary = as.integer(isTRUE(input$responsibility_primary)),
        person_name = if (is.na(person_id)) "" else names(people)[match(as.character(person_id), people)],
        organisation_name = if (is.na(organisation_id)) "" else names(organisations)[match(as.character(organisation_id), organisations)],
        stringsAsFactors = FALSE
      )
      rows <- responsibilities()
      if (row$is_primary == 1L && nrow(rows) > 0L) {
        rows$is_primary[rows$role_code == row$role_code] <- 0L
      }
      selected <- input$responsibility_table_rows_selected
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      responsibilities(rows)
      clear_responsibility_form()
    })
    shiny::observeEvent(input$clear_responsibility, { access_require("edit", touch = TRUE); clear_responsibility_form() })
    shiny::observeEvent(input$remove_responsibility, {
      access_require("edit", touch = TRUE)
      selected <- input$responsibility_table_rows_selected
      rows <- responsibilities()
      if (length(selected) == 1L && selected <= nrow(rows)) responsibilities(rows[-selected, , drop = FALSE])
      clear_responsibility_form()
    })

    clear <- function() {
      loaded_record(FALSE)
      shiny::updateTextInput(session, "metadata_uuid", value = uuid::UUIDgenerate())
      shiny::updateTextInput(session, "geographic_extent_name", value = "")
      shiny::updateTextInput(session, "metadata_creation_date", value = "")
      shiny::updateSelectInput(session, "metadata_scope", selected = "dataset")
      shiny::updateSelectInput(session, "metadata_language", selected = "eng")
      shiny::updateSelectInput(session, "resource_language", selected = "")
      shiny::updateSelectInput(session, "resource_encoding", selected = "")
      shiny::updateSelectInput(session, "metadata_encoding", selected = "utf8")
      shiny::updateSelectizeInput(session, "topic_categories", selected = character())
      citation_dates(empty_iso_citation_dates())
      responsibilities(empty_metadata_responsibilities())
      clear_date_form(); clear_responsibility_form()
      invisible(TRUE)
    }
    load <- function(dataset_id, record = NULL) {
      if (is.null(dataset_id) || is.na(dataset_id[1])) return(clear())
      if (is.null(record)) record <- get_dataset_record(dataset_id, state$data_pond)
      record <- as.data.frame(record, stringsAsFactors = FALSE)
      loaded_record(TRUE)
      shiny::updateTextInput(session, "geographic_extent_name", value = wave3_text_default(record$geographic_extent_name, ""))
      shiny::updateTextInput(session, "metadata_creation_date", value = wave3_text_default(record$metadata_creation_date, ""))
      shiny::updateTextInput(session, "metadata_uuid", value = record$metadata_uuid[1] %||% uuid::UUIDgenerate())
      shiny::updateSelectInput(session, "metadata_scope", selected = record$metadata_resource_scope_code[1] %||% "dataset")
      shiny::updateSelectInput(session, "metadata_language", selected = record$metadata_language_code[1] %||% "eng")
      shiny::updateSelectInput(session, "metadata_encoding", selected = record$metadata_character_encoding[1] %||% "utf8")
      shiny::updateSelectInput(session, "resource_language", selected = wave3_text_default(record$resource_language_code[1], ""))
      shiny::updateSelectInput(session, "resource_encoding", selected = wave3_text_default(record$resource_character_encoding[1], ""))
      topics <- get_dataset_topic_categories(dataset_id, state$data_pond)
      shiny::updateSelectizeInput(session, "topic_categories", selected = topics$topic_category_code)
      dates <- get_dataset_citation_dates(dataset_id, state$data_pond)
      publication <- which(dates$date_type_code == "publication")
      if (length(publication) > 0L) dates <- dates[-publication[1], , drop = FALSE]
      if (nrow(dates) == 0L) citation_dates(empty_iso_citation_dates()) else citation_dates(
        dates[, c("date_value", "date_type_code", "date_precision", "date_note"), drop = FALSE]
      )
      responsibilities(get_metadata_responsibilities(dataset_id, state$data_pond)[,
        c("person_id", "organisation_id", "role_code", "is_primary", "person_name", "organisation_name"), drop = FALSE
      ])
      clear_date_form(); clear_responsibility_form()
      invisible(TRUE)
    }

    session$onFlushed(function() clear(), once = TRUE)

    list(
      validate = shiny::reactive(tryCatch({
        if (non_empty_scalar(input$metadata_creation_date))
          iso_export_date(input$metadata_creation_date, "Metadata creation date")
        list(valid = TRUE, message = "")
      }, error = function(e) list(valid = FALSE, message = conditionMessage(e)))),
      profile = shiny::reactive({
        value <- list(
        metadata_uuid = input$metadata_uuid,
        geographic_extent_name = input$geographic_extent_name %||% "",
        metadata_resource_scope_code = input$metadata_scope %||% "dataset",
        metadata_language_code = input$metadata_language %||% "eng",
        resource_language_code = input$resource_language %||% "",
        resource_character_encoding = input$resource_encoding %||% "",
        metadata_character_encoding = input$metadata_encoding %||% "utf8"
        )
        # A blank unsaved form must not erase the timestamp assigned on INSERT.
        if (loaded_record() || non_empty_scalar(input$metadata_creation_date))
          value$metadata_creation_date <- input$metadata_creation_date %||% ""
        value
      }),
      topic_categories = shiny::reactive(input$topic_categories %||% character()),
      citation_dates = citation_dates,
      responsibilities = responsibilities,
      clear = clear,
      mark_saved = function(dataset_id) {
        record <- get_dataset_record(dataset_id, state$data_pond)
        shiny::updateTextInput(session, "metadata_creation_date",
          value = wave3_text_default(record$metadata_creation_date, ""))
        loaded_record(TRUE)
      },
      load = load
    )
  })
}
