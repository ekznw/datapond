# ISO metadata Wave 3 editors ----------------------------------------------

mod_vertical_extents_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "datapond-specialist-metadata",
    metadata_optional_details(
      "Vertical extents \u2014 height and depth coverage",
      shiny::tags$p("Describe the range of heights, depths or pressures represented by the data. Skip this section if vertical coverage is not relevant. Use the reference system documented with your source data."),
      shiny::helpText("Complete one extent, then choose Add / update extent. Select a row to edit it, or Clear form to start another. Save dataset to keep your changes."),
      DT::DTOutput(ns("vertical_table")),
      metadata_field_help(
        shiny::textInput(ns("vertical_label"), "Extent label"),
        ns("vertical_label"),
        "Give this range a short name, such as \"Survey depths\" or \"Terrain elevation\". Useful when the dataset has several vertical ranges."
      ),
      shiny::fluidRow(
        shiny::column(6, metadata_field_help(
            shiny::numericInput(ns("vertical_min"), metadata_field_label("Minimum", "Conditional", "Required when adding or updating a vertical extent."), value = NA_real_),
            ns("vertical_min"),
            "Enter the lower numeric bound in the unit below. Keep the sign convention used by the source; for example, -20 for heights below the reference surface."
        )),
        shiny::column(2, metadata_field_help(
            shiny::numericInput(ns("vertical_max"), metadata_field_label("Maximum", "Conditional", "Required when adding or updating a vertical extent."), value = NA_real_),
            ns("vertical_max"),
            "Enter the upper numeric bound using the same unit and reference as the minimum. It must be greater than or equal to the minimum."
        )),
        shiny::column(2, metadata_field_help(
            shiny::textInput(ns("vertical_unit"), metadata_field_label("Unit", "Conditional", "Required when adding or updating a vertical extent."), placeholder = "m"),
            ns("vertical_unit"),
            "State the unit used for both bounds, for example m for metres or hPa for pressure. Required when adding an extent."
        )),
        shiny::column(2, metadata_field_help(
            shiny::selectInput(ns("vertical_type"), metadata_field_label("Reference type", "Conditional", "Choose the reference type for this extent; Geoidal is selected by default."),
              choices = stats::setNames(iso_vertical_crs_types(), c("Geoidal (height)", "Depth", "Barometric (pressure)", "Other"))),
            ns("vertical_type"),
            "Choose what the values refer to: geoidal for heights relative to a geoid, depth for depths, barometric for pressure-based coverage, or other for another vertical reference."
        ))
      ),
      shiny::fluidRow(
        shiny::column(4, metadata_field_help(
            shiny::textInput(ns("vertical_crs"), metadata_field_label("Vertical CRS name", "Conditional", "Required when adding or updating a vertical extent."), placeholder = "EGM2008 height"),
            ns("vertical_crs"),
            "Name the vertical coordinate reference system (CRS): the reference used to interpret heights or depths. Copy it from the source documentation; this is required when adding an extent."
        )),
        shiny::column(4, metadata_field_help(
            shiny::textInput(ns("vertical_crs_id"), "Vertical CRS identifier", placeholder = "EPSG:3855"),
            ns("vertical_crs_id"),
            "If known, copy the authority and code identifying that vertical reference system, such as EPSG:3855 for EGM2008 height. Do not use the horizontal map CRS code here."
        )),
        shiny::column(4, metadata_field_help(
            shiny::textInput(ns("vertical_crs_uri"), "Vertical CRS URI"),
            ns("vertical_crs_uri"),
            "If available, enter the full web address or other absolute URI identifying the vertical reference system. Leave blank if no authoritative reference is known."
        ))
      ),
      metadata_field_help(
        shiny::textInput(ns("vertical_unit_uri"), "Unit URI (optional)"),
        ns("vertical_unit_uri"),
        "Optional: enter the full identifier or web address for the unit in a unit vocabulary. This identifies the unit formally; the short symbol still belongs in Unit."
      ),
      metadata_field_help(
        shiny::textAreaInput(ns("vertical_description"), "Vertical extent note", rows = 2),
        ns("vertical_description"),
        "Explain what the range represents and any caveats, including the reference surface and whether depth increases downwards. For example, \"Samples collected 0\u201320 m below the water surface\"."
      ),
      shiny::actionButton(ns("add_vertical"), "Add / update extent"),
      shiny::actionButton(ns("clear_vertical"), "Clear form"),
      shiny::actionButton(ns("remove_vertical"), "Remove extent")
    )
  )
}

mod_feature_catalogue_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "datapond-specialist-metadata",
    metadata_optional_details(
      "Data dictionary export (advanced)",
      shiny::tags$p("Describe your table columns and vector fields in Data files. DataPond can export those saved definitions as an ISO feature catalogue so recipients can interpret the data. You do not need to write a second dictionary here."),
      shiny::uiOutput(ns("catalogue_eligibility")),
      metadata_optional_details(
        "Export settings",
        metadata_field_help(
          shiny::checkboxInput(
            ns("catalogue_enabled"),
            "Include a data dictionary in ISO export",
            TRUE
          ),
          ns("catalogue_enabled"),
          "Uses saved vector and tabular field definitions. Raster band descriptions are handled separately. Clear to omit this export."
        ),
        metadata_field_help(
          shiny::textInput(ns("catalogue_title"), "Catalogue title override"),
          ns("catalogue_title"),
          "Leave blank to use the dataset title followed by \"feature catalogue\". Enter a title only to override it."
        ),
        shiny::fluidRow(
          shiny::column(4, metadata_field_help(
              shiny::textInput(ns("catalogue_version"), "Version override"),
              ns("catalogue_version"),
              "Leave blank to use the dataset version, or enter a separate dictionary version, e.g. 1.2."
          )),
          shiny::column(4, metadata_field_help(
              shiny::dateInput(ns("catalogue_date"), "Version date override", value = NULL, format = "yyyy-mm-dd"),
              ns("catalogue_date"),
              "Leave blank to use the dataset citation date, or enter the date of this dictionary edition."
          )),
          shiny::column(4, metadata_field_help(
              shiny::textInput(ns("catalogue_language"), "Language code", value = "eng"),
              ns("catalogue_language"),
              "Enter the three-letter language code for the catalogue descriptions, for example eng for English, afr for Afrikaans or zul for isiZulu."
          ))
        )
      )
    )
  )
}

wave3_empty_vertical <- function() data.frame(
  extent_label = character(), minimum_value = numeric(), maximum_value = numeric(),
  unit_symbol = character(), unit_uri = character(), vertical_crs_name = character(),
  vertical_crs_identifier = character(), vertical_crs_uri = character(),
  vertical_crs_type = character(), description = character(), stringsAsFactors = FALSE
)

mod_iso_wave3_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    vertical <- shiny::reactiveVal(wave3_empty_vertical())
    catalogue_uuid <- shiny::reactiveVal(NA_character_)
    output$vertical_table <- DT::renderDT({ access_require("read"); datapond_datatable(
      vertical()[, c("extent_label", "minimum_value", "maximum_value", "unit_symbol", "vertical_crs_name"), drop = FALSE],
      rownames = FALSE, selection = "single", options = list(dom = "t", paging = FALSE, ordering = FALSE)
    ) })
    eligible_attributes <- shiny::reactive({
      state$refresh_attributes
      id <- suppressWarnings(as.integer(state$current_dataset_id[1]))
      if (!length(id) || is.na(id)) return(data.frame())
      dataset_feature_catalogue_attributes(id, state$data_pond, coverage = FALSE)
    })
    output$catalogue_eligibility <- shiny::renderUI({
      access_require("read")
      n <- nrow(eligible_attributes())
      shiny::tags$p(if (n) paste(n, "eligible vector/tabular attribute(s) will be exported.")
                    else "No saved vector or tabular attributes are currently eligible.")
    })
    clear_vertical_form <- function() {
      shiny::updateTextInput(session, "vertical_label", value = "")
      shiny::updateNumericInput(session, "vertical_min", value = NA_real_)
      shiny::updateNumericInput(session, "vertical_max", value = NA_real_)
      for (field in c("vertical_unit", "vertical_crs", "vertical_crs_id", "vertical_crs_uri", "vertical_unit_uri"))
        shiny::updateTextInput(session, field, value = "")
      shiny::updateTextAreaInput(session, "vertical_description", value = "")
      shiny::updateSelectInput(session, "vertical_type", selected = "geoidal")
      DT::selectRows(DT::dataTableProxy("vertical_table", session = session), NULL)
    }
    shiny::observeEvent(input$vertical_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$vertical_table_rows_selected; rows <- vertical()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateTextInput(session, "vertical_label", value = row$extent_label %||% "")
      shiny::updateNumericInput(session, "vertical_min", value = row$minimum_value)
      shiny::updateNumericInput(session, "vertical_max", value = row$maximum_value)
      shiny::updateTextInput(session, "vertical_unit", value = row$unit_symbol)
      shiny::updateTextInput(session, "vertical_unit_uri", value = row$unit_uri %||% "")
      shiny::updateTextInput(session, "vertical_crs", value = row$vertical_crs_name)
      shiny::updateTextInput(session, "vertical_crs_id", value = row$vertical_crs_identifier %||% "")
      shiny::updateTextInput(session, "vertical_crs_uri", value = row$vertical_crs_uri %||% "")
      shiny::updateSelectInput(session, "vertical_type", selected = row$vertical_crs_type)
      shiny::updateTextAreaInput(session, "vertical_description", value = row$description %||% "")
    })
    shiny::observeEvent(input$add_vertical, {
      access_require("edit", touch = TRUE)
      row <- data.frame(
        extent_label = input$vertical_label %||% "", minimum_value = input$vertical_min,
        maximum_value = input$vertical_max, unit_symbol = input$vertical_unit %||% "",
        unit_uri = input$vertical_unit_uri %||% "", vertical_crs_name = input$vertical_crs %||% "",
        vertical_crs_identifier = input$vertical_crs_id %||% "", vertical_crs_uri = input$vertical_crs_uri %||% "",
        vertical_crs_type = input$vertical_type, description = input$vertical_description %||% "", stringsAsFactors = FALSE)
      row <- tryCatch(validate_vertical_extent_row(row), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error"); NULL
      })
      if (is.null(row)) return()
      rows <- vertical(); selected <- input$vertical_table_rows_selected
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      vertical(rows); clear_vertical_form()
    })
    shiny::observeEvent(input$clear_vertical, { access_require("edit", touch = TRUE); clear_vertical_form() })
    shiny::observeEvent(input$remove_vertical, {
      access_require("edit", touch = TRUE)
      selected <- input$vertical_table_rows_selected; rows <- vertical()
      if (length(selected) == 1L && selected <= nrow(rows)) vertical(rows[-selected, , drop = FALSE])
      clear_vertical_form()
    })
    feature_catalogue <- shiny::reactive({
      dataset_id <- suppressWarnings(as.integer(state$current_dataset_id[1]))
      record <- if (length(dataset_id) && !is.na(dataset_id)) {
        as.data.frame(get_dataset_record(dataset_id, state$data_pond), stringsAsFactors = FALSE)
      } else {
        data.frame()
      }
      record_value <- function(field) {
        if (!nrow(record) || !field %in% names(record)) return(NA_character_)
        wave3_optional_text(record[[field]][1])
      }
      eligible <- nrow(eligible_attributes()) > 0L
      enabled <- !identical(input$catalogue_enabled, FALSE)
      title <- wave3_optional_text(input$catalogue_title)
      active <- enabled && (
        eligible || !is.na(title) || !is.na(wave3_optional_text(catalogue_uuid()))
      )
      if (eligible && enabled && is.na(title)) {
        dataset_title <- wave3_text_default(record_value("title"), "Dataset")
        title <- paste(dataset_title, "feature catalogue", sep = " - ")
      }
      version <- wave3_optional_text(input$catalogue_version)
      if (eligible && enabled && is.na(version)) version <- record_value("version")
      version_date <- if (is.null(input$catalogue_date) || is.na(input$catalogue_date)) {
        record_value("citation_date")
      } else {
        format(input$catalogue_date, "%Y-%m-%d")
      }
      language <- wave3_optional_text(input$catalogue_language)
      if (is.na(language)) language <- wave3_text_default(record_value("metadata_language_code"), "eng")
      list(
        catalogue_uuid = catalogue_uuid(),
        title = if (active) title else NA_character_,
        version = version,
        version_date = version_date,
        language_code = language,
        producer_person_id = NA_integer_,
        producer_organisation_id = NA_integer_
      )
    })
    clear <- function() {
      vertical(wave3_empty_vertical()); catalogue_uuid(NA_character_); clear_vertical_form()
      shiny::updateTextInput(session, "catalogue_title", value = "")
      shiny::updateTextInput(session, "catalogue_version", value = "")
      shiny::updateDateInput(session, "catalogue_date", value = NULL)
      shiny::updateTextInput(session, "catalogue_language", value = "eng")
      shiny::updateCheckboxInput(session, "catalogue_enabled", value = TRUE)
      invisible(TRUE)
    }
    load <- function(dataset_id) {
      clear(); if (is.null(dataset_id) || is.na(as.integer(dataset_id[1]))) return(invisible(TRUE))
      rows <- get_dataset_vertical_extents(dataset_id, state$data_pond)
      if (nrow(rows)) vertical(rows[, names(wave3_empty_vertical()), drop = FALSE])
      fc <- get_dataset_feature_catalogue(dataset_id, state$data_pond)
      if (nrow(fc)) {
        catalogue_uuid(fc$catalogue_uuid[1]); shiny::updateTextInput(session, "catalogue_title", value = fc$title[1])
        shiny::updateTextInput(session, "catalogue_version", value = fc$version[1] %||% "")
        shiny::updateDateInput(session, "catalogue_date", value = if (is.na(fc$version_date[1])) NULL else as.Date(fc$version_date[1]))
        shiny::updateTextInput(session, "catalogue_language", value = fc$language_code[1])
      }
      invisible(TRUE)
    }
    session$onFlushed(function() clear(), once = TRUE)
    list(vertical_extents = vertical, feature_catalogue = feature_catalogue, clear = clear, load = load)
  })
}
