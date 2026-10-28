# ISO metadata Wave 2 editors ----------------------------------------------

iso_quality_templates <- function() {
  data.frame(
    template_code = c("completeness", "logical", "positional", "thematic", "temporal"),
    measure_identifier = c(
      "DATAPOND-COMPLETENESS", "DATAPOND-LOGICAL-CONSISTENCY",
      "DATAPOND-POSITIONAL-ACCURACY", "DATAPOND-THEMATIC-ACCURACY",
      "DATAPOND-TEMPORAL-QUALITY"
    ),
    measure_name = c(
      "Completeness", "Logical consistency", "Positional accuracy",
      "Thematic accuracy", "Temporal quality"
    ),
    measure_type_code = iso_quality_measure_type_codes(),
    stringsAsFactors = FALSE
  )
}

wave2_online_resource_defaults <- function(
  resource_url,
  function_code,
  protocol = NA_character_,
  media_type = NA_character_
) {
  url <- wave2_optional_text(resource_url)
  function_code <- wave2_optional_text(function_code)
  if (is.na(function_code)) function_code <- "download"
  protocol <- wave2_optional_text(protocol)
  media_type <- wave2_optional_text(media_type)

  if (is.na(protocol) && !is.na(url) && grepl("^https?://", url, ignore.case = TRUE)) {
    protocol <- if (identical(function_code, "download")) {
      "WWW:DOWNLOAD-1.0-http--download"
    } else {
      "WWW:LINK-1.0-http--link"
    }
  }

  if (is.na(media_type) && !is.na(url)) {
    clean_url <- sub("[?#].*$", "", tolower(url))
    extension <- tools::file_ext(clean_url)
    media_types <- c(
      csv = "text/csv",
      geojson = "application/geo+json",
      gpkg = "application/geopackage+sqlite3",
      json = "application/json",
      pdf = "application/pdf",
      tif = "image/tiff",
      tiff = "image/tiff",
      zip = "application/zip"
    )
    if (extension %in% names(media_types)) media_type <- media_types[[extension]]
  }

  list(protocol = protocol, media_type = media_type)
}

mod_wave2_alternate_titles_ui <- function(id, heading = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (isTRUE(heading)) shiny::h4("Alternate titles"),
    DT::DTOutput(ns("alternate_titles_table")),
    metadata_field_help(
      shiny::textInput(ns("alternate_title"), metadata_field_label("Alternate title", "Conditional", "Required when adding an alternate title.")),
      ns("alternate_title"),
      "Another recognised name for this dataset, such as a former title, abbreviation or translated title. Add one title at a time; leave this section empty if the main title is sufficient."
    ),
    shiny::helpText("Choose Add title to add an entry. Select a row to replace its title or remove it. Save dataset to keep your changes."),
    shiny::actionButton(ns("add_alternate_title"), "Add title"),
    shiny::actionButton(ns("remove_alternate_title"), "Remove title")
  )
}

mod_wave2_temporal_extents_ui <- function(id, heading = TRUE) {
  ns <- shiny::NS(id)
  indeterminate <- c("Known value" = "", "Unknown" = "unknown", "Now / ongoing" = "now",
                     "Before value" = "before", "After value" = "after")
  precision <- c("Day" = "day", "Month" = "month", "Year" = "year", "Date and time" = "dateTime")
  shiny::tagList(
    if (isTRUE(heading)) shiny::h4("Temporal extents"),
    shiny::tags$p(
      class = "text-muted",
      "Record the period represented by the observations, not when the dataset was published. Add separate periods for distinct surveys or gaps. Skip this section if time coverage does not apply."
    ),
    DT::DTOutput(ns("temporal_extents_table")),
    shiny::fluidRow(
      shiny::column(6, metadata_field_help(
        shiny::textInput(ns("temporal_start"), metadata_field_label("Start", "Conditional", "When adding a period, enter a date or choose an indeterminate Start position."), placeholder = "YYYY-MM-DD"),
        ns("temporal_start"),
        "Earliest date covered by this period, e.g. 2024-06-01. If only the year or month is known, change Start precision below. For an unknown start, leave blank and choose Unknown under Start position."
      )),
      shiny::column(6, metadata_field_help(
        shiny::textInput(ns("temporal_end"), metadata_field_label("End", "Conditional", "When adding a period, enter a date or choose an indeterminate End position."), placeholder = "YYYY-MM-DD"),
        ns("temporal_end"),
        "Latest date covered by this period, e.g. 2024-08-31. For an ongoing period, leave blank and choose Now / ongoing under End position. For a single day, use the same date as Start."
      ))
    ),
    metadata_optional_details(
      "Advanced temporal options",
      metadata_field_help(
        shiny::textInput(ns("temporal_label"), "Extent label"),
        ns("temporal_label"),
        "Optional short name for this period, e.g. 'Summer survey 2024'. Useful when adding several periods; leave blank if a label is unnecessary."
      ),
      shiny::fluidRow(
        shiny::column(6, metadata_field_help(
          shiny::selectInput(ns("temporal_start_precision"), "Start precision", precision),
          ns("temporal_start_precision"),
          "Match Start: Year for 2024, Month for 2024-06, Day for 2024-06-01, or Date and time for a recorded timestamp such as 2024-06-01T09:30:00."
        )),
        shiny::column(6, metadata_field_help(
          shiny::selectInput(ns("temporal_end_precision"), "End precision", precision),
          ns("temporal_end_precision"),
          "Match End using the same date formats as Start precision. The two endpoints may have different precision; this setting is unused when End has no date."
        ))
      ),
      shiny::fluidRow(
        shiny::column(6, metadata_field_help(
          shiny::selectInput(ns("temporal_start_indeterminate"), metadata_field_label("Start position", "Conditional", "Choose an indeterminate position when no Start date is supplied."), indeterminate),
          ns("temporal_start_indeterminate"),
          "Use Known value with a Start date, or Unknown if it is not known. Other options mark a moving (Now), earlier (Before) or later (After) boundary. Every option except Known value requires Start to be blank."
        )),
        shiny::column(6, metadata_field_help(
          shiny::selectInput(ns("temporal_end_indeterminate"), metadata_field_label("End position", "Conditional", "Choose an indeterminate position when no End date is supplied."), indeterminate),
          ns("temporal_end_indeterminate"),
          "Use Known value with an End date, Unknown if it is not known, or Now / ongoing if coverage continues. Before / After mark an uncertain earlier / later boundary. Every option except Known value requires End to be blank."
        ))
      ),
      metadata_field_help(
        shiny::textAreaInput(ns("temporal_description"), "Extent note", rows = 2),
        ns("temporal_description"),
        "Optional context about gaps, uncertainty or how the dates were established, e.g. 'Monthly surveys; no sampling in July'. For Before / After positions, explain the reference event here."
      )
    ),
    shiny::helpText("Choose Add period to add an entry. Select a row to edit or remove it; Clear form starts another period. Move up / down changes the list order. Save dataset to keep your changes."),
    shiny::actionButton(ns("add_temporal_extent"), "Add period"),
    shiny::actionButton(ns("clear_temporal_extent"), "Clear form"),
    shiny::actionButton(ns("move_temporal_up"), "Move up"),
    shiny::actionButton(ns("move_temporal_down"), "Move down"),
    shiny::actionButton(ns("remove_temporal_extent"), "Remove extent")
  )
}

mod_wave2_distribution_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::hr(),
    shiny::h4("Online access"),
    shiny::tags$p(
      class = "text-muted",
      paste(
        "Enter a URL and add the resource. Use the details below only",
        "when you need to describe its purpose or access method."
      )
    ),
    DT::DTOutput(ns("online_resources_table")),
    shiny::textInput(ns("resource_url"), metadata_field_label("URL or URI", "Conditional", "Required when adding an online resource; use a complete URL or URI."), placeholder = "https://example.org/dataset"),
    metadata_optional_details(
      "Online-resource details",
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(ns("resource_name"), "Resource name")),
        shiny::column(6, shiny::selectInput(ns("resource_function"), metadata_field_label("Function", "Conditional", "Required when adding an online resource; a default is provided."), choices = stats::setNames(
          iso_online_resource_function_codes(), iso_online_resource_function_codes()
        )))
      ),
      shiny::textInput(ns("resource_protocol"), "Protocol", placeholder = "Automatically inferred for HTTP downloads"),
      shiny::textAreaInput(ns("resource_description"), "Resource description", rows = 2),
      shiny::textInput(ns("resource_media_type"), "Media type", placeholder = "text/html"),
      shiny::checkboxInput(ns("resource_primary"), "Primary online resource", FALSE)
    ),
    shiny::actionButton(ns("add_online_resource"), "Add resource"),
    shiny::actionButton(ns("clear_online_resource"), "Clear form"),
    shiny::actionButton(ns("move_resource_up"), "Move up"),
    shiny::actionButton(ns("move_resource_down"), "Move down"),
    shiny::actionButton(ns("remove_online_resource"), "Remove resource"),
    metadata_optional_details(
      "Explicit distribution-format specifications",
      shiny::tags$p(
        class = "text-muted",
        "Optional for inventory: add a specification when an explicit distribution format is needed in ISO export. Detected file types remain available in the file inventory."
      ),
      DT::DTOutput(ns("distribution_formats_table")),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(ns("format_title"), metadata_field_label("Format specification title", "Conditional", "Required when adding a distribution format."), placeholder = "GeoPackage")),
        shiny::column(3, shiny::textInput(ns("format_version"), "Version")),
        shiny::column(3, shiny::textInput(ns("format_media_type"), "Media type", placeholder = "application/geopackage+sqlite3"))
      ),
      shiny::textInput(ns("format_uri"), "Format specification URI"),
      shiny::actionButton(ns("add_distribution_format"), "Add format"),
      shiny::actionButton(ns("clear_distribution_format"), "Clear form"),
      shiny::actionButton(ns("remove_distribution_format"), "Remove format")
    )
  )
}

mod_wave2_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  templates <- iso_quality_templates()
  shiny::tagList(
    shiny::tags$p("Record tests you have actually performed to help others judge whether the data is suitable for their work. File scans alone do not establish data quality. Skip this section if no quality evaluation is available."),
    shiny::helpText("Choose a template, describe the test and record its result, then select Add quality result. Select a saved row to edit it, or Clear form to start another. Save dataset to keep your changes."),
    DT::DTOutput(ns("quality_results_table")),
    shiny::fluidRow(
      shiny::column(8, metadata_field_help(
          shiny::selectInput(ns("quality_template"), "Start from a quality template",
            choices = stats::setNames(templates$template_code, templates$measure_name)),
          ns("quality_template"),
          "Choose the kind of check you performed, then select Apply template to fill the measure identifier, name and type. Review those fields to match your actual test."
      )),
      shiny::column(4, shiny::br(), shiny::actionButton(ns("apply_quality_template"), "Apply template"))
    ),
    shiny::fluidRow(
      shiny::column(4, metadata_field_help(
          shiny::selectInput(ns("quality_scope"), metadata_field_label("Scope", "Conditional", "Required when adding a quality result; Dataset is selected by default."), choices = stats::setNames(
              iso_quality_scope_codes(), c("Dataset", "Dataset series", "Service", "Software", "Model",
                "Nongeographic dataset", "Feature type", "Attribute (field or column)")
          )),
          ns("quality_scope"),
          "Choose the level being assessed: Dataset for the whole resource, Dataset series for a collection, Feature type for a kind of feature, or Attribute for a field or column. Other scopes cover services, software, models and nongeographic data."
      )),
      shiny::column(4, metadata_field_help(
          shiny::selectInput(ns("quality_data_file"), "Data file", choices = c("Dataset-wide" = "")),
          ns("quality_data_file"),
          "Select the file that was tested. Leave Dataset-wide for a check covering the whole dataset. File choices come from the saved data-file inventory."
      )),
      shiny::column(4, metadata_field_help(
          shiny::selectInput(ns("quality_attribute"), metadata_field_label("Attribute", "Conditional", "Required when Scope is Attribute."), choices = c("No attribute" = "")),
          ns("quality_attribute"),
          "For an attribute-level check, select the field or column that was tested. Leave No attribute for broader checks; an attribute is required for attribute scope."
      ))
    ),
    shiny::fluidRow(
      shiny::column(4, metadata_field_help(
          shiny::textInput(ns("quality_measure_identifier"), metadata_field_label("Measure identifier", "Conditional", "Required when adding any quality result.")),
          ns("quality_measure_identifier"),
          "Enter a stable code or reference for the test, such as your organisation's documented test ID. Applying a template supplies a DataPond code. Required for every result."
      )),
      shiny::column(4, metadata_field_help(
          shiny::textInput(ns("quality_measure_name"), "Measure name"),
          ns("quality_measure_name"),
          "Give the test a readable name, such as \"Missing species names\" or \"Horizontal positional error\"."
      )),
      shiny::column(4, metadata_field_help(
          shiny::selectInput(ns("quality_measure_type"), metadata_field_label("Measure type", "Conditional", "Required when adding a result; choose a specific test before ISO export."), choices = iso_quality_element_choices()),
          ns("quality_measure_type"),
          "Choose the specific kind of error or quality being measured. A broad category can be saved as a draft but needs a more specific selection before ISO export."
      ))
    ),
    metadata_field_help(
      shiny::textAreaInput(ns("quality_method"), metadata_field_label("Evaluation method", "Conditional", "Required when adding any quality result."), rows = 3),
      ns("quality_method"),
      "Describe how the test was performed: the rule, reference data, sample size and calculation where relevant. For example, \"Count blank species_name values in all 250 records\". Required for every result."
    ),
    metadata_field_help(
      shiny::textInput(ns("quality_date"), "Evaluation date or recorded timestamp",
        placeholder = "2026, 2026-10, 2026-10-19, or 2026-10-19T14:30:00+02:00"),
      ns("quality_date"),
      "Record when the check was performed, using only the precision you know: year, year-month, full date, or a recorded timestamp. Do not invent a time or timezone."
    ),
    shiny::fluidRow(
      shiny::column(4, metadata_field_help(
          shiny::selectInput(ns("quality_result_type"), metadata_field_label("Result type", "Conditional", "Required when adding a result; controls which result fields are needed."),
            choices = c("Quantitative" = "quantitative", "Conformance" = "conformance", "Descriptive" = "descriptive")),
          ns("quality_result_type"),
          "Choose Quantitative for a measured number, Conformance for a pass/fail check against a specification, or Descriptive for a written finding."
      )),
      shiny::column(4, metadata_field_help(
          shiny::textInput(ns("quality_result_value"), metadata_field_label("Result value", "Conditional", "Required for a Quantitative result.")),
          ns("quality_result_value"),
          "For a quantitative result, enter the measured value, for example 5 for five missing records or 98 for 98 percent. Put its unit in the next field."
      )),
      shiny::column(4, metadata_field_help(
          shiny::textInput(ns("quality_result_unit"), "Unit"),
          ns("quality_result_unit"),
          "State how to interpret the value, for example records, %, or m. Use the unit from the test method."
      ))
    ),
    metadata_field_help(
      shiny::textInput(ns("quality_specification"), metadata_field_label("Conformance specification", "Conditional", "Required for a Conformance result.")),
      ns("quality_specification"),
      "For a conformance result, name the standard or rule tested, including its version or clause where known. For example, \"Survey rules v2, clause 4: no blank species names\"."
    ),
    metadata_field_help(
      shiny::selectInput(ns("quality_pass"), metadata_field_label("Conformance outcome", "Conditional", "Required for a Conformance result; choose Pass or Fail."),
        choices = c("Not applicable / not assessed" = "", "Pass" = "1", "Fail" = "0")),
      ns("quality_pass"),
      "For a conformance result, select Pass if the specification was met or Fail if it was not. Other result types can use Not applicable / not assessed."
    ),
    metadata_field_help(
      shiny::textAreaInput(ns("quality_explanation"), metadata_field_label("Explanation", "Conditional", "Required for a Descriptive result; optional for other result types."), rows = 3),
      ns("quality_explanation"),
      "Explain the finding, limitations and what it means for users. Required for a descriptive result; useful context for numbers and pass/fail results too."
    ),
    shiny::actionButton(ns("add_quality_result"), "Add quality result"),
    shiny::actionButton(ns("clear_quality_result"), "Clear form"),
    shiny::actionButton(ns("remove_quality_result"), "Remove result")
  )
}

wave2_empty_alternate_titles <- function() data.frame(alternate_title = character(), stringsAsFactors = FALSE)
wave2_empty_online_resources <- function() data.frame(
  resource_url = character(), resource_name = character(), resource_description = character(),
  protocol = character(), function_code = character(), media_type = character(), is_primary = integer(),
  stringsAsFactors = FALSE
)
wave2_empty_formats <- function() data.frame(
  specification_title = character(), format_version = character(), media_type = character(), format_uri = character(),
  stringsAsFactors = FALSE
)
wave2_empty_quality <- function() data.frame(
  scope_code = character(), data_file_id = integer(), attribute_id = integer(), measure_identifier = character(),
  measure_name = character(), measure_type_code = character(), quality_element_code = character(), evaluation_method = character(), evaluation_date = character(),
  result_type_code = character(), result_value = character(), result_unit = character(),
  conformance_specification = character(), explanation = character(), pass = integer(), stringsAsFactors = FALSE
)
wave2_empty_temporal <- function() data.frame(
  extent_label = character(), start_value = character(), end_value = character(), start_precision = character(),
  end_precision = character(), start_indeterminate = character(), end_indeterminate = character(),
  description = character(), stringsAsFactors = FALSE
)

move_wave2_row <- function(rows, selected, direction) {
  selected <- suppressWarnings(as.integer(selected[1]))
  if (length(selected) == 0L || is.na(selected) || nrow(rows) < 2L) return(rows)
  target <- selected + if (direction == "up") -1L else 1L
  if (target < 1L || target > nrow(rows)) return(rows)
  rows[c(selected, target), ] <- rows[c(target, selected), ]
  rownames(rows) <- NULL
  rows
}

mod_iso_wave2_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    alternate_titles <- shiny::reactiveVal(wave2_empty_alternate_titles())
    online_resources <- shiny::reactiveVal(wave2_empty_online_resources())
    formats <- shiny::reactiveVal(wave2_empty_formats())
    quality <- shiny::reactiveVal(wave2_empty_quality())
    temporal <- shiny::reactiveVal(wave2_empty_temporal())

    simple_table <- function(rows, columns) {
      display <- rows[, intersect(columns, names(rows)), drop = FALSE]
      names(display) <- gsub("_", " ", tools::toTitleCase(names(display)))
      datapond_datatable(display, rownames = FALSE, selection = "single", options = list(
        dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE
      ), class = "compact stripe hover")
    }
    output$alternate_titles_table <- DT::renderDT({ access_require("read"); simple_table(alternate_titles(), "alternate_title") })
    output$online_resources_table <- DT::renderDT({ access_require("read"); simple_table(online_resources(), c("resource_url", "resource_name", "function_code", "media_type", "is_primary")) })
    output$distribution_formats_table <- DT::renderDT({ access_require("read"); simple_table(formats(), c("specification_title", "format_version", "media_type")) })
    output$quality_results_table <- DT::renderDT({
      access_require("read")
      rows <- quality()
      codes <- ifelse(is.na(rows$quality_element_code) | !nzchar(rows$quality_element_code),
                      rows$measure_type_code, rows$quality_element_code)
      choices <- iso_quality_element_choices()
      rows$measure_type <- names(choices)[match(codes, unname(choices))]
      simple_table(rows, c("scope_code", "measure_identifier", "measure_type", "result_type_code", "result_value", "pass"))
    })
    output$temporal_extents_table <- DT::renderDT({ access_require("read"); simple_table(temporal(), c("extent_label", "start_value", "start_indeterminate", "end_value", "end_indeterminate")) })

    clear_alternate <- function() {
      shiny::updateTextInput(session, "alternate_title", value = "")
      DT::selectRows(DT::dataTableProxy("alternate_titles_table", session = session), NULL)
    }
    shiny::observeEvent(input$add_alternate_title, {
      access_require("edit", touch = TRUE)
      value <- wave2_optional_text(input$alternate_title)
      if (is.na(value)) return(shiny::showNotification("Enter an alternate title.", type = "error"))
      rows <- alternate_titles(); selected <- input$alternate_titles_table_rows_selected
      row <- data.frame(alternate_title = value, stringsAsFactors = FALSE)
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      alternate_titles(rows); clear_alternate()
    })
    shiny::observeEvent(input$alternate_titles_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$alternate_titles_table_rows_selected; rows <- alternate_titles()
      if (length(selected) == 1L && selected <= nrow(rows)) shiny::updateTextInput(session, "alternate_title", value = rows$alternate_title[selected])
    })
    shiny::observeEvent(input$remove_alternate_title, {
      access_require("edit", touch = TRUE)
      selected <- input$alternate_titles_table_rows_selected; rows <- alternate_titles()
      if (length(selected) == 1L && selected <= nrow(rows)) alternate_titles(rows[-selected, , drop = FALSE])
      clear_alternate()
    })

    clear_resource <- function() {
      for (field in c("resource_url", "resource_name", "resource_protocol", "resource_media_type")) shiny::updateTextInput(session, field, value = "")
      shiny::updateTextAreaInput(session, "resource_description", value = "")
      shiny::updateSelectInput(session, "resource_function", selected = "download")
      shiny::updateCheckboxInput(session, "resource_primary", value = FALSE)
      DT::selectRows(DT::dataTableProxy("online_resources_table", session = session), NULL)
    }
    shiny::observeEvent(input$online_resources_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$online_resources_table_rows_selected; rows <- online_resources()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateTextInput(session, "resource_url", value = row$resource_url)
      shiny::updateTextInput(session, "resource_name", value = row$resource_name %||% "")
      shiny::updateTextAreaInput(session, "resource_description", value = row$resource_description %||% "")
      shiny::updateTextInput(session, "resource_protocol", value = row$protocol %||% "")
      shiny::updateSelectInput(session, "resource_function", selected = row$function_code)
      shiny::updateTextInput(session, "resource_media_type", value = row$media_type %||% "")
      shiny::updateCheckboxInput(session, "resource_primary", value = row$is_primary == 1L)
    })
    shiny::observeEvent(input$add_online_resource, {
      access_require("edit", touch = TRUE)
      if (!wave2_valid_uri(input$resource_url)) return(shiny::showNotification("Enter a valid absolute URL or URI.", type = "error"))
      defaults <- wave2_online_resource_defaults(
        input$resource_url,
        input$resource_function,
        input$resource_protocol,
        input$resource_media_type
      )
      row <- data.frame(
        resource_url = trimws(input$resource_url), resource_name = input$resource_name %||% "",
        resource_description = input$resource_description %||% "", protocol = defaults$protocol,
        function_code = input$resource_function, media_type = defaults$media_type,
        is_primary = as.integer(isTRUE(input$resource_primary)), stringsAsFactors = FALSE
      )
      rows <- online_resources(); selected <- input$online_resources_table_rows_selected
      if (row$is_primary == 1L && nrow(rows) > 0L) rows$is_primary <- 0L
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      if (nrow(rows) == 1L) rows$is_primary[1] <- 1L
      online_resources(rows); clear_resource()
    })
    shiny::observeEvent(input$clear_online_resource, { access_require("edit", touch = TRUE); clear_resource() })
    shiny::observeEvent(input$remove_online_resource, {
      access_require("edit", touch = TRUE)
      selected <- input$online_resources_table_rows_selected; rows <- online_resources()
      if (length(selected) == 1L && selected <= nrow(rows)) rows <- rows[-selected, , drop = FALSE]
      if (nrow(rows) > 0L && !any(rows$is_primary == 1L)) rows$is_primary[1] <- 1L
      online_resources(rows); clear_resource()
    })
    shiny::observeEvent(input$move_resource_up, { access_require("edit", touch = TRUE); online_resources(move_wave2_row(online_resources(), input$online_resources_table_rows_selected, "up")) })
    shiny::observeEvent(input$move_resource_down, { access_require("edit", touch = TRUE); online_resources(move_wave2_row(online_resources(), input$online_resources_table_rows_selected, "down")) })

    clear_format <- function() {
      for (field in c("format_title", "format_version", "format_media_type", "format_uri")) shiny::updateTextInput(session, field, value = "")
      DT::selectRows(DT::dataTableProxy("distribution_formats_table", session = session), NULL)
    }
    shiny::observeEvent(input$distribution_formats_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$distribution_formats_table_rows_selected; rows <- formats()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateTextInput(session, "format_title", value = row$specification_title)
      shiny::updateTextInput(session, "format_version", value = row$format_version %||% "")
      shiny::updateTextInput(session, "format_media_type", value = row$media_type %||% "")
      shiny::updateTextInput(session, "format_uri", value = row$format_uri %||% "")
    })
    shiny::observeEvent(input$add_distribution_format, {
      access_require("edit", touch = TRUE)
      title <- wave2_optional_text(input$format_title)
      if (is.na(title)) return(shiny::showNotification("Enter a format specification title.", type = "error"))
      if (!is.na(wave2_optional_text(input$format_uri)) && !wave2_valid_uri(input$format_uri)) return(shiny::showNotification("Format URI must be absolute.", type = "error"))
      row <- data.frame(specification_title = title, format_version = input$format_version %||% "",
                        media_type = input$format_media_type %||% "", format_uri = input$format_uri %||% "", stringsAsFactors = FALSE)
      rows <- formats(); selected <- input$distribution_formats_table_rows_selected
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      formats(rows); clear_format()
    })
    shiny::observeEvent(input$clear_distribution_format, { access_require("edit", touch = TRUE); clear_format() })
    shiny::observeEvent(input$remove_distribution_format, {
      access_require("edit", touch = TRUE)
      selected <- input$distribution_formats_table_rows_selected; rows <- formats()
      if (length(selected) == 1L && selected <= nrow(rows)) formats(rows[-selected, , drop = FALSE])
      clear_format()
    })

    clear_temporal <- function() {
      shiny::updateTextInput(session, "temporal_label", value = "")
      shiny::updateTextInput(session, "temporal_start", value = "")
      shiny::updateTextInput(session, "temporal_end", value = "")
      shiny::updateSelectInput(session, "temporal_start_precision", selected = "day")
      shiny::updateSelectInput(session, "temporal_end_precision", selected = "day")
      shiny::updateSelectInput(session, "temporal_start_indeterminate", selected = "")
      shiny::updateSelectInput(session, "temporal_end_indeterminate", selected = "")
      shiny::updateTextAreaInput(session, "temporal_description", value = "")
      DT::selectRows(DT::dataTableProxy("temporal_extents_table", session = session), NULL)
    }
    shiny::observeEvent(input$temporal_extents_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$temporal_extents_table_rows_selected; rows <- temporal()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateTextInput(session, "temporal_label", value = row$extent_label %||% "")
      shiny::updateTextInput(session, "temporal_start", value = row$start_value %||% "")
      shiny::updateTextInput(session, "temporal_end", value = row$end_value %||% "")
      shiny::updateSelectInput(session, "temporal_start_precision", selected = row$start_precision)
      shiny::updateSelectInput(session, "temporal_end_precision", selected = row$end_precision)
      shiny::updateSelectInput(session, "temporal_start_indeterminate", selected = row$start_indeterminate %||% "")
      shiny::updateSelectInput(session, "temporal_end_indeterminate", selected = row$end_indeterminate %||% "")
      shiny::updateTextAreaInput(session, "temporal_description", value = row$description %||% "")
    })
    shiny::observeEvent(input$add_temporal_extent, {
      access_require("edit", touch = TRUE)
      checked <- tryCatch({
        wave2_validate_temporal_value(input$temporal_start, input$temporal_start_precision, input$temporal_start_indeterminate, "start")
        wave2_validate_temporal_value(input$temporal_end, input$temporal_end_precision, input$temporal_end_indeterminate, "end")
        TRUE
      }, error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); FALSE })
      if (!checked) return()
      row <- data.frame(extent_label = input$temporal_label %||% "", start_value = input$temporal_start %||% "",
                        end_value = input$temporal_end %||% "", start_precision = input$temporal_start_precision,
                        end_precision = input$temporal_end_precision, start_indeterminate = input$temporal_start_indeterminate %||% "",
                        end_indeterminate = input$temporal_end_indeterminate %||% "", description = input$temporal_description %||% "",
                        stringsAsFactors = FALSE)
      rows <- temporal(); selected <- input$temporal_extents_table_rows_selected
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      temporal(rows); clear_temporal()
    })
    shiny::observeEvent(input$clear_temporal_extent, { access_require("edit", touch = TRUE); clear_temporal() })
    shiny::observeEvent(input$remove_temporal_extent, {
      access_require("edit", touch = TRUE)
      selected <- input$temporal_extents_table_rows_selected; rows <- temporal()
      if (length(selected) == 1L && selected <= nrow(rows)) temporal(rows[-selected, , drop = FALSE])
      clear_temporal()
    })
    shiny::observeEvent(input$move_temporal_up, { access_require("edit", touch = TRUE); temporal(move_wave2_row(temporal(), input$temporal_extents_table_rows_selected, "up")) })
    shiny::observeEvent(input$move_temporal_down, { access_require("edit", touch = TRUE); temporal(move_wave2_row(temporal(), input$temporal_extents_table_rows_selected, "down")) })

    refresh_quality_choices <- function() {
      dataset_id <- suppressWarnings(as.integer(state$current_dataset_id[1]))
      if (length(dataset_id) == 0L || is.na(dataset_id)) {
        shiny::updateSelectInput(session, "quality_data_file", choices = c("Dataset-wide" = ""))
        shiny::updateSelectInput(session, "quality_attribute", choices = c("No attribute" = ""))
        return()
      }
      conn <- get_conn(state$data_pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
      files <- DBI::dbGetQuery(conn, "SELECT id, COALESCE(NULLIF(file_name, ''), data_file_rel_path_full) AS label FROM data_file_tbl WHERE dataset_id = ? ORDER BY id", params = list(dataset_id))
      attributes <- DBI::dbGetQuery(conn, "SELECT id, data_field_name AS label FROM dataset_attribute_tbl WHERE dataset_id = ? ORDER BY id", params = list(dataset_id))
      shiny::updateSelectInput(session, "quality_data_file", choices = c("Dataset-wide" = "", stats::setNames(as.character(files$id), files$label)))
      shiny::updateSelectInput(session, "quality_attribute", choices = c("No attribute" = "", stats::setNames(as.character(attributes$id), attributes$label)))
    }
    shiny::observe({ access_require("read");  state$refresh_datasets; state$current_dataset_id; refresh_quality_choices() })
    shiny::observeEvent(input$apply_quality_template, {
      access_require("edit", touch = TRUE)
      template <- iso_quality_templates(); row <- template[template$template_code == input$quality_template, , drop = FALSE]
      if (nrow(row) == 0L) return()
      shiny::updateTextInput(session, "quality_measure_identifier", value = row$measure_identifier)
      shiny::updateTextInput(session, "quality_measure_name", value = row$measure_name)
      shiny::updateSelectInput(session, "quality_measure_type", selected = row$measure_type_code)
    })
    clear_quality <- function() {
      shiny::updateSelectInput(session, "quality_scope", selected = "dataset")
      shiny::updateSelectInput(session, "quality_data_file", selected = "")
      shiny::updateSelectInput(session, "quality_attribute", selected = "")
      for (field in c("quality_measure_identifier", "quality_measure_name", "quality_result_value", "quality_result_unit", "quality_specification")) shiny::updateTextInput(session, field, value = "")
      shiny::updateTextAreaInput(session, "quality_method", value = "")
      shiny::updateTextAreaInput(session, "quality_explanation", value = "")
      shiny::updateTextInput(session, "quality_date", value = "")
      shiny::updateSelectInput(session, "quality_result_type", selected = "quantitative")
      shiny::updateSelectInput(session, "quality_pass", selected = "")
      DT::selectRows(DT::dataTableProxy("quality_results_table", session = session), NULL)
    }
    shiny::observeEvent(input$quality_results_table_rows_selected, {
      access_require("read", touch = TRUE)
      selected <- input$quality_results_table_rows_selected; rows <- quality()
      if (length(selected) != 1L || selected > nrow(rows)) return()
      row <- rows[selected, ]
      shiny::updateSelectInput(session, "quality_scope", selected = row$scope_code)
      shiny::updateSelectInput(session, "quality_data_file", selected = row$data_file_id %||% "")
      shiny::updateSelectInput(session, "quality_attribute", selected = row$attribute_id %||% "")
      shiny::updateTextInput(session, "quality_measure_identifier", value = row$measure_identifier)
      shiny::updateTextInput(session, "quality_measure_name", value = row$measure_name %||% "")
      shiny::updateSelectInput(session, "quality_measure_type", selected = if (non_empty_scalar(row$quality_element_code)) row$quality_element_code else row$measure_type_code)
      shiny::updateTextAreaInput(session, "quality_method", value = row$evaluation_method)
      shiny::updateTextInput(session, "quality_date", value = if (is.na(row$evaluation_date)) "" else row$evaluation_date)
      shiny::updateSelectInput(session, "quality_result_type", selected = row$result_type_code)
      shiny::updateTextInput(session, "quality_result_value", value = row$result_value %||% "")
      shiny::updateTextInput(session, "quality_result_unit", value = row$result_unit %||% "")
      shiny::updateTextInput(session, "quality_specification", value = row$conformance_specification %||% "")
      shiny::updateTextAreaInput(session, "quality_explanation", value = row$explanation %||% "")
      shiny::updateSelectInput(session, "quality_pass", selected = if (is.na(row$pass)) "" else as.character(row$pass))
    })
    shiny::observeEvent(input$add_quality_result, {
      access_require("edit", touch = TRUE)
      date <- wave2_optional_text(input$quality_date)
      selection <- iso_quality_type_selection(input$quality_measure_type)
      row <- data.frame(
        scope_code = input$quality_scope, data_file_id = wave2_optional_integer(input$quality_data_file),
        attribute_id = wave2_optional_integer(input$quality_attribute), measure_identifier = input$quality_measure_identifier %||% "",
        measure_name = input$quality_measure_name %||% "", measure_type_code = selection$measure_type_code,
        quality_element_code = selection$quality_element_code,
        evaluation_method = input$quality_method %||% "", evaluation_date = date,
        result_type_code = input$quality_result_type, result_value = input$quality_result_value %||% "",
        result_unit = input$quality_result_unit %||% "", conformance_specification = input$quality_specification %||% "",
        explanation = input$quality_explanation %||% "", pass = wave2_optional_integer(input$quality_pass), stringsAsFactors = FALSE
      )
      checked <- tryCatch({
        # Repository validation is mirrored here without creating a database row.
        if (!is.na(date)) iso_export_date(date, "Evaluation date")
        if (is.na(wave2_optional_text(row$measure_identifier)) || is.na(wave2_optional_text(row$evaluation_method))) stop("Enter a measure identifier and evaluation method.")
        if (row$scope_code == "attribute" && is.na(row$attribute_id)) stop("Select an attribute for attribute scope.")
        if (row$result_type_code == "quantitative" && is.na(wave2_optional_text(row$result_value))) stop("Enter a quantitative result value.")
        if (row$result_type_code == "conformance" && (is.na(wave2_optional_text(row$conformance_specification)) || is.na(row$pass))) stop("Enter a specification and pass/fail outcome.")
        if (row$result_type_code == "descriptive" && is.na(wave2_optional_text(row$explanation))) stop("Enter a descriptive explanation.")
        TRUE
      }, error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); FALSE })
      if (!checked) return()
      rows <- quality(); selected <- input$quality_results_table_rows_selected
      if (length(selected) == 1L && selected <= nrow(rows)) rows[selected, ] <- row else rows <- rbind(rows, row)
      quality(rows); clear_quality()
    })
    shiny::observeEvent(input$clear_quality_result, { access_require("edit", touch = TRUE); clear_quality() })
    shiny::observeEvent(input$remove_quality_result, {
      access_require("edit", touch = TRUE)
      selected <- input$quality_results_table_rows_selected; rows <- quality()
      if (length(selected) == 1L && selected <= nrow(rows)) quality(rows[-selected, , drop = FALSE])
      clear_quality()
    })

    clear <- function() {
      alternate_titles(wave2_empty_alternate_titles()); online_resources(wave2_empty_online_resources())
      formats(wave2_empty_formats()); quality(wave2_empty_quality()); temporal(wave2_empty_temporal())
      clear_alternate(); clear_resource(); clear_format(); clear_temporal(); clear_quality()
      invisible(TRUE)
    }
    load <- function(dataset_id) {
      if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) return(clear())
      subset_fields <- function(rows, fields, empty) {
        if (nrow(rows) == 0L) return(empty())
        rows[, fields, drop = FALSE]
      }
      alternate_titles(subset_fields(get_dataset_alternate_titles(dataset_id, state$data_pond), "alternate_title", wave2_empty_alternate_titles))
      online_resources(subset_fields(get_dataset_online_resources(dataset_id, state$data_pond), names(wave2_empty_online_resources()), wave2_empty_online_resources))
      formats(subset_fields(get_dataset_distribution_formats(dataset_id, state$data_pond), names(wave2_empty_formats()), wave2_empty_formats))
      quality(subset_fields(get_dataset_quality_results(dataset_id, state$data_pond), names(wave2_empty_quality()), wave2_empty_quality))
      temporal(subset_fields(get_dataset_temporal_extents(dataset_id, state$data_pond), names(wave2_empty_temporal()), wave2_empty_temporal))
      clear_alternate(); clear_resource(); clear_format(); clear_temporal(); clear_quality(); refresh_quality_choices()
      invisible(TRUE)
    }
    compatibility_temporal_values <- shiny::reactive({
      rows <- temporal()
      list(
        temporal_coverage_start_date = if (nrow(rows) == 0L) NA_character_ else wave2_optional_text(rows$start_value[1]),
        temporal_coverage_end_date = if (nrow(rows) == 0L) NA_character_ else wave2_optional_text(rows$end_value[1])
      )
    })
    session$onFlushed(function() clear(), once = TRUE)
    list(
      alternate_titles = alternate_titles, online_resources = online_resources,
      distribution_formats = formats, quality_results = quality, temporal_extents = temporal,
      compatibility_temporal_values = compatibility_temporal_values,
      clear = clear, load = load
    )
  })
}
