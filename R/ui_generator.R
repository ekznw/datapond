# One convention for entry requirements throughout the app. Keep the marker's
# text available to assistive technology as well as the visible symbol.
metadata_requirement_marker <- function(requirement) {
  requirement <- match.arg(requirement, c("Mandatory", "Conditional", "Optional"))
  if (identical(requirement, "Optional")) return(NULL)
  shiny::tagList(
    shiny::tags$span(class = paste("flow-field-requirement", tolower(requirement)),
      title = requirement, `aria-hidden` = "true",
      if (identical(requirement, "Mandatory")) "*" else "\u25c7"),
    shiny::tags$span(class = "sr-only", paste0(" (", requirement, ")"))
  )
}

metadata_field_label <- function(label, requirement = "Optional", guidance = NULL) {
  shiny::tagList(label, " ", metadata_requirement_marker(requirement),
    if (!is.null(guidance)) shiny::tags$small(
      class = "metadata-requirement-help", guidance
    ))
}

metadata_requirement_legend <- function() {
  shiny::tags$p(class = "metadata-requirement-legend",
    metadata_requirement_marker("Mandatory"), " Required for the stated action or completion. ",
    metadata_requirement_marker("Conditional"), " Required when the stated condition applies. ",
    "Unmarked entries are optional; supplied values must still be valid.")
}

get_keyword_vocab_field <- function(field_name) {
  if (grepl("^keyword_object", field_name)) return("keyword_object")
  if (grepl("^keyword_discipline", field_name)) return("keyword_discipline")
  # keyword_topic is a read-only legacy alias for the combined theme field.
  if (grepl("^keyword_topic", field_name)) return("keyword_object")
  if (grepl("^keyword_geo", field_name)) return("keyword_geo")
  return(NULL)
}

metadata_optional_details <- function(summary, ...) {
  shiny::tags$details(
    class = "datapond-optional-details",
    shiny::tags$summary(summary),
    shiny::tags$div(
      style = "padding-top:12px;",
      ...
    )
  )
}

# Keep guidance beside its control and expose the same explanation to screen readers.
metadata_field_help <- function(input, input_id, explanation) {
  help_id <- paste0(input_id, "-help")
  for (selector in c("input", "select", "textarea")) {
    input <- htmltools::tagQuery(input)$find(selector)$
      addAttrs(`aria-describedby` = help_id)$allTags()
  }
  shiny::tags$div(
    class = "metadata-guided-field",
    input,
    shiny::tags$p(id = help_id, class = "help-block metadata-field-help", explanation)
  )
}

dataset_field_uses_textarea <- function(field, type) {
  field <- if (is.null(field) || length(field) == 0L) {
    ""
  } else {
    as.character(field[1])
  }
  type <- if (is.null(type) || length(type) == 0L) {
    ""
  } else {
    as.character(type[1])
  }

  identical(type, "textarea") || identical(field, "purpose")
}

# used to select fields for dropdowns based on choices listed in the UI schema
# or a vocab (csv into db) table
build_select_input <- function(f, ns, choices_schema, get_choices_fn, label = f$label) {
  vocab_field <- get_keyword_vocab_field(f$field)
  # KEYWORD FIELDS -> selectize + multi + free text
  if (!is.null(vocab_field)) {
    choices_db <- get_choices_fn(vocab_field)
    if (is.null(choices_db)) {
      choices_db <- c()
    }
      return(
        shiny::tagList(selectizeInput(
          ns(f$field),
          label,
          choices = choices_db,
          multiple = TRUE,
          options = list(
            create = I("function(input) {
              var value = input.trim();
              if (!value || /[;\\r\\n]/.test(value)) return false;
              var key = value.toLowerCase(), options = this.options, matches = [];
              Object.keys(options).forEach(function(id) {
                var item = options[id];
                if (String(item.value).toLowerCase() === key) matches.unshift(item);
                else if (String(item.label).toLowerCase() === key) matches.push(item);
              });
              if (matches.length === 1) return matches[0];
              return {value: value, label: value + ' (Custom)'};
            }"),
            delimiter = ";",
            splitOn = I("/;/"),
            persist = TRUE,
            plugins = list("remove_button"),
            placeholder = "Select or add keywords..."
          )
        ), if (identical(vocab_field, "keyword_object")) shiny::helpText(
          "Choose suggested theme / subject keywords, or type your own and press Enter. Separate terms with semicolons; commas stay within a term.",
          "The first theme keyword is used in the folder name and citation key."
        ))
      )
  }
  # NON-KEYWORD SELECTS
  choices_db <- get_choices_fn(f$field)
  # fallback to schema options
  if (is.null(choices_db)) {
    if (!is.null(choices_schema) && length(choices_schema) > 0) {
      choices_db <- setNames(choices_schema, choices_schema)
    }
  }
  return(
    selectInput(ns(f$field), label, choices = choices_db)
  )
}

blank_initial_date_input <- function(
  input_id,
  label,
  format = "yyyy-mm-dd"
) {
  # Construct the standard Shiny date input with a valid temporary
  # value so that Shiny does not issue a coercion warning.
  control <- shiny::dateInput(
    inputId = input_id,
    label = label,
    value = Sys.Date(),
    format = format
  )

  # The Shiny JavaScript binding reads data-initial-date when the
  # control is initialised. An empty string leaves the control blank.
  htmltools::tagQuery(
    control
  )$
    find("input")$
    removeAttrs("data-initial-date")$
    addAttrs(
      `data-initial-date` = ""
    )$
    allTags()
}

generate_section <- function(dt, ns, get_choices_fn, population_visibility = FALSE) {
  dt <- dt[!is.na(field) & field != "" & !is.na(type) & !type %ilike% "hidden"]
  tagList(
    lapply(seq_len(nrow(dt)), function(i) {
      f <- dt[i, ]
      requirement <- if ("obligation" %in% names(f) && identical(as.character(f$obligation), "M")) {
        if (f$field %in% metadata_inventory_fields()) "Mandatory" else "Conditional"
      } else "Optional"
      label <- metadata_field_label(f$label, requirement,
        if (requirement == "Mandatory") "Required to create or save the dataset."
        else if (requirement == "Conditional") "Required to save in Full metadata mode.")
      # parse schema options
      choices <- NULL
      if (!is.na(f$options) && f$options != "") {
        choices <- trimws(strsplit(f$options, ";")[[1]])
      }
      input_element <- switch(f$type,
        text = {
          if (f$field == "version") {
            tagList(
              textInput(
                ns(f$field),
                label,
                value = "0.1.0",
                placeholder = "major.minor.patch"
              ),
              fluidRow(
                column(
                  width = 4,
                  sliderInput(
                    ns("version_major"),
                    "Major",
                    min = 0,
                    max = 20,
                    value = 0,
                    step = 1
                  )
                ),
                column(
                  width = 4,
                  sliderInput(
                    ns("version_minor"),
                    "Minor",
                    min = 0,
                    max = 99,
                    value = 1,
                    step = 1
                  )
                ),
                column(
                  width = 4,
                  sliderInput(
                    ns("version_patch"),
                    "Patch",
                    min = 0,
                    max = 99,
                    value = 0,
                    step = 1
                  )
                )
              )
            )
          } else if (
            f$field %in% c("title", "dataset_title")
          ) {
            textAreaInput(
              ns(f$field),
              label,
              value = "",
              rows = 3,
              width = "100%",
              resize = "vertical",
              placeholder = paste(
                "Enter the full descriptive title",
                "of the dataset."
              )
            )
          } else if (dataset_field_uses_textarea(f$field, f$type)) {
            textAreaInput(
              ns(f$field),
              label,
              value = "",
              rows = 5,
              width = "100%",
              resize = "vertical",
              placeholder = "Describe why the dataset was originally created."
            )
          } else {
            textInput(
              ns(f$field),
              label,
              width = "100%"
            )
          }
        },
        textarea = textAreaInput(
          ns(f$field),
          label,
          value = "",
          width = "100%",
          resize = "vertical"
        ),
        date = {
          field_name <- as.character(
            f$field[1]
          )

          if (
            field_name %in% c(
              "citation_date",
              "temporal_coverage_start_date",
              "temporal_coverage_end_date"
            )
          ) {
            blank_initial_date_input(
              input_id = ns(field_name),
              label = label,
              format = "yyyy-mm-dd"
            )
          } else {
            shiny::dateInput(
              inputId = ns(field_name),
              label = label,
              value = NULL,
              format = "yyyy-mm-dd"
            )
          }
        },
        select = build_select_input(f, ns, choices, get_choices_fn, label = label),
        checkbox = checkboxInput(ns(f$field), label, value = FALSE),
        textInput(ns(f$field), label)
      )
      # help text
      help_text <- NULL
      if (!is.na(f$description) && f$description != "") {
        help_text <- tags$small(
          style = "color:#666; display:block;",
          f$description
        )
      }
      field_ui <- tagList(input_element, help_text, br())
      if (isTRUE(population_visibility) && !f$field %in% metadata_inventory_fields()) {
        metadata_population_detail(field_ui)
      } else {
        field_ui
      }
    })
  )
}

get_section <- function(schema, tab, subtab) {
  if (!tab %in% names(schema)) return(NULL)
  if (!subtab %in% names(schema[[tab]])) return(NULL)
  schema[[tab]][[subtab]]
}
