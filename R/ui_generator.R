get_keyword_vocab_field <- function(field_name) {
  if (grepl("^keyword_object", field_name)) return("keyword_object")
  if (grepl("^keyword_discipline", field_name)) return("keyword_discipline")
  if (grepl("^keyword_topic", field_name)) return("keyword_topic")
  if (grepl("^keyword_geo", field_name)) return("keyword_geo")
  return(NULL)
}

# used to select fields for dropdowns based on choices listed in the schema xlsx
# or a vocab (csv into db) table
build_select_input <- function(f, ns, choices_schema, get_choices_fn) {
  vocab_field <- get_keyword_vocab_field(f$field)
  # KEYWORD FIELDS -> selectize + multi + free text
  if (!is.null(vocab_field)) {
    choices_db <- get_choices_fn(vocab_field)
    if (is.null(choices_db)) {
      choices_db <- c()
    }
      return(
        selectizeInput(
          ns(f$field),
          f$label,
          choices = choices_db,
          multiple = TRUE,
          options = list(
            create = TRUE,
            persist = TRUE,
            plugins = list("remove_button"),
            placeholder = "Select or add keywords..."
          )
        )
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
    selectInput(ns(f$field), f$label, choices = choices_db)
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
    addAttrs(
      `data-initial-date` = ""
    )$
    allTags()
}

generate_section <- function(dt, ns, get_choices_fn) {
  dt <- dt[!is.na(field) & field != "" & !is.na(type) & !type %ilike% "hidden"]
  tagList(
    lapply(seq_len(nrow(dt)), function(i) {
      f <- dt[i, ]
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
                "Version",
                value = "0.1.0",
                placeholder = "major.minor.patch"
              ),
              tags$small(
                style = "color:#666; display:block;",
                "Version is generated from the sliders using major.minor.patch, e.g. 0.1.0, 1.0.0, 1.1.0."
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
              f$label,
              value = "",
              rows = 3,
              width = "100%",
              resize = "vertical",
              placeholder = paste(
                "Enter the full descriptive title",
                "of the dataset."
              )
            )
          } else {
            textInput(
              ns(f$field),
              f$label,
              width = "100%"
            )
          }
        },
        textarea = textAreaInput(ns(f$field), f$label, value = "",
          width = "100%", resize = "vertical"
        ),
        date = {
          field_name <- as.character(
            f$field[1]
          )

          if (
            field_name %in% c(
              "temporal_coverage_start_date",
              "temporal_coverage_end_date"
            )
          ) {
            blank_initial_date_input(
              input_id = ns(field_name),
              label = f$label,
              format = "yyyy-mm-dd"
            )
          } else {
            shiny::dateInput(
              inputId = ns(field_name),
              label = f$label,
              value = NULL,
              format = "yyyy-mm-dd"
            )
          }
        },
        select = build_select_input(f, ns, choices, get_choices_fn),
        checkbox = checkboxInput(ns(f$field), f$label, value = FALSE),
        textInput(ns(f$field), f$label)
      )
      # help text
      help_text <- NULL
      if (!is.na(f$description) && f$description != "") {
        help_text <- tags$small(
          style = "color:#666; display:block;",
          f$description
        )
      }
      tagList(
        input_element,
        help_text,
        br()
      )
    })
  )
}

get_section <- function(schema, tab, subtab) {
  if (!tab %in% names(schema)) return(NULL)
  if (!subtab %in% names(schema[[tab]])) return(NULL)
  schema[[tab]][[subtab]]
}
