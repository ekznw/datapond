# mod_data_access.R
# Dataset-level distribution, access-condition and embargo metadata.
# Values are returned to the main save workflow and stored in entry_tbl.

mod_data_access_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      "Distribution and embargo"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F1E9DA;",
        "border:1px solid #B99572;",
        "margin-bottom:12px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "The Sharing rights tab records the authoritative",
          "access classification and licence. Use this section",
          "to record where the dataset is distributed and any",
          "additional operational constraints or embargo."
        )
      )
    ),

    shiny::h4(
      "Distribution"
    ),

    shiny::textInput(
      ns("data_distribution_url"),
      "Distribution URL",
      value = "",
      placeholder = paste(
      "Enter a stable catalogue, repository, download",
      "or dataset request URL when available."
    )),

    shiny::selectizeInput(
      ns("data_distributor_organisation_id"),
      "Primary distribution organisation",
      choices = NULL,
      selected = character(),
      multiple = FALSE,
      options = list(
        placeholder =
          "Select an organisation"
      )
    ),

    shiny::textAreaInput(
      ns("data_distributor_other"),
      "Other distribution service or organisation",
      placeholder = paste(
        "Describe another catalogue, website, organisation",
        "or service from which the dataset is available."
      ),
      rows = 3
    ),

    shiny::hr(),

    shiny::h4(
      "Additional access conditions"
    ),

    shiny::textAreaInput(
      ns("access_constraints"),
      "Additional access constraints",
      placeholder = paste(
        "Record practical access restrictions not already",
        "represented by the structured sharing assessment."
      ),
      rows = 4
    ),

    shiny::textAreaInput(
      ns("use_limitations"),
      "Additional use limitations",
      placeholder = paste(
        "Record operational caveats or conditions not already",
        "represented by the licence and redistribution fields."
      ),
      rows = 4
    ),

    shiny::hr(),

    shiny::h4(
      "Embargo"
    ),

    shiny::selectInput(
      ns("embargo_status"),
      "Is the dataset currently embargoed?",
      choices = c(
        "No" = "No",
        "Yes" = "Yes"
      ),
      selected = "No"
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Yes'",
        ns("embargo_status")
      ),

      shiny::textAreaInput(
        ns("embargo_reason"),
        "Embargo reason",
        placeholder = paste(
          "For example: pending publication, contractual",
          "restriction or review of sensitive information."
        ),
        rows = 3
      ),

      shiny::dateInput(
        ns("embargo_end_date"),
        "Embargo end date",
        value = NULL,
        format = "yyyy-mm-dd"
      )
    )
  )
}


mod_data_access_server <- function(id, state) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # helper functions ----
      selected_distributor <- reactive({
        selected_id <- suppressWarnings(
          as.integer(
            input$data_distributor_organisation_id
          )
        )

        if (is.na(selected_id)) {
          return(
            list(
              id = NA_integer_,
              name = ""
            )
          )
        }

        organisations <- organisation_records()

        if (
          is.null(organisations) ||
          nrow(organisations) == 0L ||
          !"id" %in% names(organisations)
        ) {
          return(
            list(
              id = selected_id,
              name = ""
            )
          )
        }

        selected <- organisations[
          as.integer(organisations$id) ==
            selected_id,
          ,
          drop = FALSE
        ]

        if (nrow(selected) == 0L) {
          return(
            list(
              id = selected_id,
              name = ""
            )
          )
        }

        full_name <- if (
          "organisation_name_full" %in%
            names(selected)
        ) {
          data_access_scalar(
            selected$organisation_name_full,
            default = ""
          )
        } else {
          ""
        }

        short_name <- if (
          "organisation_name_short" %in%
            names(selected)
        ) {
          data_access_scalar(
            selected$organisation_name_short,
            default = ""
          )
        } else {
          ""
        }

        display_name <- if (
          nzchar(full_name)
        ) {
          full_name
        } else {
          short_name
        }

        list(
          id = selected_id,
          name = display_name
        )
      })

      safe_embargo_date <- function(x) {
        if (is.null(x) || length(x) == 0L || all(is.na(x))) return(NULL)
        x <- trimws(as.character(x[1]))
        if (!nzchar(x)) return(NULL)
        parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
        if (is.na(parsed)) return(NULL)
        parsed
      }

      # Ensure the columns exist whenever the active data pond changes.
      shiny::observe({
        base_path <- state$data_pond
        shiny::req(base_path)

        tryCatch(
          ensure_entry_data_access_columns(
            base_path
          ),
          error = function(e) {
            warning(
              "Could not ensure entry_tbl data-access columns: ",
              conditionMessage(e)
            )
          }
        )
      })

      organisation_records <- reactive({
        # Explicitly refresh when organisations are added,
        # edited or reseeded.
        state$refresh_org

        base_path <- state$data_pond

        if (
          is.null(base_path) ||
          length(base_path) == 0L ||
          is.na(base_path[1]) ||
          !nzchar(
            trimws(
              as.character(base_path[1])
            )
          )
        ) {
          return(
            data.frame()
          )
        }

        organisations <- get_organisations(
          base_path
        )

        if (
          is.null(organisations) ||
          nrow(organisations) == 0L
        ) {
          return(
            data.frame()
          )
        }

        organisations <- as.data.frame(
          organisations,
          stringsAsFactors = FALSE
        )

        organisations
      })


      organisation_choices <- reactive({
        organisations <- organisation_records()

        if (
          is.null(organisations) ||
          nrow(organisations) == 0L
        ) {
          return(
            stats::setNames(
              character(),
              character()
            )
          )
        }

        full_name <- if (
          "organisation_name_full" %in%
            names(organisations)
        ) {
          organisations$organisation_name_full
        } else {
          rep(
            "",
            nrow(organisations)
          )
        }

        short_name <- if (
          "organisation_name_short" %in%
            names(organisations)
        ) {
          organisations$organisation_name_short
        } else {
          rep(
            "",
            nrow(organisations)
          )
        }

        code <- if (
          "organisation_code" %in%
            names(organisations)
        ) {
          organisations$organisation_code
        } else {
          rep(
            "",
            nrow(organisations)
          )
        }

        full_name <- ifelse(
          is.na(full_name),
          "",
          trimws(
            as.character(full_name)
          )
        )

        short_name <- ifelse(
          is.na(short_name),
          "",
          trimws(
            as.character(short_name)
          )
        )

        code <- ifelse(
          is.na(code),
          "",
          trimws(
            as.character(code)
          )
        )

        display_name <- ifelse(
          nzchar(short_name),
          short_name,
          full_name
        )

        has_code <- nzchar(code)

        display_name[has_code] <- paste0(
          display_name[has_code],
          " [",
          code[has_code],
          "]"
        )

        missing_label <- !nzchar(
          display_name
        )

        display_name[missing_label] <- paste0(
          "Organisation ",
          organisations$id[missing_label]
        )

        stats::setNames(
          as.character(
            organisations$id
          ),
          display_name
        )
      })

      values <- reactive({
        distributor <- selected_distributor()

        embargo_status <- normalise_embargo_status(
          input$embargo_status,
          default = "No"
        )

        embargo_reason <- data_access_scalar(
          input$embargo_reason, default = ""
        )

        embargo_active <- identical(embargo_status, "Yes")

        embargo_end_date <- if (embargo_active) {
          selected_date <- safe_embargo_date(
            input$embargo_end_date
          )

          if (is.null(selected_date)) {
            NA_character_
          } else {
            format(
              selected_date,
              "%Y-%m-%d"
            )
          }
        } else {
          NA_character_
        }

        if (!embargo_active) {
          embargo_reason <- NA_character_
        }

        list(
          data_distribution_url =
            data_access_scalar(
              input$data_distribution_url,
              default = ""
            ),

          data_distributor_organisation_id =
            distributor$id,

          # Human-readable snapshot for metadata reports
          # and older integrations.
          data_distributor =
            distributor$name,

          data_distributor_other =
            data_access_scalar(
              input$data_distributor_other,
              default = ""
            ),

          access_constraints =
            data_access_scalar(
              input$access_constraints,
              default = ""
            ),

          use_limitations =
            data_access_scalar(
              input$use_limitations,
              default = ""
            ),

          embargo_status =
            embargo_status,

          embargo_reason =
            embargo_reason,

          embargo_end_date =
            embargo_end_date
        )
      })

      observe({
        choices <- organisation_choices()

        current_selection <- isolate(
          input$data_distributor_organisation_id
        )

        selected <- if (
          !is.null(current_selection) &&
          length(current_selection) > 0L &&
          as.character(current_selection[1]) %in%
            unname(choices)
        ) {
          as.character(
            current_selection[1]
          )
        } else {
          character()
        }

        updateSelectizeInput(
          session,
          "data_distributor_organisation_id",
          choices = choices,
          selected = selected,
          server = FALSE
        )
      })

      shiny::observeEvent(
        input$embargo_status,
        {
          if (
            identical(
              input$embargo_status,
              "No"
            )
          ) {
            shiny::updateTextAreaInput(
              session,
              "embargo_reason",
              value = ""
            )

            shiny::updateDateInput(
              session,
              "embargo_end_date",
              value = NULL
            )
          }
        },
        ignoreInit = TRUE
      )

      validate <- function(strict = FALSE) {
        vals <- values()
        missing <- character()

        # Draft dataset records may be saved with incomplete access metadata.
        # The Dataset status tab reports those gaps. Set strict = TRUE later
        # for a publication/submission gate if required.

        distributor <- selected_distributor()

        other_distributor <- data_access_scalar(
          input$data_distributor_other,
          default = ""
        )

        has_selected_distributor <-
          !is.na(distributor$id)

        has_other_distributor <-
          nzchar(other_distributor)

        if (
          has_selected_distributor &&
          has_other_distributor
        ) {
          return(
            list(
              valid = FALSE,
              message = paste(
                "Choose a primary distribution organisation",
                "or enter an other distributor, not both."
              )
            )
          )
        }

        if (
          isTRUE(strict) &&
          !has_selected_distributor &&
          !has_other_distributor
        ) {
          return(
            list(
              valid = FALSE,
              message = paste(
                "Select a primary distribution organisation",
                "or enter an other distributor."
              )
            )
          )
        }

        if (isTRUE(strict)) {
          if (!nzchar(vals$access_constraints)) {
            missing <- c(
              missing,
              "Access constraints"
            )
          }

          if (!nzchar(vals$use_limitations)) {
            missing <- c(
              missing,
              "Use limitations"
            )
          }

          if (!vals$embargo_status %in% c("Yes", "No")) {
            missing <- c(
              missing,
              "Embargo status"
            )
          }
        }

        # An active embargo is internally inconsistent without its end date,
        # so this validation applies even during draft saves.
        if (
          identical(vals$embargo_status, "Yes") &&
          (
            is.null(vals$embargo_end_date) ||
            length(vals$embargo_end_date) == 0L ||
            is.na(vals$embargo_end_date[1]) ||
            !nzchar(trimws(as.character(vals$embargo_end_date[1])))
          )
        ) {
          missing <- c(
            missing,
            "Embargo end date"
          )
        }

        list(
          valid = length(missing) == 0L,
          missing = missing,
          message = if (length(missing) == 0L) {
            ""
          } else {
            paste0(
              "Complete the following data-access fields: ",
              paste(missing, collapse = ", "),
              "."
            )
          }
        )
      }

      clear <- function() {
        shiny::updateTextInput(
          session,
          "data_distribution_url",
          value = ""
        )

        updateSelectizeInput(
          session,
          "data_distributor_organisation_id",
          choices = organisation_choices(),
          selected = character(),
          server = FALSE
        )

        shiny::updateTextAreaInput(
          session,
          "data_distributor_other",
          value = ""
        )

        shiny::updateTextAreaInput(
          session,
          "access_constraints",
          value = ""
        )

        shiny::updateTextAreaInput(
          session,
          "use_limitations",
          value = ""
        )

        shiny::updateSelectInput(
          session,
          "embargo_status",
          selected = "No"
        )

        shiny::updateTextAreaInput(
          session,
          "embargo_reason",
          value = ""
        )

        shiny::updateDateInput(
          session,
          "embargo_end_date",
          value = NULL
        )

        invisible(TRUE)
      }

      load_record <- function(rec) {
        if (
          is.null(rec) ||
          nrow(as.data.frame(rec)) == 0L
        ) {
          clear()
          return(invisible(FALSE))
        }

        rec <- as.data.frame(
          rec,
          stringsAsFactors = FALSE
        )

        embargo_status <- normalise_embargo_status(
          data_access_record_value(
            rec,
            "embargo_status",
            default = "No"
          ),
          default = "No"
        )

        embargo_end_text <- data_access_record_value(
          rec,
          "embargo_end_date",
          default = ""
        )

        embargo_end_date <- safe_embargo_date(embargo_end_text)

        stored_distributor_id <- suppressWarnings(
          as.integer(
            data_access_record_value(
              rec,
              "data_distributor_organisation_id",
              default = NA_character_
            )
          )
        )

        stored_distributor_name <- data_access_record_value(
          rec,
          "data_distributor",
          default = ""
        )

        selected_id <- NA_integer_

        if (!is.na(stored_distributor_id)) {
          selected_id <- stored_distributor_id
        } else if (nzchar(stored_distributor_name)) {
          # Compatibility for records saved before the
          # organisation-ID selector was introduced.
          organisations <- organisation_records()

          if (
            !is.null(organisations) &&
            nrow(organisations) > 0L
          ) {
            full_names <- if (
              "organisation_name_full" %in%
                names(organisations)
            ) {
              trimws(
                as.character(
                  organisations$organisation_name_full
                )
              )
            } else {
              rep(
                "",
                nrow(organisations)
              )
            }

            short_names <- if (
              "organisation_name_short" %in%
                names(organisations)
            ) {
              trimws(
                as.character(
                  organisations$organisation_name_short
                )
              )
            } else {
              rep(
                "",
                nrow(organisations)
              )
            }

            matched <- which(
              tolower(full_names) ==
                tolower(stored_distributor_name) |
              tolower(short_names) ==
                tolower(stored_distributor_name)
            )

            if (length(matched) > 0L) {
              selected_id <- as.integer(
                organisations$id[
                  matched[1]
                ]
              )
            }
          }
        }

        updateSelectizeInput(
          session,
          "data_distributor_organisation_id",
          choices = organisation_choices(),
          selected = if (
            is.na(selected_id)
          ) {
            character()
          } else {
            as.character(selected_id)
          },
          server = FALSE
        )

        shiny::updateTextInput(
          session,
          "data_distribution_url",
          value = data_access_record_value(
            rec,
            "data_distribution_url",
            default = ""
          )
        )

        shiny::updateTextAreaInput(
          session,
          "data_distributor_other",
          value = data_access_record_value(
            rec,
            "data_distributor_other",
            default = ""
          )
        )

        shiny::updateTextAreaInput(
          session,
          "access_constraints",
          value = data_access_record_value(
            rec,
            "access_constraints",
            default = ""
          )
        )

        shiny::updateTextAreaInput(
          session,
          "use_limitations",
          value = data_access_record_value(
            rec,
            "use_limitations",
            default = ""
          )
        )

        shiny::updateSelectInput(
          session,
          "embargo_status",
          selected = embargo_status
        )

        shiny::updateTextAreaInput(
          session,
          "embargo_reason",
          value = data_access_record_value(
            rec,
            "embargo_reason",
            default = ""
          )
        )

        shiny::updateDateInput(
          session,
          "embargo_end_date",
          value = embargo_end_date
        )

        invisible(TRUE)
      }

      load <- function(dataset_id) {
        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0L ||
          is.na(dataset_id[1])
        ) {
          clear()
          return(invisible(FALSE))
        }

        rec <- get_dataset_record(
          dataset_id = as.integer(dataset_id[1]),
          base_path = state$data_pond
        )

        load_record(rec)
      }

      list(
        values = values,
        validate = validate,
        clear = clear,
        load = load,
        load_record = load_record
      )
    }
  )
}
