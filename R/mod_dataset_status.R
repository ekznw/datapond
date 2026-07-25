# mod_dataset_status.R

dataset_status_scalar_text <- function(
  x,
  default = ""
) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    is.na(x[1]) ||
    !nzchar(
      trimws(
        as.character(
          x[1]
        )
      )
    )
  ) {
    return(default)
  }

  trimws(
    as.character(
      x[1]
    )
  )
}


dataset_status_summary_card <- function(
  label,
  value,
  background,
  border
) {
  shiny::tags$div(
    style = paste(
      "padding:10px 12px;",
      paste0(
        "background:",
        background,
        ";"
      ),
      paste0(
        "border:1px solid ",
        border,
        ";"
      ),
      "margin-bottom:8px;",
      "min-height:66px;"
    ),

    shiny::tags$div(
      style = paste(
        "font-size:12px;",
        "color:#555;",
        "margin-bottom:3px;"
      ),

      label
    ),

    shiny::tags$div(
      style = paste(
        "font-size:22px;",
        "font-weight:600;"
      ),

      as.character(value)
    )
  )
}


mod_dataset_status_ui <- function(id) {
  ns <- shiny::NS(id)
  palette <- dataset_status_palette()
  borders <- dataset_status_border_palette()

  shiny::tagList(
    shiny::h3(
      "Dataset completion status"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        paste0("background:", app_earth_palette()[["parchment"]], ";"),
        paste0(
          "border:1px solid ",
          app_earth_palette()[["tan"]],
          ";"
        ),
        "margin-bottom:14px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "This table summarises the metadata and supporting",
          "records currently saved for the selected dataset.",
          "Rows are colour-coded by completion status."
        )
      )
    ),

    shiny::uiOutput(
      ns("status_summary")
    ),

    shiny::h4(
      "Section status"
    ),

    DT::DTOutput(
      ns("status_table")
    ),

    shiny::br(),

    shiny::tags$div(
      style = paste(
        "padding:8px 10px;",
        paste0("background:", app_earth_palette()[["cream"]], ";"),
        paste0(
          "border:1px solid ",
          app_earth_palette()[["stone"]],
          ";"
        )
      ),

      shiny::strong(
        "Colour key: "
      ),

      shiny::tags$span(
        style = paste(
          paste0("background:", palette[["complete"]], ";"),
          paste0("border:1px solid ", borders[["complete"]], ";"),
          "padding:3px 7px;",
          "margin-right:5px;"
        ),
        "Complete"
      ),

      shiny::tags$span(
        style = paste(
          paste0("background:", palette[["partial"]], ";"),
          paste0("border:1px solid ", borders[["partial"]], ";"),
          "padding:3px 7px;",
          "margin-right:5px;"
        ),
        "Partially complete"
      ),

      shiny::tags$span(
        style = paste(
          paste0("background:", palette[["needs_review"]], ";"),
          paste0(
            "border:1px solid ",
            borders[["needs_review"]],
            ";"
          ),
          "padding:3px 7px;",
          "margin-right:5px;"
        ),
        "Needs review"
      ),

      shiny::tags$span(
        style = paste(
          paste0("background:", palette[["not_started"]], ";"),
          paste0(
            "border:1px solid ",
            borders[["not_started"]],
            ";"
          ),
          "padding:3px 7px;"
        ),
        "Not started"
      )
    )
  )
}


mod_dataset_status_server <- function(
  id,
  state,
  schema_grouped
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      status_data <- shiny::reactive({
        # Refresh when any section capable of affecting
        # completion status changes.
        state$refresh_datasets
        state$refresh_org
        state$refresh_metadata_reviews
        state$refresh_data_files_saved
        state$refresh_attributes
        state$refresh_spatial_metadata
        state$refresh_lineage
        state$refresh_supporting_docs

        dataset_id <-
          state$current_dataset_id

        base_path <-
          state$data_pond

        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0L ||
          is.na(dataset_id[1])
        ) {
          return(
            data.table::data.table(
              message =
                "Select or save a dataset to view its completion status."
            )
          )
        }

        if (
          is.null(base_path) ||
          length(base_path) == 0L ||
          is.na(base_path[1]) ||
          !nzchar(
            trimws(
              as.character(
                base_path[1]
              )
            )
          )
        ) {
          return(
            data.table::data.table(
              message =
                "No active data pond is available."
            )
          )
        }

        tryCatch(
          get_dataset_completion_status(
            dataset_id =
              as.integer(
                dataset_id[1]
              ),
            base_path =
              as.character(
                base_path[1]
              ),
            schema_grouped =
              schema_grouped
          ),

          error = function(e) {
            data.table::data.table(
              message = paste(
                "Dataset status could not be calculated:",
                conditionMessage(e)
              )
            )
          }
        )
      })


      normalised_status_data <- shiny::reactive({
        df <- status_data()

        if (
          is.null(df) ||
          nrow(as.data.frame(df)) == 0L ||
          "message" %in% names(df)
        ) {
          return(df)
        }

        dt <- data.table::as.data.table(
          data.table::copy(df)
        )

        required_columns <- list(
          section =
            NA_character_,

          status =
            "not_started",

          status_label =
            NA_character_,

          required_missing =
            NA_integer_,

          missing_fields =
            NA_character_,

          saved_records =
            0L,

          next_action =
            NA_character_
        )

        for (field_name in names(
          required_columns
        )) {
          if (!field_name %in% names(dt)) {
            dt[
              ,
              (field_name) :=
                required_columns[[field_name]]
            ]
          }
        }

        # Convert text columns explicitly using [[ ]].
        # This avoids collisions between column names such as
        # status_label and functions with the same name.
        text_columns <- c(
          "section",
          "status",
          "status_label",
          "missing_fields",
          "next_action"
        )

        for (field_name in text_columns) {
          field_values <- as.character(
            dt[[field_name]]
          )

          field_values[
            is.na(field_values)
          ] <- ""

          dt[
            ,
            (field_name) :=
              field_values
          ]
        }

        # Normalise the stored status codes.
        dt[
          ,
          status :=
            tolower(
              trimws(
                as.character(status)
              )
            )
        ]

        dt[
          is.na(status) |
            !nzchar(status),
          status :=
            "not_started"
        ]

        # Retrieve the global helper function explicitly because
        # the table also has a column called status_label.
        status_label_function <- get(
          x = "status_label",
          mode = "function",
          inherits = TRUE
        )

        missing_status_label <- (
          is.na(
            dt$status_label
          ) |
            !nzchar(
              trimws(
                dt$status_label
              )
            )
        )

        if (any(
          missing_status_label,
          na.rm = TRUE
        )) {
          generated_labels <- vapply(
            dt$status[
              missing_status_label
            ],
            status_label_function,
            character(1)
          )

          dt[
            which(
              missing_status_label
            ),
            status_label :=
              generated_labels
          ]
        }

        # The current status helper may still emit both the new
        # structured licensing row and the older legacy row.
        if (
          "Licensing and sharing" %in%
            dt$section
        ) {
          dt <- dt[
            section != "Licensing"
          ]
        }

        dt[
          ,
          required_missing :=
            suppressWarnings(
              as.integer(
                required_missing
              )
            )
        ]

        dt[
          ,
          saved_records :=
            suppressWarnings(
              as.integer(
                saved_records
              )
            )
        ]

        dt[
          is.na(saved_records),
          saved_records :=
            0L
        ]

        dt[]
      })


      status_display <- shiny::reactive({
        dt <- normalised_status_data()

        if (
          is.null(dt) ||
          nrow(as.data.frame(dt)) == 0L ||
          "message" %in% names(dt)
        ) {
          return(dt)
        }

        dt[
          ,
          .(
            status_code =
              status,

            Section =
              section,

            Status =
              status_label,

            `Missing required items` =
              data.table::fifelse(
                is.na(
                  required_missing
                ),
                "--",
                as.character(
                  required_missing
                )
              ),

            `Missing or incomplete fields` =
              data.table::fifelse(
                nzchar(
                  trimws(
                    missing_fields
                  )
                ),
                missing_fields,
                "--"
              ),

            `Saved records` =
              saved_records,

            `Recommended next action` =
              data.table::fifelse(
                nzchar(
                  trimws(
                    next_action
                  )
                ),
                next_action,
                "--"
              )
          )
        ]
      })


      output$status_summary <-
        shiny::renderUI({
          dt <- normalised_status_data()

          if (
            is.null(dt) ||
            nrow(as.data.frame(dt)) == 0L
          ) {
            return(NULL)
          }

          if ("message" %in% names(dt)) {
            return(
              shiny::tags$div(
                style = paste(
                  "padding:10px;",
                  "background:#F8F4EA;",
                  "border:1px solid #ccc;",
                  "margin-bottom:12px;"
                ),

                dt$message[1]
              )
            )
          }

          # Lifecycle is descriptive rather than a metadata
          # completion section, so exclude it from the counts.
          assessment <- dt[
            section != "Lifecycle"
          ]

          complete_count <- sum(
            assessment$status %in%
              c(
                "complete",
                "not_applicable"
              ),
            na.rm = TRUE
          )

          partial_count <- sum(
            assessment$status %in%
              c(
                "partial",
                "needs_review"
              ),
            na.rm = TRUE
          )

          not_started_count <- sum(
            assessment$status ==
              "not_started",
            na.rm = TRUE
          )

          total_count <-
            nrow(assessment)

          lifecycle_row <- dt[
            section == "Lifecycle"
          ]

          lifecycle_text <- if (
            nrow(lifecycle_row) > 0L
          ) {
            dataset_status_scalar_text(
              lifecycle_row$status_label[1],
              default = "Not recorded"
            )
          } else {
            "Not recorded"
          }

          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 3,

                dataset_status_summary_card(
                  label =
                    "Sections assessed",
                  value =
                    total_count,
                  background =
                    "#F8F4EA",
                  border =
                    "#806855"
                )
              ),

              shiny::column(
                width = 3,

                dataset_status_summary_card(
                  label =
                    "Complete",
                  value =
                    complete_count,
                  background =
                    "#D1DDC7",
                  border =
                    "#526657"
                )
              ),

              shiny::column(
                width = 3,

                dataset_status_summary_card(
                  label =
                    "Partially complete / review",
                  value =
                    partial_count,
                  background =
                    "#E9DFB5",
                  border =
                    "#C3A35B"
                )
              ),

              shiny::column(
                width = 3,

                dataset_status_summary_card(
                  label =
                    "Not started",
                  value =
                    not_started_count,
                  background =
                    "#D3CEC4",
                  border =
                    "#806855"
                )
              )
            ),

            shiny::tags$div(
              style = paste(
                "padding:8px 10px;",
                "background:#F1E9DA;",
                "border:1px solid #B99572;",
                "margin-bottom:14px;"
              ),

              shiny::strong(
                "Lifecycle status: "
              ),

              lifecycle_text
            )
          )
        })


      output$status_table <-
        DT::renderDT({
          display <- status_display()

          if (
            is.null(display) ||
            nrow(as.data.frame(display)) == 0L
          ) {
            return(
              DT::datatable(
                data.frame(
                  Message =
                    "No status records are available."
                ),
                rownames = FALSE,
                selection = "none",
                options = list(
                  dom = "t",
                  ordering = FALSE,
                  paging = FALSE,
                  searching = FALSE,
                  info = FALSE
                )
              )
            )
          }

          if ("message" %in% names(display)) {
            message_table <- data.frame(
              Message =
                as.character(
                  display$message
                ),
              stringsAsFactors = FALSE
            )

            return(
              DT::datatable(
                message_table,
                rownames = FALSE,
                selection = "none",
                escape = TRUE,
                options = list(
                  dom = "t",
                  ordering = FALSE,
                  paging = FALSE,
                  searching = FALSE,
                  info = FALSE
                )
              )
            )
          }

          display <- as.data.frame(
            display,
            stringsAsFactors = FALSE
          )

          table_widget <- DT::datatable(
            display,
            rownames = FALSE,
            selection = "none",
            filter = "top",
            escape = TRUE,
            extensions = "Buttons",

            options = list(
              pageLength = 20L,
              scrollX = TRUE,
              autoWidth = FALSE,
              dom = "Bfrtip",
              buttons = c(
                "copy",
                "csv"
              ),

              # Preserve the logical section order returned
              # by get_dataset_completion_status().
              order = list(),

              columnDefs = list(
                list(
                  visible = FALSE,
                  targets = 0L
                ),

                list(
                  width = "165px",
                  targets = 1L
                ),

                list(
                  width = "145px",
                  targets = 2L
                ),

                list(
                  width = "115px",
                  targets = 3L
                ),

                list(
                  width = "300px",
                  targets = 4L
                ),

                list(
                  width = "105px",
                  targets = 5L
                ),

                list(
                  width = "340px",
                  targets = 6L
                )
              )
            )
          )

          palette <-
            dataset_status_palette()

          table_widget <- DT::formatStyle(
            table_widget,
            columns =
              "status_code",
            target =
              "row",

            backgroundColor =
              DT::styleEqual(
                names(palette),
                unname(palette)
              ),

            verticalAlign =
              "top"
          )

          table_widget <- DT::formatStyle(
            table_widget,
            columns =
              "Status",
            fontWeight =
              "600"
          )

          table_widget <- DT::formatStyle(
            table_widget,
            columns = c(
              "Section",
              "Missing or incomplete fields",
              "Recommended next action"
            ),
            whiteSpace =
              "normal",
            wordBreak =
              "break-word",
            lineHeight =
              "1.35",
            verticalAlign =
              "top"
          )

          table_widget
        })


      invisible(
        list(
          status =
            normalised_status_data,

          display =
            status_display
        )
      )
    }
  )
}
