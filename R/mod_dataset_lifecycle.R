# mod_dataset_lifecycle.R
# Dataset lifecycle dashboard and selected-dataset lifecycle controls.
#
# This module uses only the current lifecycle helper interfaces:
#   get_dataset_lifecycle()
#   get_lifecycle_dashboard()
#   get_dataset_folder_comparison()
#   set_dataset_lifecycle()
#   rename_draft_dataset_folder()
#   move_dataset_folder()
#
# The app.R calls remain:
#   mod_dataset_lifecycle_ui("dataset_lifecycle")
#   mod_dataset_lifecycle_server(
#     "dataset_lifecycle",
#     state = state,
#     schema_grouped = schema
#   )


# Shared earthy application palette ----
# Keep this helper in one file only. If it has already been added to
# utils_dt_display.R, remove this duplicate definition from this module.
if (!exists("app_earth_palette", mode = "function")) {
  app_earth_palette <- function() {
    c(
      forest = "#526657",
      deep_forest = "#3F5146",
      sage = "#A8B49A",
      pale_sage = "#D1DDC7",
      moss = "#87966C",
      olive = "#A7A06A",
      ochre = "#C3A35B",
      pale_ochre = "#E9DFB5",
      sand = "#E4D6B8",
      clay = "#C98F78",
      terracotta = "#B8755E",
      bark = "#806855",
      tan = "#B99572",
      stone = "#D3CEC4",
      parchment = "#F1E9DA",
      cream = "#F8F4EA",
      ink = "#403B35",
      muted_ink = "#625C52"
    )
  }
}


lifecycle_display_palette <- function() {
  earth <- app_earth_palette()

  c(
    mismatch = unname(earth[["clay"]]),
    archived = unname(earth[["tan"]]),
    released = unname(earth[["sage"]]),
    ready = unname(earth[["pale_sage"]]),
    draft = unname(earth[["pale_ochre"]]),
    unknown = unname(earth[["stone"]]),
    changed = unname(earth[["sand"]]),
    unchanged = unname(earth[["cream"]])
  )
}


lifecycle_clean_text <- function(x, default = "") {
  if (
    is.null(x) ||
      length(x) == 0L ||
      all(is.na(x))
  ) {
    return(default)
  }

  value <- trimws(as.character(x[1]))

  if (
    is.na(value) ||
      !nzchar(value)
  ) {
    return(default)
  }

  value
}


lifecycle_stage_label <- function(x) {
  x <- tolower(
    lifecycle_clean_text(
      x,
      default = ""
    )
  )

  switch(
    x,
    src = "Source (src/)",
    dev = "Development (dev/)",
    arc = "Archive (arc/)",
    "Unknown location"
  )
}


lifecycle_lock_label <- function(x) {
  locked <- suppressWarnings(
    as.integer(x[1])
  )

  if (
    length(locked) == 0L ||
      is.na(locked)
  ) {
    return("Not recorded")
  }

  if (locked == 1L) {
    "Locked"
  } else {
    "Unlocked"
  }
}


lifecycle_yes_no <- function(x) {
  if (isTRUE(x %in% TRUE)) {
    "Yes"
  } else {
    "No"
  }
}


lifecycle_status_colour <- function(
  lifecycle_status,
  ready_for_archive = FALSE,
  mismatch = FALSE
) {
  palette <- lifecycle_display_palette()

  lifecycle_status <- tolower(
    lifecycle_clean_text(
      lifecycle_status,
      default = "unknown"
    )
  )

  if (isTRUE(mismatch)) {
    return(unname(palette[["mismatch"]]))
  }

  if (identical(lifecycle_status, "archived")) {
    return(unname(palette[["archived"]]))
  }

  if (identical(lifecycle_status, "released")) {
    return(unname(palette[["released"]]))
  }

  if (
    identical(lifecycle_status, "draft") &&
      isTRUE(ready_for_archive)
  ) {
    return(unname(palette[["ready"]]))
  }

  if (identical(lifecycle_status, "draft")) {
    return(unname(palette[["draft"]]))
  }

  unname(palette[["unknown"]])
}


lifecycle_summary_card <- function(
  title,
  value,
  background,
  border,
  width = 3L
) {
  shiny::column(
    width = width,

    shiny::tags$div(
      class = "lifecycle-summary-card",
      style = paste(
        "min-height:92px;",
        "padding:10px 12px;",
        paste0("background:", background, ";"),
        paste0("border:1px solid ", border, ";"),
        "border-radius:4px;",
        "margin-bottom:12px;"
      ),

      shiny::tags$div(
        class = "lifecycle-summary-label",
        style = paste(
          "font-size:12px;",
          "font-weight:600;",
          "text-transform:uppercase;",
          "letter-spacing:0.04em;",
          "margin-bottom:5px;"
        ),
        title
      ),

      shiny::tags$div(
        class = "lifecycle-summary-value",
        style = paste(
          "font-size:16px;",
          "font-weight:600;",
          "line-height:1.3;",
          "overflow-wrap:anywhere;"
        ),
        value
      )
    )
  )
}


# UI ----
mod_dataset_lifecycle_ui <- function(id, record_controls = NULL) {
  ns <- shiny::NS(id)
  earth <- app_earth_palette()

  shiny::tagList(
    shiny::h2(
      "Dataset lifecycle"
    ),
    shiny::tags$div(
      class = "lifecycle-help-cards",
      shiny::tags$section(class = "lifecycle-help-card",
        shiny::h3("Work on a draft"),
        shiny::p("Select a dataset in Pond overview, then open Dataset lifecycle. These controls apply to the loaded dataset shown below."),
        shiny::p("Save metadata in Create / edit dataset before previewing a release. Prepare and download the current metadata PDF, then review it and the dataset files.")
      ),
      shiny::tags$section(class = "lifecycle-help-card",
        shiny::h3("Freeze a version"),
        shiny::p("Preview the saved dataset, confirm your review and enter release notes. Freeze version; keep working copy creates a verified ZIP in arc/ so development can continue."),
        shiny::p("Freeze version and close development retires the working folder after verification. A frozen release is not automatically published. Restore a release to resume work as a draft.")
      ),
      shiny::tags$section(class = "lifecycle-help-card",
        shiny::h3("Manage status and folders"),
        shiny::p("Saving Draft, Released or Archived changes the recorded lifecycle status; it does not move the folder or create a frozen release. Released and archived records are locked."),
        shiny::p("Move or rename folders separately, while the dataset is an unlocked draft. Folder verification checks identity and location and applies safe same-root path repairs.")
      ),
      shiny::tags$section(class = "lifecycle-help-card",
        shiny::h3("Delete or restore a record"),
        shiny::p("Delete record (recoverable) hides the catalogue record until you restore it. To find it again, enable Show hidden datasets in Pond overview, select it and return here."),
        shiny::p("Hard-delete permanently removes metadata and linked catalogue rows and is restricted to administrators. Records with frozen releases cannot be hard-deleted. Neither action deletes dataset files.")
      )
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px 12px;",
        paste0(
          "background:",
          unname(earth[["parchment"]]),
          ";"
        ),
        paste0(
          "border:1px solid ",
          unname(earth[["tan"]]),
          ";"
        ),
        "border-radius:4px;",
        "margin-bottom:14px;"
      ),

      shiny::tags$p(
        style = "margin:0; line-height:1.45;",
        paste(
          "Review the selected dataset's lifecycle state, folder location",
          "and metadata readiness. Lifecycle status changes are explicit;",
          "folder moves and draft-folder renames are separate operations."
        )
      )
    ),

    shiny::uiOutput(
      ns("current_lifecycle_summary")
    ),

    mod_dataset_releases_ui(ns("releases")),

    shiny::tags$div(
      style = paste(
        "padding:10px 12px;",
        paste0("background:", unname(earth[["cream"]]), ";"),
        paste0("border:1px solid ", unname(earth[["stone"]]), ";"),
        "border-radius:4px;",
        "margin-bottom:14px;"
      ),

      shiny::h3(
        style = "margin:0 0 5px 0;",
        "Dataset folder verification"
      ),

      shiny::tags$small(
        style = "display:block; color:#625C52; margin-bottom:8px;",
        paste(
          "Run the same UUID-marker and lifecycle-root location check",
          "used when the pond is opened. Safe same-root path repairs are applied automatically."
        )
      ),

      shiny::tags$div(
        style = "display:flex; flex-wrap:wrap; gap:7px;",

        shiny::actionButton(
          ns("verify_selected_folder"),
          "Verify selected dataset folder",
          class = "datapond-requires-active-user"
        ),

        shiny::actionButton(
          ns("verify_all_folders"),
          "Verify all pond dataset folders",
          class = "datapond-requires-active-user"
        )
      ),

      shiny::uiOutput(
        ns("folder_verification_result")
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::h3(
          "Selected dataset"
        ),

        DT::DTOutput(
          ns("current_lifecycle_table")
        )
      ),

      shiny::column(
        width = 5,

        shiny::tags$div(
          style = paste(
            "padding:12px;",
            paste0(
              "background:",
              unname(earth[["cream"]]),
              ";"
            ),
            paste0(
              "border:1px solid ",
              unname(earth[["stone"]]),
              ";"
            ),
            "border-radius:4px;",
            "margin-bottom:12px;"
          ),

          shiny::h3(
            style = "margin-top:0;",
            "Lifecycle controls"
          ),

          shiny::selectInput(
            ns("lifecycle_status"),
            metadata_field_label("Lifecycle status", "Mandatory", "Choose the status to save; Draft is selected by default."),
            choices = c(
              "Draft" = "draft",
              "Released" = "released",
              "Archived" = "archived"
            ),
            selected = "draft"
          ),

          shiny::checkboxInput(
            ns("folder_locked"),
            "Lock the dataset folder",
            value = FALSE
          ),

          shiny::tags$small(
            style = paste(
              "display:block;",
              "color:#625C52;",
              "margin-bottom:10px;"
            ),
            paste(
              "Released and archived datasets are always saved as locked.",
              "Draft datasets may remain unlocked for controlled folder changes."
            )
          ),

          shiny::actionButton(
            ns("save_lifecycle"),
            "Save lifecycle status",
            class = "datapond-requires-active-user"
          ),

          shiny::hr(),

          shiny::selectInput(
            ns("target_stage"),
            metadata_field_label("Move unlocked draft folder to", "Conditional", "Choose a destination when moving an unlocked draft folder."),
            choices = c(
              "Source (src/)" = "src",
              "Development (dev/)" = "dev",
              "Archive (arc/)" = "arc"
            ),
            selected = "dev"
          ),

          shiny::actionButton(
            ns("move_folder"),
            "Move draft folder",
            class = "datapond-requires-active-user"
          ),

          shiny::br(),
          shiny::br(),

          shiny::actionButton(
            ns("rename_folder"),
            "Apply proposed draft-folder name",
            class = "datapond-requires-active-user"
          ),

          shiny::tags$small(
            style = paste(
              "display:block;",
              "color:#625C52;",
              "margin-top:8px;"
            ),
            paste(
              "Folder moves and renames are allowed only for an unlocked",
              "draft dataset. Existing target folders are never overwritten."
            )
          )
        )
      )
    ),

    shiny::h3(
      "Folder-name comparison"
    ),

    shiny::tags$small(
      style = paste(
        "display:block;",
        "color:#625C52;",
        "margin-bottom:8px;"
      ),
      paste(
        "The proposed name is generated from the current dataset metadata.",
        "No rename occurs until the explicit rename action is selected."
      )
    ),

    DT::DTOutput(
      ns("folder_comparison_table")
    ),

    record_controls,

    shiny::hr(),

    shiny::h3(
      "Data-pond lifecycle dashboard"
    ),

    shiny::tags$small(
      style = paste(
        "display:block;",
        "color:#625C52;",
        "margin-bottom:8px;"
      ),
      paste(
        "Earth-toned row colours indicate lifecycle and readiness.",
        "Clay-coloured rows indicate a lifecycle-status and folder-location mismatch."
      )
    ),

    DT::DTOutput(
      ns("lifecycle_dashboard")
    )
  )
}


# Server ----
mod_dataset_lifecycle_server <- function(
  id,
  state,
  schema_grouped,
  on_restored = function() NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      refresh_token <- shiny::reactiveVal(0L)
      folder_verification_result <- shiny::reactiveVal(NULL)
      mod_dataset_releases_server("releases", state, on_changed = function() {
        refresh_selected_state()
        on_restored()
      })


      valid_dataset_context <- shiny::reactive({
        dataset_id <- state$current_dataset_id
        base_path <- state$data_pond

        valid_id <- (
          !is.null(dataset_id) &&
            length(dataset_id) > 0L &&
            !is.na(dataset_id[1])
        )

        valid_path <- (
          !is.null(base_path) &&
            length(base_path) > 0L &&
            !is.na(base_path[1]) &&
            nzchar(
              trimws(
                as.character(base_path[1])
              )
            )
        )

        list(
          valid = isTRUE(valid_id && valid_path),
          dataset_id = if (valid_id) {
            as.integer(dataset_id[1])
          } else {
            NA_integer_
          },
          base_path = if (valid_path) {
            as.character(base_path[1])
          } else {
            NA_character_
          }
        )
      })


      current_lifecycle <- shiny::reactive({
        refresh_token()
        state$refresh_datasets

        context <- valid_dataset_context()

        if (!isTRUE(context$valid)) {
          return(NULL)
        }

        get_dataset_lifecycle(
          dataset_id = context$dataset_id,
          base_path = context$base_path
        )
      })


      current_stage <- shiny::reactive({
        record <- current_lifecycle()

        if (
          is.null(record) ||
            nrow(as.data.frame(record)) == 0L ||
            !"dataset_path" %in% names(record)
        ) {
          return(NA_character_)
        }

        path <- lifecycle_clean_text(
          record$dataset_path,
          default = ""
        )

        if (!nzchar(path)) {
          return(NA_character_)
        }

        dataset_lifecycle_stage(path, state$data_pond)
      })


      current_ready_for_archive <- shiny::reactive({
        context <- valid_dataset_context()

        if (!isTRUE(context$valid)) {
          return(FALSE)
        }

        isTRUE(
          tryCatch(
            is_dataset_ready_for_archive(
              dataset_id = context$dataset_id,
              base_path = context$base_path,
              schema_grouped = schema_grouped
            ),
            error = function(e) {
              FALSE
            }
          )
        )
      })


      current_mismatch <- shiny::reactive({
        record <- current_lifecycle()
        stage <- current_stage()

        if (
          is.null(record) ||
            nrow(as.data.frame(record)) == 0L
        ) {
          return(FALSE)
        }

        status <- tolower(
          lifecycle_clean_text(
            record$lifecycle_status,
            default = "draft"
          )
        )

        isTRUE(
          (
            identical(status, "archived") &&
              !identical(stage, "arc")
          ) ||
            (
              identical(status, "released") &&
                identical(stage, "arc")
            )
        )
      })


      folder_comparison <- shiny::reactive({
        refresh_token()
        state$refresh_datasets

        context <- valid_dataset_context()

        if (!isTRUE(context$valid)) {
          return(
            data.frame(
              message = "Select a saved dataset to compare folder names.",
              stringsAsFactors = FALSE
            )
          )
        }

        tryCatch(
          get_dataset_folder_comparison(
            dataset_id = context$dataset_id,
            base_path = context$base_path
          ),
          error = function(e) {
            data.frame(
              message = paste(
                "Folder comparison unavailable:",
                conditionMessage(e)
              ),
              stringsAsFactors = FALSE
            )
          }
        )
      })


      lifecycle_dashboard_data <- shiny::reactive({
        refresh_token()
        state$refresh_datasets

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
            data.table::data.table()
          )
        }

        dashboard <- tryCatch(
          get_lifecycle_dashboard(
            base_path = as.character(base_path[1]),
            schema_grouped = schema_grouped
          ),
          error = function(e) {
            data.table::data.table(
              message = paste(
                "Lifecycle dashboard unavailable:",
                conditionMessage(e)
              )
            )
          }
        )

        if (
          is.null(dashboard) ||
            nrow(as.data.frame(dashboard)) == 0L ||
            "message" %in% names(dashboard)
        ) {
          return(dashboard)
        }

        dashboard <- data.table::as.data.table(
          data.table::copy(dashboard)
        )

        datasets <- tryCatch(
          get_datasets(
            as.character(base_path[1])
          ),
          error = function(e) {
            data.frame()
          }
        )

        if (
          !is.null(datasets) &&
            nrow(as.data.frame(datasets)) > 0L &&
            all(c("id", "title") %in% names(datasets))
        ) {
          titles <- data.table::as.data.table(
            data.table::copy(datasets)
          )[
            ,
            .(
              id = suppressWarnings(
                as.integer(id)
              ),
              title = as.character(title)
            )
          ]

          dashboard[
            ,
            dashboard_order := .I
          ]

          dashboard <- merge(
            dashboard,
            titles,
            by = "id",
            all.x = TRUE,
            sort = FALSE
          )

          data.table::setorder(
            dashboard,
            dashboard_order
          )

          dashboard[
            ,
            dashboard_order := NULL
          ]
        }

        required_columns <- list(
          id = NA_integer_,
          title = NA_character_,
          uuid = NA_character_,
          citation_key = NA_character_,
          dataset_path = NA_character_,
          lifecycle_status = "draft",
          lifecycle_label = "Draft",
          folder_locked = 0L,
          ready_for_archive = FALSE,
          stage = NA_character_,
          next_action = "Complete metadata",
          lifecycle_stage_mismatch = FALSE
        )

        for (field_name in names(required_columns)) {
          if (!field_name %in% names(dashboard)) {
            dashboard[
              ,
              (field_name) :=
                required_columns[[field_name]]
            ]
          }
        }

        dashboard[
          ,
          lifecycle_status := tolower(
            trimws(
              as.character(lifecycle_status)
            )
          )
        ]

        dashboard[
          is.na(lifecycle_status) |
            !nzchar(lifecycle_status),
          lifecycle_status := "draft"
        ]

        dashboard[
          ,
          stage := tolower(
            trimws(
              as.character(stage)
            )
          )
        ]

        dashboard[
          ,
          ready_for_archive :=
            ready_for_archive %in% TRUE
        ]

        dashboard[
          ,
          lifecycle_stage_mismatch :=
            lifecycle_stage_mismatch %in% TRUE
        ]

        dashboard[
          ,
          row_class := data.table::fcase(
            lifecycle_stage_mismatch,
            "mismatch",

            lifecycle_status == "archived",
            "archived",

            lifecycle_status == "released",
            "released",

            lifecycle_status == "draft" &
              ready_for_archive,
            "ready",

            lifecycle_status == "draft",
            "draft",

            default = "unknown"
          )
        ]

        current_id <- state$current_dataset_id

        current_id <- if (
          is.null(current_id) ||
            length(current_id) == 0L ||
            is.na(current_id[1])
        ) {
          NA_integer_
        } else {
          suppressWarnings(
            as.integer(current_id[1])
          )
        }

        dashboard[
          ,
          current_record := data.table::fifelse(
            !is.na(current_id) &
              as.integer(id) == current_id,
            "yes",
            "no"
          )
        ]

        dashboard[
          ,
          title_display := data.table::fcase(
            !is.na(title) &
              nzchar(trimws(title)),
            as.character(title),

            !is.na(citation_key) &
              nzchar(trimws(citation_key)),
            as.character(citation_key),

            default = paste(
              "Dataset",
              id
            )
          )
        ]

        dashboard[
          ,
          stage_display := vapply(
            stage,
            lifecycle_stage_label,
            character(1)
          )
        ]

        dashboard[
          ,
          lock_display := data.table::fifelse(
            suppressWarnings(
              as.integer(folder_locked)
            ) == 1L,
            "Locked",
            "Unlocked"
          )
        ]

        dashboard[
          ,
          readiness_display := data.table::fifelse(
            ready_for_archive,
            "Ready",
            "Metadata incomplete"
          )
        ]

        dashboard[
          ,
          mismatch_display := data.table::fifelse(
            lifecycle_stage_mismatch,
            "Yes",
            "No"
          )
        ]

        dashboard[]
      })


      refresh_selected_state <- function() {
        context <- shiny::isolate(
          valid_dataset_context()
        )

        if (!isTRUE(context$valid)) {
          return(invisible(FALSE))
        }

        record <- get_dataset_lifecycle(
          dataset_id = context$dataset_id,
          base_path = context$base_path
        )

        if (
          is.null(record) ||
            nrow(as.data.frame(record)) == 0L
        ) {
          return(invisible(FALSE))
        }

        if ("dataset_path" %in% names(record)) {
          state$dataset_path <- lifecycle_clean_text(
            record$dataset_path,
            default = NULL
          )

          state$current_dataset_folder <-
            state$dataset_path
        }

        if ("data_path" %in% names(record)) {
          state$data_path <- lifecycle_clean_text(
            record$data_path,
            default = NULL
          )
        }

        if ("uuid" %in% names(record)) {
          state$current_dataset_uuid <- lifecycle_clean_text(
            record$uuid,
            default = NULL
          )
        }

        if ("citation_key" %in% names(record)) {
          state$current_citation_key <- lifecycle_clean_text(
            record$citation_key,
            default = NULL
          )
        }

        if ("folder_key" %in% names(record)) {
          state$current_folder_key <- lifecycle_clean_text(
            record$folder_key,
            default = NULL
          )
        }

        refresh_token(
          refresh_token() + 1L
        )

        state$refresh_datasets <-
          state$refresh_datasets + 1L

        invisible(TRUE)
      }


      folder_location_status_label <- function(status) {
        labels <- c(
          ok = "Location verified",
          repaired_within_lifecycle_root = "Path repaired within lifecycle root",
          missing = "Dataset folder not found",
          missing_uuid = "Dataset UUID is missing",
          duplicate_uuid = "Duplicate dataset UUID",
          uuid_marker_conflict = "Folder and marker UUID conflict",
          data_pond_marker_conflict = "Data-pond marker conflict",
          stored_path_conflict = "Stored path conflicts with detected folder",
          lifecycle_location_conflict = "Detected folder is in another lifecycle root"
        )

        status <- lifecycle_clean_text(
          status,
          default = "unknown"
        )

        label <- unname(labels[status])

        if (length(label) == 0L || is.na(label)) {
          status
        } else {
          label
        }
      }


      output$folder_verification_result <- shiny::renderUI({
        access_require("read")
        result <- folder_verification_result()

        if (is.null(result)) {
          return(NULL)
        }

        earth <- app_earth_palette()

        shiny::tags$div(
          style = paste(
            "margin-top:9px;",
            "padding:7px 9px;",
            paste0(
              "background:",
              if (isTRUE(result$ok)) {
                unname(earth[["pale_sage"]])
              } else {
                unname(earth[["sand"]])
              },
              ";"
            ),
            paste0("border:1px solid ", unname(earth[["tan"]]), ";"),
            "font-size:12px;"
          ),
          result$message
        )
      })


      shiny::observeEvent(
        input$verify_selected_folder,
        {
          access_require("edit", touch = TRUE)
          if (!require_active_user_for_write(
            state,
            "verifying the selected dataset folder"
          )) {
            return()
          }

          context <- valid_dataset_context()

          if (!isTRUE(context$valid)) {
            shiny::showNotification(
              "Select a saved dataset before verifying its folder.",
              type = "warning"
            )
            return()
          }

          tryCatch(
            {
              report <- reconcile_dataset_locations(
                base_path = context$base_path,
                dataset_ids = context$dataset_id
              )

              if (nrow(report) != 1L) {
                stop("No folder-verification result was returned.")
              }

              status <- report$location_status[1]
              ok <- status %in% c(
                "ok",
                "repaired_within_lifecycle_root"
              )

              folder_verification_result(list(
                ok = ok,
                message = paste(
                  "Selected dataset:",
                  folder_location_status_label(status)
                )
              ))

              refresh_selected_state()

              shiny::showNotification(
                folder_location_status_label(status),
                type = if (isTRUE(ok)) "message" else "warning",
                duration = 7
              )
            },
            error = function(e) {
              folder_verification_result(list(
                ok = FALSE,
                message = paste(
                  "Verification failed:",
                  conditionMessage(e)
                )
              ))
              shiny::showNotification(
                conditionMessage(e),
                type = "error",
                duration = 8
              )
            }
          )
        }
      )


      shiny::observeEvent(
        input$verify_all_folders,
        {
          access_require("edit", touch = TRUE)
          if (!require_active_user_for_write(
            state,
            "verifying all pond dataset folders"
          )) {
            return()
          }

          base_path <- lifecycle_clean_text(
            state$data_pond,
            default = ""
          )

          if (!nzchar(base_path)) {
            shiny::showNotification(
              "No data pond is selected.",
              type = "warning"
            )
            return()
          }

          tryCatch(
            {
              report <- reconcile_dataset_locations(base_path)
              total <- nrow(report)
              verified <- sum(
                report$location_status %in% c(
                  "ok",
                  "repaired_within_lifecycle_root"
                ),
                na.rm = TRUE
              )
              repaired <- sum(
                report$location_status ==
                  "repaired_within_lifecycle_root",
                na.rm = TRUE
              )
              attention <- total - verified

              message <- paste0(
                "Checked ", total, " dataset folder",
                if (total == 1L) "" else "s",
                ": ", verified, " verified",
                if (repaired > 0L) {
                  paste0(" (", repaired, " repaired)")
                } else {
                  ""
                },
                ", ", attention, " requiring attention."
              )

              folder_verification_result(list(
                ok = attention == 0L,
                message = message
              ))

              refresh_selected_state()

              shiny::showNotification(
                message,
                type = if (attention == 0L) "message" else "warning",
                duration = 8
              )
            },
            error = function(e) {
              folder_verification_result(list(
                ok = FALSE,
                message = paste(
                  "Verification failed:",
                  conditionMessage(e)
                )
              ))
              shiny::showNotification(
                conditionMessage(e),
                type = "error",
                duration = 8
              )
            }
          )
        }
      )


      output$current_lifecycle_summary <- shiny::renderUI({
        access_require("read")
        record <- current_lifecycle()
        earth <- app_earth_palette()

        if (
          is.null(record) ||
            nrow(as.data.frame(record)) == 0L
        ) {
          return(
            shiny::tags$div(
              style = paste(
                "padding:10px 12px;",
                paste0(
                  "background:",
                  unname(earth[["stone"]]),
                  ";"
                ),
                paste0(
                  "border:1px solid ",
                  unname(earth[["bark"]]),
                  ";"
                ),
                "border-radius:4px;",
                "margin-bottom:12px;"
              ),
              "Select a saved dataset to view lifecycle details."
            )
          )
        }

        status <- lifecycle_clean_text(
          record$lifecycle_status,
          default = "draft"
        )

        status_label <- lifecycle_status_label(
          status
        )

        stage <- current_stage()
        ready <- current_ready_for_archive()
        mismatch <- current_mismatch()
        locked <- lifecycle_lock_label(
          record$folder_locked
        )

        status_background <- lifecycle_status_colour(
          lifecycle_status = status,
          ready_for_archive = ready,
          mismatch = mismatch
        )

        stage_background <- switch(
          lifecycle_clean_text(
            stage,
            default = ""
          ),
          src = unname(earth[["pale_ochre"]]),
          dev = unname(earth[["pale_sage"]]),
          arc = unname(earth[["tan"]]),
          unname(earth[["stone"]])
        )

        readiness_background <- if (isTRUE(ready)) {
          unname(earth[["sage"]])
        } else {
          unname(earth[["sand"]])
        }

        lock_background <- if (identical(locked, "Locked")) {
          unname(earth[["tan"]])
        } else {
          unname(earth[["parchment"]])
        }

        shiny::fluidRow(
          lifecycle_summary_card(
            title = "Lifecycle status",
            value = status_label,
            background = status_background,
            border = unname(earth[["bark"]])
          ),

          lifecycle_summary_card(
            title = "Pond location",
            value = lifecycle_stage_label(stage),
            background = stage_background,
            border = unname(earth[["olive"]])
          ),

          lifecycle_summary_card(
            title = "Archive readiness",
            value = if (isTRUE(ready)) {
              "Required metadata complete"
            } else {
              "Metadata incomplete"
            },
            background = readiness_background,
            border = unname(earth[["ochre"]])
          ),

          lifecycle_summary_card(
            title = "Folder state",
            value = locked,
            background = lock_background,
            border = unname(earth[["bark"]])
          )
        )
      })


      output$current_lifecycle_table <- DT::renderDT({
        access_require("read")
        record <- current_lifecycle()

        if (
          is.null(record) ||
            nrow(as.data.frame(record)) == 0L
        ) {
          return(
            datapond_datatable(
              data.frame(
                Message =
                  "Select a saved dataset to view lifecycle details.",
                stringsAsFactors = FALSE
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

        record <- as.data.frame(
          record,
          stringsAsFactors = FALSE
        )

        value <- function(field, default = "Not recorded") {
          if (!field %in% names(record)) {
            return(default)
          }

          lifecycle_clean_text(
            record[[field]],
            default = default
          )
        }

        display <- data.frame(
          `Lifecycle attribute` = c(
            "Dataset ID",
            "Dataset title",
            "Dataset UUID",
            "Citation key",
            "Folder key",
            "Version",
            "Lifecycle status",
            "Pond location",
            "Folder lock",
            "Dataset folder",
            "Data folder",
            "Source dataset ID",
            "Released at",
            "Archived at"
          ),

          `Current value` = c(
            value("id"),
            value("title"),
            value("uuid"),
            value("citation_key"),
            value("folder_key"),
            value("version"),
            lifecycle_status_label(
              value(
                "lifecycle_status",
                default = "draft"
              )
            ),
            lifecycle_stage_label(
              current_stage()
            ),
            lifecycle_lock_label(
              record$folder_locked
            ),
            value("dataset_path"),
            value("data_path"),
            value("source_dataset_id"),
            value("released_at"),
            value("archived_at")
          ),

          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        table_widget <- datapond_datatable(
          display,
          rownames = FALSE,
          selection = "none",
          escape = TRUE,
          options = list(
            dom = "t",
            ordering = FALSE,
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = FALSE,
            columnDefs = list(
              list(
                width = "180px",
                targets = 0L
              ),
              list(
                width = "520px",
                targets = 1L
              )
            )
          )
        )

        DT::formatStyle(
          table_widget,
          columns = "Current value",
          whiteSpace = "normal",
          wordBreak = "break-word",
          lineHeight = "1.35"
        )
      })


      output$folder_comparison_table <- DT::renderDT({
        access_require("read")
        comparison <- folder_comparison()

        if (
          is.null(comparison) ||
            nrow(as.data.frame(comparison)) == 0L
        ) {
          comparison <- data.frame(
            message = "No folder comparison is available.",
            stringsAsFactors = FALSE
          )
        }

        if ("message" %in% names(comparison)) {
          names(comparison) <- "Message"

          return(
            datapond_datatable(
              comparison,
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

        comparison <- data.table::as.data.table(
          data.table::copy(comparison)
        )

        required_columns <- list(
          item = NA_character_,
          current = NA_character_,
          proposed = NA_character_
        )

        for (field_name in names(required_columns)) {
          if (!field_name %in% names(comparison)) {
            comparison[
              ,
              (field_name) :=
                required_columns[[field_name]]
            ]
          }
        }

        comparison[
          ,
          current := as.character(current)
        ]

        comparison[
          ,
          proposed := as.character(proposed)
        ]

        comparison[
          is.na(current),
          current := ""
        ]

        comparison[
          is.na(proposed),
          proposed := ""
        ]

        comparison[
          ,
          comparison_class := data.table::fifelse(
            current != proposed,
            "changed",
            "unchanged"
          )
        ]

        display <- comparison[
          ,
          .(
            comparison_class,
            `Folder attribute` = item,
            `Current value` = current,
            `Proposed value` = proposed
          )
        ]

        palette <- lifecycle_display_palette()

        table_widget <- datapond_datatable(
          as.data.frame(
            display,
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          selection = "none",
          filter = "none",
          escape = TRUE,
          options = list(
            dom = "t",
            ordering = FALSE,
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = FALSE,
            columnDefs = list(
              list(
                visible = FALSE,
                targets = 0L
              ),
              list(
                width = "170px",
                targets = 1L
              ),
              list(
                width = "390px",
                targets = c(
                  2L,
                  3L
                )
              )
            )
          )
        )

        table_widget <- DT::formatStyle(
          table_widget,
          columns = "comparison_class",
          target = "row",
          backgroundColor = DT::styleEqual(
            c(
              "changed",
              "unchanged"
            ),
            c(
              unname(palette[["changed"]]),
              unname(palette[["unchanged"]])
            )
          ),
          verticalAlign = "top"
        )

        DT::formatStyle(
          table_widget,
          columns = c(
            "Current value",
            "Proposed value"
          ),
          whiteSpace = "normal",
          wordBreak = "break-word",
          lineHeight = "1.35"
        )
      })


      output$lifecycle_dashboard <- DT::renderDT({
        access_require("read")
        dashboard <- lifecycle_dashboard_data()

        if (
          is.null(dashboard) ||
            nrow(as.data.frame(dashboard)) == 0L
        ) {
          return(
            datapond_datatable(
              data.frame(
                Message =
                  "No datasets are available in the active data pond.",
                stringsAsFactors = FALSE
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

        if ("message" %in% names(dashboard)) {
          message_table <- data.frame(
            Message = as.character(
              dashboard$message
            ),
            stringsAsFactors = FALSE
          )

          return(
            datapond_datatable(
              message_table,
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

        dashboard <- data.table::as.data.table(
          data.table::copy(dashboard)
        )

        display <- dashboard[
          ,
          .(
            row_class,
            current_record,
            `Dataset ID` = id,
            `Dataset title` = title_display,
            `Citation key` = citation_key,
            `Lifecycle status` = lifecycle_label,
            `Pond location` = stage_display,
            `Folder lock` = lock_display,
            `Archive readiness` = readiness_display,
            `Location / status mismatch` = mismatch_display,
            `Recommended next action` = next_action,
            `Dataset folder` = dataset_path
          )
        ]

        palette <- lifecycle_display_palette()

        table_widget <- datapond_datatable(
          as.data.frame(
            display,
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          selection = "none",
          filter = "top",
          escape = TRUE,
          extensions = "Buttons",
          class = "display compact stripe hover",
          options = list(
            pageLength = 15L,
            scrollX = TRUE,
            autoWidth = FALSE,
            dom = "Bfrtip",
            buttons = c(
              "copy",
              "csv"
            ),
            columnDefs = list(
              # Hidden helper columns used for row colouring and
              # emphasis of the currently selected dataset.
              list(
                visible = FALSE,
                targets = c(
                  0L,
                  1L
                )
              ),

              # Keep the numeric ID compact.
              list(
                width = "55px",
                className = "dt-center",
                targets = 2L
              ),

              # Give dataset titles enough space to wrap at words.
              list(
                width = "360px",
                targets = 3L
              ),

              list(
                width = "165px",
                targets = 4L
              ),

              list(
                width = "115px",
                targets = 5L
              ),

              list(
                width = "125px",
                targets = 6L
              ),

              list(
                width = "85px",
                targets = 7L
              ),

              list(
                width = "135px",
                targets = 8L
              ),

              list(
                width = "145px",
                targets = 9L
              ),

              list(
                width = "210px",
                targets = 10L
              ),

              list(
                width = "320px",
                targets = 11L
              )
            )
          )
        )

        table_widget <- DT::formatStyle(
          table_widget,
          columns = "row_class",
          target = "row",
          backgroundColor = DT::styleEqual(
            c(
              "mismatch",
              "archived",
              "released",
              "ready",
              "draft",
              "unknown"
            ),
            c(
              unname(palette[["mismatch"]]),
              unname(palette[["archived"]]),
              unname(palette[["released"]]),
              unname(palette[["ready"]]),
              unname(palette[["draft"]]),
              unname(palette[["unknown"]])
            )
          ),
          verticalAlign = "top"
        )

        table_widget <- DT::formatStyle(
          table_widget,
          columns = "current_record",
          target = "row",
          fontWeight = DT::styleEqual(
            c(
              "yes",
              "no"
            ),
            c(
              "700",
              "400"
            )
          )
        )

        table_widget <- DT::formatStyle(
          table_widget,
          columns = "Location / status mismatch",
          fontWeight = DT::styleEqual(
            c(
              "Yes",
              "No"
            ),
            c(
              "700",
              "400"
            )
          )
        )

        # Use compact text and cell padding across the visible table.
        table_widget <- DT::formatStyle(
          table_widget,
          columns = names(display)[3:12],
          fontSize = "12px",
          paddingTop = "4px",
          paddingBottom = "4px",
          verticalAlign = "top"
        )

        # The ID should remain narrow and on one line.
        table_widget <- DT::formatStyle(
          table_widget,
          columns = "Dataset ID",
          width = "55px",
          minWidth = "55px",
          maxWidth = "55px",
          whiteSpace = "nowrap",
          textAlign = "center"
        )

        # Titles may wrap only between words, not inside ordinary words.
        table_widget <- DT::formatStyle(
          table_widget,
          columns = "Dataset title",
          width = "360px",
          minWidth = "360px",
          whiteSpace = "normal",
          wordBreak = "normal",
          overflowWrap = "break-word",
          lineHeight = "1.3"
        )

        # Long codes, actions and paths may wrap without forcing the
        # title column to become narrow.
        table_widget <- DT::formatStyle(
          table_widget,
          columns = c(
            "Citation key",
            "Recommended next action",
            "Dataset folder"
          ),
          whiteSpace = "normal",
          wordBreak = "normal",
          overflowWrap = "anywhere",
          lineHeight = "1.3"
        )

        table_widget
      })


      shiny::observe({
        access_require("read")
        record <- current_lifecycle()

        if (
          is.null(record) ||
            nrow(as.data.frame(record)) == 0L
        ) {
          shiny::updateSelectInput(
            session,
            "lifecycle_status",
            selected = "draft"
          )

          shiny::updateCheckboxInput(
            session,
            "folder_locked",
            value = FALSE
          )

          return()
        }

        status <- lifecycle_clean_text(
          record$lifecycle_status,
          default = "draft"
        )

        if (!status %in% c(
          "draft",
          "released",
          "archived"
        )) {
          status <- "draft"
        }

        locked <- suppressWarnings(
          as.integer(
            record$folder_locked[1]
          )
        )

        shiny::freezeReactiveValue(
          input,
          "lifecycle_status"
        )

        shiny::updateSelectInput(
          session,
          "lifecycle_status",
          selected = status
        )

        shiny::freezeReactiveValue(
          input,
          "folder_locked"
        )

        shiny::updateCheckboxInput(
          session,
          "folder_locked",
          value = !is.na(locked) &&
            locked == 1L
        )

        stage <- current_stage()

        if (stage %in% c(
          "src",
          "dev",
          "arc"
        )) {
          shiny::freezeReactiveValue(
            input,
            "target_stage"
          )

          shiny::updateSelectInput(
            session,
            "target_stage",
            selected = stage
          )
        }
      })


      shiny::observeEvent(
        input$lifecycle_status,
        {
          access_require("edit", touch = TRUE)
          status <- lifecycle_clean_text(
            input$lifecycle_status,
            default = "draft"
          )

          if (status %in% c(
            "released",
            "archived"
          )) {
            shiny::updateCheckboxInput(
              session,
              "folder_locked",
              value = TRUE
            )
          }
        },
        ignoreInit = TRUE
      )


      shiny::observeEvent(
        input$save_lifecycle,
        {
          access_require("edit", touch = TRUE)
          if (!require_active_user_for_write(
            state,
            "saving lifecycle status"
          )) {
            return()
          }

          context <- valid_dataset_context()

          if (!isTRUE(context$valid)) {
            shiny::showNotification(
              "Select a saved dataset before changing lifecycle status.",
              type = "warning"
            )

            return()
          }

          status <- lifecycle_clean_text(
            input$lifecycle_status,
            default = "draft"
          )

          if (!status %in% c(
            "draft",
            "released",
            "archived"
          )) {
            shiny::showNotification(
              "Select a valid lifecycle status.",
              type = "error"
            )

            return()
          }

          folder_locked <- if (status %in% c(
            "released",
            "archived"
          )) {
            1L
          } else {
            as.integer(
              isTRUE(
                input$folder_locked
              )
            )
          }

          user_person_id <- tryCatch(
            get_active_user_person_id(
              active_user_id = state$active_user_id,
              base_path = context$base_path
            ),
            error = function(e) {
              NA_integer_
            }
          )

          tryCatch(
            {
              set_dataset_lifecycle(
                dataset_id = context$dataset_id,
                base_path = context$base_path,
                lifecycle_status = status,
                folder_locked = folder_locked,
                user_person_id = user_person_id
              )

              tryCatch(
                write_pond_bibliography(
                  context$base_path
                ),
                error = function(e) {
                  shiny::showNotification(
                    paste(
                      "Lifecycle status was saved, but datapond.bib",
                      "could not be refreshed:",
                      conditionMessage(e)
                    ),
                    type = "warning",
                    duration = 8
                  )
                }
              )

              refresh_selected_state()

              stage <- current_stage()

              mismatch_warning <- (
                identical(status, "archived") &&
                  !identical(stage, "arc")
              ) ||
                (
                  identical(status, "released") &&
                    identical(stage, "arc")
                )

              shiny::showNotification(
                if (isTRUE(mismatch_warning)) {
                  paste(
                    "Lifecycle status saved.",
                    "The lifecycle dashboard now flags a folder-location mismatch."
                  )
                } else {
                  "Lifecycle status saved."
                },
                type = if (isTRUE(mismatch_warning)) {
                  "warning"
                } else {
                  "message"
                },
                duration = 6
              )
            },
            error = function(e) {
              shiny::showNotification(
                conditionMessage(e),
                type = "error",
                duration = 8
              )
            }
          )
        }
      )


      shiny::observeEvent(
        input$move_folder,
        {
          access_require("edit", touch = TRUE)
          if (!require_active_user_for_write(
            state,
            "moving the dataset folder"
          )) {
            return()
          }

          context <- valid_dataset_context()

          if (!isTRUE(context$valid)) {
            shiny::showNotification(
              "Select a saved dataset before moving its folder.",
              type = "warning"
            )

            return()
          }

          target_stage <- lifecycle_clean_text(
            input$target_stage,
            default = ""
          )

          if (!target_stage %in% c(
            "src",
            "dev",
            "arc"
          )) {
            shiny::showNotification(
              "Select a valid target lifecycle folder.",
              type = "error"
            )

            return()
          }

          stage <- current_stage()

          if (identical(stage, target_stage)) {
            shiny::showNotification(
              paste(
                "The dataset folder is already in",
                lifecycle_stage_label(target_stage),
                "."
              ),
              type = "message"
            )

            return()
          }

          user_person_id <- tryCatch(
            get_active_user_person_id(
              active_user_id = state$active_user_id,
              base_path = context$base_path
            ),
            error = function(e) {
              NA_integer_
            }
          )

          tryCatch(
            {
              move_dataset_folder(
                dataset_id = context$dataset_id,
                base_path = context$base_path,
                target_stage = target_stage,
                user_person_id = user_person_id
              )

              refresh_selected_state()

              shiny::showNotification(
                paste(
                  "Dataset folder moved to",
                  lifecycle_stage_label(target_stage),
                  "."
                ),
                type = "message",
                duration = 5
              )
            },
            error = function(e) {
              shiny::showNotification(
                conditionMessage(e),
                type = "error",
                duration = 8
              )
            }
          )
        }
      )


      shiny::observeEvent(
        input$rename_folder,
        {
          access_require("edit", touch = TRUE)
          if (!require_active_user_for_write(
            state,
            "renaming the dataset folder"
          )) {
            return()
          }

          context <- valid_dataset_context()

          if (!isTRUE(context$valid)) {
            shiny::showNotification(
              "Select a saved dataset before renaming its folder.",
              type = "warning"
            )

            return()
          }

          tryCatch(
            {
              result <- rename_draft_dataset_folder(
                dataset_id = context$dataset_id,
                base_path = context$base_path
              )

              refresh_selected_state()

              message <- if (
                is.list(result) &&
                  !is.null(result$message)
              ) {
                result$message
              } else {
                "Dataset folder rename completed."
              }

              shiny::showNotification(
                message,
                type = "message",
                duration = 6
              )
            },
            error = function(e) {
              shiny::showNotification(
                conditionMessage(e),
                type = "error",
                duration = 8
              )
            }
          )
        }
      )


      list(
        current_lifecycle = current_lifecycle,
        lifecycle_dashboard = lifecycle_dashboard_data,
        folder_comparison = folder_comparison,
        refresh = function() {
          refresh_token(
            refresh_token() + 1L
          )

          invisible(TRUE)
        }
      )
    }
  )
}
