# Config-panel UI helpers ----
config_section_box <- function(
  title,
  ...,
  background = "#ffffff",
  border = "#D3CEC4"
) {
  shiny::tags$div(
    class = "config-section-box",
    style = paste(
      "padding:10px;",
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
      "margin-bottom:12px;"
    ),

    shiny::h4(
      style = "margin-top:0;",
      title
    ),

    ...
  )
}


config_panel_ui <- function() {
  shiny::tagList(
    shiny::h3(
      style = "margin-top:0;",
      "Data pond configuration"
    ),

    shiny::tags$p(
      class = "config-intro",

      paste(
        "Choose the active data pond and application user,",
        "then set the defaults used when creating new dataset records."
      )
    ),

    shiny::tags$details(
      class = "config-help",
      open = TRUE,

      shiny::tags$summary(
        "How data moves through the pond"
      ),

      shiny::tags$p(
        paste(
          "A data pond combines a lifecycle folder structure",
          "with a local metadata database."
        )
      ),

      shiny::tags$ul(
        shiny::tags$li(
          shiny::strong(
            "src/ \u2014 "
          ),
          paste(
            "sourced, received or reference data retained",
            "close to its supplied form."
          )
        ),

        shiny::tags$li(
          shiny::strong(
            "dev/ \u2014 "
          ),
          paste(
            "active development, transformation, analysis",
            "and draft dataset outputs."
          )
        ),

        shiny::tags$li(
          shiny::strong(
            "arc/ \u2014 "
          ),
          paste(
            "released, frozen, superseded or audit-retained",
            "dataset versions."
          )
        )
      ),

      shiny::tags$p(
        paste(
          "New dataset folders default to dev/. Existing datasets",
          "retain their saved location until an explicit lifecycle",
          "or folder-repair action changes it."
        )
      )
    ),

    shiny::tags$details(
      class = "config-help",

      shiny::tags$summary(
        "How metadata is populated"
      ),

      shiny::tags$ol(
        shiny::tags$li(
          "Select the data pond and active user."
        ),

        shiny::tags$li(
          paste(
            "Create a new dataset record or select",
            "an existing dataset."
          )
        ),

        shiny::tags$li(
          paste(
            "Complete citation, description, licensing,",
            "access and contact metadata."
          )
        ),

        shiny::tags$li(
          paste(
            "Scan data files and review attribute",
            "and spatial metadata."
          )
        ),

        shiny::tags$li(
          paste(
            "Record supporting documents, lineage",
            "and review notes."
          )
        ),

        shiny::tags$li(
          paste(
            "Use Dataset status to identify missing",
            "or incomplete sections before release."
          )
        )
      ),

      shiny::tags$p(
        paste(
          "The main Save Dataset action writes dataset-level",
          "metadata and creates a metadata update-history record."
        )
      )
    ),

    shiny::hr(),

    config_section_box(
      title = "Active application user",
      background = "#D1DDC7",
      border = "#87966C",

      shiny::selectInput(
        "active_user",
        "User responsible for this session",
        choices = NULL
      ),

      shiny::tags$div(
        class = "config-current-value",

        shiny::strong(
          "Current user"
        ),

        shiny::textOutput(
          "current_user_display"
        )
      ),

      shiny::tags$small(
        style = "color:#666;",

        paste(
          "The active user is used as the default reviewer",
          "or editor for metadata changes."
        )
      )
    ),

    config_section_box(
      title = "New dataset defaults",
      background = "#E9DFB5",
      border = "#C3A35B",

      shiny::selectInput(
        "dataset_parent_dir",
        "Create new dataset folders under",
        choices = c(
          "dev \u2014 active development and internally produced datasets" = "dev",
          "src \u2014 sourced or reference datasets" = "src"
        ),
        selected = "dev"
      ),

      shiny::tags$div(
        class = "config-current-value",

        shiny::strong(
          "New dataset location"
        ),

        shiny::textOutput(
          "dataset_parent_dir_display"
        )
      ),

      shiny::tags$small(
        style = "color:#666;",

        paste(
          "This setting applies only when a new dataset folder",
          "is created. It does not relocate existing datasets."
        )
      )
    ),

    config_section_box(
      title = "Optional profiling",
      background = "#F8F4EA",
      border = "#D3CEC4",

      shiny::checkboxInput(
        "attribute_profile_enabled",
        "Enable attribute-table and raster-layer profiling",
        value = FALSE
      ),

      shiny::tags$small(
        style = "color:#666;",

        paste(
          "When enabled, selected saved data files can be",
          "profiled to populate the attribute data dictionary.",
          "Profiling is always started explicitly from Data files."
        )
      )
    ),

    config_section_box(
      title = "Current data pond",
      background = "#F1E9DA",
      border = "#B99572",

      shiny::strong(
        "Active pond path"
      ),

      shiny::verbatimTextOutput(
        "current_path"
      ),

      shiny::strong(
        "Lifecycle folder status"
      ),

      shiny::verbatimTextOutput(
        "pond_structure"
      )
    ),

    shiny::tags$details(
      class = "config-help config-advanced",

      shiny::tags$summary(
        "Change data pond"
      ),

      shiny::tags$p(
        paste(
          "Changing the pond switches the active SQLite database,",
          "reference vocabulary and dataset inventory."
        )
      ),

      shinyFiles::shinyDirButton(
        "data_pond_input",
        "Select data-pond folder",
        "Select an existing or new data-pond folder"
      ),

      shiny::br(),
      shiny::br(),

      shiny::textInput(
        "manual_path",
        "Manually enter a data-pond path",
        placeholder =
          "/path/to/data_pond"
      ),

      shiny::actionButton(
        "set_manual",
        "Set data pond"
      )
    )
  )
}

pond_overview_ui <- function() {
  shiny::tagList(
    shiny::h2(
      "Pond overview"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F1E9DA;",
        "border:1px solid #B99572;",
        "margin-bottom:14px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "This page summarises datasets in the active data pond.",
          "Select a dataset from the table or map to load its",
          "metadata throughout the application."
        )
      )
    ),

    # Retain the existing input internally because the established
    # dataset hydration observer depends on input$dataset_select.
    shiny::tags$div(
      style = "display:none;",

      shiny::selectInput(
        "dataset_select",
        "Current dataset",
        choices = NULL
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 8,

        shiny::h3(
          "Dataset extents"
        ),

        shiny::tags$small(
          style = paste(
            "color:#666;",
            "display:block;",
            "margin-bottom:8px;"
          ),

          paste(
            "Only datasets with a valid derived EPSG:4326",
            "bounding box are displayed on the map.",
            "Map colours follow the pond-status legend."
          )
        ),

        leaflet::leafletOutput(
          "pond_dataset_map",
          height = "500px"
        )
      ),

      shiny::column(
        width = 4,

        shiny::h3(
          "Pond summary"
        ),

        shiny::uiOutput(
          "pond_overview_summary"
        ),

        shiny::hr(),

        shiny::h4(
          "Selected dataset folder"
        ),

        shiny::verbatimTextOutput(
          "current_dataset_folder"
        ),

        shiny::actionButton(
          "repair_dataset_location",
          "Repair selected dataset location"
        )
      )
    ),

    shiny::hr(),

    shiny::h3(
      "Dataset inventory"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:8px;"
      ),

      paste(
        "Select one row to load that dataset.",
        "The selected row is highlighted independently",
        "of the pond-status colour."
      )
    ),

    DT::DTOutput(
      "dataset_table"
    )
  )
}

metadata_review_update_type_choices <- function() {
  c(
    "New metadata record" =
      "new_metadata_record",

    "Metadata development / refinement" =
      "metadata_development",

    "Metadata revision" =
      "metadata_revision",

    "Metadata correction" =
      "metadata_correction",

    "Routine metadata review" =
      "routine_review",

    "File inventory refresh" =
      "file_inventory_refresh",

    "Spatial metadata refresh" =
      "spatial_metadata_refresh",

    "Attribute metadata update" =
      "attribute_metadata_update",

    "Contact update" =
      "contact_update",

    "Citation update" =
      "citation_update",

    "Keyword update" =
      "keyword_update",

    "Quality review" =
      "quality_review",

    "Other update" =
      "other"
  )
}


metadata_review_section_choices <- function() {
  c(
    "Dataset overview" =
      "dataset_overview",

    "Citation" =
      "citation",

    "About" =
      "about",

    "Parties" =
      "parties",

    "Licensing" =
      "licensing",

    "Data access" =
      "data_access",

    "Lineage" =
      "lineage",

    "Data files" =
      "data_files",

    "Spatial metadata" =
      "spatial_metadata",

    "Attribute metadata" =
      "attribute_metadata",

    "General" =
      "general"
  )
}


metadata_review_status_choices <- function() {
  c(
    "Draft" =
      "draft",

    "Reviewed" =
      "reviewed",

    "Approved" =
      "approved",

    "Needs attention" =
      "needs_attention",

    "Deprecated" =
      "deprecated"
  )
}


metadata_review_ui <- function() {
  earth <- app_earth_palette()

  shiny::tagList(
    shiny::h2(
      "Metadata review"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px 12px;",

        paste0(
          "background:",
          unname(
            earth[["parchment"]]
          ),
          ";"
        ),

        paste0(
          "border:1px solid ",
          unname(
            earth[["tan"]]
          ),
          ";"
        ),

        "border-radius:4px;",
        "margin-bottom:14px;"
      ),

      shiny::tags$p(
        style =
          "margin:0; line-height:1.45;",

        paste(
          "Record why metadata changed, which sections were",
          "affected and whether the updated metadata remains",
          "in development or has been reviewed."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 5,

        shiny::tags$div(
          style = paste(
            "padding:12px;",

            paste0(
              "background:",
              unname(
                earth[["cream"]]
              ),
              ";"
            ),

            paste0(
              "border:1px solid ",
              unname(
                earth[["stone"]]
              ),
              ";"
            ),

            "border-radius:4px;",
            "margin-bottom:12px;"
          ),

          shiny::h3(
            style = "margin-top:0;",
            "Metadata update record"
          ),

          shiny::selectInput(
            "metadata_reviewer_user",
            "Reviewer / editor",
            choices = NULL
          ),

          shiny::tags$small(
            style = paste(
              "color:#625C52;",
              "display:block;",
              "margin-bottom:8px;"
            ),

            paste(
              "Defaults to the active application user.",
              "Change it only when another person is",
              "responsible for this update."
            )
          ),

          shiny::tags$div(
            style = paste(
              "padding:8px 10px;",

              paste0(
                "background:",
                unname(
                  earth[["pale_sage"]]
                ),
                ";"
              ),

              paste0(
                "border:1px solid ",
                unname(
                  earth[["moss"]]
                ),
                ";"
              ),

              "border-radius:3px;",
              "margin-bottom:10px;"
            ),

            shiny::strong(
              "Reviewer to be saved"
            ),

            shiny::textOutput(
              "metadata_reviewer_display"
            )
          ),

          shiny::uiOutput(
            "metadata_review_context"
          ),

          shiny::selectizeInput(
            "metadata_update_type",
            "Update type",
            choices =
              metadata_review_update_type_choices(),
            selected =
              "new_metadata_record",
            multiple = TRUE,
            options = list(
              plugins =
                list(
                  "remove_button"
                ),
              placeholder =
                "Select one or more update types."
            )
          ),

          shiny::selectizeInput(
            "metadata_section",
            "Metadata section(s) affected",
            choices =
              metadata_review_section_choices(),
            selected = c(
              "dataset_overview",
              "citation",
              "about"
            ),
            multiple = TRUE,
            options = list(
              plugins =
                list(
                  "remove_button"
                ),
              placeholder =
                "Select one or more affected sections."
            )
          ),

          shiny::textAreaInput(
            "metadata_update_reason",
            "Reason for metadata update",
            placeholder = paste(
              "Explain why this metadata record",
              "is being created or updated."
            ),
            rows = 3
          ),

          shiny::textAreaInput(
            "metadata_update_summary",
            "Summary of update",
            placeholder =
              "Briefly describe what changed.",
            rows = 4
          ),

          shiny::selectInput(
            "metadata_status",
            "Metadata status",
            choices =
              metadata_review_status_choices(),
            selected = "draft"
          )
        )
      ),

      shiny::column(
        width = 7,

        shiny::h3(
          "Metadata update history"
        ),

        shiny::tags$small(
          style = paste(
            "color:#625C52;",
            "display:block;",
            "margin-bottom:10px;"
          ),

          paste(
            "Rows are coloured by metadata-review status.",
            "The underlying lifecycle status of the dataset",
            "is recorded separately on the Lifecycle tab."
          )
        ),

        DT::DTOutput(
          "metadata_review_table"
        )
      )
    )
  )
}

# UI layout ----
ui <- fluidPage(
  actionButton("toggle_config", "Show / hide configuration"),
  tags$style(
    HTML(
      "
      .form-group {
        margin-bottom: 15px;
      }

      /* Shiny defaults non-inline input containers to roughly 300px. Let form
         controls use their full layout-column width. */
      .shiny-input-container:not(.shiny-input-container-inline) {
        width: 100% !important;
        max-width: none;
      }

      .shiny-input-container > input[type='text'].form-control,
      .shiny-input-container > textarea.form-control,
      .shiny-input-container select.shiny-input-select,
      .shiny-input-container .selectize-control {
        width: 100%;
        max-width: none;
        box-sizing: border-box;
      }

      small {
        font-size: 12px;
      }

      #config_panel {
        background-color: #F8F4EA;
        padding: 12px;
        border-right: 1px solid #ddd;
        max-height: calc(100vh - 20px);
        overflow-y: auto;
        position: sticky;
        top: 10px;
      }

      #main_panel {
        transition: width 0.2s ease;
      }

      /* Earth-toned navigation tabs replace Bootstrap's white/blue defaults. */
      .nav-tabs {
        border-bottom-color: #87966C;
      }

      .nav-tabs > li > a {
        color: #3F5146 !important;
        background-color: #D1DDC7;
        border-color: #A8B59D #A8B59D #87966C;
        margin-right: 4px;
      }

      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus {
        color: #FFFFFF !important;
        background-color: #87966C;
        border-color: #526657;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #FFFFFF !important;
        background-color: #526657 !important;
        border-color: #526657 #526657 #526657 !important;
      }

      .config-intro {
        color: #555;
        line-height: 1.45;
      }

      .config-help {
        background: #fff;
        border: 1px solid #ccc;
        margin-bottom: 10px;
        padding: 8px 10px;
      }

      .config-help summary {
        position: relative;
        cursor: pointer;
        font-weight: 600;
        color: #3F5146;
        padding: 7px 115px 7px 24px;
        border-radius: 3px;
        user-select: none;
        list-style: none;
      }

      .config-help summary::-webkit-details-marker {
        display: none;
      }

      .config-help summary::before {
        content: '>';
        position: absolute;
        left: 4px;
        top: 7px;
        font-size: 12px;
        color: #3F5146;
      }

      .config-help[open] summary::before {
        content: 'v';
      }

      .config-help summary::after {
        content: 'Click to expand';
        position: absolute;
        right: 5px;
        top: 8px;
        font-size: 11px;
        font-weight: normal;
        color: #666;
      }

      .config-help[open] summary::after {
        content: 'Click to collapse';
      }

      .config-help summary:hover {
        background: #D1DDC7;
        color: #3F5146;
      }

      .config-help summary:focus {
        outline: 2px solid #87966C;
        outline-offset: 2px;
      }

      .config-help p,
      .config-help ul,
      .config-help ol {
        margin-top: 8px;
        margin-bottom: 8px;
      }

      .config-help ul,
      .config-help ol {
        padding-left: 22px;
      }

      .config-current-value {
        padding: 7px;
        background: #fff;
        border: 1px solid #ccc;
        margin-top: 4px;
        margin-bottom: 6px;
        overflow-wrap: anywhere;
      }

      .config-section-box .form-group:last-of-type {
        margin-bottom: 8px;
      }

      .config-advanced {
        margin-bottom: 14px;
      }

      #dataset_table table.dataTable tbody tr {
        cursor: pointer;
      }

      #dataset_table table.dataTable tbody tr:hover td {
        box-shadow:
          inset 0 0 0 9999px
          rgba(255, 255, 255, 0.22);
      }

      #dataset_table table.dataTable tbody tr.selected td {
        background-color: #526657 !important;
        box-shadow:
          inset 0 0 0 9999px
          rgba(47, 111, 104, 0.96) !important;
        color: #ffffff !important;
      }

      #dataset_table table.dataTable tbody tr.selected a {
        color: #ffffff !important;
      }
      "
    )
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleConfig', function(message) {
      var panel = document.getElementById('config_panel');
      var mainPanel = document.getElementById('main_panel');

      if (panel.style.display === 'none') {
        panel.style.display = 'block';
        mainPanel.classList.remove('col-sm-12');
        mainPanel.classList.add('col-sm-9');
      } else {
        panel.style.display = 'none';
        mainPanel.classList.remove('col-sm-9');
        mainPanel.classList.add('col-sm-12');
      }

      window.setTimeout(function() {
        window.dispatchEvent(new Event('resize'));
      }, 220);
    });
  ")),
  fluidRow(
    # LEFT CONFIG PANEL
    shiny::column(
      width = 3,
      id = "config_panel",

      config_panel_ui()
    ),

    # MAIN APP
    column(
      width = 9,
      id = "main_panel",
      titlePanel("Data pond \u2014 v0.5"),
      tabsetPanel(
        shiny::tabPanel(
          "Pond overview",
          pond_overview_ui()
        ),
        shiny::tabPanel(
          "Dataset intro",
          fluidRow(
            column(
              width = 7,
              h3("Citation"),
              uiOutput("citation_fields"),
              mod_authors_ui("authors"),
              mod_dataset_org_ui("orgs"),
              hr(),
              h3("About"),
              uiOutput("about_fields")
            ),
            column(
              width = 5,
              h4("Citation summary"),
              tags$style(
                HTML(
                  "
                  .suggested-citation {
                    margin: 0.25rem 0 0.75rem 0;
                    padding: 0.65rem 0.85rem 0.65rem 2.85rem;
                    text-indent: -2rem;
                    line-height: 1.55;
                    white-space: normal;
                    overflow-wrap: anywhere;
                    background: #F8F4EA;
                    border-left: 3px solid #3F5146;
                    border-radius: 3px;
                  }
                  "
                )
              ),
              tags$div(
                class = "citation-preview-panel",

                tags$p(
                  strong(
                    "Dataset folder key"
                  )
                ),

                verbatimTextOutput(
                  "folder_key_preview"
                ),

                tags$hr(),

                tags$p(
                  strong(
                    "Bibliographic citation key"
                  )
                ),
                verbatimTextOutput(
                  "citation_key_preview"
                ),

                tags$hr(),

                tags$p(
                  strong(
                    "Suggested citation"
                  )
                ),
                checkboxInput(
                  "use_organisation_as_author",
                  "Use organisation as citation author",
                  value = TRUE
                ),
                uiOutput(
                  "citation_preview"
                )
              )
            )
          )
        ),
        tabPanel(
          "Dataset status",
          mod_dataset_status_ui("dataset_status")
        ),
        tabPanel(
          "Dataset lifecycle",
          mod_dataset_lifecycle_ui("dataset_lifecycle")
        ),
        shiny::tabPanel(
          "Metadata review",

          metadata_review_ui()
        ),
        shiny::tabPanel(
          "Licensing & data access",

          shiny::tabsetPanel(
            id = "licensing_access_section",
            selected = "sharing",

            shiny::tabPanel(
              title =
                "Sharing rights & licence",
              value =
                "sharing",

              mod_licensing_ui(
                "licensing"
              )
            ),

            shiny::tabPanel(
              title =
                "Distribution & embargo",
              value =
                "distribution",

              shiny::fluidRow(
                shiny::column(
                  width = 8,

                  mod_data_access_ui(
                    "data_access"
                  )
                ),

                shiny::column(
                  width = 4,

                  shiny::h4(
                    "How this fits together"
                  ),

                  shiny::tags$div(
                    style = paste(
                      "padding:10px;",
                      "background:#F8F4EA;",
                      "border:1px solid #ddd;"
                    ),

                    shiny::tags$ul(
                      shiny::tags$li(
                        paste(
                          "Sharing rights records who owns or",
                          "controls redistribution."
                        )
                      ),

                      shiny::tags$li(
                        paste(
                          "Licence records what recipients may do."
                        )
                      ),

                      shiny::tags$li(
                        paste(
                          "Distribution records where the dataset",
                          "can be obtained."
                        )
                      ),

                      shiny::tags$li(
                        paste(
                          "Embargo records a temporary access delay."
                        )
                      )
                    )
                  )
                )
              )
            ),

            shiny::tabPanel(
              title =
                "Access contacts",
              value =
                "contacts",

              mod_poc_ui(
                "poc"
              )
            )
          )
        ),
        tabPanel(
          "Data files",
          mod_data_files_ui("data_files")
        ),
        tabPanel(
          "Spatial metadata",
          data_files_spatial_ui("data_files")
        ),
        tabPanel(
          "Supporting documents",
          mod_supporting_documents_ui("supporting_docs")
        ),
        tabPanel(
          "Lineage",
          mod_lineage_ui("lineage")
        ),
        tabPanel(
          "Parties",
          mod_parties_ui("parties")
        ),
        tabPanel(
          "Keyword administration",
          mod_keyword_admin_ui("keyword_admin")
        )
      ),
      br(),
      br(),
      hr(),
      actionButton("clear_dataset_form", "Clear form / Start new record"),
      actionButton("new_dataset", "New Dataset Folder"),
      actionButton("open_dataset_folder", "Open Dataset Folder"),
      actionButton("open_data_folder", "Open Data Folder"),
      actionButton("save", "Save Dataset"),
      br(),
      hr(),
    )
  )
)


# server ----
server <- function(input, output, session) {
  ns <- session$ns
  schema_path <- getOption("datapond.schema_path")

  if (
    is.null(schema_path) ||
    length(schema_path) == 0L ||
    is.na(schema_path[1]) ||
    !nzchar(trimws(as.character(schema_path[1])))
  ) {
    schema_path <- datapond_resource("metadata_schema.xlsx")
  }

  schema <- load_schema_grouped(
    normalizePath(
      path.expand(as.character(schema_path[1])),
      mustWork = TRUE,
      winslash = "/"
    )
  )

  ## Resolve initial pond ----
  requested_data_pond <- getOption("datapond.data_pond")
  has_requested_data_pond <- (
    !is.null(requested_data_pond) &&
    length(requested_data_pond) > 0L &&
    !is.na(requested_data_pond[1]) &&
    nzchar(trimws(as.character(requested_data_pond[1])))
  )

  config <- if (isTRUE(has_requested_data_pond)) {
    requested_path <- normalizePath(
      path.expand(as.character(requested_data_pond[1])),
      mustWork = FALSE,
      winslash = "/"
    )
    read_pond_config(requested_path)
  } else {
    load_config()
  }

  data_pond_path <- if (
    isTRUE(has_requested_data_pond)
  ) {
    normalizePath(
      path.expand(as.character(requested_data_pond[1])),
      mustWork = FALSE,
      winslash = "/"
    )
  } else if (
    !is.null(config) &&
    !is.null(config$data_pond) &&
    config$data_pond != ""
  ) {
    config$data_pond
  } else {
    normalizePath("~/Documents/data_pond", mustWork = FALSE)
  }

  if (isTRUE(has_requested_data_pond)) {
    write_last_data_pond_path(data_pond_path)
  }
  initial_pond_config <- read_pond_config(data_pond_path)

  initial_active_user_id <-
    initial_pond_config$active_user %||%
    config$active_user %||%
    NULL

  initial_dataset_parent_dir <- normalise_dataset_parent_dir(
    initial_pond_config$dataset_parent_dir %||%
      config$dataset_parent_dir %||%
      "dev"
  )


  ## Create and migrate the initial pond database ----
  ensure_pond_structure(data_pond_path)
  init_db(data_pond_path)
  ensure_entry_data_access_columns(data_pond_path)
  backfill_missing_dataset_uuids(data_pond_path)
  backfill_missing_citation_keys(data_pond_path)
  backfill_missing_folder_keys(data_pond_path)

  conn <- get_conn(data_pond_path)

  initial_data_pond_id <- ensure_data_pond_id(
    data_pond_path,
    conn = conn
  )

  DBI::dbDisconnect(conn)


  ## schema sections ----
  # load keywords from excel
  keywords_vocab <- load_vocab(datapond_resource("vocab_keywords.csv"))
  org_dt <- load_organisations_csv(
    datapond_resource("vocab_organisations.csv")
  )
  persons_dt <- fread(datapond_resource("vocab_persons.csv"))
  citation_dt <- get_section(schema, "Dataset overview", "Citation")
  about_dt    <- get_section(schema, "Dataset overview", "About")


  ## helper functions ----
  # 1. refresh helper
  refresh_dataset_context <- function(
    datasets = TRUE,
    reviews = TRUE,
    lineage = TRUE,
    supporting_docs = TRUE,
    data_files = TRUE,
    attributes = TRUE,
    spatial_metadata = TRUE
  ) {
    if (isTRUE(datasets)) {
      state$refresh_datasets <- state$refresh_datasets + 1L
    }
    if (isTRUE(reviews)) {
      state$refresh_metadata_reviews <- state$refresh_metadata_reviews + 1L
    }
    if (isTRUE(lineage)) {
      state$refresh_lineage <- state$refresh_lineage + 1L
    }
    if (isTRUE(supporting_docs)) {
      state$refresh_supporting_docs <- state$refresh_supporting_docs + 1L
    }
    if (isTRUE(data_files)) {
      state$refresh_data_files_saved <- state$refresh_data_files_saved + 1L
    }
    if (isTRUE(attributes)) {
      state$refresh_attributes <- state$refresh_attributes + 1L
    }
    if (isTRUE(spatial_metadata)) {
      state$refresh_spatial_metadata <- state$refresh_spatial_metadata + 1L
    }
    invisible(TRUE)
  }

  # 2. metadata review context and reset helpers ----

  metadata_review_scalar <- function(
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


  metadata_review_record_value <- function(
    record,
    field,
    default = ""
  ) {
    if (
      is.null(record) ||
      nrow(as.data.frame(record)) == 0L ||
      !field %in% names(record)
    ) {
      return(default)
    }

    metadata_review_scalar(
      record[[field]],
      default = default
    )
  }


  metadata_review_major_version <- function(
    version
  ) {
    version <- metadata_review_scalar(
      version
    )

    if (!nzchar(version)) {
      return(
        NA_integer_
      )
    }

    match <- regexec(
      "^\\s*([0-9]+)(?:\\.[0-9]+){0,2}\\s*$",
      version
    )

    parts <- regmatches(
      version,
      match
    )[[1]]

    if (length(parts) < 2L) {
      return(
        NA_integer_
      )
    }

    suppressWarnings(
      as.integer(
        parts[2]
      )
    )
  }


  metadata_review_dataset_stage <- function(
    dataset_path
  ) {
    dataset_path <- metadata_review_scalar(
      dataset_path
    )

    if (!nzchar(dataset_path)) {
      return("")
    }

    dataset_path <- normalizePath(
      path.expand(
        dataset_path
      ),
      mustWork = FALSE,
      winslash = "/"
    )

    stage <- tolower(
      basename(
        dirname(
          dataset_path
        )
      )
    )

    if (!stage %in% c(
      "src",
      "dev",
      "arc"
    )) {
      return("")
    }

    stage
  }


  metadata_review_defaults <- function(
    record = NULL,
    new_record = FALSE
  ) {
    if (isTRUE(new_record)) {
      return(
        list(
          mode =
            "new",

          update_type =
            "new_metadata_record",

          sections =
            c(
              "dataset_overview",
              "citation",
              "about"
            ),

          metadata_status =
            "draft",

          reason =
            "Initial metadata record created.",

          lifecycle_status =
            "draft",

          stage =
            "dev",

          version =
            ""
        )
      )
    }

    lifecycle_status <- tolower(
      metadata_review_record_value(
        record,
        "lifecycle_status",
        default = "draft"
      )
    )

    dataset_path <-
      metadata_review_record_value(
        record,
        "dataset_path"
      )

    stage <- metadata_review_dataset_stage(
      dataset_path
    )

    version <- metadata_review_record_value(
      record,
      "version"
    )

    major_version <-
      metadata_review_major_version(
        version
      )

    # A missing version in an unlocked draft development
    # record is also treated as pre-release development.
    pre_release_development <- (
      identical(
        lifecycle_status,
        "draft"
      ) &&
        identical(
          stage,
          "dev"
        ) &&
        (
          is.na(
            major_version
          ) ||
            major_version < 1L
        )
    )

    if (isTRUE(
      pre_release_development
    )) {
      return(
        list(
          mode =
            "development",

          update_type =
            "metadata_development",

          sections =
            "dataset_overview",

          metadata_status =
            "draft",

          reason = paste(
            "Ongoing metadata development and refinement",
            "before the first release."
          ),

          lifecycle_status =
            lifecycle_status,

          stage =
            stage,

          version =
            version
        )
      )
    }

    list(
      mode =
        "revision",

      update_type =
        "metadata_revision",

      sections =
        "dataset_overview",

      metadata_status =
        "reviewed",

      reason =
        "",

      lifecycle_status =
        lifecycle_status,

      stage =
        stage,

      version =
        version
    )
  }


  reset_metadata_review_inputs <- function(
    new_record = TRUE,
    record = NULL
  ) {
    defaults <- metadata_review_defaults(
      record = record,
      new_record = new_record
    )

    shiny::updateSelectizeInput(
      session,
      "metadata_update_type",
      choices =
        metadata_review_update_type_choices(),
      selected =
        defaults$update_type,
      server = TRUE
    )

    shiny::updateSelectizeInput(
      session,
      "metadata_section",
      choices =
        metadata_review_section_choices(),
      selected =
        defaults$sections,
      server = TRUE
    )

    shiny::updateSelectInput(
      session,
      "metadata_status",
      choices =
        metadata_review_status_choices(),
      selected =
        defaults$metadata_status
    )

    shiny::updateTextAreaInput(
      session,
      "metadata_update_reason",
      value =
        defaults$reason
    )

    shiny::updateTextAreaInput(
      session,
      "metadata_update_summary",
      value = ""
    )

    invisible(
      defaults
    )
  }

  # 3. pond reset helper
  reset_context_after_pond_change <- function() {
    cancel_dataset_intro_hydration(
      reset_ui_readiness = TRUE
    )

    clear_current_dataset_context(
      state,
      refresh = FALSE
    )

    clear_dataset_overview_forms(
      session = session,
      citation_dt = citation_dt,
      about_dt = about_dt
    )

    authors_mod$clear()
    poc_mod$clear()
    org_mod$clear()
    data_access_mod$clear()

    supporting_docs_mod$clear()
    licensing_mod$clear()
    lineage_mod$clear()
    data_files_mod$clear()

    last_hydrated_dataset_id(
      NULL
    )

    freezeReactiveValue(
      input,
      "dataset_select"
    )

    updateSelectInput(
      session,
      "dataset_select",
      choices = c(
        "-- No dataset selected --" = ""
      ),
      selected = ""
    )

    updateCheckboxInput(
      session,
      "use_organisation_as_author",
      value = TRUE
    )

    reset_metadata_review_inputs()

    invisible(TRUE)
  }

  # 4. pond-switch helper
  sync_dataset_parent_dir <- function(parent = NULL) {
    parent <- normalise_dataset_parent_dir(
      parent %||% input$dataset_parent_dir %||% state$dataset_parent_dir
    )
    state$dataset_parent_dir <- parent
    if (!is.null(state$data_pond) &&
        length(state$data_pond) > 0 &&
        !is.na(state$data_pond) &&
        state$data_pond != "") {
      save_config(
        path = state$data_pond,
        active_user_id = state$active_user_id,
        dataset_parent_dir = parent,
        attribute_profile_enabled = input$attribute_profile_enabled
      )
      state$pond_config <- read_pond_config(state$data_pond)
    }
    parent
  }

  # 5. citation choices helpers ----

  get_choices_for_pond <- function(
    field_code,
    base_path
  ) {
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
        character()
      )
    }

    base_path <- normalizePath(
      path.expand(
        as.character(
          base_path[1]
        )
      ),
      mustWork = FALSE,
      winslash = "/"
    )

    get_keywords(
      field_code = field_code,
      base_path = base_path
    )
  }


  # Compatibility wrapper for calls made inside ordinary
  # reactive contexts such as renderUI().
  get_choices <- function(field_code) {
    base_path <- shiny::isolate(
      state$data_pond
    )

    get_choices_for_pond(
      field_code = field_code,
      base_path = base_path
    )
  }


  # 6. seed pond data helper
  seed_pond_reference_data <- function(path, user_id = NULL) {
    if (
      is.null(path) ||
      length(path) == 0L ||
      is.na(path[1]) ||
      !nzchar(trimws(as.character(path[1])))
    ) {
      stop(
        "A valid data pond path is required for reference-data seeding."
      )
    }

    path <- normalizePath(
      path.expand(as.character(path[1])),
      mustWork = FALSE,
      winslash = "/"
    )

    vocab_result <- seed_keyword_vocab_if_empty(
      vocab_dt = keywords_vocab,
      base_path = path,
      user_id = user_id
    )

    parties_result <- seed_pond_parties(
      base_path = path,
      organisation_dt = org_dt,
      person_dt = persons_dt
    )

    invisible(
      list(
        vocabulary = vocab_result,
        parties = parties_result
      )
    )
  }

  # 7. switch data pond helper
  switch_data_pond <- function(path) {
    path <- normalizePath(
      path.expand(trimws(path[1])),
      mustWork = FALSE,
      winslash = "/"
    )

    if (!nzchar(path)) {
      stop("A valid data pond path is required.")
    }

    ensure_pond_structure(path)
    init_db(path)
    ensure_entry_data_access_columns(path)
    backfill_missing_dataset_uuids(path)
    backfill_missing_citation_keys(path)
    backfill_missing_folder_keys(path)

    conn <- get_conn(path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    data_pond_id <- ensure_data_pond_id(
      path,
      conn = conn
    )

    pond_config <- read_pond_config(path)

    reset_context_after_pond_change()

    state$data_pond <- path
    state$data_pond_id <- data_pond_id
    state$pond_config <- pond_config
    state$active_user_id <-
      pond_config$active_user %||% NULL
    state$dataset_parent_dir <- normalise_dataset_parent_dir(
      pond_config$dataset_parent_dir %||% "dev"
    )

    seed_pond_reference_data(
      path = path,
      user_id = state$active_user_id %||% NULL
    )

    state$refresh_vocab <- state$refresh_vocab + 1L

    state$refresh_org <-
      state$refresh_org + 1L

    state$refresh_person <-
      state$refresh_person + 1L

    updateSelectInput(
      session,
      "dataset_parent_dir",
      selected = state$dataset_parent_dir
    )

    save_config(
      path = path,
      active_user_id = state$active_user_id,
      dataset_parent_dir = state$dataset_parent_dir,
      attribute_profile_enabled =
        pond_config$attribute_profile$enabled %||% FALSE
    )

    refresh_dataset_context()

    showNotification(
      paste("Data pond set to:", path),
      type = "message"
    )

    invisible(path)
  }

  # 8. save helper
  save_dataset_relationship_sections <- function(dataset_id) {
    if (is.null(dataset_id) ||
        length(dataset_id) == 0 ||
        is.na(dataset_id)) {
      stop("dataset_id is required to save relationship sections.")
    }
    clear_dataset_relationships(dataset_id, state$data_pond)
    authors_df <- authors_mod$authors()
    if (!is.null(authors_df) && nrow(authors_df) > 0) {
      insert_authors(
        dataset_id,
        as.integer(authors_df$id),
        state$data_pond
      )
    }
    poc_df <- poc_mod$poc()
    if (!is.null(poc_df) && nrow(poc_df) > 0) {
      for (i in seq_len(nrow(poc_df))) {
        insert_poc(
          dataset_id,
          as.integer(poc_df$id[i]),
          poc_df$role[i],
          state$data_pond
        )
      }
    }
    org_df <- org_mod$orgs()
    if (!is.null(org_df) && nrow(org_df) > 0) {
      for (i in seq_len(nrow(org_df))) {
        insert_dataset_org(
          dataset_id,
          as.integer(org_df$id[i]),
          org_df$role[i],
          state$data_pond
        )
      }
    }
    invisible(TRUE)
  }


  ## Create reactive state ----
  state <- reactiveValues(
    data_pond_id = initial_data_pond_id,

    active_user_id = initial_active_user_id,

    dataset_parent_dir = initial_dataset_parent_dir,
    data_pond = data_pond_path,
    pond_config = initial_pond_config,

    dataset_path = NULL,
    data_path = NULL,
    current_dataset_id = NULL,
    current_dataset_uuid = NULL,
    current_citation_key = NULL,
    current_folder_key = NULL,
    current_dataset_folder = NULL,

    refresh_org = 0L,
    refresh_person = 0L,
    refresh_vocab = 0L,
    refresh_datasets = 0L,
    refresh_metadata_reviews = 0L,
    refresh_data_files_saved = 0L,
    refresh_attributes = 0L,
    refresh_spatial_metadata = 0L,
    refresh_lineage = 0L,
    refresh_supporting_docs = 0L
  )

  ## Seed database ----
  # Seed the initial pond once per Shiny session.
  seed_pond_reference_data(
    path = data_pond_path,
    user_id = initial_active_user_id
  )

  ## Initialize modules ----
  dataset_status_mod <- mod_dataset_status_server(
    "dataset_status",
    state = state,
    schema_grouped = schema
  )
  dataset_lifecycle_mod <- mod_dataset_lifecycle_server(
    "dataset_lifecycle",
    state = state,
    schema_grouped = schema
  )
  mod_parties_server("parties", state = state)
  mod_keyword_admin_server(
    "keyword_admin",
    state = state,
    vocab_path = datapond_resource("vocab_keywords.csv")
  )
  authors_mod <- mod_authors_server("authors", state = state)
  poc_mod <- mod_poc_server("poc", state = state)
  org_mod <- mod_dataset_org_server("orgs", state = state)

  citation_author_style <- shiny::reactive({
    use_organisation <-
      input$use_organisation_as_author

    if (is.null(use_organisation)) {
      return(
        "organisation"
      )
    }

    if (isTRUE(use_organisation)) {
      "organisation"
    } else {
      "author"
    }
  })

  suggested_citation_text <- shiny::reactive({
    vals <- as.list(
      c(
        collect_inputs(
          input,
          citation_dt
        ),

        collect_inputs(
          input,
          about_dt
        )
      )
    )

    tryCatch(
      generate_citation(
        vals = vals,
        authors_df =
          authors_mod$authors(),
        org_df =
          org_mod$orgs(),
        style =
          citation_author_style()
      ),
      error = function(e) {
        paste(
          "Suggested citation unavailable:",
          conditionMessage(e)
        )
      }
    )
  })

  licensing_mod <- mod_licensing_server(
    "licensing",
    state = state,
    org_mod = org_mod,
    citation_text =
      suggested_citation_text
  )

  data_access_mod <- mod_data_access_server(
    "data_access", state = state
  )
  data_files_mod <- mod_data_files_server("data_files", state)

  required_data_files_methods <- c(
    "clear",
    "creation_scale_values",
    "creation_scale_validation",
    "load_creation_scale"
  )

  missing_data_files_methods <- setdiff(
    required_data_files_methods,
    names(data_files_mod)
  )

  if (length(missing_data_files_methods) > 0L) {
    stop(
      paste(
        "mod_data_files_server() did not return required methods:",
        paste(
          missing_data_files_methods,
          collapse = ", "
        )
      )
    )
  }

  non_function_data_files_methods <-
    required_data_files_methods[
      !vapply(
        data_files_mod[
          required_data_files_methods
        ],
        is.function,
        logical(1)
      )
    ]

  if (length(non_function_data_files_methods) > 0L) {
    stop(
      paste(
        "These data-files module methods are not functions:",
        paste(
          non_function_data_files_methods,
          collapse = ", "
        )
      )
    )
  }

  supporting_docs_mod <- mod_supporting_documents_server(
    "supporting_docs", state = state
  )
  lineage_mod <- mod_lineage_server("lineage", state)


  ## Dataset Intro dynamic UI and deferred hydration ----

  citation_fields_ready <- shiny::reactiveVal(
    FALSE
  )

  about_fields_ready <- shiny::reactiveVal(
    FALSE
  )

  pending_dataset_intro_hydration <-
    shiny::reactiveVal(
      NULL
    )

  dataset_intro_hydration_token <-
    shiny::reactiveVal(
      0L
    )

  dataset_intro_hydration_scheduled <-
    shiny::reactiveVal(
      FALSE
    )


  queue_dataset_intro_hydration <- function(
    dataset_id,
    rec,
    base_path = NULL
  ) {
    dataset_id <- suppressWarnings(
      as.integer(
        dataset_id[1]
      )
    )

    if (
      is.na(dataset_id) ||
      is.null(rec) ||
      nrow(as.data.frame(rec)) == 0L
    ) {
      return(
        invisible(FALSE)
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
      base_path <- shiny::isolate(
        state$data_pond
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
        invisible(FALSE)
      )
    }

    base_path <- normalizePath(
      path.expand(
        as.character(
          base_path[1]
        )
      ),
      mustWork = FALSE,
      winslash = "/"
    )

    token <-
      dataset_intro_hydration_token() +
        1L

    dataset_intro_hydration_token(
      token
    )

    pending_dataset_intro_hydration(
      list(
        dataset_id = dataset_id,

        record = as.data.frame(
          rec,
          stringsAsFactors = FALSE
        ),

        base_path = base_path,

        token = token
      )
    )

    invisible(TRUE)
  }

  cancel_dataset_intro_hydration <- function(
    reset_ui_readiness = FALSE
  ) {
    dataset_intro_hydration_token(
      dataset_intro_hydration_token() +
        1L
    )

    pending_dataset_intro_hydration(
      NULL
    )

    dataset_intro_hydration_scheduled(
      FALSE
    )

    if (isTRUE(
      reset_ui_readiness
    )) {
      citation_fields_ready(
        FALSE
      )

      about_fields_ready(
        FALSE
      )
    }

    invisible(TRUE)
  }


  output$citation_fields <- shiny::renderUI({
    # Explicit dependencies ensure this UI is rebuilt when
    # the active pond or its keyword vocabulary changes.
    state$data_pond
    state$refresh_vocab

    citation_fields_ready(
      FALSE
    )

    base_path <- state$data_pond

    section_ui <- generate_section(
      citation_dt,
      ns,
      function(field_code) {
        get_choices_for_pond(
          field_code = field_code,
          base_path = base_path
        )
      }
    )

    session$onFlushed(
      function() {
        citation_fields_ready(
          TRUE
        )
      },
      once = TRUE
    )

    section_ui
  })


  output$about_fields <- shiny::renderUI({
    # Explicit dependencies ensure this UI is rebuilt when
    # the active pond or its keyword vocabulary changes.
    state$data_pond
    state$refresh_vocab

    about_fields_ready(
      FALSE
    )

    base_path <- state$data_pond

    section_ui <- generate_section(
      about_dt,
      ns,
      function(field_code) {
        get_choices_for_pond(
          field_code = field_code,
          base_path = base_path
        )
      }
    )

    session$onFlushed(
      function() {
        about_fields_ready(
          TRUE
        )
      },
      once = TRUE
    )

    section_ui
  })


  # Dataset Intro is not the initially visible tab.
  # Render these outputs even while their tab is hidden.
  shiny::outputOptions(
    output,
    "citation_fields",
    suspendWhenHidden = FALSE
  )

  shiny::outputOptions(
    output,
    "about_fields",
    suspendWhenHidden = FALSE
  )


  # Apply queued field values only after both renderUI outputs
  # have reached the browser.
  shiny::observe({
    pending <-
      pending_dataset_intro_hydration()

    if (is.null(pending)) {
      return()
    }

    if (
      !isTRUE(
        citation_fields_ready()
      ) ||
      !isTRUE(
        about_fields_ready()
      )
    ) {
      return()
    }

    if (isTRUE(
      dataset_intro_hydration_scheduled()
    )) {
      return()
    }

    scheduled_token <-
      pending$token

    dataset_intro_hydration_scheduled(
      TRUE
    )

    # Wait one additional browser flush after both renderUI
    # sections have been delivered.
    session$onFlushed(
      function() {
        dataset_intro_hydration_scheduled(
          FALSE
        )

        current_pending <- shiny::isolate(
          pending_dataset_intro_hydration()
        )

        if (
          is.null(current_pending) ||
          is.null(
            current_pending$token
          ) ||
          !identical(
            current_pending$token,
            scheduled_token
          )
        ) {
          return()
        }

        current_dataset_id <- shiny::isolate(
          state$current_dataset_id
        )

        if (
          is.null(current_dataset_id) ||
          length(current_dataset_id) == 0L ||
          is.na(current_dataset_id[1]) ||
          as.integer(
            current_dataset_id[1]
          ) !=
            as.integer(
              current_pending$dataset_id
            )
        ) {
          return()
        }

        hydration_base_path <-
          current_pending$base_path

        if (
          is.null(hydration_base_path) ||
          length(hydration_base_path) == 0L ||
          is.na(hydration_base_path[1]) ||
          !nzchar(
            trimws(
              as.character(
                hydration_base_path[1]
              )
            )
          )
        ) {
          pending_dataset_intro_hydration(
            NULL
          )

          return()
        }

        # This callback uses the non-reactive pond path stored
        # when the hydration request was queued. It therefore
        # does not access state$data_pond outside a consumer.
        hydration_choices <- function(
          field_code
        ) {
          get_choices_for_pond(
            field_code = field_code,
            base_path =
              hydration_base_path
          )
        }

        hydrate_dataset_overview_forms(
          session = session,
          rec =
            current_pending$record,
          citation_dt =
            citation_dt,
          about_dt =
            about_dt,
          get_choices_fn =
            hydration_choices
        )

        pending_dataset_intro_hydration(
          NULL
        )
      },
      once = TRUE
    )
  })

  output$citation_preview <- shiny::renderUI({
    shiny::tags$div(
      class =
        "suggested-citation",
      `data-citation-author-style` =
        citation_author_style(),

      suggested_citation_text()
    )
  })

  current_dataset_parent_dir <- reactive({
    parent <- input$dataset_parent_dir %||% state$dataset_parent_dir
    normalise_dataset_parent_dir(parent)
  })

  observeEvent(input$dataset_parent_dir, {
    sync_dataset_parent_dir(input$dataset_parent_dir)
  }, ignoreInit = TRUE)

  # Initialise the selectInput from the non-reactive initial value.
  session$onFlushed(function() {
    updateSelectInput(
      session,
      "dataset_parent_dir",
      selected = initial_dataset_parent_dir
    )
  }, once = TRUE)

  # observe username input
  active_users <- reactive({
    state$refresh_person
    req(state$data_pond)
    users <- get_users(state$data_pond)
    if (is.null(users) || nrow(users) == 0) {
      return(data.frame())
    }
    users
  })
  observe({
    users <- active_users()
    if (is.null(users) || nrow(users) == 0) {
      state$active_user_id <- NULL
      updateSelectInput(
        session,
        "active_user",
        choices = character(),
        selected = character(0)
      )
      return()
    }
    choices <- setNames(
      as.character(users$id),
      paste(users$name_first, users$name_last)
    )
    selected_user <- state$active_user_id
    if (is.null(selected_user) ||
        !as.character(selected_user) %in% as.character(users$id)) {
      selected_user <- as.character(users$id[1])
      state$active_user_id <- as.integer(selected_user)
      save_config(
        state$data_pond,
        state$active_user_id,
        state$dataset_parent_dir
      )
    }
    updateSelectInput(
      session,
      "active_user",
      choices = choices,
      selected = selected_user
    )
  })

  observeEvent(input$repair_dataset_location, {
    dataset_id <- state$current_dataset_id
    base_path <- state$data_pond

    if (
      is.null(dataset_id) ||
      length(dataset_id) == 0 ||
      is.na(dataset_id[1])
    ) {
      showNotification(
        "Select the dataset first.",
        type = "warning"
      )
      return()
    }

    if (
      is.null(base_path) ||
      length(base_path) == 0 ||
      is.na(base_path[1]) ||
      !nzchar(trimws(base_path[1]))
    ) {
      showNotification(
        "No data pond is selected.",
        type = "error"
      )
      return()
    }

    current_path <- state$dataset_path

    if (
      is.null(current_path) ||
      length(current_path) == 0 ||
      is.na(current_path[1]) ||
      !nzchar(trimws(current_path[1]))
    ) {
      showNotification(
        "The selected dataset has no stored dataset path.",
        type = "error"
      )
      return()
    }

    folder_name <- basename(
      normalizePath(
        current_path[1],
        mustWork = FALSE,
        winslash = "/"
      )
    )

    new_dataset_path <- file.path(
      base_path,
      "dev",
      folder_name
    )

    new_dataset_path <- normalizePath(
      new_dataset_path,
      mustWork = FALSE,
      winslash = "/"
    )

    new_data_path <- file.path(
      new_dataset_path,
      "data"
    )

    if (!dir.exists(new_dataset_path)) {
      showNotification(
        paste(
          "The expected folder does not exist:",
          new_dataset_path
        ),
        type = "error",
        duration = NULL
      )
      return()
    }

    if (!dir.exists(new_data_path)) {
      showNotification(
        paste(
          "The expected data subfolder does not exist:",
          new_data_path
        ),
        type = "error",
        duration = NULL
      )
      return()
    }

    tryCatch(
      {
        update_dataset_folder_paths(
          dataset_id = dataset_id,
          base_path = base_path,
          dataset_path = new_dataset_path,
          data_path = new_data_path
        )

        repair_cached_absolute_paths_after_dataset_move(
          dataset_id = dataset_id,
          base_path = base_path,
          dataset_path = new_dataset_path,
          data_path = new_data_path
        )

        # Update the active Shiny session immediately.
        state$dataset_path <- new_dataset_path
        state$data_path <- new_data_path

        showNotification(
          paste(
            "Dataset location repaired:",
            new_dataset_path
          ),
          type = "message",
          duration = 8
        )
      },
      error = function(e) {
        showNotification(
          paste(
            "Dataset location repair failed:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  observeEvent(input$active_user, {
    if (is.null(input$active_user) ||
        length(input$active_user) == 0 ||
        input$active_user == "") {
      state$active_user_id <- NULL
      save_config(state$data_pond, NULL, state$dataset_parent_dir)
      updateSelectInput(
        session,
        "metadata_reviewer_user",
        selected = character(0)
      )
      return()
    }
    state$active_user_id <- as.integer(input$active_user)
    save_config(
      state$data_pond,
      state$active_user_id,
      state$dataset_parent_dir
    )
    updateSelectInput(
      session,
      "metadata_reviewer_user",
      selected = as.character(state$active_user_id)
    )
  })
  output$dataset_parent_dir_display <- renderText({
    req(state$data_pond)
    file.path(state$data_pond, current_dataset_parent_dir())
  })
  output$metadata_reviewer_display <- renderText({
    users <- active_users()

    if (is.null(users) || nrow(users) == 0) {
      return("No metadata reviewer selected.")
    }

    selected_reviewer_user_id <-
      input$metadata_reviewer_user

    if (
      is.null(selected_reviewer_user_id) ||
      length(selected_reviewer_user_id) == 0 ||
      selected_reviewer_user_id == ""
    ) {
      selected_reviewer_user_id <-
        state$active_user_id
    }

    if (
      is.null(selected_reviewer_user_id) ||
      length(selected_reviewer_user_id) == 0 ||
      is.na(selected_reviewer_user_id)
    ) {
      return("No metadata reviewer selected.")
    }

    reviewer <- users[
      as.character(users$id) ==
        as.character(selected_reviewer_user_id),
      ,
      drop = FALSE
    ]

    if (nrow(reviewer) == 0) {
      return("No metadata reviewer selected.")
    }

    paste(
      reviewer$name_first[1],
      reviewer$name_last[1]
    )
  })

metadata_review_context_data <-
  shiny::reactive({
    dataset_id <-
      state$current_dataset_id

    if (
      is.null(dataset_id) ||
      length(dataset_id) == 0L ||
      is.na(dataset_id[1])
    ) {
      has_new_folder <- (
        !is.null(
          state$current_dataset_folder
        ) &&
          length(
            state$current_dataset_folder
          ) > 0L &&
          !is.na(
            state$current_dataset_folder[1]
          ) &&
          nzchar(
            trimws(
              as.character(
                state$current_dataset_folder[1]
              )
            )
          )
      )

      if (isTRUE(
        has_new_folder
      )) {
        return(
          metadata_review_defaults(
            new_record = TRUE
          )
        )
      }

      return(
        list(
          mode = "none",
          lifecycle_status = "",
          stage = "",
          version = ""
        )
      )
    }

    record <- get_dataset_record(
      dataset_id =
        as.integer(
          dataset_id[1]
        ),
      base_path =
        state$data_pond
    )

    metadata_review_defaults(
      record = record,
      new_record = FALSE
    )
  })


  output$metadata_review_context <-
    shiny::renderUI({
      context <-
        metadata_review_context_data()

      earth <-
        app_earth_palette()

      appearance <- switch(
        metadata_review_scalar(
          context$mode,
          default = "none"
        ),

        new = list(
          background =
            unname(
              earth[["parchment"]]
            ),
          border =
            unname(
              earth[["tan"]]
            ),
          title =
            "New metadata record",
          text = paste(
            "The initial review record will remain in draft",
            "until the dataset metadata is ready for review."
          )
        ),

        development = list(
          background =
            unname(
              earth[["pale_sage"]]
            ),
          border =
            unname(
              earth[["moss"]]
            ),
          title =
            "Pre-release metadata development",
          text = paste(
            "This is a draft dataset in dev/ with a version below 1.0.",
            "Edits default to metadata development rather than correction."
          )
        ),

        revision = list(
          background =
            unname(
              earth[["sand"]]
            ),
          border =
            unname(
              earth[["ochre"]]
            ),
          title =
            "Established metadata revision",
          text = paste(
            "This dataset does not meet the pre-release development rule.",
            "Edits default to metadata revision; choose correction only",
            "when fixing an actual error."
          )
        ),

        list(
          background =
            unname(
              earth[["stone"]]
            ),
          border =
            unname(
              earth[["bark"]]
            ),
          title =
            "No dataset selected",
          text =
            "Select a dataset or begin a new record."
        )
      )

      version_label <-
        metadata_review_scalar(
          context$version,
          default = "Not recorded"
        )

      stage_label <- switch(
        metadata_review_scalar(
          context$stage
        ),
        src = "src/",
        dev = "dev/",
        arc = "arc/",
        "Not recorded"
      )

      lifecycle_label <-
        metadata_review_scalar(
          context$lifecycle_status,
          default = "Not recorded"
        )

      shiny::tags$div(
        style = paste(
          "padding:9px 10px;",

          paste0(
            "background:",
            appearance$background,
            ";"
          ),

          paste0(
            "border:1px solid ",
            appearance$border,
            ";"
          ),

          "border-radius:3px;",
          "margin-bottom:12px;"
        ),

        shiny::strong(
          appearance$title
        ),

        shiny::tags$p(
          style =
            "margin:5px 0; line-height:1.4;",

          appearance$text
        ),

        if (!identical(
          context$mode,
          "none"
        )) {
          shiny::tags$small(
            paste0(
              "Lifecycle: ",
              humanise_code_value(
                lifecycle_label
              ),
              " | Pond location: ",
              stage_label,
              " | Version: ",
              version_label
            )
          )
        }
      )
    })

  output$current_user_display <- renderText({
    if (is.null(state$active_user_id)) return("No user selected.")
    users <- active_users()
    user <- users[
      as.character(users$id) == as.character(state$active_user_id),
      ,
      drop = FALSE
    ]
    if (nrow(user) == 0) return("No user selected.")
    paste(
      user$name_first[1],
      user$name_last[1],
      "--",
      user$role_title[1] %||% "",
      ifelse(isTRUE(user$is_admin[1] == 1), "(Admin)", "")
    )
  })
  observe({
    users <- active_users()
    if (is.null(users) || nrow(users) == 0) {
      updateSelectInput(
        session,
        "metadata_reviewer_user",
        choices = character(),
        selected = character(0)
      )
      return()
    }
    choices <- setNames(
      as.character(users$id),
      paste(users$name_first, users$name_last)
    )
    current_reviewer <- input$metadata_reviewer_user
    active_user <- state$active_user_id
    selected_reviewer <- character(0)
    # Prefer current reviewer if still valid.
    if (!is.null(current_reviewer) &&
        length(current_reviewer) > 0 &&
        current_reviewer != "" &&
        as.character(current_reviewer) %in% as.character(users$id)) {
      selected_reviewer <- current_reviewer
    # Otherwise default to active config user if valid.
    } else if (!is.null(active_user) &&
              length(active_user) > 0 &&
              !is.na(active_user) &&
              as.character(active_user) %in% as.character(users$id)) {
      selected_reviewer <- as.character(active_user)
    }
    updateSelectInput(
      session,
      "metadata_reviewer_user",
      choices = choices,
      selected = selected_reviewer
    )
  })
  observe({
    parent <- normalise_dataset_parent_dir(state$dataset_parent_dir)
    if (is.null(input$dataset_parent_dir) ||
        length(input$dataset_parent_dir) == 0 ||
        input$dataset_parent_dir != parent) {
      updateSelectInput(
        session,
        "dataset_parent_dir",
        selected = parent
      )
    }
  })

  observeEvent(
    eventExpr = list(
      state$data_pond,
      state$refresh_vocab
    ),
    handlerExpr = {
      req(state$data_pond)

      fields <- c(
        "keyword_object",
        "keyword_discipline",
        "keyword_topic",
        "keyword_geo"
      )

      for (field in fields) {
        approved <- get_keywords(
          field_code = field,
          base_path = state$data_pond
        )

        # Preserve the current selection without creating a reactive
        # dependency on the input that this observer updates.
        selected <- isolate(
          input[[field]]
        ) %||% character()

        merged <- merge_keyword_choices(
          choices = approved,
          selected = selected
        )

        freezeReactiveValue(
          input,
          field
        )

        updateSelectizeInput(
          session = session,
          inputId = field,
          choices = merged$choices,
          selected = merged$selected,
          server = TRUE
        )
      }
    },
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  # Data pond location selection ----
  # for selection of data pond location
  roots <- c(home = "~")
  shinyFiles::shinyDirChoose(
    input,
    "data_pond_input",
    roots = roots,
    session = session
  )
  observeEvent(input$set_manual, {
    req(input$manual_path)
    tryCatch(
      switch_data_pond(input$manual_path),
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error"
        )
      }
    )
  })
  observeEvent(input$data_pond_input, {
    req(input$data_pond_input)
    path <- shinyFiles::parseDirPath(
      roots,
      input$data_pond_input
    )
    if (length(path) == 0) {
      return()
    }
    tryCatch(
      switch_data_pond(path[1]),
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error"
        )
      }
    )
  })
  output$pond_structure <- shiny::renderText({
    base_path <- state$data_pond

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
        "No active data pond."
      )
    }

    base_path <- normalizePath(
      path.expand(
        as.character(
          base_path[1]
        )
      ),
      mustWork = FALSE,
      winslash = "/"
    )

    expected_dirs <- c(
      "src",
      "dev",
      "arc"
    )

    status <- vapply(
      expected_dirs,
      function(folder) {
        folder_path <- file.path(
          base_path,
          folder
        )

        paste0(
          folder,
          "/: ",
          if (dir.exists(folder_path)) {
            "available"
          } else {
            "missing"
          }
        )
      },
      character(1)
    )

    paste(
      status,
      collapse = "\n"
    )
  })

  # observe the selected/new dataset folder
  output$current_dataset_folder <- renderText({
    if (!is.null(state$dataset_path) &&
        length(state$dataset_path) > 0 &&
        state$dataset_path != "") {
      return(state$dataset_path)
    }
    if (!is.null(state$current_dataset_folder) &&
        length(state$current_dataset_folder) > 0 &&
        state$current_dataset_folder != "") {
      return(state$current_dataset_folder)
    }
    "No active dataset folder yet."
  })
  # observe show/hide the config sidepanel
  observeEvent(input$toggle_config, {
    session$sendCustomMessage(
      type = "toggleConfig",
      list()
    )
  })

  output$current_path <- renderText({
    state$data_pond
  })
  observe({
    cfg <- state$pond_config
    if (!is.null(cfg) && !is.null(cfg$attribute_profile)) {
      updateCheckboxInput(
        session,
        "attribute_profile_enabled",
        value = isTRUE(cfg$attribute_profile$enabled)
      )
    }
  })
  observeEvent(input$attribute_profile_enabled, {
    req(state$data_pond)
    save_config(
      state$data_pond,
      state$active_user_id,
      state$dataset_parent_dir,
      attribute_profile_enabled = isTRUE(input$attribute_profile_enabled)
    )
    state$pond_config <- read_pond_config(state$data_pond)
  }, ignoreInit = TRUE)

  # Clear the current form and start a new record ----

  shiny::observeEvent(
    input$clear_dataset_form,
    {
      cancel_dataset_intro_hydration(
        reset_ui_readiness = FALSE
      )

      clear_current_dataset_context(
        state,
        refresh = FALSE
      )

      last_hydrated_dataset_id(
        NULL
      )

      clear_dataset_overview_forms(
        session = session,
        citation_dt = citation_dt,
        about_dt = about_dt
      )

      authors_mod$clear()
      poc_mod$clear()
      org_mod$clear()
      data_access_mod$clear()
      supporting_docs_mod$clear()
      lineage_mod$clear()
      data_files_mod$clear()
      licensing_mod$clear()

      reset_metadata_review_inputs(
        new_record = TRUE
      )

      freezeReactiveValue(
        input,
        "dataset_select"
      )

      updateSelectInput(
        session,
        "dataset_select",
        selected = ""
      )

      updateCheckboxInput(
        session,
        "use_organisation_as_author",
        value = TRUE
      )

      refresh_dataset_context()

      showNotification(
        paste(
          "Form cleared.",
          "You can now start a new dataset record."
        ),
        type = "message",
        duration = 4
      )
    }
  )

  output$citation_key_preview <- renderText({
    vals <- c(
      collect_inputs(input, citation_dt),
      collect_inputs(input, about_dt)
    )

    vals <- as.list(vals)
    org_df <- org_mod$orgs()

    tryCatch(
      generate_citation_key(
        vals,
        org_df,
        existing_key = state$current_citation_key
      ),
      error = function(e) {
        paste("Citation key preview unavailable:", conditionMessage(e))
      }
    )
  })
  output$folder_key_preview <- renderText({
    vals <- c(
      collect_inputs(
        input,
        citation_dt
      ),
      collect_inputs(
        input,
        about_dt
      )
    )

    vals <- as.list(vals)

    tryCatch(
      generate_folder_key(
        vals = vals,
        org_df = org_mod$orgs()
      ),
      error = function(e) {
        paste(
          "Folder key preview unavailable:",
          conditionMessage(e)
        )
      }
    )
  })
  observe({
    state$refresh_datasets
    req(state$data_pond)

    df <- get_datasets(state$data_pond)

    # Always include an explicit blank choice so Shiny does not
    # automatically select the first dataset.
    choices <- c(
      "-- No dataset selected --" = ""
    )

    if (!is.null(df) && nrow(df) > 0) {
      dataset_choices <- setNames(
        as.character(df$id),
        paste0(
          df$id,
          " - ",
          ifelse(
            is.na(df$title) | trimws(df$title) == "",
            "Untitled dataset",
            df$title
          )
        )
      )

      choices <- c(
        choices,
        dataset_choices
      )
    }

    selected_id <- state$current_dataset_id

    if (
      is.null(selected_id) ||
      length(selected_id) == 0 ||
      is.na(selected_id) ||
      !as.character(selected_id) %in% unname(choices)
    ) {
      selected_id <- ""
    } else {
      selected_id <- as.character(selected_id)
    }

    freezeReactiveValue(
      input,
      "dataset_select"
    )

    updateSelectInput(
      session,
      "dataset_select",
      choices = choices,
      selected = selected_id
    )
  })

  # Load selected dataset ----

  last_hydrated_dataset_id <-
    shiny::reactiveVal(
      NULL
    )


  shiny::observeEvent(
    input$dataset_select,
    {
      selected_value <-
        input$dataset_select

      # The blank option explicitly represents
      # new-record mode.
      if (
        is.null(selected_value) ||
        length(selected_value) == 0L ||
        identical(
          selected_value,
          ""
        )
      ) {
        return()
      }

      dataset_id <- suppressWarnings(
        as.integer(
          selected_value[1]
        )
      )

      if (
        length(dataset_id) == 0L ||
        is.na(dataset_id)
      ) {
        showNotification(
          "The selected dataset ID is invalid.",
          type = "error"
        )

        return()
      }

      # Ignore programmatic re-selection only after the same
      # dataset has already been hydrated successfully.
      current_hydrated_id <-
        last_hydrated_dataset_id()

      same_hydrated_dataset <- (
        !is.null(current_hydrated_id) &&
          length(current_hydrated_id) > 0L &&
          !is.na(current_hydrated_id[1]) &&
          identical(
            as.integer(
              current_hydrated_id[1]
            ),
            as.integer(
              dataset_id
            )
          ) &&
          !is.null(
            state$current_dataset_id
          ) &&
          length(
            state$current_dataset_id
          ) > 0L &&
          !is.na(
            state$current_dataset_id[1]
          ) &&
          identical(
            as.integer(
              state$current_dataset_id[1]
            ),
            as.integer(
              dataset_id
            )
          )
      )

      if (isTRUE(
        same_hydrated_dataset
      )) {
        return()
      }

      # Cancel any still-pending hydration belonging to
      # a previously selected dataset.
      cancel_dataset_intro_hydration(
        reset_ui_readiness = FALSE
      )

      ok <- load_selected_dataset_into_state(
        dataset_id = dataset_id,
        base_path = state$data_pond,
        state = state
      )

      if (!isTRUE(ok)) {
        showNotification(
          "Could not load the selected dataset record.",
          type = "warning"
        )

        return()
      }

      rec <- get_dataset_record(
        dataset_id = dataset_id,
        base_path = state$data_pond
      )

      if (
        is.null(rec) ||
        nrow(as.data.frame(rec)) == 0L
      ) {
        showNotification(
          "The selected dataset record was not found.",
          type = "warning"
        )

        return()
      }

      rec <- as.data.frame(
        rec,
        stringsAsFactors = FALSE
      )

      # Restore the saved citation-author mode.
      citation_author_type <-
        citation_text_value(
          rec$citation_author_type,
          default = "organisation"
        )

      citation_author_type <- tolower(
        citation_author_type
      )

      use_organisation <-
        !citation_author_type %in%
          c(
            "author",
            "authors",
            "person",
            "personal"
          )

      updateCheckboxInput(
        session,
        "use_organisation_as_author",
        value = use_organisation
      )

      # The Dataset Intro fields are created through renderUI().
      # Queue their values rather than sending updates before
      # those browser inputs exist.
      queue_dataset_intro_hydration(
        dataset_id = dataset_id,
        rec = rec,
        base_path = state$data_pond
      )

      # These modules have static UI and can be hydrated now.
      data_files_mod$load_creation_scale(
        rec
      )

      data_access_mod$load_record(
        rec
      )

      authors_mod$load(
        dataset_id
      )

      poc_mod$load(
        dataset_id
      )

      # Load organisations before licensing because licensing
      # may resolve the owning organisation as rights holder.
      org_mod$load(
        dataset_id
      )

      licensing_mod$load(
        dataset_id
      )

      reset_metadata_review_inputs(
        new_record = FALSE,
        record = rec
      )

      if (
        !is.null(
          state$active_user_id
        ) &&
        length(
          state$active_user_id
        ) > 0L &&
        !is.na(
          state$active_user_id[1]
        )
      ) {
        updateSelectInput(
          session,
          "metadata_reviewer_user",
          selected = as.character(
            state$active_user_id[1]
          )
        )
      }

      # Mark this as the active selection after the record and
      # linked modules have loaded. The Dataset Intro hydration
      # remains queued until its renderUI inputs are ready.
      last_hydrated_dataset_id(
        dataset_id
      )

      showNotification(
        paste(
          "Current dataset set to ID",
          dataset_id,
          "- forms and linked sections loaded."
        ),
        type = "message",
        duration = 4,
        id = "dataset-loaded"
      )
    },
    ignoreInit = FALSE
  )

  observe({
  req(input$version_major, input$version_minor, input$version_patch)
    updateTextInput(
      session,
      "version",
      value = make_dataset_version(
        input$version_major,
        input$version_minor,
        input$version_patch
      )
    )
  })

  observeEvent(input$new_dataset, {
    vals <- c(
      collect_inputs(
        input,
        citation_dt
      ),
      collect_inputs(
        input,
        about_dt
      )
    )

    vals <- as.list(vals)

    temporal_validation <- validate_temporal_coverage(
      start_date = vals$temporal_coverage_start_date,
      end_date = vals$temporal_coverage_end_date
    )

    if (!isTRUE(temporal_validation$valid)) {
      showNotification(
        temporal_validation$message,
        type = "error"
      )
      return()
    }

    missing_cit <- validate_required(
      citation_dt,
      input
    )

    missing_abt <- validate_required(
      about_dt,
      input
    )

    missing_all <- c(
      missing_cit,
      missing_abt
    )

    if (length(missing_all) > 0L) {
      showNotification(
        paste(
          "Please complete required fields before creating a dataset folder:",
          paste(missing_all, collapse = ", ")
        ),
        type = "warning"
      )
      return()
    }

    if (!validate_version(vals$version)) {
      showNotification(
        paste(
          "Version must use major.minor.patch format,",
          "for example 0.1.0, 1.0.0 or 1.1.0."
        ),
        type = "error"
      )
      return()
    }

    # Validation passed. The app can now enter new-record mode.
    last_hydrated_dataset_id(NULL)

    freezeReactiveValue(
      input,
      "dataset_select"
    )

    updateSelectInput(
      session,
      "dataset_select",
      selected = ""
    )

    # This action starts a new dataset identity.
    state$current_dataset_id <- NULL
    state$current_dataset_uuid <- NULL
    state$current_citation_key <- NULL
    state$current_folder_key <- NULL

    org_df <- org_mod$orgs()
    citation_key <- generate_citation_key(
      vals,
      org_df,
      existing_key = NULL,
      regenerate_random = TRUE
    )
    folder_key <- generate_folder_key(
      vals = vals,
      org_df = org_df
    )
    uuid <- generate_unique_dataset_uuid(base_path = state$data_pond)

    state$current_citation_key <- citation_key
    state$current_dataset_uuid <- uuid
    state$current_folder_key <- folder_key
    parent_dir <- sync_dataset_parent_dir(input$dataset_parent_dir)
    folder_info <- tryCatch(
      create_dataset_folder_structure(
        base_path = state$data_pond,
        folder_key = folder_key,
        uuid = state$current_dataset_uuid,
        parent_dir = parent_dir,
        subdirs = c("data", "docs", "scripts")
      ),
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error"
        )
        return(NULL)
      }
    )
    if (is.null(folder_info)) {
      return()
    }
    # This is a new unsaved dataset record.
    state$current_dataset_id <- NULL
    state$current_dataset_folder <- folder_info$dataset_path
    state$dataset_path <- folder_info$dataset_path
    state$data_path <- folder_info$data_path

    # A newly created dataset cannot inherit these values from
    # the previously selected dataset.
    supporting_docs_mod$clear()
    lineage_mod$clear()
    data_files_mod$clear()

    # The form and relationship modules belong to this new dataset.
    # Do not clear them here.
    refresh_dataset_context(
      datasets = TRUE,
      reviews = TRUE,
      lineage = TRUE,
      supporting_docs = TRUE,
      data_files = TRUE,
      attributes = TRUE,
      spatial_metadata = TRUE
    )
    if (!is.null(state$active_user_id) &&
        length(state$active_user_id) > 0 &&
        !is.na(state$active_user_id)) {
      updateSelectInput(
        session,
        "metadata_reviewer_user",
        selected = as.character(state$active_user_id)
      )
    }

    reset_metadata_review_inputs(
      new_record = TRUE
    )

    shiny::updateTextAreaInput(
      session,
      "metadata_update_summary",
      value = paste0(
        "Dataset folder created under ",
        parent_dir,
        " at: ",
        folder_info$dataset_path
      )
    )
  })

  # save input ----
  # save dataset ----
  observeEvent(input$save, {
    citation_dt <- get_section(
      schema,
      "Dataset overview",
      "Citation"
    )

    about_dt <- get_section(
      schema,
      "Dataset overview",
      "About"
    )

    access_validation <- data_access_mod$validate(
      strict = FALSE
    )

    if (!isTRUE(access_validation$valid)) {
      showNotification(
        access_validation$message,
        type = "error"
      )

      return()
    }

    sharing_validation <-
      licensing_mod$validate(
        strict = FALSE
      )

    if (!isTRUE(
      sharing_validation$valid
    )) {
      showNotification(
        sharing_validation$message,
        type = "error",
        duration = 8
      )

      return()
    }

    scale_validation <- data_files_mod$creation_scale_validation()

    if (!isTRUE(scale_validation$valid)) {
      showNotification(
        scale_validation$message,
        type = "error",
        duration = 8
      )
      return()
    }

    vals <- c(
      collect_inputs(
        input,
        citation_dt
      ),

      collect_inputs(
        input,
        about_dt
      ),

      data_access_mod$values(),
      data_files_mod$creation_scale_values()
    )

    vals <- as.list(vals)

    temporal_validation <- validate_temporal_coverage(
      start_date = vals$temporal_coverage_start_date,
      end_date = vals$temporal_coverage_end_date
    )

    if (!isTRUE(temporal_validation$valid)) {
      showNotification(temporal_validation$message, type = "error")
      return()
    }

    # generate uuid ----
    authors_df <- authors_mod$authors()
    org_df     <- org_mod$orgs()

    citation_style <- citation_author_style()

    # Persist the selected citation-author mode in entry_tbl.
    vals$citation_author_type <- citation_style

    citation_txt <- generate_citation(
      vals = vals,
      authors_df = authors_df,
      org_df = org_df,
      style = citation_style
    )
    # parse doi for the citation
    if (!is.null(vals$doi) && vals$doi != "") {
      citation_txt <- paste0(citation_txt, " ", vals$doi)
    }
    vals$citation <- citation_txt
    if (!validate_version(vals$version)) {
      showNotification(
        "Version must use major.minor.patch format, e.g. 0.1.0, 1.0.0, 1.1.0.",
        type = "error"
      )
      return()
    }

    # check that all values have been filled in
    missing_cit <- validate_required(citation_dt, input)
    missing_abt <- validate_required(about_dt, input)
    missing_all <- c(missing_cit, missing_abt)
    if (length(missing_all) > 0) {
      showNotification(
        paste("Missing required fields:", paste(missing_all, collapse = ", ")),
        type = "error"
      )
      return()
    }
    if (is.null(input$metadata_reviewer_user) ||
        length(input$metadata_reviewer_user) == 0 ||
        input$metadata_reviewer_user == ""
    ) {
      showNotification(
        "Please select a metadata reviewer/editor.",
        type = "error"
      )
      return()
    }

    # Determine whether this save creates a new dataset record.
    is_new_dataset <- (
      is.null(
        state$current_dataset_id
      ) ||
        length(
          state$current_dataset_id
        ) == 0L ||
        is.na(
          state$current_dataset_id[1]
        )
    )


    review_record <- if (
      isTRUE(
        is_new_dataset
      )
    ) {
      NULL
    } else {
      get_dataset_record(
        dataset_id =
          as.integer(
            state$current_dataset_id[1]
          ),
        base_path =
          state$data_pond
      )
    }


    review_defaults <-
      metadata_review_defaults(
        record =
          review_record,
        new_record =
          is_new_dataset
      )


    metadata_update_type_to_save <-
      input$metadata_update_type

    if (
      is.null(
        metadata_update_type_to_save
      ) ||
        length(
          metadata_update_type_to_save
        ) == 0L
    ) {
      metadata_update_type_to_save <-
        review_defaults$update_type
    }


    metadata_section_to_save <-
      input$metadata_section

    if (
      is.null(
        metadata_section_to_save
      ) ||
        length(
          metadata_section_to_save
        ) == 0L
    ) {
      metadata_section_to_save <-
        review_defaults$sections
    }


    metadata_update_reason_to_save <-
      input$metadata_update_reason

    if (
      is.null(
        metadata_update_reason_to_save
      ) ||
        length(
          metadata_update_reason_to_save
        ) == 0L ||
        is.na(
          metadata_update_reason_to_save[1]
        ) ||
        !nzchar(
          trimws(
            as.character(
              metadata_update_reason_to_save[1]
            )
          )
        )
    ) {
      metadata_update_reason_to_save <-
        review_defaults$reason
    }


    metadata_update_summary_to_save <-
      input$metadata_update_summary


    metadata_status_to_save <-
      input$metadata_status

    if (
      is.null(
        metadata_status_to_save
      ) ||
        length(
          metadata_status_to_save
        ) == 0L ||
        is.na(
          metadata_status_to_save[1]
        ) ||
        !nzchar(
          trimws(
            as.character(
              metadata_status_to_save[1]
            )
          )
        )
    ) {
      metadata_status_to_save <-
        review_defaults$metadata_status
    }

    if (isTRUE(is_new_dataset)) {
      if (is.null(metadata_update_type_to_save) ||
          length(metadata_update_type_to_save) == 0) {
        metadata_update_type_to_save <- c("new_metadata_record")
      }

      if (is.null(metadata_section_to_save) ||
          length(metadata_section_to_save) == 0) {
        metadata_section_to_save <- c("dataset_overview", "citation", "about")
      }

      if (is.null(metadata_update_reason_to_save) ||
          length(metadata_update_reason_to_save) == 0 ||
          trimws(metadata_update_reason_to_save) == "") {
        metadata_update_reason_to_save <- "Initial metadata record created."
      }

      if (is.null(metadata_status_to_save) ||
          length(metadata_status_to_save) == 0 ||
          metadata_status_to_save == "") {
        metadata_status_to_save <- "draft"
      }
    }

    
    # Metadata review validation
    if (is.null(metadata_update_type_to_save) ||
        length(metadata_update_type_to_save) == 0) {
      showNotification(
        "Please select at least one metadata update type.",
        type = "error"
      )
      return()
    }

    if (is.null(metadata_section_to_save) ||
        length(metadata_section_to_save) == 0) {
      showNotification(
        "Please select at least one metadata section affected.",
        type = "error"
      )
      return()
    }

    if (is.null(metadata_update_reason_to_save) ||
        length(metadata_update_reason_to_save) == 0 ||
        trimws(metadata_update_reason_to_save) == "") {
      showNotification(
        "Please provide a reason for the metadata update before saving.",
        type = "error"
      )
      return()
    }

    # Resolve and validate the metadata reviewer before making changes.
    users <- active_users()

    selected_reviewer_user_id <-
      input$metadata_reviewer_user %||%
      state$active_user_id

    if (
      is.null(selected_reviewer_user_id) ||
      length(selected_reviewer_user_id) == 0L ||
      is.na(selected_reviewer_user_id[1]) ||
      !nzchar(trimws(as.character(selected_reviewer_user_id[1])))
    ) {
      showNotification(
        "Please select a metadata reviewer/editor.",
        type = "error"
      )
      return()
    }

    reviewer <- users[
      as.character(users$id) ==
        as.character(selected_reviewer_user_id[1]),
      ,
      drop = FALSE
    ]

    if (nrow(reviewer) == 0L) {
      showNotification(
        "The selected metadata reviewer could not be found.",
        type = "error"
      )
      return()
    }

    reviewer_person_id <- as.integer(
      reviewer$person_id[1]
    )

    reviewer_name <- paste(
      reviewer$name_first[1],
      reviewer$name_last[1]
    )

    # clean data values before insert
    for (n in names(vals)) {
      val <- vals[[n]]
      # preserve keyword multi-values
      if (grepl("^keyword_", n)) {
        vals[[n]] <- paste(val, collapse = "; ")
        next
      }
      # handle empty / NULL / length 0
      if (length(val) == 0 || is.null(val)) {
        vals[[n]] <- NA
        next
      }
      # enforce scalar (take first if vector)
      if (length(val) > 1) {
        val <- val[1]
      }
      # handle Date
      if (inherits(val, "Date")) {
        vals[[n]] <- format(val, "%Y-%m-%d")
        next
      }
      # handle character
      if (is.character(val)) {
        vals[[n]] <- val
        next
      }
      # fallback (numeric, logical, etc.)
      vals[[n]] <- as.character(val)
    }

    # UUID handling ----
    # New dataset:
    #   create a UUID once and keep it.
    # Existing dataset:
    #   preserve the existing UUID; never regenerate it during Save Dataset.
    if (isTRUE(is_new_dataset)) {
      if (is.null(state$current_dataset_uuid) ||
          length(state$current_dataset_uuid) == 0 ||
          is.na(state$current_dataset_uuid) ||
          state$current_dataset_uuid == "") {
        state$current_dataset_uuid <- generate_dataset_uuid()
      }

      vals$uuid <- state$current_dataset_uuid

      # If user clicked Save without first clicking New Dataset,
      # create the dataset folder automatically.
      if (is.null(state$current_dataset_folder) ||
          length(state$current_dataset_folder) == 0 ||
          is.na(state$current_dataset_folder) ||
          state$current_dataset_folder == "" ||
          !dir.exists(state$current_dataset_folder)) {

        citation_key <- generate_citation_key(
          vals,
          org_mod$orgs(),
          existing_key = state$current_citation_key
        )

        folder_key <- generate_folder_key(
          vals = vals,
          org_df = org_mod$orgs()
        )

        state$current_citation_key <- citation_key
        state$current_folder_key <- folder_key

        parent_dir <- sync_dataset_parent_dir(input$dataset_parent_dir)

        folder_info <- create_dataset_folder_structure(
          base_path = state$data_pond,
          folder_key = folder_key,
          uuid = state$current_dataset_uuid,
          parent_dir = parent_dir,
          subdirs = c("data", "docs", "scripts")
        )

        state$current_dataset_folder <- folder_info$dataset_path
        state$dataset_path <- folder_info$dataset_path
        state$data_path <- folder_info$data_path
      }

    } else {
      existing_uuid <- state$current_dataset_uuid

      if (is.null(existing_uuid) ||
          length(existing_uuid) == 0 ||
          is.na(existing_uuid) ||
          existing_uuid == "") {
        existing_uuid <- get_or_create_dataset_uuid(
          state$current_dataset_id,
          state$data_pond
        )
      }

      vals$uuid <- existing_uuid
      state$current_dataset_uuid <- existing_uuid
    }
    vals$dataset_path <- state$dataset_path %||% NA_character_
    vals$data_path <- state$data_path %||% NA_character_
    vals$data_pond_id <- state$data_pond_id %||% ensure_data_pond_id(state$data_pond)
    vals$citation_key <- generate_citation_key(
      vals,
      org_mod$orgs(),
      existing_key =
        state$current_citation_key
    )

    generated_folder_key <-
      generate_folder_key(
        vals = vals,
        org_df = org_mod$orgs()
      )

    # For a newly created but not-yet-saved folder,
    # retain the exact key used for physical folder creation.
    if (
      isTRUE(is_new_dataset) &&
      !is.null(state$current_folder_key) &&
      length(state$current_folder_key) > 0 &&
      !is.na(state$current_folder_key[1]) &&
      nzchar(trimws(as.character(state$current_folder_key[1])))
    ) {
      vals$folder_key <- as.character(state$current_folder_key[1])
    } else {
      # For existing draft records, metadata edits may update
      # the proposed folder key. Renaming remains explicit.
      vals$folder_key <- generated_folder_key

      state$current_folder_key <- generated_folder_key
    }

    state$current_citation_key <- vals$citation_key
    if (isTRUE(is_new_dataset)) {
      vals$lifecycle_status <- vals$lifecycle_status %||% "draft"
      vals$folder_locked <- vals$folder_locked %||% 0L
      vals$source_dataset_id <- vals$source_dataset_id %||% NA_integer_
    }

    conn <- get_conn(
      state$data_pond
    )

    ensure_columns(
      conn = conn,
      table = "entry_tbl",
      fields = names(vals)
    )

    DBI::dbDisconnect(conn)

    dataset_id <- save_entry_dynamic(
      table = "entry_tbl",
      data = vals,
      base_path = state$data_pond,
      id = state$current_dataset_id
    )

    pending_added <- register_pending_keyword_values(
      values = vals,
      base_path = state$data_pond,
      user_id = state$active_user_id
    )

    if (pending_added > 0L) {
      state$refresh_vocab <- state$refresh_vocab + 1L
    }

    state$current_dataset_id <- dataset_id
    state$current_folder_key <- vals$folder_key
    save_dataset_relationship_sections(dataset_id)

    sharing_saved <- tryCatch(
      {
        licensing_mod$save(
          dataset_id = dataset_id,
          strict = FALSE
        )

        TRUE
      },
      error = function(e) {
        showNotification(
          paste(
            "The dataset record was saved, but the",
            "sharing assessment could not be saved:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )

        FALSE
      }
    )

    if (!isTRUE(sharing_saved)) {
      return()
    }

    if (
      !is.null(metadata_update_reason_to_save) &&
      length(metadata_update_reason_to_save) > 0 &&
      !is.na(metadata_update_reason_to_save[1]) &&
      trimws(metadata_update_reason_to_save[1]) != ""
    ) {
      insert_metadata_review(
        dataset_id = dataset_id,
        base_path = state$data_pond,
        reviewer_id = reviewer_person_id,
        reviewer_name = reviewer_name,
        reviewer_role = NA_character_,
        update_type = metadata_update_type_to_save,
        update_reason = metadata_update_reason_to_save,
        update_summary = metadata_update_summary_to_save %||% NA_character_,
        metadata_section = metadata_section_to_save,
        metadata_status = metadata_status_to_save,
        source_of_change = "manual_dataset_save",
        app_version = "1.0.20",
        notes = NA_character_
      )
    }
    # trigger refresh
    refresh_dataset_context()
    showNotification("Saved successfully", type = "message")
  })

  observeEvent(input$open_dataset_folder, {
    folder_path <- state$dataset_path %||% state$current_dataset_folder
    tryCatch(
      {
        open_folder(folder_path)
        showNotification(
          "Dataset folder opened.",
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "warning"
        )
      }
    )
  })
  observeEvent(input$open_data_folder, {
    data_path <- state$data_path

    if (is.null(data_path) ||
        length(data_path) == 0 ||
        is.na(data_path) ||
        data_path == "") {

      dataset_path <- state$dataset_path %||% state$current_dataset_folder

      if (!is.null(dataset_path) &&
          length(dataset_path) > 0 &&
          !is.na(dataset_path) &&
          dataset_path != "") {
        data_path <- file.path(dataset_path, "data")
      }
    }

    tryCatch(
      {
        open_folder(data_path)
        showNotification(
          "Data folder opened.",
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "warning"
        )
      }
    )
  })

  pond_overview_data <- shiny::reactive({
    state$refresh_datasets
    state$refresh_data_files_saved
    state$refresh_attributes
    state$refresh_spatial_metadata
    state$refresh_lineage
    state$refresh_supporting_docs

    base_path <- state$data_pond

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
        data.table::data.table()
      )
    }

    build_pond_overview_data(
      base_path = base_path,
      schema_grouped = schema
    )
  })

  output$dataset_table <- DT::renderDT({
    overview <- pond_overview_data()

    if (
      is.null(overview) ||
      nrow(as.data.frame(overview)) == 0L
    ) {
      return(
        clean_dt_widget(
          data.frame(
            message =
              "No dataset records are available."
          ),
          buttons = FALSE,
          filter = "none"
        )
      )
    }

    display <- overview[
      ,
      .(
        `Dataset ID` = id,
        `Overview class` =
          overview_class,
        Title =
          display_title,
        Authors =
          authors,
        `Pond location` =
          pond_location,
        `Lifecycle status` =
          lifecycle_label,
        `Metadata readiness` =
          metadata_readiness,
        `Next action` =
          next_action,
        `Spatial extent` =
          spatial_extent,
        Version =
          version,
        `Citation date` =
          citation_date,
        `Dataset folder` =
          dataset_path
      )
    ]

    table_widget <- DT::datatable(
      as.data.frame(
        display,
        stringsAsFactors = FALSE
      ),
      rownames = FALSE,
      selection = "single",
      filter = "top",
      escape = TRUE,
      extensions = "Buttons",
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
          list(
            visible = FALSE,
            targets = c(
              0L,
              1L
            )
          ),
          list(
            width = "260px",
            targets = 2L
          ),
          list(
            width = "210px",
            targets = 3L
          ),
          list(
            width = "150px",
            targets = 4L
          ),
          list(
            width = "145px",
            targets = 5L
          ),
          list(
            width = "190px",
            targets = 6L
          ),
          list(
            width = "220px",
            targets = 7L
          ),
          list(
            width = "155px",
            targets = 8L
          ),
          list(
            width = "340px",
            targets = 11L
          )
        )
      )
    )

    palette <- pond_overview_palette()

    table_widget <- DT::formatStyle(
      table_widget,
      columns = "Overview class",
      target = "row",
      backgroundColor =
        DT::styleEqual(
          names(palette),
          unname(palette)
        ),
      verticalAlign = "top"
    )

    table_widget <- DT::formatStyle(
      table_widget,
      columns = c(
        "Title",
        "Authors",
        "Next action",
        "Dataset folder"
      ),
      whiteSpace = "normal",
      wordBreak = "break-word",
      lineHeight = "1.35"
    )

    table_widget
  })

  dataset_table_proxy <- DT::dataTableProxy("dataset_table")


  shiny::observeEvent(
    input$dataset_table_rows_selected,
    {
      selected_row <-
        input$dataset_table_rows_selected

      if (
        is.null(selected_row) ||
        length(selected_row) != 1L
      ) {
        return()
      }

      overview <- pond_overview_data()

      if (
        is.null(overview) ||
        nrow(as.data.frame(overview)) <
          selected_row
      ) {
        return()
      }

      selected_dataset_id <-
        overview$id[selected_row]

      if (
        is.na(selected_dataset_id)
      ) {
        return()
      }

      shiny::updateSelectInput(
        session,
        "dataset_select",
        selected =
          as.character(
            selected_dataset_id
          )
      )
    }
  )

  shiny::observe({
    overview <- pond_overview_data()
    selected_id <- state$current_dataset_id

    if (
      is.null(selected_id) ||
      length(selected_id) == 0L ||
      is.na(selected_id[1]) ||
      is.null(overview) ||
      nrow(as.data.frame(overview)) == 0L
    ) {
      DT::selectRows(
        dataset_table_proxy,
        NULL
      )

      return()
    }

    selected_row <- which(
      as.integer(overview$id) ==
        as.integer(selected_id[1])
    )

    if (length(selected_row) == 0L) {
      DT::selectRows(
        dataset_table_proxy,
        NULL
      )
    } else {
      DT::selectRows(
        dataset_table_proxy,
        selected_row[1]
      )
    }
  })

  output$pond_overview_summary <- shiny::renderUI({
    overview <- pond_overview_data()

    if (
      is.null(overview) ||
      nrow(as.data.frame(overview)) == 0L
    ) {
      return(
        shiny::tags$div(
          style = paste(
            "padding:10px;",
            "background:#F8F4EA;",
            "border:1px solid #ccc;"
          ),

          "No datasets are recorded in the active pond."
        )
      )
    }

    stat_row <- function(
      label,
      value,
      background
    ) {
      shiny::tags$div(
        style = paste(
          "padding:8px 10px;",
          paste0(
            "background:",
            background,
            ";"
          ),
          "border:1px solid #bbb;",
          "margin-bottom:7px;"
        ),

        shiny::strong(
          label
        ),

        shiny::tags$span(
          style = "float:right;",

          as.character(value)
        )
      )
    }

    shiny::tagList(
      stat_row(
        "Total datasets",
        nrow(overview),
        "#F8F4EA"
      ),

      stat_row(
        "Source \u2014 src/",
        sum(
          overview$stage == "src",
          na.rm = TRUE
        ),
        "#E9DFB5"
      ),

      stat_row(
        "Development \u2014 dev/",
        sum(
          overview$stage == "dev",
          na.rm = TRUE
        ),
        "#D1DDC7"
      ),

      stat_row(
        "Archive \u2014 arc/",
        sum(
          overview$stage == "arc",
          na.rm = TRUE
        ),
        "#B99572"
      ),

      stat_row(
        "Required metadata complete",
        sum(
          overview$ready_for_archive %in%
            TRUE,
          na.rm = TRUE
        ),
        "#A8C49A"
      ),

      stat_row(
        "Mapped datasets",
        sum(
          overview$bbox_available %in%
            TRUE,
          na.rm = TRUE
        ),
        "#F1E9DA"
      )
    )
  })

  output$pond_dataset_map <-
    leaflet::renderLeaflet({
      overview <- pond_overview_data()

      map <- leaflet::leaflet(
        options =
          leaflet::leafletOptions(
            zoomControl = TRUE,
            attributionControl = TRUE
          )
      )

      map <- leaflet::addProviderTiles(
        map,
        provider =
          "CartoDB.Positron"
      )

      if (
        is.null(overview) ||
        nrow(as.data.frame(overview)) == 0L
      ) {
        return(
          leaflet::setView(
            map,
            lng = 0,
            lat = 0,
            zoom = 1
          )
        )
      }

      mapped <- data.table::as.data.table(
        data.table::copy(overview)
      )

      # Retain only valid, non-zero EPSG:4326 extents.
      mapped <- mapped[
        bbox_available %in% TRUE &
          is.finite(west) &
          is.finite(south) &
          is.finite(east) &
          is.finite(north) &
          east > west &
          north > south
      ]

      if (nrow(mapped) == 0L) {
        return(
          leaflet::setView(
            map,
            lng = 0,
            lat = 0,
            zoom = 1
          )
        )
      }

      palette <-
        pond_overview_palette()

      escape_text <- function(x) {
        if (
          is.null(x) ||
          length(x) == 0L ||
          is.na(x[1])
        ) {
          return("")
        }

        htmltools::htmlEscape(
          as.character(
            x[1]
          )
        )
      }

      # ------------------------------------------------------------
      # Bounding-box size and selected-dataset status
      # ------------------------------------------------------------

      mapped[
        ,
        bbox_area_proxy :=
          (east - west) *
          (north - south)
      ]

      selected_id <-
        state$current_dataset_id

      selected_id_is_valid <- (
        !is.null(selected_id) &&
          length(selected_id) > 0L &&
          !is.na(selected_id[1])
      )

      if (isTRUE(
        selected_id_is_valid
      )) {
        mapped[
          ,
          is_selected :=
            as.integer(id) ==
              as.integer(
                selected_id[1]
              )
        ]
      } else {
        mapped[
          ,
          is_selected := FALSE
        ]
      }

      # Relative size thresholds are used only to assign panes
      # and display opacity.
      area_breaks <- stats::quantile(
        mapped$bbox_area_proxy,
        probs = c(
          0.33,
          0.67
        ),
        na.rm = TRUE,
        names = FALSE
      )

      mapped[
        ,
        bbox_size_class :=
          data.table::fcase(
            bbox_area_proxy >=
              area_breaks[2],
            "broad",

            bbox_area_proxy >=
              area_breaks[1],
            "medium",

            default = "narrow"
          )
      ]

      mapped[
        ,
        bbox_pane :=
          data.table::fcase(
            bbox_size_class ==
              "broad",
            "pond_bbox_broad",

            bbox_size_class ==
              "medium",
            "pond_bbox_medium",

            default =
              "pond_bbox_narrow"
          )
      ]

      mapped[
        ,
        rectangle_opacity :=
          data.table::fcase(
            bbox_size_class ==
              "broad",
            0.06,

            bbox_size_class ==
              "medium",
            0.13,

            default = 0.24
          )
      ]

      # Within each pane, large rectangles are still added first.
      # Smaller rectangles added later therefore remain above larger
      # rectangles that happen to share the same pane.
      data.table::setorder(
        mapped,
        -bbox_area_proxy,
        id
      )

      # ------------------------------------------------------------
      # Explicit Leaflet drawing panes
      # ------------------------------------------------------------

      map <- leaflet::addMapPane(
        map,
        name = "pond_bbox_broad",
        zIndex = 410
      )

      map <- leaflet::addMapPane(
        map,
        name = "pond_bbox_medium",
        zIndex = 420
      )

      map <- leaflet::addMapPane(
        map,
        name = "pond_bbox_narrow",
        zIndex = 430
      )

      # This pane contains only the non-interactive selected outline.
      map <- leaflet::addMapPane(
        map,
        name = "pond_bbox_selected",
        zIndex = 440
      )

      # ------------------------------------------------------------
      # Add interactive bounding-box rectangles
      # ------------------------------------------------------------

      for (i in seq_len(nrow(mapped))) {
        fill_colour <-
          unname(
            palette[
              mapped$overview_class[i]
            ]
          )

        if (
          length(fill_colour) == 0L ||
          is.na(fill_colour)
        ) {
          fill_colour <-
            unname(
              palette[
                "Unclassified location"
              ]
            )
        }

        popup <- htmltools::HTML(
          paste0(
            "<strong>",
            escape_text(
              mapped$display_title[i]
            ),
            "</strong><br>",

            "Pond location: ",
            escape_text(
              mapped$pond_location[i]
            ),
            "<br>",

            "Lifecycle status: ",
            escape_text(
              mapped$lifecycle_label[i]
            ),
            "<br>",

            "Metadata readiness: ",
            escape_text(
              mapped$metadata_readiness[i]
            ),
            "<br>",

            "Bounding box: ",
            escape_text(
              mapped$bbox_wgs84[i]
            )
          )
        )

        map <- leaflet::addRectangles(
          map,
          lng1 = mapped$west[i],
          lat1 = mapped$south[i],
          lng2 = mapped$east[i],
          lat2 = mapped$north[i],

          layerId =
            as.character(
              mapped$id[i]
            ),

          color = "#625C52",
          weight = 2,
          opacity = 0.75,

          fillColor =
            fill_colour,

          fillOpacity =
            mapped$rectangle_opacity[i],

          dashArray = if (
            identical(
              mapped$bbox_size_class[i],
              "broad"
            )
          ) {
            "6,4"
          } else {
            NULL
          },

          options =
            leaflet::pathOptions(
              pane =
                mapped$bbox_pane[i],
              interactive = TRUE
            ),

          # Do not dynamically reorder rectangles during hover.
          highlightOptions =
            leaflet::highlightOptions(
              weight = 4,
              color = "#526657",
              fillOpacity = 0.32,
              bringToFront = FALSE
            ),

          popup = popup,

          label =
            mapped$display_title[i]
        )
      }

      # ------------------------------------------------------------
      # Add the selected dataset as a non-interactive outline
      # ------------------------------------------------------------

      selected_extent <- mapped[
        is_selected %in% TRUE
      ]

      if (nrow(selected_extent) == 1L) {
        map <- leaflet::addRectangles(
          map,
          lng1 =
            selected_extent$west[1],
          lat1 =
            selected_extent$south[1],
          lng2 =
            selected_extent$east[1],
          lat2 =
            selected_extent$north[1],

          color = "#3F5146",
          weight = 5,
          opacity = 1,

          fillOpacity = 0,

          options =
            leaflet::pathOptions(
              pane =
                "pond_bbox_selected",
              interactive = FALSE,
              pointerEvents = "none"
            )
        )
      }

      # ------------------------------------------------------------
      # Legend and map extent
      # ------------------------------------------------------------

      classes_present <- names(palette)[
        names(palette) %in%
          unique(
            mapped$overview_class
          )
      ]

      if (length(classes_present) > 0L) {
        map <- leaflet::addLegend(
          map,
          position = "bottomright",
          colors =
            unname(
              palette[
                classes_present
              ]
            ),
          labels =
            classes_present,
          opacity = 0.85,
          title =
            "Dataset status"
        )
      }

      leaflet::fitBounds(
        map,
        lng1 = min(
          mapped$west,
          na.rm = TRUE
        ),
        lat1 = min(
          mapped$south,
          na.rm = TRUE
        ),
        lng2 = max(
          mapped$east,
          na.rm = TRUE
        ),
        lat2 = max(
          mapped$north,
          na.rm = TRUE
        )
      )
    })

  shiny::observeEvent(
    input$pond_dataset_map_shape_click,
    {
      click <-
        input$pond_dataset_map_shape_click

      if (
        is.null(click$id) ||
        length(click$id) == 0L ||
        is.na(click$id[1])
      ) {
        return()
      }

      selected_dataset_id <-
        suppressWarnings(
          as.integer(
            click$id[1]
          )
        )

      if (is.na(selected_dataset_id)) {
        return()
      }

      shiny::updateSelectInput(
        session,
        "dataset_select",
        selected =
          as.character(
            selected_dataset_id
          )
      )
    }
  )

  metadata_reviews <- shiny::reactive({
    state$refresh_metadata_reviews

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
            "No dataset selected."
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

    reviews <- get_metadata_reviews(
      dataset_id =
        as.integer(
          dataset_id[1]
        ),
      base_path =
        as.character(
          base_path[1]
        )
    )

    if (
      is.null(reviews) ||
      nrow(as.data.frame(reviews)) == 0L
    ) {
      return(
        data.table::data.table(
          message =
            "No metadata review or update records exist yet."
        )
      )
    }

    data.table::as.data.table(
      reviews
    )
  })


  metadata_reviews_display <-
    shiny::reactive({
      df <- metadata_reviews()

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
        id =
          NA_integer_,

        review_timestamp =
          NA_character_,

        reviewer_name =
          NA_character_,

        reviewer_role =
          NA_character_,

        update_type =
          NA_character_,

        metadata_section =
          NA_character_,

        metadata_status =
          "draft",

        update_reason =
          NA_character_,

        update_summary =
          NA_character_,

        source_of_change =
          NA_character_,

        notes =
          NA_character_
      )

      for (field_name in names(
        required_columns
      )) {
        if (!field_name %in% names(dt)) {
          dt[, (field_name) := required_columns[[field_name]]]
        }
      }

      dt[
        ,
        row_class :=
          tolower(
            trimws(
              as.character(
                metadata_status
              )
            )
          )
      ]

      dt[
        is.na(row_class) |
          !nzchar(row_class),
        row_class :=
          "draft"
      ]

      code_columns <- c(
        "update_type",
        "metadata_section",
        "metadata_status",
        "source_of_change"
      )

      for (field_name in code_columns) {
        dt[
          ,
          (field_name) :=
            vapply(
              as.character(
                get(field_name)
              ),
              humanise_code_value,
              character(1)
            )
        ]
      }

      dt[,
        .(
          row_class,
          `Review ID` =
            id,
          `Review date and time` =
            review_timestamp,
          Reviewer =
            reviewer_name,
          `Reviewer role` =
            reviewer_role,
          `Update type` =
            update_type,
          `Metadata section` =
            metadata_section,
          `Metadata status` =
            metadata_status,
          `Reason for update` =
            update_reason,
          `Update summary` =
            update_summary,
          `Source of change` =
            source_of_change,
          Notes =
            notes
        )
      ]
    })


  output$metadata_review_table <- DT::renderDT({
    display_df <- metadata_reviews_display()

    if (
      is.null(display_df) ||
      nrow(as.data.frame(display_df)) == 0L
    ) {
      return(
        DT::datatable(
          data.frame(
            Message =
              "No metadata review records are available."
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

    if ("message" %in% names(display_df)) {
      return(
        DT::datatable(
          data.frame(
            Message =
              as.character(
                display_df$message
              ),
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

    earth <- app_earth_palette()

    table_widget <- DT::datatable(
      as.data.frame(display_df, stringsAsFactors = FALSE),
      rownames = FALSE, selection = "none", filter = "top",
      escape = TRUE, extensions = "Buttons",
      options = list(
        pageLength = 10L,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        order = list(list(2L, "desc")),
        columnDefs = list(list(visible = FALSE, targets = c(0L, 1L)),
          list(width = "145px", targets = 2L),
          list(width = "140px", targets = 3L),
          list(
            width = "125px",
            targets = 4L
          ),
          list(
            width = "180px",
            targets = c(
              5L,
              6L
            )
          ),
          list(
            width = "135px",
            targets = 7L
          ),
          list(
            width = "320px",
            targets = 8L
          ),
          list(
            width = "360px",
            targets = 9L
          ),
          list(
            width = "160px",
            targets = 10L
          ),
          list(
            width = "300px",
            targets = 11L
          )
        )
      )
    )

    table_widget <- DT::formatStyle(
      table_widget,
      columns =
        "row_class",
      target =
        "row",

      backgroundColor =
        DT::styleEqual(
          c(
            "draft",
            "reviewed",
            "approved",
            "needs_attention",
            "deprecated"
          ),
          c(
            unname(
              earth[["pale_ochre"]]
            ),
            unname(
              earth[["sand"]]
            ),
            unname(
              earth[["sage"]]
            ),
            unname(
              earth[["clay"]]
            ),
            unname(
              earth[["stone"]]
            )
          )
        ),
      verticalAlign = "top"
    )

    table_widget <- DT::formatStyle(
      table_widget,
      columns =
        "Metadata status",
      fontWeight =
        "600"
    )

    DT::formatStyle(
      table_widget,
      columns = c(
        "Update type",
        "Metadata section",
        "Reason for update",
        "Update summary",
        "Notes"
      ),
      whiteSpace = "normal",
      wordBreak = "break-word",
      lineHeight = "1.35",
      verticalAlign = "top"
    )
  })
}

datapond_app <- function() {
  shinyApp(
    ui = ui,
    server = server
  )
}
