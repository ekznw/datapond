# Live scan controls ----
#
# The scan result itself is displayed by the unified
# inventory table in data_files_inventory_ui().
data_files_live_scan_ui <- function(ns) {
  shiny::tagList(
    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F1E9DA;",
        "border:1px solid #B99572;",
        "margin-bottom:12px;"
      ),

      shiny::strong(
        "Live filesystem scan"
      ),

      shiny::tags$p(
        style = "margin:4px 0 0 0;",

        paste(
          "This workflow reads the files currently present on disk.",
          "A live scan is not saved to the metadata database until",
          "'Save live scan to database' is selected."
        )
      )
    ),

    shiny::h4(
      "Live scan source and scope"
    ),

    shiny::radioButtons(
      ns("mode"),
      "Live scan source",
      choices = c(
        "Current selected dataset" = "default",
        "Custom folder" = "custom"
      ),
      selected = "default"
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'default'",
        ns("mode")
      ),

      shiny::radioButtons(
        ns("scan_scope"),
        "Scan scope",
        choices = c(
          "Published outputs \u2014 data/outputs when available" =
            "outputs",

          "All managed data \u2014 complete data directory" =
            "all"
        ),
        selected = "outputs"
      )
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'custom'",
        ns("mode")
      ),

      shinyFiles::shinyDirButton(
        ns("custom_dir"),
        "Select custom scan folder",
        paste(
          "The selected folder will be",
          "scanned exactly as chosen."
        )
      )
    ),

    shiny::checkboxInput(
      ns("exclude_transient_dirs"),
      "Ignore transient working directories",
      value = TRUE
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:8px;"
      ),

      paste(
        "Default exclusions:",
        paste(
          default_inventory_excluded_dir_names(),
          collapse = ", "
        ),
        ". Directory names are matched exactly and",
        "case-insensitively."
      )
    ),

    shiny::tags$div(
      style = paste(
        "margin-top:8px;",
        "margin-bottom:10px;",
        "padding:8px;",
        "background:#F8F4EA;",
        "border:1px solid #ddd;"
      ),

      shiny::strong(
        "Resolved scan scope:"
      ),

      shiny::verbatimTextOutput(
        ns("scan_scope_status")
      )
    ),

    shiny::tags$div(
      style = paste(
        "margin-top:10px;",
        "padding:8px;",
        "background:#F8F4EA;",
        "border:1px solid #ddd;"
      ),

      shiny::strong(
        "Live scan status:"
      ),

      shiny::verbatimTextOutput(
        ns("data_status")
      )
    ),

    shiny::actionButton(
      ns("refresh_scan"),
      "Run live scan / refresh files"
    ),

    shiny::br(),
    shiny::br(),

    shiny::actionButton(
      ns("save_file_scan"),
      "Save live scan to database"
    ),

    shiny::br(),
    shiny::br(),

    DT::DTOutput(
      ns("file_scan_save_summary")
    )
  )
}

# Unified saved/live inventory UI ----
data_files_inventory_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Data-file inventory"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#f5f5f5;",
        "border:1px solid #ccc;",
        "margin-bottom:12px;"
      ),

      shiny::strong(
        "Inventory source"
      ),

      shiny::tags$p(
        style = "margin:4px 0 0 0;",

        paste(
          "The saved inventory is the metadata-database record.",
          "The live inventory shows the latest filesystem scan",
          "performed during this app session."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::radioButtons(
          ns("inventory_view"),
          "Inventory view",
          choices = c(
            "Saved database inventory" = "saved",
            "Current live scan" = "live"
          ),
          selected = "saved",
          inline = TRUE
        )
      ),

      shiny::column(
        width = 5,

        shiny::conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'live'",
            ns("inventory_view")
          ),

          shiny::checkboxInput(
            ns("show_sidecars"),
            "Show sidecar and component files",
            value = FALSE
          )
        )
      )
    ),

    shiny::uiOutput(
      ns("inventory_summary")
    ),

    DT::DTOutput(
      ns("inventory_table")
    ),

    shiny::uiOutput(
      ns("inventory_view_note")
    ),

    shiny::br(),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F8F4EA;",
        "border:1px solid #ccc;",
        "margin-bottom:12px;"
      ),

      shiny::fluidRow(
        shiny::column(
          width = 8,

          shiny::strong(
            "Live scan / filesystem comparison"
          ),

          shiny::uiOutput(
            ns("live_scan_summary")
          )
        ),

        shiny::column(
          width = 4,

          style = "text-align:right;",

          shiny::checkboxInput(
            ns("show_live_scan_panel"),
            "Show live scan and rescan controls",
            value = FALSE
          )
        )
      )
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == true",
        ns("show_live_scan_panel")
      ),

      data_files_live_scan_ui(
        ns
      )
    ),

    shiny::hr()
  )
}

# Shared selected saved-file control ----
#
# The selected saved file is shared by the File review
# and Attributes tabs.
data_files_selected_file_ui <- function(ns) {
  shiny::tags$div(
    style = paste(
      "padding:10px;",
      "background:#F8F4EA;",
      "border:1px solid #ccc;",
      "margin-bottom:14px;"
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::selectInput(
          ns("saved_file_select"),
          "Selected saved data file",
          choices = NULL
        )
      ),

      shiny::column(
        width = 5,

        shiny::uiOutput(
          ns("selected_saved_file_status")
        )
      )
    )
  )
}

# Saved file review UI ----
data_files_review_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Saved data-file review"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:10px;"
      ),

      paste(
        "Review saved logical data files and record their",
        "description, role, review status and profiling eligibility."
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::h4(
          "Saved data-file review summary"
        ),

        DT::DTOutput(
          ns("saved_file_review_table")
        )
      ),

      shiny::column(
        width = 5,

        shiny::tags$div(
          style = paste(
            "padding:12px;",
            "background:#fafafa;",
            "border:1px solid #ddd;"
          ),

          shiny::h4(
            "Selected-file review"
          ),

          shiny::tags$small(
            style = paste(
              "color:#666;",
              "display:block;",
              "margin-bottom:10px;"
            ),

            paste(
              "The form applies to the saved data file",
              "selected above."
            )
          ),

          shiny::textAreaInput(
            ns("data_file_description"),
            "Description of selected data file",
            placeholder = paste(
              "Describe what this file contains,",
              "what it represents and its role in the dataset."
            ),
            rows = 4
          ),

          shiny::textAreaInput(
            ns("data_file_notes"),
            "Internal review notes",
            placeholder = paste(
              "Add caveats, data-quality comments,",
              "review findings or other internal observations."
            ),
            rows = 4
          ),

          shiny::selectInput(
            ns("content_hint"),
            "File content / role",
            choices = c(
              "unknown",
              "raw data",
              "derived output",
              "input data",
              "working data",
              "scanned table",
              "map scan",
              "metadata document",
              "not data-bearing"
            ),
            selected = "unknown"
          ),

          shiny::selectInput(
            ns("review_status"),
            "Review status",
            choices = c(
              "not reviewed",
              "needs review",
              "described",
              "not data-bearing"
            ),
            selected = "not reviewed"
          ),

          shiny::checkboxInput(
            ns("include_in_attribute_scan"),
            "Include in attribute profiling",
            value = FALSE
          ),

          shiny::actionButton(
            ns("save_file_review"),
            "Save file review"
          )
        )
      )
    )
  )
}

# Attribute profiling and data dictionary UI ----
data_files_attributes_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Attribute profile / data dictionary"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:10px;"
      ),

      paste(
        "Profile the selected saved file and review or edit",
        "the resulting vector fields or raster-layer records."
      )
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F1E9DA;",
        "border:1px solid #B99572;",
        "margin-bottom:12px;"
      ),

      shiny::strong(
        "Attribute profiling"
      ),

      shiny::tags$p(
        style = "margin:4px 0 8px 0;",

        paste(
          "The profiling action applies to the saved data file",
          "selected above. The file must first be marked",
          "'Include in attribute profiling' in File review."
        )
      ),

      shiny::actionButton(
        ns("profile_selected_file"),
        "Profile selected file attributes / raster layers"
      ),

      shiny::tags$small(
        style = paste(
          "color:#666;",
          "display:block;",
          "margin-top:6px;"
        ),

        paste(
          "Vector files create one profile per attribute field.",
          "Raster files create one profile per raster layer or band."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::h4(
          "Saved attribute profile"
        ),

        DT::DTOutput(
          ns("attribute_table")
        )
      ),

      shiny::column(
        width = 5,

        shiny::tags$div(
          style = paste(
            "padding:12px;",
            "background:#fafafa;",
            "border:1px solid #ddd;"
          ),

          shiny::h4(
            "Selected attribute"
          ),

          shiny::selectInput(
            ns("attribute_select"),
            "Attribute or raster layer",
            choices = NULL
          ),

          shiny::textInput(
            ns("data_field_alias"),
            "Field alias"
          ),

          shiny::textAreaInput(
            ns("data_field_desc"),
            "Field description",
            rows = 4
          ),

          shiny::textInput(
            ns("data_field_units"),
            "Units"
          ),

          shiny::checkboxInput(
            ns("legend_field"),
            "Use for legend",
            value = FALSE
          ),

          shiny::textAreaInput(
            ns("legend_field_notes"),
            "Legend notes",
            placeholder = paste(
              "Identify the authoritative legend resource,",
              "classification scheme or interpretation notes."
            ),
            rows = 4
          ),

          shiny::actionButton(
            ns("save_attribute_note"),
            "Save attribute note"
          )
        )
      )
    )
  )
}

# Spatial metadata overview UI ----
data_files_spatial_overview_ui <- function(ns) {
  shiny::tagList(
    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F1E9DA;",
        "border:1px solid #B99572;",
        "margin-bottom:14px;"
      ),

      shiny::strong(
        "Spatial metadata workflow"
      ),

      shiny::tags$p(
        style = "margin:5px 0 0 0;",

        paste(
          "Extract spatial structure from saved data files, review",
          "the resulting file- and layer-level records, and choose",
          "which valid extents contribute to the dataset bounding box."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::h3(
          "Extract spatial metadata"
        ),

        shiny::tags$p(
          paste(
            "Extraction uses the current live scan when one is",
            "available. Otherwise, it uses the saved database",
            "inventory and locates the corresponding files on disk."
          )
        ),

        shiny::actionButton(
          ns("extract_spatial"),
          "Extract or refresh spatial metadata"
        )
      ),

      shiny::column(
        width = 5,

        shiny::tags$div(
          style = paste(
            "padding:10px;",
            "background:#F8F4EA;",
            "border:1px solid #ddd;"
          ),

          shiny::strong(
            "What extraction records"
          ),

          shiny::tags$ul(
            shiny::tags$li(
              "Spatial file and layer type"
            ),
            shiny::tags$li(
              "Coordinate reference system"
            ),
            shiny::tags$li(
              "Native spatial extent"
            ),
            shiny::tags$li(
              "Raster dimensions and cell size"
            ),
            shiny::tags$li(
              "Vector geometry and feature count"
            ),
            shiny::tags$li(
              "EPSG:4326 bounding-box eligibility"
            )
          )
        )
      )
    ),

    shiny::hr(),

    shiny::h3(
      "Dataset creation / compilation scale"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:10px;"
      ),

      paste(
        "Record the scale or scale range at which the dataset",
        "was captured, digitised, compiled or generalised.",
        "This is dataset-level interpretive metadata stored",
        "in entry_tbl."
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        shiny::numericInput(
          ns("max_scale"),
          paste(
            "Largest creation scale",
            "-- smallest denominator: 1 :"
          ),
          value = NA_real_,
          min = 1,
          step = 1000
        ),

        shiny::tags$small(
          style = "color:#666; display:block;",

          paste(
            "Example: enter 5000 for a largest",
            "creation scale of 1:5,000."
          )
        )
      ),

      shiny::column(
        width = 6,

        shiny::numericInput(
          ns("min_scale"),
          paste(
            "Smallest creation scale",
            "-- largest denominator: 1 :"
          ),
          value = NA_real_,
          min = 1,
          step = 1000
        ),

        shiny::tags$small(
          style = "color:#666; display:block;",

          paste(
            "Example: enter 50000 for a smallest",
            "creation scale of 1:50,000."
          )
        )
      )
    ),

    shiny::textAreaInput(
      ns("creation_scale_note"),
      "Creation / compilation scale note",
      placeholder = paste(
        "Explain how the scale was determined, including",
        "source-map scales, imagery scales, mixed source",
        "scales, generalisation, or why scale is not applicable."
      ),
      rows = 4
    ),

    shiny::uiOutput(
      ns("creation_scale_status")
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-top:4px;",
        "margin-bottom:12px;"
      ),

      paste(
        "Creation-scale values are saved when the main",
        "'Save Dataset' button is clicked."
      )
    ),

    shiny::hr(),

    shiny::h3(
      "Spatial metadata summary"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:8px;"
      ),

      paste(
        "Compact summary of extracted spatial structure, CRS,",
        "extent, dataset-bounding-box status and review notes."
      )
    ),

    DT::DTOutput(
      ns("spatial_summary_table")
    )
  )
}


# Derived dataset extent UI ----
data_files_spatial_dataset_extent_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Derived dataset bounding box"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F8F4EA;",
        "border:1px solid #ddd;",
        "margin-bottom:12px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "The derived bounding box is the EPSG:4326 envelope",
          "of all eligible spatial records and manual extents",
          "explicitly selected for inclusion."
        )
      )
    ),

    shiny::uiOutput(
      ns("dataset_bbox_status")
    ),

    leaflet::leafletOutput(
      ns("dataset_bbox_map"),
      height = "420px"
    ),

    shiny::br(),

    shiny::h4(
      "Included source assessment"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:8px;"
      ),

      paste(
        "This table shows which extracted records and manual",
        "extents were assessed, included, skipped or found",
        "ineligible for the aggregate extent."
      )
    ),

    DT::DTOutput(
      ns("dataset_bbox_contributors_table")
    )
  )
}


# Manual spatial extents UI ----
data_files_spatial_manual_extents_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Manual spatial extents"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#E9DFB5;",
        "border:1px solid #C3A35B;",
        "margin-bottom:12px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "Use a manual extent when geographic coverage cannot be",
          "extracted directly, such as for spreadsheets, tabular",
          "monitoring data or documentary datasets. The source CRS",
          "must be supplied explicitly."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 4,

        shiny::selectInput(
          ns("manual_extent_select"),
          "Existing manual extent",
          choices = c(
            "-- New manual extent --" = ""
          )
        ),

        shiny::textInput(
          ns("manual_extent_label"),
          "Extent label",
          placeholder =
            "For example: KwaZulu-Natal rainfall stations"
        ),

        shiny::selectInput(
          ns("manual_extent_data_file_id"),
          "Linked data file",
          choices = c(
            "Dataset-level extent" = ""
          )
        ),

        shiny::textInput(
          ns("manual_extent_crs"),
          "Coordinate reference system",
          placeholder =
            "For example: EPSG:4326 or EPSG:32736"
        )
      ),

      shiny::column(
        width = 4,

        shiny::numericInput(
          ns("manual_extent_xmin"),
          "Minimum X",
          value = NA_real_
        ),

        shiny::numericInput(
          ns("manual_extent_ymin"),
          "Minimum Y",
          value = NA_real_
        ),

        shiny::numericInput(
          ns("manual_extent_xmax"),
          "Maximum X",
          value = NA_real_
        ),

        shiny::numericInput(
          ns("manual_extent_ymax"),
          "Maximum Y",
          value = NA_real_
        )
      ),

      shiny::column(
        width = 4,

        shiny::textAreaInput(
          ns("manual_extent_note"),
          "Extent source / justification",
          placeholder = paste(
            "Describe how the extent was determined and cite",
            "the supporting source where appropriate."
          ),
          rows = 5
        ),

        shiny::checkboxInput(
          ns("manual_extent_include"),
          "Include in derived dataset bounding box",
          value = TRUE
        ),

        shiny::checkboxInput(
          ns("manual_extent_active"),
          "Active manual extent",
          value = TRUE
        ),

        shiny::actionButton(
          ns("manual_extent_new"),
          "New extent"
        ),

        shiny::actionButton(
          ns("manual_extent_save"),
          "Save extent"
        ),

        shiny::actionButton(
          ns("manual_extent_deactivate"),
          "Deactivate selected"
        )
      )
    ),

    shiny::uiOutput(
      ns("manual_extent_validation")
    ),

    shiny::h4(
      "Extent preview"
    ),

    leaflet::leafletOutput(
      ns("manual_extent_map"),
      height = "340px"
    ),

    shiny::br(),

    shiny::h4(
      "Saved manual extents"
    ),

    DT::DTOutput(
      ns("manual_extent_table")
    )
  )
}


# Individual spatial record review UI ----
data_files_spatial_record_review_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Spatial file and layer review"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F8F4EA;",
        "border:1px solid #ddd;",
        "margin-bottom:12px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "Review one extracted spatial record at a time.",
          "A record may represent an individual file, vector layer,",
          "raster layer or raster band."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 4,

        shiny::selectInput(
          ns("spatial_record_select"),
          "Spatial file or layer",
          choices = NULL
        ),

        shiny::h4(
          "Extracted record"
        ),

        shiny::verbatimTextOutput(
          ns("spatial_record_details")
        ),

        shiny::uiOutput(
          ns("spatial_bbox_inclusion_ui")
        ),

        shiny::hr(),

        shiny::textAreaInput(
          ns("spatial_user_notes"),
          "Review notes",
          placeholder = paste(
            "For example: CRS could not be detected,",
            "layer failed to open, geometry appears invalid,",
            "or metadata requires verification."
          ),
          rows = 6
        ),

        shiny::actionButton(
          ns("save_spatial_notes"),
          "Save review notes"
        )
      ),

      shiny::column(
        width = 8,

        shiny::h4(
          "Bounding-box preview"
        ),

        shiny::uiOutput(
          ns("spatial_bbox_status")
        ),

        leaflet::leafletOutput(
          ns("spatial_bbox_map"),
          height = "430px"
        )
      )
    )
  )
}


# Full technical metadata UI ----
data_files_spatial_technical_ui <- function(ns) {
  shiny::tagList(
    shiny::h3(
      "Full spatial technical details"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F8F4EA;",
        "border:1px solid #ddd;",
        "margin-bottom:12px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "This table exposes the complete saved spatial metadata",
          "record for technical inspection, troubleshooting and export.",
          "The Overview tab provides the normal user-facing summary."
        )
      )
    ),

    DT::DTOutput(
      ns("spatial_details_table")
    )
  )
}


# Main spatial metadata UI ----
data_files_spatial_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h2(
      "Spatial metadata"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:14px;"
      ),

      paste(
        "Extract, review and maintain spatial metadata for the",
        "saved data-file inventory associated with the selected dataset."
      )
    ),

    shiny::tabsetPanel(
      id = ns("spatial_metadata_section"),
      selected = "overview",
      type = "tabs",

      shiny::tabPanel(
        title = "Overview",
        value = "overview",

        data_files_spatial_overview_ui(
          ns
        )
      ),

      shiny::tabPanel(
        title = "Dataset extent",
        value = "dataset_extent",

        data_files_spatial_dataset_extent_ui(
          ns
        )
      ),

      shiny::tabPanel(
        title = "Manual extents",
        value = "manual_extents",

        data_files_spatial_manual_extents_ui(
          ns
        )
      ),

      shiny::tabPanel(
        title = "Record review",
        value = "record_review",

        data_files_spatial_record_review_ui(
          ns
        )
      ),

      shiny::tabPanel(
        title = "Technical details",
        value = "technical",

        data_files_spatial_technical_ui(
          ns
        )
      )
    )
  )
}

# Main data files UI ----
mod_data_files_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      "Data files"
    ),

    shiny::tags$small(
      style = paste(
        "color:#666;",
        "display:block;",
        "margin-bottom:10px;"
      ),

      paste(
        "Maintain the saved data-file inventory, review individual",
        "files and build the dataset attribute dictionary."
      )
    ),

    data_files_selected_file_ui(
      ns
    ),

    shiny::tabsetPanel(
      id = ns("data_files_section"),
      selected = "inventory",
      type = "tabs",

      shiny::tabPanel(
        title = "Inventory",
        value = "inventory",

        data_files_inventory_ui(
          ns
        )
      ),

      shiny::tabPanel(
        title = "File review",
        value = "review",

        data_files_review_ui(
          ns
        )
      ),

      shiny::tabPanel(
        title = "Attributes",
        value = "attributes",

        data_files_attributes_ui(
          ns
        )
      )
    )
  )
}

mod_data_files_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Live scan panel visibility ----
    live_scan_dataset_key <- shiny::reactiveVal(NULL)
    live_scan_programmatic_update <- shiny::reactiveVal(FALSE)

    ## helper functions ----
    # helper functions to clear data files detection and metadata summaries
    empty_file_scan <- function(
      message = "No file scan has been run for the current folder."
    ) {
      data.table::data.table(
        message = message
      )
    }
    empty_spatial_metadata <- function(
      message = "Spatial metadata has not been extracted yet."
    ) {
      data.table::data.table(
        message = message
      )
    }
    logical_dataset_rows <- function(df) {
      if ("message" %in% names(df)) {
        return(df)
      }
      df <- data.table::as.data.table(data.table::copy(df))
      df[
        primary_dataset == TRUE &
          !file_role %in% c("sidecar", "container_support") &
          !data_file_format %in% c("sidecar", "container_support", "archive")
      ]
    }

    ## function to prepare a table showing file review info
    prepare_saved_file_review_table <- function(df) {
      df <- data.table::as.data.table(
        data.table::copy(df)
      )

      required_columns <- list(
        data_file_id = NA_integer_,
        file_name = NA_character_,
        data_file_rel_path_full = NA_character_,
        data_file_format = NA_character_,
        content_hint = NA_character_,
        review_status = NA_character_,
        data_file_description = NA_character_,
        data_file_notes = NA_character_,
        include_in_attribute_scan = 0L,
        active = 1L,
        attribute_count = 0L,
        layer_count = 0L,
        profile_status = NA_character_,
        profile_message = NA_character_,
        latest_attribute_profile_at = NA_character_,
        profile_completed_at = NA_character_,
        reviewed_by_name = NA_character_,
        profiled_by_name = NA_character_
      )

      for (nm in names(required_columns)) {
        if (!nm %in% names(df)) {
          df[, (nm) := required_columns[[nm]]]
        }
      }

      clean_text <- function(x) {
        x <- as.character(x)

        x[
          is.na(x) |
            trimws(x) == ""
        ] <- ""

        x
      }

      text_columns <- c(
        "file_name",
        "data_file_rel_path_full",
        "data_file_format",
        "content_hint",
        "review_status",
        "data_file_description",
        "data_file_notes",
        "profile_status",
        "profile_message",
        "latest_attribute_profile_at",
        "profile_completed_at",
        "reviewed_by_name",
        "profiled_by_name"
      )

      for (nm in text_columns) {
        df[, (nm) := clean_text(get(nm))]
      }

      df[
        ,
        active_label :=
          data.table::fifelse(
            as.integer(active) == 1L,
            "Active",
            "Missing"
          )
      ]

      df[
        ,
        attribute_scan_label :=
          data.table::fifelse(
            as.integer(include_in_attribute_scan) == 1L,
            "Yes",
            "No"
          )
      ]

      df[
        ,
        profile_check := data.table::fcase(
          as.integer(include_in_attribute_scan) != 1L,
          "Not requested",

          profile_status == "failed",
          "Failed",

          profile_status %in% c("pending", "running"),
          "In progress",

          as.integer(attribute_count) > 0L,
          "Profiled",

          profile_status == "complete" &
            as.integer(attribute_count) == 0L,
          "Complete: no attributes",

          default = "Not profiled"
        )
      ]

      df[
        ,
        profile_date :=
          data.table::fifelse(
            nzchar(profile_completed_at),
            profile_completed_at,
            latest_attribute_profile_at
          )
      ]

      df[
        ,
        .(
          data_file_id,
          file = file_name,
          relative_path = data_file_rel_path_full,
          format = data_file_format,
          content_hint,
          review_status,
          description = data_file_description,
          notes = data_file_notes,
          attribute_scan = attribute_scan_label,
          attribute_profile = profile_check,
          attributes = as.integer(attribute_count),
          layers = as.integer(layer_count),
          profile_date,
          profile_message,
          reviewed_by = reviewed_by_name,
          profiled_by = profiled_by_name,
          file_status = active_label
        )
      ]
    }

    prepare_saved_inventory_table <- function(df) {
      if (
        is.null(df) ||
        nrow(as.data.frame(df)) == 0L
      ) {
        return(
          data.table::data.table(
            message = paste(
              "No data-file inventory has been saved",
              "for the selected dataset."
            )
          )
        )
      }

      dt <- data.table::as.data.table(
        data.table::copy(df)
      )

      required_columns <- list(
        id = NA_integer_,
        file_name = NA_character_,
        data_file_rel_path_full = NA_character_,
        data_file_format = NA_character_,
        file_role = NA_character_,
        file_size_kb = NA_real_,
        modified_time = NA_character_,
        scan_status = NA_character_,
        active = 1L,
        last_seen_at = NA_character_,
        missing_since = NA_character_
      )

      for (field in names(required_columns)) {
        if (!field %in% names(dt)) {
          dt[
            ,
            (field) := required_columns[[field]]
          ]
        }
      }

      dt[
        ,
        file_status :=
          data.table::fifelse(
            as.integer(active) == 1L,
            "Active",
            "Missing"
          )
      ]

      dt[
        ,
        .(
          id,
          File = file_name,
          `Relative path` =
            data_file_rel_path_full,
          Format = data_file_format,
          Role = file_role,
          `Size KB` = file_size_kb,
          Modified = as.character(
            modified_time
          ),
          `Scan status` = scan_status,
          `File status` = file_status,
          `Last seen` = as.character(
            last_seen_at
          ),
          `Missing since` = as.character(
            missing_since
          )
        )
      ]
    }

    prepare_live_inventory_table <- function(df) {
      if (
        is.null(df) ||
        nrow(as.data.frame(df)) == 0L
      ) {
        return(
          data.table::data.table(
            message = "No live files have been detected."
          )
        )
      }

      if ("message" %in% names(df)) {
        return(
          data.table::as.data.table(
            data.table::copy(df)
          )
        )
      }

      dt <- data.table::as.data.table(
        data.table::copy(df)
      )

      required_columns <- list(
        file_name = NA_character_,
        data_file_rel_path_full = NA_character_,
        data_file_format = NA_character_,
        file_role = NA_character_,
        primary_dataset = FALSE,
        logical_file_name = NA_character_,
        logical_rel_path = NA_character_,
        logical_size_kb = NA_real_,
        sidecar_summary = NA_character_,
        file_size_kb = NA_real_,
        modified_time = NA_character_
      )

      for (field in names(required_columns)) {
        if (!field %in% names(dt)) {
          dt[
            ,
            (field) := required_columns[[field]]
          ]
        }
      }

      dt[
        ,
        `Primary dataset` :=
          data.table::fifelse(
            primary_dataset %in% TRUE,
            "Yes",
            "No"
          )
      ]

      dt[
        ,
        .(
          File = file_name,
          `Relative path` =
            data_file_rel_path_full,
          Format = data_file_format,
          Role = file_role,
          `Primary dataset`,
          `Logical file` =
            logical_file_name,
          `Logical path` =
            logical_rel_path,
          `Logical size KB` =
            logical_size_kb,
          `Sidecar summary` =
            sidecar_summary,
          `File size KB` =
            file_size_kb,
          Modified = as.character(
            modified_time
          )
        )
      ]
    }

    ## render standard DataTables (DT)
    render_data_files_dt <- function(
      df,
      page_length = 10L,
      selection = "none",
      filter = "top",
      buttons = TRUE,
      scroll_x = TRUE,
      hidden_columns = integer()
    ) {
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(
          message = "No records are available."
        )
      }

      df <- as.data.frame(
        df,
        stringsAsFactors = FALSE
      )

      is_message_table <- identical(
        names(df),
        "message"
      )

      if (isTRUE(is_message_table)) {
        return(
          DT::datatable(
            df,
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

      extensions <- if (isTRUE(buttons)) {
        "Buttons"
      } else {
        character()
      }

      dom_value <- if (isTRUE(buttons)) {
        "Bfrtip"
      } else {
        "frtip"
      }

      column_defs <- list()

      if (length(hidden_columns) > 0) {
        column_defs <- list(
          list(
            visible = FALSE,
            targets = as.integer(hidden_columns)
          )
        )
      }

      DT::datatable(
        df,
        rownames = FALSE,
        selection = selection,
        filter = filter,
        escape = TRUE,
        extensions = extensions,
        options = list(
          pageLength = as.integer(page_length),
          scrollX = isTRUE(scroll_x),
          autoWidth = TRUE,
          dom = dom_value,
          buttons = if (isTRUE(buttons)) {
            c("copy", "csv")
          } else {
            NULL
          },
          columnDefs = column_defs
        )
      )
    }

    clear_live_scan_outputs <- function(
      reason = "Live scan settings changed."
    ) {
      scanned_files(
        empty_file_scan(
          paste(
            reason,
            "Run the live scan to inspect the selected folder."
          )
        )
      )

      file_scan_save_summary(
        data.table::data.table(
          message = paste(
            reason,
            "No refreshed live scan has been saved."
          )
        )
      )

      invisible(TRUE)
    }

    creation_scale_validation <- reactive({
      validate_creation_scale_metadata(
        max_scale = input$max_scale,
        min_scale = input$min_scale,
        creation_scale_note =
          input$creation_scale_note
      )
    })


    creation_scale_values <- reactive({
      validation <- creation_scale_validation()

      if (!isTRUE(validation$valid)) {
        return(
          list(
            max_scale = input$max_scale,
            min_scale = input$min_scale,
            creation_scale_note =
              input$creation_scale_note
          )
        )
      }

      validation$values
    })


    clear_creation_scale <- function() {
      updateNumericInput(
        session,
        "max_scale",
        value = NA_real_
      )

      updateNumericInput(
        session,
        "min_scale",
        value = NA_real_
      )

      updateTextAreaInput(
        session,
        "creation_scale_note",
        value = ""
      )

      invisible(TRUE)
    }


    load_creation_scale_record <- function(rec) {
      if (
        is.null(rec) ||
        nrow(as.data.frame(rec)) == 0L
      ) {
        clear_creation_scale()
        return(invisible(FALSE))
      }

      max_scale <- creation_scale_numeric(
        creation_scale_record_value(
          rec,
          "max_scale",
          default = NA_real_
        )
      )

      min_scale <- creation_scale_numeric(
        creation_scale_record_value(
          rec,
          "min_scale",
          default = NA_real_
        )
      )

      note <- creation_scale_text(
        creation_scale_record_value(
          rec,
          "creation_scale_note",
          default = NA_character_
        )
      )

      if (is.na(note)) {
        note <- ""
      }

      updateNumericInput(
        session,
        "max_scale",
        value = max_scale
      )

      updateNumericInput(
        session,
        "min_scale",
        value = min_scale
      )

      updateTextAreaInput(
        session,
        "creation_scale_note",
        value = note
      )

      invisible(TRUE)
    }

    output$creation_scale_status <- renderUI({
      validation <- creation_scale_validation()

      status <- validation$status

      appearance <- switch(
        status,

        valid = list(
          background = "#D1DDC7",
          border = "#526657",
          heading = "Valid"
        ),

        note_only = list(
          background = "#F1E9DA",
          border = "#806855",
          heading = "Scale note"
        ),

        not_recorded = list(
          background = "#F8F4EA",
          border = "#D3CEC4",
          heading = "Not recorded"
        ),

        list(
          background = "#E9DFB5",
          border = "#C3A35B",
          heading = "Check required"
        )
      )

      shiny::tags$div(
        style = paste(
          "padding:8px;",
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
          "margin-top:8px;"
        ),

        shiny::strong(
          paste0(
            appearance$heading,
            ": "
          )
        ),

        validation$message
      )
    })

    clear_data_files_module <- function() {
      clear_creation_scale()

      live_scan_dataset_key(NULL)

      shiny::updateTabsetPanel(
        session,
        "data_files_section",
        selected = "inventory"
      )

      shiny::updateRadioButtons(
        session,
        "inventory_view",
        selected = "live"
      )

      scanned_files(
        empty_file_scan(
          "No live filesystem scan has been run for the new record."
        )
      )

      spatial_metadata(
        empty_spatial_metadata(
          "No spatial metadata is associated with the new record."
        )
      )

      file_scan_save_summary(
        data.table::data.table(
          message = paste(
            "No live file scan has been saved",
            "for the new record."
          )
        )
      )

      updateSelectInput(
        session,
        "saved_file_select",
        choices = character(),
        selected = character(0)
      )

      updateTextAreaInput(
        session,
        "data_file_description",
        value = ""
      )

      updateTextAreaInput(
        session,
        "data_file_notes",
        value = ""
      )

      updateSelectInput(
        session,
        "content_hint",
        selected = "unknown"
      )

      updateSelectInput(
        session,
        "review_status",
        selected = "not reviewed"
      )

      updateCheckboxInput(
        session,
        "include_in_attribute_scan",
        value = FALSE
      )

      updateSelectInput(
        session,
        "attribute_select",
        choices = character(),
        selected = character(0)
      )

      updateTextInput(
        session,
        "data_field_alias",
        value = ""
      )

      updateTextAreaInput(
        session,
        "data_field_desc",
        value = ""
      )

      updateTextInput(
        session,
        "data_field_units",
        value = ""
      )

      updateCheckboxInput(
        session,
        "legend_field",
        value = FALSE
      )

      updateTextAreaInput(
        session,
        "legend_field_notes",
        value = ""
      )

      updateSelectInput(
        session,
        "spatial_record_select",
        choices = character(),
        selected = character(0)
      )

      updateTextAreaInput(
        session,
        "spatial_user_notes",
        value = ""
      )

      shiny::updateCheckboxInput(
        session,
        "show_live_scan_panel",
        value = TRUE
      )

      invisible(TRUE)
    }

    # spatial bbox ----
    # manual extent clear
    clear_manual_extent_form <- function() {
      freezeReactiveValue(
        input,
        "manual_extent_select"
      )

      updateSelectInput(
        session,
        "manual_extent_select",
        selected = ""
      )

      updateTextInput(
        session,
        "manual_extent_label",
        value = ""
      )

      updateSelectInput(
        session,
        "manual_extent_data_file_id",
        selected = ""
      )

      updateTextInput(
        session,
        "manual_extent_crs",
        value = ""
      )

      updateNumericInput(
        session,
        "manual_extent_xmin",
        value = NA_real_
      )

      updateNumericInput(
        session,
        "manual_extent_ymin",
        value = NA_real_
      )

      updateNumericInput(
        session,
        "manual_extent_xmax",
        value = NA_real_
      )

      updateNumericInput(
        session,
        "manual_extent_ymax",
        value = NA_real_
      )

      updateTextAreaInput(
        session,
        "manual_extent_note",
        value = ""
      )

      updateCheckboxInput(
        session,
        "manual_extent_include",
        value = TRUE
      )

      updateCheckboxInput(
        session,
        "manual_extent_active",
        value = TRUE
      )

      invisible(TRUE)
    }

    # Custom scan roots can be anywhere accessible.
    custom_scan_roots <- c(
      home = "~"
    )

    shinyDirChoose(
      input,
      "custom_dir",
      roots = custom_scan_roots,
      session = session
    )


    first_usable_directory <- function(paths) {
      paths <- as.character(paths)

      paths <- paths[
        !is.na(paths) &
          nzchar(trimws(paths))
      ]

      if (length(paths) == 0L) {
        return(NULL)
      }

      paths <- unique(
        vapply(
          paths,
          normalise_inventory_path,
          character(1)
        )
      )

      existing <- paths[
        dir.exists(paths)
      ]

      if (length(existing) > 0L) {
        return(existing[1])
      }

      # Return the expected path even when it is missing,
      # allowing the UI to explain what could not be found.
      paths[1]
    }


    nonempty_path_value <- function(x) {
      if (
        is.null(x) ||
        length(x) == 0L ||
        is.na(x[1]) ||
        !nzchar(trimws(as.character(x[1])))
      ) {
        return(NA_character_)
      }

      as.character(x[1])
    }


    selected_dataset_data_dir <- reactive({
      dataset_root <- nonempty_path_value(
        state$dataset_path
      )

      current_dataset_root <- nonempty_path_value(
        state$current_dataset_folder
      )

      explicit_data_path <- nonempty_path_value(
        state$data_path
      )

      candidates <- c(
        if (!is.na(dataset_root)) {
          file.path(
            dataset_root,
            "data"
          )
        } else {
          NA_character_
        },

        if (!is.na(current_dataset_root)) {
          file.path(
            current_dataset_root,
            "data"
          )
        } else {
          NA_character_
        },

        explicit_data_path
      )

      first_usable_directory(
        candidates
      )
    })


    custom_scan_dir <- reactive({
      if (is.null(input$custom_dir)) {
        return(NULL)
      }

      path <- parseDirPath(
        custom_scan_roots,
        input$custom_dir
      )

      if (
        is.null(path) ||
        length(path) == 0L
      ) {
        return(NULL)
      }

      normalise_inventory_path(
        path[1]
      )
    })


    scan_scope_info <- reactive({
      mode <- input$mode

      if (
        is.null(mode) ||
        !mode %in% c(
          "default",
          "custom"
        )
      ) {
        mode <- "default"
      }

      if (identical(
        mode,
        "custom"
      )) {
        custom_dir <- custom_scan_dir()

        return(
          list(
            mode = "custom",
            requested_scope = "custom",
            effective_scope = "custom",
            relative_base_dir =
              custom_dir,
            scan_root = custom_dir,
            outputs_fallback = FALSE
          )
        )
      }

      data_dir <-
        selected_dataset_data_dir()

      requested_scope <-
        input$scan_scope

      if (
        is.null(requested_scope) ||
        !requested_scope %in%
          c(
            "outputs",
            "all"
          )
      ) {
        requested_scope <- "outputs"
      }

      if (is.null(data_dir)) {
        return(
          list(
            mode = "default",
            requested_scope =
              requested_scope,
            effective_scope =
              requested_scope,
            relative_base_dir = NULL,
            scan_root = NULL,
            outputs_fallback = FALSE
          )
        )
      }

      if (identical(
        requested_scope,
        "outputs"
      )) {
        outputs_dir <- file.path(
          data_dir,
          "outputs"
        )

        if (dir.exists(outputs_dir)) {
          return(
            list(
              mode = "default",
              requested_scope = "outputs",
              effective_scope = "outputs",
              relative_base_dir =
                data_dir,
              scan_root =
                normalise_inventory_path(
                  outputs_dir
                ),
              outputs_fallback = FALSE
            )
          )
        }

        return(
          list(
            mode = "default",
            requested_scope = "outputs",
            effective_scope = "all",
            relative_base_dir =
              data_dir,
            scan_root = data_dir,
            outputs_fallback = TRUE
          )
        )
      }

      list(
        mode = "default",
        requested_scope = "all",
        effective_scope = "all",
        relative_base_dir = data_dir,
        scan_root = data_dir,
        outputs_fallback = FALSE
      )
    })


    # Canonical data directory belonging to the selected dataset.
    active_data_dir <- reactive({
      selected_dataset_data_dir()
    })


    # Base directory used to construct relative paths during
    # the current live scan.
    scan_relative_base_dir <- reactive({
      scan_scope_info()$relative_base_dir
    })

    # Directory actually passed to inventory_scan_local().
    active_scan_dir <- reactive({
      scan_scope_info()$scan_root
    })

    output$scan_scope_status <- renderText({
      scope_info <- scan_scope_info()

      scan_root <- scope_info$scan_root

      relative_base_dir <-
        scope_info$relative_base_dir

      if (
        is.null(scan_root) ||
        length(scan_root) == 0L ||
        is.na(scan_root[1]) ||
        !nzchar(
          trimws(
            as.character(
              scan_root[1]
            )
          )
        )
      ) {
        if (identical(
          scope_info$mode,
          "custom"
        )) {
          return(
            paste(
              "Select a custom folder to scan."
            )
          )
        }

        return(
          paste(
            "Select or create a dataset before",
            "running the live file scan."
          )
        )
      }

      scan_result <- scanned_files()

      ignored_files <- attr(
        scan_result,
        "ignored_files"
      )

      ignored_rows <- attr(
        scan_result,
        "ignored_rows"
      )

      if (
        is.null(ignored_files) ||
        length(ignored_files) == 0L ||
        is.na(ignored_files)
      ) {
        ignored_files <- 0L
      }

      if (
        is.null(ignored_rows) ||
        length(ignored_rows) == 0L ||
        is.na(ignored_rows)
      ) {
        ignored_rows <- 0L
      }

      scope_label <- switch(
        scope_info$effective_scope,

        outputs = "Published outputs",

        all = if (
          isTRUE(
            scope_info$outputs_fallback
          )
        ) {
          paste(
            "All managed data",
            "(outputs fallback)"
          )
        } else {
          "All managed data"
        },

        custom = "Custom folder",

        "Unknown"
      )

      exclusions_label <- if (
        isTRUE(
          input$exclude_transient_dirs
        )
      ) {
        paste(
          default_inventory_excluded_dir_names(),
          collapse = ", "
        )
      } else {
        "Disabled"
      }

      paste0(
        "Scope: ",
        scope_label,
        "\n",
        "Scan root: ",
        scan_root,
        "\n",
        "Relative-path base: ",
        relative_base_dir,
        "\n",
        "Transient-directory exclusions: ",
        exclusions_label,
        "\n",
        "Ignored in latest scan: ",
        ignored_files,
        " file",
        ifelse(
          ignored_files == 1L,
          "",
          "s"
        ),
        " / ",
        ignored_rows,
        " inventory row",
        ifelse(
          ignored_rows == 1L,
          "",
          "s"
        )
      )
    })

    scanned_files <- reactiveVal(
      empty_file_scan(
        paste(
          "No live filesystem scan has been run",
          "during this app session."
        )
      )
    )

    spatial_metadata <- reactiveVal(
      empty_spatial_metadata()
    )

    file_scan_save_summary <- reactiveVal(
      data.table::data.table(
        message = paste(
          "Detected files have not been saved",
          "to the database yet."
        )
      )
    )

    # scan_refresh <- reactiveVal(0)
    observeEvent(
      input$refresh_scan,
      {
        scope_info <- scan_scope_info()

        scan_root <- scope_info$scan_root

        relative_base_dir <-
          scope_info$relative_base_dir

        if (
          is.null(scan_root) ||
          length(scan_root) == 0L ||
          is.na(scan_root[1]) ||
          !nzchar(
            trimws(
              as.character(
                scan_root[1]
              )
            )
          )
        ) {
          scanned_files(
            empty_file_scan(
              paste(
                "No selected-dataset data directory",
                "or custom scan folder is available."
              )
            )
          )

          spatial_metadata(
            empty_spatial_metadata(
              paste(
                "Spatial metadata was cleared because",
                "no scan folder is selected."
              )
            )
          )

          showNotification(
            "No live scan folder is available.",
            type = "warning"
          )

          return()
        }

        excluded_dir_names <- if (
          isTRUE(
            input$exclude_transient_dirs
          )
        ) {
          default_inventory_excluded_dir_names()
        } else {
          character()
        }

        scan_result <- scan_dataset_files(
          data_dir = scan_root,
          excluded_dir_names =
            excluded_dir_names,
          relative_base_dir =
            relative_base_dir
        )

        scanned_files(
          scan_result
        )

        shiny::updateRadioButtons(
          session,
          "inventory_view",
          selected = "live"
        )

        shiny::updateTabsetPanel(
          session,
          "data_files_section",
          selected = "inventory"
        )

        ignored_files <- attr(
          scan_result,
          "ignored_files"
        )

        if (
          is.null(ignored_files) ||
          length(ignored_files) == 0L ||
          is.na(ignored_files)
        ) {
          ignored_files <- 0L
        }

        notification_message <- paste0(
          "Live file scan refreshed"
        )

        if (ignored_files > 0L) {
          notification_message <- paste0(
            notification_message,
            "; ",
            ignored_files,
            " file",
            ifelse(
              ignored_files == 1L,
              "",
              "s"
            ),
            " ignored in transient directories"
          )
        }

        if (isTRUE(
          scope_info$outputs_fallback
        )) {
          notification_message <- paste0(
            notification_message,
            ". No data/outputs directory was found,",
            " so the full data directory was scanned"
          )
        }

        showNotification(
          notification_message,
          type = "message",
          duration = 8
        )
      }
    )

    observeEvent(
      input$mode,
      {
        clear_live_scan_outputs(
          "Live scan source changed."
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$scan_scope,
      {
        clear_live_scan_outputs(
          "Live scan scope changed."
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$custom_dir,
      {
        clear_live_scan_outputs(
          "Custom scan folder changed."
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$exclude_transient_dirs,
      {
        clear_live_scan_outputs(
          paste(
            "Transient-directory exclusion",
            "settings changed."
          )
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      state$data_path,
      {
        clear_live_scan_outputs(
          "Selected dataset data directory changed."
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      state$data_pond,
      {
        clear_live_scan_outputs(
          "Data pond changed."
        )
      },
      ignoreInit = TRUE
    )

    files <- reactive({
      scanned_files()
    })

    display_files <- reactive({
      df <- files()
      if ("message" %in% names(df)) {
        return(df)
      }
      df <- data.table::as.data.table(data.table::copy(df))
      show_components <- isTRUE(input$show_sidecars)
      if (!show_components) {
        # Default logical dataset view.
        # Keep primary container representatives, but hide true sidecars.
        df <- logical_dataset_rows(df)
      }
      # Use logical fields for the normal user-facing view.
      # Use physical fields when expanded for sidecar/component inspection.
      df[, display_name := data.table::fifelse(
        !show_components &
          !is.na(logical_file_name) &
          logical_file_name != "",
        logical_file_name,
        file_name
      )]
      df[, display_path := data.table::fifelse(
        !show_components &
          !is.na(logical_rel_path) &
          logical_rel_path != "",
        logical_rel_path,
        data_file_rel_path_full
      )]
      df[, display_format := data.table::fifelse(
        !is.na(logical_format) &
          logical_format != "" &
          primary_dataset == TRUE,
        logical_format,
        data_file_format
      )]
      df[, display_role := data.table::fifelse(
        !show_components &
          primary_dataset == TRUE &
          file_role == "dataset_container_component",
        "dataset_container",
        file_role
      )]
      df[, role_rank := data.table::fifelse(
        primary_dataset == TRUE, 1L,
        data.table::fifelse(
          file_role == "file", 2L,
          data.table::fifelse(
            file_role == "dataset_container_component", 3L,
            data.table::fifelse(file_role == "sidecar", 4L, 5L)
          )
        )
      )]

      # Display size:
      # - logical size in trimmed view
      # - physical component size in expanded view
      df[, display_size_kb := file_size_kb]
      df[
        !show_components & !is.na(logical_size_kb),
        display_size_kb := logical_size_kb
      ]
      # Display modified time:
      # Always work with character values in the UI layer.
      df[, display_modified_time := as.character(modified_time)]
      if ("component_modtime_latest" %in% names(df)) {
        df[, component_modtime_latest_chr := as.character(component_modtime_latest)]
        df[
          !show_components &
            !is.na(component_modtime_latest_chr) &
            component_modtime_latest_chr != "",
          display_modified_time := component_modtime_latest_chr
        ]
      }
      df[, sort_group := data.table::fifelse(
        !is.na(group_key) & group_key != "",
        group_key,
        display_path
      )]
      df[, sort_name := tolower(display_name)]
      data.table::setorder(df, sort_group, role_rank, sort_name)
      df[, .(
        file_name = display_name,
        data_file_rel_path_full = display_path,
        data_file_format = display_format,
        file_role = display_role,
        primary_dataset,
        logical_file_name,
        logical_rel_path,
        logical_size_kb,
        sidecar_summary,
        file_size_kb = display_size_kb,
        modified_time = display_modified_time
      )]
    })

    saved_file_review_summary <- reactive({
      state$refresh_data_files_saved
      state$refresh_attributes

      dataset_id <- state$current_dataset_id
      base_path <- state$data_pond

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0 ||
        is.na(dataset_id) ||
        is.null(base_path) ||
        length(base_path) == 0 ||
        is.na(base_path) ||
        base_path == ""
      ) {
        return(data.frame())
      }

      get_data_file_review_summary(
        dataset_id = dataset_id,
        base_path = base_path,
        active_only = FALSE
      )
    })

    output$saved_file_review_table <- DT::renderDT({
      df <- saved_file_review_summary()

      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(
              message = paste(
                "No saved data file review records",
                "are available for the selected dataset."
              )
            ),
            rownames = FALSE,
            selection = "none",
            options = list(
              dom = "t",
              ordering = FALSE
            )
          )
        )
      }

      display_df <- prepare_saved_file_review_table(df)

      tbl <- DT::datatable(
        display_df,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        escape = TRUE,
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c(
            "copy",
            "csv"
          ),
          columnDefs = list(
            list(
              visible = FALSE,
              targets = 0
            ),
            list(
              width = "180px",
              targets = 1
            ),
            list(
              width = "260px",
              targets = 2
            ),
            list(
              width = "160px",
              targets = 6
            ),
            list(
              width = "260px",
              targets = 7
            ),
            list(
              width = "220px",
              targets = 13
            )
          )
        ),
        colnames = c(
          "ID",
          "File",
          "Relative path",
          "Format",
          "Content hint",
          "Review status",
          "Description",
          "Notes",
          "Attribute scan",
          "Attribute profile",
          "Attributes",
          "Layers",
          "Profile date",
          "Profile message",
          "Reviewed by",
          "Profiled by",
          "File status"
        )
      )

      tbl <- DT::formatStyle(
        tbl,
        columns = "attribute_profile",
        backgroundColor = DT::styleEqual(
          c(
            "Profiled",
            "Complete: no attributes",
            "Not profiled",
            "Not requested",
            "In progress",
            "Failed"
          ),
          c(
            "#D1DDC7",
            "#D1DDC7",
            "#E9DFB5",
            "#D3CEC4",
            "#D1DDC7",
            "#E4D6B8"
          )
        )
      )

      tbl <- DT::formatStyle(
        tbl,
        columns = "file_status",
        backgroundColor = DT::styleEqual(
          c(
            "Active",
            "Missing"
          ),
          c(
            "#D1DDC7",
            "#E4D6B8"
          )
        )
      )

      tbl
    })

    # let the DT table be used as a selector
    observeEvent(
      input$saved_file_review_table_rows_selected,
      {
        selected_row <-
          input$saved_file_review_table_rows_selected

        if (
          is.null(selected_row) ||
          length(selected_row) == 0
        ) {
          return()
        }

        df <- saved_file_review_summary()

        if (is.null(df) || nrow(df) == 0) {
          return()
        }

        display_df <-
          prepare_saved_file_review_table(df)

        if (
          selected_row < 1L ||
          selected_row > nrow(display_df)
        ) {
          return()
        }

        data_file_id <-
          display_df$data_file_id[selected_row]

        updateSelectInput(
          session,
          "saved_file_select",
          selected = as.character(data_file_id)
        )

        shiny::updateTabsetPanel(
          session,
          "data_files_section",
          selected = "review"
        )
      }
    )

    spatial_summary <- reactive({
      df <- spatial_metadata()

      if (
        is.null(df) ||
        nrow(df) == 0L ||
        "message" %in% names(df)
      ) {
        return(df)
      }

      df_display <- data.table::as.data.table(
        data.table::copy(df)
      )

      required_columns <- list(
        id = NA_integer_,

        file_name = NA_character_,
        data_file_rel_path_full = NA_character_,
        file_path = NA_character_,
        data_file_format = NA_character_,

        spatial_status = NA_character_,
        spatial_type = NA_character_,
        layer_name = NA_character_,

        crs_auth_code = NA_character_,
        crs_name = NA_character_,
        crs_input = NA_character_,
        crs_status = NA_character_,

        bbox_native = NA_character_,
        bbox_status = NA_character_,

        geometry_type = NA_character_,
        feature_count = NA_integer_,

        raster_nrow = NA_integer_,
        raster_ncol = NA_integer_,
        raster_nlyr = NA_integer_,
        raster_res_x = NA_real_,
        raster_res_y = NA_real_,

        include_in_dataset_bbox = 0L,

        spatial_note = NA_character_,
        user_notes = NA_character_
      )

      for (field in names(required_columns)) {
        if (!field %in% names(df_display)) {
          df_display[
            ,
            (field) := required_columns[[field]]
          ]
        }
      }

      clean_scalar_text <- function(
        x,
        default = ""
      ) {
        if (
          is.null(x) ||
          length(x) == 0L ||
          is.na(x[1]) ||
          !nzchar(trimws(as.character(x[1])))
        ) {
          return(default)
        }

        trimws(
          as.character(x[1])
        )
      }

      format_count <- function(x) {
        x <- suppressWarnings(
          as.numeric(x[1])
        )

        if (!is.finite(x)) {
          return(NA_character_)
        }

        format(
          as.integer(x),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      }

      file_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          row <- df_display[i]

          file_name <- clean_scalar_text(
            row$file_name
          )

          if (nzchar(file_name)) {
            return(file_name)
          }

          relative_path <- clean_scalar_text(
            row$data_file_rel_path_full
          )

          if (nzchar(relative_path)) {
            return(
              basename(relative_path)
            )
          }

          file_path <- clean_scalar_text(
            row$file_path
          )

          if (nzchar(file_path)) {
            return(
              basename(file_path)
            )
          }

          "Unnamed spatial file"
        },
        character(1)
      )

      layer_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          layer <- clean_scalar_text(
            df_display$layer_name[i]
          )

          if (nzchar(layer)) {
            layer
          } else {
            "--"
          }
        },
        character(1)
      )

      crs_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          row <- df_display[i]

          authority <- clean_scalar_text(
            row$crs_auth_code
          )

          if (nzchar(authority)) {
            return(authority)
          }

          crs_name <- clean_scalar_text(
            row$crs_name
          )

          if (nzchar(crs_name)) {
            return(crs_name)
          }

          crs_input <- clean_scalar_text(
            row$crs_input
          )

          if (nzchar(crs_input)) {
            return("CRS definition stored")
          }

          "Missing or unknown"
        },
        character(1)
      )

      structure_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          row <- df_display[i]

          spatial_type <- tolower(
            clean_scalar_text(
              row$spatial_type
            )
          )

          if (identical(spatial_type, "raster")) {
            nrow_label <- format_count(
              row$raster_nrow
            )

            ncol_label <- format_count(
              row$raster_ncol
            )

            layer_count <- suppressWarnings(
              as.integer(
                row$raster_nlyr[1]
              )
            )

            dimensions <- if (
              !is.na(nrow_label) &&
              !is.na(ncol_label)
            ) {
              paste0(
                nrow_label,
                " x ",
                ncol_label,
                " cells"
              )
            } else {
              "Raster dimensions not recorded"
            }

            layers <- if (
              is.finite(layer_count)
            ) {
              paste0(
                layer_count,
                " layer",
                ifelse(
                  layer_count == 1L,
                  "",
                  "s"
                )
              )
            } else {
              "Layer count not recorded"
            }

            return(
              paste(
                dimensions,
                layers,
                sep = "; "
              )
            )
          }

          if (identical(spatial_type, "vector")) {
            geometry <- clean_scalar_text(
              row$geometry_type,
              default = "Geometry type not recorded"
            )

            feature_count <- format_count(
              row$feature_count
            )

            features <- if (
              is.na(feature_count)
            ) {
              "Feature count not recorded"
            } else {
              paste0(
                feature_count,
                " feature",
                ifelse(
                  as.integer(row$feature_count[1]) == 1L,
                  "",
                  "s"
                )
              )
            }

            return(
              paste(
                geometry,
                features,
                sep = "; "
              )
            )
          }

          geometry <- clean_scalar_text(
            row$geometry_type
          )

          if (nzchar(geometry)) {
            return(geometry)
          }

          "Spatial structure not recorded"
        },
        character(1)
      )

      cell_size_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          spatial_type <- tolower(
            clean_scalar_text(
              df_display$spatial_type[i]
            )
          )

          if (!identical(spatial_type, "raster")) {
            return("--")
          }

          format_raster_cell_size(
            df_display[i]
          )
        },
        character(1)
      )

      extent_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          clean_scalar_text(
            df_display$bbox_native[i],
            default = "Not recorded"
          )
        },
        character(1)
      )

      extraction_status_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          status <- clean_scalar_text(
            df_display$spatial_status[i],
            default = "Unknown"
          )

          if (identical(tolower(status), "ok")) {
            "OK"
          } else {
            status
          }
        },
        character(1)
      )

      bbox_status_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          status <- clean_scalar_text(
            df_display$bbox_status[i],
            default = "Not assessed"
          )

          if (identical(tolower(status), "valid")) {
            "Valid"
          } else {
            status
          }
        },
        character(1)
      )

      inclusion_labels <- data.table::fifelse(
        suppressWarnings(
          as.integer(
            df_display$include_in_dataset_bbox
          )
        ) == 1L,
        "Included",
        "Not included"
      )

      note_labels <- vapply(
        seq_len(nrow(df_display)),
        function(i) {
          user_note <- clean_scalar_text(
            df_display$user_notes[i]
          )

          if (nzchar(user_note)) {
            return(user_note)
          }

          extraction_note <- clean_scalar_text(
            df_display$spatial_note[i]
          )

          if (nzchar(extraction_note)) {
            return(extraction_note)
          }

          ""
        },
        character(1)
      )

      output <- data.table::data.table(
        file = file_labels,
        layer = layer_labels,
        format = as.character(
          df_display$data_file_format
        ),
        extraction_status =
          extraction_status_labels,
        spatial_type = as.character(
          df_display$spatial_type
        ),
        structure = structure_labels,
        native_cell_size = cell_size_labels,
        native_crs = crs_labels,
        native_extent = extent_labels,
        bbox_status = bbox_status_labels,
        dataset_bbox = inclusion_labels,
        review_note = note_labels
      )

      data.table::setnames(
        output,
        old = c(
          "file",
          "layer",
          "format",
          "extraction_status",
          "spatial_type",
          "structure",
          "native_cell_size",
          "native_crs",
          "native_extent",
          "bbox_status",
          "dataset_bbox",
          "review_note"
        ),
        new = c(
          "File",
          "Layer",
          "Format",
          "Extraction status",
          "Spatial type",
          "Structure",
          "Native cell size",
          "Native CRS",
          "Native extent",
          "BBox status",
          "Dataset bbox",
          "Review note"
        )
      )

      output[]
    })

    saved_data_files <- reactive({
      state$refresh_data_files_saved
      if (is.null(state$current_dataset_id) || length(state$current_dataset_id) == 0 || is.na(state$current_dataset_id)) {
        return(data.frame())
      }
      get_data_files_for_dataset(state$current_dataset_id, state$data_pond)
    })

    selected_saved_file_record <- reactive({
      selected_id <- input$saved_file_select
      df <- saved_data_files()

      if (
        is.null(selected_id) ||
        length(selected_id) == 0L ||
        is.na(selected_id[1]) ||
        !nzchar(
          trimws(
            as.character(
              selected_id[1]
            )
          )
        ) ||
        is.null(df) ||
        nrow(as.data.frame(df)) == 0L
      ) {
        return(NULL)
      }

      row <- df[
        as.character(df$id) ==
          as.character(selected_id[1]),
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0L) {
        return(NULL)
      }

      row[1, , drop = FALSE]
    })

    output$selected_saved_file_status <- shiny::renderUI({
      row <- selected_saved_file_record()

      if (
        is.null(row) ||
        nrow(as.data.frame(row)) == 0L
      ) {
        return(
          shiny::tags$div(
            style = paste(
              "margin-top:25px;",
              "color:#666;"
            ),

            "No saved data file is selected."
          )
        )
      }

      show_value <- function(
        x,
        default = "Not recorded"
      ) {
        if (
          is.null(x) ||
          length(x) == 0L ||
          is.na(x[1]) ||
          !nzchar(trimws(as.character(x[1])))
        ) {
          return(default)
        }

        trimws(
          as.character(x[1])
        )
      }

      file_status <- if (
        "active" %in% names(row) &&
        !is.na(row$active[1]) &&
        as.integer(row$active[1]) != 1L
      ) {
        "Missing from filesystem"
      } else {
        "Active saved file"
      }

      shiny::tags$div(
        style = paste(
          "margin-top:22px;",
          "padding:8px;",
          "background:#fff;",
          "border:1px solid #ddd;"
        ),

        shiny::strong(
          show_value(
            row$file_name,
            default = "Unnamed saved file"
          )
        ),

        shiny::tags$br(),

        shiny::tags$small(
          paste(
            show_value(
              row$data_file_format,
              default = "Unknown format"
            ),
            "|",
            file_status
          )
        )
      )
    })

    has_saved_file_inventory <- shiny::reactive({
      df <- saved_data_files()
      !is.null(df) && nrow(df) > 0L
    })


    saved_file_inventory_count <- shiny::reactive({
      df <- saved_data_files()
      if (is.null(df) || nrow(df) == 0L) return(0L)
      as.integer(nrow(df))
    })

    inventory_counts <- shiny::reactive({
      saved_df <- saved_data_files()
      live_df <- files()

      saved_total <- if (
        is.null(saved_df) ||
        nrow(as.data.frame(saved_df)) == 0L
      ) {
        0L
      } else {
        as.integer(
          nrow(saved_df)
        )
      }

      saved_active <- if (
        saved_total == 0L ||
        !"active" %in% names(saved_df)
      ) {
        saved_total
      } else {
        as.integer(
          sum(
            is.na(saved_df$active) |
              as.integer(saved_df$active) == 1L,
            na.rm = TRUE
          )
        )
      }

      saved_missing <- if (
        saved_total == 0L ||
        !"active" %in% names(saved_df)
      ) {
        0L
      } else {
        as.integer(
          sum(
            !is.na(saved_df$active) &
              as.integer(saved_df$active) != 1L,
            na.rm = TRUE
          )
        )
      }

      live_available <- (
        !is.null(live_df) &&
          nrow(as.data.frame(live_df)) > 0L &&
          !"message" %in% names(live_df)
      )

      live_rows <- if (live_available) {
        as.integer(
          nrow(live_df)
        )
      } else {
        0L
      }

      live_logical <- if (
        live_available &&
        "primary_dataset" %in% names(live_df)
      ) {
        as.integer(
          sum(
            live_df$primary_dataset %in% TRUE,
            na.rm = TRUE
          )
        )
      } else {
        0L
      }

      live_components <- if (
        live_available &&
        "file_role" %in% names(live_df)
      ) {
        as.integer(
          sum(
            live_df$file_role %in%
              c(
                "sidecar",
                "dataset_container_component",
                "container_support"
              ),
            na.rm = TRUE
          )
        )
      } else {
        0L
      }

      ignored_files <- attr(
        live_df,
        "ignored_files"
      )

      if (
        is.null(ignored_files) ||
        length(ignored_files) == 0L ||
        is.na(ignored_files[1])
      ) {
        ignored_files <- 0L
      }

      list(
        saved_total = saved_total,
        saved_active = saved_active,
        saved_missing = saved_missing,
        live_rows = live_rows,
        live_logical = live_logical,
        live_components =
          live_components,
        ignored_files =
          as.integer(ignored_files[1])
      )
    })

    output$inventory_summary <- shiny::renderUI({
      counts <- inventory_counts()

      shiny::tags$div(
        style = paste(
          "padding:8px 10px;",
          "margin-bottom:10px;",
          "background:#F8F4EA;",
          "border:1px solid #ddd;"
        ),

        shiny::tags$span(
          shiny::strong(
            "Saved:"
          ),
          paste0(
            " ",
            counts$saved_active,
            " active"
          )
        ),

        shiny::tags$span(
          style = "margin-left:18px;",

          shiny::strong(
            "Missing:"
          ),
          paste0(
            " ",
            counts$saved_missing
          )
        ),

        shiny::tags$span(
          style = "margin-left:18px;",

          shiny::strong(
            "Live logical files:"
          ),
          paste0(
            " ",
            counts$live_logical
          )
        ),

        shiny::tags$span(
          style = "margin-left:18px;",

          shiny::strong(
            "Components:"
          ),
          paste0(
            " ",
            counts$live_components
          )
        ),

        shiny::tags$span(
          style = "margin-left:18px;",

          shiny::strong(
            "Ignored:"
          ),
          paste0(
            " ",
            counts$ignored_files
          )
        )
      )
    })

    output$inventory_view_note <- shiny::renderUI({
      view <- input$inventory_view

      if (identical(
        view,
        "live"
      )) {
        return(
          shiny::tags$small(
            style = paste(
              "color:#666;",
              "display:block;",
              "margin-top:6px;"
            ),

            paste(
              "This table represents the current unsaved live scan.",
              "Use the live-scan controls below to rescan or save",
              "the logical inventory to the database."
            )
          )
        )
      }

      shiny::tags$small(
        style = paste(
          "color:#666;",
          "display:block;",
          "margin-top:6px;"
        ),

        paste(
          "This table represents the inventory currently saved",
          "in the metadata database. Missing records are retained",
          "for audit and lifecycle review."
        )
      )
    })

    saved_attributes <- reactive({
      state$refresh_attributes
      if (is.null(state$current_dataset_id) || length(state$current_dataset_id) == 0 || is.na(state$current_dataset_id)) {
        return(data.frame())
      }
      get_dataset_attributes(state$current_dataset_id, state$data_pond)
    })

    output$live_scan_summary <- shiny::renderUI({
      saved_count <- saved_file_inventory_count()

      if (saved_count > 0L) {
        return(
          tags$p(
            style = paste(
              "margin:4px 0 0 0;",
              "color:#666;"
            ),

            paste0(
              saved_count,
              " saved logical data file",
              ifelse(
                saved_count == 1L,
                "",
                "s"
              ),
              " exist for this dataset. ",
              paste(
                "Open the Live scan area only when comparing",
                "the saved inventory with the filesystem",
                "or rescanning changed files."
              )
            )
          )
        )
      }

      tags$p(
        style = paste(
          "margin:4px 0 0 0;",
          "color:#666;"
        ),

        paste(
          "No saved data-file inventory exists.",
          "Open this section to scan and save files."
        )
      )
    })

    shiny::observe({
      dataset_id <- state$current_dataset_id

      dataset_key <- if (
        is.null(dataset_id) ||
        length(dataset_id) == 0L ||
        is.na(dataset_id[1])
      ) {
        "<new-record>"
      } else {
        as.character(
          dataset_id[1]
        )
      }

      # Force evaluation after the selected dataset changes.
      has_saved <- has_saved_file_inventory()

      if (
        identical(
          live_scan_dataset_key(),
          dataset_key
        )
      ) {
        return()
      }

      live_scan_dataset_key(
        dataset_key
      )

      shiny::updateTabsetPanel(
        session,
        "data_files_section",
        selected = "inventory"
      )

      live_scan_programmatic_update(
        TRUE
      )

      shiny::updateCheckboxInput(
        session,
        "show_live_scan_panel",
        value = !has_saved
      )

      session$onFlushed(
        function() {
          live_scan_programmatic_update(
            FALSE
          )
        },
        once = TRUE
      )
    })

    shiny::observeEvent(
      input$show_live_scan_panel,
      {
        if (
          isTRUE(
            live_scan_programmatic_update()
          )
        ) {
          return()
        }

        # No action is needed. Reading the input here makes the
        # distinction between user and programmatic changes explicit.
        invisible(
          input$show_live_scan_panel
        )
      },
      ignoreInit = TRUE
    )

    spatial_metadata_refresh <- reactiveVal(0L)
    manual_extent_refresh <- reactiveVal(0L)

    manual_extents <- reactive({
      manual_extent_refresh()

      dataset_id <- state$current_dataset_id
      base_path <- state$data_pond

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0L ||
        is.na(dataset_id[1]) ||
        is.null(base_path) ||
        length(base_path) == 0L ||
        is.na(base_path[1]) ||
        !nzchar(trimws(base_path[1]))
      ) {
        return(data.frame())
      }

      get_manual_spatial_extents(
        dataset_id = dataset_id,
        base_path = base_path,
        active_only = FALSE
      )
    })

    observe({
      extents <- manual_extents()

      choices <- c(
        "-- New manual extent --" = ""
      )

      if (
        !is.null(extents) &&
        nrow(extents) > 0L
      ) {
        labels <- paste0(
          extents$extent_label,
          ifelse(
            as.integer(extents$active) == 1L,
            "",
            " [inactive]"
          )
        )

        choices <- c(
          choices,
          stats::setNames(
            as.character(extents$id),
            labels
          )
        )
      }

      current <- isolate(
        input$manual_extent_select
      ) %||% ""

      if (!current %in% unname(choices)) {
        current <- ""
      }

      updateSelectInput(
        session,
        "manual_extent_select",
        choices = choices,
        selected = current
      )
    })


    observe({
      files <- saved_data_files()

      choices <- c(
        "Dataset-level extent" = ""
      )

      if (
        !is.null(files) &&
        nrow(as.data.frame(files)) > 0L &&
        !"message" %in% names(files) &&
        "id" %in% names(files)
      ) {
        file_labels <- if (
          "data_file_rel_path_full" %in% names(files)
        ) {
          paste0(
            files$file_name,
            " \u2014 ",
            files$data_file_rel_path_full
          )
        } else {
          as.character(
            files$file_name
          )
        }

        choices <- c(
          choices,
          stats::setNames(
            as.character(files$id),
            file_labels
          )
        )
      }

      current <- isolate(
        input$manual_extent_data_file_id
      ) %||% ""

      if (!current %in% unname(choices)) {
        current <- ""
      }

      updateSelectInput(
        session,
        "manual_extent_data_file_id",
        choices = choices,
        selected = current
      )
    })


    observeEvent(
      input$manual_extent_new,
      {
        clear_manual_extent_form()
      }
    )


    observeEvent(
      state$current_dataset_id,
      {
        clear_manual_extent_form()

        manual_extent_refresh(
          manual_extent_refresh() + 1L
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$manual_extent_select,
      {
        selected_id <- suppressWarnings(
          as.integer(
            input$manual_extent_select
          )
        )

        if (
          is.na(selected_id) ||
          identical(
            input$manual_extent_select,
            ""
          )
        ) {
          return()
        }

        extents <- manual_extents()

        record <- extents[
          as.integer(extents$id) ==
            selected_id,
          ,
          drop = FALSE
        ]

        if (nrow(record) == 0L) {
          return()
        }

        crs_display <- record$crs_auth_code[1]

        if (
          is.na(crs_display) ||
          !nzchar(trimws(crs_display))
        ) {
          crs_display <- record$crs_input[1]
        }

        linked_file <- if (
          is.na(record$data_file_id[1])
        ) {
          ""
        } else {
          as.character(
            record$data_file_id[1]
          )
        }

        updateTextInput(
          session,
          "manual_extent_label",
          value = record$extent_label[1]
        )

        updateSelectInput(
          session,
          "manual_extent_data_file_id",
          selected = linked_file
        )

        updateTextInput(
          session,
          "manual_extent_crs",
          value = crs_display
        )

        updateNumericInput(
          session,
          "manual_extent_xmin",
          value = record$xmin[1]
        )

        updateNumericInput(
          session,
          "manual_extent_ymin",
          value = record$ymin[1]
        )

        updateNumericInput(
          session,
          "manual_extent_xmax",
          value = record$xmax[1]
        )

        updateNumericInput(
          session,
          "manual_extent_ymax",
          value = record$ymax[1]
        )

        updateTextAreaInput(
          session,
          "manual_extent_note",
          value = record$source_note[1] %||% ""
        )

        updateCheckboxInput(
          session,
          "manual_extent_include",
          value = as.integer(
            record$include_in_dataset_bbox[1]
          ) == 1L
        )

        updateCheckboxInput(
          session,
          "manual_extent_active",
          value = as.integer(
            record$active[1]
          ) == 1L
        )
      }
    )

    manual_extent_validation_result <- reactive({
      has_coordinates <- all(
        is.finite(
          suppressWarnings(
            as.numeric(
              c(
                input$manual_extent_xmin,
                input$manual_extent_ymin,
                input$manual_extent_xmax,
                input$manual_extent_ymax
              )
            )
          )
        )
      )

      has_crs <- (
        !is.null(input$manual_extent_crs) &&
          length(input$manual_extent_crs) > 0L &&
          !is.na(input$manual_extent_crs[1]) &&
          nzchar(trimws(input$manual_extent_crs[1]))
      )

      if (
        !isTRUE(has_coordinates) ||
        !isTRUE(has_crs)
      ) {
        preview <- spatial_bbox_unavailable(
          status = "manual_extent_incomplete",
          message = paste(
            "Enter all four coordinates and an explicit CRS",
            "to validate the manual extent."
          )
        )

        return(
          list(
            valid = FALSE,
            status = preview$status,
            message = preview$message,
            record = NULL,
            preview = preview
          )
        )
      }

      validate_manual_spatial_extent(
        crs_value = input$manual_extent_crs,
        xmin = input$manual_extent_xmin,
        ymin = input$manual_extent_ymin,
        xmax = input$manual_extent_xmax,
        ymax = input$manual_extent_ymax
      )
    })

    output$manual_extent_validation <- renderUI({
      validation <- manual_extent_validation_result()

      tags$div(
        style = paste(
          "padding:8px;",
          if (isTRUE(validation$valid)) {
            "background:#D1DDC7;"
          } else {
            "background:#E9DFB5;"
          },
          if (isTRUE(validation$valid)) {
            "border:1px solid #526657;"
          } else {
            "border:1px solid #C3A35B;"
          },
          "margin-top:8px;",
          "margin-bottom:8px;"
        ),

        strong(
          if (isTRUE(validation$valid)) {
            "Valid manual extent: "
          } else {
            "Manual extent not ready: "
          }
        ),

        validation$message
      )
    })

    output$manual_extent_map <- leaflet::renderLeaflet({
      validation <- manual_extent_validation_result()
      preview <- validation$preview

      map <- leaflet::leaflet()

      map <- leaflet::addProviderTiles(
        map,
        provider = "CartoDB.Positron"
      )

      if (!isTRUE(preview$eligible)) {
        return(
          leaflet::setView(
            map,
            lng = 0,
            lat = 0,
            zoom = 1
          )
        )
      }

      map <- leaflet::addRectangles(
        map,
        lng1 = preview$west,
        lat1 = preview$south,
        lng2 = preview$east,
        lat2 = preview$north,
        weight = 3,
        fillOpacity = 0.12,
        label = "Manual extent transformed to EPSG:4326"
      )

      leaflet::fitBounds(
        map,
        lng1 = preview$west,
        lat1 = preview$south,
        lng2 = preview$east,
        lat2 = preview$north
      )
    })

    observeEvent(
      input$manual_extent_save,
      {
        dataset_id <- state$current_dataset_id

        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0L ||
          is.na(dataset_id[1])
        ) {
          showNotification(
            "Select or save a dataset before adding a manual extent.",
            type = "warning"
          )

          return()
        }

        validation <- manual_extent_validation_result()

        if (!isTRUE(validation$valid)) {
          showNotification(
            validation$message,
            type = "error",
            duration = 8
          )

          return()
        }

        extent_id <- suppressWarnings(
          as.integer(
            input$manual_extent_select
          )
        )

        if (
          is.null(input$manual_extent_select) ||
          identical(input$manual_extent_select, "") ||
          is.na(extent_id)
        ) {
          extent_id <- NULL
        }

        linked_file_id <- suppressWarnings(
          as.integer(
            input$manual_extent_data_file_id
          )
        )

        if (
          is.null(input$manual_extent_data_file_id) ||
          identical(
            input$manual_extent_data_file_id,
            ""
          ) ||
          is.na(linked_file_id)
        ) {
          linked_file_id <- NULL
        }

        tryCatch(
          {
            result <- save_manual_spatial_extent(
              dataset_id = dataset_id,
              base_path = state$data_pond,
              extent_label = input$manual_extent_label,
              crs_value = input$manual_extent_crs,
              xmin = input$manual_extent_xmin,
              ymin = input$manual_extent_ymin,
              xmax = input$manual_extent_xmax,
              ymax = input$manual_extent_ymax,
              data_file_id = linked_file_id,
              source_note = input$manual_extent_note,
              include_in_dataset_bbox =
                input$manual_extent_include,
              active = input$manual_extent_active,
              manual_extent_id = extent_id,
              user_id = state$active_user_id
            )

            manual_extent_refresh(
              manual_extent_refresh() + 1L
            )

            spatial_metadata_refresh(
              spatial_metadata_refresh() + 1L
            )
            state$refresh_spatial_metadata <-
              state$refresh_spatial_metadata + 1L

            updateSelectInput(
              session,
              "manual_extent_select",
              selected = as.character(
                result$id
              )
            )

            showNotification(
              "Manual spatial extent saved.",
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              conditionMessage(e),
              type = "error",
              duration = 8
            )
          }
        )
      }
    )


    observeEvent(
      input$manual_extent_deactivate,
      {
        extent_id <- suppressWarnings(
          as.integer(
            input$manual_extent_select
          )
        )

        if (
          is.na(extent_id) ||
          identical(
            input$manual_extent_select,
            ""
          )
        ) {
          showNotification(
            "Select a manual extent first.",
            type = "warning"
          )

          return()
        }

        tryCatch(
          {
            set_manual_spatial_extent_active(
              manual_extent_id = extent_id,
              active = FALSE,
              base_path = state$data_pond,
              user_id = state$active_user_id
            )

            manual_extent_refresh(
              manual_extent_refresh() + 1L
            )

            spatial_metadata_refresh(
              spatial_metadata_refresh() + 1L
            )

          state$refresh_spatial_metadata <-
            state$refresh_spatial_metadata + 1L

            clear_manual_extent_form()

            showNotification(
              "Manual extent deactivated.",
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              conditionMessage(e),
              type = "error",
              duration = 8
            )
          }
        )
      }
    )

    output$manual_extent_table <- DT::renderDT({
      extents <- manual_extents()

      if (
        is.null(extents) ||
        nrow(as.data.frame(extents)) == 0L
      ) {
        return(
          DT::datatable(
            data.frame(
              message = "No manual spatial extents have been saved."
            ),
            rownames = FALSE,
            options = list(
              dom = "t"
            )
          )
        )
      }

      display <- data.table::as.data.table(
        data.table::copy(extents)
      )

      display[
        ,
        linked_source := data.table::fifelse(
          !is.na(file_name) &
            nzchar(trimws(file_name)),
          file_name,
          "Dataset-level"
        )
      ]

      display[
        ,
        inclusion := data.table::fifelse(
          as.integer(include_in_dataset_bbox) == 1L,
          "Included",
          "Not included"
        )
      ]

      display[
        ,
        record_status := data.table::fifelse(
          as.integer(active) == 1L,
          "Active",
          "Inactive"
        )
      ]

      display[
        ,
        crs_display := data.table::fcoalesce(
          crs_auth_code,
          crs_input
        )
      ]

      display <- display[
        ,
        .(
          id,
          record_status,
          inclusion,
          extent_label,
          extent_scope,
          linked_source,
          crs = crs_display,
          bbox_native,
          source_note,
          updated_at,
          updated_by_name
        )
      ]

      DT::datatable(
        display,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        options = list(
          pageLength = 10L,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        colnames = c(
          "ID",
          "Status",
          "Dataset bbox",
          "Extent label",
          "Scope",
          "Linked source",
          "CRS",
          "Native bbox",
          "Source / justification",
          "Updated",
          "Updated by"
        )
      )
    })

    observeEvent(
      input$manual_extent_table_rows_selected,
      {
        row_index <- input$manual_extent_table_rows_selected
        extents <- manual_extents()

        if (
          length(row_index) != 1L ||
          nrow(extents) < row_index
        ) {
          return()
        }

        updateSelectInput(
          session,
          "manual_extent_select",
          selected = as.character(
            extents$id[row_index]
          )
        )
      }
    )

    dataset_bbox_summary <- reactive({
      spatial_metadata_refresh()

      dataset_id <- state$current_dataset_id
      base_path <- state$data_pond

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0L ||
        is.na(dataset_id[1]) ||
        is.null(base_path) ||
        length(base_path) == 0L ||
        is.na(base_path[1]) ||
        !nzchar(trimws(base_path[1]))
      ) {
        return(NULL)
      }

      get_dataset_spatial_summary(
        dataset_id = dataset_id,
        base_path = base_path
      )
    })


    dataset_bbox_contributors <- reactive({
      spatial_metadata_refresh()

      dataset_id <- state$current_dataset_id
      base_path <- state$data_pond

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0L ||
        is.na(dataset_id[1]) ||
        is.null(base_path) ||
        length(base_path) == 0L ||
        is.na(base_path[1]) ||
        !nzchar(trimws(base_path[1]))
      ) {
        return(data.frame())
      }

      get_dataset_bbox_contributors(
        dataset_id = dataset_id,
        base_path = base_path
      )
    })

    output$dataset_bbox_status <- renderUI({
      summary <- dataset_bbox_summary()

      if (
        is.null(summary) ||
        nrow(as.data.frame(summary)) == 0L
      ) {
        return(
          tags$div(
            style = paste(
              "padding:8px;",
              "background:#F8F4EA;",
              "border:1px solid #ccc;",
              "margin-bottom:8px;"
            ),
            strong("Not derived: "),
            paste(
              "No dataset bounding-box summary exists yet.",
              "Select one or more eligible records for inclusion."
            )
          )
        )
      }

      status <- as.character(
        summary$bbox_status[1]
      )

      is_valid <- identical(
        status,
        "valid"
      )

      tags$div(
        style = paste(
          "padding:8px;",
          if (is_valid) {
            "background:#D1DDC7;"
          } else {
            "background:#E9DFB5;"
          },
          if (is_valid) {
            "border:1px solid #526657;"
          } else {
            "border:1px solid #C3A35B;"
          },
          "margin-bottom:8px;"
        ),

        strong(
          if (is_valid) {
            "Derived extent: "
          } else {
            "Extent unavailable: "
          }
        ),

        summary$status_message[1],

        tags$br(),

        tags$small(
          paste(
            "Included:",
            summary$included_source_count[1],
            "| Eligible:",
            summary$eligible_source_count[1],
            "| Skipped:",
            summary$skipped_source_count[1]
          )
        ),

        if (is_valid) {
          tagList(
            tags$br(),
            tags$small(
              paste(
                "EPSG:4326 bbox:",
                summary$bbox_wgs84[1]
              )
            )
          )
        }
      )
    })

    output$dataset_bbox_map <- leaflet::renderLeaflet({
      summary <- dataset_bbox_summary()
      contributors <- dataset_bbox_contributors()

      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = TRUE,
          attributionControl = TRUE
        )
      )

      map <- leaflet::addProviderTiles(
        map,
        provider = "CartoDB.Positron"
      )

      if (
        is.null(summary) ||
        nrow(as.data.frame(summary)) == 0L ||
        !identical(
          as.character(summary$bbox_status[1]),
          "valid"
        )
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

      eligible <- data.table::as.data.table(
        data.table::copy(contributors)
      )

      if (nrow(eligible) > 0L) {
        eligible <- eligible[
          eligible == TRUE &
            is.finite(west) &
            is.finite(south) &
            is.finite(east) &
            is.finite(north)
        ]

        if (nrow(eligible) > 0L) {
          for (i in seq_len(nrow(eligible))) {
            map <- leaflet::addRectangles(
              map,
              lng1 = eligible$west[i],
              lat1 = eligible$south[i],
              lng2 = eligible$east[i],
              lat2 = eligible$north[i],
              weight = 1,
              fillOpacity = 0.04,
              label = eligible$source_label[i],
              group = "Included sources"
            )
          }
        }
      }

      map <- leaflet::addRectangles(
        map,
        lng1 = summary$xmin[1],
        lat1 = summary$ymin[1],
        lng2 = summary$xmax[1],
        lat2 = summary$ymax[1],
        weight = 3,
        fillOpacity = 0.10,
        label = paste(
          "Derived dataset bbox from",
          summary$eligible_source_count[1],
          "source(s)"
        ),
        group = "Derived dataset bbox"
      )

      map <- leaflet::addLayersControl(
        map,
        overlayGroups = c(
          "Included sources",
          "Derived dataset bbox"
        ),
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      )

      leaflet::fitBounds(
        map,
        lng1 = summary$xmin[1],
        lat1 = summary$ymin[1],
        lng2 = summary$xmax[1],
        lat2 = summary$ymax[1]
      )
    })

    output$dataset_bbox_contributors_table <- DT::renderDT({
      contributors <- dataset_bbox_contributors()

      if (
        is.null(contributors) ||
        nrow(as.data.frame(contributors)) == 0L
      ) {
        return(
          DT::datatable(
            data.frame(
              message = paste(
                "No spatial records or manual extents are currently",
                "selected for dataset-bbox inclusion."
              )
            ),
            rownames = FALSE,
            options = list(
              dom = "t"
            )
          )
        )
      }

      contributors <- data.table::as.data.table(
        data.table::copy(contributors)
      )

      required_columns <- list(
        source_type = NA_character_,
        source_label = NA_character_,
        data_file_format = NA_character_,
        source_crs = NA_character_,
        eligible = FALSE,
        eligibility_status = NA_character_,
        eligibility_message = NA_character_,
        west = NA_real_,
        south = NA_real_,
        east = NA_real_,
        north = NA_real_,
        bbox_wgs84 = NA_character_
      )

      for (field in names(required_columns)) {
        if (!field %in% names(contributors)) {
          contributors[
            ,
            (field) := required_columns[[field]]
          ]
        }
      }

      # Defensive reconstruction for contributor results generated by an
      # older version of derive_dataset_bbox_from_records().
      missing_bbox_text <- (
        is.na(contributors$bbox_wgs84) |
          !nzchar(trimws(contributors$bbox_wgs84))
      )

      valid_bbox <- (
        contributors$eligible %in% TRUE &
          is.finite(contributors$west) &
          is.finite(contributors$south) &
          is.finite(contributors$east) &
          is.finite(contributors$north)
      )

      contributors[
        missing_bbox_text & valid_bbox,
        bbox_wgs84 := paste(
          format(
            round(west, 6L),
            trim = TRUE,
            scientific = FALSE
          ),
          format(
            round(south, 6L),
            trim = TRUE,
            scientific = FALSE
          ),
          format(
            round(east, 6L),
            trim = TRUE,
            scientific = FALSE
          ),
          format(
            round(north, 6L),
            trim = TRUE,
            scientific = FALSE
          ),
          sep = "; "
        )
      ]

      contributors[
        is.na(source_type) |
          !nzchar(trimws(source_type)),
        source_type := "extracted_spatial_record"
      ]

      contributors[
        is.na(source_label) |
          !nzchar(trimws(source_label)),
        source_label := "Unnamed spatial source"
      ]

      display <- contributors[
        ,
        .(
          source_type,
          source = source_label,
          format = data_file_format,
          source_crs,
          eligibility = eligibility_status,
          bbox_wgs84,
          message = eligibility_message
        )
      ]

      DT::datatable(
        display,
        rownames = FALSE,
        selection = "none",
        filter = "top",
        options = list(
          pageLength = 10L,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        colnames = c(
          "Source type",
          "File / layer / extent",
          "Format",
          "Source CRS",
          "Eligibility",
          "EPSG:4326 bbox",
          "Assessment"
        )
      )
    })

    saved_spatial_records <- reactive({
      spatial_metadata_refresh()

      dataset_id <- state$current_dataset_id
      base_path <- state$data_pond

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0 ||
        is.na(dataset_id[1]) ||
        is.null(base_path) ||
        length(base_path) == 0 ||
        is.na(base_path[1]) ||
        base_path[1] == ""
      ) {
        return(data.frame())
      }

      get_saved_data_file_spatial_metadata(
        dataset_id = dataset_id,
        base_path = base_path
      )
    })

    observe({
      df <- saved_spatial_records()

      if (
        is.null(df) ||
        nrow(df) == 0
      ) {
        updateSelectInput(
          session,
          "spatial_record_select",
          choices = character(),
          selected = character(0)
        )

        updateTextAreaInput(
          session,
          "spatial_user_notes",
          value = ""
        )

        return()
      }

      required_columns <- list(
        id = NA_integer_,
        file_name = NA_character_,
        data_file_rel_path_full = NA_character_,
        layer_name = NA_character_,
        data_file_format = NA_character_
      )

      df <- data.table::as.data.table(
        data.table::copy(df)
      )

      for (nm in names(required_columns)) {
        if (!nm %in% names(df)) {
          df[, (nm) := required_columns[[nm]]]
        }
      }

      file_label <- ifelse(
        !is.na(df$file_name) &
          nzchar(trimws(df$file_name)),
        df$file_name,
        basename(df$data_file_rel_path_full)
      )

      layer_label <- ifelse(
        !is.na(df$layer_name) &
          nzchar(trimws(df$layer_name)),
        paste0(" | ", df$layer_name),
        ""
      )

      format_label <- ifelse(
        !is.na(df$data_file_format) &
          nzchar(trimws(df$data_file_format)),
        paste0(" [", df$data_file_format, "]"),
        ""
      )

      labels <- paste0(
        file_label,
        layer_label,
        format_label
      )

      choices <- setNames(
        as.character(df$id),
        labels
      )

      valid_ids <- unname(choices)
      current_id <- input$spatial_record_select

      if (
        is.null(current_id) ||
        length(current_id) == 0 ||
        is.na(current_id[1]) ||
        !as.character(current_id[1]) %in% valid_ids
      ) {
        current_id <- valid_ids[1]
      } else {
        current_id <- as.character(current_id[1])
      }

      updateSelectInput(
        session,
        "spatial_record_select",
        choices = choices,
        selected = current_id
      )
    })

    selected_spatial_record <- reactive({
      selected_id <- input$spatial_record_select
      df <- saved_spatial_records()

      if (
        is.null(selected_id) ||
        length(selected_id) == 0 ||
        is.na(selected_id[1]) ||
        identical(as.character(selected_id[1]), "") ||
        is.null(df) ||
        nrow(df) == 0
      ) {
        return(NULL)
      }

      row <- df[
        as.character(df$id) ==
          as.character(selected_id[1]),
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(NULL)
      }

      row[1, , drop = FALSE]
    })

    selected_spatial_preview <- reactive({
      record <- selected_spatial_record()

      if (
        is.null(record) ||
        nrow(as.data.frame(record)) == 0L
      ) {
        return(
          spatial_bbox_unavailable(
            status = "no_record",
            message = "No spatial metadata record is selected."
          )
        )
      }

      spatial_bbox_preview(
        record
      )
    })

    output$spatial_bbox_status <- renderUI({
      preview <- selected_spatial_preview()

      if (isTRUE(preview$eligible)) {
        background <- "#D1DDC7"
        border <- "#526657"
        heading <- "Eligible"
      } else {
        background <- "#E9DFB5"
        border <- "#C3A35B"
        heading <- "Preview unavailable"
      }

      tags$div(
        style = paste(
          "padding:8px;",
          paste0("background:", background, ";"),
          paste0("border:1px solid ", border, ";"),
          "margin-bottom:8px;"
        ),
        strong(
          paste0(
            heading,
            ": "
          )
        ),
        preview$message
      )
    })

    output$spatial_bbox_map <- leaflet::renderLeaflet({
      preview <- selected_spatial_preview()

      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = TRUE,
          attributionControl = TRUE
        )
      )

      map <- leaflet::addProviderTiles(
        map,
        provider = "CartoDB.Positron"
      )

      if (!isTRUE(preview$eligible)) {
        return(
          leaflet::setView(
            map,
            lng = 0,
            lat = 0,
            zoom = 1
          )
        )
      }

      map <- leaflet::addRectangles(
        map,
        lng1 = preview$west,
        lat1 = preview$south,
        lng2 = preview$east,
        lat2 = preview$north,
        weight = 2,
        fillOpacity = 0.12,
        label = paste(
          "Bounding box transformed to",
          preview$target_crs
        )
      )

      leaflet::fitBounds(
        map,
        lng1 = preview$west,
        lat1 = preview$south,
        lng2 = preview$east,
        lat2 = preview$north
      )
    })

    observe({
      row <- selected_spatial_record()

      if (is.null(row)) {
        updateTextAreaInput(
          session,
          "spatial_user_notes",
          value = ""
        )

        return()
      }

      notes_value <- row$user_notes[1]

      if (
        is.null(notes_value) ||
        length(notes_value) == 0 ||
        is.na(notes_value) ||
        !nzchar(trimws(as.character(notes_value)))
      ) {
        notes_value <- ""
      } else {
        notes_value <- as.character(notes_value)
      }

      updateTextAreaInput(
        session,
        "spatial_user_notes",
        value = notes_value
      )
    })

    output$spatial_bbox_inclusion_ui <- renderUI({
      record <- selected_spatial_record()
      preview <- selected_spatial_preview()

      if (
        is.null(record) ||
        nrow(as.data.frame(record)) == 0L
      ) {
        return(
          tags$small(
            style = "color:#666;",
            "Select a saved spatial record."
          )
        )
      }

      included <- FALSE

      if (
        "include_in_dataset_bbox" %in% names(record) &&
        !is.na(record$include_in_dataset_bbox[1])
      ) {
        included <- as.integer(
          record$include_in_dataset_bbox[1]
        ) == 1L
      }

      control <- checkboxInput(
        session$ns(
          "include_in_dataset_bbox"
        ),
        paste(
          "Include this file or layer in the",
          "derived dataset bounding box"
        ),
        value = included
      )

      if (!isTRUE(preview$eligible)) {
        control <- htmltools::tagQuery(
          control
        )$
          find("input")$
          addAttrs(
            disabled = "disabled"
          )$
          allTags()

        return(
          tagList(
            control,
            tags$small(
              style = "color:#8a6d3b;",
              paste(
                "Inclusion is disabled until the record has a valid",
                "bbox, an explicit CRS and a successful EPSG:4326",
                "transformation."
              )
            )
          )
        )
      }

      tagList(
        control,

        actionButton(
          session$ns(
            "save_bbox_inclusion"
          ),
          "Save bbox inclusion"
        )
      )
    })

    observeEvent(
      input$save_bbox_inclusion,
      {
        record <- selected_spatial_record()
        preview <- selected_spatial_preview()

        if (
          is.null(record) ||
          nrow(as.data.frame(record)) == 0L
        ) {
          showNotification(
            "Select a spatial metadata record first.",
            type = "warning"
          )

          return()
        }

        if (!isTRUE(preview$eligible)) {
          showNotification(
            preview$message,
            type = "warning",
            duration = 8
          )

          return()
        }

        include_value <- isTRUE(
          input$include_in_dataset_bbox
        )

        tryCatch(
          {
            update_spatial_bbox_inclusion(
              spatial_metadata_id = record$id[1],
              include_in_dataset_bbox = include_value,
              base_path = state$data_pond,
              user_id = state$active_user_id
            )

            spatial_metadata_refresh(
              spatial_metadata_refresh() + 1L
            )
            state$refresh_spatial_metadata <-
              state$refresh_spatial_metadata + 1L

            saved_spatial <-
              get_saved_data_file_spatial_metadata(
                dataset_id = state$current_dataset_id,
                base_path = state$data_pond
              )

            spatial_metadata(
              data.table::as.data.table(
                saved_spatial
              )
            )

            showNotification(
              if (isTRUE(include_value)) {
                paste(
                  "Spatial record included in the derived",
                  "dataset bounding box."
                )
              } else {
                paste(
                  "Spatial record removed from the derived",
                  "dataset bounding box."
                )
              },
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              conditionMessage(e),
              type = "error",
              duration = 8
            )
          }
        )
      }
    )

    output$spatial_record_details <- renderText({
      row <- selected_spatial_record()

      if (is.null(row)) {
        return(
          "No saved spatial metadata record is selected."
        )
      }

      show_spatial_value <- function(x) {
        if (
          is.null(x) ||
          length(x) == 0 ||
          is.na(x[1]) ||
          trimws(as.character(x[1])) == ""
        ) {
          return("Not recorded")
        }

        as.character(x[1])
      }

      paste0(
        "File: ",
        show_spatial_value(row$data_file_rel_path_full),
        "\n",
        "Layer: ",
        show_spatial_value(row$layer_name),
        "\n",
        "Extraction status: ",
        show_spatial_value(row$spatial_status),
        "\n",
        "Spatial type: ",
        show_spatial_value(row$spatial_type),
        "\n",
        "CRS: ",
        show_spatial_value(row$crs_auth_code),
        "\n",
        "Geometry: ",
        show_spatial_value(row$geometry_type)
      )
    })

    observeEvent(input$save_spatial_notes, {
      selected_id <- input$spatial_record_select

      if (
        is.null(selected_id) ||
        length(selected_id) == 0 ||
        identical(selected_id, "")
      ) {
        showNotification(
          "Select a spatial file or layer first.",
          type = "warning"
        )

        return()
      }

      update_data_file_spatial_metadata_notes(
        spatial_metadata_id =
          as.integer(selected_id),

        base_path =
          state$data_pond,

        user_notes =
          input$spatial_user_notes,

        user_id =
          state$active_user_id
      )

      spatial_metadata_refresh(
        spatial_metadata_refresh() + 1L
      )
      state$refresh_spatial_metadata <-
        state$refresh_spatial_metadata + 1L

      refreshed <- saved_spatial_records()

      spatial_metadata(
        data.table::as.data.table(
          refreshed
        )
      )

      showNotification(
        "Spatial metadata notes saved.",
        type = "message"
      )
      updateSelectInput(
        session,
        "spatial_record_select",
        selected = as.character(selected_id)
      )
    })

    observe({
      df <- saved_data_files()

      if (
        is.null(df) ||
        nrow(as.data.frame(df)) == 0L
      ) {
        shiny::freezeReactiveValue(
          input,
          "saved_file_select"
        )

        shiny::updateSelectInput(
          session,
          "saved_file_select",
          choices = character(),
          selected = character(0)
        )

        return()
      }

      df <- data.table::as.data.table(
        data.table::copy(df)
      )

      required_columns <- list(
        id = NA_integer_,
        file_name = NA_character_,
        data_file_rel_path_full = NA_character_,
        data_file_format = NA_character_,
        active = 1L
      )

      for (field in names(required_columns)) {
        if (!field %in% names(df)) {
          df[
            ,
            (field) := required_columns[[field]]
          ]
        }
      }

      labels <- vapply(
        seq_len(nrow(df)),
        function(i) {
          file_label <- if (
            !is.na(df$file_name[i]) &&
            nzchar(trimws(df$file_name[i]))
          ) {
            df$file_name[i]
          } else {
            basename(
              df$data_file_rel_path_full[i]
            )
          }

          path_label <- if (
            !is.na(
              df$data_file_rel_path_full[i]
            ) &&
            nzchar(
              trimws(
                df$data_file_rel_path_full[i]
              )
            )
          ) {
            paste0(
              " \u2014 ",
              df$data_file_rel_path_full[i]
            )
          } else {
            ""
          }

          status_label <- if (
            !is.na(df$active[i]) &&
            as.integer(df$active[i]) != 1L
          ) {
            " [missing]"
          } else {
            ""
          }

          paste0(
            file_label,
            path_label,
            status_label
          )
        },
        character(1)
      )

      choices <- stats::setNames(
        as.character(df$id),
        labels
      )

      current <- isolate(
        input$saved_file_select
      )

      if (
        is.null(current) ||
        length(current) == 0L ||
        is.na(current[1]) ||
        !as.character(current[1]) %in%
          unname(choices)
      ) {
        current <- unname(choices)[1]
      } else {
        current <- as.character(
          current[1]
        )
      }

      shiny::freezeReactiveValue(
        input,
        "saved_file_select"
      )

      shiny::updateSelectInput(
        session,
        "saved_file_select",
        choices = choices,
        selected = current
      )
    })

    shiny::observe({
      row <- selected_saved_file_record()

      if (
        is.null(row) ||
        nrow(as.data.frame(row)) == 0L
      ) {
        shiny::updateTextAreaInput(
          session,
          "data_file_description",
          value = ""
        )

        shiny::updateTextAreaInput(
          session,
          "data_file_notes",
          value = ""
        )

        shiny::updateSelectInput(
          session,
          "content_hint",
          selected = "unknown"
        )

        shiny::updateSelectInput(
          session,
          "review_status",
          selected = "not reviewed"
        )

        shiny::updateCheckboxInput(
          session,
          "include_in_attribute_scan",
          value = FALSE
        )

        return()
      }

      text_value <- function(
        field,
        default = ""
      ) {
        if (
          !field %in% names(row) ||
          is.null(row[[field]]) ||
          length(row[[field]]) == 0L ||
          is.na(row[[field]][1]) ||
          !nzchar(
            trimws(
              as.character(
                row[[field]][1]
              )
            )
          )
        ) {
          return(default)
        }

        as.character(
          row[[field]][1]
        )
      }

      shiny::updateTextAreaInput(
        session,
        "data_file_description",
        value = text_value(
          "data_file_description"
        )
      )

      shiny::updateTextAreaInput(
        session,
        "data_file_notes",
        value = text_value(
          "data_file_notes"
        )
      )

      shiny::updateSelectInput(
        session,
        "content_hint",
        selected = text_value(
          "content_hint",
          default = "unknown"
        )
      )

      shiny::updateSelectInput(
        session,
        "review_status",
        selected = text_value(
          "review_status",
          default = "not reviewed"
        )
      )

      include_value <- (
        "include_in_attribute_scan" %in%
          names(row) &&
          !is.na(
            row$include_in_attribute_scan[1]
          ) &&
          as.integer(
            row$include_in_attribute_scan[1]
          ) == 1L
      )

      shiny::updateCheckboxInput(
        session,
        "include_in_attribute_scan",
        value = include_value
      )
    })

    observeEvent(input$save_file_review, {
      req(input$saved_file_select)

      update_data_file_review(
        data_file_id =
          as.integer(input$saved_file_select),
        base_path =
          state$data_pond,
        data_file_description =
          trimws(
            input$data_file_description %||% ""
          ),
        data_file_notes =
          trimws(
            input$data_file_notes %||% ""
          ),
        content_hint =
          input$content_hint,
        review_status =
          input$review_status,
        include_in_attribute_scan =
          input$include_in_attribute_scan,
        user_id =
          state$active_user_id
      )

      state$refresh_data_files_saved <- state$refresh_data_files_saved + 1L

      showNotification(
        "Data file review saved.",
        type = "message"
      )
    })

    observeEvent(input$profile_selected_file, {
      req(input$saved_file_select)

      cfg <- state$pond_config %||% default_pond_config()
      if (!isTRUE(cfg$attribute_profile$enabled)) {
        showNotification("Attribute profiling is disabled in the data pond config.", type = "warning")
        return()
      }

      df <- saved_data_files()
      row <- df[as.character(df$id) == as.character(input$saved_file_select), , drop = FALSE]
      if (nrow(row) == 0) {
        showNotification("Selected data file not found.", type = "warning")
        return()
      }

      fmt <- trimws(
        as.character(
          row$data_file_format[1]
        )
      )

      supported_profile_formats <- c(
        "vector",
        "esri_file_geodatabase",
        "raster",
        "virtual_raster",
        "esri_arcinfo_grid"
      )

      if (!fmt %in% supported_profile_formats) {
        showNotification(
          paste(
            "Attribute profiling does not currently support",
            "the selected format:",
            fmt
          ),
          type = "warning",
          duration = 8
        )

        return()
      }

      include_in_scan <- (
        "include_in_attribute_scan" %in% names(row) &&
          !is.na(
            row$include_in_attribute_scan[1]
          ) &&
          as.integer(
            row$include_in_attribute_scan[1]
          ) == 1L
      )

      if (!isTRUE(include_in_scan)) {
        showNotification(
          paste(
            "Mark this saved file as",
            "'Include in attribute profiling'",
            "and save the file review first."
          ),
          type = "warning",
          duration = 8
        )

        return()
      }

      profile_path <- nonempty_path_value(
        row$file_path
      )

      relative_path <- nonempty_path_value(
        row$data_file_rel_path_full
      )

      dataset_data_dir <- selected_dataset_data_dir()

      dataset_data_dir <- if (
        is.null(dataset_data_dir) ||
        length(dataset_data_dir) == 0L ||
        is.na(dataset_data_dir[1]) ||
        !nzchar(
          trimws(
            as.character(
              dataset_data_dir[1]
            )
          )
        )
      ) {
        NA_character_
      } else {
        normalizePath(
          path.expand(
            as.character(
              dataset_data_dir[1]
            )
          ),
          mustWork = FALSE,
          winslash = "/"
        )
      }


      # Prefer a valid saved absolute path. Reconstruct it from
      # the selected dataset's data directory when it is missing
      # or stale.
      saved_path_is_usable <- (
        !is.na(profile_path) &&
          file.exists(profile_path)
      )

      if (!isTRUE(saved_path_is_usable)) {
        if (
          !is.na(dataset_data_dir) &&
          dir.exists(dataset_data_dir) &&
          !is.na(relative_path)
        ) {
          profile_path <- file.path(
            dataset_data_dir,
            relative_path
          )
        } else {
          profile_path <- NA_character_
        }
      }


      if (
        is.na(profile_path) ||
        !nzchar(trimws(profile_path))
      ) {
        showNotification(
          paste(
            "The selected file has no usable saved path and",
            "could not be resolved from the selected dataset's",
            "data directory."
          ),
          type = "error",
          duration = 8
        )

        return()
      }


      profile_path <- normalizePath(
        path.expand(profile_path),
        mustWork = FALSE,
        winslash = "/"
      )


      if (!file.exists(profile_path)) {
        showNotification(
          paste(
            "Cannot profile attributes because the file",
            "could not be found:",
            profile_path
          ),
          type = "error",
          duration = 8
        )

        return()
      }

      profile_dt <- profile_data_file_attributes(
        path = profile_path,
        format_label = fmt,
        config = cfg,
        layer = NULL
      )

      conn <- get_conn(state$data_pond)
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      profile_result <- save_attribute_profile(
        conn = conn,
        dataset_id = state$current_dataset_id,
        data_file_row = row,
        data_pond_id = state$data_pond_id,
        profile_dt = profile_dt,
        user_id = state$active_user_id,
        profile_type =
          attribute_profile_type_for_format(
            fmt
          )
      )

      state$refresh_attributes <- state$refresh_attributes + 1L
      state$refresh_data_files_saved <- state$refresh_data_files_saved + 1L

      shiny::updateTabsetPanel(
        session,
        "data_files_section",
        selected = "attributes"
      )

      showNotification(
        paste(
          profile_result$records_saved,
          if (
            identical(
              profile_result$profile_type,
              "raster_layers"
            )
          ) {
            "raster layer profile(s) saved."
          } else {
            "attribute profile(s) saved."
          }
        ),
        type = "message"
      )
    })

    shiny::observe({
      at <- saved_attributes()

      if (
        is.null(at) ||
        nrow(as.data.frame(at)) == 0L
      ) {
        shiny::freezeReactiveValue(
          input,
          "attribute_select"
        )

        shiny::updateSelectInput(
          session,
          "attribute_select",
          choices = character(),
          selected = character(0)
        )

        return()
      }

      at <- data.table::as.data.table(
        data.table::copy(at)
      )

      required_columns <- list(
        id = NA_integer_,
        data_file_rel_path_full =
          NA_character_,
        layer_name = NA_character_,
        data_field_name = NA_character_
      )

      for (field in names(required_columns)) {
        if (!field %in% names(at)) {
          at[
            ,
            (field) := required_columns[[field]]
          ]
        }
      }

      clean_part <- function(x) {
        x <- as.character(x)

        x[
          is.na(x) |
            !nzchar(trimws(x))
        ] <- ""

        x
      }

      path_label <- clean_part(
        at$data_file_rel_path_full
      )

      layer_label <- clean_part(
        at$layer_name
      )

      field_label <- clean_part(
        at$data_field_name
      )

      labels <- vapply(
        seq_len(nrow(at)),
        function(i) {
          parts <- c(
            path_label[i],
            layer_label[i],
            field_label[i]
          )

          parts <- parts[
            nzchar(parts)
          ]

          if (length(parts) == 0L) {
            return(
              paste(
                "Attribute",
                at$id[i]
              )
            )
          }

          paste(
            parts,
            collapse = " | "
          )
        },
        character(1)
      )

      choices <- stats::setNames(
        as.character(at$id),
        labels
      )

      current <- isolate(
        input$attribute_select
      )

      if (
        is.null(current) ||
        length(current) == 0L ||
        is.na(current[1]) ||
        !as.character(current[1]) %in%
          unname(choices)
      ) {
        current <- unname(choices)[1]
      } else {
        current <- as.character(
          current[1]
        )
      }

      shiny::freezeReactiveValue(
        input,
        "attribute_select"
      )

      shiny::updateSelectInput(
        session,
        "attribute_select",
        choices = choices,
        selected = current
      )
    })

    observeEvent(input$attribute_select, {
      selected_id <- input$attribute_select

      if (
        is.null(selected_id) ||
        length(selected_id) == 0 ||
        identical(selected_id, "")
      ) {
        return()
      }

      at <- saved_attributes()

      if (is.null(at) || nrow(at) == 0) {
        return()
      }

      row <- at[
        as.character(at$id) == as.character(selected_id),
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return()
      }

      row <- row[1, , drop = FALSE]

      updateTextInput(
        session,
        "data_field_alias",
        value = row$data_field_alias[1] %||% ""
      )

      updateTextAreaInput(
        session,
        "data_field_desc",
        value = row$data_field_desc[1] %||% ""
      )

      updateTextInput(
        session,
        "data_field_units",
        value = row$data_field_units[1] %||% ""
      )

      updateCheckboxInput(
        session,
        "legend_field",
        value = isTRUE(
          as.integer(row$legend_field[1]) == 1L
        )
      )

      updateTextAreaInput(
        session,
        "legend_field_notes",
        value = row$legend_field_notes[1] %||% ""
      )
    })

    observeEvent(input$save_attribute_note, {
      req(input$attribute_select)
      update_dataset_attribute_note(
        attribute_id = as.integer(input$attribute_select),
        base_path = state$data_pond,
        data_field_alias = input$data_field_alias,
        data_field_desc = input$data_field_desc,
        data_field_units = input$data_field_units,
        legend_field = input$legend_field,
        legend_field_notes = input$legend_field_notes,
        user_id = state$active_user_id
      )
      state$refresh_attributes <- state$refresh_attributes + 1L
      showNotification("Attribute note saved.", type = "message")
    })

    output$attribute_table <- DT::renderDT({
      at <- saved_attributes()

      if (is.null(at) || nrow(at) == 0) {
        return(
          render_data_files_dt(
            data.frame(
              message = paste(
                "No saved attribute profile is available",
                "for the selected dataset."
              )
            )
          )
        )
      }

      cols <- intersect(
        c(
          "id",
          "data_file_rel_path_full",
          "layer_name",
          "data_field_name",
          "data_field_alias",
          "attribute_type",
          "example_values",
          "value_count",
          "missing_count",
          "unique_count",
          "min_value",
          "max_value",
          "data_field_desc",
          "data_field_units",
          "legend_field",
          "legend_field_notes",
          "profile_updated_at"
        ),
        names(at)
      )

      display_at <- data.table::as.data.table(
        data.table::copy(at[, cols, drop = FALSE])
      )

      if ("legend_field" %in% names(display_at)) {
        display_at[
          ,
          legend_field :=
            data.table::fifelse(
              as.integer(legend_field) == 1L,
              "Yes",
              "No"
            )
        ]
      }

      tbl <- render_data_files_dt(
        display_at,
        page_length = 15L,
        selection = "single",
        filter = "top",
        buttons = TRUE,
        hidden_columns = if ("id" %in% names(display_at)) {
          which(names(display_at) == "id") - 1L
        } else {
          integer()
        }
      )

      tbl
    })

    # let the attribute table be used for attribute selection
    observeEvent(
      input$attribute_table_rows_selected,
      {
        selected_row <-
          input$attribute_table_rows_selected

        if (
          is.null(selected_row) ||
          length(selected_row) == 0
        ) {
          return()
        }

        at <- saved_attributes()

        if (is.null(at) || nrow(at) == 0) {
          return()
        }

        cols <- intersect(
          c(
            "id",
            "data_file_rel_path_full",
            "layer_name",
            "data_field_name",
            "data_field_alias",
            "attribute_type",
            "example_values",
            "value_count",
            "missing_count",
            "unique_count",
            "min_value",
            "max_value",
            "data_field_desc",
            "data_field_units",
            "legend_field",
            "legend_field_notes",
            "profile_updated_at"
          ),
          names(at)
        )

        display_at <- at[, cols, drop = FALSE]

        if (
          selected_row < 1L ||
          selected_row > nrow(display_at)
        ) {
          return()
        }

        attribute_id <-
          display_at$id[selected_row]

        updateSelectInput(
          session,
          "attribute_select",
          selected = as.character(attribute_id)
        )
      }
    )

    # input: extract_spatial ----
    observeEvent(input$extract_spatial, {
      dataset_id <- state$current_dataset_id
      base_path <- state$data_pond

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0 ||
        is.na(dataset_id[1]) ||
        is.null(base_path) ||
        length(base_path) == 0 ||
        is.na(base_path[1]) ||
        base_path[1] == ""
      ) {
        spatial_metadata(
          empty_spatial_metadata(
            "No saved dataset is selected."
          )
        )

        showNotification(
          "Select a saved dataset before extracting spatial metadata.",
          type = "warning"
        )

        return()
      }


      # 1. Select the inventory source ----

      live_df <- files()

      use_live_scan <- (
        !is.null(live_df) &&
          nrow(live_df) > 0 &&
          !"message" %in% names(live_df)
      )

      if (isTRUE(use_live_scan)) {
        df <- data.table::as.data.table(
          data.table::copy(live_df)
        )

        df <- logical_dataset_rows(df)

        inventory_source <- "live filesystem scan"
      } else {
        saved_df <- saved_data_files()

        if (
          is.null(saved_df) ||
          nrow(saved_df) == 0
        ) {
          spatial_metadata(
            empty_spatial_metadata(
              paste(
                "No live scan or saved data-file inventory",
                "is available for the selected dataset."
              )
            )
          )

          showNotification(
            paste(
              "No live scan or saved data-file inventory",
              "is available for this dataset."
            ),
            type = "warning"
          )

          return()
        }

        df <- data.table::as.data.table(
          data.table::copy(saved_df)
        )

        # Ignore files previously marked as missing.
        if ("active" %in% names(df)) {
          df <- df[
            is.na(active) |
              as.integer(active) == 1L
          ]
        }

        # The saved inventory should already contain logical rows,
        # but retain only primary records when that field exists.
        if ("primary_dataset" %in% names(df)) {
          df <- df[
            is.na(primary_dataset) |
              as.integer(primary_dataset) == 1L
          ]
        }

        inventory_source <- "saved database inventory"
      }

      if (nrow(df) == 0) {
        spatial_metadata(
          empty_spatial_metadata(
            "No active logical data files are available for spatial extraction."
          )
        )

        showNotification(
          "No active logical data files are available.",
          type = "warning"
        )

        return()
      }

      # 2. Normalise inventory columns ----

      required_columns <- list(
        data_file_rel_path_full = NA_character_,
        file_name = NA_character_,
        file_path = NA_character_,
        data_file_format = NA_character_,
        file_role = "file",
        primary_dataset = TRUE,
        logical_file_name = NA_character_,
        logical_rel_path = NA_character_,
        logical_file_path = NA_character_,
        logical_format = NA_character_
      )

      for (nm in names(required_columns)) {
        if (!nm %in% names(df)) {
          df[, (nm) := required_columns[[nm]]]
        }
      }

      df[
        is.na(logical_file_name) |
          logical_file_name == "",
        logical_file_name := file_name
      ]

      df[
        is.na(logical_rel_path) |
          logical_rel_path == "",
        logical_rel_path := data_file_rel_path_full
      ]

      df[
        is.na(logical_format) |
          logical_format == "",
        logical_format := data_file_format
      ]

      # 3. Locate the selected dataset data directory ----

      data_dir <- selected_dataset_data_dir()

      if (
        is.null(data_dir) ||
        length(data_dir) == 0L ||
        is.na(data_dir[1]) ||
        !nzchar(trimws(as.character(data_dir[1]))) ||
        !dir.exists(data_dir[1])
      ) {
        spatial_metadata(
          empty_spatial_metadata(
            paste(
              "Data-file records exist, but the selected",
              "dataset data folder cannot be located."
            )
          )
        )

        showNotification(
          paste(
            "The selected dataset data folder could not be located.",
            "Check the dataset folder or run a live scan after",
            "correcting its location."
          ),
          type = "error",
          duration = 8
        )

        return()
      }

      data_dir <- normalizePath(
        path.expand(
          as.character(data_dir[1])
        ),
        mustWork = FALSE,
        winslash = "/"
      )

      # 4. Repair cached absolute file paths ----

      df[
        ,
        resolved_file_path :=
          as.character(file_path)
      ]

      repair_path <- (
        is.na(df$resolved_file_path) |
          df$resolved_file_path == "" |
          !file.exists(df$resolved_file_path)
      )

      df[
        repair_path,
        resolved_file_path :=
          file.path(
            data_dir,
            data_file_rel_path_full
          )
      ]

      df[
        ,
        file_path :=
          normalizePath(
            resolved_file_path,
            mustWork = FALSE,
            winslash = "/"
          )
      ]

      df[
        ,
        logical_file_path :=
          file_path
      ]

      file_exists <- file.exists(
        df$file_path
      )

      missing_file_count <- sum(
        !file_exists,
        na.rm = TRUE
      )

      df <- df[
        file_exists
      ]

      if (nrow(df) == 0) {
        spatial_metadata(
          empty_spatial_metadata(
            paste(
              "Saved data-file records exist, but none",
              "of the corresponding files could be found."
            )
          )
        )

        showNotification(
          paste(
            "No saved data files could be located on disk.",
            "Run a live scan if the dataset folder has changed."
          ),
          type = "error"
        )

        return()
      }

      # 5. Extract spatial metadata ----

      spatial_dt <- extract_spatial_metadata(
        df,
        data_dir = data_dir
      )

      if (
        is.null(spatial_dt) ||
        nrow(spatial_dt) == 0
      ) {
        spatial_metadata(
          empty_spatial_metadata(
            "Spatial extraction returned no results."
          )
        )

        showNotification(
          "Spatial extraction returned no results.",
          type = "warning"
        )

        return()
      }

      spatial_dt <- data.table::as.data.table(
        data.table::copy(spatial_dt)
      )

      if ("message" %in% names(spatial_dt)) {
        spatial_metadata(spatial_dt)

        showNotification(
          spatial_dt$message[1],
          type = "warning"
        )

        return()
      }

      if (!"file_path" %in% names(spatial_dt)) {
        spatial_metadata(
          empty_spatial_metadata(
            paste(
              "Spatial extraction succeeded, but the result",
              "does not contain a file_path field."
            )
          )
        )

        showNotification(
          paste(
            "The spatial extractor did not return file paths.",
            "The metadata could not be linked to saved files."
          ),
          type = "error"
        )

        return()
      }

      # 6. Link extraction rows back to the saved inventory ----

      normalise_path_key <- function(x) {
        if (
          is.null(x) ||
          length(x) == 0 ||
          is.na(x[1]) ||
          !nzchar(trimws(as.character(x[1])))
        ) {
          return("")
        }

        normalizePath(
          as.character(x[1]),
          mustWork = FALSE,
          winslash = "/"
        )
      }

      inventory_map <- df[
        ,
        .(
          path_key = vapply(
            file_path,
            normalise_path_key,
            character(1)
          ),
          inventory_rel_path =
            data_file_rel_path_full,
          inventory_file_name =
            file_name
        )
      ]

      inventory_map <- unique(
        inventory_map,
        by = "path_key"
      )

      inventory_map <- inventory_map[
        path_key != ""
      ]

      if (
        !"data_file_rel_path_full" %in%
          names(spatial_dt)
      ) {
        spatial_dt[
          ,
          data_file_rel_path_full :=
            NA_character_
        ]
      }

      if (!"file_name" %in% names(spatial_dt)) {
        spatial_dt[
          ,
          file_name :=
            NA_character_
        ]
      }

      spatial_dt[
        ,
        path_key :=
          vapply(
            file_path,
            normalise_path_key,
            character(1)
          )
      ]

      matched_index <- match(
        spatial_dt$path_key,
        inventory_map$path_key
      )

      matched_rel_path <-
        inventory_map$inventory_rel_path[
          matched_index
        ]

      matched_file_name <-
        inventory_map$inventory_file_name[
          matched_index
        ]

      fill_rel_path <- (
        is.na(
          spatial_dt$data_file_rel_path_full
        ) |
          spatial_dt$data_file_rel_path_full == ""
      )

      spatial_dt$data_file_rel_path_full[
        fill_rel_path
      ] <- matched_rel_path[
        fill_rel_path
      ]

      fill_file_name <- (
        is.na(spatial_dt$file_name) |
          spatial_dt$file_name == ""
      )

      spatial_dt$file_name[
        fill_file_name
      ] <- matched_file_name[
        fill_file_name
      ]

      spatial_dt[
        ,
        path_key := NULL
      ]

      # 7. Save and display the extraction result ----

      save_result <- save_data_file_spatial_metadata(
        dataset_id = dataset_id,
        spatial_dt = spatial_dt,
        base_path = base_path,
        user_id = state$active_user_id
      )

      spatial_metadata_refresh(spatial_metadata_refresh() + 1L)

      state$refresh_spatial_metadata <-
        state$refresh_spatial_metadata + 1L

      saved_spatial <- get_saved_data_file_spatial_metadata(
        dataset_id = dataset_id,
        base_path = base_path
      )

      spatial_metadata(
        data.table::as.data.table(
          saved_spatial
        )
      )

      saved_spatial <- data.table::as.data.table(
        saved_spatial
      )

      first_record_id <- as.character(
        saved_spatial$id[1]
      )

      first_notes <- saved_spatial$user_notes[1]

      if (
        is.null(first_notes) ||
        length(first_notes) == 0 ||
        is.na(first_notes) ||
        !nzchar(trimws(as.character(first_notes)))
      ) {
        first_notes <- ""
      } else {
        first_notes <- as.character(first_notes)
      }

      file_label <- if (
        !is.na(saved_spatial$file_name[1]) &&
        nzchar(trimws(saved_spatial$file_name[1]))
      ) {
        saved_spatial$file_name[1]
      } else {
        basename(
          saved_spatial$data_file_rel_path_full[1]
        )
      }

      layer_label <- if (
        !is.na(saved_spatial$layer_name[1]) &&
        nzchar(trimws(saved_spatial$layer_name[1]))
      ) {
        paste0(
          " | ",
          saved_spatial$layer_name[1]
        )
      } else {
        ""
      }

      updateSelectInput(
        session,
        "spatial_record_select",
        choices = setNames(
          first_record_id,
          paste0(file_label, layer_label)
        ),
        selected = first_record_id
      )

      updateTextAreaInput(
        session,
        "spatial_user_notes",
        value = first_notes
      )

      # Retain this only if another table depends on the counter.
      state$refresh_data_files_saved <- state$refresh_data_files_saved + 1L

      rows_saved <- if (
        !is.null(save_result) &&
        "rows_saved" %in% names(save_result)
      ) {
        as.integer(
          save_result$rows_saved[1]
        )
      } else {
        nrow(spatial_dt)
      }

      rows_skipped <- if (
        !is.null(save_result) &&
        "rows_skipped" %in% names(save_result)
      ) {
        as.integer(
          save_result$rows_skipped[1]
        )
      } else {
        0L
      }

      notification_text <- paste0(
        "Spatial metadata extracted using the ",
        inventory_source,
        ". ",
        rows_saved,
        " row",
        ifelse(rows_saved == 1L, "", "s"),
        " saved."
      )

      if (rows_skipped > 0L) {
        notification_text <- paste0(
          notification_text,
          " ",
          rows_skipped,
          " extraction row",
          ifelse(rows_skipped == 1L, "", "s"),
          " could not be linked to a saved file."
        )
      }

      if (missing_file_count > 0L) {
        notification_text <- paste0(
          notification_text,
          " ",
          missing_file_count,
          " inventory file",
          ifelse(
            missing_file_count == 1L,
            "",
            "s"
          ),
          " could not be found on disk."
        )
      }

      showNotification(
        notification_text,
        type = "message",
        duration = 7
      )
    })

    observeEvent(
      state$current_dataset_id,
      {
        dataset_id <- state$current_dataset_id
        base_path <- state$data_pond

        # Clear the previous dataset's spatial form immediately.
        updateSelectInput(
          session,
          "spatial_record_select",
          choices = character(),
          selected = character(0)
        )

        updateTextAreaInput(
          session,
          "spatial_user_notes",
          value = ""
        )

        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0 ||
          is.na(dataset_id[1]) ||
          is.null(base_path) ||
          length(base_path) == 0 ||
          is.na(base_path[1]) ||
          base_path[1] == ""
        ) {
          spatial_metadata(
            empty_spatial_metadata(
              "No spatial metadata is associated with the current record."
            )
          )

          return()
        }

        # Force database-backed spatial records to refresh.
        spatial_metadata_refresh(
          spatial_metadata_refresh() + 1L
        )

        saved_spatial <- get_saved_data_file_spatial_metadata(
          dataset_id = dataset_id,
          base_path = base_path
        )

        if (
          is.null(saved_spatial) ||
          nrow(saved_spatial) == 0
        ) {
          spatial_metadata(
            empty_spatial_metadata(
              paste(
                "No saved spatial metadata exists",
                "for the selected dataset."
              )
            )
          )

          return()
        }

        spatial_metadata(
          data.table::as.data.table(
            saved_spatial
          )
        )
      },
      ignoreInit = FALSE
    )

    output$spatial_details_table <- DT::renderDT({
      df <- spatial_metadata()

      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(
          message = "No spatial metadata details are available."
        )
      }

      render_data_files_dt(
        df,
        page_length = 10L,
        selection = "none",
        filter = "top",
        buttons = TRUE
      )
    })

    output$spatial_summary_table <- DT::renderDT({
      df <- spatial_summary()

      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(
          message = "Spatial metadata has not been extracted."
        )
      }

      render_data_files_dt(
        df,
        page_length = 10L,
        selection = "none",
        filter = "top",
        buttons = TRUE
      )
    })

    output$data_status <- renderText({
      data_dir <- active_scan_dir()
      scan_df <- files()
      saved_df <- saved_data_files()

      saved_count <- if (
        is.null(saved_df) ||
        nrow(saved_df) == 0
      ) {
        0L
      } else {
        nrow(saved_df)
      }

      if (is.null(data_dir)) {
        if (saved_count > 0L) {
          return(
            paste0(
              "No live scan folder is currently selected.",
              "\n",
              saved_count,
              " saved logical data file",
              ifelse(saved_count == 1L, "", "s"),
              " exist in the database for this dataset.",
              "\n",
              paste(
                "Run a live scan to compare the saved inventory",
                "with the current filesystem."
              )
            )
          )
        }

        return(
          paste(
            "No live scan folder is currently selected.",
            "No saved data-file inventory exists for this dataset."
          )
        )
      }

      if (!dir.exists(data_dir)) {
        return(
          paste0(
            "Expected live scan folder does not exist:",
            "\n",
            data_dir,
            "\n",
            "Saved database files: ",
            saved_count
          )
        )
      }

      if ("message" %in% names(scan_df)) {
        if (saved_count > 0L) {
          return(
            paste0(
              "Live scan folder: ",
              data_dir,
              "\n",
              "No live scan has been run during this app session.",
              "\n",
              saved_count,
              " saved logical data file",
              ifelse(saved_count == 1L, "", "s"),
              " exist in the database.",
              "\n",
              paste(
                "Click 'Run live scan / refresh files'",
                "to inspect the current filesystem."
              )
            )
          )
        }

        return(
          paste0(
            "Live scan folder: ",
            data_dir,
            "\n",
            "No live scan has been run during this app session.",
            "\n",
            "No saved data-file inventory exists."
          )
        )
      }

      n_total <- nrow(scan_df)

      n_logical <- if (
        "primary_dataset" %in% names(scan_df)
      ) {
        sum(
          scan_df$primary_dataset %in% TRUE,
          na.rm = TRUE
        )
      } else {
        NA_integer_
      }

      n_components <- if (
        "file_role" %in% names(scan_df)
      ) {
        sum(
          scan_df$file_role %in%
            c(
              "sidecar",
              "dataset_container_component"
            ),
          na.rm = TRUE
        )
      } else {
        NA_integer_
      }

      paste0(
        "Live scan folder: ",
        data_dir,
        "\n",
        "Current live inventory rows: ",
        n_total,
        "\n",
        "Current logical datasets: ",
        n_logical,
        "\n",
        "Current sidecar/component rows: ",
        n_components,
        "\n",
        "Saved logical files in database: ",
        saved_count
      )
    })

    output$inventory_table <- DT::renderDT({
      view <- input$inventory_view

      if (
        is.null(view) ||
        !view %in% c(
          "saved",
          "live"
        )
      ) {
        view <- "saved"
      }

      if (identical(
        view,
        "live"
      )) {
        display_df <-
          prepare_live_inventory_table(
            display_files()
          )

        return(
          render_data_files_dt(
            display_df,
            page_length = 15L,
            selection = "none",
            filter = if (
              "message" %in% names(display_df)
            ) {
              "none"
            } else {
              "top"
            },
            buttons =
              !"message" %in% names(display_df),
            scroll_x = TRUE
          )
        )
      }

      display_df <-
        prepare_saved_inventory_table(
          saved_data_files()
        )

      if ("message" %in% names(display_df)) {
        return(
          render_data_files_dt(
            display_df,
            page_length = 15L,
            selection = "none",
            filter = "none",
            buttons = FALSE,
            scroll_x = TRUE
          )
        )
      }

      table_widget <- DT::datatable(
        as.data.frame(
          display_df,
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
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c(
            "copy",
            "csv"
          ),
          columnDefs = list(
            list(
              visible = FALSE,
              targets = 0
            )
          )
        )
      )

      DT::formatStyle(
        table_widget,
        columns = "File status",
        backgroundColor = DT::styleEqual(
          c(
            "Active",
            "Missing"
          ),
          c(
            "#D1DDC7",
            "#E4D6B8"
          )
        )
      )
    })

    shiny::observeEvent(
      input$inventory_table_rows_selected,
      {
        if (!identical(
          input$inventory_view,
          "saved"
        )) {
          return()
        }

        selected_row <-
          input$inventory_table_rows_selected

        if (
          is.null(selected_row) ||
          length(selected_row) != 1L
        ) {
          return()
        }

        display_df <-
          prepare_saved_inventory_table(
            saved_data_files()
          )

        if (
          "message" %in% names(display_df) ||
          selected_row < 1L ||
          selected_row > nrow(display_df)
        ) {
          return()
        }

        selected_file_id <-
          display_df$id[selected_row]

        shiny::updateSelectInput(
          session,
          "saved_file_select",
          selected =
            as.character(
              selected_file_id
            )
        )

        shiny::updateTabsetPanel(
          session,
          "data_files_section",
          selected = "review"
        )
      }
    )

    observeEvent(input$save_file_scan, {
      df <- files()
      if ("message" %in% names(df)) {
        file_scan_save_summary(data.table::data.table(
          message = "No scanned files available to save."
        ))
        showNotification(
          "No scanned files available to save.",
          type = "warning"
        )
        return()
      }
      if (is.null(state$current_dataset_id) || is.na(state$current_dataset_id)) {
        file_scan_save_summary(data.table::data.table(
          message = "No dataset record is selected. Save or select a dataset before saving detected files."
        ))
        showNotification(
          "Save or select a dataset before saving detected files.",
          type = "warning"
        )
        return()
      }

      scope_info <- scan_scope_info()

      if (identical(
        scope_info$mode,
        "custom"
      )) {
        file_scan_save_summary(
          data.table::data.table(
            message = paste(
              "Custom-folder scans are currently inspection-only.",
              "Use the selected dataset scan source before saving",
              "an inventory to the database."
            )
          )
        )

        showNotification(
          paste(
            "A custom-folder scan cannot be saved to the selected",
            "dataset because its paths are not anchored to the",
            "dataset's data directory."
          ),
          type = "warning",
          duration = 9
        )

        return()
      }

      df <- data.table::as.data.table(data.table::copy(df))
      df_save <- logical_dataset_rows(df)
      if (nrow(df_save) == 0) {
        file_scan_save_summary(data.table::data.table(
          message = "No primary/logical dataset rows available to save."
        ))
        showNotification(
          "No primary/logical dataset rows available to save.",
          type = "warning"
        )
        return()
      }

      res <- save_data_file_scan(
        dataset_id = state$current_dataset_id,
        files_dt = df_save,
        base_path = state$data_pond,
        data_dir = active_data_dir(),
        data_pond_id = state$data_pond_id %||% ensure_data_pond_id(state$data_pond)
      )

      state$refresh_data_files_saved <- state$refresh_data_files_saved + 1L

      file_scan_save_summary(res)

      shiny::updateRadioButtons(
        session,
        "inventory_view",
        selected = "saved"
      )

      shiny::updateTabsetPanel(
        session,
        "data_files_section",
        selected = "inventory"
      )

      showNotification(
        "Detected logical datasets saved to database.",
        type = "message"
      )

      # The saved inventory is now authoritative again.
      shiny::updateCheckboxInput(
        session,
        "show_live_scan_panel",
        value = FALSE
      )
    })

    output$file_scan_save_summary <- DT::renderDT({
      df <- file_scan_save_summary()

      render_data_files_dt(
        df,
        page_length = 5L,
        selection = "none",
        filter = "none",
        buttons = FALSE,
        scroll_x = TRUE
      )
    })

    return(
      list(
        clear = clear_data_files_module,

        files = files,
        display_files = display_files,
        saved_data_files = saved_data_files,
        saved_attributes = saved_attributes,
        saved_spatial = saved_spatial_records,

        spatial_metadata = spatial_metadata,
        data_dir = active_data_dir,

        creation_scale_values =
          creation_scale_values,

        creation_scale_validation =
          creation_scale_validation,

        load_creation_scale =
          load_creation_scale_record
      )
    )
  })
}
