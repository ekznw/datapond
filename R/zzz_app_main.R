# Config-panel UI helpers ----
config_section_box <- function(
  title,
  ...,
  background = "#ffffff",
  border = "#D3CEC4",
  class_name = NULL
) {
  shiny::tags$div(
    class = paste(c("config-section-box", class_name), collapse = " "),
    style = paste(
      "padding:12px;",
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
      "border-radius:10px;",
      "margin-bottom:10px;"
    ),

    shiny::h4(
      style = "margin-top:0;",
      title
    ),

    ...
  )
}


data_pond_folder_roots <- function(
  os_type = .Platform$OS.type,
  sysname = Sys.info()[["sysname"]],
  volume_provider = shinyFiles::getVolumes(),
  linux_mount_candidates = c(
    `/run/mnt` = "/run/mnt",
    `/run/media` = "/run/media",
    `/media` = "/media",
    `/mnt` = "/mnt"
  )
) {
  home_root <- c(home = "~")

  if (identical(os_type, "windows")) {
    volumes <- tryCatch(
      {
        if (is.function(volume_provider)) {
          volume_provider()
        } else {
          volume_provider
        }
      },
      error = function(e) NULL
    )

    if (is.null(volumes) || length(volumes) == 0L) {
      return(home_root)
    }

    return(c(home_root, volumes))
  }

  if (identical(sysname, "Linux")) {
    available_mounts <- linux_mount_candidates[
      dir.exists(unname(linux_mount_candidates)) &
        file.access(unname(linux_mount_candidates), mode = 4L) == 0L
    ]

    return(c(home_root, available_mounts))
  }

  home_root
}


config_panel_ui <- function() {
  shiny::tags$aside(
    class = "dataset-guide pond-config-guide",
    `aria-label` = "Data pond configuration",

    shiny::tags$div(
      class = "dataset-guide-heading",
      shiny::actionLink(
        "hide_config_panel",
        label = NULL,
        icon = shiny::icon("chevron-left"),
        class = "dataset-guide-hide",
        title = "Hide data pond configuration",
        `aria-label` = "Hide data pond configuration",
        `aria-controls` = "config_panel"
      ),
      shiny::tags$div(
        shiny::tags$span(
          class = "dataset-guide-eyebrow",
          "Active pond"
        )
      )
    ),

    shiny::tags$img(
      src = "datapond-assets/datapond-logo.png",
      alt = "DataPond finfoot logo",
      class = "datapond-config-logo",
      width = 120,
      height = 120
    ),

    shiny::tags$div(
      class = "pond-config-intro-card",
      shiny::tags$p(
        class = "config-intro",
        paste(
          "Choose the active data pond,",
          "then set defaults for new dataset records."
        )
      )
    ),

    config_section_box(
      title = "Session context",
      background = "#D1DDC7",
      border = "#87966C",
      class_name = "config-session-context",

      shiny::tags$div(
        class = "config-current-value",
        shiny::strong("Current data pond"),
        shiny::textOutput("current_path")
      ),

      shiny::actionButton("access_logout", "Sign out"),
      shiny::uiOutput("access_manage_control"),

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
      ),

      shiny::tags$details(
        class = "config-help config-context-details",
        shiny::tags$summary("Lifecycle folder status"),
        dataset_folder_selection_ui("dataset_parent_dir"),
        shiny::tags$div(
          class = "config-current-value",
          shiny::strong("New dataset location"),
          shiny::textOutput("dataset_parent_dir_display")
        ),
        shiny::tags$p(class = "help-block", "For new folders only; existing datasets stay in place."),
        lifecycle_folder_help_ui(),
        shiny::verbatimTextOutput("pond_structure")
      )
    ),

    config_section_box(
      title = "Metadata entry",
      metadata_population_controls()
    ),

    config_section_box(
      title = "Data files | Attribute profiling",
      mod_attribute_profile_config_ui("attribute_profile_config")
    ),

    shiny::tags$details(
      class = "config-help config-advanced pond-management",
      shiny::tags$summary(shiny::strong("Pond management")),
      shiny::tags$p(
        "Connect, switch, rename, disconnect, open or create data ponds."
      ),

      shiny::tags$div(
        class = "config-subsection",
        shiny::tags$h4("Connected data ponds"),
        shiny::textInput(
          "current_pond_nickname",
          "Current pond nickname / repository name"
        ),
        shiny::actionButton(
          "save_current_pond_nickname",
          "Save current pond name"
        ),
        shiny::tags$details(
          shiny::tags$summary("Connect another existing pond"),
          shiny::tags$p(
            class = "help-block",
            paste(
              "Connected ponds are catalogued read-only. Switch the active pond",
              "before editing one of its datasets."
            )
          ),
          shinyFiles::shinyDirButton(
            "connected_pond_input",
            "Select Data Pond folder",
            "Select an existing Data Pond folder"
          ),
          shiny::verbatimTextOutput("connected_pond_path_preview"),
          shiny::textInput(
            "connected_pond_name",
            "Nickname / repository name",
            placeholder = "For example: Conservation planning repo"
          ),
          shiny::actionButton("add_connected_pond", "Connect pond")
        ),
        shiny::selectInput(
          "connected_pond_select",
          metadata_field_label("Connected pond", "Conditional", "Select a connected pond before switching to it or disconnecting it."),
          choices = c("No connected ponds" = ""),
          selectize = FALSE
        ),
        access_credentials_form("connected_login_username", "connected_login_password", "connected_login",
          "Selected pond username", "Selected pond password", "Sign in to selected catalogue"),
        shiny::p(class = "help-block", "Sign in to each connected pond to show its datasets here. Access lasts for this session; editing still requires switching ponds."),
        shiny::tags$div(
          class = "config-action-stack",
          shiny::actionButton("switch_connected_pond", "Switch to selected pond"),
          shiny::actionButton("remove_connected_pond", "Disconnect selected pond")
        )
      ),

      shiny::tags$div(
        class = "config-subsection",
        shiny::tags$h4("Open or create a data pond"),
        shiny::tags$p("Open a pond you already use, or create a new one."),
        shiny::tags$div(
          class = "config-action-stack",
          shinyFiles::shinyDirButton(
            "data_pond_input", "Open existing pond",
            "Select an existing Data Pond folder"
          ),
          shiny::actionButton("new_pond", "Create new pond", class = "btn-primary")
        ),
        shiny::helpText(
          paste(
            "Creating a pond adds the standard folders and a metadata database,",
            "then switches to your new pond."
          )
        ),
        shiny::tags$details(
          shiny::tags$summary("Open an existing pond by path"),
          shiny::textInput(
            "manual_path", metadata_field_label("Existing pond folder", "Conditional", "Required when opening a pond by path."), placeholder = "/path/to/data_pond"
          ),
          shiny::actionButton("set_manual", "Open pond at this path")
        )
      )
    ),

    mod_document_tools_ui("document_tools"),

    config_section_box(
      "Display & sound",
      shiny::selectInput(
        "display_theme_mode",
        "Appearance",
        choices = c(
          "System default" = "system",
          "Light" = "light",
          "Dark \u2014 DataPond earth" = "dark",
          "Dark \u2014 Noctis Bordo" = "bordo"
        ),
        selected = "system",
        selectize = FALSE
      ),
      shiny::checkboxInput(
        "enable_finfoot_sound",
        "Play the finfoot call for milestones *",
        value = TRUE
      ),
      shiny::tags$p(
        class = "help-block",
        paste("*"),
        " ",
        shiny::tags$a(
          "Dries Van de Loock, XC1036489",
          href = "https://xeno-canto.org/1036489",
          target = "_blank",
          rel = "noopener"
        ),
        ". Licensed under ",
        shiny::tags$a(
          "CC BY-NC-ND 4.0",
          href = "https://creativecommons.org/licenses/by-nc-nd/4.0/",
          target = "_blank",
          rel = "noopener"
        ),
        "."
      )
    )
  )
}

new_data_pond_dialog <- function(parent_path = "") {
  shiny::modalDialog(
    title = "Create new pond",
    shiny::tags$p(
      "Choose where to create your pond and give its new folder a name."
    ),
    shinyFiles::shinyDirButton(
      "new_pond_parent_input", "Choose location", "Select the parent folder for your new pond"
    ),
    shiny::textInput("new_pond_parent", metadata_field_label("Location (parent folder)", "Mandatory", "Choose the parent folder in which to create the new pond."), value = parent_path),
    shiny::textInput("new_pond_name", metadata_field_label("New pond name", "Mandatory", "Required when creating a pond."), placeholder = "For example: Wetland surveys"),
    shiny::textOutput("new_pond_destination"),
    shiny::helpText(
      "Creates the standard folders and a metadata database, then switches to your new pond. Next, set up a pond administrator and sign in."
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("create_pond", "Create and open pond", class = "btn-primary")
    ),
    easyClose = FALSE
  )
}

connected_pond_catalogues_ui <- function() {
  shiny::tags$details(
    class = "config-help",
    open = TRUE,
    shiny::tags$summary(
      shiny::strong("Connected pond catalogues")
    ),
    shiny::tags$p(
      paste(
        "These rows are read-only summaries from other connected ponds.",
        "Select a row and switch ponds before editing that dataset."
      )
    ),
    shiny::tags$div(
      style = "display:flex; flex-wrap:wrap; gap:8px; align-items:center;",
      shiny::checkboxInput(
        "show_connected_ponds",
        "Show connected pond datasets",
        value = TRUE
      ),
      shiny::actionButton(
        "refresh_connected_ponds",
        "Refresh connected catalogues"
      ),
      shiny::actionButton(
        "switch_to_selected_connected_pond",
        "Switch to selected dataset's pond"
      )
    ),
    shiny::uiOutput("connected_pond_summary"),
    DT::DTOutput("connected_dataset_table"),
    shiny::uiOutput("cross_pond_duplicate_alert"),
    DT::DTOutput("cross_pond_duplicate_table")
  )
}


pond_overview_ui <- function() {
  shiny::tagList(
    shiny::h2(
      "Pond overview"
    ),
    shiny::actionButton("overview_open_pond_folder", "Open pond directory",
      icon = shiny::icon("folder-open"), class = "datapond-requires-active-user",
      title = "Open the active pond's root folder in the file manager"),

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
          "This page summarises the active pond and connected pond catalogues.",
          "Select a dataset from the table or map to preview it. Open the metadata editor when you want to load its forms.",
          "The inventory includes connected ponds; rows marked \u2605 Current belong to this pond."
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

    shiny::h3(
      "Dataset inventory search"
    ),

    shiny::checkboxInput("overview_current_only", "Show current pond datasets only", FALSE),

    shiny::fluidRow(
      shiny::column(
        width = 7,
        shiny::textInput(
          "pond_dataset_search",
          "Search pond datasets",
          placeholder = paste(
            "Title, author, organisation, contact, citation key,",
            "status or folder"
          )
        )
      ),
      shiny::column(
        width = 2,
        style = "padding-top:25px;",
        shiny::actionButton(
          "refresh_pond_overview",
          "Refresh",
          icon = shiny::icon("refresh"),
          title = paste(
            "Clear the overview selection and refresh",
            "the dataset map"
          )
        )
      ),
      shiny::column(
        width = 3,
        shiny::checkboxInput(
          "show_hidden_datasets",
          "Show hidden datasets",
          value = FALSE
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 8,

        shiny::h3(
          "Current pond locations"
        ),

        leaflet::leafletOutput(
          "pond_dataset_map",
          height = "500px"
        ),

        shiny::radioButtons(
          "pond_map_display",
          "Map display",
          choices = c(
            "Location points" = "points",
            "Extent boxes" = "extents"
          ),
          selected = "points",
          inline = TRUE
        ),

        shiny::tags$small(
          style = paste(
            "color:#666;",
            "display:block;",
            "margin-bottom:8px;"
          ),

          paste(
            "The map shows current-pond datasets with a valid derived EPSG:4326 bounding box.",
            "Points show bounding-box centres, not sampling sites.",
            "Click a cluster to zoom in or separate overlapping points.",
            "The selected dataset's extent is outlined in either view.",
            "Map colours follow the pond-status legend."
          )
        ),

        shiny::h4(
          style = "margin:10px 0 6px 0;",
          "Selected dataset location"
        ),

        shiny::uiOutput(
          "selected_dataset_location_details"
        ),
        shiny::uiOutput("overview_dataset_abstract"),
        shiny::uiOutput("overview_editor_action"),

        shiny::uiOutput("dataset_record_controls")
      ),

      shiny::column(
        width = 4,

        shiny::h3(
          "Current pond summary"
        ),

        shiny::uiOutput(
          "pond_overview_summary"
        ),

        shiny::hr(),

        shiny::h4(
          "Dataset citation"
        ),

        shiny::tags$small(
          style = paste(
            "color:#666;",
            "display:block;",
            "margin-bottom:8px;"
          ),

          paste(
            "The bibliography is generated from saved metadata.",
            "Unsaved form edits are not included."
          )
        ),

        shiny::uiOutput(
          "selected_dataset_citation"
        ),

        shiny::tags$div(
          style = paste(
            "display:flex;",
            "flex-wrap:wrap;",
            "gap:6px;",
            "margin:8px 0;"
          ),

          shiny::actionButton(
            "copy_citation_key",
            "Copy key"
          ),

          shiny::actionButton(
            "copy_quarto_citation",
            "Copy [@key]"
          ),

          shiny::actionButton(
            "copy_biblatex_entry",
            "Copy BibLaTeX entry"
          )
        ),

        shiny::actionButton(
          "refresh_pond_bibliography",
          "Refresh pond bibliography",
          class = "datapond-requires-active-user"
        ),

        shiny::uiOutput(
          "pond_bibliography_status"
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
        "\u2605 Current identifies this pond's datasets; select one to preview its saved metadata.",
        "Connected records are read-only here. Select one and use Open selected pond to view it in its local pond or in Flow.",
        "The selected row is highlighted independently",
        "of the pond-status colour."
      )
    ),

    shiny::uiOutput("overview_connected_summary"),
    DT::DTOutput(
      "dataset_table"
    ),
    shiny::uiOutput("overview_connection_details"),
    shiny::actionButton("overview_open_connected", "Open selected pond",
      title = "Switch to the selected connected pond, or open a remote pond in Flow")
  )
}


pond_link_help_card <- function(title, sign_in, ...) {
  shiny::tags$section(
    class = "pond-link-help-card",
    shiny::h4(title),
    shiny::p(class = "pond-link-sign-in", shiny::strong("Sign in to: "), sign_in),
    ...
  )
}

dataset_transfer_ui <- function() {
  shiny::tagList(
    shiny::h2("Pond links"),
    shiny::tags$div(
      style = paste(
        "padding:10px 12px; background:#F8F4EA;",
        "border:1px solid #B99572; margin-bottom:14px;"
      ),
      shiny::tags$p(
        style = "margin:0;",
        paste(
          "Every action below uses the active pond you are signed into.",
          "Browsing a connected pond does not switch your sign-in.",
          "Use Switch to selected dataset's pond and sign in there before working on its dataset."
        )
      )
    ),
    connected_pond_catalogues_ui(),
    shiny::h3("Portable dataset transfer"),
    shiny::div(
      class = "pond-link-help-cards",
      `aria-label` = "Where to create and import dataset bundles",
      pond_link_help_card(
        "Create a dataset bundle", "the source pond that holds the dataset.",
        shiny::p("With edit access, open the dataset in Create / edit dataset and save any changes. Then use Compress selected dataset below. The ZIP is saved in that dataset's folder; send it to the recipient.")),
      pond_link_help_card(
        "Import a dataset bundle", "the receiving pond where the copy should be stored.",
        shiny::p("With edit access, choose the ZIP under Import dataset and click Import into active pond. This creates a local record and folder in the receiving pond."))
    ),
    shiny::tags$p(
      paste(
        "Create a ZIP from the selected dataset, or import one prepared in",
        "another Data Pond. Dataset files, metadata and source user/pond",
        "provenance travel together."
      )
    ),
    shiny::tabsetPanel(
      id = "dataset_transfer_section",
      shiny::tabPanel(
        title = "Import dataset",
        value = "import",
        shiny::h3("Drop a dataset bundle here"),
        shiny::fileInput(
          "dataset_bundle_file",
          metadata_field_label("Data Pond dataset bundle (.zip)", "Conditional", "Choose a DataPond ZIP when importing a dataset."),
          accept = c(".zip", "application/zip"),
          multiple = FALSE,
          buttonLabel = "Browse...",
          placeholder = "Drag and drop a Data Pond ZIP here"
        ),
        shiny::uiOutput("dataset_bundle_preview"),
        shiny::actionButton(
          "import_dataset_bundle",
          "Import into active pond",
          class = "btn-primary datapond-requires-active-user"
        ),
        shiny::tags$small(
          style = "display:block; color:#666; margin-top:8px;",
          paste(
            "Import creates a new local dataset record and folder. Existing",
            "datasets and folders are never overwritten."
          )
        )
      ),
      shiny::tabPanel(
        title = "Compress selected dataset",
        value = "export",
        shiny::h3("Create a portable dataset bundle"),
        shiny::tags$p(
          paste(
            "The ZIP is written inside the selected dataset folder. It includes",
            "the complete folder and a dataset-only SQLite metadata snapshot."
          )
        ),
        shiny::uiOutput("dataset_bundle_export_context"),
        shiny::actionButton(
          "create_dataset_bundle",
          "Compress selected dataset",
          class = "btn-primary datapond-requires-active-user"
        ),
        shiny::uiOutput("dataset_bundle_export_result")
      ),
      shiny::tabPanel(
        title = "Metadata review exchange",
        value = "review",
        mod_dataset_review_ui("dataset_review")
      ),
      shiny::tabPanel(
        title = "Import provenance",
        value = "provenance",
        shiny::h3("Selected dataset transfer history"),
        shiny::uiOutput("dataset_import_provenance")
      )
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

    shiny::tags$ol(
      class = "metadata-review-guide",
      `aria-label` = "Metadata review steps",
      shiny::tags$li(
        shiny::strong("1. Record who"),
        shiny::tags$p(shiny::strong("Reviewer / editor: "),
          "your signed-in account. Records who made this update.")
      ),
      shiny::tags$li(
        shiny::strong("2. Explain the change"),
        shiny::tags$p(shiny::strong("Update type: "), "kind of change."),
        shiny::tags$p(shiny::strong("Section(s): "), "where it changed."),
        shiny::tags$p(shiny::strong("Reason: "), "why it was needed."),
        shiny::tags$p(shiny::strong("Summary: "), "what changed (optional).")
      ),
      shiny::tags$li(
        shiny::strong("3. Set review status"),
        shiny::tags$p(shiny::strong("Metadata status: "),
          "shows whether the metadata is draft, checked or approved."),
        shiny::tags$p("Save dataset to keep this update in the history.")
      ),
      shiny::tags$li(
        class = "metadata-responsibility-card",
        shiny::strong("4. Name ongoing contacts"),
        shiny::tags$p(shiny::strong("Creator: "), "prepared the metadata."),
        shiny::tags$p(shiny::strong("Maintainer: "), "keeps it current."),
        shiny::tags$p(shiny::strong("Point of contact: "), "answers questions."),
        shiny::tags$p("Your account is the default when no primary contact is assigned. Add others below."),
        shiny::tags$p("One person can hold all roles.")
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
            metadata_field_label("Reviewer / editor", "Mandatory", "The active signed-in user is recorded as the editor when saving."),
            choices = NULL
          ),

          shiny::tags$small(
            style = paste(
              "color:#625C52;",
              "display:block;",
              "margin-bottom:8px;"
            ),

            paste(
              "The person responsible for this update should",
              "sign in with their own account."
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
            metadata_field_label("Update type", "Mandatory", "Select at least one type when saving; initial creation supplies a default."),
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
            metadata_field_label("Metadata section(s) affected", "Mandatory", "Select at least one affected section when saving; initial creation supplies defaults."),
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
            metadata_field_label("Reason for metadata update", "Conditional", "Required when saving changes to an existing dataset; initial creation supplies a reason."),
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
            "Cells show up to five lines; click text to read it in full.",
            "Drag headings to reorder; open the table icon for Copy, CSV, column widths and column visibility.",
            "The underlying lifecycle status of the dataset",
            "is recorded separately on the Lifecycle tab."
          )
        ),

        shiny::tags$div(
          class = "metadata-review-history",
          DT::DTOutput("metadata_review_table")
        )
      )
    )
  )
}

# Dataset workflow-panel UI helpers ----
dataset_workflow_folder_summary <- function(
  dataset_path = NULL,
  current_dataset_folder = NULL,
  folder_key = NULL,
  dataset_uuid = NULL
) {
  clean_value <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NULL)
    x <- trimws(as.character(x[1]))
    if (!nzchar(x)) NULL else x
  }

  path <- clean_value(dataset_path)
  if (is.null(path)) path <- clean_value(current_dataset_folder)
  if (!is.null(path)) {
    return(list(
      label = "Dataset folder",
      name = basename(path),
      detail = path,
      created = TRUE
    ))
  }

  key <- clean_value(folder_key)
  uuid <- clean_value(dataset_uuid)
  if (!is.null(key)) {
    return(list(
      label = "Proposed folder name",
      name = paste0(key, "__", uuid %||% "<dataset UUID>"),
      detail = paste(
        "Complete Dataset intro, then create the folder.",
        "The dataset UUID is appended to keep the folder identity stable."
      ),
      created = FALSE
    ))
  }

  list(
    label = "Dataset folder",
    name = "Not named yet",
    detail = "Start in Dataset intro to generate the folder name.",
    created = FALSE
  )
}


dataset_workflow_panel_ui <- function() {
  workflow_step <- function(id, number, title, description, section,
                            main_tab = "dataset_workspace") {
    shiny::tags$li(
      class = "dataset-guide-step",
      `data-guide-section` = section,
      `data-guide-main-tab` = main_tab,
      shiny::actionLink(
        id,
        shiny::tagList(
          shiny::tags$span(class = "dataset-guide-step-number", number),
          shiny::tags$span(
            class = "dataset-guide-step-copy",
            shiny::strong(title),
            shiny::tags$small(description)
          )
        )
      )
    )
  }

  shiny::tags$aside(
    class = "dataset-guide",
    `aria-label` = "Dataset workflow guide",
    shiny::tags$div(
      class = "dataset-guide-heading",
      shiny::tags$div(
        shiny::tags$span(class = "dataset-guide-eyebrow", "Current dataset"),
        shiny::h3("Dataset guide")
      ),
      shiny::actionLink(
        "hide_dataset_guide",
        label = NULL,
        icon = shiny::icon("chevron-right"),
        class = "dataset-guide-hide",
        title = "Hide the dataset guide",
        `aria-label` = "Hide the dataset guide",
        `aria-controls` = "dataset_guide_panel"
      )
    ),

    shiny::uiOutput("dataset_guide_folder"),

    shiny::tags$div(
      class = "dataset-guide-primary-actions",
      shiny::actionButton(
        "guide_start_new_dataset",
        "Start a new dataset",
        icon = shiny::icon("plus")
      ),
      dataset_folder_selection_ui("guide_dataset_parent_dir"),
      shiny::actionButton(
        "guide_create_dataset_folder",
        "Create folder & save",
        icon = shiny::icon("folder-plus"),
        class = "btn-primary datapond-requires-active-user"
      )
    ),

    shiny::tags$div(
      class = "dataset-guide-citation-card",
      shiny::tags$h4("Current citation"),
      shiny::uiOutput("dataset_guide_citation")
    ),

    shiny::tags$div(
      class = "dataset-guide-file-actions",
      shiny::actionButton(
        "guide_save_dataset",
        "Save dataset",
        icon = shiny::icon("floppy-disk"),
        class = "btn-success datapond-requires-active-user"
      ),
      shiny::actionButton(
        "guide_open_dataset_folder",
        "Open folder",
        icon = shiny::icon("folder-open")
      )
    ),

    shiny::tags$h4(class = "dataset-guide-steps-heading", "Populate metadata"),
    shiny::tags$p(
      class = "dataset-guide-intro",
      paste(
        "Creating the folder performs the initial save.",
        "Return to Save dataset as you continue working."
      )
    ),
    shiny::tags$div(
      class = "dataset-guide-field-legend",
      shiny::tags$p(flow_requirement_marker("Mandatory"), " Required for the stated action or completion."),
      shiny::tags$p(flow_requirement_marker("Conditional"), " Conditional: see the field guidance."),
      shiny::tags$p("Unmarked fields are optional. Markers describe DataPond entry and completion requirements.")
    ),
    shiny::tags$ol(
      class = "dataset-guide-steps",
      workflow_step(
        "guide_tab_dataset_intro", "1", "Dataset intro",
        "Name the folder; add citation, authors, organisations and description.",
        section = "dataset_intro"
      ),
      workflow_step(
        "guide_tab_licensing", "2", "Licensing & access",
        "Record sharing rights, licence, distribution and contacts.",
        section = "licensing_access"
      ),
      workflow_step(
        "guide_tab_data_files", "3", "Data files",
        "Scan and describe the files contained in the dataset.",
        section = "data_files"
      ),
      workflow_step(
        "guide_tab_spatial", "4", "Spatial metadata",
        "Review extent, reference system and spatial characteristics.",
        section = "spatial_metadata"
      ),
      workflow_step(
        "guide_tab_supporting", "5", "Supporting documents",
        "Catalogue reports, methods, agreements and other documents.",
        section = "supporting_documents"
      ),
      workflow_step(
        "guide_tab_lineage", "6", "Lineage",
        "Document where the data came from and how it was produced.",
        section = "lineage"
      ),
      workflow_step(
        "guide_tab_review", "7", "Metadata review",
        "Record who reviewed the metadata and why it changed.",
        section = "metadata_review"
      ),
      workflow_step(
        "guide_tab_status", "8", "Dataset status",
        "Check completeness and see the recommended next action.",
        section = "dataset_status", main_tab = "dataset_status"
      )
    )
  )
}


dataset_admin_parties_tab <- function() {
  shiny::tabPanel(
    "Parties",
    value = "parties",
    mod_parties_ui("parties")
  )
}


dataset_admin_keywords_tab <- function() {
  shiny::tabPanel(
    "Keyword administration",
    value = "keyword_admin",
    mod_keyword_admin_ui("keyword_admin")
  )
}

# UI layout ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = datapond_asset_url("display-theme.css")),
    tags$script(src = datapond_asset_url("display-theme.js")),
    tags$script(src = datapond_asset_url("dataset-guide.js")),
    tags$script(
      HTML(
        "
        (function () {
          function fallbackCopy(text) {
            var area = document.createElement('textarea');
            area.value = text;
            area.setAttribute('readonly', '');
            area.style.position = 'fixed';
            area.style.opacity = '0';
            document.body.appendChild(area);
            area.select();
            document.execCommand('copy');
            document.body.removeChild(area);
          }

          Shiny.addCustomMessageHandler(
            'datapond-copy-text',
            function (message) {
              var text = String(message.text || '');

              if (
                navigator.clipboard &&
                window.isSecureContext
              ) {
                navigator.clipboard.writeText(text).catch(
                  function () {
                    fallbackCopy(text);
                  }
                );
              } else {
                fallbackCopy(text);
              }
            }
          );

          Shiny.addCustomMessageHandler(
            'datapond-click-input',
            function (message) {
              var target = document.getElementById(String(message.id || ''));
              if (target && !target.disabled) {
                target.click();
              }
            }
          );

          var writeLockActive = true;

          function applyWriteLock(root) {
            var scope = root || document;
            var controls = scope.querySelectorAll(
              '.datapond-requires-active-user'
            );

            controls.forEach(function (control) {
              if (writeLockActive) {
                if (!control.hasAttribute('data-datapond-was-disabled')) {
                  control.setAttribute(
                    'data-datapond-was-disabled',
                    control.disabled ? 'true' : 'false'
                  );
                }
                control.disabled = true;
                control.setAttribute('aria-disabled', 'true');
                control.setAttribute(
                  'title',
                  'Editor or administrator access is required to make changes.'
                );
              } else {
                var wasDisabled = control.getAttribute(
                  'data-datapond-was-disabled'
                );
                if (wasDisabled !== null) {
                  control.disabled = wasDisabled === 'true';
                  control.removeAttribute('data-datapond-was-disabled');
                }
                control.removeAttribute('aria-disabled');
                control.removeAttribute('title');
              }
            });
          }

          Shiny.addCustomMessageHandler(
            'datapond-active-user-write-lock',
            function (message) {
              writeLockActive = Boolean(message.locked);
              document.body.classList.toggle(
                'datapond-write-locked',
                writeLockActive
              );
              applyWriteLock(document);
            }
          );

          document.addEventListener('shiny:connected', function () {
            var observer = new MutationObserver(function (mutations) {
              mutations.forEach(function (mutation) {
                mutation.addedNodes.forEach(function (node) {
                  if (node.nodeType !== 1) {
                    return;
                  }
                  if (node.matches('.datapond-requires-active-user')) {
                    applyWriteLock(node.parentNode || document);
                  } else if (node.querySelector) {
                    applyWriteLock(node);
                  }
                });
              });
            });

            observer.observe(document.body, {
              childList: true,
              subtree: true
            });
            applyWriteLock(document);
          });
        })();
        "
      )
    )
  ),
  tags$audio(
    id = "datapond_finfoot_audio",
    preload = "auto",
    playsinline = NA,
    `aria-hidden` = "true",
    style = "display:none;",
    tags$source(
      src = datapond_asset_url("finfoot-xc1036489.mp3"),
      type = "audio/mpeg"
    )
  ),
  tags$div(
    class = "datapond-panel-toolbar",
    actionButton(
      "toggle_config", label = NULL,
      icon = shiny::icon("gear"),
      title = "Hide data pond configuration",
      `aria-label` = "Hide data pond configuration",
      `aria-controls` = "config_panel",
      `aria-expanded` = "true"
    ),
    tags$a(
      id = "open_user_guide", href = "datapond-guide/index.html",
      target = "_blank", rel = "noopener", class = "btn btn-default datapond-guide-link",
      title = "Open user guide", `aria-label` = "Open user guide",
      tags$span(`aria-hidden` = "true", "?")
    ),
    actionButton(
      "toggle_dataset_guide", "Dataset guide",
      icon = shiny::icon("chevron-right"),
      title = "Hide the dataset guide",
      `aria-label` = "Hide the dataset guide",
      `aria-controls` = "dataset_guide_panel",
      `aria-expanded` = "true"
    )
  ),
  shiny::uiOutput("active_user_write_status"),
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

      .dataset-guide-field-legend {
        margin: 8px 0 12px;
        color: #6A645B;
        font-size: 11px;
        line-height: 1.4;
      }
      .dataset-guide-field-legend p { margin: 3px 0; }
      .dataset-guide-field-legend .flow-field-requirement {
        display: inline-block;
        font-size: 14px;
        font-weight: 600;
        line-height: 1;
      }
      .dataset-guide-field-legend .mandatory { color: #7A2525; }
      .dataset-guide-field-legend .conditional { color: #65512E; }

      .datapond-specialist-metadata .row {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .datapond-specialist-metadata .row::before,
      .datapond-specialist-metadata .row::after { display: none; }
      .datapond-specialist-metadata .row > [class*='col-'] {
        width: auto;
        min-width: 0;
      }
      .datapond-specialist-metadata .shiny-input-container { width: 100%; }
      .metadata-guided-field { margin-bottom: 18px; }
      .metadata-guided-field .shiny-input-container { margin-bottom: 4px; }
      .metadata-field-help {
        color: #59564E;
        font-size: 13px;
        line-height: 1.5;
      }
      /* Match the existing 12px field hints; preserve labels, headings,
         disclosure titles and the citation preview at their current sizes. */
      .tab-pane[data-value='dataset_intro'] .help-block,
      .tab-pane[data-value='dataset_intro'] p.text-muted,
      .datapond-specialist-metadata p,
      .datapond-specialist-metadata .help-block,
      .datapond-metadata-guidance p,
      .datapond-metadata-guidance .help-block {
        font-size: 12px;
        line-height: 1.45;
      }
      @media (max-width: 767px) {
        .datapond-specialist-metadata .row { grid-template-columns: minmax(0, 1fr); }
      }

      .datapond-write-lock-status {
        padding: 9px 12px;
        margin: 8px 0 12px 0;
        border: 1px solid #C3A35B;
        border-radius: 3px;
        background: #FFF4CE;
        color: #5F4B13;
      }

      .datapond-write-locked .datapond-requires-active-user {
        cursor: not-allowed;
        opacity: 0.55;
      }

      #config_panel {
        padding-right: 0;
        max-height: calc(100vh - 20px);
        overflow-y: auto;
        position: sticky;
        top: 10px;
      }

      #main_panel {
        transition: width 0.2s ease;
        min-width: 0;
      }

      .datapond-title-line {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
        width: 100%;
      }

      .datapond-title-version {
        font-size: 11px;
        font-weight: 400;
        color: #77736B;
        white-space: nowrap;
        margin-left: 8px;
      }

      .datapond-title-brand {
        display: inline-flex;
        align-items: center;
        gap: 10px;
        max-width: 100%;
      }

      .datapond-title-logo {
        width: 56px;
        height: 56px;
        object-fit: contain;
        flex-shrink: 0;
      }

      .datapond-config-logo {
        display: block;
        width: 120px;
        max-width: 100%;
        height: auto;
        margin: 0 auto 10px;
      }

      #population_title_badge {
        margin-left: auto;
        text-align: right;
        line-height: 1;
      }

      /* Rounded surfaces, leaving table cells and layout columns intact. */
      .dataset-guide,
      .pond-config-intro-card,
      .dataset-guide-folder-card,
      .dataset-guide-citation-card,
      .config-section-box,
      .pond-config-guide .config-help,
      .pond-config-guide .config-current-value,
      .datapond-optional-details,
      .datapond-write-lock-status,
      .panel, .well, .alert, .modal-content,
      #main_panel details,
      #main_panel div[style*='border:'] {
        border-radius: 10px !important;
      }

      #main_panel > h2 {
        border-radius: 10px 10px 0 0;
      }

      #main_panel > .tabbable > .nav-tabs {
        border-radius: 0 0 10px 10px;
      }

      .datapond-panel-toolbar {
        display: flex;
        flex-wrap: wrap;
        gap: 6px;
        margin-bottom: 8px;
      }

      #dataset_guide_panel {
        max-height: calc(100vh - 20px);
        overflow-y: auto;
        position: sticky;
        top: 10px;
      }

      .dataset-guide {
        padding: 12px;
        background: #F8F4EA;
        border: 1px solid #C3A35B;
        color: #35443A;
      }

      .dataset-guide-heading {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 8px;
        margin-bottom: 10px;
      }

      .dataset-guide-heading h3 {
        margin: 1px 0 0 0;
      }

      .pond-config-guide .dataset-guide-heading {
        text-align: right;
      }

      .dataset-guide-eyebrow {
        display: block;
        color: #6A645B;
        font-size: 11px;
        font-weight: 600;
        letter-spacing: 0.06em;
        text-transform: uppercase;
      }

      .dataset-guide-hide {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 32px;
        min-height: 32px;
        font-size: 14px;
        white-space: nowrap;
      }

      .pond-config-intro-card {
        padding: 9px 10px;
        margin-bottom: 10px;
        background: #FFFFFF;
        border: 1px solid #D3CEC4;
        border-left: 3px solid #526657;
      }

      .pond-config-intro-card p {
        margin: 0;
      }

      .pond-config-guide .form-group {
        margin-bottom: 10px;
      }

      .pond-config-guide .config-section-box h4 {
        margin: 0 0 10px 0 !important;
        font-size: 15px;
        line-height: 1.25;
      }

      .pond-config-guide .config-help {
        border-radius: 3px;
        margin-bottom: 8px;
      }

      .pond-config-guide .config-current-value {
        border-radius: 3px;
        font-size: 12px;
        line-height: 1.4;
      }

      .pond-config-guide .config-section-box > .btn,
      .pond-config-guide .config-subsection > .btn,
      .pond-config-guide details > .btn,
      .pond-config-guide .config-advanced > .btn {
        width: 100%;
        margin-bottom: 6px;
        white-space: normal;
      }

      .pond-config-guide .config-action-stack {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        margin-top: 8px;
      }

      .pond-config-guide .config-action-stack .btn {
        width: 100%;
        white-space: normal;
      }

      .pond-config-guide pre {
        max-height: 120px;
        margin: 5px 0 9px 0;
        padding: 7px;
        overflow: auto;
        white-space: pre-wrap;
        overflow-wrap: anywhere;
        font-size: 11px;
        line-height: 1.35;
      }

      .pond-config-guide hr {
        margin: 10px 0;
      }

      .pond-config-guide .config-subsection {
        padding-top: 10px;
        margin-top: 10px;
        border-top: 1px solid #D3CEC4;
      }

      .pond-config-guide .config-subsection h4,
      .pond-config-guide .config-advanced > h4 {
        margin: 2px 0 9px;
        font-size: 14px;
      }

      .pond-config-guide .config-context-details {
        margin: 9px 0 0;
      }

      .pond-config-guide .dataTables_wrapper {
        font-size: 11px;
      }

      .dataset-guide-folder-card,
      .dataset-guide-citation-card {
        padding: 10px;
        margin-bottom: 10px;
        background: #FFFFFF;
        border: 1px solid #D3CEC4;
        border-left: 3px solid #526657;
      }

      .dataset-guide-folder-label,
      .dataset-guide-citation-card h4 {
        display: block;
        margin: 0 0 5px 0;
        color: #625C52;
        font-size: 12px;
        font-weight: 600;
        text-transform: uppercase;
      }

      .dataset-guide-folder-name {
        display: block;
        font-family: Menlo, Monaco, Consolas, monospace;
        font-size: 12px;
        overflow-wrap: anywhere;
      }

      .dataset-guide-folder-detail,
      .dataset-guide-intro,
      .dataset-guide-citation-empty {
        display: block;
        margin: 5px 0 0 0;
        color: #6A645B;
        font-size: 11px;
        line-height: 1.4;
        overflow-wrap: anywhere;
      }

      .dataset-guide-folder-state {
        display: inline-block;
        margin-top: 7px;
        padding: 2px 6px;
        border-radius: 10px;
        background: #E9DFB5;
        color: #5F4B13;
        font-size: 10px;
        font-weight: 600;
      }

      .dataset-guide-folder-state.is-created {
        background: #D1DDC7;
        color: #35443A;
      }

      .dataset-guide-citation {
        margin: 0;
        padding-left: 1.25rem;
        text-indent: -1.25rem;
        font-size: 12px;
        line-height: 1.5;
        overflow-wrap: anywhere;
      }

      .dataset-guide-primary-actions,
      .dataset-guide-file-actions {
        display: grid;
        grid-template-columns: 1fr;
        gap: 6px;
        margin-bottom: 10px;
      }

      .dataset-guide-primary-actions .btn,
      .dataset-guide-file-actions .btn {
        width: 100%;
        white-space: normal;
      }

      .dataset-guide-steps-heading {
        margin: 14px 0 2px 0;
      }

      .dataset-guide-steps {
        list-style: none;
        margin: 9px 0 0 0;
        padding: 0;
      }

      .dataset-guide-step {
        margin: 0 0 6px 0;
      }

      .dataset-guide-step > a {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 6px;
        padding: 9px 8px;
        border: 1px solid transparent;
        border-radius: 3px;
        color: #3F5146;
        text-align: center;
        text-decoration: none;
      }

      .dataset-guide-step > a:hover,
      .dataset-guide-step > a:focus {
        background: #E7EEE3;
        border-color: #A8B59D;
        text-decoration: none;
      }

      .dataset-guide-step.is-active > a {
        background: #FFF4CE;
        border-color: #C3A35B;
      }

      .dataset-guide-step-number {
        display: inline-flex;
        flex: 0 0 28px;
        align-items: center;
        justify-content: center;
        width: 28px;
        min-width: 28px;
        height: 28px;
        padding: 0;
        margin: 0 auto 1px auto;
        border-radius: 999px;
        background: #526657;
        box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.22);
        color: #FFFFFF;
        font-size: 12px;
        font-weight: 700;
        font-variant-numeric: tabular-nums;
        line-height: 1;
        text-align: center;
      }

      .dataset-guide-step-copy {
        min-width: 0;
        width: 100%;
      }

      .dataset-guide-step-copy strong,
      .dataset-guide-step-copy small {
        display: block;
      }

      .dataset-guide-step-copy small {
        margin-top: 2px;
        color: #6A645B;
        line-height: 1.35;
      }

      /* Keep the primary title and navigation together on a warm parchment
         header while leaving the active tab content on white. */
      #main_panel > h2 {
        color: #35443A;
        background-color: #F1E9DA;
        border: 1px solid #C3A35B;
        border-bottom: 0;
        margin: 0;
        padding: 12px 14px 8px;
      }

      #main_panel > .tabbable > .nav-tabs {
        background-color: #F1E9DA;
        border: 1px solid #C3A35B;
        border-top: 0;
        padding: 0 10px 4px;
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

      #dataset_table table.dataTable {
        font-size: 12px;
      }
      .overview-abstract-card {
        margin: 8px 0;
        padding: 9px;
        background: #F8F4EA;
        border: 1px solid #D3CEC4;
        font-size: 12px;
      }
      .overview-abstract-card h4 {
        margin: 0 0 7px;
        font-size: 14px;
      }
      #dataset_table table.dataTable th,
      #dataset_table table.dataTable td {
        padding: 5px 7px;
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

      #main_dataset_tabs > li > a[data-value='parties'],
      #main_dataset_tabs > li > a[data-value='keyword_admin'] {
        color: #625C52 !important;
        background: #ECEAE5 !important;
        border-color: #D3CEC4 !important;
        font-size: 12px;
      }

      #main_dataset_tabs > li > a[data-value='parties']:hover,
      #main_dataset_tabs > li > a[data-value='keyword_admin']:hover,
      #main_dataset_tabs > li.active > a[data-value='parties'],
      #main_dataset_tabs > li.active > a[data-value='keyword_admin'] {
        color: #FFFFFF !important;
        background: #7A8079 !important;
        border-color: #6B716A !important;
      }

      @media (max-width: 767px) {
        #config_panel,
        #dataset_guide_panel {
          position: static;
          max-height: none;
        }

        #dataset_guide_panel {
          margin-top: 12px;
        }
      }
      "
    )
  ),
  tags$script(HTML("
    function syncDatapondPanelColumns() {
      var configPanel = document.getElementById('config_panel');
      var guidePanel = document.getElementById('dataset_guide_panel');
      var mainPanel = document.getElementById('main_panel');
      var configVisible = configPanel.style.display !== 'none';
      var guideVisible = guidePanel.style.display !== 'none';

      function syncToggle(id, visible, collapseDirection, panelName) {
        var button = document.getElementById(id);
        var icon = button.querySelector('i');
        var direction = visible ? collapseDirection :
          (collapseDirection === 'left' ? 'right' : 'left');
        if (id !== 'toggle_config') {
          icon.classList.remove('fa-chevron-left', 'fa-chevron-right');
          icon.classList.add('fa-chevron-' + direction);
        }
        var label = (visible ? 'Hide ' : 'Show ') + panelName;
        button.setAttribute('title', label);
        button.setAttribute('aria-label', label);
        button.setAttribute('aria-expanded', String(visible));
      }

      syncToggle('toggle_config', configVisible, 'left', 'data pond configuration');
      syncToggle('toggle_dataset_guide', guideVisible, 'right', 'the dataset guide');

      mainPanel.classList.remove('col-sm-6', 'col-sm-9', 'col-sm-12');

      if (configVisible && guideVisible) {
        mainPanel.classList.add('col-sm-6');
      } else if (configVisible || guideVisible) {
        mainPanel.classList.add('col-sm-9');
      } else {
        mainPanel.classList.add('col-sm-12');
      }

      window.setTimeout(function() {
        window.dispatchEvent(new Event('resize'));
      }, 220);
    }

    Shiny.addCustomMessageHandler('toggleConfig', function(message) {
      var panel = document.getElementById('config_panel');

      if (panel.style.display === 'none') {
        panel.style.display = 'block';
      } else {
        panel.style.display = 'none';
      }

      syncDatapondPanelColumns();
    });

    Shiny.addCustomMessageHandler('toggleDatasetGuide', function(message) {
      var panel = document.getElementById('dataset_guide_panel');
      panel.style.display = panel.style.display === 'none' ? 'block' : 'none';
      syncDatapondPanelColumns();
    });

    function showDatapondDatasetGuide() {
      var panel = document.getElementById('dataset_guide_panel');
      if (panel.style.display === 'none') {
        panel.style.display = 'block';
        syncDatapondPanelColumns();
      }
    }

    function hideDatapondSidePanels() {
      var configPanel = document.getElementById('config_panel');
      var guidePanel = document.getElementById('dataset_guide_panel');

      if (configPanel) {
        configPanel.style.display = 'none';
      }
      if (guidePanel) {
        guidePanel.style.display = 'none';
      }

      syncDatapondPanelColumns();
    }

    Shiny.addCustomMessageHandler('showDatasetGuide', function(message) {
      showDatapondDatasetGuide();
    });

    Shiny.addCustomMessageHandler('hideDatapondSidePanels', function(message) {
      hideDatapondSidePanels();
    });

    document.addEventListener('click', function(event) {
      var link = event.target.closest(
        '#main_dataset_tabs > li > a[data-value=dataset_workspace]'
      );
      if (link) {
        showDatapondDatasetGuide();
      }
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
      width = 6,
      id = "main_panel",
      titlePanel(
        shiny::tags$span(
          class = "datapond-title-line",
          shiny::tags$span(
            class = "datapond-title-brand",
            shiny::tags$img(
              src = "datapond-assets/datapond-logo.png",
              alt = "",
              class = "datapond-title-logo",
              width = 56,
              height = 56
            ),
            shiny::tags$span(
              "Data pond",
              shiny::tags$span(
                class = "datapond-title-version",
                paste0("v", as.character(utils::packageVersion("datapond")))
              )
            )
          ),
          shiny::uiOutput("population_title_badge", inline = TRUE, `aria-live` = "polite")
        ),
        windowTitle = "Data pond"
      ),
      tabsetPanel(
        id = "main_dataset_tabs",
        selected = "pond_overview",
        shiny::tabPanel(
          "Pond overview",
          value = "pond_overview",
          pond_overview_ui()
        ),
        shiny::tabPanel(
          "Flow",
          value = "data_movement",
          mod_data_movement_ui("data_movement")
        ),
        shiny::tabPanel(
          "Pond links",
          value = "dataset_transfer",
          dataset_transfer_ui()
        ),
        tabPanel(
          "Dataset lifecycle",
          value = "dataset_lifecycle",
          mod_dataset_lifecycle_ui("dataset_lifecycle",
            record_controls = shiny::uiOutput("lifecycle_record_controls"))
        ),
        shiny::tabPanel(
          "Create / edit dataset",
          value = "dataset_workspace",
          shiny::tabsetPanel(
            id = "dataset_edit_sections",
            type = "hidden",
            selected = "dataset_intro",
            shiny::tabPanel(
              "Dataset intro",
              value = "dataset_intro",
          fluidRow(
            column(
              width = 7,
              h3("Essential metadata"),
              h4("Citation"),
              uiOutput("citation_fields"),
              mod_authors_ui("authors"),
              mod_dataset_org_ui("orgs"),
              hr(),
              h4("About"),
              uiOutput("about_fields"),
              uiOutput("about_discovery_fields"),
              metadata_population_detail(mod_topic_categories_ui("iso_profile")),
              hr(),
              metadata_population_detail(metadata_optional_details(
                "Optional metadata",
                tags$p(class = "text-muted", "Open only the sections relevant to this dataset. Closing a box keeps your entries."),
                metadata_optional_details(
                  "Alternate titles",
                  mod_wave2_alternate_titles_ui("iso_wave2", heading = FALSE)
                ),
                metadata_optional_details(
                  "Additional citation dates",
                  mod_citation_dates_ui("iso_profile", heading = FALSE)
                ),
                metadata_optional_details(
                  "Temporal extents \u2014 time covered by the data",
                  mod_wave2_temporal_extents_ui("iso_wave2", heading = FALSE)
                ),
                metadata_optional_details(
                  "Update frequency",
                  uiOutput("about_optional_fields")
                )
              ))
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
        shiny::tabPanel(
          "Licensing & data access",
          value = "licensing_access",

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
                  ),

                  mod_wave2_distribution_ui(
                    "iso_wave2"
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
          value = "data_files",
          mod_data_files_ui(
            "data_files",
            dictionary_ui = mod_feature_catalogue_ui("iso_wave3"),
            quality_ui = tags$div(
              class = "datapond-specialist-metadata",
              metadata_optional_details(
                "Data quality \u2014 tests and results",
                mod_wave2_quality_ui("iso_wave2")
              )
            )
          )
        ),
        tabPanel(
          "Spatial metadata",
          value = "spatial_metadata",
          data_files_spatial_ui(
            "data_files",
            vertical_ui = mod_vertical_extents_ui("iso_wave3")
          )
        ),
        tabPanel(
          "Supporting documents",
          value = "supporting_documents",
          mod_supporting_documents_ui("supporting_docs")
        ),
        tabPanel(
          "Lineage",
          value = "lineage",
          mod_lineage_ui("lineage")
        ),
        shiny::tabPanel(
          "Metadata review",
          value = "metadata_review",

          metadata_review_ui(),
          tags$div(
            class = "datapond-metadata-guidance",
            metadata_optional_details(
              "Metadata responsibilities",
              mod_metadata_responsibilities_ui("iso_profile", heading = FALSE)
            ),
            mod_metadata_profile_ui("iso_profile")
          )
        ),
          )
        ),
        tabPanel(
          "Dataset status",
          value = "dataset_status",
          mod_metadata_document_ui("metadata_document"),
          shiny::hr(),
          mod_iso_export_ui("iso_export"),
          shiny::hr(),
          mod_dataset_status_ui("dataset_status")
        )
      ),
      br(),
      br(),
      hr(),
      actionButton("clear_dataset_form", "Clear form / Start new record"),
      actionButton(
        "new_dataset",
        "Create Folder & Save Dataset",
        class = "datapond-requires-active-user"
      ),
      actionButton("open_dataset_folder", "Open Dataset Folder"),
      actionButton("open_data_folder", "Open Data Folder"),
      actionButton(
        "save",
        "Save Dataset",
        class = "datapond-requires-active-user"
      ),
      br(),
      hr(),
    ),

    # RIGHT DATASET WORKFLOW PANEL
    shiny::column(
      width = 3,
      id = "dataset_guide_panel",
      dataset_workflow_panel_ui()
    )
  )
)


# server ----
pond_startup_error_dialog <- function(message, registry = read_connected_pond_registry()) {
  choices <- if (nrow(registry)) stats::setNames(registry$data_pond_path, paste0(
    registry$pond_name, " - ", registry$data_pond_path,
    ifelse(vapply(registry$data_pond_path, function(path) {
      dir.exists(path) && file.exists(get_db_path(path))
    }, logical(1)), "", " (unavailable)")
  )) else character()
  shiny::modalDialog(
    title = "Data Pond could not be opened",
    shiny::p(message),
    shiny::p("Check that the pond's drive or network folder is connected and accessible, then retry."),
    shiny::p("Your saved pond selection has been kept."),
    shiny::tags$hr(),
    shiny::h4("Open a different pond"),
    if (length(choices)) shiny::selectInput(
      "recovery_known_pond", "Connected ponds",
      choices = c("Choose a pond" = "", choices)
    ),
    shiny::textInput("recovery_pond_path", "Existing pond folder", placeholder = "/path/to/data_pond"),
    shiny::p("Opening another pond makes it active for this session and remembers it for the next launch."),
    easyClose = FALSE,
    footer = shiny::tagList(
      shiny::actionButton("exit_datapond", "Exit DataPond"),
      shiny::actionButton("retry_pond_startup", "Retry opening pond"),
      shiny::actionButton("open_recovery_pond", "Open selected pond", class = "btn-primary")
    )
  )
}

pond_startup_recovery_server <- function(input, output, session) {
  roots <- data_pond_folder_roots()
  shinyFiles::shinyDirChoose(
    access_directory_input(input), "recovery_pond_folder", roots = roots, session = session,
    allowDirCreate = FALSE
  )
  opened_state <- shiny::reactiveVal(NULL)
  observers <- list(
    shiny::observeEvent(input$retry_pond_startup, {
      session$reload()
    }, ignoreInit = TRUE),
    shiny::observeEvent(input$recovery_known_pond, {
      shiny::req(nzchar(input$recovery_known_pond))
      shiny::updateTextInput(session, "recovery_pond_path", value = input$recovery_known_pond)
    }, ignoreInit = TRUE),
    shiny::observeEvent(input$recovery_pond_folder, {
      path <- shinyFiles::parseDirPath(roots, input$recovery_pond_folder)
      if (length(path) == 1L && nzchar(path)) {
        shiny::updateTextInput(session, "recovery_pond_path", value = path)
      }
    }, ignoreInit = TRUE),
    shiny::observeEvent(input$open_recovery_pond, {
      tryCatch({
        path <- validate_existing_data_pond(input$recovery_pond_path)
        # Continue in this session. Changing the process-wide launch option
        # would also redirect other browser sessions using this R process.
        state <- server(input, output, session, startup_path = path, recovering = TRUE)
        opened_state(state)
        for (observer in observers) observer$destroy()
        shiny::removeModal(session = session)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", session = session)
      })
    }, ignoreInit = TRUE)
  )
  list(opened_state = opened_state)
}

exit_data_pond <- function(session) {
  # The desktop launcher owns its R runApp() call. Hosted apps must close only
  # this browser session, leaving other users connected.
  if (isTRUE(getOption("datapond.stop_on_exit", FALSE))) {
    shiny::stopApp()
  } else {
    session$close()
  }
}

server <- function(input, output, session, startup_path = NULL, recovering = FALSE) {
  # Register before opening storage, including when startup cannot find a pond.
  # Recovery re-enters this function in the same session: keep one observer.
  if (!isTRUE(recovering)) {
    mod_document_tools_server("document_tools")
    shiny::observeEvent(input$exit_datapond, {
      shiny::req(input$exit_datapond > 0)
      exit_data_pond(session)
    }, ignoreInit = TRUE)
  }
  ns <- session$ns
  schema <- dataset_overview_schema()

  ## Resolve initial pond ----
  requested_data_pond <- startup_path %||% getOption("datapond.data_pond")
  has_requested_data_pond <- (
    !is.null(requested_data_pond) &&
    length(requested_data_pond) > 0L &&
    !is.na(requested_data_pond[1]) &&
    nzchar(trimws(as.character(requested_data_pond[1])))
  )

  # Stop before registering dataset modules if the selected storage cannot be
  # opened. Keep the session alive so reconnecting the drive needs only a retry.
  startup_error <- tryCatch({
    config <- if (isTRUE(has_requested_data_pond)) {
      requested_path <- normalizePath(
        path.expand(as.character(requested_data_pond[1])),
        mustWork = FALSE,
        winslash = "/"
      )
      if (identical(requested_path, read_last_data_pond_path())) {
        assert_saved_data_pond_available(requested_path)
      }
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

    movement_assert_writable(data_pond_path)
    initial_pond_config <- read_pond_config(data_pond_path)

    # User identity is session-specific. Do not inherit a reviewer/user saved
    # by somebody else in the shared pond configuration.
    initial_active_user_id <- NULL

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
    backfill_dataset_markers(data_pond_path)
    backfill_missing_citation_keys(data_pond_path)
    backfill_missing_folder_keys(data_pond_path)

    conn <- get_conn(data_pond_path)

    initial_data_pond_id <- tryCatch(
      ensure_data_pond_id(data_pond_path, conn = conn),
      finally = DBI::dbDisconnect(conn)
    )
    initial_pond_config$data_pond_id <- initial_data_pond_id
    write_last_data_pond_path(data_pond_path)
    NULL
  }, error = function(e) e)

  if (!is.null(startup_error)) {
    if (isTRUE(recovering)) stop(startup_error)
    shiny::showModal(pond_startup_error_dialog(conditionMessage(startup_error)), session = session)
    startup_recovery <- pond_startup_recovery_server(input, output, session)
    return(invisible(NULL))
  }

  ## schema sections ----
  # Load packaged reference vocabularies.
  keywords_vocab <- load_vocab(datapond_resource("vocab_keywords.csv"))
  org_dt <- load_organisations_csv(
    datapond_resource("vocab_organisations.csv")
  )
  persons_dt <- fread(datapond_resource("vocab_persons.csv"))
  citation_dt <- get_section(schema, "Dataset overview", "Citation")
  about_dt    <- get_section(schema, "Dataset overview", "About")
  first_keyword_row <- which(grepl("^keyword_", about_dt$field))[1]
  if (is.na(first_keyword_row)) {
    about_intro_dt <- about_dt
    about_discovery_dt <- about_dt[0]
  } else {
    about_intro_dt <- about_dt[seq_len(first_keyword_row - 1L)]
    about_discovery_dt <- about_dt[first_keyword_row:nrow(about_dt)]
  }
  about_theme_dt <- about_discovery_dt[
    about_discovery_dt$field == "keyword_object",
  ]
  about_extra_keywords_dt <- about_discovery_dt[
    grepl("^keyword_", about_discovery_dt$field) &
      about_discovery_dt$field != "keyword_object",
  ]
  about_optional_dt <- about_discovery_dt[
    !grepl("^keyword_", about_discovery_dt$field),
  ]


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

    stage <- dataset_lifecycle_stage(
      dataset_path,
      state$data_pond
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
    overview_preview_id(NULL)
    overview_selected_connection(NULL)
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
    iso_profile_mod$clear()
    iso_wave2_mod$clear()
    iso_wave3_mod$clear()
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
      parent %||% state$dataset_parent_dir
    )
    state$dataset_parent_dir <- parent
    if (!is.null(state$data_pond) &&
        length(state$data_pond) > 0 &&
        !is.na(state$data_pond) &&
        state$data_pond != "") {
      save_config(
        path = state$data_pond,
        active_user_id = state$active_user_id,
        dataset_parent_dir = parent
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
  switch_data_pond <- function(path, created = FALSE) {
    # Opening must never silently initialise an unrelated or empty folder.
    path <- if (created) path else validate_existing_data_pond(path)

    # A browser may select a pond before signing in, but cannot migrate or seed
    # that catalogue until it authenticates there. New-pond creation is admin-only.
    if (!created && !access_allowed("read", path)) {
      auth$invalidate()
      reset_context_after_pond_change()
      state$data_pond <- path
      state$data_pond_id <- NULL
      state$pond_config <- read_pond_config(path)
      auth$pond <- path
      auth$configured <- file.exists(access_db_path(path))
      write_last_data_pond_path(path)
      return(invisible(path))
    }

    ensure_pond_structure(path)
    init_db(path)
    ensure_entry_data_access_columns(path)
    backfill_missing_dataset_uuids(path)
    backfill_dataset_markers(path)
    backfill_missing_citation_keys(path)
    backfill_missing_folder_keys(path)

    conn <- get_conn(path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    data_pond_id <- ensure_data_pond_id(
      path,
      conn = conn
    )

    pond_config <- read_pond_config(path)

    auth$invalidate()
    reset_context_after_pond_change()

    state$data_pond <- path
    state$data_pond_id <- data_pond_id
    state$pond_config <- pond_config
    state$active_user_id <- NULL
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
      dataset_parent_dir = state$dataset_parent_dir
    )

    try(
      register_connected_pond(path),
      silent = TRUE
    )
    connected_registry_tick(connected_registry_tick() + 1L)
    connected_inventory_refresh_tick(connected_inventory_refresh_tick() + 1L)

    refresh_dataset_context()

    showNotification(
      if (created) paste0("Created and opened '", basename(path), "'.") else
        paste("Data pond opened:", path),
      type = "message",
      duration = 10
    )
    play_finfoot_cue(session, if (created) "pond-created" else "pond-opened")

    auth$invalidate()
    auth$pond <- path
    auth$configured <- file.exists(access_db_path(path))

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
          state$data_pond,
          organisation_id = as.integer(
            poc_df$organisation_id[i]
          )
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

  auth <- access_server(input, output, session, state, switch_data_pond,
    on_login = function() {
      path <- state$data_pond
      ensure_pond_structure(path)
      init_db(path)
      ensure_entry_data_access_columns(path)
      backfill_missing_dataset_uuids(path)
      backfill_dataset_markers(path)
      backfill_missing_citation_keys(path)
      backfill_missing_folder_keys(path)
      state$data_pond_id <- ensure_data_pond_id(path)
      state$pond_config <- read_pond_config(path)
      state$dataset_parent_dir <- normalise_dataset_parent_dir(state$pond_config$dataset_parent_dir %||% "dev")
      seed_pond_reference_data(path, state$active_user_id)
      state$refresh_person <- state$refresh_person + 1L
      state$refresh_org <- state$refresh_org + 1L
      state$refresh_vocab <- state$refresh_vocab + 1L
      reset_context_after_pond_change()
      refresh_dataset_context()
      # Keep the local chooser current after signing into a pond selected at login.
      try(register_connected_pond(path), silent = TRUE)
      connected_registry_tick(connected_registry_tick() + 1L)
      play_finfoot_cue(session, "startup")
    })

  connected_registry_tick <- shiny::reactiveVal(0L)
  connected_inventory_refresh_tick <- shiny::reactiveVal(0L)
  connected_inventory_cache <- new.env(parent = emptyenv())

  try(
    register_connected_pond(state$data_pond),
    silent = TRUE
  )

  connected_pond_registry <- shiny::reactive({
    connected_registry_tick()
    read_connected_pond_registry()
  })

  # Poll access and catalogue changes without invalidating tables on every
  # security heartbeat. reactiveVal publishes only when the snapshot changes.
  connected_pond_poll_state <- shiny::reactiveVal(NULL)
  shiny::observe({
    access_require("read")
    auth$tick
    paths <- unique(c(state$data_pond, connected_pond_registry()$data_pond_path))
    snapshot <- lapply(paths, function(path) {
      list(path = path, exists = dir.exists(path),
        signature = connected_pond_database_signature(path),
        readable = tryCatch({ connected_pond_require_read(path); TRUE }, error = function(e) FALSE))
    })
    connected_pond_poll_state(snapshot)
  })

  ## Seed database ----
  # Seed the initial pond once per Shiny session.
  seed_pond_reference_data(
    path = data_pond_path,
    user_id = initial_active_user_id
  )

  population_profile <- shiny::reactive({
    match.arg(input$population_profile %||% "rough", c("rough", "full"))
  })
  output$population_title_badge <- shiny::renderUI({
    access_require("read")
    metadata_population_title_badge(population_profile())
  })

  ## Initialize modules ----
  overview_remote_pond_request <- shiny::reactiveVal(NULL)
  movement_state <- mod_data_movement_server(
    "data_movement",
    pond = shiny::reactive(state$data_pond),
    dataset_path = shiny::reactive(state$dataset_path),
    open_pond_request = overview_remote_pond_request
  )

  dataset_status_mod <- mod_dataset_status_server(
    "dataset_status",
    state = state,
    schema_grouped = schema,
    population_profile = population_profile
  )
  dataset_lifecycle_mod <- mod_dataset_lifecycle_server(
    "dataset_lifecycle",
    state = state,
    schema_grouped = schema,
    on_restored = function() {
      last_hydrated_dataset_id(NULL)
      refresh_dataset_context()
    }
  )
  mod_parties_server("parties", state = state)
  mod_keyword_admin_server(
    "keyword_admin",
    state = state,
    vocab_path = datapond_resource("vocab_keywords.csv")
  )
  authors_mod <- mod_authors_server("authors", state = state)
  org_mod <- mod_dataset_org_server("orgs", state = state)
  iso_profile_mod <- mod_iso_profile_server("iso_profile", state = state)
  iso_wave2_mod <- mod_iso_wave2_server("iso_wave2", state = state)
  iso_wave3_mod <- mod_iso_wave3_server("iso_wave3", state = state)
  mod_metadata_document_server("metadata_document", state = state)
  mod_iso_export_server("iso_export", state = state, open_section = function(section, field) {
    shiny::updateCheckboxInput(session, "show_population_detail", value = TRUE)
    shiny::updateTabsetPanel(session, "main_dataset_tabs", selected = "dataset_workspace")
    shiny::updateTabsetPanel(session, "dataset_edit_sections", selected = section)
    if (section == "data_files" && startsWith(field, "iso_wave3-catalogue_")) {
      shiny::updateTabsetPanel(session, "data_files-data_files_section", selected = "attributes")
    } else if (section == "data_files" && startsWith(field, "iso_wave2-quality_")) {
      shiny::updateTabsetPanel(session, "data_files-data_files_section", selected = "quality")
    } else if (section == "spatial_metadata" && startsWith(field, "iso_wave3-vertical_")) {
      shiny::updateTabsetPanel(session, "data_files-spatial_metadata_section", selected = "dataset_extent")
    }
    if (nzchar(field)) session$onFlushed(function() {
      session$sendCustomMessage("datapondIsoField", field)
    }, once = TRUE)
  })

  poc_mod <- mod_poc_server(
    "poc",
    state = state,
    organisation_rows = org_mod$orgs
  )

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

  folder_key_preview_text <- shiny::reactive({
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
  lineage_mod <- mod_lineage_server(
    "lineage",
    state,
    open_supporting_documents = function() {
      shiny::updateTabsetPanel(
        session,
        "main_dataset_tabs",
        selected = "dataset_workspace"
      )
      shiny::updateTabsetPanel(
        session,
        "dataset_edit_sections",
        selected = "supporting_documents"
      )
    }
  )


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
    access_require("read")
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
      },
      population_visibility = TRUE
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
    access_require("read")
    # Explicit dependencies ensure this UI is rebuilt when
    # the active pond or its keyword vocabulary changes.
    state$data_pond
    state$refresh_vocab

    about_fields_ready(
      FALSE
    )

    base_path <- state$data_pond

    section_ui <- generate_section(
      about_intro_dt,
      ns,
      function(field_code) {
        get_choices_for_pond(
          field_code = field_code,
          base_path = base_path
        )
      },
      population_visibility = TRUE
    )

    section_ui
  })


  output$about_discovery_fields <- shiny::renderUI({
    access_require("read")
    state$data_pond
    state$refresh_vocab

    about_fields_ready(FALSE)
    base_path <- state$data_pond
    keyword_choices <- function(field_code) {
      get_choices_for_pond(field_code = field_code, base_path = base_path)
    }
    section_ui <- tagList(
      generate_section(about_theme_dt, ns, keyword_choices),
      metadata_population_detail(metadata_optional_details(
        "Additional keywords",
        generate_section(about_extra_keywords_dt, ns, keyword_choices)
      ))
    )
    session$onFlushed(
      function() about_fields_ready(TRUE),
      once = TRUE
    )
    section_ui
  })


  output$about_optional_fields <- shiny::renderUI({
    access_require("read")
    state$data_pond
    state$refresh_vocab

    base_path <- state$data_pond
    generate_section(
      about_optional_dt,
      ns,
      function(field_code) {
        get_choices_for_pond(
          field_code = field_code,
          base_path = base_path
        )
      }
    )
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

  shiny::outputOptions(
    output,
    "about_discovery_fields",
    suspendWhenHidden = FALSE
  )

  shiny::outputOptions(
    output,
    "about_optional_fields",
    suspendWhenHidden = FALSE
  )


  # Apply queued field values only after both renderUI outputs
  # have reached the browser.
  shiny::observe({
    access_require("read")
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
    access_require("read")
    shiny::tags$div(
      class =
        "suggested-citation",
      `data-citation-author-style` =
        citation_author_style(),

      suggested_citation_text()
    )
  })

  current_dataset_parent_dir <- reactive({
    normalise_dataset_parent_dir(state$dataset_parent_dir)
  })

  mod_attribute_profile_config_server("attribute_profile_config", state)

  dataset_folder_selection_server(input, session, state, function(parent) {
    access_require("edit", touch = TRUE)
    sync_dataset_parent_dir(parent)
  })

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

  active_user_is_admin <- reactive({
    req(state$data_pond)
    is_user_admin(
      user_id = state$active_user_id,
      base_path = state$data_pond
    )
  })

  admin_tabs_visible <- shiny::reactiveVal(FALSE)
  admin_tabs_workspace <- shiny::reactiveVal(NULL)

  observe({
    access_require("read")
    # Each sign-in renders fresh workspace DOM, without the dynamic admin tabs.
    if (!identical(auth$workspace_nonce, shiny::isolate(admin_tabs_workspace()))) {
      admin_tabs_visible(FALSE)
      admin_tabs_workspace(auth$workspace_nonce)
    }
    should_show <- isTRUE(active_user_is_admin())
    if (should_show) shiny::showTab("main_dataset_tabs", "data_movement", session = session)
    else shiny::hideTab("main_dataset_tabs", "data_movement", session = session)
    currently_shown <- isTRUE(shiny::isolate(admin_tabs_visible()))
    if (identical(should_show, currently_shown)) return()

    if (should_show) {
      shiny::insertTab(
        inputId = "main_dataset_tabs",
        tab = dataset_admin_keywords_tab(),
        target = "dataset_status",
        position = "after",
        select = FALSE,
        session = session
      )
      shiny::insertTab(
        inputId = "main_dataset_tabs",
        tab = dataset_admin_parties_tab(),
        target = "dataset_status",
        position = "after",
        select = FALSE,
        session = session
      )
    } else {
      if (input$main_dataset_tabs %in% c("parties", "keyword_admin")) {
        shiny::updateTabsetPanel(
          session,
          "main_dataset_tabs",
          selected = "dataset_status"
        )
      }
      shiny::removeTab(
        inputId = "main_dataset_tabs",
        target = "keyword_admin",
        session = session
      )
      shiny::removeTab(
        inputId = "main_dataset_tabs",
        target = "parties",
        session = session
      )
    }

    admin_tabs_visible(should_show)
  })

  observe({
    access_require("read")
    locked <- !access_allowed("edit", state$data_pond)
    session$sendCustomMessage("datapond-active-user-write-lock", list(locked = locked))
  })

  output$active_user_write_status <- shiny::renderUI({
    access_require("read")
    if (access_allowed("edit", state$data_pond)) return(NULL)
    shiny::div(class = "datapond-write-lock-status",
      shiny::strong("Viewer access: "), "You can browse this pond. An administrator can grant editing access.")
  })
  output$access_manage_control <- shiny::renderUI({
    access_require("read")
    if (access_allowed("admin", state$data_pond)) shiny::actionButton("manage_access", "Manage accounts")
  })

  observeEvent(input$repair_dataset_location, {
    access_require("edit", touch = TRUE)
    dataset_id <- overview_record_id()
    req(dataset_id)
    if (!require_active_user_for_write(
      state,
      "repairing the dataset location"
    )) {
      return()
    }

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

    tryCatch(
      {
        report <- reconcile_dataset_locations(base_path, dataset_ids = dataset_id)
        result <- report[id == as.integer(dataset_id[1])]
        if (nrow(result) != 1L) {
          stop("No unambiguous folder candidate was found for this UUID.")
        }
        if (!result$location_status[1] %in% c(
          "ok",
          "repaired_within_lifecycle_root"
        )) {
          stop(
            "Location was not repaired: ",
            result$location_status[1],
            ". Cross-lifecycle moves and identity conflicts require manual review."
          )
        }

        record <- get_dataset_record(dataset_id, base_path)
        if (identical(as.integer(state$current_dataset_id), as.integer(dataset_id))) {
          state$dataset_path <- record$dataset_path[1]
          state$data_path <- record$data_path[1]
          state$current_dataset_folder <- record$dataset_path[1]
        }
        refresh_dataset_context(datasets = TRUE)

        showNotification(
          paste(
            if (result$location_status[1] == "ok") {
              "Dataset location is already correct:"
            } else {
              "Dataset location repaired within its lifecycle root:"
            },
            record$dataset_path[1]
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

  output$dataset_parent_dir_display <- renderText({
    access_require("read")
    req(state$data_pond)
    file.path(state$data_pond, current_dataset_parent_dir())
  })
  output$metadata_reviewer_display <- renderText({
    access_require("read")
    users <- active_users()

    if (is.null(users) || nrow(users) == 0) {
      return("No metadata reviewer selected.")
    }

    selected_reviewer_user_id <- state$active_user_id

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
      access_require("read")
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
    access_require("read")
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
      paste0("(", access_session(auth$pond, auth$token)$role[1], ")")
    )
  })
  observe({
    access_require("read")
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
    users <- users[as.character(users$id) == as.character(state$active_user_id), , drop = FALSE]
    choices <- setNames(as.character(users$id), paste(users$name_first, users$name_last))
    active_user <- state$active_user_id
    selected_reviewer <- default_metadata_reviewer_user_id(
      users,
      active_user
    )
    updateSelectInput(
      session,
      "metadata_reviewer_user",
      choices = c("-- Select reviewer / editor --" = "", choices),
      selected = selected_reviewer
    )
  })

  observeEvent(
    eventExpr = list(
      state$data_pond,
      state$refresh_vocab
    ),
    handlerExpr = {
      access_require("read", touch = TRUE)
      req(state$data_pond)

      fields <- c(
        "keyword_object",
        "keyword_discipline",
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
  # Use concrete roots because shinyFiles 0.9.x cannot safely navigate when
  # shinyDirChoose receives the dynamic closure returned by getVolumes().
  # Windows sessions include mapped/removable drives. Linux sessions include
  # readable standard mount locations in addition to the home-root browser.
  roots <- data_pond_folder_roots()
  shinyFiles::shinyDirChoose(
    access_directory_input(input),
    "data_pond_input",
    roots = roots,
    allowDirCreate = FALSE, session = session
  )
  shinyFiles::shinyDirChoose(
    access_directory_input(input),
    "connected_pond_input",
    roots = roots,
    allowDirCreate = FALSE, session = session
  )
  shinyFiles::shinyDirChoose(
    access_directory_input(input), "new_pond_parent_input", roots = roots, allowDirCreate = FALSE, session = session
  )

  observeEvent(input$new_pond, {
    access_require("admin", touch = TRUE)
    shiny::showModal(new_data_pond_dialog(dirname(state$data_pond)))
  })

  observeEvent(input$new_pond_parent_input, {
    access_require("edit", touch = TRUE)
    path <- shinyFiles::parseDirPath(roots, input$new_pond_parent_input)
    if (length(path) > 0L) {
      shiny::updateTextInput(session, "new_pond_parent", value = path[1])
    }
  })

  output$new_pond_destination <- shiny::renderText({
    access_require("read")
    tryCatch(
      paste("New pond folder:", new_data_pond_path(input$new_pond_parent, input$new_pond_name)),
      error = function(e) conditionMessage(e)
    )
  })

  observeEvent(input$create_pond, {
    access_require("admin", touch = TRUE)
    tryCatch({
      path <- create_data_pond_folder(input$new_pond_parent, input$new_pond_name)
      # The active-user prompt replaces the creation dialog after a successful switch.
      switch_data_pond(path, created = TRUE)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })

  pending_connected_pond_path <- shiny::reactiveVal(NULL)

  observeEvent(input$connected_pond_input, {
    access_require("edit", touch = TRUE)
    req(input$connected_pond_input)
    path <- shinyFiles::parseDirPath(roots, input$connected_pond_input)
    if (length(path) > 0L) pending_connected_pond_path(path[1])
  })

  output$connected_pond_path_preview <- renderText({
    access_require("read")
    pending_connected_pond_path() %||% "No additional pond selected."
  })

  observeEvent(input$add_connected_pond, {
    access_require("admin", touch = TRUE)
    path <- pending_connected_pond_path()
    if (is.null(path) || !nzchar(path)) {
      showNotification("Select an existing Data Pond folder first.", type = "warning")
      return()
    }
    tryCatch(
      {
        registry <- register_connected_pond(
          path = path,
          pond_name = input$connected_pond_name
        )
        connected_registry_tick(connected_registry_tick() + 1L)
        connected_inventory_refresh_tick(connected_inventory_refresh_tick() + 1L)
        updateTextInput(session, "connected_pond_name", value = "")
        showNotification(
          paste("Connected read-only catalogue:", registry$pond_name[registry$data_pond_path == normalizePath(path, winslash = "/")][1]),
          type = "message"
        )
      },
      error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)
    )
  })

  observe({
    access_require("read")
    registry <- connected_pond_registry()
    row <- registry[registry$data_pond_id == state$data_pond_id, , drop = FALSE]
    name <- if (nrow(row) == 0L) basename(state$data_pond) else row$pond_name[1]
    updateTextInput(session, "current_pond_nickname", value = name)
  })

  observeEvent(input$save_current_pond_nickname, {
    access_require("admin", touch = TRUE)
    tryCatch(
      {
        set_data_pond_label(
          state$data_pond,
          input$current_pond_nickname
        )
        rename_connected_pond(
          state$data_pond_id,
          input$current_pond_nickname
        )
        connected_registry_tick(connected_registry_tick() + 1L)
        showNotification("Current Data Pond name saved.", type = "message")
      },
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  connected_pond_registry_display <- shiny::reactive({
    access_require("read")
    req(length(state$data_pond_id) == 1L)
    connected_pond_poll_state()
    registry <- connected_pond_registry()
    if (nrow(registry) == 0L) return(data.frame())
    status <- vapply(
      registry$data_pond_path,
      function(path) {
        if (!dir.exists(path)) "Unavailable folder"
        else if (!file.exists(get_db_path(path))) "Missing metadata.db"
        else tryCatch({ connected_pond_require_read(path); "Available" }, error = function(e) "Sign-in required")
      },
      character(1)
    )
    data.frame(
      `Pond name` = registry$pond_name,
      Path = registry$data_pond_path,
      `Pond ID` = registry$data_pond_id,
      Status = status,
      Active = ifelse(registry$data_pond_id == state$data_pond_id, "Active", "Read-only"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  shiny::observe({
    access_require("read")
    display <- connected_pond_registry_display()
    if (nrow(display) == 0L) {
      shiny::updateSelectInput(
        session,
        "connected_pond_select",
        choices = c("No connected ponds" = ""),
        selected = ""
      )
      return()
    }

    labels <- paste(
      display[["Pond name"]],
      display$Active,
      display$Status,
      sep = " \u2014 "
    )
    choices <- c(
      "Select a connected pond" = "",
      stats::setNames(display[["Pond ID"]], make.unique(labels))
    )
    selected <- shiny::isolate(input$connected_pond_select %||% "")
    active_pond_id <- state$data_pond_id %||% ""
    if (!selected %in% display[["Pond ID"]]) {
      selected <- if (active_pond_id %in% display[["Pond ID"]]) {
        active_pond_id
      } else {
        ""
      }
    }
    shiny::updateSelectInput(
      session,
      "connected_pond_select",
      choices = choices,
      selected = selected
    )
  })

  selected_connected_pond <- shiny::reactive({
    selected <- input$connected_pond_select
    registry <- connected_pond_registry()
    if (is.null(selected) || length(selected) != 1L || !nzchar(selected)) {
      return(NULL)
    }
    match_row <- which(registry$data_pond_id == selected)
    if (length(match_row) != 1L) return(NULL)
    registry[match_row, , drop = FALSE]
  })

  observeEvent(input$connected_login, {
    access_require("read", touch = TRUE)
    pond <- selected_connected_pond()
    req(!is.null(pond))
    password <- input$connected_login_password
    updateTextInput(session, "connected_login_password", value = "")
    tryCatch({
      connected_pond_sign_in(pond$data_pond_path[1], input$connected_login_username, password)
      connected_inventory_refresh_tick(connected_inventory_refresh_tick() + 1L)
      showNotification("Signed in. Connected datasets are now available in Pond overview.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })

  observeEvent(input$switch_connected_pond, {
    access_require("read", touch = TRUE)
    pond <- selected_connected_pond()
    if (is.null(pond)) {
      showNotification("Select a connected pond first.", type = "warning")
      return()
    }
    tryCatch(
      switch_data_pond(pond$data_pond_path[1]),
      error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)
    )
  })

  observeEvent(input$remove_connected_pond, {
    access_require("admin", touch = TRUE)
    pond <- selected_connected_pond()
    if (is.null(pond)) {
      showNotification("Select a connected pond first.", type = "warning")
      return()
    }
    if (identical(pond$data_pond_id[1], state$data_pond_id)) {
      showNotification("The active pond cannot be disconnected. Switch ponds first.", type = "warning")
      return()
    }
    path <- normalizePath(pond$data_pond_path[1], mustWork = FALSE, winslash = "/")
    tokens <- auth$connected_tokens
    if (!is.null(tokens[[path]])) try(access_logout(path, tokens[[path]]), silent = TRUE)
    tokens[[path]] <- NULL
    auth$connected_tokens <- tokens
    remove_connected_pond(pond$data_pond_id[1])
    connected_registry_tick(connected_registry_tick() + 1L)
    connected_inventory_refresh_tick(connected_inventory_refresh_tick() + 1L)
    showNotification("Data Pond disconnected from this app catalogue.", type = "message")
  })
  observeEvent(input$set_manual, {
    access_require("read", touch = TRUE)
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
    access_require("read", touch = TRUE)
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
    access_require("read")
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
    access_require("read")
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
  toggle_config_panel <- function() {
    session$sendCustomMessage(
      type = "toggleConfig",
      message = list()
    )
  }

  observeEvent(input$toggle_config, {
    access_require("read", touch = TRUE)
    toggle_config_panel()
  })

  observeEvent(input$hide_config_panel, {
    access_require("read", touch = TRUE)
    toggle_config_panel()
  })

  toggle_dataset_guide <- function() {
    session$sendCustomMessage(
      type = "toggleDatasetGuide",
      message = list()
    )
  }

  show_dataset_guide <- function() {
    session$sendCustomMessage(
      type = "showDatasetGuide",
      message = list()
    )
  }

  observeEvent(input$toggle_dataset_guide, {
    access_require("read", touch = TRUE)
    toggle_dataset_guide()
  })

  observeEvent(input$hide_dataset_guide, {
    access_require("read", touch = TRUE)
    toggle_dataset_guide()
  })

  observeEvent(input$main_dataset_tabs, {
    access_require("read", touch = TRUE)
    if (identical(input$main_dataset_tabs, "dataset_workspace")) {
      show_dataset_guide()
    }
  }, ignoreInit = TRUE)

  shiny::observe({
    access_require("read")
    if (
      identical(input$main_dataset_tabs, "dataset_workspace") &&
        identical(input$dataset_edit_sections, "data_files") &&
        identical(
          input[["data_files-data_files_section"]],
          "attributes"
        )
    ) {
      session$sendCustomMessage(
        type = "hideDatapondSidePanels",
        message = list()
      )
    }
  })

  workflow_tab_targets <- c(
    guide_tab_dataset_intro = "dataset_intro",
    guide_tab_licensing = "licensing_access",
    guide_tab_data_files = "data_files",
    guide_tab_spatial = "spatial_metadata",
    guide_tab_supporting = "supporting_documents",
    guide_tab_lineage = "lineage",
    guide_tab_review = "metadata_review"
  )

  for (shortcut_id in names(workflow_tab_targets)) {
    local({
      id <- shortcut_id
      target <- unname(workflow_tab_targets[[id]])
      observeEvent(input[[id]], {
        access_require("read", touch = TRUE)
        shiny::updateTabsetPanel(
          session,
          "main_dataset_tabs",
          selected = "dataset_workspace"
        )
        shiny::updateTabsetPanel(
          session,
          "dataset_edit_sections",
          selected = target
        )
        show_dataset_guide()
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$guide_tab_status, {
    access_require("read", touch = TRUE)
    shiny::updateTabsetPanel(
      session,
      "main_dataset_tabs",
      selected = "dataset_status"
    )
  }, ignoreInit = TRUE)

  observe_dataset_guide_proxy <- function(
    proxy_id,
    target_id,
    selected_section = NULL
  ) {
    force(proxy_id)
    force(target_id)
    force(selected_section)
    observeEvent(input[[proxy_id]], {
      access_require("read", touch = TRUE)
      if (!is.null(selected_section)) {
        shiny::updateTabsetPanel(
          session,
          "main_dataset_tabs",
          selected = "dataset_workspace"
        )
        shiny::updateTabsetPanel(
          session,
          "dataset_edit_sections",
          selected = selected_section
        )
        show_dataset_guide()
      }
      session$sendCustomMessage(
        type = "datapond-click-input",
        message = list(id = target_id)
      )
    }, ignoreInit = TRUE)
  }

  observe_dataset_guide_proxy(
    "guide_start_new_dataset",
    "clear_dataset_form",
    selected_section = "dataset_intro"
  )
  observe_dataset_guide_proxy(
    "guide_create_dataset_folder",
    "new_dataset",
    selected_section = "dataset_intro"
  )
  observe_dataset_guide_proxy(
    "guide_save_dataset",
    "save"
  )
  observe_dataset_guide_proxy(
    "guide_open_dataset_folder",
    "open_dataset_folder"
  )

  output$current_pond_name <- renderText({
    access_require("read")
    registry <- connected_pond_registry()
    name <- registry$pond_name[match(state$data_pond_id, registry$data_pond_id)]
    if (!length(name) || is.na(name) || !nzchar(trimws(name))) basename(state$data_pond) else name
  })

  output$current_path <- renderText({
    access_require("read")
    state$data_pond
  })
  # Clear the current form and start a new record ----

  shiny::observeEvent(
    input$clear_dataset_form,
    {
      access_require("edit", touch = TRUE)
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
      iso_profile_mod$clear()
      iso_wave2_mod$clear()
      iso_wave3_mod$clear()
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
    access_require("read")
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
    access_require("read")
    folder_key_preview_text()
  })

  output$dataset_guide_folder <- shiny::renderUI({
    access_require("read")
    preview_key <- folder_key_preview_text()
    if (
      is.null(preview_key) || length(preview_key) == 0L ||
        is.na(preview_key[1]) ||
        startsWith(as.character(preview_key[1]), "Folder key preview unavailable:")
    ) {
      preview_key <- NULL
    }

    summary <- dataset_workflow_folder_summary(
      dataset_path = state$dataset_path,
      current_dataset_folder = state$current_dataset_folder,
      folder_key = preview_key,
      dataset_uuid = state$current_dataset_uuid
    )

    shiny::tags$div(
      class = "dataset-guide-folder-card",
      shiny::tags$span(
        class = "dataset-guide-folder-label",
        summary$label
      ),
      shiny::tags$strong(
        class = "dataset-guide-folder-name",
        summary$name
      ),
      shiny::tags$span(
        class = "dataset-guide-folder-detail",
        summary$detail
      ),
      shiny::tags$span(
        class = paste(
          "dataset-guide-folder-state",
          if (isTRUE(summary$created)) "is-created" else "is-proposed"
        ),
        if (isTRUE(summary$created)) "Folder created" else "Intro step"
      )
    )
  })

  output$dataset_guide_citation <- shiny::renderUI({
    access_require("read")
    citation <- suggested_citation_text()
    citation <- if (
      is.null(citation) || length(citation) == 0L || is.na(citation[1])
    ) {
      ""
    } else {
      trimws(as.character(citation[1]))
    }

    if (!nzchar(citation) || startsWith(citation, "Suggested citation unavailable:")) {
      return(
        shiny::tags$p(
          class = "dataset-guide-citation-empty",
          "Complete the citation fields in Dataset intro to build the citation."
        )
      )
    }

    shiny::tags$p(
      class = "dataset-guide-citation",
      citation
    )
  })
  observe({
    access_require("read")
    state$refresh_datasets
    req(state$data_pond)

    df <- get_datasets(
      state$data_pond,
      include_hidden = isTRUE(input$show_hidden_datasets),
      include_deleted = isTRUE(input$show_hidden_datasets)
    )

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


  overview_preview_id <- shiny::reactiveVal(NULL)
  overview_current_id <- shiny::reactive({
    if (isTRUE(pond_overview_selection_cleared())) return(NULL)
    if (!is.null(overview_selected_connection())) return(NULL)
    overview_preview_id() %||% state$current_dataset_id
  })
  overview_editor_ready <- shiny::reactive({
    id <- overview_current_id()
    length(id) == 1L && identical(as.integer(id), as.integer(state$current_dataset_id)) &&
      identical(as.integer(id), as.integer(last_hydrated_dataset_id()))
  })
  output$overview_editor_ready <- shiny::renderText({ access_require("read"); if (overview_editor_ready()) "true" else "false" })
  shiny::outputOptions(output, "overview_editor_ready", suspendWhenHidden = FALSE)

  hydrate_selected_dataset <- function(selected_value) {

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
        return(invisible(TRUE))
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

      data_access_mod$load(
        dataset_id
      )

      authors_mod$load(
        dataset_id
      )

      iso_profile_mod$load(
        dataset_id,
        rec
      )

      iso_wave2_mod$load(
        dataset_id
      )

      iso_wave3_mod$load(
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
      invisible(TRUE)
  }
  shiny::observeEvent(input$dataset_select, { access_require("read", touch = TRUE); hydrate_selected_dataset(input$dataset_select) }, ignoreInit = FALSE)

  open_overview_editor <- function() {
    id <- overview_current_id()
    if (length(id) != 1L || !id %in% pond_overview_all_data()$id) return(invisible(FALSE))
    if (!isTRUE(hydrate_selected_dataset(as.character(id)))) return(invisible(FALSE))
    shiny::updateSelectInput(session, "dataset_select", selected = as.character(id))
    invisible(TRUE)
  }
  shiny::observeEvent(input$overview_open_editor, {
    access_require("read", touch = TRUE)
    if (isTRUE(open_overview_editor()))
      shiny::updateTabsetPanel(session, "main_dataset_tabs", selected = "dataset_workspace")
    else showNotification("Select a current-pond dataset in the overview first.", type = "warning")
  })
  shiny::observeEvent(input$main_dataset_tabs, {
    access_require("read", touch = TRUE)
    if (input$main_dataset_tabs %in% c("dataset_workspace", "dataset_lifecycle")) open_overview_editor()
  }, ignoreInit = TRUE)

  mod_dataset_review_server("dataset_review", state, on_accepted = function() {
    last_hydrated_dataset_id(NULL)
    refresh_dataset_context()
  })

  # Portable dataset transfer ----
  last_created_bundle <- shiny::reactiveVal(NULL)

  output$dataset_bundle_export_context <- shiny::renderUI({
    access_require("read")
    dataset_id <- state$current_dataset_id
    if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) {
      return(shiny::tags$div(
        class = "alert alert-warning",
        "Select and save a dataset before creating a bundle."
      ))
    }

    record <- get_dataset_record(dataset_id, state$data_pond)
    if (is.null(record)) {
      return(shiny::tags$div(class = "alert alert-warning", "Dataset not found."))
    }

    shiny::tags$div(
      class = "well well-sm",
      shiny::tags$strong(transfer_scalar_text(record$title, "Untitled dataset")),
      shiny::tags$br(),
      shiny::tags$code(transfer_scalar_text(record$dataset_path, "No folder")),
      shiny::tags$br(),
      shiny::tags$small(paste("Dataset UUID:", transfer_scalar_text(record$uuid, "missing")))
    )
  })

  output$dataset_bundle_export_result <- shiny::renderUI({
    access_require("read")
    result <- last_created_bundle()
    if (is.null(result)) return(NULL)
    shiny::tags$div(
      class = "alert alert-success",
      style = "margin-top:12px; overflow-wrap:anywhere;",
      shiny::tags$strong("Bundle created in the dataset folder:"),
      shiny::tags$br(),
      shiny::tags$code(result$path)
    )
  })

  uploaded_bundle_manifest <- shiny::reactive({
    upload <- input$dataset_bundle_file
    if (is.null(upload) || nrow(upload) == 0L) return(NULL)
    tryCatch(
      list(ok = TRUE, manifest = read_dataset_transfer_manifest(upload$datapath[1])),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
  })

  output$dataset_bundle_preview <- shiny::renderUI({
    access_require("read")
    preview <- uploaded_bundle_manifest()
    if (is.null(preview)) return(NULL)
    if (!isTRUE(preview$ok)) {
      return(shiny::tags$div(
        class = "alert alert-danger",
        preview$error
      ))
    }

    manifest <- preview$manifest
    source_user <- transfer_scalar_text(
      manifest$source_exported_by_user_name,
      "Unknown user"
    )
    shiny::tags$div(
      class = "well well-sm",
      shiny::tags$strong(transfer_scalar_text(manifest$dataset_title, "Untitled dataset")),
      shiny::tags$dl(
        shiny::tags$dt("Source dataset UUID"),
        shiny::tags$dd(transfer_scalar_text(manifest$source_dataset_uuid, "Unknown")),
        shiny::tags$dt("Source Data Pond ID"),
        shiny::tags$dd(transfer_scalar_text(manifest$source_data_pond_id, "Unknown")),
        shiny::tags$dt("Source user"),
        shiny::tags$dd(paste0(
          source_user,
          " (source user ID ",
          transfer_scalar_integer(manifest$source_exported_by_user_id),
          ")"
        )),
        shiny::tags$dt("Original location"),
        shiny::tags$dd(paste0(
          transfer_scalar_text(manifest$source_parent_dir, "dev"),
          "/",
          if (nzchar(transfer_scalar_text(manifest$source_relative_parent, ""))) {
            paste0(transfer_scalar_text(manifest$source_relative_parent), "/")
          } else {
            ""
          },
          transfer_scalar_text(manifest$source_dataset_folder, "dataset")
        ))
      )
    )
  })

  output$dataset_import_provenance <- shiny::renderUI({
    access_require("read")
    dataset_id <- state$current_dataset_id
    if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) {
      return(shiny::tags$p("Select a dataset to view its transfer history."))
    }
    provenance <- get_dataset_import_provenance(dataset_id, state$data_pond)
    if (nrow(provenance) == 0L) {
      return(shiny::tags$p("This dataset was created in the active pond; no import record exists."))
    }
    shiny::tags$div(style = "overflow-x:auto;",
      datapond_summary_table(data.frame(
        `Source pond` = vapply(provenance$source_data_pond_id, transfer_scalar_text, character(1), "Unknown"),
        `Exported by` = vapply(provenance$source_exported_by_user_name, transfer_scalar_text, character(1), "Unknown"),
        Exported = vapply(provenance$exported_at, transfer_scalar_text, character(1), "Unknown"),
        Imported = vapply(provenance$imported_at, transfer_scalar_text, character(1), "Unknown"),
        `Bundle ID` = vapply(provenance$bundle_id, transfer_scalar_text, character(1), "Unknown"),
        check.names = FALSE
      )),
      shiny::helpText("Transfer history is carried forward in format 5 bundles. Numeric IDs are local to each source pond; dataset UUIDs retain identity.")
    )
  })

  shiny::observeEvent(input$create_dataset_bundle, {
    access_require("edit", touch = TRUE)
    if (!require_active_user_for_write(state, "compressing a dataset")) return()
    dataset_id <- state$current_dataset_id
    if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) {
      showNotification("Select and save a dataset first.", type = "warning")
      return()
    }

    tryCatch(
      {
        result <- create_dataset_transfer_bundle(
          dataset_id = as.integer(dataset_id[1]),
          base_path = state$data_pond,
          exporting_user_id = state$active_user_id
        )
        last_created_bundle(result)
        showNotification(
          paste("Dataset bundle created:", result$archive_name),
          type = "message",
          duration = 8
        )
      },
      error = function(e) showNotification(
        paste("Dataset compression failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    )
  })

  shiny::observeEvent(input$import_dataset_bundle, {
    access_require("edit", touch = TRUE)
    if (!require_active_user_for_write(state, "importing a dataset")) return()
    upload <- input$dataset_bundle_file
    if (is.null(upload) || nrow(upload) == 0L) {
      showNotification("Drop or choose a Data Pond ZIP first.", type = "warning")
      return()
    }
    preview <- uploaded_bundle_manifest()
    if (is.null(preview) || !isTRUE(preview$ok)) {
      showNotification(
        if (is.null(preview$error)) "The selected bundle is invalid." else preview$error,
        type = "error",
        duration = NULL
      )
      return()
    }

    tryCatch(
      {
        result <- import_dataset_transfer_bundle(
          path = upload$datapath[1],
          base_path = state$data_pond,
          importing_user_id = state$active_user_id
        )
        last_hydrated_dataset_id(NULL)
        state$current_dataset_id <- result$dataset_id
        state$current_dataset_uuid <- result$dataset_uuid
        state$current_dataset_folder <- result$dataset_path
        state$dataset_path <- result$dataset_path
        state$data_path <- file.path(result$dataset_path, "data")
        refresh_dataset_context()
        updateSelectInput(
          session,
          "dataset_select",
          selected = as.character(result$dataset_id)
        )
        try(write_pond_bibliography(state$data_pond), silent = TRUE)
        showNotification(
          paste(
            "Dataset imported as local ID", result$dataset_id,
            "from Data Pond", result$source_data_pond_id
          ),
          type = "message",
          duration = 10
        )
      },
      error = function(e) showNotification(
        paste("Dataset import failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    )
  })

  observe({
    access_require("read")
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

  dataset_save_request <- reactiveVal(0L)
  automatic_dataset_save <- reactiveVal(FALSE)

  request_dataset_save <- function(automatic = FALSE) {
    automatic_dataset_save(isTRUE(automatic))
    dataset_save_request(
      isolate(dataset_save_request()) + 1L
    )
    invisible(TRUE)
  }

  observeEvent(input$save, {
    access_require("edit", touch = TRUE)
    request_dataset_save(automatic = FALSE)
  }, ignoreInit = TRUE)

  validate_dataset_form <- function(values) {
    checks <- list(
      iso_profile_mod$validate(),
      validate_metadata_population(
        values, citation_dt, about_dt, profile = population_profile(),
        scope = iso_profile_mod$profile()$metadata_resource_scope_code,
        topics = iso_profile_mod$topic_categories(),
        organisations = org_mod$orgs()
      ),
      data_access_mod$validate(strict = FALSE),
      licensing_mod$validate(strict = FALSE),
      data_files_mod$creation_scale_validation()
    )
    for (check in checks) {
      if (!isTRUE(check$valid)) {
        showNotification(check$message, type = "error", duration = 8)
        return(FALSE)
      }
    }
    TRUE
  }

  observeEvent(input$new_dataset, {
    access_require("edit", touch = TRUE)
    if (!require_active_user_for_write(
      state,
      "creating a dataset folder"
    )) {
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
      )
    )

    vals <- as.list(vals)

    vals <- utils::modifyList(vals, iso_wave2_mod$compatibility_temporal_values())
    if (!validate_dataset_form(vals)) return()

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
    parent_dir <- sync_dataset_parent_dir()
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

    request_dataset_save(automatic = TRUE)
  })

  # save input ----
  # save dataset ----
  observeEvent(dataset_save_request(), {
    access_require("edit", touch = TRUE)
    automatic_save <- isolate(automatic_dataset_save())
    automatic_dataset_save(FALSE)

    if (!require_active_user_for_write(
      state,
      "saving the dataset"
    )) {
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
      data_files_mod$creation_scale_values(),
      iso_wave2_mod$compatibility_temporal_values()
    )

    vals <- as.list(vals)

    if (!validate_dataset_form(vals)) return()

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
    reviewer_user_input <- state$active_user_id
    if (is.null(reviewer_user_input) ||
        length(reviewer_user_input) == 0 ||
        is.na(reviewer_user_input[1]) ||
        !nzchar(trimws(as.character(reviewer_user_input[1])))
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

    if (isTRUE(is_new_dataset)) {
      vals$lifecycle_status <- "draft"
      vals$folder_locked <- 0L
    } else {
      lifecycle_record <- get_dataset_lifecycle(
        dataset_id =
          as.integer(state$current_dataset_id[1]),
        base_path =
          state$data_pond
      )

      vals$lifecycle_status <-
        lifecycle_record$lifecycle_status[1] %||%
          "draft"

      vals$folder_locked <-
        lifecycle_record$folder_locked[1] %||%
          0L
    }


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
      if (isTRUE(automatic_save)) {
        metadata_update_type_to_save <- c("new_metadata_record")
        metadata_section_to_save <- c(
          "dataset_overview",
          "citation",
          "about"
        )
        metadata_update_reason_to_save <-
          "Initial metadata record created with its dataset folder."
        metadata_update_summary_to_save <- paste0(
          "Dataset folder created and initial dataset record saved at: ",
          state$current_dataset_folder
        )
        metadata_status_to_save <- "draft"
      }

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

    selected_reviewer_user_id <- state$active_user_id

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
        vals[[n]] <- paste(normalise_keyword_values(val,
          get_keywords(canonical_keyword_field_code(n), state$data_pond)), collapse = "; ")
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
      vals$created_by_user_id <- state$active_user_id %||% NA_integer_
      vals$is_hidden <- 0L

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

        parent_dir <- sync_dataset_parent_dir()

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
    wave2_editor_person_id <- get_active_user_person_id(
      state$active_user_id,
      state$data_pond
    )
    metadata_responsibilities <- metadata_responsibilities_with_default(
      iso_profile_mod$responsibilities(),
      wave2_editor_person_id
    )
    save_iso_profile_sections(
      dataset_id = dataset_id,
      profile = iso_profile_mod$profile(),
      topic_categories = iso_profile_mod$topic_categories(),
      citation_dates = iso_profile_mod$citation_dates(),
      responsibilities = metadata_responsibilities,
      primary_publication_date = vals$citation_date,
      base_path = state$data_pond,
      user_person_id = wave2_editor_person_id
    )
    iso_profile_mod$mark_saved(dataset_id)
    save_iso_wave2_sections(
      dataset_id = dataset_id,
      alternate_titles = iso_wave2_mod$alternate_titles(),
      online_resources = iso_wave2_mod$online_resources(),
      distribution_formats = iso_wave2_mod$distribution_formats(),
      quality_results = iso_wave2_mod$quality_results(),
      temporal_extents = iso_wave2_mod$temporal_extents(),
      base_path = state$data_pond,
      user_person_id = wave2_editor_person_id
    )
    save_iso_wave3_sections(
      dataset_id = dataset_id,
      vertical_extents = iso_wave3_mod$vertical_extents(),
      feature_catalogue = iso_wave3_mod$feature_catalogue(),
      base_path = state$data_pond,
      user_person_id = wave2_editor_person_id
    )
    save_dataset_release_rights(
      dataset_id = dataset_id,
      data = data_access_mod$rights_values(),
      base_path = state$data_pond,
      user_person_id = wave2_editor_person_id
    )

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
        source_of_change = if (isTRUE(automatic_save)) {
          "dataset_folder_creation"
        } else {
          "manual_dataset_save"
        },
        app_version = as.character(utils::packageVersion("datapond")),
        notes = NA_character_
      )
    }
    # trigger refresh
    refresh_dataset_context()

    bibliography_result <- tryCatch(
      write_pond_bibliography(
        state$data_pond
      ),
      error = function(e) {
        showNotification(
          paste(
            "The dataset was saved, but datapond.bib",
            "could not be refreshed:",
            conditionMessage(e)
          ),
          type = "warning",
          duration = 8
        )

        NULL
      }
    )

    if (!is.null(bibliography_result)) {
      bibliography_refresh(
        bibliography_result
      )
    }

    showNotification(
      if (isTRUE(automatic_save)) {
        "Dataset folder created and dataset saved successfully"
      } else {
        "Saved successfully"
      },
      type = "message"
    )
    play_finfoot_cue(
      session,
      if (isTRUE(automatic_save)) "dataset-created" else "dataset-saved"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$overview_open_pond_folder, {
    access_require("edit", touch = TRUE)
    tryCatch({
      open_folder(state$data_pond)
      showNotification("Pond directory open request sent to the file manager.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
  })

  observeEvent(input$open_dataset_folder, {
    access_require("read", touch = TRUE)
    if (!access_allowed("edit")) {
      showNotification("Opening dataset folders requires editor access.", type = "warning")
      return()
    }
    tryCatch(
      {
        folder_path <- if (identical(input$main_dataset_tabs, "pond_overview")) {
          pond_overview_folder_path(
            pond_overview_data(), overview_current_id(),
            pond_overview_selection_cleared(), overview_selected_connection()
          )
        } else {
          state$dataset_path %||% state$current_dataset_folder
        }
        open_folder(folder_path)
        showNotification(
          "Dataset folder open request sent to the file manager.",
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
    access_require("edit", touch = TRUE)
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
        if (identical(input$main_dataset_tabs, "pond_overview")) {
          data_path <- file.path(pond_overview_folder_path(
            pond_overview_data(), overview_current_id(),
            pond_overview_selection_cleared(), overview_selected_connection()
          ), "data")
        }
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

  bibliography_refresh <- shiny::reactiveVal(
    NULL
  )

  selected_dataset_citation_record <-
    shiny::reactive({
      state$refresh_datasets
      bibliography_refresh()

      dataset_id <- overview_current_id()

      if (
        is.null(dataset_id) ||
        length(dataset_id) == 0L ||
        is.na(dataset_id[1])
      ) {
        return(NULL)
      }

      record <- get_dataset_record(
        dataset_id =
          as.integer(dataset_id[1]),
        base_path =
          state$data_pond
      )

      if (
        is.null(record) ||
        nrow(as.data.frame(record)) == 0L
      ) {
        return(NULL)
      }

      as.data.frame(
        record,
        stringsAsFactors = FALSE
      )[1, , drop = FALSE]
    })

  selected_dataset_biblatex <-
    shiny::reactive({
      record <-
        selected_dataset_citation_record()

      if (is.null(record)) {
        return(NULL)
      }

      tryCatch(
        dataset_biblatex_entry(
          dataset_id =
            as.integer(record$id[1]),
          base_path =
            state$data_pond
        ),
        error = function(e) {
          paste(
            "BibLaTeX preview unavailable:",
            conditionMessage(e)
          )
        }
      )
    })

  output$selected_dataset_citation <-
    shiny::renderUI({
      access_require("read")
      record <-
        selected_dataset_citation_record()

      if (is.null(record)) {
        return(
          shiny::tags$div(
            style = paste(
              "padding:9px;",
              "background:#F8F4EA;",
              "border:1px solid #ccc;",
              "color:#666;"
            ),
            if (!is.null(overview_selected_connection())) "Open the connected pond to view its full citation." else
              "Select a saved dataset to view its citation."
          )
        )
      }

      key <- biblatex_scalar(
        record$citation_key,
        default = "Citation key unavailable"
      )

      shiny::tags$div(
        style = paste(
          "padding:9px;",
          "background:#F1E9DA;",
          "border:1px solid #B99572;"
        ),

        shiny::tags$strong(
          key
        ),

        shiny::tags$div(
          style = paste(
            "margin-top:6px;",
            "font-family:monospace;",
            "font-size:12px;",
            "white-space:pre-wrap;",
            "overflow-wrap:anywhere;",
            "max-height:180px;",
            "overflow-y:auto;"
          ),
          selected_dataset_biblatex()
        )
      )
    })

  output$pond_bibliography_status <-
    shiny::renderUI({
      access_require("read")
      bibliography_refresh()
      state$refresh_datasets

      base_path <- state$data_pond

      if (
        is.null(base_path) ||
        length(base_path) == 0L ||
        is.na(base_path[1])
      ) {
        return(NULL)
      }

      path <- pond_bibliography_path(
        base_path
      )

      if (!file.exists(path)) {
        return(
          shiny::tags$small(
            style = paste(
              "display:block;",
              "margin-top:8px;",
              "color:#666;"
            ),
            paste0(
              "Not generated: ",
              path
            )
          )
        )
      }

      info <- file.info(
        path
      )

      shiny::tags$small(
        style = paste(
          "display:block;",
          "margin-top:8px;",
          "color:#526657;",
          "overflow-wrap:anywhere;"
        ),
        paste0(
          path,
          " \u2014 refreshed ",
          format(
            info$mtime[1],
            "%Y-%m-%d %H:%M:%S"
          )
        )
      )
    })

  copy_to_clipboard <- function(
    text,
    label
  ) {
    if (
      is.null(text) ||
      length(text) == 0L ||
      is.na(text[1]) ||
      !nzchar(as.character(text[1]))
    ) {
      showNotification(
        paste(
          label,
          "is unavailable."
        ),
        type = "warning"
      )

      return(
        invisible(FALSE)
      )
    }

    session$sendCustomMessage(
      type = "datapond-copy-text",
      message = list(
        text = as.character(text[1])
      )
    )

    showNotification(
      paste(
        label,
        "copied."
      ),
      type = "message",
      duration = 3
    )

    invisible(TRUE)
  }

  shiny::observeEvent(
    input$copy_citation_key,
    {
      access_require("read", touch = TRUE)
      record <-
        selected_dataset_citation_record()

      copy_to_clipboard(
        if (is.null(record)) {
          NULL
        } else {
          record$citation_key
        },
        "Citation key"
      )
    }
  )

  shiny::observeEvent(
    input$copy_quarto_citation,
    {
      access_require("read", touch = TRUE)
      record <-
        selected_dataset_citation_record()

      key <- if (is.null(record)) {
        ""
      } else {
        biblatex_scalar(
          record$citation_key,
          default = ""
        )
      }

      copy_to_clipboard(
        if (nzchar(key)) {
          paste0(
            "[@",
            key,
            "]"
          )
        } else {
          NULL
        },
        "Quarto citation"
      )
    }
  )

  shiny::observeEvent(
    input$copy_biblatex_entry,
    {
      access_require("read", touch = TRUE)
      copy_to_clipboard(
        selected_dataset_biblatex(),
        "BibLaTeX entry"
      )
    }
  )

  shiny::observeEvent(
    input$refresh_pond_bibliography,
    {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(
        state,
        "refreshing the pond bibliography"
      )) {
        return()
      }

      tryCatch(
        {
          result <- write_pond_bibliography(
            state$data_pond
          )

          bibliography_refresh(
            result
          )

          if (
            !is.null(state$current_dataset_id) &&
            length(state$current_dataset_id) > 0L &&
            !is.na(state$current_dataset_id[1])
          ) {
            refreshed_record <- get_dataset_record(
              dataset_id =
                as.integer(
                  state$current_dataset_id[1]
                ),
              base_path =
                state$data_pond
            )

            if (
              !is.null(refreshed_record) &&
              nrow(as.data.frame(refreshed_record)) > 0L
            ) {
              state$current_citation_key <-
                refreshed_record$citation_key[1]
            }
          }

          refresh_dataset_context(
            datasets = TRUE
          )

          showNotification(
            paste0(
              "Pond bibliography refreshed: ",
              result$entries,
              " dataset entr",
              if (result$entries == 1L) {
                "y"
              } else {
                "ies"
              },
              "."
            ),
            type = "message",
            duration = 5
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

  pond_overview_all_data <- shiny::reactive({
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

    rows <- build_pond_overview_data(
      base_path = base_path,
      schema_grouped = schema,
      include_hidden = isTRUE(input$show_hidden_datasets)
    )
    rows[, .search_text := pond_overview_search_index(rows)]
    rows
  })

  overview_search <- shiny::debounce(shiny::reactive(input$pond_dataset_search), 250)

  pond_overview_data <- shiny::reactive({
    filter_pond_overview_data(
      pond_overview_all_data(),
      overview_search()
    )
  })

  pond_overview_selection_cleared <- shiny::reactiveVal(FALSE)
  pond_overview_map_refresh <- shiny::reactiveVal(0L)

  shiny::observeEvent(input$refresh_connected_ponds, {
    access_require("read", touch = TRUE)
    cached_keys <- ls(envir = connected_inventory_cache, all.names = TRUE)
    if (length(cached_keys) > 0L) {
      rm(list = cached_keys, envir = connected_inventory_cache)
    }
    connected_inventory_refresh_tick(connected_inventory_refresh_tick() + 1L)
    showNotification("Connected pond catalogues refreshed.", type = "message")
  })

  federated_pond_catalogue <- shiny::reactive({
    access_require("read")
    req(length(state$data_pond_id) == 1L, !is.na(state$data_pond_id), nzchar(state$data_pond_id))
    connected_pond_poll_state()
    auth$connected_tokens
    connected_inventory_refresh_tick()
    state$refresh_datasets
    registry <- connected_pond_registry()
    if (!state$data_pond_id %in% registry$data_pond_id) {
      registry <- rbind(
        registry,
        data.frame(
          data_pond_id = state$data_pond_id,
          data_pond_path = state$data_pond,
          pond_name = basename(state$data_pond),
          connected_at = NA_character_,
          last_seen_at = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    }
    build_connected_pond_catalogue(
      registry = registry,
      cache = connected_inventory_cache,
      active_pond_path = state$data_pond,
      include_hidden = isTRUE(input$show_hidden_datasets)
    )
  })

  connected_pond_dataset_data <- shiny::reactive({
    catalogue <- federated_pond_catalogue()
    connected_pond_dataset_rows(catalogue)
  })

  # Remote metadata changes independently of the active pond. Cache this reactive
  # separately so editing the active pond never reopens every remote catalogue.
  overview_remote_inventory <- shiny::reactive({
    movement_state$cache_tick
    connected_inventory_refresh_tick()
    build_connected_overview_inventory(
      data.table::data.table(), movement_state$ponds,
      include_hidden = isTRUE(input$show_hidden_datasets)
    )
  })
  overview_connected_inventory <- shiny::reactive({
    local <- build_connected_overview_inventory(connected_pond_dataset_data(), list())
    remote <- overview_remote_inventory()
    list(data = data.table::rbindlist(list(local$data, remote$data), fill = TRUE, use.names = TRUE),
      unavailable = c(attr(federated_pond_catalogue(), "unavailable"), local$unavailable, remote$unavailable))
  })
  overview_inventory_all <- shiny::reactive({
    # Short circuit before touching connected databases when the user wants
    # current records only. Search never performs catalogue or network I/O.
    connected <- if (isTRUE(input$overview_current_only)) NULL else overview_connected_inventory()$data
    registry <- connected_pond_registry()
    name <- registry$pond_name[match(state$data_pond_id, registry$data_pond_id)]
    if (!length(name) || is.na(name)) name <- basename(state$data_pond)
    merge_pond_overview_inventory(pond_overview_all_data(), connected, name, state$data_pond)
  })
  overview_inventory_data <- shiny::reactive({
    filter_pond_overview_data(overview_inventory_all(), overview_search())
  })
  output$overview_connected_summary <- shiny::renderUI({
    access_require("read")
    rows <- overview_inventory_data()
    unavailable <- if (isTRUE(input$overview_current_only)) character() else overview_connected_inventory()$unavailable
    shiny::tagList(
      shiny::p(class = "help-block", paste(nrow(rows), "matching dataset(s):",
        sum(rows$active_pond %in% TRUE), "from the current pond and",
        sum(rows$active_pond %in% FALSE), "from connected ponds."),
        " Connected metadata is read locally; refresh remote metadata in Flow."),
      if (length(unavailable)) shiny::tags$div(class = "alert alert-warning",
        "Some connected catalogues are unavailable. Sign in to local ponds in Pond connections; refresh remote caches in Flow.",
        shiny::tags$ul(lapply(unavailable, shiny::tags$li)))
    )
  })
  overview_selected_connection <- shiny::reactiveVal(NULL)
  overview_selected_abstract <- shiny::reactive({
    selected <- overview_selected_connection()
    if (!is.null(selected)) return(tryCatch(
      list(title = selected$title, abstract = read_connected_overview_abstract(selected), error = NULL),
      error = function(e) list(title = selected$title, abstract = NULL, error = conditionMessage(e))
    ))
    selected_id <- overview_current_id()
    if (length(selected_id) != 1L) return(NULL)
    rows <- pond_overview_all_data()
    row <- rows[rows$id %in% selected_id, ]
    if (!nrow(row)) return(NULL)
    list(title = row$display_title[1], abstract = row$abstract[1], error = NULL)
  })
  output$overview_dataset_abstract <- shiny::renderUI({
    access_require("read")
    selected <- overview_selected_abstract()
    text <- selected$abstract
    if (!length(text) || is.na(text) || !nzchar(trimws(text))) text <- "No abstract recorded."
    shiny::div(class = "overview-abstract-card",
      shiny::h4("Selected dataset abstract"),
      if (is.null(selected)) shiny::p("Select a dataset in the inventory or map to preview its abstract.") else shiny::tagList(
        shiny::tags$strong(selected$title),
        shiny::p(style = "white-space:pre-wrap; overflow-wrap:anywhere; max-height:240px; overflow-y:auto; margin-top:6px;",
          selected$error %||% text)
      )
    )
  })
  output$overview_editor_action <- shiny::renderUI({
    access_require("read")
    if (is.null(overview_record_id())) return(NULL)
    shiny::tagList(
      shiny::p(class = "help-block", "Overview preview: saved metadata only. Forms load when you open the metadata editor or the dataset lifecycle tab."),
      shiny::actionButton("overview_open_editor", "Open selected dataset in metadata editor"),
      shiny::actionButton("repair_dataset_location", "Repair selected dataset location",
        class = "datapond-requires-active-user")
    )
  })
  output$overview_connection_details <- shiny::renderUI({
    access_require("read")
    selected <- overview_selected_connection()
    if (is.null(selected)) return(NULL)
    shiny::p(class = "help-block", paste(selected$title, "\u2014", selected$pond_name, "\u2014", selected$connection_type),
      shiny::tags$br(), paste("Files / cache:", selected$availability, "\u2014 Metadata:", selected$refreshed),
      shiny::tags$br(), "This connected record is read-only here. Open its pond to continue; the current pond's editor is unchanged.")
  })
  shiny::observeEvent(overview_inventory_data(), {
    access_require("read", touch = TRUE)
    rows <- overview_inventory_data()
    selected <- overview_selected_connection()
    if (!is.null(selected)) {
      match <- which(rows$record_key == selected$record_key)
      if (length(match) == 1L) {
        overview_selected_connection(as.list(rows[match]))
      } else {
        overview_selected_connection(NULL)
        overview_preview_id(NULL)
        pond_overview_selection_cleared(TRUE)
      }
    } else {
      id <- overview_preview_id() %||% state$current_dataset_id
      if (length(id) == 1L && !any(rows$active_pond %in% TRUE & rows$id %in% id)) {
        overview_preview_id(NULL)
        pond_overview_selection_cleared(TRUE)
      }
    }
  }, priority = 10)
  shiny::observeEvent(input$overview_open_connected, {
    access_require("read", touch = TRUE)
    selected <- overview_selected_connection()
    rows <- overview_inventory_data()
    if (is.null(selected)) {
      if (!is.null(overview_record_id()))
        showNotification("This dataset belongs to the active pond. Use Open pond directory to open its root folder, or Open selected dataset in metadata editor to edit it.", type = "message")
      else showNotification("Select a dataset row in the overview first.", type = "warning")
      return()
    }
    match <- which(rows$record_key == selected$record_key)
    if (isTRUE(pond_overview_selection_cleared()) || length(match) != 1L) {
      showNotification("The selected connected dataset is no longer available. Select a current row first.", type = "warning")
      return()
    }
    selected <- as.list(rows[match])
    if (identical(selected$connection_type, "Remote cache (read-only)")) {
      overview_remote_pond_request(list(cache = selected$open_path, request_id = new_uuid()))
      shiny::updateTabsetPanel(session, "main_dataset_tabs", selected = "data_movement")
    } else tryCatch(
      switch_data_pond(selected$open_path),
      error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)
    )
  })

  output$connected_pond_summary <- shiny::renderUI({
    access_require("read")
    registry <- connected_pond_registry()
    connected <- registry[registry$data_pond_id != state$data_pond_id, , drop = FALSE]
    catalogue <- connected_pond_dataset_data()
    available <- sum(vapply(
      connected$data_pond_path,
      function(path) tryCatch({ connected_pond_require_read(path); file.exists(get_db_path(path)) }, error = function(e) FALSE),
      logical(1)
    ))
    duplicates <- cross_pond_duplicate_datasets(federated_pond_catalogue())
    resolution_count <- if (nrow(duplicates) == 0L) 0L else
      sum(duplicates$needs_resolution %in% TRUE)
    shiny::tags$div(
      style = paste(
        "display:flex; flex-wrap:wrap; gap:8px; margin:9px 0;",
        "font-size:12px;"
      ),
      shiny::tags$span(
        class = "label label-default",
        paste(nrow(connected), "connected pond(s)")
      ),
      shiny::tags$span(
        class = "label label-success",
        paste(available, "available")
      ),
      shiny::tags$span(
        class = "label label-info",
        paste(nrow(catalogue), "read-only dataset(s)")
      ),
      if (resolution_count > 0L) {
        shiny::tags$span(
          class = "label label-warning",
          paste(resolution_count, "duplicate(s) need resolution")
        )
      }
    )
  })

  connected_dataset_display <- shiny::reactive({
    catalogue <- connected_pond_dataset_data()
    if (nrow(catalogue) == 0L) return(data.frame())
    data.frame(
      pond_id = catalogue$data_pond_id,
      pond_path = catalogue$data_pond_path,
      dataset_id = catalogue$id,
      `Pond` = catalogue$pond_name,
      `Location` = pond_stage_label(catalogue$stage),
      `Title` = ifelse(
        is.na(catalogue$title) | !nzchar(trimws(catalogue$title)),
        "Untitled dataset",
        catalogue$title
      ),
      `Version` = catalogue$version,
      `Citation key` = catalogue$citation_key,
      `Dataset UUID` = catalogue$uuid,
      `Dataset folder` = catalogue$dataset_path,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  output$connected_dataset_table <- DT::renderDT({
    access_require("read")
    if (!isTRUE(input$show_connected_ponds)) {
      return(clean_dt_widget(
        data.frame(message = "Connected pond datasets are hidden."),
        buttons = FALSE,
        filter = "none"
      ))
    }
    display <- connected_dataset_display()
    if (nrow(display) == 0L) {
      return(clean_dt_widget(
        data.frame(message = "No visible datasets are available from connected ponds."),
        buttons = FALSE,
        filter = "none"
      ))
    }
    datapond_datatable(
      display,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10L,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        columnDefs = list(list(visible = FALSE, targets = c(0L, 1L, 2L)))
      )
    )
  })

  selected_connected_dataset <- shiny::reactive({
    selected <- input$connected_dataset_table_rows_selected
    display <- connected_dataset_display()
    if (
      !isTRUE(input$show_connected_ponds) || is.null(selected) ||
        length(selected) != 1L || nrow(display) < selected[1]
    ) {
      return(NULL)
    }
    display[selected[1], , drop = FALSE]
  })

  observeEvent(input$switch_to_selected_connected_pond, {
    access_require("read", touch = TRUE)
    selected <- selected_connected_dataset()
    if (is.null(selected)) {
      showNotification("Select a connected dataset row first.", type = "warning")
      return()
    }
    tryCatch(
      switch_data_pond(selected$pond_path[1]),
      error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)
    )
  })

  cross_pond_duplicates <- shiny::reactive({
    cross_pond_duplicate_datasets(federated_pond_catalogue())
  })

  output$cross_pond_duplicate_alert <- shiny::renderUI({
    access_require("read")
    duplicates <- cross_pond_duplicates()
    if (nrow(duplicates) == 0L) {
      return(shiny::tags$div(
        class = "alert alert-success",
        style = "margin-top:10px;",
        "No cross-pond duplicate dataset identities were detected."
      ))
    }
    needs_resolution <- sum(duplicates$needs_resolution %in% TRUE)
    tolerated <- nrow(duplicates) - needs_resolution
    shiny::tags$div(
      class = if (needs_resolution > 0L) "alert alert-warning" else "alert alert-info",
      style = "margin-top:10px;",
      shiny::tags$strong(paste(needs_resolution, "duplicate group(s) need resolution.")),
      paste(
        tolerated,
        "development-only duplicate group(s) are tolerated.",
        "Duplicates are matched by dataset UUID, then citation or folder key when UUID is absent.",
        if (needs_resolution > 0L) {
          paste(
            "Use the connected catalogue to switch to each affected pond and",
            "decide which src/ or arc/ record is authoritative."
          )
        } else {
          ""
        }
      )
    )
  })

  output$cross_pond_duplicate_table <- DT::renderDT({
    access_require("read")
    duplicates <- cross_pond_duplicates()
    if (nrow(duplicates) == 0L) return(NULL)
    display <- data.frame(
      `Title` = duplicates$title,
      `Match basis` = duplicates$match_basis,
      `Dataset UUID` = duplicates$dataset_uuid,
      `Copies` = duplicates$copies,
      `Ponds and locations` = duplicates$ponds,
      `Status` = duplicates$status,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    widget <- datapond_datatable(
      display,
      rownames = FALSE,
      selection = "none",
      options = list(pageLength = 5L, dom = "tip", scrollX = TRUE)
    )
    DT::formatStyle(
      widget,
      columns = "Status",
      target = "row",
      backgroundColor = DT::styleEqual(
        c(
          "Resolve duplicate source/archive record",
          "Tolerated duplicate development copy"
        ),
        c("#E9DFB5", "#D1DDC7")
      )
    )
  })

  output$selected_dataset_location_details <- shiny::renderUI({
    access_require("read")
    overview <- pond_overview_all_data()
    selected_id <- overview_current_id()

    if (
      is.null(selected_id) ||
      length(selected_id) == 0L ||
      is.na(selected_id[1]) ||
      is.null(overview) ||
      nrow(as.data.frame(overview)) == 0L
    ) {
      return(
        shiny::tags$div(
          style = paste(
            "padding:7px 9px;",
            "background:#F8F4EA;",
            "border:1px solid #D3CEC4;",
            "color:#666;",
            "font-size:12px;"
          ),
          if (!is.null(overview_selected_connection())) "Open the connected pond for its location checks. Its saved abstract is shown below." else
            "Select a dataset to view its folder and location checks."
        )
      )
    }

    selected <- overview[
      as.integer(id) == as.integer(selected_id[1])
    ]

    if (nrow(selected) == 0L) {
      return(NULL)
    }

    selected <- selected[1]

    text_value <- function(field, default = "Not recorded") {
      if (
        !field %in% names(selected) ||
        is.null(selected[[field]]) ||
        length(selected[[field]]) == 0L ||
        is.na(selected[[field]][1]) ||
        !nzchar(trimws(as.character(selected[[field]][1])))
      ) {
        return(default)
      }

      as.character(selected[[field]][1])
    }

    location_labels <- c(
      ok = "Verified",
      repaired_within_lifecycle_root = "Verified; stored path repaired",
      missing = "Folder not found",
      missing_uuid = "Dataset UUID missing",
      duplicate_uuid = "Duplicate UUID",
      uuid_marker_conflict = "Folder/marker UUID conflict",
      data_pond_marker_conflict = "Data-pond marker conflict",
      stored_path_conflict = "Stored path conflict",
      lifecycle_location_conflict = "Lifecycle-root conflict",
      not_scanned = "Not checked"
    )

    location_code <- text_value(
      "location_status",
      default = "not_scanned"
    )
    location_check <- unname(
      location_labels[location_code]
    )
    if (length(location_check) == 0L || is.na(location_check)) {
      location_check <- location_code
    }

    compact_item <- function(label, value, wide = FALSE) {
      shiny::tags$div(
        style = paste(
          if (isTRUE(wide)) "grid-column:1 / -1;" else "",
          "min-width:0;",
          "padding:6px 8px;",
          "background:#F8F4EA;",
          "border:1px solid #D3CEC4;"
        ),
        shiny::tags$span(
          style = paste(
            "display:block;",
            "font-size:10px;",
            "font-weight:600;",
            "text-transform:uppercase;",
            "color:#625C52;"
          ),
          label
        ),
        shiny::tags$span(
          style = paste(
            "display:block;",
            "font-size:12px;",
            "line-height:1.25;",
            "overflow-wrap:anywhere;"
          ),
          value
        )
      )
    }

    shiny::tags$div(
      style = paste(
        "display:grid;",
        "grid-template-columns:repeat(3, minmax(0, 1fr));",
        "gap:5px;"
      ),
      compact_item(
        "Dataset folder",
        text_value("dataset_path"),
        wide = TRUE
      ),
      compact_item(
        "Pond location",
        text_value("pond_location")
      ),
      compact_item(
        "Record visibility",
        text_value("record_visibility")
      ),
      compact_item(
        "Location check",
        location_check
      ),
      compact_item(
        "Record owner",
        text_value("record_owner")
      ),
      compact_item(
        "Contacts",
        text_value("poc"),
        wide = TRUE
      )
    )
  })

  overview_record_id <- shiny::reactive({
    if (isTRUE(pond_overview_selection_cleared())) return(NULL)
    id <- overview_current_id()
    if (length(id) != 1L || is.na(id) || !id %in% pond_overview_all_data()$id) return(NULL)
    as.integer(id)
  })
  pending_record_delete <- shiny::reactiveVal(NULL)

  lifecycle_record_id <- shiny::reactive({
    state$refresh_datasets
    if (!identical(input$main_dataset_tabs, "dataset_lifecycle")) return(NULL)
    id <- state$current_dataset_id
    if (length(id) != 1L || is.na(id)) return(NULL)
    record <- get_dataset_record(id, state$data_pond)
    if (is.null(record)) return(NULL)
    as.integer(id)
  })

  output$dataset_record_controls <- shiny::renderUI({
    access_require("read")
    dataset_id <- overview_record_id()
    if (is.null(dataset_id)) return(NULL)
    permissions <- dataset_record_permissions(
      dataset_id,
      state$active_user_id,
      state$data_pond
    )
    if (!isTRUE(permissions$can_manage)) {
      return(shiny::tags$small(
        style = "display:block; color:#666; margin-top:8px;",
        paste(
          "Only the metadata-record owner or an administrator can hide this record.",
          "Deletion and restoration are available in Dataset lifecycle."
        )
      ))
    }

    shiny::tags$div(
      style = "margin-top:8px; display:flex; flex-wrap:wrap; gap:6px;",
      if (!isTRUE(permissions$is_deleted)) {
        shiny::actionButton(
          "toggle_dataset_hidden",
          if (isTRUE(permissions$is_hidden)) "Unhide record" else "Hide record",
          class = "datapond-requires-active-user"
        )
      },
      shiny::tags$small(class = "help-block",
        "Deletion and restoration are available in Dataset lifecycle.")
    )
  })

  output$lifecycle_record_controls <- shiny::renderUI({
    access_require("read")
    dataset_id <- lifecycle_record_id()
    if (is.null(dataset_id)) return(NULL)
    permissions <- dataset_record_permissions(dataset_id, state$active_user_id, state$data_pond)
    record <- get_dataset_record(dataset_id, state$data_pond)
    shiny::tags$section(class = "lifecycle-record-controls",
      shiny::h3("Delete or restore the selected record"),
      shiny::p(shiny::strong(transfer_scalar_text(record$title, "Untitled dataset")),
        paste0(" (ID ", dataset_id, ")")),
      shiny::p("Record actions do not remove or move the physical dataset folder."),
      if (!isTRUE(permissions$can_manage)) shiny::p(
        "Only the metadata-record owner or an administrator can delete or restore this record."
      ) else shiny::tags$div(class = "lifecycle-record-actions",
        if (isTRUE(permissions$is_deleted)) {
          shiny::actionButton(
            "restore_dataset_record",
            "Restore record",
            class = "datapond-requires-active-user"
          )
        } else {
          shiny::actionButton(
            "soft_delete_dataset",
            "Delete record (recoverable)",
            class = "datapond-requires-active-user"
          )
        },
        if (isTRUE(permissions$is_admin)) {
          shiny::actionButton(
            "hard_delete_dataset",
            "Hard-delete record",
            class = "datapond-requires-active-user"
          )
        }
      )
    )
  })

  refresh_after_record_control <- function() {
    refresh_dataset_context(datasets = TRUE, reviews = TRUE)
    invisible(TRUE)
  }

  shiny::observeEvent(input$toggle_dataset_hidden, {
    access_require("edit", touch = TRUE)
    dataset_id <- overview_record_id()
    req(dataset_id)
    if (!require_active_user_for_write(
      state,
      "changing dataset-record visibility"
    )) {
      return()
    }

    permissions <- dataset_record_permissions(
      dataset_id,
      state$active_user_id,
      state$data_pond
    )
    tryCatch(
      {
        set_dataset_hidden(
          dataset_id,
          !isTRUE(permissions$is_hidden),
          state$active_user_id,
          state$data_pond
        )
        refresh_after_record_control()
      },
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  shiny::observeEvent(input$soft_delete_dataset, {
    access_require("edit", touch = TRUE)
    dataset_id <- lifecycle_record_id()
    req(dataset_id)
    if (!require_active_user_for_write(
      state,
      "deleting the dataset record"
    )) {
      return()
    }

    tryCatch(
      {
        soft_delete_dataset_record(
          dataset_id,
          state$active_user_id,
          state$data_pond
        )
        refresh_after_record_control()
        showNotification(
          "Dataset record deleted recoverably; the physical folder was untouched.",
          type = "message"
        )
      },
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  shiny::observeEvent(input$restore_dataset_record, {
    access_require("edit", touch = TRUE)
    dataset_id <- lifecycle_record_id()
    req(dataset_id)
    if (!require_active_user_for_write(
      state,
      "restoring the dataset record"
    )) {
      return()
    }

    tryCatch(
      {
        restore_dataset_record(
          dataset_id,
          state$active_user_id,
          state$data_pond
        )
        refresh_after_record_control()
      },
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  shiny::observeEvent(input$hard_delete_dataset, {
    access_require("admin", touch = TRUE)
    dataset_id <- lifecycle_record_id()
    req(dataset_id)
    if (!require_active_user_for_write(
      state,
      "hard-deleting the dataset record"
    )) {
      return()
    }

    record <- get_dataset_record(dataset_id, state$data_pond)
    req(record)
    pending_record_delete(list(id = dataset_id, uuid = record$uuid,
      pond = state$data_pond, workspace = auth$workspace_nonce))
    shiny::showModal(shiny::modalDialog(
      title = "Hard-delete metadata record?",
      shiny::p(shiny::strong(transfer_scalar_text(record$title, "Untitled dataset")),
        paste0(" (ID ", dataset_id, ")")),
      paste(
        "This permanently removes the metadata record and linked catalogue rows.",
        "The physical dataset folder and files will remain untouched."
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "confirm_hard_delete_dataset",
          "Hard-delete record",
          class = "datapond-requires-active-user"
        )
      )
    ))
  })

  shiny::observeEvent(input$confirm_hard_delete_dataset, {
    access_require("admin", touch = TRUE)
    target <- pending_record_delete()
    req(target)
    record <- get_dataset_record(target$id, state$data_pond)
    if (!identical(target$pond, state$data_pond) ||
        !identical(target$workspace, auth$workspace_nonce) ||
        !identical(target$id, lifecycle_record_id()) || is.null(record) ||
        !identical(target$uuid, record$uuid)) {
      pending_record_delete(NULL)
      shiny::removeModal()
      showNotification("The selected dataset or pond changed. Select the record and confirm deletion again.", type = "warning")
      return()
    }
    if (!require_active_user_for_write(
      state,
      "hard-deleting the dataset record"
    )) {
      return()
    }

    dataset_id <- target$id
    tryCatch(
      {
        hard_delete_dataset_record(
          dataset_id,
          state$active_user_id,
          state$data_pond
        )
        shiny::removeModal()
        pending_record_delete(NULL)
        if (identical(as.integer(state$current_dataset_id), dataset_id)) {
          state$current_dataset_id <- NULL
          state$current_dataset_uuid <- NULL
        }
        overview_preview_id(NULL)
        pond_overview_selection_cleared(TRUE)
        refresh_after_record_control()
        showNotification(
          "Metadata record permanently deleted; the physical folder was untouched.",
          type = "message"
        )
      },
      error = function(e) showNotification(conditionMessage(e), type = "error")
    )
  })

  output$dataset_table <- DT::renderDT({
    access_require("read")
    overview <- overview_inventory_data()

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

    display <- pond_overview_inventory_display(
      overview
    )

    selected_rows <- shiny::isolate({
      if (isTRUE(pond_overview_selection_cleared())) integer() else {
        connected <- overview_selected_connection()
        if (!is.null(connected)) which(overview$record_key == connected$record_key) else
          which(overview$active_pond %in% TRUE & overview$id %in% overview_current_id())
      }
    })

    table_widget <- datapond_datatable(
      as.data.frame(
        display,
        stringsAsFactors = FALSE
      ),
      rownames = FALSE,
      selection = list(mode = "single", selected = selected_rows),
      filter = "top",
      escape = TRUE,
      extensions = pond_overview_table_extensions(),
      options = pond_overview_responsive_table_options()
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
        "Organisations"
      ),
      whiteSpace = "normal",
      wordBreak = "normal",
      lineHeight = "1.35"
    )

    table_widget <- DT::formatStyle(
      table_widget,
      columns = "Citation date",
      whiteSpace = "nowrap"
    )

    table_widget
  }, server = TRUE)

  dataset_table_proxy <- DT::dataTableProxy("dataset_table")

  select_overview_table_rows <- function(rows) {
    rows <- as.integer(rows)
    current <- shiny::isolate(input$dataset_table_rows_selected)
    current <- as.integer(current %||% integer())

    # DT reports proxy selections through the same input used for user row
    # clicks. Do not send an already-applied selection back to the browser:
    # that echo can make the editor and overview selections drive each other.
    if (identical(current, rows)) return(invisible(FALSE))

    DT::selectRows(dataset_table_proxy, rows)
    invisible(TRUE)
  }


  shiny::observeEvent(input$refresh_pond_overview, {
    access_require("read", touch = TRUE)
    overview_selected_connection(NULL)
    overview_preview_id(NULL)
    pond_overview_selection_cleared(TRUE)
    state$refresh_datasets <- state$refresh_datasets + 1L
    connected_inventory_refresh_tick(connected_inventory_refresh_tick() + 1L)
    cached_keys <- ls(envir = connected_inventory_cache, all.names = TRUE)
    if (length(cached_keys)) rm(list = cached_keys, envir = connected_inventory_cache)
    select_overview_table_rows(integer())
    pond_overview_map_refresh(
      pond_overview_map_refresh() + 1L
    )
  }, ignoreInit = TRUE)


  shiny::observeEvent(state$current_dataset_id, {
    access_require("read", touch = TRUE)

    # While Pond overview is visible its preview selection is authoritative.
    # A delayed editor/dropdown update must not replace a row the user has just
    # chosen there; opening an editor already hydrates the preview explicitly.
    if (identical(input$main_dataset_tabs, "pond_overview")) return()

    overview_selected_connection(NULL)
    overview_preview_id(state$current_dataset_id)
    pond_overview_selection_cleared(FALSE)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)


  shiny::observeEvent(
    input$dataset_table_rows_selected,
    {
      access_require("read", touch = TRUE)
      selected_row <-
        input$dataset_table_rows_selected

      if (
        is.null(selected_row) ||
        length(selected_row) != 1L || is.na(selected_row) || selected_row < 1L
      ) {
        # DT can report an empty selection while replacing its rows. Keep the
        # selection by identity; Refresh or filtering it out clears it explicitly.
        return()
      }

      overview <- overview_inventory_data()

      if (
        is.null(overview) ||
        nrow(as.data.frame(overview)) <
          selected_row
      ) {
        return()
      }

      pond_overview_selection_cleared(FALSE)
      selected <- overview[selected_row]
      if (!isTRUE(selected$active_pond)) {
        overview_selected_connection(as.list(selected))
        return()
      }
      overview_selected_connection(NULL)
      selected_dataset_id <- selected$id

      if (
        is.na(selected_dataset_id)
      ) {
        return()
      }

      overview_preview_id(selected_dataset_id)
    }
  )

  shiny::observe({
    access_require("read")
    overview <- overview_inventory_data()
    connected <- overview_selected_connection()
    selected_id <- overview_current_id()
    selection_cleared <- pond_overview_selection_cleared()

    if (!isTRUE(selection_cleared) && !is.null(connected)) {
      selected_row <- which(overview$record_key == connected$record_key)
      select_overview_table_rows(if (length(selected_row) == 1L) selected_row else integer())
      return()
    }

    if (
      isTRUE(selection_cleared) ||
      is.null(selected_id) ||
      length(selected_id) == 0L ||
      is.na(selected_id[1]) ||
      is.null(overview) ||
      nrow(as.data.frame(overview)) == 0L
    ) {
      select_overview_table_rows(integer())

      return()
    }

    selected_row <- which(
      overview$active_pond %in% TRUE & as.integer(overview$id) ==
        as.integer(selected_id[1])
    )

    if (length(selected_row) == 0L) {
      select_overview_table_rows(integer())
    } else {
      select_overview_table_rows(selected_row[1])
    }
  })

  output$pond_overview_summary <- shiny::renderUI({
    access_require("read")
    overview <- pond_overview_data()
    all_overview <- pond_overview_all_data()

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
          if (
            !is.null(all_overview) &&
              nrow(as.data.frame(all_overview)) > 0L
          ) {
            "No datasets match the current pond search."
          } else {
            "No visible datasets are recorded in the active pond."
          }
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
        "Datasets shown",
        paste0(nrow(overview), " of ", nrow(all_overview)),
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
      ),

      stat_row(
        "Metadata needing work",
        sum(
          !overview$ready_for_archive %in% TRUE,
          na.rm = TRUE
        ),
        "#E9DFB5"
      ),

      stat_row(
        "Location conflicts",
        sum(
          overview$lifecycle_stage_mismatch %in% TRUE,
          na.rm = TRUE
        ),
        "#C98F78"
      ),

      if (isTRUE(input$show_hidden_datasets)) {
        stat_row(
          "Hidden / deleted records",
          sum(
            overview$record_visibility != "Visible",
            na.rm = TRUE
          ),
          "#D3CEC4"
        )
      }
    )
  })

  output$pond_dataset_map <-
    leaflet::renderLeaflet({
      access_require("read")
      pond_overview_map_refresh()

      overview <- pond_overview_data()
      show_points <- !identical(input$pond_map_display, "extents")

      map <- leaflet::leaflet(
        options =
          leaflet::leafletOptions(
            zoomControl = TRUE,
            attributionControl = TRUE
          )
      )

      map <- add_datapond_basemap(map)

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
      # Bounding-box size
      # ------------------------------------------------------------

      mapped[
        ,
        bbox_area_proxy :=
          (east - west) *
          (north - south)
      ]

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
      # Add interactive location points or bounding-box rectangles
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

        if (show_points) {
          map <- leaflet::addCircleMarkers(
            map,
            lng = (mapped$west[i] + mapped$east[i]) / 2,
            lat = (mapped$south[i] + mapped$north[i]) / 2,
            layerId = as.character(mapped$id[i]),
            radius = 6,
            color = "#625C52",
            weight = 1,
            opacity = 1,
            fillColor = fill_colour,
            fillOpacity = 0.9,
            clusterOptions = leaflet::markerClusterOptions(
              showCoverageOnHover = FALSE,
              spiderfyOnMaxZoom = TRUE
            ),
            clusterId = "pond_dataset_locations",
            popup = popup,
            label = mapped$display_title[i]
          )
        } else {
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
      }

      # ------------------------------------------------------------
      # Add the selected dataset as a non-interactive outline
      # ------------------------------------------------------------

      map <- pond_overview_selection_outline(map, mapped, shiny::isolate(
        if (isTRUE(pond_overview_selection_cleared())) NULL else overview_current_id()
      ))

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

  shiny::observe({
    access_require("read")
    rows <- pond_overview_data()
    pond_overview_map_refresh()
    input$pond_map_display
    selected_id <- if (isTRUE(pond_overview_selection_cleared())) NULL else overview_current_id()
    session$onFlushed(function() {
      map <- leaflet::clearGroup(leaflet::leafletProxy("pond_dataset_map", session = session), "overview_selection")
      pond_overview_selection_outline(map, rows, selected_id)
    }, once = TRUE)
  })

  select_pond_map_dataset <- function(click) {
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

    if (!selected_dataset_id %in% pond_overview_data()$id) return()
    pond_overview_selection_cleared(FALSE)
    overview_selected_connection(NULL)
    overview_preview_id(selected_dataset_id)
  }

  shiny::observeEvent(
    input$pond_dataset_map_shape_click,
    { access_require("read", touch = TRUE); select_pond_map_dataset(input$pond_dataset_map_shape_click) }
  )

  shiny::observeEvent(
    input$pond_dataset_map_marker_click,
    { access_require("read", touch = TRUE); select_pond_map_dataset(input$pond_dataset_map_marker_click) }
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
    access_require("read")
    display_df <- metadata_reviews_display()

    if (
      is.null(display_df) ||
      nrow(as.data.frame(display_df)) == 0L
    ) {
      return(
        datapond_datatable(
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
        datapond_datatable(
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

    table_widget <- datapond_datatable(
      as.data.frame(display_df, stringsAsFactors = FALSE),
      rownames = FALSE, selection = "none", filter = "top",
      escape = TRUE, extensions = c("Buttons", "ColReorder"),
      class = "compact stripe hover",
      table_tools = list(history = TRUE),
      options = list(
        pageLength = 10L,
        scrollX = TRUE,
        scrollY = "360px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        dom = "Bfrtip",
        colReorder = list(fixedColumnsLeft = 2L),
        order = list(list(2L, "desc")),
        columnDefs = c(
          list(
            list(visible = FALSE, targets = c(0L, 1L)),
            list(className = "metadata-history-status", targets = 7L),
            list(targets = 2:11, className = "metadata-history-public", render = DT::JS(
              "function(data, type, row, meta) {",
              "  if (type !== 'display') return data;",
              "  return window.datapondTableTools.cell(data, meta);",
              "}"
            ))
          ),
          lapply(2:11, function(index) list(
            targets = index, className = paste0("metadata-history-width-", index)
          ))
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

    table_widget
  })
  invisible(state)
}

datapond_app <- function() {
  shiny::addResourcePath(
    "datapond-assets",
    system.file("www", package = "datapond", mustWork = TRUE)
  )
  shiny::addResourcePath(
    "datapond-guide",
    system.file("guide", package = "datapond", mustWork = TRUE)
  )
  current_limit <- getOption("shiny.maxRequestSize", 0)
  transfer_limit <- getOption("datapond.max_bundle_size", 2 * 1024^3)
  options(shiny.maxRequestSize = max(current_limit, transfer_limit))
  shinyApp(
    ui = access_ui(),
    server = server
  )
}
