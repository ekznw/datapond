flow_requirement_marker <- function(requirement) {
  metadata_requirement_marker(requirement)
}

mod_data_movement_ui <- function(id) {
  ns <- shiny::NS(id)
  field_label <- function(label, help, requirement = "Optional", when = NULL) {
    shiny::tagList(label, " ",
      flow_requirement_marker(requirement),
      shiny::tags$small(class = "flow-field-help", if (!is.null(when)) paste0(when, " "), help))
  }
  shiny::div(class = "datapond-flow",
    shiny::tags$style(shiny::HTML("
      .datapond-flow .flow-navigation { display:grid; grid-template-columns:repeat(auto-fit, minmax(180px, 1fr)); gap:10px; padding:0; list-style:none; margin:12px 0; }
      .datapond-flow .flow-navigation li { border:1px solid var(--dp-box-border, #D3CEC4); border-radius:6px; padding:12px; background:var(--dp-surface, #F8F6F0); color:var(--dp-text, #302F2C); }
      .datapond-flow .flow-navigation li.flow-setup { background:var(--dp-inset, #F1E9DA); }
      .datapond-flow .flow-navigation p { margin:6px 0 0; font-size:13px; }
      .datapond-flow > .tabbable > .nav-tabs > li > a { min-height:44px; padding:5px 12px; line-height:16px; display:flex; align-items:center; justify-content:center; text-align:center; }
      .datapond-flow .flow-progress-card { background:var(--dp-surface, #F8F6F0); color:var(--dp-text, #302F2C); border:1px solid var(--dp-box-border, #D3CEC4); padding:12px; margin:10px 0; }
      .datapond-flow .flow-progress-card h4 { margin:0 0 8px; }
      .datapond-flow .flow-progress-metrics { display:flex; flex-wrap:wrap; gap:12px 24px; margin:10px 0; }
      .datapond-flow .flow-progress-metric small, .datapond-flow .flow-progress-metric strong { display:block; }
      .datapond-flow .flow-progress-paths, .datapond-flow .flow-progress-card li { overflow-wrap:anywhere; }
      .datapond-flow .flow-progress-card .progress { margin:10px 0; }
      .datapond-flow .flow-progress-card .progress-bar { background:#526657; }
      .datapond-flow .flow-terminal { background:#F8F6F0; color:#302F2C; border:1px solid #D3CEC4; font-size:12px; line-height:1.5; max-height:320px; overflow:auto; white-space:pre-wrap; overflow-wrap:anywhere; }
      .datapond-flow .flow-field-requirement {
        display: inline-block;
        font-size: 14px;
        font-weight: 600;
        line-height: 1;
        vertical-align: baseline;
      }
      .datapond-flow .flow-field-requirement.mandatory {
        color: #7A2525;
      }
      .datapond-flow .flow-field-requirement.conditional {
        color: #65512E;
      }
      .datapond-flow .flow-field-help {
        display: block;
        color: #6B6B6B;
        font-size: 12px;
        font-weight: normal;
        line-height: 1.4;
        margin-top: 3px;
      }
      .datapond-flow > .tabbable > .nav-tabs > li > a:is([data-value='config'], [data-value='design']) {
        color: #65512E !important;
        background-color: #F1E9DA;
        border-color: #C6B797;
      }
      .datapond-flow > .tabbable > .nav-tabs > li > a:is([data-value='config'], [data-value='design']):hover,
      .datapond-flow > .tabbable > .nav-tabs > li > a:is([data-value='config'], [data-value='design']):focus {
        color: #4C3A1E !important;
        background-color: #E9DFB5;
        border-color: #8A7045;
      }
      .datapond-flow > .tabbable > .nav-tabs > li.active > a:is([data-value='config'], [data-value='design']) {
        color: #FFFFFF !important;
        background-color: #79613C !important;
        border-color: #79613C !important;
      }
    ")),
    shiny::h3("Flow"),
    shiny::p(class = "flow-field-help", "Flow uses rclone to copy and sync local and cloud files. It is installed separately from DataPond; this check confirms it is ready."),
    shiny::uiOutput(ns("rclone_status")),
    shiny::actionButton(ns("check_rclone"), "Check Flow (sync) engine", class = "btn-sm"),
    shiny::p(class = "help-block", "Choose a tab for the task you need. The cards follow the tab order."),
    shiny::tags$ol(class = "flow-navigation", `aria-label` = "Flow tabs",
      shiny::tags$li(shiny::strong("1. Read remote pond"), shiny::p("Browse a published pond, refresh its cached metadata and download selected datasets. The remote pond stays read-only.")),
      shiny::tags$li(shiny::strong("2. Run Flows"), shiny::p("Preview and run saved flows, or publish the active pond. Publication settings are saved for each pond for later retries.")),
      shiny::tags$li(class = "flow-setup", shiny::strong("3. Define connections"), shiny::p("Set up remote connections, name local folders and choose transfer limits. Hide this tab once setup is complete.")),
      shiny::tags$li(class = "flow-setup", shiny::strong("4. Design flow"), shiny::p("Choose the source, destination and copy or mirror operation, then save a reusable flow."))
    ),
    shiny::p(class = "help-block", "Connections and flow design are setup tasks; complete them before running a new routine. Pond publication uses its own settings in Run Flows."),
    shiny::p(class = "flow-field-help",
      shiny::tags$span(class = "flow-field-requirement mandatory", `aria-hidden` = "true", "*"),
      " Mandatory for the action. ",
      shiny::tags$span(class = "flow-field-requirement conditional", `aria-hidden` = "true", "\u25c7"),
      " Conditional: see the field guidance. Unmarked fields are optional."),
    shiny::checkboxInput(ns("show_config"), "Show Define connections tab", TRUE),
    shiny::p(class = "flow-field-help", "Cloud services such as SharePoint can throttle requests. Previews and folder listings also contact the service. Cloud operations share a queue across app sessions using this computer's app settings. Avoid repeated previews and simultaneous transfers in other tools; let rclone wait and retry when throttled."),
    shiny::tabsetPanel(
      id = ns("flow_tabs"), selected = "remote",
      shiny::tabPanel(shiny::tags$span("Read remote", shiny::tags$br(), " pond"), value = "remote",
        shiny::selectInput(ns("cached_pond"), field_label("Saved remote pond (available offline)", "Choose a saved pond to browse cached metadata, or New connection to connect another pond."), choices = c("New connection" = "")),
        shiny::conditionalPanel("input.cached_pond && input.cached_pond !== ''", ns = ns,
          shiny::checkboxInput(ns("edit_remote_settings"), "Edit connection settings", FALSE)
        ),
        shiny::conditionalPanel("!input.cached_pond || input.edit_remote_settings", ns = ns,
          shiny::textInput(ns("remote_name"), field_label("Pond nickname", "A friendly name for this remote pond in your saved list, such as Team archive. Leave blank to use its folder path.")),
          shiny::textInput(ns("remote"), field_label("Remote pond folder", "Enter the folder where the owner published the pond, such as team_data:pond, or a local folder path. Use a configured remote name, not a SharePoint web link.", requirement = "Mandatory", when = "Required to connect or refresh metadata; filled automatically for saved ponds."), placeholder = "team_data:pond"),
          shiny::textInput(ns("cache"), field_label("Local pond cache", "Choose an empty local folder for a new connection, or this pond's existing cache. Metadata and any datasets you download are stored here.", requirement = "Mandatory", when = "Required to connect or refresh metadata; filled automatically for saved ponds."), placeholder = "E:/pond_cache/team or /mnt/external/pond_cache/team"),
          shinyFiles::shinyDirButton(ns("cache_folder"), "Choose local pond cache", "Choose an empty folder for the local pond cache")
        ),
        shiny::actionButton(ns("refresh_cache"), "Connect / refresh metadata only"),
        shiny::helpText("The owner first uses Publish pond. Connecting downloads the catalogue and metadata exports only. Cached metadata works offline. This connection never uploads to the remote pond."),
        shiny::textOutput(ns("cache_status")),
        DT::DTOutput(ns("catalogue")),
        shiny::actionButton(ns("download"), "Download / update selected dataset", class = "btn-primary"),
        shiny::actionButton(ns("evict"), "Remove selected download from local pond cache"),
        shiny::textOutput(ns("download_path")),
        shiny::uiOutput(ns("metadata_detail"))
      ),
      shiny::tabPanel("Run Flows", value = "run",
        shiny::h4("Select saved flows"),
        shiny::uiOutput(ns("routine_help")),
        DT::DTOutput(ns("routine_components")),
        shiny::textOutput(ns("routine_selection")),
        shiny::actionButton(ns("select_all_routines"), "Select all"),
        shiny::actionButton(ns("clear_routine_selection"), "Clear selection"),
        shiny::actionButton(ns("edit_routine"), "Edit selected flow"),
        shiny::actionButton(ns("preview_routines"), "Preview selected flows", class = "btn-primary"),
        shiny::actionButton(ns("delete_routines"), "Remove selected flows"),
        shiny::helpText("Flows run with their saved settings, in the order shown before table sorting. Preview first, review the plan and log, then run."),
        shiny::hr(),
        shiny::h4("Publish pond"),
        shiny::uiOutput(ns("publication_summary")),
        shiny::tags$details(open = "open",
          shiny::tags$summary("Publication settings"),
          shiny::textInput(ns("publish_destination"), field_label("Publication destination",
            "Use a dedicated remote folder, such as team_data:published_pond, or a local publication folder. The source is the active pond. These settings are separate from individual flows.", "Mandatory"), placeholder = "team_data:published_pond"),
          shiny::checkboxInput(ns("publish_data"), field_label("Also upload registered dataset folders (data, documents and scripts)",
            "Leave unchecked to publish the catalogue and metadata only. Registered folders retain their src/dev/arc paths."), FALSE),
          shiny::actionButton(ns("save_publication"), "Save publication settings")
        ),
        shiny::actionButton(ns("preview_publish"), "Preview / retry pond publication", class = "btn-primary"),
        shiny::helpText("Preview also saves these settings. After a failure or restart, return here, preview again, then choose Run flow. Completed files that still match are reused; incomplete files may need to restart. Checking existing files still contacts the remote."),
        shiny::helpText("For overnight publishing, sign in shortly before starting and keep the app connected and the computer awake. Jobs stop when the session closes or reaches its 12-hour limit. The 15-minute preview expiry limits when a run can start; it does not limit a running transfer to 15 minutes."),
        shiny::hr(),
        shiny::h4("Flow preview"),
        shiny::textOutput(ns("preview_status")),
        shiny::checkboxInput(ns("fresh_preview"), "Request a fresh preview even if a recent matching preview is available", FALSE),
        shiny::helpText("A matching preview is reused for up to 15 minutes. Select this option before previewing again if files have changed. A preview is a point-in-time check, not a guarantee that files will stay unchanged."),
        DT::DTOutput(ns("transfer_plan")),
        shiny::conditionalPanel("output.mirror_ack_required === 'true'", ns = ns,
          shiny::checkboxInput(ns("delete_ack"), field_label("I understand extra destination files will be deleted",
            "Required before running the previewed mirror sync, including a batch containing any mirror flow.", requirement = "Conditional"), FALSE)
        ),
        shiny::actionButton(ns("run"), "Run flow", class = "btn-primary"),
        shiny::helpText("Run requires a successful preview. Changes to the flow require a new preview. Previews describe current differences; files may change before execution.")
      ),
      shiny::tabPanel(shiny::tags$span("Define", shiny::tags$br(), " connections"), value = "config",
        shiny::h4("Define remote and local connections"),
        shiny::p("Give frequently used folders a name, then reuse them when designing flows. You can hide this tab once setup is complete."),
        shiny::fluidRow(
          shiny::column(6,
            shiny::h4("Remote connections"),
            shiny::p("Existing rclone connections are detected automatically. To add SharePoint or OneDrive, sign in and choose a site and document library."),
            shiny::textInput(ns("connection_name"), field_label("New remote connection name", "Start with a letter and use at least two letters, digits, underscores or hyphens, with no spaces. Enter an existing name to resume or reconnect it.", requirement = "Conditional", when = "Required only when signing in or reconnecting a remote library."), placeholder = "team_data"),
            shiny::actionButton(ns("connect_library"), "Sign in and discover libraries"),
            shiny::actionButton(ns("reconnect_library"), "Resume / reconnect named library"),
            shiny::uiOutput(ns("config_question")),
            shiny::helpText("Requires rclone on this computer. Credentials stay in rclone's configuration; no drive mount is needed."),
            shiny::p(shiny::tags$a(href = "https://rclone.org/onedrive/", target = "_blank", "SharePoint setup and permission help"))
          ),
          shiny::column(6,
            shiny::h4("Saved locations"),
            shiny::selectInput(ns("saved_location"), field_label("Edit a saved location", "Choose New location to add a folder, or select a saved location to edit its name and path."), choices = c("New location" = "")),
            shiny::textInput(ns("location_name"), field_label("Location name", "Use a recognisable name, such as Field laptop or Team library. This name appears in the flow editor.", requirement = "Mandatory", when = "Required to save a location."), placeholder = "Field laptop or Team library"),
            shiny::textInput(ns("location_path"), field_label("Local folder or remote folder", "Choose an existing local folder or drive, or enter a folder in a configured remote, such as team_data:Shared/data.", requirement = "Mandatory", when = "Required to save a location."), placeholder = "/mnt/field_data or team_data:Shared/data"),
            shinyFiles::shinyDirButton(ns("local_folder"), "Choose local folder", "Choose a local connection folder"),
            shiny::actionButton(ns("save_location"), "Save location", class = "btn-primary"),
            shiny::actionButton(ns("remove_location"), "Remove saved location"),
            shiny::helpText("Save local folders, external drives or folders in a configured remote. Flows retain their saved paths when a location is edited or removed.")
          )
        ),
        shiny::hr(),
        shiny::h4("Connection speed"),
        shiny::selectInput(ns("speed_connection"), field_label("Apply settings to", "Cloud defaults cover every remote without its own profile. Local defaults apply when both paths are local. A named remote profile covers all folders in that connection."),
          choices = c("All cloud connections (default)" = "__cloud", "Local transfers (default)" = "__local")),
        shiny::selectInput(ns("speed_preset"), field_label("Speed profile", "Cloud cautious reduces simultaneous work and API calls. Local has no API or bandwidth cap. Custom lets you set each limit."),
          choices = c("Cloud cautious" = "cloud", "Local" = "local", "Custom" = "custom")),
        shiny::div(class = "flow-field-help", shiny::textOutput(ns("speed_summary"))),
        shiny::conditionalPanel("input.speed_preset === 'custom'", ns = ns,
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("speed_transfers"), field_label("Simultaneous file transfers", "Maximum files transferred at once. Start with 1 for SharePoint.", "Conditional", "Required for Custom."), value = 1, min = 1, step = 1),
              shiny::numericInput(ns("speed_checkers"), field_label("Simultaneous file checks", "Parallel checks used to compare source and destination. Start with 2 for cloud connections.", "Conditional", "Required for Custom."), value = 2, min = 1, step = 1),
              shiny::numericInput(ns("speed_bandwidth"), field_label("Bandwidth limit (MiB/s)", "Limits transferred data, separately from API calls. Zero means unlimited. Lower this to leave bandwidth for other work.", "Conditional", "Required for Custom."), value = 0, min = 0, step = 0.5)
            ),
            shiny::column(6,
              shiny::numericInput(ns("speed_tps"), field_label("API requests per second", "Limits HTTP transactions, including listings and previews. Zero means unlimited; 2 is the cautious starting point, not a guaranteed service quota.", "Conditional", "Required for Custom."), value = 2, min = 0, step = 0.5),
              shiny::numericInput(ns("speed_burst"), field_label("API request burst", "Maximum short burst of API requests. Start with 1 to spread requests out.", "Conditional", "Required for Custom."), value = 1, min = 1, step = 1)
            )
          )
        ),
        shiny::actionButton(ns("save_speed"), "Save speed settings"),
        shiny::actionButton(ns("reset_speed"), "Use default for this connection"),
        shiny::helpText("Saved settings apply to previews, runs, listings, publishing and remote pond downloads. Remote-to-remote flows use the stricter limits of their two connections. Running and queued jobs keep the settings they started with; changes require a new preview."),
        shiny::hr(),
        shiny::tags$details(
          shiny::tags$summary("Browse locations and libraries"),
          shiny::actionButton(ns("discover"), "Detect configured libraries and drives"),
          shiny::selectInput(ns("location"), field_label("Available location", "Choose a saved folder, configured remote library or local drive to fill the browsing path below."), choices = character()),
          shiny::textInput(ns("browse_path"), field_label("Folder or rclone remote:path", "Enter or adjust the folder path, then list its contents. Select a folder in the results to open it, or use Parent folder to go up.", requirement = "Conditional", when = "Required when listing files and folders.")),
          shiny::actionButton(ns("browse"), "List files and folders"),
          shiny::actionButton(ns("enter"), "Open selected folder"),
          shiny::actionButton(ns("up"), "Parent folder"),
          DT::DTOutput(ns("listing")),
          shiny::actionButton(ns("save_browsed_location"), "Define this folder as a location"),
          shiny::actionButton(ns("use_source"), "Use folder as flow source"),
          shiny::actionButton(ns("use_item"), "Use selected file / folder as flow source"),
          shiny::actionButton(ns("use_destination"), "Use folder as flow destination"),
          shiny::actionButton(ns("use_remote"), "Use as remote pond")
        ),
        shiny::hr(),
        shiny::actionButton(ns("design_tasks"), "Next: Design flow", class = "btn-primary")
      ),
      shiny::tabPanel("Design flow", value = "design",
        shiny::h4("Design a flow"),
        shiny::p("Choose saved folders or configured remotes, adjust the full paths if needed, then save a named flow to reuse. Existing syncs.json components are supported as flows."),
        shiny::selectInput(ns("routine"), field_label("Edit flow", "Choose a saved flow to change its settings, or New flow to start a separate flow."), choices = c("New flow" = "")),
        shiny::actionButton(ns("new_routine"), "New flow"),
        shiny::textInput(ns("routine_name"), field_label("Flow name", "Describe the job and direction, such as Upload field observations. Changing the name and saving renames the current flow.", requirement = "Conditional", when = "Required to save or duplicate a flow; optional for a one-off preview."), placeholder = "Upload field observations"),
        shiny::fluidRow(
          shiny::column(6,
            shiny::selectInput(ns("source_location"), field_label("Choose a source location", "Choose a saved folder or configured remote to fill the source path. For a remote, add the required subfolder to the path below."), choices = c("Enter path below" = "")),
            shiny::textInput(ns("source"), field_label("Flow from \u2014 source", "Enter the file or folder to copy. A folder's contents go directly into the destination. You can extend a saved path to a subfolder.", requirement = "Mandatory", when = "Required for a file transfer or saved flow."), placeholder = "/mnt/drive/dataset or team_data:dataset"),
            shiny::actionButton(ns("selected_dataset"), "Use current dataset folder")
          ),
          shiny::column(6,
            shiny::selectInput(ns("destination_location"), field_label("Choose a destination location", "Choose a saved folder or configured remote to fill the destination path. For a remote, add a dedicated destination subfolder to the path below."), choices = c("Enter path below" = "")),
            shiny::textInput(ns("destination"), field_label("Flow to \u2014 destination", "Enter the full folder to receive the files. Include a dataset folder name if needed, such as team_data:shared/observations.", requirement = "Mandatory", when = "Required for transfers and saved flows. Pond publication has its own destination in Run Flows."), placeholder = "team_data:shared/dataset or E:/cache/dataset")
          )
        ),
        shiny::actionButton(ns("swap"), "Swap upload / download direction"),
        shiny::selectInput(ns("mode"), field_label("Operation", "Copy / update replaces changed files and keeps extra destination files. Mirror sync also deletes extras; use a dedicated destination folder.", requirement = "Conditional", when = "Required for transfers and saved flows; Copy / update is selected by default. Pond publication uses copy."), choices = c("Copy / update (keep extra destination files)" = "copy", "Mirror sync (delete extra destination files)" = "sync")),
        shiny::textAreaInput(ns("excludes"), field_label("Exclude patterns (one per line)", "Skip matching files or folders: for example, *.tmp for temporary files or **/.git/** for Git folders. Leave blank to include everything."), value = "**/.git/**", rows = 3),
        shiny::actionButton(ns("save_routine"), "Save flow", class = "btn-primary"),
        shiny::actionButton(ns("duplicate_routine"), "Duplicate flow"),
        shiny::actionButton(ns("preview_transfer"), "Preview flow"),
        shiny::helpText("Save changes before running flows from the Run Flows tab. Preview flow uses only the editor's current settings, including unsaved changes."),
        shiny::actionButton(ns("go_run"), "Next: select flows and run"),
        shiny::hr(),
        shiny::tags$details(
          shiny::tags$summary("Import / export flows"),
          shiny::fileInput(ns("import"), field_label("Import flows (syncs.json)", "Choose a syncs.json file to add saved flows. Importing only loads their settings; it does not run transfers.", requirement = "Conditional", when = "Choose a file only when importing flows."), accept = ".json"),
          shiny::downloadButton(ns("export_routines"), "Export flows")
        )
      )
    ),
    shiny::hr(),
    shiny::strong(shiny::textOutput(ns("status"))),
    shiny::div(role = "status", `aria-live` = "polite", shiny::textOutput(ns("cloud_status"))),
    shiny::div(class = "flow-field-help", shiny::textOutput(ns("operation_speed"))),
    shiny::uiOutput(ns("operation_progress")),
    shiny::actionButton(ns("cancel"), "Cancel current operation"),
    shiny::helpText("An active transfer or queued job keeps your session active. Keep the app connected and the computer awake. Signing out, closing the app, revoked access or the 12-hour session limit stops the job."),
    shiny::h4("Recent file actions and warnings"),
    shiny::p(class = "flow-field-help", "Messages from this operation's rclone process. Repeated progress snapshots are shown in the monitor above; this list keeps proposed changes, completed actions, warnings and errors."),
    shiny::tags$pre(id = ns("operation_events"), class = "shiny-text-output flow-terminal"),
    shiny::tags$details(
      shiny::tags$summary("Technical output (recent rclone log)"),
      shiny::p(class = "flow-field-help", "Recent output only, not a complete audit log. INFO means normal status information; it does not mean an error."),
      shiny::tags$pre(id = ns("log"), class = "shiny-text-output flow-terminal")
    )
  )
}

mod_data_movement_server <- function(id, pond, dataset_path, state_directory = app_state_dir(), open_pond_request = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    rclone_status <- shiny::eventReactive(input$check_rclone, {
      movement_rclone_status()
    }, ignoreNULL = FALSE)
    output$rclone_status <- shiny::renderUI({
      access_require("admin")
      status <- rclone_status()
      if (isTRUE(status$available)) {
        return(shiny::tags$p(class = "text-success", role = "status",
          shiny::strong("rclone is available. "), status$version))
      }
      shiny::tags$div(class = "alert alert-warning", role = "status",
        shiny::strong("rclone was not found or could not be started."),
        shiny::tags$p("rclone is not bundled with DataPond. Install it on the computer running the app to enable previews, transfers, publication and remote refreshes. Existing cached metadata can still be browsed offline."),
        shiny::tags$p(shiny::tags$a(href = "https://rclone.org/install/",
          target = "_blank", rel = "noopener noreferrer",
          "Download and install rclone: official Windows, macOS and Linux instructions")),
        shiny::tags$p("Make sure rclone is available on your system PATH, then click Check Flow (sync) engine. If you changed PATH and it is still not found, restart R and reopen DataPond.")
      )
    })

    state <- shiny::reactiveValues(job = NULL, done = NULL, failure = NULL, private = FALSE, status = "Ready", log = "", report = NULL,
      preview = NULL, publication_settings = NULL, publication_pond = NULL, listing = data.frame(), config = NULL, config_name = NULL,
      cache = NULL, cache_tick = 0L, download_path = "", routines = list(), ponds = list(),
      editing_id = "", selected_routine_ids = character(), deleting_routine_ids = character(),
      locations = list(), editing_location_id = "", available_locations = character(), remote_locations = character(), speed_tick = 0L, job_tick = 0L)
    routines_file <- file.path(state_directory, "movement-routines.rds")
    ponds_file <- file.path(state_directory, "remote-pond-caches.rds")
    locations_file <- file.path(state_directory, "movement-locations.rds")
    locations_load_error <- NULL
    if (file.exists(locations_file)) state$locations <- tryCatch({
      saved <- readRDS(locations_file)
      if (!is.list(saved) || !all(vapply(saved, function(x) {
        is.list(x) && all(vapply(x[c("id", "name", "path")], function(value)
          is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value), logical(1)))
      }, logical(1)))) stop("Invalid saved locations.")
      saved
    }, error = function(e) {
      locations_load_error <<- paste("Saved locations could not be loaded:", conditionMessage(e))
      state$status <- locations_load_error
      list()
    })
    routines_load_error <- NULL
    if (file.exists(routines_file)) state$routines <- tryCatch(
      lapply(readRDS(routines_file), movement_normalise_profile), error = function(e) {
        routines_load_error <<- paste("Saved flows could not be loaded:", conditionMessage(e))
        state$status <- routines_load_error
        list()
      }
    )
    if (file.exists(ponds_file)) state$ponds <- tryCatch(readRDS(ponds_file), error = function(e) list())
    save_settings <- function(value, path) {
      if (identical(path, routines_file) && !is.null(routines_load_error)) stop(routines_load_error)
      if (identical(path, locations_file) && !is.null(locations_load_error)) stop(locations_load_error)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(value, path)
    }
    protect <- function(code) tryCatch(force(code), error = function(e) {
      state$status <- conditionMessage(e)
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
    require_fields <- function(action, fields) {
      missing <- names(fields)[!vapply(fields, function(value) {
        is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
      }, logical(1))]
      if (length(missing)) stop(paste0("Required for ", action, ": ", paste(missing, collapse = ", "), "."), call. = FALSE)
      invisible(TRUE)
    }
    shiny::observe({
      access_require("admin")
      shiny::req(pond())
      key <- movement_endpoint(pond())
      if (identical(key, state$publication_pond)) return()
      state$publication_pond <- key
      state$publication_settings <- NULL
      shiny::updateTextInput(session, "publish_destination", value = "")
      shiny::updateCheckboxInput(session, "publish_data", value = FALSE)
      protect({
        saved <- movement_publication_read(state_directory, key)
        state$publication_settings <- saved
        if (!is.null(saved)) {
          shiny::updateTextInput(session, "publish_destination", value = saved$destination)
          shiny::updateCheckboxInput(session, "publish_data", value = saved$include_data)
        }
      })
    })
    save_publication <- function() {
      require_fields("publishing a pond", list("Publication destination" = input$publish_destination))
      state$publication_settings <- movement_publication_save(state_directory, pond(),
        input$publish_destination, isTRUE(input$publish_data))
    }
    shiny::observeEvent(input$save_publication, { access_require("admin", touch = TRUE); protect({
      save_publication()
      state$status <- "Publication settings saved for this pond. Preview when ready; no remote requests were sent."
    }) })
    output$publication_summary <- shiny::renderUI({
      access_require("admin")
      saved <- state$publication_settings
      shiny::tagList(
        shiny::p("Source: ", shiny::strong(pond())),
        if (is.null(saved)) shiny::p(class = "help-block", "Choose a destination below once. Saving settings does not start a publication.") else
          shiny::p("Saved destination: ", shiny::strong(saved$destination), " \u00b7 ",
            if (saved$include_data) "Catalogue and registered dataset folders" else "Catalogue and metadata only"))
    })
    preview_needs_mirror_ack <- function(previewed) {
      !is.null(previewed) && (identical(previewed$spec$mode, "sync") ||
        (identical(previewed$spec$kind, "routines") &&
          any(vapply(previewed$spec$profiles, function(x) identical(x$mode, "sync"), logical(1)))))
    }
    output$mirror_ack_required <- shiny::renderText({ access_require("admin"); if (preview_needs_mirror_ack(state$preview)) "true" else "false" })
    shiny::outputOptions(output, "mirror_ack_required", suspendWhenHidden = FALSE)
    idle <- function() {
      if (!is.null(state$job) && state$job$status == "running") stop("Wait for the current operation, or cancel it first.")
    }
    start <- function(steps, label, done = function() NULL, private = FALSE, failure = function() NULL, labels = NULL,
                      profiles = movement_speed_read(state_directory), not_after = NULL) {
      idle()
      state$job <- tryCatch(movement_job_start(steps, control_directory = state_directory, profiles = profiles,
        not_after = not_after), error = function(e) { failure(); stop(e) })
      state$job$labels <- labels
      state$done <- done; state$failure <- failure; state$private <- private; state$status <- label
      state$log <- if (private && steps[[1]][1] == "config") "Complete Microsoft sign-in in your browser when prompted. Configuration responses are not recorded in the transfer log." else "Starting\u2026"
      state$report <- movement_job_report(state$job, private = private)
    }
    last_job_activity <- -Inf
    shiny::observe({
      access_require("admin")
      shiny::invalidateLater(500, session)
      job <- state$job
      if (is.null(job) || job$status != "running") return()
      # Authorised background work counts as activity, but cannot revive an
      # expired/revoked session. Do not keep an unfinished OAuth prompt alive.
      # Throttle writes independently of the more frequent progress polling.
      if (!isTRUE(state$private) && access_now() - last_job_activity >= 60) {
        access_require("admin", touch = TRUE)
        last_job_activity <<- access_now()
      }
      protect({
        status <- movement_job_poll(job)
        state$job_tick <- shiny::isolate(state$job_tick) + 1L
        if (!state$private) state$log <- movement_job_log(job)
        else if (status == "running") {
          # OAuth emits a localhost login URL on stderr; expose only that URL.
          log <- movement_job_log(job)
          url <- regmatches(log, regexpr("https?://(127[.]0[.]0[.]1|localhost):[0-9]+/[^ \n\r]+", log))
          if (length(url) && nzchar(url)) state$log <- paste("If your sign-in browser did not open, visit:", url)
        }
        state$report <- movement_job_report(job, log = state$log,
          current_log = if (state$private) "" else movement_job_log(job, current = TRUE, lines = 250L), private = state$private)
        if (status == "complete") {
          callback <- state$done; failed <- state$failure
          state$done <- NULL; state$failure <- NULL
          state$status <- "Completed"
          tryCatch(callback(), error = function(e) { failed(); stop(e) })
        } else if (status == "failed") {
          state$preview <- NULL
          state$status <- "Failed. No later steps were run. Review the log and retry; existing files can be reused."
          if (!is.null(job$error)) state$status <- paste(state$status, job$error)
          if (!is.null(state$failure)) state$failure()
          state$failure <- NULL
          if (state$private) {
            state$status <- "Connection or listing failed. Check the location, rclone configuration and Microsoft permissions."
            unlink(job$directory, recursive = TRUE)
          }
        }
      })
    })
    session$onSessionEnded(function() shiny::isolate({
      movement_job_cancel(state$job)
      if (!is.null(state$failure)) state$failure()
      if (state$private && !is.null(state$job)) unlink(state$job$directory, recursive = TRUE)
      if (!is.null(state$preview$plan$snapshot)) unlink(state$preview$plan$snapshot$directory, recursive = TRUE)
    }))
    session$userData$datapond_access_cleanup <- c(session$userData$datapond_access_cleanup, list(function() {
      movement_job_cancel(state$job)
      if (!is.null(state$failure)) state$failure()
      state$failure <- NULL; state$done <- NULL
    }))
    shiny::observeEvent(input$cancel, { access_require("admin", touch = TRUE); protect({
      movement_job_cancel(state$job)
      if (!is.null(state$report) && identical(state$job$status, "cancelled")) {
        report <- state$report; report$status <- "cancelled"; report$queued <- FALSE; state$report <- report
      }
      if (!is.null(state$failure)) state$failure()
      if (state$private && !is.null(state$job)) unlink(state$job$directory, recursive = TRUE)
      state$done <- NULL; state$failure <- NULL; state$preview <- NULL
      state$status <- "Cancelled. Partial files may remain; retrying reuses completed files."
    }) })
    output$status <- shiny::renderText({
      access_require("admin")
      state$job_tick
      job <- state$job
      if (!is.null(job) && job$status == "running" && length(job$labels)) {
        paste(state$status, "Flow", job$index, "of", length(job$steps), "-", job$labels[job$index])
      } else state$status
    })
    output$log <- shiny::renderText({ access_require("admin"); state$log })
    output$operation_progress <- shiny::renderUI({ access_require("admin"); movement_progress_ui(state$report) })
    output$operation_events <- shiny::renderText({
      access_require("admin")
      if (isTRUE(state$report$private)) return(state$log)
      events <- state$report$events
      if (!length(events)) return("No file actions or warnings in the recent output. See the operation monitor for progress.")
      paste(events, collapse = "\n")
    })
    output$cloud_status <- shiny::renderText({ access_require("admin");  state$job_tick; movement_job_feedback(state$job) })
    output$operation_speed <- shiny::renderText({
      access_require("admin")
      if (is.null(state$report)) return("Engine: rclone for cloud and local transfers. Cloud cautious is the default for remote connections.")
      state$report$speed_summary
    })
    load_speed_form <- function() {
      target <- input$speed_connection %||% "__cloud"
      profiles <- movement_speed_read(state_directory)
      profile <- profiles[[target]] %||% (if (target == "__local") movement_speed_profile("local") else
        profiles[["__cloud"]] %||% movement_speed_profile("cloud"))
      shiny::updateSelectInput(session, "speed_preset", selected = profile$preset)
      for (field in c("transfers", "checkers", "tps", "burst", "bandwidth"))
        shiny::updateNumericInput(session, paste0("speed_", field), value = profile[[field]])
    }
    shiny::observeEvent(input$speed_connection, { access_require("admin", touch = TRUE); protect(load_speed_form()) })
    output$speed_summary <- shiny::renderText({
      access_require("admin")
      preset <- input$speed_preset %||% "cloud"
      if (preset == "custom") return("Custom: API limits control request traffic; bandwidth limits control bytes transferred. Save to apply these values.")
      if (preset == "local") return("Local: 4 transfers, 4 checks, unlimited API rate and bandwidth. Use Cloud cautious for SharePoint and other services with request limits.")
      "Cloud cautious: 1 transfer, 2 checks, 2 API requests/second, burst 1; unlimited bandwidth. A starting point, not a guarantee against throttling."
    })
    speed_changed <- function(message) {
      state$speed_tick <- state$speed_tick + 1L
      if (!is.null(state$preview)) { saved <- state$preview; saved$stale <- TRUE; state$preview <- saved }
      state$status <- message
    }
    shiny::observeEvent(input$save_speed, { access_require("admin", touch = TRUE); protect({
      profile <- movement_speed_profile(input$speed_preset %||% "cloud", input$speed_transfers,
        input$speed_checkers, input$speed_tps, input$speed_burst, input$speed_bandwidth)
      movement_speed_save(state_directory, input$speed_connection %||% "__cloud", profile)
      speed_changed("Speed settings saved. Queued and running operations keep their original settings. Preview again before the next run.")
    }) })
    shiny::observeEvent(input$reset_speed, { access_require("admin", touch = TRUE); protect({
      movement_control(state_directory, function(con) DBI::dbExecute(con,
        "DELETE FROM speed WHERE connection = ?", params = list(input$speed_connection %||% "__cloud")))
      load_speed_form()
      speed_changed("Default speed settings restored. Preview again before the next run.")
    }) })
    shiny::observeEvent(input$show_config, {
      access_require("admin", touch = TRUE)
      if (isTRUE(input$show_config)) shiny::showTab("flow_tabs", "config", session = session)
      else {
        if (is.null(input$flow_tabs) || identical(input$flow_tabs, "config"))
          shiny::updateTabsetPanel(session, "flow_tabs", selected = "run")
        shiny::hideTab("flow_tabs", "config", session = session)
      }
    })
    shiny::observeEvent(input$design_tasks, { access_require("admin", touch = TRUE); shiny::updateTabsetPanel(session, "flow_tabs", selected = "design") })
    shiny::observeEvent(input$go_run, { access_require("admin", touch = TRUE); shiny::updateTabsetPanel(session, "flow_tabs", selected = "run") })
    shiny::observeEvent(TRUE, {
      access_require("admin", touch = TRUE)
      if (length(state$routines)) {
        shiny::updateCheckboxInput(session, "show_config", value = FALSE)
        shiny::hideTab("flow_tabs", "config", session = session)
        shiny::updateTabsetPanel(session, "flow_tabs", selected = "run")
      }
    }, once = TRUE)
    location_ids <- function() vapply(state$locations, `[[`, character(1), "id")
    shiny::observe({
      access_require("admin")
      saved <- stats::setNames(location_ids(), vapply(state$locations, `[[`, character(1), "name"))
      shiny::updateSelectInput(session, "saved_location", choices = c("New location" = "", saved),
        selected = state$editing_location_id)
      choices <- list("Enter path below" = "")
      if (length(saved)) choices[["Saved folders"]] <- saved
      if (length(state$remote_locations)) choices[["Configured remotes"]] <- stats::setNames(
        paste0("remote:", unname(state$remote_locations)), names(state$remote_locations))
      shiny::updateSelectInput(session, "source_location", choices = choices, selected = "")
      shiny::updateSelectInput(session, "destination_location", choices = choices, selected = "")
      paths <- stats::setNames(vapply(state$locations, `[[`, character(1), "path"), if (length(saved)) paste0(names(saved), " (saved)") else character())
      shiny::updateSelectInput(session, "location", choices = c(paths, state$available_locations),
        selected = shiny::isolate(input$location) %||% "")
    })
    shiny::observeEvent(input$saved_location, {
      access_require("admin", touch = TRUE)
      if (identical(input$saved_location, state$editing_location_id)) return()
      index <- match(input$saved_location, location_ids())
      saved <- if (is.na(index)) NULL else state$locations[[index]]
      state$editing_location_id <- saved$id %||% ""
      shiny::updateTextInput(session, "location_name", value = saved$name %||% "")
      shiny::updateTextInput(session, "location_path", value = saved$path %||% "")
    })
    shiny::observeEvent(input$save_location, { access_require("admin", touch = TRUE); protect({
      require_fields("saving a location", list("Location name" = input$location_name, "Local folder or remote folder" = input$location_path))
      name <- trimws(input$location_name %||% "")
      if (!nzchar(name)) stop("Give this location a name.")
      path <- movement_endpoint(input$location_path)
      if (movement_remote(path)) {
        if (!sub(":.*$", "", path) %in% movement_remotes()$name)
          stop("Connect this remote first, then save a folder within it.")
      } else if (!dir.exists(path)) stop("Choose an existing local folder or connected drive.")
      index <- match(state$editing_location_id, location_ids())
      id <- if (is.na(index)) new_uuid() else state$locations[[index]]$id
      others <- state$locations[location_ids() != id]
      if (name %in% vapply(others, `[[`, character(1), "name")) stop("Another location already has that name.")
      saved <- list(id = id, name = name, path = path)
      locations <- state$locations
      if (is.na(index)) locations <- c(locations, list(saved)) else locations[[index]] <- saved
      save_settings(locations, locations_file)
      state$locations <- locations
      state$editing_location_id <- id
      state$status <- "Location saved. Use it as a source or destination in Design flow."
    }) })
    shiny::observeEvent(input$remove_location, { access_require("admin", touch = TRUE); protect({
      id <- input$saved_location %||% ""
      if (!id %in% location_ids()) stop("Select a saved location to remove.")
      locations <- state$locations[location_ids() != id]
      save_settings(locations, locations_file)
      state$locations <- locations
      state$editing_location_id <- ""
      shiny::updateTextInput(session, "location_name", value = "")
      shiny::updateTextInput(session, "location_path", value = "")
      state$status <- "Saved location removed. Files, remote credentials and flow paths were kept."
    }) })
    shiny::observeEvent(input$save_browsed_location, {
      access_require("admin", touch = TRUE)
      state$editing_location_id <- ""
      shiny::updateTextInput(session, "location_name", value = "")
      shiny::updateTextInput(session, "location_path", value = input$browse_path)
    })
    for (target in c("source", "destination")) local({
      field <- target
      shiny::observeEvent(input[[paste0(field, "_location")]], {
        access_require("admin", touch = TRUE)
        selected <- input[[paste0(field, "_location")]]
        index <- match(selected, location_ids())
        remote_index <- match(selected, paste0("remote:", unname(state$remote_locations)))
        if (!is.na(index)) {
          shiny::updateTextInput(session, field, value = state$locations[[index]]$path)
          shiny::updateSelectInput(session, paste0(field, "_location"), selected = "")
        } else if (!is.na(remote_index)) {
          shiny::updateTextInput(session, field, value = unname(state$remote_locations[remote_index]))
          shiny::updateSelectInput(session, paste0(field, "_location"), selected = "")
        }
      })
    })
    discover <- function() {
      remotes <- movement_remotes()
      roots <- data_pond_folder_roots()
      if (is.function(roots)) roots <- roots()
      remote_choices <- if (nrow(remotes)) stats::setNames(paste0(remotes$name, ":"), paste0(remotes$name, " (", remotes$type, ")")) else character()
      state$remote_locations <- remote_choices
      choices <- c(remote_choices, stats::setNames(as.character(roots), paste0(names(roots), " (local)")))
      state$available_locations <- choices
      shiny::updateSelectInput(session, "speed_connection", choices = c(
        "All cloud connections (default)" = "__cloud", "Local transfers (default)" = "__local",
        stats::setNames(remotes$name, paste0(remotes$name, " (remote)"))),
        selected = input$speed_connection %||% "__cloud")
      state$status <- paste(nrow(remotes), "configured libraries found. Choose a remote in Design flow, or save a named folder in Define connections.")
    }
    shiny::observeEvent(TRUE, { access_require("admin", touch = TRUE); protect(discover()) }, once = TRUE)
    shiny::observeEvent(input$discover, { access_require("admin", touch = TRUE); protect(discover()) })
    shiny::observeEvent(input$location, { access_require("admin", touch = TRUE); shiny::updateTextInput(session, "browse_path", value = input$location) })
    shiny::observeEvent(input$browse, { access_require("admin", touch = TRUE); protect({
      idle()
      require_fields("browsing a location", list("Folder or rclone remote:path" = input$browse_path))
      path <- movement_endpoint(input$browse_path)
      start(list(c("lsjson", path, "--no-mimetype", "--no-modtime")), "Listing location\u2026", function() {
        # lsjson stdout is JSON; stderr is separate from stdout for this job.
        result <- jsonlite::fromJSON(paste(readLines(state$job$outputs[1], warn = FALSE), collapse = "\n"))
        state$listed_path <- path
        state$listing <- if (is.data.frame(result)) result[, c("Path", "IsDir", "Size"), drop = FALSE] else data.frame()
        state$status <- paste("Listed", path)
        state$log <- ""
        unlink(state$job$directory, recursive = TRUE)
      }, private = TRUE)
    }) })
    shiny::observeEvent(input$use_item, { access_require("admin", touch = TRUE); protect({
      row <- input$listing_rows_selected
      if (length(row) != 1L) stop("Select a file or folder in the listing.")
      shiny::updateTextInput(session, "source", value = movement_join(state$listed_path, state$listing$Path[row]))
      shiny::updateTabsetPanel(session, "flow_tabs", selected = "design")
    }) })
    shiny::observeEvent(input$up, { access_require("admin", touch = TRUE); protect({
      path <- movement_endpoint(input$browse_path)
      parent <- if (movement_remote(path)) {
        root <- sub(":.*$", ":", path)
        relative <- substring(path, nchar(root) + 1L)
        if (!grepl("/", relative)) root else paste0(root, dirname(relative))
      } else dirname(path)
      shiny::updateTextInput(session, "browse_path", value = parent)
    }) })
    output$listing <- DT::renderDT({ access_require("admin"); datapond_datatable(state$listing, rownames = FALSE, selection = "single", options = list(pageLength = 8)) })
    shiny::observeEvent(input$enter, { access_require("admin", touch = TRUE); protect({
      row <- input$listing_rows_selected
      if (length(row) != 1L || !isTRUE(state$listing$IsDir[row])) stop("Select a folder in the listing.")
      shiny::updateTextInput(session, "browse_path", value = movement_join(state$listed_path, state$listing$Path[row]))
    }) })
    for (target in c("source", "destination", "remote")) local({
      field <- target
      shiny::observeEvent(input[[paste0("use_", field)]], {
        access_require("admin", touch = TRUE)
        shiny::updateTextInput(session, field, value = input$browse_path)
        shiny::updateTabsetPanel(session, "flow_tabs", selected = if (field == "remote") "remote" else "design")
      })
    })
    config_done <- function() {
      path <- state$job$outputs[length(state$job$outputs)]
      response <- jsonlite::fromJSON(paste(readLines(path, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
      unlink(state$job$directory, recursive = TRUE)
      if (!is.null(response$State) && nzchar(response$State)) {
        state$config <- response; state$status <- "Answer the connection question below."
      } else {
        state$config <- NULL; state$status <- "Library connected."; discover()
      }
    }
    shiny::observeEvent(input$connect_library, { access_require("admin", touch = TRUE); protect({
      idle()
      require_fields("connecting a remote library", list("New remote connection name" = input$connection_name))
      name <- trimws(input$connection_name)
      if (!grepl("^[A-Za-z][A-Za-z0-9_-]+$", name) || nchar(name) < 2L) stop("Use a connection name with at least two letters, digits, underscores or hyphens.")
      if (name %in% movement_remotes()$name) stop("That connection already exists. Choose a new name.")
      state$config <- NULL; state$config_name <- name
      start(list(c("config", "create", name, "onedrive", "--non-interactive")), "Starting Microsoft connection\u2026", config_done, TRUE)
    }) })
    shiny::observeEvent(input$reconnect_library, { access_require("admin", touch = TRUE); protect({
      idle()
      require_fields("reconnecting a remote library", list("New remote connection name" = input$connection_name))
      name <- trimws(input$connection_name)
      remotes <- movement_remotes()
      if (!name %in% remotes$name || !identical(remotes$type[match(name, remotes$name)], "onedrive"))
        stop("Enter the name of an existing SharePoint / OneDrive connection.")
      state$config <- NULL; state$config_name <- name
      start(list(c("config", "update", name, "--non-interactive")), "Resuming Microsoft connection\u2026", config_done, TRUE)
    }) })
    output$config_question <- shiny::renderUI({
      access_require("admin")
      question <- state$config
      if (is.null(question)) return(NULL)
      option <- question$Option
      examples <- option$Examples
      choices <- if (length(examples)) stats::setNames(vapply(examples, function(x) as.character(x$Value), character(1)),
                         vapply(examples, function(x) paste(x$Value, x$Help, sep = " \u2014 "), character(1))) else character()
      default <- if (is.null(option$Default)) "" else if (is.logical(option$Default)) tolower(as.character(option$Default)) else as.character(option$Default)
      answer_label <- shiny::tagList(option$Name, " ",
        flow_requirement_marker(if (isTRUE(option$Required)) "Mandatory" else "Optional"))
      shiny::tagList(
        shiny::p(question$Error),
        shiny::tags$pre(option$Help),
        if (isTRUE(option$Exclusive) && length(choices)) shiny::selectInput(session$ns("answer"), answer_label, choices, selected = default)
        else if (isTRUE(option$IsPassword) || isTRUE(option$Sensitive)) shiny::passwordInput(session$ns("answer"), answer_label)
        else shiny::tagList(if (length(choices)) shiny::tags$pre(paste(names(choices), collapse = "\n")), shiny::textInput(session$ns("answer"), answer_label, value = default)),
        shiny::actionButton(session$ns("answer_config"), "Continue connection")
      )
    })
    shiny::observeEvent(input$answer_config, { access_require("admin", touch = TRUE); protect({
      idle(); shiny::req(state$config)
      answer <- input$answer
      if (isTRUE(state$config$Option$Required)) require_fields("this connection step", list("Connection answer" = answer))
      start(list(c("config", "update", state$config_name, "--non-interactive", "--continue", "--state", state$config$State, paste0("--result=", answer))),
            "Connecting\u2026 complete browser sign-in if prompted.", config_done, TRUE)
    }) })
    routine_ids <- function() vapply(state$routines, `[[`, character(1), "id")
    routine_proxy <- DT::dataTableProxy("routine_components", session = session)
    clear_routine_selection <- function() {
      state$selected_routine_ids <- character()
      DT::selectRows(routine_proxy, NULL)
    }
    selected_routines <- function() {
      selected <- state$routines[routine_ids() %in% state$selected_routine_ids]
      if (!length(selected)) stop("Select at least one saved flow.")
      selected
    }
    show_profile <- function(profile = NULL) {
      state$editing_id <- profile$id %||% ""
      shiny::updateSelectInput(session, "routine", selected = state$editing_id)
      shiny::updateTextInput(session, "source", value = profile$src %||% "")
      shiny::updateTextInput(session, "destination", value = profile$dest %||% "")
      shiny::updateTextInput(session, "routine_name", value = profile$name %||% "")
      shiny::updateTextAreaInput(session, "excludes", value = paste(profile$excludes %||% "**/.git/**", collapse = "\n"))
      shiny::updateSelectInput(session, "mode", selected = profile$mode %||% "copy")
      shiny::updateSelectInput(session, "source_location", selected = "")
      shiny::updateSelectInput(session, "destination_location", selected = "")
      state$preview <- NULL
    }
    persist_routines <- function(profiles) {
      save_settings(profiles, routines_file)
      state$routines <- profiles
      clear_routine_selection()
      state$preview <- NULL
    }
    shiny::observe({
      access_require("admin")
      choices <- stats::setNames(routine_ids(), vapply(state$routines, `[[`, character(1), "name"))
      shiny::updateSelectInput(session, "routine", choices = c("New flow" = "", choices), selected = state$editing_id)
    })
    output$routine_help <- shiny::renderUI({
      access_require("admin")
      shiny::p(if (length(state$routines)) "Select the flows you want to run. Each flow has its own saved source, destination and operation."
        else "No saved flows yet. Open Design flow to create a flow or import syncs.json; start in Define connections if you need to define connections.")
    })
    output$routine_components <- DT::renderDT({
      access_require("admin")
      profiles <- state$routines
      rows <- data.frame(
        Flow = vapply(profiles, `[[`, character(1), "name"),
        Source = vapply(profiles, `[[`, character(1), "src"),
        Destination = vapply(profiles, `[[`, character(1), "dest"),
        Operation = vapply(profiles, function(x) if (x$mode == "sync") "Mirror sync (delete extra destination files)" else "Copy / update", character(1)),
        Exclusions = vapply(profiles, function(x) paste(x$excludes, collapse = "; "), character(1))
      )
      names(rows)[names(rows) == "Source"] <- "Flow from \u2014 source"
      names(rows)[names(rows) == "Destination"] <- "Flow to \u2014 destination"
      datapond_datatable(rows, rownames = FALSE, selection = "multiple", options = list(pageLength = 8, scrollX = TRUE, order = list()))
    })
    shiny::observeEvent(input$routine_components_rows_selected, {
      access_require("admin", touch = TRUE)
      rows <- input$routine_components_rows_selected
      rows <- rows[!is.na(rows) & rows >= 1L & rows <= length(state$routines)]
      state$selected_routine_ids <- routine_ids()[rows]
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$select_all_routines, {
      access_require("admin", touch = TRUE)
      state$selected_routine_ids <- routine_ids()
      DT::selectRows(routine_proxy, seq_along(state$routines))
    })
    shiny::observeEvent(input$clear_routine_selection, { access_require("admin", touch = TRUE); clear_routine_selection() })
    output$routine_selection <- shiny::renderText({ access_require("admin"); paste(length(state$selected_routine_ids), "of", length(state$routines), "flows selected.") })
    shiny::observeEvent(input$import, { access_require("admin", touch = TRUE); protect({
      imported <- movement_read_profiles(input$import$datapath)
      persist_routines(c(state$routines, imported))
      state$status <- paste(length(imported), "flows imported. Select rows to preview and run only those flows.")
    }) })
    output$export_routines <- shiny::downloadHandler(
      filename = function() { access_require("download"); "syncs.json" },
      content = function(file) { access_require("download"); movement_json_write(list(syncs = lapply(state$routines, function(x) x[setdiff(names(x), "id")])), file) }
    )
    shiny::observeEvent(input$routine, {
      access_require("admin", touch = TRUE)
      if (identical(input$routine, state$editing_id)) return()
      index <- match(input$routine, routine_ids())
      if (!nzchar(input$routine)) show_profile()
      else if (!is.na(index)) show_profile(state$routines[[index]])
    })
    shiny::observeEvent(input$new_routine, { access_require("admin", touch = TRUE); show_profile() })
    shiny::observeEvent(input$edit_routine, { access_require("admin", touch = TRUE); protect({
      profiles <- selected_routines()
      if (length(profiles) != 1L) stop("Select one flow to edit.")
      show_profile(profiles[[1]])
      shiny::updateTabsetPanel(session, "flow_tabs", selected = "design")
    }) })
    save_profile <- function(duplicate = FALSE) {
      require_fields("saving a flow", list("Flow name" = input$routine_name, "Flow from \u2014 source" = input$source,
        "Flow to \u2014 destination" = input$destination, "Operation" = input$mode))
      profile <- movement_normalise_profile(list(
        id = if (duplicate || !nzchar(state$editing_id)) new_uuid() else state$editing_id,
        name = input$routine_name, src = input$source, dest = input$destination,
        mode = input$mode %||% "copy", excludes = strsplit(input$excludes %||% "", "\n")[[1]]
      ))
      others <- state$routines[routine_ids() != profile$id]
      names <- vapply(others, `[[`, character(1), "name")
      if (duplicate) {
        base <- paste0(profile$name, " (copy)"); profile$name <- base; suffix <- 2L
        while (profile$name %in% names) {
          profile$name <- paste(base, suffix); suffix <- suffix + 1L
        }
      } else if (profile$name %in% names) stop("Another flow already has that name. Choose a different name.")
      profiles <- state$routines
      index <- match(profile$id, routine_ids())
      if (is.na(index)) profiles <- c(profiles, list(profile)) else profiles[[index]] <- profile
      persist_routines(profiles)
      show_profile(profile)
      state$status <- if (duplicate) "Flow duplicated. Edit its settings and save." else "Flow saved."
    }
    shiny::observeEvent(input$save_routine, { access_require("admin", touch = TRUE); protect(save_profile()) })
    shiny::observeEvent(input$duplicate_routine, { access_require("admin", touch = TRUE); protect(save_profile(TRUE)) })
    shiny::observeEvent(input$delete_routines, { access_require("admin", touch = TRUE); protect({
      profiles <- selected_routines()
      state$deleting_routine_ids <- vapply(profiles, `[[`, character(1), "id")
      shiny::showModal(shiny::modalDialog(
        title = "Remove saved flows?",
        shiny::p("This removes the saved settings only. Source and destination files are kept."),
        shiny::tags$ul(lapply(profiles, function(x) shiny::tags$li(x$name))),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(session$ns("confirm_delete_routines"), "Remove flows"))
      ))
    }) })
    shiny::observeEvent(input$confirm_delete_routines, { access_require("admin", touch = TRUE); protect({
      removed <- state$deleting_routine_ids
      if (!length(removed)) return()
      persist_routines(state$routines[!routine_ids() %in% removed])
      if (state$editing_id %in% removed) show_profile()
      state$deleting_routine_ids <- character()
      shiny::removeModal()
      state$status <- "Saved flows removed. Files were kept."
    }) })
    shiny::observeEvent(input$selected_dataset, { access_require("admin", touch = TRUE); protect({
      path <- dataset_path()
      if (is.null(path) || !nzchar(path)) stop("Select a dataset in the pond first.")
      shiny::updateTextInput(session, "source", value = path)
    }) })
    shiny::observeEvent(input$swap, {
      access_require("admin", touch = TRUE)
      source <- input$source
      shiny::updateTextInput(session, "source", value = input$destination)
      shiny::updateTextInput(session, "destination", value = source)
    })
    specification <- function(kind) {
      spec <- if (kind == "publish") list(kind = kind, pond = pond(), destination = input$publish_destination, include_data = isTRUE(input$publish_data))
      else if (kind == "routines") list(kind = kind, profiles = selected_routines())
      else list(kind = kind, source = input$source, destination = input$destination, mode = input$mode, excludes = strsplit(input$excludes %||% "", "\n")[[1]])
      spec$speeds <- movement_speed_read(state_directory)
      spec
    }
    shiny::observe({
      access_require("admin")
      previewed <- state$preview
      if (is.null(previewed) || isTRUE(previewed$stale)) return()
      state$speed_tick
      current <- tryCatch(specification(previewed$spec$kind), error = function(e) NULL)
      if (!identical(current, previewed$spec)) {
        previewed$stale <- TRUE; state$preview <- previewed
      }
    })
    output$preview_status <- shiny::renderText({
      access_require("admin")
      shiny::invalidateLater(30000, session)
      previewed <- state$preview
      if (is.null(previewed)) return("No completed preview. Preview saved flows or a pond publication here, or use Preview flow in Design flow.")
      age <- as.numeric(difftime(Sys.time(), previewed$completed, units = "mins"))
      paste("Preview completed", format(previewed$completed, "%Y-%m-%d %H:%M:%S %Z"),
        if (isTRUE(previewed$stale)) "- settings changed; preview again."
        else if (age >= 15) "- more than 15 minutes old; preview again before running."
        else "- review the plan and log before running.")
    })
    preview <- function(kind) {
      idle()
      if (kind == "transfer") require_fields("previewing a flow", list("Flow from \u2014 source" = input$source,
        "Flow to \u2014 destination" = input$destination, "Operation" = input$mode))
      if (kind == "publish") save_publication()
      spec <- specification(kind)
      saved <- state$preview
      if (!isTRUE(input$fresh_preview) && !is.null(saved) && !isTRUE(saved$stale) &&
          identical(saved$spec, spec) && as.numeric(difftime(Sys.time(), saved$completed, units = "mins")) < 15) {
        state$status <- "Using the recent matching preview; no new requests were sent. Select Request a fresh preview if files have changed."
        state$log <- saved$log
        state$report <- saved$report
        shiny::updateTabsetPanel(session, "flow_tabs", selected = "run")
        return(invisible(NULL))
      }
      if (!is.null(state$preview$plan$snapshot)) unlink(state$preview$plan$snapshot$directory, recursive = TRUE)
      state$preview <- NULL
      shiny::updateCheckboxInput(session, "delete_ack", value = FALSE)
      shiny::updateCheckboxInput(session, "fresh_preview", value = FALSE)
      if (kind == "publish") {
        plan <- movement_publish_plan(spec$pond, spec$destination, spec$include_data)
        steps <- plan$steps
      } else if (kind == "routines") {
        plan <- movement_routine_plan(spec$profiles)
        steps <- plan$steps
      } else {
        plan <- movement_transfer_plan(spec$source, spec$destination, spec$mode, spec$excludes)
        steps <- list(plan$args)
      }
      start(steps, "Previewing \u2014 no destination changes\u2026", function() {
        state$preview <- list(spec = spec, plan = plan, steps = state$job$steps, completed = Sys.time(), stale = FALSE, log = state$log, report = state$report)
        state$status <- if (kind == "routines") paste("Preview complete for", length(steps), "selected flows. Review the plan and log, then run.") else
          "Preview complete. Review the log, then run the previewed flow."
      }, labels = plan$summary$Component, profiles = spec$speeds,
        failure = function() { if (!is.null(plan$snapshot)) unlink(plan$snapshot$directory, recursive = TRUE) })
      shiny::updateTabsetPanel(session, "flow_tabs", selected = "run")
    }
    output$transfer_plan <- DT::renderDT({
      access_require("admin")
      previewed <- state$preview
      if (is.null(previewed)) return(NULL)
      rows <- previewed$plan$summary
      if (is.null(rows)) rows <- data.frame(
        Operation = if (previewed$spec$kind == "publish") "Publish pond" else previewed$spec$mode,
        Source = previewed$spec$source %||% previewed$spec$pond,
        Destination = previewed$spec$destination
      )
      names(rows)[names(rows) == "Component"] <- "Flow"
      names(rows)[names(rows) == "Source"] <- "Flow from \u2014 source"
      names(rows)[names(rows) == "Destination"] <- "Flow to \u2014 destination"
      rows$Operation[rows$Operation == "copy"] <- "Copy / update"
      rows$Operation[rows$Operation == "sync"] <- "Mirror sync (delete extra destination files)"
      datapond_datatable(rows, rownames = FALSE, selection = "none", options = list(dom = "t", scrollX = TRUE, ordering = FALSE))
    })
    shiny::observeEvent(input$preview_transfer, { access_require("admin", touch = TRUE); protect(preview("transfer")) })
    shiny::observeEvent(input$preview_routines, { access_require("admin", touch = TRUE); protect(preview("routines")) })
    shiny::observeEvent(input$preview_publish, { access_require("admin", touch = TRUE); protect(preview("publish")) })
    shiny::observeEvent(input$run, { access_require("admin", touch = TRUE); protect({
      idle(); previewed <- state$preview
      if (is.null(previewed)) stop("Complete a preview first.")
      if (isTRUE(previewed$stale) || !identical(specification(previewed$spec$kind), previewed$spec)) stop("Settings changed. Preview the flow again.")
      if (as.numeric(difftime(Sys.time(), previewed$completed, units = "mins")) >= 15)
        stop("The preview is more than 15 minutes old. Preview the flow again.")
      mirror <- preview_needs_mirror_ack(previewed)
      if (mirror && !isTRUE(input$delete_ack)) stop("Acknowledge destination deletions before running mirror sync.")
      steps <- lapply(previewed$steps, function(args) args[args != "--dry-run"])
      state$preview <- NULL
      start(steps, "Transferring\u2026", function() {
        state$status <- if (previewed$spec$kind == "routines") paste(length(steps), "selected", if (length(steps) == 1L) "flow" else "flows", "completed successfully.") else
          "Flow completed successfully."
        if (identical(previewed$spec$kind, "publish")) unlink(previewed$plan$snapshot$directory, recursive = TRUE)
      }, labels = previewed$plan$summary$Component, profiles = previewed$spec$speeds,
        failure = function() { if (!is.null(previewed$plan$snapshot)) unlink(previewed$plan$snapshot$directory, recursive = TRUE) },
        not_after = previewed$completed + 15 * 60)
    }) })
    roots <- data_pond_folder_roots()
    shinyFiles::shinyDirChoose(access_directory_input(input), "local_folder", roots = roots, allowDirCreate = FALSE, session = session)
    shiny::observeEvent(input$local_folder, {
      access_require("admin", touch = TRUE)
      selected <- shinyFiles::parseDirPath(roots, input$local_folder)
      if (length(selected)) shiny::updateTextInput(session, "location_path", value = as.character(selected))
    })
    shinyFiles::shinyDirChoose(access_directory_input(input), "cache_folder", roots = roots, allowDirCreate = FALSE, session = session)
    shiny::observeEvent(input$cache_folder, {
      access_require("admin", touch = TRUE)
      selected <- shinyFiles::parseDirPath(roots, input$cache_folder)
      if (length(selected)) shiny::updateTextInput(session, "cache", value = as.character(selected))
    })
    shiny::observe({
      access_require("admin")
      choices <- stats::setNames(as.character(seq_along(state$ponds)), vapply(state$ponds, function(x) x$name, character(1)))
      cache_paths <- vapply(state$ponds, function(x) x$cache, character(1))
      selected_cache <- match(state$cache, cache_paths)
      selected_cache <- if (!length(selected_cache) || is.na(selected_cache)) "" else as.character(selected_cache)
      shiny::updateSelectInput(session, "cached_pond", choices = c("New connection" = "", choices), selected = selected_cache)
    })
    show_cached_pond <- function(saved) {
      shiny::updateTextInput(session, "remote", value = saved$remote)
      shiny::updateTextInput(session, "cache", value = saved$cache)
      shiny::updateTextInput(session, "remote_name", value = saved$name)
      state$cache <- saved$cache; state$cache_tick <- state$cache_tick + 1L
    }
    if (!is.null(open_pond_request)) shiny::observeEvent(open_pond_request(), { access_require("admin", touch = TRUE); protect({
      request <- open_pond_request()
      index <- match(request$cache, vapply(state$ponds, function(x) x$cache, character(1)))
      if (is.na(index)) stop("This remote pond is no longer saved in Flow.")
      show_cached_pond(state$ponds[[index]])
      shiny::updateSelectInput(session, "cached_pond", selected = as.character(index))
      shiny::updateTabsetPanel(session, "flow_tabs", selected = "remote")
      DT::selectRows(DT::dataTableProxy("catalogue", session = session), NULL)
      state$status <- "Showing cached pond metadata. Select a dataset to inspect it or download its files."
    }) })
    shiny::observeEvent(input$cached_pond, { access_require("admin", touch = TRUE); protect({
      if (!nzchar(input$cached_pond)) { state$cache <- NULL; return() }
      show_cached_pond(state$ponds[[as.integer(input$cached_pond)]])
    }) })
    shiny::observeEvent(input$refresh_cache, { access_require("admin", touch = TRUE); protect({
      idle()
      require_fields("connecting or refreshing a remote pond", list("Remote pond folder" = input$remote, "Local pond cache" = input$cache))
      prepared <- movement_cache_prepare(input$remote, input$cache)
      nickname <- trimws(input$remote_name %||% "")
      name <- if (nzchar(nickname)) nickname else prepared$remote
      discard <- function() {
        current <- tryCatch(movement_cache_current(prepared$cache), error = function(e) NULL)
        if (is.null(current) || !identical(current$directory, prepared$directory)) unlink(prepared$directory, recursive = TRUE)
      }
      start(prepared$steps, "Fetching catalogue manifest\u2026", function() {
        fetched <- movement_cache_fetch_plan(prepared)
        start(fetched$steps, "Downloading metadata only\u2026", function() {
          movement_cache_commit(prepared, fetched$manifest)
          unlink(fetched$list_file)
          state$cache <- prepared$cache; state$cache_tick <- state$cache_tick + 1L
          state$ponds <- c(Filter(function(x) x$cache != prepared$cache, state$ponds), list(list(name = name, remote = prepared$remote, cache = prepared$cache)))
          save_settings(state$ponds, ponds_file)
          state$status <- "Metadata ready. Select a dataset to inspect it or download its files."
        }, failure = function() { discard(); unlink(fetched$list_file) })
      }, failure = discard)
    }) })
    inventory <- shiny::reactive({
      state$cache_tick
      if (is.null(state$cache)) return(data.frame())
      tryCatch(movement_cache_inventory(state$cache), error = function(e) data.frame())
    })
    output$cache_status <- shiny::renderText({
      access_require("admin")
      state$cache_tick
      if (is.null(state$cache)) return("No remote pond selected.")
      current <- movement_cache_current(state$cache)
      if (is.null(current)) return("No verified catalogue cached yet.")
      paste("Read-only \u2022", current$state$remote, "\u2022 Metadata refreshed", current$state$refreshed, "\u2022 Local pond cache:", state$cache)
    })
    output$catalogue <- DT::renderDT({
      access_require("admin")
      rows <- inventory()
      if (!nrow(rows)) return(datapond_datatable(data.frame(Message = "Connect a pond or select an offline cache."), rownames = FALSE))
      datapond_datatable(rows[, c("title", "citation_date", "stage", "cached"), drop = FALSE], rownames = FALSE, selection = "single", options = list(pageLength = 10))
    })
    selected <- shiny::reactive({
      rows <- inventory(); index <- input$catalogue_rows_selected
      if (length(index) != 1L || index > nrow(rows)) return(NULL)
      rows[index, , drop = FALSE]
    })
    output$metadata_detail <- shiny::renderUI({
      access_require("admin")
      row <- selected(); if (is.null(row)) return(NULL)
      current <- movement_cache_current(state$cache)
      conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(current$directory), flags = RSQLite::SQLITE_RO, synchronous = NULL)
      on.exit(DBI::dbDisconnect(conn))
      record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(row$id))
      visible <- names(record)[vapply(record, function(x) length(x) && !is.na(x[1]) && nzchar(as.character(x[1])), logical(1))]
      sections <- movement_cache_details(state$cache, row$id)
      shiny::tagList(shiny::h4(row$title), shiny::tags$details(open = "open", shiny::tags$summary("Metadata record (read-only)"),
        shiny::tags$dl(lapply(visible, function(field) shiny::tagList(shiny::tags$dt(gsub("_", " ", field)), shiny::tags$dd(as.character(record[[field]][1])))))),
        lapply(names(sections), function(name) {
          rows <- sections[[name]]
          shiny::tags$details(
            shiny::tags$summary(gsub("_", " ", sub("_tbl$", "", name))),
            if (nrow(rows) > 200L) shiny::p("Showing the first 200 records; the complete metadata is retained in the cached catalogue."),
            datapond_datatable(rows[seq_len(min(200L, nrow(rows))), , drop = FALSE],
              rownames = FALSE, selection = "none",
              options = list(pageLength = 10, scrollX = TRUE))
          )
        }))
    })
    shiny::observeEvent(input$download, { access_require("admin", touch = TRUE); protect({
      idle(); row <- selected()
      if (is.null(row)) stop("Select a dataset first.")
      plan <- movement_download_plan(state$cache, row$id)
      # Mark previous receipts stale before starting, including cancelled refreshes.
      if (file.exists(plan$receipt)) unlink(plan$receipt)
      start(plan$steps, paste("Downloading", row$title), function() {
        movement_json_write(list(path = plan$relative, downloaded = format(Sys.time(), tz = "UTC", usetz = TRUE)), plan$receipt)
        state$download_path <- plan$destination; state$cache_tick <- state$cache_tick + 1L
        state$status <- "Dataset downloaded. Remote metadata remains read-only."
      })
    }) })
    shiny::observeEvent(input$evict, { access_require("admin", touch = TRUE); protect({
      idle(); row <- selected()
      if (is.null(row)) stop("Select a dataset first.")
      # Capture selection so changing the table cannot change the confirmed target.
      state$eviction <- list(cache = state$cache, id = row$id)
      shiny::showModal(shiny::modalDialog(
        title = "Remove local cached files?",
        paste("Remove downloaded files for", row$title, "from this cache. The remote dataset and cached metadata remain available."),
        footer = shiny::tagList(shiny::modalButton("Keep files"), shiny::actionButton(session$ns("confirm_evict"), "Remove local download"))
      ))
    }) })
    shiny::observeEvent(input$confirm_evict, { access_require("admin", touch = TRUE); protect({
      idle(); shiny::req(state$eviction)
      movement_evict_download(state$eviction$cache, state$eviction$id)
      state$eviction <- NULL; state$download_path <- ""; state$cache_tick <- state$cache_tick + 1L
      shiny::removeModal(); state$status <- "Local download removed. Metadata and remote files were retained."
    }) })
    output$download_path <- shiny::renderText({ access_require("admin"); if (nzchar(state$download_path)) paste("Local dataset:", state$download_path) else "" })
    invisible(state)
  })
}
