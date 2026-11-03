# Metadata sync is explicit. Imports go through the normal dataset form/save
# workflow; exports modify only the cover and a marked metadata section.
data_dev_sync_fields <- function() c(title = "Dataset title", version = "Dataset version",
  abstract = "Abstract", purpose = "Purpose", uuid = "Dataset UUID")

data_dev_sync_values <- function(record) {
  stats::setNames(lapply(names(data_dev_sync_fields()), function(key)
    metadata_text(record[[key]], "")), names(data_dev_sync_fields()))
}

data_dev_sync_document <- function(path, values = NULL, output = NULL, inspect = FALSE) {
  python <- document_python_path()
  if (!nzchar(python)) stop("Python 3.9+ is required to sync development reports.")
  script <- system.file("templates", "sync_data_dev.py", package = "datapond", mustWork = TRUE)
  mode <- if (!is.null(values)) "write" else if (isTRUE(inspect)) "inspect" else "read"
  args <- c(script, mode, path)
  if (!is.null(values)) {
    json <- tempfile(fileext = ".json"); on.exit(unlink(json), add = TRUE)
    jsonlite::write_json(values, json, auto_unbox = TRUE)
    args <- c(args, output, json)
  }
  run <- processx::run(python, args, error_on_status = FALSE, timeout = 30000)
  if (run$status != 0L) stop(trimws(run$stderr), call. = FALSE)
  if (is.null(values)) jsonlite::fromJSON(run$stdout, simplifyVector = FALSE) else invisible(output)
}

data_dev_sync_context <- function(conn, dataset_id, base_path, user_id, document) {
  release_require_user(conn, user_id)
  release_open_review(conn, dataset_id)
  record <- release_entry(conn, dataset_id)
  if (isTRUE(as.integer(record$folder_locked[1]) == 1L))
    stop("Unlock the working dataset before syncing a report.")
  root <- release_working_root(base_path, record)
  if (length(document) != 1L || is.na(document) ||
      !grepl("^data_dev(?:_[A-Za-z0-9_-]+)?\\.(qmd|docx)$", document, perl = TRUE))
    stop("Choose a data_dev Word or Quarto report in the dataset root.")
  path <- file.path(root, document)
  link <- Sys.readlink(path)
  if (!file.exists(path) || dir.exists(path) || (!is.na(link) && nzchar(link)) ||
      !identical(dirname(normalizePath(path, winslash = "/")), normalizePath(root, winslash = "/")))
    stop("The report is missing or is a symbolic link. Choose a regular file in the dataset root.")
  list(record = record, root = root, path = path)
}

preview_data_dev_sync <- function(dataset_id, base_path, user_id, document,
    direction = "export", fields = c("title", "version", "abstract", "purpose"), form = NULL) {
  if (!direction %in% c("export", "import")) stop("Choose a sync direction.")
  allowed <- setdiff(names(data_dev_sync_fields()), if (direction == "import") "uuid" else character())
  if (!length(fields) || any(!fields %in% allowed)) stop("Select the fields to sync. UUID cannot be imported.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ctx <- data_dev_sync_context(conn, dataset_id, base_path, user_id, document)
  saved <- data_dev_sync_values(ctx$record)
  hash <- unname(tools::md5sum(ctx$path))
  inspected <- data_dev_sync_document(ctx$path, inspect = TRUE)
  report <- inspected$values
  if (!identical(hash, unname(tools::md5sum(ctx$path)))) stop("The report changed. Preview again.")
  if (direction == "import" && !length(report))
    stop("This report has no DataPond metadata section. Export metadata once, then edit that section to import changes.")
  if (direction == "import" && nzchar(metadata_text(report$uuid, "")) &&
      !identical(report$uuid, saved$uuid)) stop("The report belongs to a different dataset UUID.")
  source <- if (direction == "export") saved else report
  target <- if (direction == "export") report else data_dev_sync_values(form %||% ctx$record)
  fields <- fields[vapply(source[fields], function(x) nzchar(trimws(metadata_text(x, ""))), logical(1))]
  if (!length(fields)) stop("The selected source fields are empty; nothing will be copied.")
  values <- source[fields]
  if (direction == "import" && "version" %in% fields &&
      !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", values$version))
    stop("Dataset version must use major.minor.patch, for example 1.2.0.")
  if (direction == "import" && "version" %in% fields &&
      any(as.numeric(strsplit(values$version, ".", fixed = TRUE)[[1]]) > c(20, 99, 99)))
    stop("Dataset version exceeds the form limits: major 0\u201320, minor and patch 0\u201399.")
  # UUID identifies the export even when it is not one of the selected fields.
  if (direction == "export") values$uuid <- saved$uuid
  warnings <- vapply(Filter(function(w) w$field %in% fields, inspected$warnings),
    function(w) paste0(data_dev_sync_fields()[[w$field]], ": ",
      if (direction == "export") "Existing report formatting: " else "", w$message,
      if (direction == "export") " Export will replace this selected field with saved metadata." else ""), character(1))
  list(dataset_id = dataset_id, base_path = base_path, document = document,
    direction = direction, hash = hash, saved = saved, target = target,
    values = values, warnings = unname(warnings), changes = data.frame(Field = unname(data_dev_sync_fields()[fields]),
      Current = vapply(fields, function(k) metadata_text(target[[k]], ""), character(1)),
      Proposed = vapply(fields, function(k) source[[k]], character(1)), stringsAsFactors = FALSE))
}

apply_data_dev_sync <- function(preview, user_id, form = NULL) {
  conn <- get_conn(preview$base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  lock <- release_lock(preview$base_path, preview$dataset_id)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  ctx <- data_dev_sync_context(conn, preview$dataset_id, preview$base_path, user_id, preview$document)
  if (!identical(preview$saved, data_dev_sync_values(ctx$record)) ||
      !identical(preview$hash, unname(tools::md5sum(ctx$path))))
    stop("The metadata or report changed since preview. Preview again.")
  if (preview$direction == "import") {
    if (!is.null(form) && !identical(preview$target, data_dev_sync_values(form)))
      stop("The dataset form changed since preview. Preview again.")
    return(list(values = preview$values))
  }
  stage <- tempfile(".data-dev-sync-", tmpdir = ctx$root, fileext = paste0(".", tools::file_ext(ctx$path)))
  on.exit(unlink(stage), add = TRUE)
  data_dev_sync_document(ctx$path, preview$values, stage)
  # Verify both the generated section and the original just before replacing.
  written <- data_dev_sync_document(stage)
  if (!identical(written[names(preview$values)], preview$values)) stop("Report verification failed; the original is unchanged.")
  if (!identical(preview$hash, unname(tools::md5sum(ctx$path)))) stop("The report changed while syncing. Preview again.")
  backup <- tempfile(paste0(".", preview$document, "-"), tmpdir = ctx$root, fileext = ".bak")
  if (!file.copy(ctx$path, backup, overwrite = FALSE, copy.mode = TRUE)) stop("Could not back up the report.")
  Sys.chmod(stage, mode = file.info(ctx$path)$mode)
  if (!file.rename(stage, ctx$path)) stop("Could not replace the report. Backup: ", backup)
  list(path = ctx$path, backup = backup)
}

mod_data_dev_sync_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("open"), "Sync development report", icon = shiny::icon("arrows-rotate"),
    class = "datapond-requires-active-user")
}

mod_data_dev_sync_server <- function(id, state, get_form, import_values) {
  shiny::moduleServer(id, function(input, output, session) {
    pending <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal("")
    context <- shiny::reactiveVal(NULL)
    ns <- session$ns
    output$result <- shiny::renderText({ access_require("read"); result() })
    output$format_warnings <- shiny::renderUI({
      access_require("read")
      p <- pending()
      if (is.null(p) || !length(p$warnings)) return(NULL)
      shiny::div(class = "alert alert-warning", role = "alert",
        shiny::tags$strong("Check scientific notation and formatting before applying"),
        shiny::tags$ul(lapply(p$warnings, shiny::tags$li)),
        shiny::p("Review the proposed text against the report. Correct the source or deselect affected fields if needed."))
    })
    output$preview <- shiny::renderTable({ access_require("read"); p <- pending(); if (!is.null(p)) p$changes },
      striped = TRUE, bordered = TRUE, spacing = "s")
    shiny::observeEvent(input$open, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(state, "syncing a development report")) return()
      tryCatch({
        conn <- get_conn(state$data_pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
        root <- release_working_root(state$data_pond, release_entry(conn, state$current_dataset_id))
        files <- list.files(root, pattern = "^data_dev(_[A-Za-z0-9_-]+)?\\.(qmd|docx)$")
        if (!length(files)) stop("Add a development template under Lineage first, or place a data_dev QMD/DOCX report in the dataset root.")
        pending(NULL); result("")
        context(list(id = state$current_dataset_id, pond = state$data_pond))
        shiny::showModal(shiny::modalDialog(title = "Sync development report", size = "l",
          shiny::p("Export uses saved dataset metadata. Save dataset first to include your latest edits. Import loads selected values into the form; review them and use Save dataset to record the change."),
          shiny::selectInput(ns("document"), "Report", choices = files),
          shiny::radioButtons(ns("direction"), "Copy direction", choices = c(
            "Saved dataset metadata to report" = "export", "Report metadata section to dataset form" = "import")),
          shiny::checkboxGroupInput(ns("fields"), "Fields to copy", choices = stats::setNames(
            c("title", "version", "abstract", "purpose"), c("Title", "Dataset version", "Abstract", "Purpose")),
            selected = c("title", "version", "abstract", "purpose"), inline = TRUE),
          shiny::p(class = "help-block", "Empty source fields are skipped. Exports update the report title, cover details and marked Dataset metadata section, and retain a .bak copy. Edit that section for imports. UUID identifies the dataset and is never imported. Report version and methods are kept."),
          shiny::p(class = "help-block", "Word superscripts and subscripts convert to Unicode where supported (e.g. 10\u207b\u2076, m\u00b2, CO\u2082). Other characters use explicit ^{...} or _{...} notation with a warning. Quarto math and markup stay literal in metadata. Unsupported Word equations and objects receive visible placeholders and warnings."),
          shiny::actionButton(ns("preview_sync"), "Preview changes"),
          shiny::uiOutput(ns("format_warnings")),
          shiny::div(style = "overflow:auto; max-height:40vh; white-space:pre-wrap; overflow-wrap:anywhere;",
            shiny::tableOutput(ns("preview"))), shiny::textOutput(ns("result")),
          footer = shiny::tagList(shiny::modalButton("Close"),
            shiny::actionButton(ns("apply_sync"), "Apply preview", class = "btn-primary"))))
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
    shiny::observeEvent(list(input$document, input$direction, input$fields), {
      access_require("read")
      pending(NULL); result("")
    }, ignoreInit = TRUE)
    check_context <- function() {
      if (!identical(context(), list(id = state$current_dataset_id, pond = state$data_pond)))
        stop("The selected dataset changed. Close and reopen Sync development report.")
    }
    shiny::observeEvent(input$preview_sync, {
      access_require("edit", touch = TRUE)
      pending(NULL); result("")
      tryCatch({
        check_context()
        pending(preview_data_dev_sync(state$current_dataset_id, state$data_pond, state$active_user_id,
          input$document, input$direction, input$fields, get_form()))
      }, error = function(e) result(conditionMessage(e)))
    })
    shiny::observeEvent(input$apply_sync, {
      access_require("edit", touch = TRUE)
      tryCatch({
        check_context()
        p <- pending()
        if (is.null(p)) stop("Preview the changes before applying them.")
        if (!identical(p$document, input$document) || !identical(p$direction, input$direction))
          stop("The sync options changed. Preview again.")
        applied <- apply_data_dev_sync(p, state$active_user_id, get_form())
        pending(NULL)
        if (p$direction == "import") {
          import_values(applied$values)
          result("Loaded into the dataset form. Review the values, provide an update reason and use Save dataset.")
        } else {
          state$refresh_supporting_docs <- (state$refresh_supporting_docs %||% 0L) + 1L
          result(paste("Report updated. Backup:", applied$backup))
        }
      }, error = function(e) result(conditionMessage(e)))
    })
  })
}
