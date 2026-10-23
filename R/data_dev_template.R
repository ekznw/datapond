data_dev_template_names <- function(format = "docx", project = "") {
  if (length(format) != 1L || is.na(format) || !format %in% c("docx", "qmd"))
    stop("Choose Word or Quarto.", call. = FALSE)
  if (length(project) != 1L || is.na(project)) stop("Enter a project short name.", call. = FALSE)
  project <- trimws(project)
  if (nzchar(project) && !grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,59}$", project))
    stop("Use up to 60 letters, numbers, underscores or hyphens for the project short name.", call. = FALSE)
  stem <- paste0("data_dev", if (nzchar(project)) paste0("_", project))
  list(document = paste0(stem, ".", format), theme = paste0(stem, "_theme"))
}

add_dataset_data_dev_template <- function(dataset_id, base_path, user_id, format = "docx", project = "") {
  names <- data_dev_template_names(format, project)
  if (length(dataset_id) != 1L || is.na(dataset_id) || !metadata_present(base_path))
    stop("Select and save a dataset before adding a template.", call. = FALSE)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_require_user(conn, user_id)
  lock <- release_lock(base_path, dataset_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_open_review(conn, dataset_id)
  record <- release_entry(conn, dataset_id)
  if (metadata_text(record$lifecycle_status, "draft") != "draft")
    stop("Restore an archived dataset as a draft before adding a template.", call. = FALSE)
  if (isTRUE(as.integer(record$folder_locked[1]) == 1L))
    stop("Unlock the working dataset before adding a template.", call. = FALSE)
  root <- release_working_root(base_path, record)
  targets <- file.path(root, c(names$document, if (format == "qmd") names$theme))
  occupied <- function(path) {
    link <- Sys.readlink(path)
    file.exists(path) || dir.exists(path) || (!is.na(link) && nzchar(link))
  }
  check_targets <- function() {
    taken <- targets[vapply(targets, occupied, logical(1))]
    if (length(taken)) stop(paste("Already exists; nothing was replaced:", paste(basename(taken), collapse = ", ")),
      call. = FALSE)
  }
  check_targets()
  if (nrow(DBI::dbGetQuery(conn,
      "SELECT id FROM supporting_document_tbl WHERE dataset_id=? AND document_rel_path=?",
      params = list(as.integer(dataset_id), names$document))))
    stop("This document name is already registered. Choose another project short name.", call. = FALSE)
  template <- datapond_resource("data_dev_template")
  stage <- tempfile(".data-dev-template-", tmpdir = root)
  if (!dir.create(stage)) stop("Could not prepare the template folder.")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  if (!file.copy(file.path(template, paste0("data_dev.", format)), file.path(stage, names$document)))
    stop("Could not copy the bundled template.")
  if (format == "qmd") {
    if (!file.copy(file.path(template, "ekznw-theme"), stage, recursive = TRUE))
      stop("Could not copy the portable template styles.")
    if (!file.rename(file.path(stage, "ekznw-theme"), file.path(stage, names$theme)))
      stop("Could not prepare the portable template styles.")
    # A document-specific style directory cannot overwrite metadata.pdf's theme
    # or styles belonging to an existing development report.
    for (path in c(file.path(stage, names$document),
        file.path(stage, names$theme, c("ekznw.yml", "pdf-header.tex")))) {
      lines <- readLines(path, warn = FALSE)
      writeLines(gsub("ekznw-theme/", paste0(names$theme, "/"), lines, fixed = TRUE), path, useBytes = TRUE)
    }
  }
  published <- character()
  committed <- FALSE
  on.exit(if (!committed) unlink(published, recursive = TRUE), add = TRUE)
  # The lifecycle lock also excludes freeze/archive operations during insertion.
  latest <- release_entry(conn, dataset_id)
  if (isTRUE(as.integer(latest$folder_locked[1]) == 1L) ||
      !identical(release_working_root(base_path, latest), root))
    stop("The working dataset changed. Select it again before adding a template.", call. = FALSE)
  release_require_user(conn, user_id)
  release_open_review(conn, dataset_id)
  check_targets()
  DBI::dbWithTransaction(conn, {
    for (target in targets) {
      if (occupied(target) || !file.rename(file.path(stage, basename(target)), target))
        stop("The destination changed or the template could not be saved. Nothing was replaced.")
      published <- c(published, target)
    }
    person <- DBI::dbGetQuery(conn, "SELECT person_id FROM user_tbl WHERE id=?", params = list(user_id))$person_id[1]
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    DBI::dbExecute(conn, "INSERT INTO supporting_document_tbl
      (dataset_id, document_rel_path, document_name, document_dir, document_path,
       document_extension, document_type_auto_code, document_type_code, document_type_label,
       document_type_auto_text, document_type_user_set, include_in_lineage, include_in_metadata,
       notes, active, first_seen_at, last_seen_at, classified_at, classified_by_person_id)
      VALUES (?, ?, ?, '.', ?, ?, 'method_statement', 'method_statement', 'Method statement',
        'Dataset development template', 0, 0, 1, ?, 1, ?, ?, ?, ?)",
      params = list(as.integer(dataset_id), names$document, names$document, targets[1], format,
        "Created from EKZNW data_dev template 1.0.0. Complete the report before selecting it as authoritative lineage.",
        now, now, now, as.integer(person)))
  })
  committed <- TRUE
  invisible(targets[1])
}

mod_data_dev_template_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(
    class = "datapond-optional-details", style = "margin-top:12px;",
    shiny::tags$summary("Add data development template"),
    shiny::p(class = "help-block", "Start a methods and lineage report in the dataset folder. Keep the instructional notes while drafting and remove them when complete."),
    shiny::radioButtons(ns("format"), "Authoring format",
      choices = c("Microsoft Word \u2014 type directly into the report" = "docx",
        "Quarto \u2014 render to PDF, Word and HTML" = "qmd"), selected = "docx"),
    shiny::textInput(ns("project"), "Project short name (optional)", placeholder = "e.g. pisr or pu"),
    shiny::textOutput(ns("destination")),
    shiny::p(class = "help-block", "Use an unlocked draft in src/ or dev/. Existing files are never replaced. Quarto includes portable styles; adding either template requires no rendering software."),
    shiny::actionButton(ns("add"), "Add template to dataset folder", class = "datapond-requires-active-user"),
    shiny::textOutput(ns("result")),
    shiny::p(class = "help-block", "The report is registered under Supporting documents. After writing it, select it as the authoritative document and save your lineage summary. Word and Quarto copies do not synchronise automatically.")
  )
}

mod_data_dev_template_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal("")
    shiny::observeEvent(state$current_dataset_id, { access_require("read", touch = TRUE); result("") }, ignoreInit = TRUE)
    output$destination <- shiny::renderText({
      access_require("read")
      tryCatch({
        names <- data_dev_template_names(input$format %||% "docx", input$project %||% "")
        paste("Dataset root:", names$document,
          if (identical(input$format, "qmd")) paste("and", paste0(names$theme, "/")))
      }, error = function(e) conditionMessage(e))
    })
    output$result <- shiny::renderText({ access_require("read"); result() })
    shiny::observeEvent(input$add, {
      access_require("edit", touch = TRUE)
      if (!require_active_user_for_write(state, "adding a development template")) return()
      result("")
      tryCatch({
        path <- add_dataset_data_dev_template(state$current_dataset_id, state$data_pond,
          state$active_user_id, input$format %||% "docx", input$project %||% "")
        state$refresh_supporting_docs <- (state$refresh_supporting_docs %||% 0L) + 1L
        result(paste("Created:", path))
        shiny::showNotification("Template added and registered under Supporting documents.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
  })
}
