mod_public_edition_workflow_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Prepare a public edition"),
    shiny::actionButton(ns("make"), "Make public edition", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("update"), "Update this public edition", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("defaults"), "Public metadata defaults", class = "datapond-requires-active-user"),
    shiny::helpText("Choose content, review prefilled documentation, then create and open the draft. Updates reuse previous selections and preserve public edits. A recovery checkpoint keeps the previous draft."),
    shiny::verbatimTextOutput(ns("result")))
}

public_display_value <- function(value, payload = list()) {
  if (is.null(value)) return("(empty)")
  x <- public_decode(value)
  if (is.null(x)) return("(empty)")
  if (is.data.frame(x)) {
    for (column in names(x)) {
      if (grepl("person_id$", column) && !is.null(payload$persons_tbl)) {
        refs <- payload$persons_tbl; match <- match(x[[column]], refs$id)
        x[[column]] <- ifelse(is.na(match), x[[column]], paste(refs$name_first[match], refs$name_last[match]))
      }
      if (grepl("organisation_id$", column) && !is.null(payload$organisation_tbl)) {
        refs <- payload$organisation_tbl; match <- match(x[[column]], refs$id)
        x[[column]] <- ifelse(is.na(match), x[[column]], refs$organisation_name_full[match])
      }
    }
    x <- x[, !names(x) %in% c("license_template_id", "sort_order"), drop = FALSE]
    return(paste(apply(x, 1, function(row) paste(paste(gsub("_", " ", sub("_id$", "", names(x))), row, sep = ": "), collapse = "; ")), collapse = "\n"))
  }
  paste(x, collapse = ", ")
}

mod_public_edition_workflow_server <- function(id, state, source_release, on_changed = function() NULL, on_open = function(id) NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    choices <- shiny::reactiveVal(NULL); plan <- shiny::reactiveVal(NULL); params <- shiny::reactiveVal(NULL)
    workflow <- shiny::reactiveVal(NULL); step <- shiny::reactiveVal(0L); result <- shiny::reactiveVal("")
    cleanup <- function() { p <- shiny::isolate(plan()); if (!is.null(p)) unlink(p$stage, recursive = TRUE); plan(NULL) }
    session$onSessionEnded(cleanup)
    protect <- function(expr) tryCatch(force(expr), error = function(e) {
      result(conditionMessage(e)); shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
    context <- function() {
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) stop("Select a dataset first.")
      list(pond = state$data_pond, selected_id = state$current_dataset_id, user = state$active_user_id)
    }
    check_context <- function() {
      w <- workflow()
      if (is.null(w) || !identical(w$context, context()) || !identical(w$release, source_release())) stop("The selected dataset or source changed. Start the workflow again.")
      w
    }
    show_step <- function(n) {
      step(n)
      shiny::showModal(shiny::modalDialog(title = c("1. Choose public content", "2. Review public documentation", "3. Prepare public draft")[n],
        size = "l", easyClose = FALSE, shiny::uiOutput(session$ns("body")),
        footer = shiny::tagList(shiny::actionButton(session$ns("cancel"), "Cancel"),
          if (n > 1L) shiny::actionButton(session$ns("back"), "Back"),
          shiny::actionButton(session$ns(if (n == 3L) "finish" else "next_step"),
            if (n == 3L) "Create and open public draft" else "Continue", class = "btn-primary"))))
    }
    start <- function(update) protect({
      cleanup(); c <- context(); release <- source_release()
      if (!metadata_present(release)) stop("Choose a frozen in-house source first.")
      ch <- shiny::withProgress(message = "Reading verified source and previous selections", value = 0.2,
        public_edition_choices(release, c$pond, if (update) c$selected_id else NULL))
      workflow(list(context = c, release = release, id = if (update) c$selected_id else NULL))
      choices(ch); params(NULL); result(""); show_step(1L)
    })
    shiny::observeEvent(input$make, { access_require("edit", touch = TRUE); start(FALSE) })
    shiny::observeEvent(input$update, { access_require("edit", touch = TRUE); start(TRUE) })
    controls <- shiny::reactive({
      ch <- choices(); if (is.null(ch)) return(list())
      out <- list()
      for (p in input$files %||% character()) for (l in names(ch$fields[[p]]))
        out[[length(out) + 1L]] <- list(path = p, layer = l, fields = ch$fields[[p]][[l]])
      out
    })
    output$fields <- shiny::renderUI({
      access_require("read")
      ch <- choices()
      shiny::tagList(lapply(seq_along(controls()), function(i) {
        a <- controls()[[i]]; selected <- ch$selected_fields[[a$path]][[a$layer]]
        is_new_file <- !a$path %in% ch$selected_files
        if (is_new_file) selected <- a$fields
        shiny::tagList(shiny::checkboxInput(session$ns(paste0("layer_", i)), paste(a$path, a$layer, sep = " / "),
          is_new_file || a$layer %in% names(ch$selected_fields[[a$path]])),
          shiny::selectizeInput(session$ns(paste0("attributes_", i)), "Attributes to include", choices = a$fields, selected = selected, multiple = TRUE))
      }))
    })
    output$body <- shiny::renderUI({
      access_require("read")
      ns <- session$ns; ch <- choices(); p <- plan()
      if (step() == 1L) return(shiny::tagList(
        shiny::p("Data files are suggested below. Supporting documents are opt-in. Review each selection; unsupported formats are copied intact, including all their attributes. Include required companion files."),
        if (length(ch$changes)) shiny::tags$details(open = NA, shiny::tags$summary("Changes since the previous source"), shiny::tags$ul(lapply(ch$changes, shiny::tags$li))),
        shiny::selectizeInput(ns("files"), "Files to include", choices = ch$files$path, selected = ch$selected_files, multiple = TRUE),
        shiny::uiOutput(ns("fields"))))
      if (step() == 2L) {
        value <- function(key) public_decode(p$metadata[[paste0("entry:", key)]] %||% "null") %||% ""
        version <- if (is.null(p$previous)) "1.0.0" else {
          parts <- strsplit(p$previous$entry_tbl$version[1], ".", fixed = TRUE)[[1]]
          if (length(parts) == 3L && !anyNA(suppressWarnings(as.integer(parts)))) paste(c(parts[1:2], as.integer(parts[3]) + 1L), collapse = ".") else "1.0.0"
        }
        prior <- params()
        return(shiny::tagList(
          shiny::p("Descriptive metadata, authors, owners, dates and retained field definitions are prefilled. Rights and restrictions are carried forward for review. Public contacts and existing public documents stay in place when updating."),
          shiny::textInput(ns("title"), "Public title", prior$title %||% value("title")),
          shiny::textInput(ns("version"), "Public version", prior$version %||% version),
          shiny::textAreaInput(ns("method"), "Preparation method", prior$method %||% "Selected public files, layers and attributes; reviewed inherited metadata.", rows = 2),
          shiny::textAreaInput(ns("abstract"), "Public abstract", prior$edits[["entry:abstract"]] %||% value("abstract"), rows = 4),
          shiny::textAreaInput(ns("purpose"), "Purpose", prior$edits[["entry:purpose"]] %||% value("purpose"), rows = 2),
          shiny::textInput(ns("keywords"), "Object keywords", prior$edits[["entry:keyword_object"]] %||% value("keyword_object")),
          shiny::textInput(ns("extent"), "Geographic description", prior$edits[["entry:geographic_extent_name"]] %||% value("geographic_extent_name")),
          shiny::tags$details(shiny::tags$summary("All prefilled metadata and field definitions"), DT::DTOutput(ns("metadata"))),
          if (length(p$conflicts)) shiny::h4("Choose how to resolve these changes"),
          shiny::tagList(lapply(seq_along(p$conflicts), function(i) {
            key <- names(p$conflicts)[i]; conflict <- p$conflicts[[i]]
            shiny::tags$div(class = "well", shiny::strong(conflict$label),
              if (conflict$kind == "metadata") shiny::tags$pre(style = "white-space:pre-wrap", paste(
                "Previous source:", public_display_value(conflict$previous, p$source_payload), "\nCurrent public:", public_display_value(conflict$current, p$previous %||% p$source_payload),
                "\nNew source:", public_display_value(conflict$source, p$source_payload))),
              shiny::selectInput(ns(paste0("decision_", i)), "Use", choices = c("Choose\u2026" = "", "Current public" = "current", "New source" = "source"),
                selected = prior$decisions[[key]] %||% ""))
          })),
          shiny::helpText("For title, abstract, purpose, keywords and geographic description, a conflict choice first fills the field above; you can then edit the public wording. Check inherited restrictions in Licensing before distribution.")))
      }
      par <- params()
      retained <- sum(vapply(p$fields, function(x) sum(lengths(x)), numeric(1)))
      all_fields <- sum(vapply(ch$fields[p$files], function(x) sum(lengths(x)), numeric(1)))
      shiny::tagList(shiny::p(paste(length(p$files), "source files selected;", nrow(ch$files) - length(p$files), "source files excluded;", retained, "attributes retained;", all_fields - retained, "attributes excluded.")),
        shiny::p(paste(length(p$metadata), "metadata values and groups prefilled.")),
        shiny::p(paste(length(p$kept), "public metadata edits carried forward;", length(p$retained_files), "public files preserved;", length(p$conflicts), "conflicts reviewed.")),
        shiny::p(paste("Public dataset:", par$title, "\u2014 version", par$version)),
        shiny::tags$ul(lapply(p$review_items, shiny::tags$li)),
        if (any(vapply(p$conflicts, function(x) x$kind == "file", logical(1)))) shiny::p("Keeping a public file retains all its existing columns and rows instead of the selected source replacement. Inspect retained files before freezing."),
        shiny::p("The draft opens with files scanned and metadata populated. Complete review, prepare the public PDF and freeze when ready. Updating saves the previous public draft as a recovery checkpoint."))
    })
    output$metadata <- DT::renderDT({
      access_require("read")
      p <- plan(); shiny::req(p)
      rows <- data.frame(Field = vapply(names(p$metadata), public_label, character(1)),
        Value = vapply(p$metadata, public_display_value, character(1), payload = p$source_payload))
      datapond_datatable(rows, rownames = FALSE, options = list(pageLength = 8))
    })
    last_decisions <- shiny::reactiveVal(list())
    shiny::observeEvent(plan(), { access_require("read"); last_decisions(list()) }, ignoreNULL = FALSE)
    shiny::observe({
      access_require("read")
      p <- plan(); if (is.null(p) || step() != 2L) return()
      previous_choices <- shiny::isolate(last_decisions()); updated_choices <- previous_choices
      editable <- c("entry:title" = "title", "entry:abstract" = "abstract", "entry:purpose" = "purpose", "entry:keyword_object" = "keywords", "entry:geographic_extent_name" = "extent")
      for (i in seq_along(p$conflicts)) {
        key <- names(p$conflicts)[i]; decision <- input[[paste0("decision_", i)]]
        updated_choices[key] <- list(decision)
        if (!identical(previous_choices[[key]], decision) && key %in% names(editable) && length(decision) && decision %in% c("source", "current")) {
          value <- public_decode(p$conflicts[[i]][[decision]] %||% "null") %||% ""
          if (key %in% c("entry:abstract", "entry:purpose")) shiny::updateTextAreaInput(session, editable[[key]], value = value)
          else shiny::updateTextInput(session, editable[[key]], value = value)
        }
      }
      last_decisions(updated_choices)
    })
    shiny::observeEvent(input$next_step, { access_require("edit", touch = TRUE); protect({
      w <- check_context()
      if (step() == 1L) {
        selected <- input$files %||% character(); fields <- list()
        for (a in controls()) fields[[a$path]] <- list()
        for (i in seq_along(controls())) {
          a <- controls()[[i]]
          if (isTRUE(input[[paste0("layer_", i)]])) fields[[a$path]][[a$layer]] <- input[[paste0("attributes_", i)]] %||% character()
        }
        excluded <- names(fields)[!lengths(fields)]; selected <- setdiff(selected, excluded); fields <- fields[setdiff(names(fields), excluded)]
        cleanup()
        p <- shiny::withProgress(message = "Comparing metadata and preparing public content", value = 0.2,
          preview_public_dataset_draft(w$release, w$context$pond, w$context$user, selected, fields, w$id))
        plan(p)
        ch <- choices(); ch$selected_files <- selected; ch$selected_fields <- p$fields; choices(ch)
        show_step(2L)
      } else if (step() == 2L) {
        p <- plan(); decisions <- stats::setNames(lapply(seq_along(p$conflicts), function(i) input[[paste0("decision_", i)]] %||% ""), names(p$conflicts))
        if (any(!unlist(decisions) %in% c("current", "source"))) stop("Resolve each highlighted conflict before continuing.")
        if (!metadata_present(input$title) || !grepl("^[0-9]+[.][0-9]+[.][0-9]+$", input$version)) stop("Supply a public title and major.minor.patch version.")
        if (!metadata_present(input$method)) stop("Describe the preparation method.")
        params(list(title = input$title, version = input$version, method = input$method, decisions = decisions,
          edits = list("entry:abstract" = input$abstract, "entry:purpose" = input$purpose, "entry:keyword_object" = input$keywords, "entry:geographic_extent_name" = input$extent)))
        show_step(3L)
      }
    }) })
    shiny::observeEvent(input$back, { access_require("edit", touch = TRUE); protect({ check_context(); show_step(max(1L, step() - 1L)) }) })
    shiny::observeEvent(input$cancel, { access_require("edit", touch = TRUE); cleanup(); step(0L); shiny::removeModal() })
    shiny::observeEvent(input$finish, { access_require("edit", touch = TRUE); protect({
      check_context(); if (step() != 3L) stop("Review the preparation summary first.")
      p <- plan(); par <- params()
      created <- shiny::withProgress(message = "Saving and scanning public draft", value = 0.2,
        apply_public_dataset_draft(p, par$title, par$version, par$method, par$decisions, par$edits))
      plan(NULL); step(0L); shiny::removeModal(); on_changed(); on_open(created$dataset_id)
      result(paste("Public draft ready.", created$summary, "\nComplete its metadata review and prepare its PDF before freezing."))
      shiny::showNotification(created$summary, duration = 12)
    }) })
    shiny::observeEvent(input$defaults, { access_require("edit", touch = TRUE); protect({
      c <- context(); conn <- get_conn(c$pond); on.exit(DBI::dbDisconnect(conn))
      edition_editable(conn, c$selected_id, c$pond, c$user)
      policy <- DBI::dbGetQuery(conn, "SELECT excluded_json FROM dataset_public_metadata_policy_tbl WHERE dataset_id=?", params = list(as.integer(c$selected_id)))
      fields <- c(paste0("entry:", public_entry_fields()), paste0("table:", names(public_groups())))
      fields <- fields[!public_rights_key(fields)]
      orgs <- DBI::dbReadTable(conn, "organisation_tbl")
      workflow(list(context = c, release = source_release(), id = c$selected_id))
      shiny::showModal(shiny::modalDialog(title = "Public metadata defaults", size = "l",
        shiny::selectizeInput(session$ns("excluded"), "Internal metadata to omit from future public drafts", choices = stats::setNames(fields, vapply(fields, public_label, character(1))),
          selected = if (nrow(policy)) unlist(public_decode(policy$excluded_json[1])) else character(), multiple = TRUE),
        shiny::helpText("Save these defaults before freezing the in-house source. Internal notes, reviews, permissions and development history are excluded automatically. Rights and restrictions are always inherited."),
        shiny::selectInput(session$ns("contact_org"), "Organisation for public contact defaults", choices = c("Leave contact defaults unchanged" = "", stats::setNames(orgs$id, orgs$organisation_name_full))),
        shiny::uiOutput(session$ns("contact")),
        shiny::helpText("New public drafts use the first owner organisation and its nominated contact. Without a nomination they use the organisation itself. Updates preserve the public draft's existing contacts."),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(session$ns("save_defaults"), "Save defaults"))))
    }) })
    output$contact <- shiny::renderUI({
      access_require("read")
      shiny::req(input$contact_org)
      c <- context(); conn <- get_conn(c$pond); on.exit(DBI::dbDisconnect(conn))
      people <- DBI::dbGetQuery(conn, "SELECT id,name_first,name_last FROM persons_tbl WHERE organisation_id=?", params = list(as.integer(input$contact_org)))
      saved <- DBI::dbGetQuery(conn, "SELECT person_id FROM organisation_public_contact_tbl WHERE organisation_id=?", params = list(as.integer(input$contact_org)))
      shiny::selectInput(session$ns("contact_person"), "Nominated public contact", choices = c("Organisation contact (no named person)" = "", stats::setNames(people$id, paste(people$name_first, people$name_last))),
        selected = if (nrow(saved) && !is.na(saved$person_id[1])) saved$person_id[1] else "")
    })
    shiny::observeEvent(input$save_defaults, { access_require("edit", touch = TRUE); protect({
      w <- check_context(); c <- w$context
      set_public_metadata_defaults(c$selected_id, c$pond, c$user, input$excluded %||% character(),
        if (metadata_present(input$contact_org)) as.integer(input$contact_org) else NULL,
        if (metadata_present(input$contact_person)) as.integer(input$contact_person) else NULL)
      shiny::removeModal(); on_changed(); result("Public defaults saved. Freeze the in-house metadata policy before deriving a public draft from it.")
    }) })
    output$result <- shiny::renderText({ access_require("read"); result() })
  })
}
