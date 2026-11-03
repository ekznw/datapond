mod_dataset_releases_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(class = "well",
    shiny::h3("Frozen releases"),
    shiny::p("Freeze the saved dataset as a verified ZIP in arc/. Development can continue in dev/.",
      "Required metadata must be complete in the selected Essential or Full mode, and the latest review must approve the current saved metadata.",
      "The ZIP includes a dataset metadata bundle for import into another DataPond.",
      "Archive and close retires the working folder only after verification. Frozen does not mean published."),
    missing_file_cleanup_ui(ns("missing_files")),
    shiny::textAreaInput(ns("notes"), metadata_field_label("Release notes / reason for the operation", "Conditional", "Required when freezing, closing development, restoring a release or rolling back a checkpoint."), rows = 2),
    shiny::selectInput(ns("iso_policy"), "Additional ISO metadata", c("Not requested" = "none",
      "Require pinned ISO validation" = "pinned", "Require Data Pond profile validation" = "datapond")),
    shiny::actionButton(ns("preview"), "Preview saved dataset", class = "datapond-requires-active-user"),
    shiny::verbatimTextOutput(ns("preview_summary")),
    DT::DTOutput(ns("files")),
    shiny::checkboxInput(ns("documents_reviewed"), metadata_field_label("I have opened metadata.pdf and reviewed the working dataset folder shown in this preview.", "Conditional", "Required before freezing the previewed dataset."), FALSE),
    shiny::actionButton(ns("freeze"), "Freeze version; keep working copy", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("close"), "Freeze version and close development", class = "datapond-requires-active-user"),
    shiny::helpText("First prepare the PDF under Human-readable metadata. Open metadata.pdf from the dataset folder, or select the optional download checkbox for a browser copy, then inspect the dataset folder. Changed metadata requires fresh PDF preparation and review.",
      "Freezing preserves those documents; it does not generate them. Close other tools writing this dataset during freezing or restoration.",
      "All files are included. Check for temporary files or older bundles. Freezing requires Python 3."),
    shiny::h4("Release history"),
    DT::DTOutput(ns("history")),
    shiny::actionButton(ns("verify"), "Verify selected release"),
    shiny::uiOutput(ns("download_control"), inline = TRUE),
    shiny::actionButton(ns("archive"), "Close development using selected release", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("restore"), "Restore selected release as draft", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("preview_removal"), "Remove selected stale release ZIP...", class = "datapond-requires-active-user"),
    shiny::helpText("Administrators can preview and remove a superseded ZIP. Release history and metadata are retained.",
      "The newest release, the current archived dataset and releases needed by recovery checkpoints are protected.",
      "This removes the local ZIP. A copy-only upload leaves SharePoint copies in place; deletion-aware sync can remove them remotely."),
    shiny::helpText("Restore keeps the dataset identity and preserves any current draft in a private recovery checkpoint.",
      "The restored version is editable; save a new version before freezing again."),
    shiny::selectInput(ns("checkpoint"), metadata_field_label("Earlier working-draft recovery points", "Conditional", "Select a recovery point when rolling back a working draft."), choices = character()),
    shiny::actionButton(ns("rollback"), "Roll back to selected working-draft checkpoint", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("recover"), "Recover interrupted lifecycle operation", class = "datapond-requires-active-user"),
    shiny::verbatimTextOutput(ns("result"))
  )
}

mod_dataset_releases_server <- function(id, state, on_changed = function() NULL,
                                        population_profile = function() "rough") {
  shiny::moduleServer(id, function(input, output, session) {
    missing_file_cleanup_server("missing_files", state)
    preview <- shiny::reactiveVal(NULL); result <- shiny::reactiveVal(""); tick <- shiny::reactiveVal(0L)
    reviewed <- shiny::reactiveVal(FALSE)
    removal <- shiny::reactiveVal(NULL)
    clear_removal <- function() {
      if (!is.null(removal())) shiny::removeModal()
      removal(NULL)
    }
    shiny::observeEvent(preview(), {
      access_require("read", touch = TRUE)
      reviewed(FALSE)
      shiny::updateCheckboxInput(session, "documents_reviewed", value = FALSE)
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$documents_reviewed, { access_require("edit", touch = TRUE); reviewed(isTRUE(input$documents_reviewed) && !is.null(preview())) })
    context <- function() {
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) stop("Select and save a dataset first.")
      list(pond = state$data_pond, id = state$current_dataset_id, user = state$active_user_id)
    }
    protect <- function(expr) tryCatch(force(expr), error = function(e) {
      result(conditionMessage(e)); shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
    changed <- function() {
      clear_removal()
      preview(NULL); tick(tick() + 1L)
      on_changed()
    }
    shiny::observeEvent(list(state$data_pond, state$current_dataset_id), {
      access_require("read", touch = TRUE)
      preview(NULL); result("")
      clear_removal()
    }, ignoreInit = TRUE)
    shiny::observeEvent(state$refresh_datasets, { access_require("read", touch = TRUE); preview(NULL) }, ignoreInit = TRUE)
    shiny::observeEvent(list(input$history_rows_selected, state$active_user_id, state$refresh_datasets),
      { access_require("read"); clear_removal() }, ignoreInit = TRUE)
    shiny::observeEvent(population_profile(), { access_require("read"); preview(NULL); reviewed(FALSE) }, ignoreInit = TRUE)
    shiny::observe({
      access_require("read")
      tick(); state$refresh_datasets
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) return()
      rows <- dataset_release_checkpoints(state$current_dataset_id, state$data_pond)
      choices <- stats::setNames(rows$operation_id, paste(rows$happened_at, rows$note, sep = " - "))
      shiny::updateSelectInput(session, "checkpoint", choices = choices)
    })
    history <- shiny::reactive({
      tick(); state$refresh_datasets
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) return(data.frame())
      dataset_release_history(state$current_dataset_id, state$data_pond)
    })
    selected <- function() {
      rows <- history(); index <- input$history_rows_selected
      if (length(index) != 1L || index < 1 || index > nrow(rows)) stop("Select one release from history.")
      rows[index, , drop = FALSE]
    }
    output$history <- DT::renderDT({
      access_require("read")
      rows <- history()
      if (!nrow(rows)) return(datapond_datatable(data.frame(Message = "No frozen releases yet."), rownames = FALSE))
      rows$zip_size_gib <- vapply(rows$relative_path, function(path) {
        size <- file.info(file.path(state$data_pond, path, "dataset.zip"))$size
        if (is.na(size)) 0 else round(size / 1024^3, 3)
      }, numeric(1))
      datapond_datatable(rows[, c("version", "frozen_at", "state", "zip_size_gib", "notes", "release_id")], rownames = FALSE,
        selection = "single", options = list(pageLength = 5))
    })
    output$download_control <- shiny::renderUI({
      access_require("read")
      rows <- history(); index <- input$history_rows_selected
      enabled <- length(index) == 1L && !is.na(index) && index >= 1L && index <= nrow(rows)
      enabled <- enabled && identical(rows$state[index], "frozen")
      shiny::downloadButton(session$ns("download"), "Download selected release ZIP",
        enabled = enabled)
    })
    shiny::observeEvent(input$preview, { access_require("edit", touch = TRUE); protect({
      reviewed(FALSE)
      preview(NULL)
      shiny::updateCheckboxInput(session, "documents_reviewed", value = FALSE)
      c <- context()
      preview(shiny::withProgress(message = "Hashing saved dataset files", value = 0.2,
        preview_dataset_release(c$id, c$pond, c$user, population_profile())))
      result("Preview ready. Open metadata.pdf in the dataset folder, check the files, then confirm your review and record release notes.")
    }) })
    output$preview_summary <- shiny::renderText({
      access_require("read")
      p <- preview(); if (is.null(p)) return("Preview is required before freezing.")
      paste("Working folder:", p$source, "\nSaved version:", p$version,
        "\nMetadata complete:", if (p$approval$population_profile == "rough") "Essential mode" else "Full mode",
        "\nApproved by:", p$approval$reviewer, "at", p$approval$reviewed_at,
        "\nDocuments: prepared PDF and citation are current and unchanged.",
        "\nFiles:", p$file_count, "\nUncompressed bytes:", format(p$bytes, scientific = FALSE))
    })
    output$files <- DT::renderDT({
      access_require("read")
      p <- preview()
      rows <- if (is.null(p) || !length(p$files)) data.frame() else do.call(rbind, lapply(p$files, as.data.frame))
      datapond_datatable(rows, rownames = FALSE, options = list(pageLength = 5))
    })
    freeze <- function(close) protect({
      c <- context(); p <- preview()
      if (is.null(p)) stop("Preview the saved dataset before freezing.")
      if (!isTRUE(reviewed())) stop("Open metadata.pdf and review the dataset folder, then tick the review confirmation before freezing.")
      row <- shiny::withProgress(message = "Building and verifying frozen release", value = 0.1,
        create_dataset_release(c$id, c$pond, c$user, input$notes, p, close, input$iso_policy,
          population_profile = population_profile()))
      changed(); result(paste("Frozen version", row$version, "in", file.path(c$pond, row$relative_path),
        if (close) "\nDevelopment is closed." else "\nThe working dataset is unchanged."))
      play_finfoot_cue(session, if (close) "release-closed" else "release-frozen")
    })
    shiny::observeEvent(input$freeze, { access_require("edit", touch = TRUE); freeze(FALSE) })
    shiny::observeEvent(input$close, { access_require("edit", touch = TRUE); freeze(TRUE) })
    shiny::observeEvent(input$verify, { access_require("edit", touch = TRUE); protect({
      c <- context(); row <- selected()
      shiny::withProgress(message = "Verifying release bytes", value = 0.2, verify_dataset_release(row$release_id, c$pond))
      result(paste("Verified release", row$version, "including its ZIP, file manifest and private restoration snapshot."))
    }) })
    output$download <- shiny::downloadHandler(
      filename = function() { access_require("download"); paste0("dataset-release-", selected()$release_id, ".zip") },
      contentType = "application/zip", content = function(file) {
        access_require("download")
        c <- context(); paths <- verify_dataset_release(selected()$release_id, c$pond)
        if (!file.copy(paths$zip, file)) stop("Could not deliver the release ZIP.")
      })
    shiny::observeEvent(input$archive, { access_require("edit", touch = TRUE); protect({
      c <- context(); row <- selected()
      shiny::withProgress(message = "Verifying and closing development", value = 0.2,
        archive_dataset_release(row$release_id, c$pond, c$user, input$notes))
      changed(); result("Development closed. The verified release remains in arc/.")
      play_finfoot_cue(session, "release-archived")
    }) })
    shiny::observeEvent(input$restore, { access_require("edit", touch = TRUE); protect({
      c <- context(); row <- selected()
      restored <- shiny::withProgress(message = "Restoring files and metadata", value = 0.1,
        restore_dataset_release(row$release_id, c$pond, c$user, input$notes))
      changed(); result(paste("Draft restored to", restored$dataset_path, "\nRecovery checkpoint:", restored$checkpoint))
      play_finfoot_cue(session, "release-restored")
    }) })
    shiny::observeEvent(input$preview_removal, { access_require("admin", touch = TRUE); protect({
      clear_removal()
      c <- context(); row <- selected()
      if (!metadata_present(input$notes)) stop("Record the reason for removing this release ZIP in Release notes / reason for the operation.")
      plan <- shiny::withProgress(message = "Checking selected and retained releases", value = 0.2,
        preview_dataset_release_removal(row$release_id, c$pond, c$user))
      removal(list(context = c, preview = plan, note = input$notes))
      shiny::showModal(shiny::modalDialog(title = paste("Remove frozen ZIP for version", plan$version),
        shiny::p(sprintf("This permanently removes %.3f GiB from this pond. Version %s remains available.",
          plan$bytes / 1024^3, plan$retained_version)),
        shiny::p("The selected version will no longer be downloadable or restorable from this pond. Its history, metadata and checksums remain."),
        shiny::p("Reason: ", input$notes),
        shiny::p("SharePoint copies are affected only if your synchronisation propagates this deletion. Existing external caches are not removed."),
        shiny::checkboxInput(session$ns("confirm_removal"), "I understand that the selected release ZIP will be permanently removed.", FALSE),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("remove_release"), "Permanently remove this ZIP", class = "btn-danger")),
        easyClose = FALSE))
    }) })
    shiny::observeEvent(input$remove_release, { access_require("admin", touch = TRUE); protect({
      pending <- removal()
      if (is.null(pending)) stop("Preview the selected release removal again.")
      if (!isTRUE(input$confirm_removal)) stop("Confirm permanent removal in the preview before continuing.")
      c <- context()
      if (!identical(c, pending$context) || !identical(selected()$release_id[1], pending$preview$release_id))
        stop("The selected dataset, release or user changed. Preview removal again.")
      removed <- shiny::withProgress(message = "Removing selected release ZIP", value = 0.2,
        remove_dataset_release(pending$preview$release_id, c$pond, c$user, pending$note, pending$preview))
      changed()
      result(sprintf("Removed release ZIP; reclaimed %.3f GiB locally. History and metadata are retained. SharePoint copies require deletion-aware synchronisation.", removed$bytes_removed / 1024^3))
    }) })
    shiny::observeEvent(input$recover, { access_require("edit", touch = TRUE); protect({
      c <- context()
      messages <- recover_dataset_release_operations(c$id, c$pond, c$user)
      changed(); result(paste(messages, collapse = "\n"))
    }) })
    shiny::observeEvent(input$rollback, { access_require("edit", touch = TRUE); protect({
      c <- context()
      if (!input$checkpoint %in% dataset_release_checkpoints(c$id, c$pond)$operation_id) stop("Select a working-draft checkpoint.")
      restored <- shiny::withProgress(message = "Restoring the working-draft checkpoint", value = 0.1,
        restore_dataset_checkpoint(input$checkpoint, c$pond, c$user, input$notes))
      changed(); result(paste("Previous draft restored to", restored$dataset_path))
    }) })
  })
}
