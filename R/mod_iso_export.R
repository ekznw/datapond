# Saved-record assessment is independent of the population/completeness mode.

iso_export_guidance <- function(message, location = "", stage = "") {
  text <- tolower(paste(message, location))
  guidance <- function(section, label, advice, field = "") {
    list(section = section, label = label, advice = advice, field = field)
  }
  if (stage == "engine" || grepl("python|lxml|validator|hash mismatch", text))
    return(guidance("", "Validation tools",
      "Install Python 3.9+ with lxml; DATAPOND_ISO_PYTHON can select its executable. Damaged bundled resources require a package reinstall."))
  if (grepl("canonical registry|code list is outside", text))
    return(guidance("", "Exporter code-list references",
      "The exporter or its pinned resources need updating. Do not change a valid recorded value to work around a registry-reference defect."))
  if (grepl("metadata encoding", text))
    return(guidance("metadata_review", "Metadata character encoding",
      "In Metadata review > Advanced metadata profile, select UTF-8 for this XML export. Dataset encoding is separate.",
      "iso_profile-metadata_encoding"))
  if (grepl("creation|dateinfo", text))
    return(guidance("metadata_review", "Metadata-record creation date",
      "In Metadata review > Advanced metadata profile, record the known creation date at its original precision. Do not invent a time to satisfy the pinned rules.",
      "iso_profile-metadata_creation_date"))
  if (grepl("script|processstep|process step", text))
    return(guidance("lineage", "Lineage \u2014 processing steps",
      "Check the ordered step description, date and script reference. Local scripts must exist inside the dataset folder."))
  if (grepl("supporting|document folder|dataset folder|local online reference", text) || stage == "package")
    return(guidance("supporting_documents", "Supporting documents",
      "Check the saved dataset folder and relative document path. Restore the file or update the citation; only selected citations are packaged."))
  if (grepl("lineage|li_source|source citation|source temporal", text))
    return(guidance("lineage", "Lineage",
      "Check the lineage statement and the cited sources or authoritative documents."))
  if (grepl("temporal|timeperiod", text))
    return(guidance("dataset_intro", "Temporal extents",
      "In Optional metadata, check the saved start/end values and their precision. Preserve unknown times; correct an end that definitely precedes its start.",
      "iso_wave2-temporal_start"))
  if (grepl("catalogue|catalog|gfc:", text))
    return(guidance("data_files", "Data dictionary export",
      "Check the catalogue title, version, date and language; check individual field definitions in Data files.",
      "iso_wave3-catalogue_title"))
  if (grepl("quality|mdq:|measure", text))
    return(guidance("data_files", "Data quality",
      "Check the quality measure reference and its recorded result.", "iso_wave2-quality_measure_identifier"))
  if (grepl("vertical", text))
    return(guidance("spatial_metadata", "Vertical extent",
      "Check the ordered bounds, units and vertical coordinate reference system.", "iso_wave3-vertical_label"))
  if (grepl("bounding box|area covered|geographic extent", text))
    return(guidance("metadata_review", "Area covered / resource scope",
      "Record an explicit area in Metadata review > Advanced metadata profile, or supply coordinates in Spatial metadata. Use nongeographic scope only when appropriate.",
      "iso_profile-geographic_extent_name"))
  if (grepl("spatial|crs|coordinate|raster|sample dimension", text))
    return(guidance("spatial_metadata", "Spatial metadata",
      "Check included file/layer extents, coordinate reference systems and resolution. Raster statistics and attributes are in Data files."))
  if (grepl("resource language|dataset language|resource encoding", text))
    return(guidance("metadata_review", "Dataset language and encoding",
      "Record the dataset's language and encoding in Metadata review > Advanced metadata profile.",
      "iso_profile-resource_language"))
  if (grepl("topic", text))
    return(guidance("dataset_intro", "ISO topic categories",
      "Select the controlled discovery categories that describe this resource.", "iso_profile-topic_categories"))
  if (grepl("distribut|licen|legal|constraint", text))
    return(guidance("licensing_access", "Licensing & data access",
      "Check the saved distribution formats, online resources and public licence terms."))
  if (grepl("review|maintenance", text))
    return(guidance("metadata_review", "Metadata review",
      "Check the recorded review date/time and update details."))
  guidance("dataset_intro", "Dataset intro",
    "Check the saved citation and description here; responsible contacts and Advanced metadata profile are in Metadata review. Expand technical details for the exact XML rule.")
}

iso_export_issues <- function(assessment) {
  groups <- list(Pinned = assessment$validation$issues,
    `DataPond policy` = assessment$validation$datapond_profile$issues,
    Package = assessment$package$issues, `Export semantics` = assessment$semantics$issues)
  rows <- data.frame(policy = character(), stage = character(), message = character(), location = character())
  for (label in names(groups)) {
    x <- groups[[label]]
    if (is.data.frame(x) && nrow(x)) rows <- rbind(rows, data.frame(
      policy = label, stage = x$stage, message = x$message, location = x$location))
  }
  # The same issue can block both policies; show one actionable card.
  keys <- paste(rows$stage, rows$message, rows$location, sep = "\r")
  result <- rows[!duplicated(keys), , drop = FALSE]
  result$policy <- vapply(unique(keys), function(key) paste(rows$policy[keys == key], collapse = ", "), character(1))
  result
}

mod_iso_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(
    class = "metadata-profile-details", open = NA,
    shiny::tags$summary("ISO metadata validation & export"),
    shiny::tags$p("Checks the saved record. Save dataset first to include your current edits. Each download reruns validation."),
    shiny::selectInput(ns("policy"), "Export validation policy", choices = c(
      "Pinned ISO rules (default)" = "pinned",
      "DataPond creation-date precision policy" = "datapond"), selected = "pinned"),
    shiny::helpText("The DataPond policy accepts a known creation date without inventing a time. Its result is separate from the pinned result. Neither result certifies ISO compliance or acceptance by a receiving catalogue."),
    shiny::actionButton(ns("assess"), "Check saved metadata", icon = shiny::icon("check")),
    shiny::uiOutput(ns("report"), `aria-live` = "polite"),
    shiny::uiOutput(ns("downloads")),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('datapondIsoField', function(id) {
        var attempts = 0;
        function reveal() {
          var el = document.getElementById(id);
          if (!el) return;
          var p = el.parentElement;
          while (p) { if (p.tagName === 'DETAILS') p.open = true; p = p.parentElement; }
          if (!el.getClientRects().length && ++attempts < 20) { setTimeout(reveal, 50); return; }
          el.scrollIntoView({block:'center', behavior:'smooth'});
          if (el.selectize) el.selectize.focus(); else el.focus();
        }
        setTimeout(reveal, 50);
      });
    "))
  )
}

mod_iso_export_server <- function(id, state, open_section) {
  shiny::moduleServer(id, function(input, output, session) {
    assessment <- shiny::reactiveVal(NULL)
    error <- shiny::reactiveVal(NULL)
    context <- shiny::reactive({
      list(state$data_pond, state$current_dataset_id, state$refresh_datasets,
        state$refresh_org, state$refresh_person, state$refresh_vocab, state$refresh_metadata_reviews,
        state$refresh_data_files_saved, state$refresh_attributes,
        state$refresh_spatial_metadata, state$refresh_lineage, state$refresh_supporting_docs)
    })
    shiny::observeEvent(context(), {
      access_require("read", touch = TRUE)
      assessment(NULL)
      error(NULL)
    }, ignoreInit = TRUE)
    current <- function() {
      if (!non_empty_scalar(state$data_pond) || !non_empty_scalar(state$current_dataset_id))
        stop("Select and save a dataset before checking its ISO metadata.", call. = FALSE)
      list(id = state$current_dataset_id, pond = state$data_pond)
    }
    policy <- shiny::reactive(match.arg(input$policy %||% "pinned", c("pinned", "datapond")))
    shiny::observeEvent(input$assess, {
      access_require("edit", touch = TRUE)
      assessment(NULL)
      error(NULL)
      tryCatch({
        record <- current()
        result <- shiny::withProgress(message = "Checking saved ISO metadata", value = 0.2, {
          assess_iso_metadata_package(record$id, record$pond)
        })
        assessment(result)
      }, error = function(e) error(conditionMessage(e)))
    })
    issues <- shiny::reactive({
      if (is.null(assessment())) return(data.frame())
      iso_export_issues(assessment())
    })
    shiny::observeEvent(input$open_issue, {
      access_require("edit", touch = TRUE)
      i <- suppressWarnings(as.integer(input$open_issue))
      rows <- issues()
      if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(rows)) return()
      guide <- iso_export_guidance(rows$message[i], rows$location[i], rows$stage[i])
      if (nzchar(guide$section)) open_section(guide$section, guide$field)
    })
    output$report <- shiny::renderUI({
      access_require("read")
      if (!is.null(error())) return(shiny::tags$p(class = "text-danger", error()))
      a <- assessment()
      if (is.null(a)) return(shiny::helpText("No current report. Check again after saving or changing datasets."))
      status <- function(label, result) shiny::tags$li(
        shiny::strong(paste0(label, ": ")), result$status)
      rows <- issues()
      cards <- lapply(seq_len(nrow(rows)), function(i) {
        guide <- iso_export_guidance(rows$message[i], rows$location[i], rows$stage[i])
        shiny::tags$div(style = "border:1px solid #ddd; border-radius:8px; padding:10px; margin:8px 0;",
          shiny::tags$small(rows$policy[i]),
          shiny::tags$p(rows$message[i]),
          shiny::tags$p(guide$advice),
          if (nzchar(guide$section)) shiny::tags$button(type = "button", class = "btn btn-default",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", session$ns("open_issue"), i),
            paste("Open", guide$label)))
      })
      deviations <- a$validation$datapond_profile$accepted_deviations
      warnings <- a$validation$warnings
      semantic_warnings <- a$semantics$warnings
      if (is.data.frame(semantic_warnings) && nrow(semantic_warnings))
        warnings <- if (is.data.frame(warnings)) rbind(warnings, semantic_warnings) else semantic_warnings
      catalogue_warnings <- a$validation$feature_catalogue$warnings
      if (is.data.frame(catalogue_warnings) && nrow(catalogue_warnings)) {
        catalogue_warnings$location <- paste("Feature catalogue:", catalogue_warnings$location)
        warnings <- if (is.data.frame(warnings)) rbind(warnings, catalogue_warnings) else catalogue_warnings
      }
      shiny::tagList(
        shiny::tags$p(paste("Saved-record snapshot:", a$assessed_at, "\u2014 crosswalk", a$crosswalk_version)),
        shiny::tags$ul(status("Pinned rules (with documented namespace/context adaptations)", a$validation),
          status("DataPond date policy", a$validation$datapond_profile),
          status("Export semantics / code lists", a$semantics),
          status("Local supporting files", a$package)),
        shiny::helpText("Receiving-catalogue compatibility: not assessed (no target selected)."),
        if (is.data.frame(deviations) && nrow(deviations)) shiny::tags$p(
          "The DataPond policy accepts the recorded creation-date precision; the pinned result still fails. Confirm the receiving catalogue accepts this policy."),
        if (is.data.frame(warnings) && nrow(warnings)) shiny::tags$details(
          shiny::tags$summary(paste(nrow(warnings), "warnings / unknown values")),
          shiny::tags$ul(lapply(paste(warnings$reason, "\u2014", warnings$location), shiny::tags$li))),
        shiny::tags$p(paste(nrow(a$package$files), "local supporting files;",
          length(a$package$external), "external references retained without checking availability.")),
        cards,
        shiny::tags$details(shiny::tags$summary("Technical validation details"),
          shiny::tags$pre(style = "max-height:350px; overflow:auto;",
            jsonlite::toJSON(iso_package_report(a, policy()), pretty = TRUE, auto_unbox = TRUE, na = "null")))
      )
    })
    output$downloads <- shiny::renderUI({
      access_require("read")
      a <- assessment()
      if (is.null(a)) return(NULL)
      ready <- isTRUE(iso_package_selected_result(a, policy())$valid) &&
        isTRUE(a$semantics$valid) && isTRUE(a$package$valid)
      shiny::tagList(
        shiny::downloadButton(session$ns("report_json"), "Download validation report"),
        if (ready) shiny::downloadButton(session$ns("package_zip"), "Validate & download ISO package"),
        shiny::helpText(if (ready) paste(
          "Includes XML, any generated feature catalogue, the validation report and cited local documents/scripts.",
          "Dataset data files are not included. The saved record and files are rechecked for every download.")
          else "Package download is blocked until the selected policy, export semantics and local file checks pass.")
      )
    })
    output$report_json <- shiny::downloadHandler(
      filename = function() { access_require("download"); "iso-validation-report.json" },
      content = function(file) {
        access_require("download")
        shiny::req(assessment())
        jsonlite::write_json(iso_package_report(assessment(), policy()), file,
          pretty = TRUE, auto_unbox = TRUE, na = "null")
      }, contentType = "application/json")
    output$package_zip <- shiny::downloadHandler(
      filename = function() { access_require("download"); paste0("iso-metadata-", state$current_dataset_id, ".zip") },
      content = function(file) {
        access_require("download")
        tryCatch({
          record <- current()
          iso_download_metadata_package(record$id, record$pond, file, policy())
        }, error = function(e) {
          assessment(NULL)
          error(conditionMessage(e))
          shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
          stop(e)
        })
      }, contentType = "application/zip")
    list(assessment = assessment, issues = issues)
  })
}
