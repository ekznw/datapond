mod_dataset_editions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(class = "well", open = NA,
    shiny::tags$summary(shiny::h3("In-house and public editions", style = "display:inline")),
    shiny::p("Each edition has its own dataset identity, files, documentation and frozen versions. Public editions record the exact in-house release used to prepare them."),
    shiny::verbatimTextOutput(ns("identity")),
    DT::DTOutput(ns("family")),
    shiny::actionButton(ns("in_house"), "Designate this draft as in-house", class = "datapond-requires-active-user"),
    shiny::selectInput(ns("source"), "Frozen in-house source", choices = character()),
    shiny::textAreaInput(ns("method"), "Public preparation method", rows = 3,
      placeholder = "Describe file/attribute selection, any simplification and how public documentation was prepared."),
    shiny::actionButton(ns("link"), "Link this existing public draft to source", class = "datapond-requires-active-user"),
    shiny::helpText("Linking records provenance for files you have already prepared. It does not replace files. Use Update this public edition below to carry forward the saved selections and compare metadata from a newer source. Manually changing this link clears the automated comparison baseline."),
    mod_public_edition_workflow_ui(ns("workflow")),
    shiny::h4("Record public distribution"),
    shiny::selectInput(ns("public_release"), "Frozen public release", choices = character()),
    shiny::textInput(ns("location"), "Publication location or URL"),
    shiny::textAreaInput(ns("publication_note"), "Publication / withdrawal note", rows = 2),
    shiny::actionButton(ns("publish"), "Record as currently published", class = "datapond-requires-active-user"),
    shiny::actionButton(ns("withdraw"), "Record withdrawal", class = "datapond-requires-active-user"),
    DT::DTOutput(ns("publication_history")),
    shiny::helpText("Records distribution already carried out and authorised separately. These controls do not upload, remove remote copies or grant permission to distribute. The family table shows the latest in-house freeze and currently published public version independently."),
    shiny::verbatimTextOutput(ns("result"))
  )
}

mod_dataset_editions_server <- function(id, state, on_changed = function() NULL, on_open = function(id) NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal("")
    tick <- shiny::reactiveVal(0L)
    context <- function() {
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) stop("Select a saved dataset first.")
      list(pond = state$data_pond, id = state$current_dataset_id, user = state$active_user_id)
    }
    protect <- function(expr) tryCatch(force(expr), error = function(e) {
      result(conditionMessage(e)); shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
    changed <- function() { tick(tick() + 1L); on_changed() }
    mod_public_edition_workflow_server("workflow", state, source_release = function() input$source,
      on_changed = changed, on_open = on_open)
    shiny::observeEvent(list(state$data_pond, state$current_dataset_id), {
      access_require("read"); result("")
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) return()
      c <- context(); conn <- get_conn(c$pond); on.exit(DBI::dbDisconnect(conn))
      edition <- edition_row(conn, c$id)
      shiny::updateTextAreaInput(session, "method", value = if (nrow(edition)) metadata_text(edition$preparation_method, "") else "")
    })
    shiny::observe({
      access_require("read"); tick(); state$refresh_datasets
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) return()
      c <- context(); conn <- get_conn(c$pond); on.exit(DBI::dbDisconnect(conn))
      ensure_dataset_edition_schema(conn); release_ensure_schema(conn)
      edition <- edition_row(conn, c$id)
      sources <- DBI::dbGetQuery(conn, "SELECT r.release_id,d.title,r.version,e.family_uuid,r.dataset_id
        FROM dataset_release_tbl r JOIN entry_tbl d ON d.id=r.dataset_id
        JOIN dataset_edition_tbl e ON e.dataset_id=d.id WHERE e.edition='in_house' AND r.state='frozen'
        ORDER BY r.frozen_at DESC,r.rowid DESC")
      if (nrow(edition)) sources <- sources[sources$family_uuid == edition$family_uuid[1], , drop = FALSE]
      selection <- shiny::isolate(input$source)
      if (!length(selection) || !selection %in% sources$release_id) {
        selection <- if (nrow(edition) && edition$source_release_id[1] %in% sources$release_id) edition$source_release_id[1] else utils::head(sources$release_id, 1)
      }
      shiny::updateSelectInput(session, "source", choices = stats::setNames(sources$release_id,
        paste(sources$title, sources$version, substr(sources$release_id, 1, 8), sep = " | ")), selected = selection)
      releases <- dataset_release_history(c$id, c$pond)
      releases <- releases[releases$state == "frozen", , drop = FALSE]
      if (!nrow(edition) || edition$edition[1] != "public") releases <- releases[FALSE, , drop = FALSE]
      shiny::updateSelectInput(session, "public_release", choices = stats::setNames(releases$release_id, releases$version))
    })
    output$identity <- shiny::renderText({
      access_require("read"); tick(); state$refresh_datasets
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) return("Select a dataset.")
      c <- context(); conn <- get_conn(c$pond); on.exit(DBI::dbDisconnect(conn))
      row <- edition_row(conn, c$id)
      if (!nrow(row)) return("Edition not designated. Existing releases are unchanged.")
      paste("Edition:", if (row$edition[1] == "in_house") "In-house" else "Public", "\nFamily:", row$family_uuid[1],
        if (row$edition[1] == "public") paste("\nIn-house source version:", row$source_version[1], "\nSource release:", row$source_release_id[1]) else "")
    })
    output$family <- DT::renderDT({
      access_require("read"); tick(); state$refresh_datasets
      rows <- if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id)) data.frame() else
        dataset_edition_family(state$current_dataset_id, state$data_pond)
      if (nrow(rows)) {
        rows <- rows[, c("dataset_id", "title", "edition", "latest_frozen", "published_version", "source_version", "publication_location")]
        names(rows) <- c("Dataset ID", "Title", "Edition", "Latest frozen version", "Currently published version", "In-house source version", "Publication location")
      }
      datapond_datatable(rows, rownames = FALSE, selection = "none", options = list(pageLength = 5))
    })
    output$publication_history <- DT::renderDT({
      access_require("read"); tick(); state$refresh_datasets
      if (!metadata_present(state$data_pond) || !metadata_present(state$current_dataset_id))
        return(datapond_datatable(data.frame(), rownames = FALSE))
      c <- context(); conn <- get_conn(c$pond); on.exit(DBI::dbDisconnect(conn))
      ensure_dataset_edition_schema(conn); release_ensure_schema(conn)
      rows <- DBI::dbGetQuery(conn, "SELECT r.version,p.action,p.location,p.happened_at,p.user_id,p.note
        FROM dataset_publication_tbl p JOIN dataset_release_tbl r ON r.release_id=p.release_id
        WHERE p.dataset_id=? ORDER BY p.id DESC", params = list(as.integer(c$id)))
      datapond_datatable(rows, rownames = FALSE, selection = "none", options = list(pageLength = 5))
    })
    shiny::observeEvent(input$in_house, { access_require("edit", touch = TRUE); protect({
      c <- context(); set_dataset_edition(c$id, c$pond, c$user, "in_house")
      changed(); result("In-house edition designated. Prepare and review metadata again before the next freeze.")
    }) })
    shiny::observeEvent(input$link, { access_require("edit", touch = TRUE); protect({
      c <- context(); set_dataset_edition(c$id, c$pond, c$user, "public", input$source, input$method)
      changed(); result("Public source and preparation method recorded. Files are unchanged. Review the public metadata and prepare its PDF before freezing.")
    }) })
    publication <- function(action) protect({
      c <- context()
      # Selection must belong to this dataset, even if inputs were stale.
      history <- dataset_release_history(c$id, c$pond)
      if (!input$public_release %in% history$release_id) stop("Select a frozen release of this public edition.")
      record_dataset_publication(input$public_release, c$pond, c$user, action, input$location, input$publication_note)
      changed(); result(if (action == "published") "Currently published version recorded. No files were uploaded." else "Withdrawal recorded. Remote copies have not been removed.")
    })
    shiny::observeEvent(input$publish, { access_require("edit", touch = TRUE); publication("published") })
    shiny::observeEvent(input$withdraw, { access_require("edit", touch = TRUE); publication("withdrawn") })
    output$result <- shiny::renderText({ access_require("read"); result() })
  })
}
