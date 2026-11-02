mod_dataset_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Metadata review exchange"),
    shiny::p("Issue one review at a time, let a colleague edit in their own pond, then inspect and accept their return into the master."),
    shiny::div(class = "pond-link-help-cards", `aria-label` = "Which pond to use for each review step",
      pond_link_help_card("1. Request a review", "the master pond holding the original dataset.",
        shiny::p("With edit access, open and save the dataset, name the reviewer below, then click Create metadata review request. This creates the request ZIP; download it and send it to your colleague."),
        shiny::p("Use this request ZIP for the review workflow. Compress selected dataset creates an ordinary transfer bundle without a review request.")),
      pond_link_help_card("2. Edit and return the review", "the reviewer's own pond.",
        shiny::p("With edit access, use Import dataset to import the request ZIP into a pond that does not already contain that dataset UUID. Edit and save the imported metadata, and record the work in Metadata review."),
        shiny::p("Keep that dataset selected, return here and click Create metadata review return. Send the return ZIP back to the master pond's curator.")),
      pond_link_help_card("3. Accept the reviewed metadata", "the original master pond, as a pond administrator.",
        shiny::p("Select the original dataset. Under Review returned bundle, choose the return ZIP and click Inspect return. Check the changes, enter a decision note and accept the reviewed metadata into the master."),
        shiny::p("Use Review returned bundle for this step. The master must still match the revision used to issue the request."))
    ),
    shiny::div(class="alert alert-info",
      "Review requests can include dataset files for reference. Returns and acceptance update metadata only; changes to files, supporting-document records, inventories and shared parties/vocabularies are not accepted. Save dataset edits before issuing, returning or accepting a review. Colleagues import the request using Import dataset into a pond without that dataset UUID."),
    shiny::tags$details(open=TRUE,
      shiny::tags$summary("Send the selected dataset for review"),
      shiny::textInput(ns("assignee"),metadata_field_label("Colleague responsible for the review", "Conditional", "Required when creating a metadata review request."),placeholder="Name and, if useful, email"),
      shiny::checkboxInput(ns("include_files"), "Include the dataset folder in the review request", value = TRUE),
      shiny::helpText("Includes supporting documents, data, other subfolders and files in the dataset root, including hidden files. Existing DataPond transfer ZIPs and temporary transfer metadata are excluded. Clear this option for a smaller, metadata-only request. Files outside the dataset folder are not included."),
      shiny::actionButton(ns("issue"),"Create metadata review request",class="datapond-requires-active-user"),
      shiny::helpText("The request reserves the current master revision. Subsequent master metadata or file changes will block acceptance; cancel and issue a fresh request if necessary.")),
    shiny::tags$details(open=TRUE,
      shiny::tags$summary("Return my review"),
      shiny::p("After importing a request, edit and save metadata and record the work in Metadata review. Select that dataset here to create its return. File edits made locally are not included."),
      shiny::actionButton(ns("return"),"Create metadata review return",class="datapond-requires-active-user")),
    shiny::uiOutput(ns("artifact")),
    shiny::tags$details(open=TRUE,
      shiny::tags$summary("Review returned bundle"),
      shiny::fileInput(ns("bundle"),metadata_field_label("Returned metadata review ZIP", "Conditional", "Choose the returned ZIP before inspecting a review."),accept=".zip"),
      shiny::actionButton(ns("inspect"),"Inspect return"),
      shiny::uiOutput(ns("assessment")),
      DT::DTOutput(ns("changes")),
      shiny::textAreaInput(ns("decision"),metadata_field_label("Curator acceptance decision / cancellation reason", "Conditional", "Required when accepting a return or cancelling a review request."),placeholder="Describe what you checked and why you accept these changes."),
      shiny::uiOutput(ns("accept_control")),
      shiny::helpText("Acceptance requires an active pond administrator, a matching unchanged master and a decision note. Previous metadata is retained, and failed acceptance rolls back. Keep file editors closed during acceptance.")),
    shiny::tags$details(
      shiny::tags$summary("Selected dataset review requests"),
      DT::DTOutput(ns("requests")),
      shiny::actionButton(ns("cancel"),"Cancel active master request",class="datapond-requires-active-user"),
      shiny::helpText("Uses the cancellation reason above. Cancelling keeps the history and permits a fresh request; any old return will be rejected."))
  )
}

mod_dataset_review_server <- function(id, state, on_accepted = function() NULL) {
  shiny::moduleServer(id,function(input,output,session) {
    assessment <- shiny::reactiveVal(NULL)
    artifact <- shiny::reactiveVal(NULL)
    refresh <- shiny::reactiveVal(0L)
    protect <- function(code) tryCatch(force(code),error=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=NULL,session=session))
    changed <- function() refresh(shiny::isolate(refresh())+1L)
    shiny::observeEvent(list(state$data_pond,state$current_dataset_id,input$bundle), {
      access_require("read", touch = TRUE)
      assessment(NULL); artifact(NULL)
    },ignoreInit=FALSE)
    current <- function() {
      shiny::req(state$data_pond,state$current_dataset_id)
      if (!has_active_user_id(state$active_user_id)) stop("Select an active application user first.")
    }
    shiny::observeEvent(input$issue,{ access_require("edit", touch = TRUE); protect({
      shiny::req(input$issue > 0); current()
      result <- shiny::withProgress(message="Preparing metadata review",value=0.2,
        issue_dataset_review(state$current_dataset_id,state$data_pond,state$active_user_id,input$assignee,
          include_files = isTRUE(input$include_files)))
      artifact(result); assessment(NULL); changed()
    }) },ignoreInit=TRUE)
    shiny::observeEvent(input$return,{ access_require("edit", touch = TRUE); protect({
      shiny::req(input$return > 0); current()
      artifact(create_dataset_review_return(state$current_dataset_id,state$data_pond,state$active_user_id)); changed()
    }) },ignoreInit=TRUE)
    shiny::observeEvent(input$inspect,{ access_require("edit", touch = TRUE); protect({
      shiny::req(input$inspect > 0,state$data_pond,input$bundle)
      assessment(NULL)
      assessment(shiny::withProgress(message="Comparing returned metadata",value=0.2,
        inspect_dataset_review_return(input$bundle$datapath[1],state$data_pond)))
    }) },ignoreInit=TRUE)
    output$assessment <- shiny::renderUI({
      access_require("read")
      report <- assessment(); if (is.null(report)) return(shiny::p("Inspect a returned bundle to see the proposed changes."))
      shiny::div(class=if(report$can_accept) "alert alert-success" else "alert alert-warning",
        shiny::strong(report$returned$entry_tbl$title[1]),shiny::br(),
        paste("Review",report$review_id,"for",report$request$assignee[1]),shiny::br(),
        if (report$can_accept) "The master matches the issued revision. Review the differences below before accepting." else paste(report$conflicts,collapse=" "),
        shiny::br(),"Acceptance rechecks this result; inspection does not lock the master.")
    })
    output$changes <- DT::renderDT({
      access_require("read")
      report <- assessment(); shiny::req(report)
      datapond_datatable(report$changes,rownames=FALSE,escape=TRUE,options=list(pageLength=10,scrollX=TRUE))
    })
    output$accept_control <- shiny::renderUI({
      access_require("read")
      report <- assessment()
      if (is.null(report) || !report$can_accept) return(NULL)
      if (!identical(as.integer(state$current_dataset_id), as.integer(report$dataset_id)))
        return(shiny::p("Select this dataset in the master before accepting its return."))
      if (!has_active_user_id(state$active_user_id) || !is_user_admin(state$active_user_id,state$data_pond))
        return(shiny::p("A pond administrator must accept the return."))
      shiny::actionButton(session$ns("accept"),"Accept reviewed metadata into master",class="btn-primary datapond-requires-active-user")
    })
    shiny::observeEvent(input$accept,{ access_require("admin", touch = TRUE); protect({
      shiny::req(input$accept > 0,input$bundle)
      if (!has_active_user_id(state$active_user_id)) stop("Select an active application user first.")
      report <- assessment(); if (is.null(report)) stop("Inspect this return first.")
      if (!identical(as.integer(state$current_dataset_id), as.integer(report$dataset_id)))
        stop("Select the returned dataset in the master, then inspect it again.")
      result <- shiny::withProgress(message="Accepting reviewed metadata",value=0.2,
        accept_dataset_review_return(input$bundle$datapath[1],state$data_pond,state$active_user_id,input$decision,report$return_hash))
      assessment(NULL); changed(); on_accepted()
      shiny::showNotification(paste("Review accepted. Previous metadata:",result$backup_path),type="message",duration=12,session=session)
    }) },ignoreInit=TRUE)
    shiny::observeEvent(input$cancel,{ access_require("admin", touch = TRUE); protect({
      shiny::req(input$cancel > 0); current()
      rows <- review_requests(state$data_pond,state$current_dataset_id)
      rows <- rows[rows$role=="master" & rows$status=="issued",,drop=FALSE]
      if (nrow(rows)!=1L) stop("The selected dataset has no active master request.")
      cancel_dataset_review(rows$review_id[1],state$data_pond,state$active_user_id,input$decision)
      assessment(NULL); changed()
      shiny::showNotification("Review cancelled; its history has been kept.",session=session)
    }) },ignoreInit=TRUE)
    output$artifact <- shiny::renderUI({
      access_require("read")
      value <- artifact(); if (is.null(value)) return(NULL)
      shiny::div(class="alert alert-success",shiny::p("Review bundle created:"),shiny::code(value$path),
        shiny::br(),shiny::downloadButton(session$ns("download"),"Download review ZIP"))
    })
    output$download <- shiny::downloadHandler(filename=function() { access_require("download"); basename(artifact()$path) },content=function(file) {
      access_require("download")
      value <- artifact(); shiny::req(value,file.exists(value$path))
      if (!file.copy(value$path,file,overwrite=TRUE)) stop("Could not download review bundle.")
    })
    output$requests <- DT::renderDT({
      access_require("read")
      refresh(); shiny::req(state$data_pond,state$current_dataset_id)
      rows <- review_requests(state$data_pond,state$current_dataset_id)
      fields <- c("review_id","role","status","assignee","issued_at","accepted_at","decision_note")
      rows <- rows[,fields,drop=FALSE]
      names(rows) <- c("Review ID","Role","Status","Reviewer","Issued","Accepted","Decision / reason")
      datapond_datatable(rows,rownames=FALSE,escape=TRUE,options=list(pageLength=5,scrollX=TRUE))
    })
    list(assessment=assessment,artifact=artifact)
  })
}
