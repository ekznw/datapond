# A semantic form lets password managers recognise a login. Submit through
# Shiny, explicitly including autofilled values (which may emit no change event).
access_credentials_form <- function(username_id, password_id, submit_id,
                                    username_label, password_label, submit_label) {
  field <- function(control, id, autocomplete) {
    htmltools::tagQuery(control)$find("input")$addAttrs(
      name = id, autocomplete = autocomplete
    )$allTags()
  }
  shiny::tags$form(
    id = paste0(submit_id, "_form"), method = "post", autocomplete = "on",
    onsubmit = sprintf(paste0(
      "event.preventDefault();",
      "Shiny.setInputValue('%s', this.elements['%s'].value);",
      "Shiny.setInputValue('%s', this.elements['%s'].value);",
      "Shiny.setInputValue('%s', Date.now(), {priority:'event'}); return false;"
    ), username_id, username_id, password_id, password_id, submit_id),
    field(shiny::textInput(username_id, username_label), username_id, "username"),
    field(shiny::passwordInput(password_id, password_label), password_id, "current-password"),
    shiny::tags$button(id = submit_id, type = "submit", class = "btn btn-default btn-primary", submit_label)
  )
}

access_ui <- function() {
  shiny::fluidPage(
    title = "DataPond",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = datapond_asset_url("display-theme.css")),
      shiny::tags$script(src = datapond_asset_url("display-theme.js")),
      shiny::tags$script(src = datapond_asset_url("access-session.js")),
      shiny::tags$script(src = datapond_asset_url("access-help.js"))),
    shiny::uiOutput("access_screen")
  )
}

access_console_commands <- function(pond) {
  path <- encodeString(pond, quote = '"')
  list(
    setup_existing = paste0("code <- datapond::setup_pond_access(\n  data_pond = ", path,
      ",\n  username = \"your.username\",\n  person_id = YOUR_PERSON_ID\n)\ncode"),
    setup_new = paste0("code <- datapond::setup_pond_access(\n  data_pond = ", path,
      ",\n  username = \"your.username\",\n  first_name = \"Your first name\",\n  last_name = \"Your last name\"\n)\ncode"),
    recover = paste0("code <- datapond::recover_pond_access(\n  data_pond = ", path,
      ",\n  username = \"your.username\"\n)\ncode")
  )
}

access_console_help_ui <- function(pond, configured) {
  commands <- access_console_commands(pond)
  command_box <- function(key, title) {
    id <- paste0("access_", key, "_command")
    shiny::div(class = "datapond-console-command",
      shiny::h5(title),
      shiny::tags$pre(id = id, shiny::tags$code(commands[[key]]), .noWS = "inside"),
      shiny::tags$button(type = "button", class = "btn btn-default",
        `data-copy-command` = id, "Copy R command"),
      shiny::tags$span(class = "datapond-copy-status", role = "status", `aria-live` = "polite"))
  }
  metadata_optional_details("Administrator: R console commands",
    shiny::p("Run these commands in a separate local R terminal on the computer and OS account running DataPond. Replace the example username and person details before running. The pond path is already filled in."),
    if (!configured) shiny::tagList(
      shiny::p("Set up the first administrator using one of these commands. For an existing metadata person, replace YOUR_PERSON_ID with their verified numeric person ID."),
      command_box("setup_existing", "First administrator: existing metadata person"),
      command_box("setup_new", "First administrator: new metadata person")
    ) else shiny::tagList(
      command_box("recover", "Issue a new activation or recovery code"),
      shiny::p("This works for an existing active account or an account awaiting activation. For a pending access request, sign in as an administrator and use Manage accounts > Approve pending request.")
    ),
    shiny::p("The terminal prints a single-use code valid for 24 hours. Give it privately to the account holder, who enters it in Activate / recover access and chooses a passphrase. After first-time setup, refresh this page to show that tab.")
  )
}

access_login_ui <- function(pond, configured, registry = read_connected_pond_registry()) {
  registry <- registry[order(registry$last_seen_at, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  saved_ponds <- if (nrow(registry)) stats::setNames(registry$data_pond_path, paste0(
    registry$pond_name, " - ", registry$data_pond_path,
    ifelse(vapply(registry$data_pond_path, function(path) {
      dir.exists(path) && file.exists(get_db_path(path))
    }, logical(1)), "", " (unavailable)")
  )) else character()
  shiny::div(class = "datapond-login",
    shiny::h2("Sign in to DataPond"),
    metadata_requirement_legend(),
    shiny::p("Pond: ", shiny::strong(basename(pond))),
    if (!configured) shiny::div(class = "alert alert-info",
      shiny::p("This pond needs its first administrator. Ask the person managing this installation to run setup_pond_access() in the local R console."),
      shiny::p("Existing metadata and seeded people are preserved. A seeded name does not grant login access.")),
    if (configured) shiny::tabsetPanel(id = "access_tab",
      shiny::tabPanel("Sign in",
        access_credentials_form("access_username", "access_password", "access_login",
          metadata_field_label("Username", "Mandatory", "Required to sign in."),
          metadata_field_label("Passphrase", "Mandatory", "Required to sign in."), "Sign in")),
      shiny::tabPanel("Request access",
        shiny::p("An administrator must verify and approve your request. They will give you an activation code."),
        shiny::textInput("access_request_username", metadata_field_label("Requested username", "Mandatory", "Required to request access.")),
        shiny::textInput("access_first", metadata_field_label("First name", "Mandatory", "Required to request access.")),
        shiny::textInput("access_last", metadata_field_label("Last name", "Mandatory", "Required to request access.")),
        shiny::textInput("access_email", "Contact email (optional)"),
        shiny::textInput("access_org", "Organisation (optional)"),
        shiny::actionButton("access_request", "Submit request")),
      shiny::tabPanel("Activate / recover access",
        shiny::p("Ask your administrator for a single-use code after they verify your identity. Codes expire after 24 hours."),
        shiny::textInput("access_activate_username", metadata_field_label("Username", "Mandatory", "Required to activate or recover access.")),
        shiny::passwordInput("access_code", metadata_field_label("Activation or recovery code", "Mandatory", "Required to activate or recover access.")),
        shiny::passwordInput("access_new_password", metadata_field_label("New passphrase (at least 15 characters)", "Mandatory", "Required to set your passphrase.")),
        shiny::passwordInput("access_confirm_password", metadata_field_label("Repeat passphrase", "Mandatory", "Repeat the new passphrase to confirm it.")),
        shiny::actionButton("access_activate", "Set passphrase"))),
    shiny::tags$hr(),
    shiny::p(shiny::a("Account setup and help", href = "datapond-guide/first-pond.html", target = "_blank", rel = "noopener")),
    metadata_optional_details("Open another pond",
      if (length(saved_ponds)) shiny::selectInput("access_known_pond", "Saved ponds",
        choices = c("Choose a saved pond" = "", saved_ponds), selected = ""),
      shiny::textInput("access_pond_path",
        metadata_field_label("Existing pond folder", "Mandatory", "Required to open the pond."),
        value = pond),
      shiny::p("Choose a saved pond or enter an existing pond's folder path. You will sign in to that pond after opening it."),
      shiny::actionButton("access_open_pond", "Open pond", class = "btn-primary")),
    access_console_help_ui(pond, configured),
    shiny::actionButton("exit_datapond", "Exit DataPond")
  )
}

access_server <- function(input, output, session, state, switch_pond, on_login = function() NULL) {
  auth <- shiny::reactiveValues(pond = shiny::isolate(state$data_pond), token = NULL,
    connected_tokens = list(), authenticated = FALSE, ready = FALSE, workspace_nonce = NULL, tick = 0L,
    configured = file.exists(access_db_path(shiny::isolate(state$data_pond))))
  session$userData$datapond_access <- auth
  if (is.function(session$handleRequest)) {
    original_request <- session$handleRequest
    access_replace_session_method(session, "handleRequest", function(req) {
      permission <- if (grepl("^/(download|upload)/", req$PATH_INFO)) "edit" else "read"
      access_http_filter(original_request, auth, permission)(req)
    })
  }
  if (is.function(session$registerDataObj)) {
    original_register <- session$registerDataObj
    access_replace_session_method(session, "registerDataObj", function(name, data, filterFunc) {
      original_register(name, data, access_http_filter(filterFunc, auth, bind_identity = TRUE))
    })
  }
  notify_error <- function(code) tryCatch(force(code), error = function(e) {
    if (!inherits(e, "shiny.silent.error")) shiny::showNotification(conditionMessage(e), type = "error", duration = 12, session = session)
    invisible(NULL)
  })
  clear_secrets <- function() for (id in c("access_password", "access_code", "access_new_password", "access_confirm_password"))
    shiny::updateTextInput(session, id, value = "")
  invalidate <- function() {
    connected_pond_sign_out(auth)
    if (!is.null(auth$token)) try(access_logout(auth$pond, auth$token), silent = TRUE)
    auth$token <- NULL; auth$authenticated <- FALSE; auth$ready <- FALSE
    auth$workspace_nonce <- NULL
    for (cleanup in session$userData$datapond_access_cleanup) try(cleanup(), silent = TRUE)
    state$active_user_id <- NULL
    clear_secrets()
    shiny::removeModal(session)
  }
  auth$invalidate <- invalidate
  # Replace the complete screen in one output so Shiny unbinds the previous
  # controls and tables before inserting the new login or workspace.
  output$access_screen <- shiny::renderUI({
    if (!auth$authenticated) return(access_login_ui(auth$pond, auth$configured))
    shiny::tagList(ui, shiny::div(style = "display:none",
      shiny::textInput("access_workspace_ready", NULL, value = auth$workspace_nonce)))
  })
  shiny::outputOptions(output, "access_screen", suspendWhenHidden = FALSE)
  shiny::observeEvent(input$access_workspace_ready, {
    if (isTRUE(auth$authenticated) && !isTRUE(auth$ready) &&
        identical(input$access_workspace_ready, auth$workspace_nonce)) {
      auth$ready <- TRUE
      tryCatch(on_login(), error = function(e) {
        invalidate()
        shiny::showNotification(paste("Could not open the authenticated workspace:", conditionMessage(e)),
          type = "error", duration = NULL, session = session)
      })
    }
  }, priority = 1000)
  shiny::observe({
    shiny::invalidateLater(5000, session)
    if (!shiny::isolate(auth$authenticated)) return()
    auth$tick <- shiny::isolate(auth$tick) + 1L
    current <- access_session(shiny::isolate(auth$pond), shiny::isolate(auth$token))
    if (is.null(current)) {
      invalidate()
      shiny::showNotification("Your session ended. Sign in again.", type = "warning", session = session)
      session$reload()
    }
  })
  shiny::observeEvent(input$access_login, notify_error({
    shiny::req(input$access_login > 0)
    shiny::req(!isTRUE(auth$authenticated))
    token <- access_login(auth$pond, input$access_username, input$access_password)
    identity <- access_session(auth$pond, token)
    if (!is.null(auth$token)) access_logout(auth$pond, auth$token)
    auth$token <- token
    state$active_user_id <- identity$user_id[1]
    auth$workspace_nonce <- access_secret()
    auth$authenticated <- TRUE
    clear_secrets()
  }), ignoreInit = TRUE)
  shiny::observeEvent(input$access_request, notify_error({
    shiny::req(input$access_request > 0)
    access_request(auth$pond, input$access_request_username, input$access_first,
      input$access_last, input$access_email, input$access_org)
    shiny::showNotification("Request received. Contact your pond administrator for approval or account recovery.", duration = 15, session = session)
  }), ignoreInit = TRUE)
  shiny::observeEvent(input$access_activate, notify_error({
    shiny::req(input$access_activate > 0)
    if (!identical(input$access_new_password, input$access_confirm_password)) stop("The passphrases do not match.")
    access_activate(auth$pond, input$access_activate_username, input$access_code, input$access_new_password)
    clear_secrets()
    shiny::updateTabsetPanel(session, "access_tab", selected = "Sign in")
    shiny::showNotification("Passphrase set. You can now sign in.", session = session)
  }), ignoreInit = TRUE)
  shiny::observeEvent(input$access_logout, {
    invalidate()
    session$reload()
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$access_known_pond, {
    shiny::req(nzchar(input$access_known_pond))
    shiny::updateTextInput(session, "access_pond_path", value = input$access_known_pond)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$access_open_pond, notify_error({
    shiny::req(input$access_open_pond > 0)
    path <- validate_existing_data_pond(input$access_pond_path)
    invalidate()
    switch_pond(path)
  }), ignoreInit = TRUE)
  session$onSessionEnded(function() {
    connected_pond_sign_out(auth)
    shiny::isolate(if (!is.null(auth$token)) try(access_logout(auth$pond, auth$token), silent = TRUE))
  })

  admin_tick <- shiny::reactiveVal(0L)
  issued_code <- shiny::reactiveVal(NULL)
  account_rows <- shiny::reactive({
    admin_tick(); access_require("admin")
    access_accounts(auth$pond, auth$token)
  })
  output$access_accounts <- DT::renderDT({
    datapond_datatable(account_rows(), selection = "single", rownames = FALSE, escape = TRUE,
      options = list(pageLength = 8, scrollX = TRUE))
  })
  shiny::observeEvent(input$access_accounts_rows_selected, {
    access_require("admin")
    rows <- account_rows(); index <- input$access_accounts_rows_selected
    if (length(index) != 1L || index < 1L || index > nrow(rows)) return()
    issued_code(NULL)
    shiny::updateSelectInput(session, "access_role", selected = rows$role[index])
    shiny::updateSelectInput(session, "access_status", selected = rows$status[index])
  })
  selected_account <- function() {
    access_require("admin", touch = TRUE)
    rows <- account_rows(); index <- input$access_accounts_rows_selected
    if (length(index) != 1L || index < 1L || index > nrow(rows)) stop("Select an account in the table.")
    rows$id[index]
  }
  output$access_issued_code <- shiny::renderUI({
    access_require("admin")
    value <- issued_code()
    if (is.null(value)) return(NULL)
    shiny::div(class = "alert alert-warning", shiny::p("Give this code privately to the verified person. It expires in 24 hours and can be used once."), shiny::tags$code(value))
  })
  output$access_audit <- DT::renderDT({
    access_require("admin"); admin_tick()
    conn <- access_connect(auth$pond); on.exit(DBI::dbDisconnect(conn))
    rows <- DBI::dbGetQuery(conn, "SELECT happened_at, actor_id, event, target_id, detail FROM audit ORDER BY id DESC LIMIT 200")
    rows$happened_at <- format(as.POSIXct(rows$happened_at, origin = "1970-01-01", tz = "UTC"), tz = "UTC", usetz = TRUE)
    datapond_datatable(rows, rownames = FALSE, escape = TRUE, options = list(pageLength = 8, scrollX = TRUE))
  })
  shiny::observeEvent(input$manage_access, notify_error({
    access_require("admin", touch = TRUE)
    issued_code(NULL)
    people <- get_persons(auth$pond)
    choices <- c("Create a new person" = "", stats::setNames(as.character(people$id),
      paste(people$name_first, people$name_last, paste0("[", people$id, "]"))))
    shiny::showModal(shiny::modalDialog(title = "Pond accounts", size = "l",
      shiny::p("Verify each person's identity before approving access or issuing recovery codes. Requests have no access until activation."),
      DT::DTOutput("access_accounts"),
      shiny::selectInput("access_role", "Role", c("Viewer" = "viewer", "Editor" = "editor", "Pond administrator" = "admin")),
      shiny::selectInput("access_person", "Link a pending request to a verified metadata person", choices),
      shiny::actionButton("access_approve", "Approve pending request"),
      shiny::actionButton("access_recover", "Issue activation / recovery code"),
      shiny::selectInput("access_status", "Account status", c("Active" = "active", "Disabled" = "disabled", "Rejected" = "rejected", "Pending approval" = "pending", "Awaiting activation" = "invited")),
      shiny::actionButton("access_update", "Apply role and status"),
      shiny::helpText("Role and status changes sign the affected account out. At least one administrator must remain."),
      shiny::uiOutput("access_issued_code"),
      shiny::tags$details(shiny::tags$summary("Access audit"), DT::DTOutput("access_audit")),
      footer = shiny::modalButton("Close")), session = session)
  }), ignoreInit = TRUE)
  shiny::observeEvent(input$access_approve, notify_error({
    id <- selected_account()
    issued_code(NULL)
    issued_code(access_approve(auth$pond, auth$token, id, input$access_role, input$access_person))
    admin_tick(admin_tick() + 1L)
    state$refresh_person <- state$refresh_person + 1L
  }), ignoreInit = TRUE)
  shiny::observeEvent(input$access_recover, notify_error({
    id <- selected_account(); issued_code(NULL)
    issued_code(access_recovery(auth$pond, auth$token, id))
    admin_tick(admin_tick() + 1L)
  }), ignoreInit = TRUE)
  shiny::observeEvent(input$access_update, notify_error({
    id <- selected_account(); issued_code(NULL)
    access_update_account(auth$pond, auth$token, id, input$access_role, input$access_status)
    admin_tick(admin_tick() + 1L)
  }), ignoreInit = TRUE)
  auth
}
