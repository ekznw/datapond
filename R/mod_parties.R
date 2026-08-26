party_person_table_columns <- function() {
  c(
    "name_first",
    "name_last",
    "organisation_name_short",
    "organisation_code",
    "department",
    "email",
    "phone_number",
    "is_user"
  )
}


mod_parties_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Organisations"),
    h4("Add / edit organisation"),
    selectInput(
      ns("org_edit_select"),
      "Select existing organisation to edit",
      choices = NULL
    ),
    textInput(ns("org_full"), "Full name"),
    textInput(ns("org_short"), "Short name"),
    textInput(ns("org_code"), "Organisation code"),
    textInput(ns("org_department"), "Department"),
    textInput(ns("org_address"), "Address"),
    actionButton(ns("add_org"), "Add as new organisation"),
    actionButton(ns("update_org"), "Update selected organisation"),
    actionButton(ns("clear_org_form"), "Clear form"),
    DT::DTOutput(ns("org_table")),
    hr(),
    h3("Persons"),
    h4("Add / edit person"),
    selectInput(
      ns("person_edit_select"),
      "Select existing person to edit",
      choices = NULL
    ),
    tags$small(
      style = "color:#666; display:block;",
      "Selecting a person loads their details into the fields below."
    ),
    br(),
    textInput(ns("first"), "First name"),
    textInput(ns("last"), "Last name"),
    selectInput(
      ns("org_select"),
      "Organisation / department",
      choices = NULL
    ),
    textInput(ns("email"), "Email"),
    textInput(ns("phone_number"), "Phone number"),
    checkboxInput(ns("is_user"), "Is application user"),
    actionButton(ns("add_person"), "Add as new person"),
    actionButton(ns("update_person"), "Update selected person"),
    actionButton(ns("clear_person_form"), "Clear form"),
    br(),
    br(),
    DT::DTOutput(ns("person_table"))
  )
}

mod_parties_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_org <- reactiveVal(0)
    refresh_person <- reactiveVal(0)

    # organisation_name_shortorganisations data
    organisations <- reactive({
      refresh_org()
      state$refresh_org
      req(state$data_pond)
      get_organisations(state$data_pond)
    })

    # organisation_name_shortpersons data
    persons <- reactive({
      refresh_person()
      state$refresh_person
      req(state$data_pond)
      get_persons(state$data_pond)
    })

    # organisation_name_shortupdate dropdown when organisations change
    observe({
      orgs <- organisations()
      if (nrow(orgs) == 0) {
        updateSelectInput(session, "org_select", choices = character(0))
        updateSelectInput(session, "org_edit_select", choices = character(0))
        return()
      }
      choices <- organisation_select_choices(orgs)
      updateSelectInput(session, "org_select", choices = choices)
      current_selected <- input$org_edit_select
      if (
        is.null(current_selected) ||
        length(current_selected) == 0L ||
        !current_selected %in% choices
      ) {
        current_selected <- character(0)
      }
      updateSelectInput(
        session,
        "org_edit_select",
        choices = choices,
        selected = current_selected
      )
    })

    reset_organisation_form <- function() {
      updateSelectInput(
        session,
        "org_edit_select",
        selected = character(0)
      )
      updateTextInput(session, "org_full", value = "")
      updateTextInput(session, "org_short", value = "")
      updateTextInput(session, "org_code", value = "")
      updateTextInput(session, "org_department", value = "")
      updateTextInput(session, "org_address", value = "")
    }

    observeEvent(input$org_edit_select, {
      if (
        is.null(input$org_edit_select) ||
        length(input$org_edit_select) == 0L ||
        !nzchar(input$org_edit_select)
      ) {
        return()
      }

      orgs <- organisations()
      organisation <- orgs[
        orgs$id == as.integer(input$org_edit_select),
        ,
        drop = FALSE
      ]
      if (nrow(organisation) == 0L) {
        return()
      }

      org_value <- function(field) {
        value <- organisation[[field]][1]
        if (is.null(value) || length(value) == 0L || is.na(value)) {
          return("")
        }
        as.character(value)
      }

      updateTextInput(session, "org_full", value = org_value("organisation_name_full"))
      updateTextInput(session, "org_short", value = org_value("organisation_name_short"))
      updateTextInput(session, "org_code", value = org_value("organisation_code"))
      updateTextInput(session, "org_department", value = org_value("organisation_department"))
      updateTextInput(session, "org_address", value = org_value("organisation_address"))
    }, ignoreInit = TRUE)

    observeEvent(input$clear_org_form, {
      reset_organisation_form()
    })

    # organisation_name_shortadd organisation
    observeEvent(input$add_org, {
      if (input$org_full == "" || input$org_short == "" || input$org_code == "") {
        showNotification("Enter name and organisation code", type = "warning")
        return()
      }
      if (!grepl("^[A-Za-z0-9]+$", input$org_code)) {
        showNotification("Code must contain only letters and numbers", type = "warning")
        return()
      }
      insert_organisation(list(
        full = input$org_full,
        short = input$org_short,
        code = input$org_code,
        department = input$org_department,
        address = input$org_address
      ), state$data_pond)

      refresh_org(refresh_org() + 1)
      state$refresh_org <- state$refresh_org + 1

      showNotification("Organisation added", type = "message")

      reset_organisation_form()
    })

    observeEvent(input$update_org, {
      req(input$org_edit_select)
      if (input$org_full == "" || input$org_short == "" || input$org_code == "") {
        showNotification("Enter name and organisation code", type = "warning")
        return()
      }
      if (!grepl("^[A-Za-z0-9]+$", input$org_code)) {
        showNotification("Code must contain only letters and numbers", type = "warning")
        return()
      }

      organisation_id <- as.integer(input$org_edit_select)
      update_organisation(
        organisation_id = organisation_id,
        data = list(
          full = input$org_full,
          short = input$org_short,
          code = input$org_code,
          department = input$org_department,
          address = input$org_address
        ),
        base_path = state$data_pond
      )

      refresh_org(refresh_org() + 1L)
      state$refresh_org <- state$refresh_org + 1L
      showNotification("Organisation updated", type = "message")
    })
    # add person
    observeEvent(input$add_person, {
      if (input$first == "" || input$last == "") {
        showNotification("Enter a name", type = "warning")
        return()
      }
      # insert person
      person_id <- insert_person(list(
        first = input$first,
        last = input$last,
        email = input$email,
        phone_number = input$phone_number,
        org_id = input$org_select
      ), state$data_pond)
      # NEW: register user if selected
      if (isTRUE(input$is_user)) {
        insert_user(person_id, state$data_pond)
      }
      refresh_person(refresh_person() + 1)
      state$refresh_person <- state$refresh_person + 1
      showNotification("Person added", type = "message")
      # reset inputs
      updateTextInput(session, "first", value = "")
      updateTextInput(session, "last", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone_number", value = "")
      updateSelectInput(session, "org_select", selected = character(0))
      updateCheckboxInput(session, "is_user", value = FALSE)
    })
    observe({
      df <- persons()
      if (nrow(df) == 0) {
        updateSelectInput(
          session,
          "person_edit_select",
          choices = character(),
          selected = character(0)
        )
        return()
      }
      choices <- setNames(
        as.character(df$id),
        paste(df$name_first, df$name_last)
      )
      current_selected <- input$person_edit_select
      if (is.null(current_selected) ||
          length(current_selected) == 0 ||
          !current_selected %in% choices) {
        current_selected <- character(0)
      }
      updateSelectInput(
        session,
        "person_edit_select",
        choices = choices,
        selected = current_selected
      )
    })
    # auto load selected person into form
    observeEvent(input$person_edit_select, {
      if (is.null(input$person_edit_select) ||
          length(input$person_edit_select) == 0 ||
          input$person_edit_select == "") {
        return()
      }
      df <- get_persons(state$data_pond)
      person <- df[
        df$id == as.integer(input$person_edit_select),
        ,
        drop = FALSE
      ]
      if (nrow(person) == 0) {
        return()
      }
      person_value <- function(field) {
        value <- person[[field]][1]
        if (is.null(value) || length(value) == 0L || is.na(value)) {
          return("")
        }
        as.character(value)
      }
      updateTextInput(session, "first", value = person$name_first[1])
      updateTextInput(session, "last", value = person$name_last[1])
      updateTextInput(
        session,
        "email",
        value = person_value("email")
      )
      updateTextInput(
        session,
        "phone_number",
        value = person_value("phone_number")
      )
      updateSelectInput(session, "org_select",
        selected = as.character(person$organisation_id[1]))
      users <- get_users(state$data_pond)
      is_user <- nrow(users) > 0 && person$id[1] %in% users$person_id
      updateCheckboxInput(session, "is_user", value = isTRUE(is_user))
    }, ignoreInit = TRUE)

    # clear form
    observeEvent(input$clear_person_form, {
      updateSelectInput(session, "person_edit_select", selected = character(0))
      updateTextInput(session, "first", value = "")
      updateTextInput(session, "last", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone_number", value = "")
      updateSelectInput(session, "org_select", selected = character(0))
      updateCheckboxInput(session, "is_user", value = FALSE)
    })
    # update selected person
    observeEvent(input$update_person, {
      req(input$person_edit_select)
      if (input$first == "" || input$last == "") {
        showNotification("Enter a name", type = "warning")
        return()
      }
      person_id <- as.integer(input$person_edit_select)
      update_person(
        person_id = person_id,
        data = list(
          first = input$first,
          last = input$last,
          email = input$email,
          phone_number = input$phone_number,
          org_id = input$org_select
        ),
        base_path = state$data_pond
      )
      set_user_status(
        person_id = person_id,
        is_user = input$is_user,
        base_path = state$data_pond
      )
      refresh_person(refresh_person() + 1)
      state$refresh_person <- state$refresh_person + 1
      showNotification("Person updated", type = "message")
    })
    # organisation_name_shorttables
    output$org_table <- DT::renderDT({
      df <- organisations()
      if (nrow(df) == 0) return(NULL)
      cols <- intersect(
        c("organisation_name_full", "organisation_name_short",
          "organisation_code", "organisation_department",
          "organisation_address"
        ), names(df)
      )
      clean_dt_widget(
        df[, cols, drop = FALSE],
        page_length = 10L,
        filter = "top",
        buttons = FALSE
      )
    })
    output$person_table <- DT::renderDT({
      df <- as.data.table(persons())
      if (nrow(df) == 0) {
        return(NULL)
      }
      users <- as.data.table(get_users(state$data_pond))
      if (nrow(users) > 0) {
        df[, is_user := fifelse(id %in% users$person_id, "yes", "")]
      } else {
        df[, is_user := ""]
      }
      cols <- intersect(
        party_person_table_columns(),
        names(df)
      )
      clean_dt_widget(
        df[, ..cols],
        page_length = 10L,
        filter = "top",
        buttons = FALSE
      )
    })
  })
}
