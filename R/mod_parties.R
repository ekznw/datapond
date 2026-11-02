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
      metadata_field_label("Select existing organisation to edit", "Conditional", "Select an organisation when updating its details."),
      choices = NULL
    ),
    textInput(ns("org_full"), metadata_field_label("Full name", "Mandatory", "Required when adding or updating an organisation.")),
    textInput(ns("org_short"), metadata_field_label("Short name", "Mandatory", "Required when adding or updating an organisation.")),
    textInput(ns("org_code"), metadata_field_label("Organisation code", "Mandatory", "Required when adding or updating an organisation; use letters and numbers only.")),
    textInput(ns("org_department"), "Department"),
    textInput(ns("org_unit"), "Organisational unit name"),
    selectInput(ns("org_parent"), "Parent organisation", choices = c("None" = "")),
    textAreaInput(ns("org_address"), "Legacy free-text address (retained)", rows = 2),
    fluidRow(
      column(6, textInput(ns("org_delivery_point"), "Delivery point / street")),
      column(3, textInput(ns("org_city"), "City")),
      column(3, textInput(ns("org_admin_area"), "Province / state"))
    ),
    fluidRow(
      column(4, textInput(ns("org_postal_code"), "Postal code")),
      column(4, textInput(ns("org_country"), "Country")),
      column(4, textInput(ns("org_contact_email"), "Contact email"))
    ),
    actionButton(ns("add_org"), "Add as new organisation"),
    actionButton(ns("update_org"), "Update selected organisation"),
    actionButton(ns("clear_org_form"), "Clear form"),
    DT::DTOutput(ns("org_table")),
    hr(),
    h4("Merge or remove organisation"),
    tags$p(
      "Merge a duplicate organisation into the correct record, or remove an ",
      "organisation that has no linked people, datasets, contacts or licences."
    ),
    selectInput(
      ns("org_merge_source"),
      metadata_field_label("Duplicate organisation to remove", "Conditional", "Select the duplicate when merging organisations."),
      choices = NULL
    ),
    selectInput(
      ns("org_merge_target"),
      metadata_field_label("Correct organisation to keep", "Conditional", "Select the organisation to keep when merging."),
      choices = NULL
    ),
    actionButton(
      ns("review_org_merge"),
      "Review merge",
      class = "btn-danger datapond-requires-active-user"
    ),
    br(),
    br(),
    selectInput(
      ns("org_remove_select"),
      metadata_field_label("Unused organisation to remove", "Conditional", "Select an unused organisation when removing it."),
      choices = NULL
    ),
    actionButton(
      ns("review_org_remove"),
      "Review removal",
      class = "btn-danger datapond-requires-active-user"
    ),
    hr(),
    h3("Persons"),
    h4("Add / edit person"),
    selectInput(
      ns("person_edit_select"),
      metadata_field_label("Select existing person to edit", "Conditional", "Select a person when updating their details."),
      choices = NULL
    ),
    tags$small(
      style = "color:#666; display:block;",
      "Selecting a person loads their details into the fields below."
    ),
    br(),
    textInput(ns("first"), metadata_field_label("First name", "Mandatory", "Required when adding or updating a person.")),
    textInput(ns("last"), metadata_field_label("Last name", "Mandatory", "Required when adding or updating a person.")),
    selectInput(
      ns("org_select"),
      "Organisation / department",
      choices = NULL
    ),
    textInput(ns("email"), "Email"),
    textInput(ns("phone_number"), "Phone number"),
    fluidRow(
      column(6, textInput(ns("person_delivery_point"), "Delivery point / street")),
      column(3, textInput(ns("person_city"), "City")),
      column(3, textInput(ns("person_admin_area"), "Province / state"))
    ),
    fluidRow(
      column(6, textInput(ns("person_postal_code"), "Postal code")),
      column(6, textInput(ns("person_country"), "Country"))
    ),
    checkboxInput(ns("is_user"), "Keep as an attribution user (does not grant login access)"),
    helpText("Manage login access through Configuration \u2192 Manage accounts."),
    actionButton(ns("add_person"), "Add as new person"),
    actionButton(ns("update_person"), "Update selected person"),
    actionButton(ns("clear_person_form"), "Clear form"),
    br(),
    br(),
    DT::DTOutput(ns("person_table")),
    hr(),
    h4("Merge duplicate person"),
    tags$p(
      "Use this when the same person was added twice. All links and user ",
      "history are moved to the person you keep, then the duplicate is removed."
    ),
    selectInput(
      ns("person_merge_source"),
      metadata_field_label("Duplicate person to remove", "Conditional", "Select the duplicate when merging people."),
      choices = NULL
    ),
    selectInput(
      ns("person_merge_target"),
      metadata_field_label("Correct person to keep", "Conditional", "Select the person to keep when merging."),
      choices = NULL
    ),
    actionButton(
      ns("review_person_merge"),
      "Review merge",
      class = "btn-danger datapond-requires-active-user"
    )
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
      access_require("read")
      orgs <- organisations()
      if (nrow(orgs) == 0) {
        for (input_id in c(
          "org_select",
          "org_parent",
          "org_edit_select",
          "org_merge_source",
          "org_merge_target",
          "org_remove_select"
        )) {
          updateSelectInput(
            session,
            input_id,
            choices = character(0),
            selected = character(0)
          )
        }
        return()
      }
      choices <- organisation_select_choices(orgs)
      updateSelectInput(session, "org_select", choices = choices)
      updateSelectInput(session, "org_parent", choices = c("None" = "", choices),
                        selected = input$org_parent %||% "")
      selected_org_choice <- function(value) {
        value <- value %||% ""
        if (length(value) == 1L && value %in% choices) {
          return(value)
        }
        character(0)
      }
      for (input_id in c(
        "org_edit_select",
        "org_merge_source",
        "org_merge_target",
        "org_remove_select"
      )) {
        updateSelectInput(
          session,
          input_id,
          choices = choices,
          selected = selected_org_choice(input[[input_id]])
        )
      }
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
      updateTextInput(session, "org_unit", value = "")
      updateSelectInput(session, "org_parent", selected = "")
      updateTextInput(session, "org_address", value = "")
      for (field in c("org_delivery_point", "org_city", "org_admin_area", "org_postal_code", "org_country", "org_contact_email"))
        updateTextInput(session, field, value = "")
    }

    observeEvent(input$org_edit_select, {
      access_require("admin", touch = TRUE)
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
      updateTextInput(session, "org_unit", value = org_value("organisational_unit_name"))
      updateSelectInput(session, "org_parent", selected = org_value("parent_organisation_id"))
      updateTextInput(session, "org_address", value = org_value("organisation_address"))
      addresses <- get_party_addresses(state$data_pond, organisation_id = organisation$id[1])
      primary <- addresses[addresses$is_primary == 1L, , drop = FALSE]
      if (!nrow(primary) && nrow(addresses)) primary <- addresses[1, , drop = FALSE]
      address_value <- function(field) if (nrow(primary) && !is.na(primary[[field]][1])) as.character(primary[[field]][1]) else ""
      updateTextInput(session, "org_delivery_point", value = address_value("delivery_point"))
      updateTextInput(session, "org_city", value = address_value("city"))
      updateTextInput(session, "org_admin_area", value = address_value("administrative_area"))
      updateTextInput(session, "org_postal_code", value = address_value("postal_code"))
      updateTextInput(session, "org_country", value = address_value("country"))
      updateTextInput(session, "org_contact_email", value = address_value("electronic_mail_address"))
    }, ignoreInit = TRUE)

    observeEvent(input$clear_org_form, {
      access_require("admin", touch = TRUE)
      reset_organisation_form()
    })

    # organisation_name_shortadd organisation
    observeEvent(input$add_org, {
      access_require("admin", touch = TRUE)
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
        address = input$org_address,
        organisational_unit_name = input$org_unit,
        parent_organisation_id = input$org_parent,
        delivery_point = input$org_delivery_point, city = input$org_city,
        administrative_area = input$org_admin_area, postal_code = input$org_postal_code,
        country = input$org_country, electronic_mail_address = input$org_contact_email
      ), state$data_pond)

      refresh_org(refresh_org() + 1)
      state$refresh_org <- state$refresh_org + 1

      showNotification("Organisation added", type = "message")

      reset_organisation_form()
    })

    observeEvent(input$update_org, {
      access_require("admin", touch = TRUE)
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
      if (identical(as.character(organisation_id), as.character(input$org_parent))) {
        showNotification("An organisation cannot be its own parent.", type = "warning")
        return()
      }
      update_organisation(
        organisation_id = organisation_id,
        data = list(
          full = input$org_full,
          short = input$org_short,
          code = input$org_code,
          department = input$org_department,
          address = input$org_address,
          organisational_unit_name = input$org_unit,
          parent_organisation_id = input$org_parent,
          delivery_point = input$org_delivery_point, city = input$org_city,
          administrative_area = input$org_admin_area, postal_code = input$org_postal_code,
          country = input$org_country, electronic_mail_address = input$org_contact_email
        ),
        base_path = state$data_pond
      )

      refresh_org(refresh_org() + 1L)
      state$refresh_org <- state$refresh_org + 1L
      showNotification("Organisation updated", type = "message")
    })

    organisation_label <- function(organisation_id) {
      choices <- organisation_select_choices(organisations())
      label <- names(choices)[
        choices == as.character(organisation_id)
      ]
      if (length(label) == 0L) {
        return(paste("Organisation", organisation_id))
      }
      label[1]
    }

    observeEvent(input$review_org_merge, {
      access_require("admin", touch = TRUE)
      req(input$org_merge_source, input$org_merge_target)
      source_id <- as.integer(input$org_merge_source)
      target_id <- as.integer(input$org_merge_target)
      if (identical(source_id, target_id)) {
        showNotification(
          "Choose different organisations for the duplicate and record to keep.",
          type = "warning"
        )
        return()
      }

      showModal(modalDialog(
        title = "Confirm organisation merge",
        tags$p(
          "Remove ", tags$strong(organisation_label(source_id)),
          " and move all linked records to ",
          tags$strong(organisation_label(target_id)), "?"
        ),
        tags$p("This merge cannot be undone in the app."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_org_merge"),
            "Merge and remove duplicate",
            class = "btn-danger datapond-requires-active-user"
          )
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_org_merge, {
      access_require("admin", touch = TRUE)
      req(input$org_merge_source, input$org_merge_target)
      source_id <- as.integer(input$org_merge_source)
      target_id <- as.integer(input$org_merge_target)

      tryCatch({
        merged <- merge_organisation_records(
          source_organisation_id = source_id,
          target_organisation_id = target_id,
          base_path = state$data_pond
        )
        removeModal()
        refresh_org(refresh_org() + 1L)
        state$refresh_org <- state$refresh_org + 1L
        refresh_person(refresh_person() + 1L)
        state$refresh_person <- state$refresh_person + 1L
        updateSelectInput(
          session,
          "org_edit_select",
          selected = as.character(target_id)
        )
        showNotification(
          paste0(
            "Organisation merged. Moved ", merged$links_moved,
            " linked record(s) and collapsed ",
            merged$duplicate_links_removed, " duplicate link(s)."
          ),
          type = "message"
        )
      }, error = function(error) {
        showNotification(
          paste("The merge was not completed:", conditionMessage(error)),
          type = "error",
          duration = NULL
        )
      })
    })

    observeEvent(input$review_org_remove, {
      access_require("admin", touch = TRUE)
      req(input$org_remove_select)
      organisation_id <- as.integer(input$org_remove_select)
      usage <- get_organisation_usage(organisation_id, state$data_pond)
      if (nrow(usage) > 0L) {
        showNotification(
          paste0(
            "This organisation is used by ", sum(usage$records),
            " linked record(s). Merge it instead of removing it."
          ),
          type = "warning",
          duration = NULL
        )
        return()
      }

      showModal(modalDialog(
        title = "Confirm organisation removal",
        tags$p(
          "Permanently remove ",
          tags$strong(organisation_label(organisation_id)), "?"
        ),
        tags$p("Only unused organisations can be removed."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_org_remove"),
            "Remove organisation",
            class = "btn-danger datapond-requires-active-user"
          )
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_org_remove, {
      access_require("admin", touch = TRUE)
      req(input$org_remove_select)
      organisation_id <- as.integer(input$org_remove_select)

      tryCatch({
        remove_organisation_record(
          organisation_id = organisation_id,
          base_path = state$data_pond
        )
        removeModal()
        reset_organisation_form()
        refresh_org(refresh_org() + 1L)
        state$refresh_org <- state$refresh_org + 1L
        showNotification("Unused organisation removed", type = "message")
      }, error = function(error) {
        showNotification(
          paste("The organisation was not removed:", conditionMessage(error)),
          type = "error",
          duration = NULL
        )
      })
    })

    # add person
    observeEvent(input$add_person, {
      access_require("admin", touch = TRUE)
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
      person_address <- data.frame(
        delivery_point = input$person_delivery_point %||% "", city = input$person_city %||% "",
        administrative_area = input$person_admin_area %||% "", postal_code = input$person_postal_code %||% "",
        country = input$person_country %||% "", electronic_mail_address = input$email %||% "",
        phone_number = input$phone_number %||% "", online_resource = NA_character_, is_primary = 1L)
      if (any(nzchar(trimws(as.character(unlist(person_address[1, setdiff(names(person_address), "is_primary")], use.names = FALSE)))), na.rm = TRUE))
        replace_party_addresses(state$data_pond, person_address, person_id = person_id)
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
      for (field in c("person_delivery_point", "person_city", "person_admin_area", "person_postal_code", "person_country"))
        updateTextInput(session, field, value = "")
      updateSelectInput(session, "org_select", selected = character(0))
      updateCheckboxInput(session, "is_user", value = FALSE)
    })
    observe({
      access_require("read")
      df <- persons()
      if (nrow(df) == 0) {
        for (input_id in c(
          "person_edit_select",
          "person_merge_source",
          "person_merge_target"
        )) {
          updateSelectInput(
            session,
            input_id,
            choices = character(),
            selected = character(0)
          )
        }
        return()
      }
      choices <- setNames(
        as.character(df$id),
        paste0(
          trimws(paste(df$name_first, df$name_last)),
          " (#", df$id, ")"
        )
      )
      current_selected <- input$person_edit_select
      if (is.null(current_selected) ||
          length(current_selected) == 0 ||
          !current_selected %in% choices) {
        current_selected <- character(0)
      }
      selected_merge_choice <- function(value) {
        value <- value %||% ""
        if (length(value) == 1L && value %in% choices) {
          return(value)
        }
        character(0)
      }
      updateSelectInput(
        session,
        "person_edit_select",
        choices = choices,
        selected = current_selected
      )
      updateSelectInput(
        session,
        "person_merge_source",
        choices = choices,
        selected = selected_merge_choice(input$person_merge_source)
      )
      updateSelectInput(
        session,
        "person_merge_target",
        choices = choices,
        selected = selected_merge_choice(input$person_merge_target)
      )
    })
    # auto load selected person into form
    observeEvent(input$person_edit_select, {
      access_require("admin", touch = TRUE)
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
      addresses <- get_party_addresses(state$data_pond, person_id = person$id[1])
      primary <- addresses[addresses$is_primary == 1L, , drop = FALSE]
      if (!nrow(primary) && nrow(addresses)) primary <- addresses[1, , drop = FALSE]
      address_value <- function(field) if (nrow(primary) && !is.na(primary[[field]][1])) as.character(primary[[field]][1]) else ""
      updateTextInput(session, "person_delivery_point", value = address_value("delivery_point"))
      updateTextInput(session, "person_city", value = address_value("city"))
      updateTextInput(session, "person_admin_area", value = address_value("administrative_area"))
      updateTextInput(session, "person_postal_code", value = address_value("postal_code"))
      updateTextInput(session, "person_country", value = address_value("country"))
      updateSelectInput(session, "org_select",
        selected = as.character(person$organisation_id[1]))
      users <- get_users(state$data_pond)
      is_user <- nrow(users) > 0 && person$id[1] %in% users$person_id
      updateCheckboxInput(session, "is_user", value = isTRUE(is_user))
    }, ignoreInit = TRUE)

    # clear form
    observeEvent(input$clear_person_form, {
      access_require("admin", touch = TRUE)
      updateSelectInput(session, "person_edit_select", selected = character(0))
      updateTextInput(session, "first", value = "")
      updateTextInput(session, "last", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone_number", value = "")
      for (field in c("person_delivery_point", "person_city", "person_admin_area", "person_postal_code", "person_country"))
        updateTextInput(session, field, value = "")
      updateSelectInput(session, "org_select", selected = character(0))
      updateCheckboxInput(session, "is_user", value = FALSE)
    })
    # update selected person
    observeEvent(input$update_person, {
      access_require("admin", touch = TRUE)
      req(input$person_edit_select)
      if (input$first == "" || input$last == "") {
        showNotification("Enter a name", type = "warning")
        return()
      }
      person_id <- as.integer(input$person_edit_select)
      if (!isTRUE(input$is_user)) {
        allowed <- tryCatch({
          access_assert_unlinked_person(state$data_pond, person_id)
          TRUE
        }, error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 12)
          FALSE
        })
        if (!allowed) return()
      }
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
      person_address <- data.frame(
        delivery_point = input$person_delivery_point %||% "", city = input$person_city %||% "",
        administrative_area = input$person_admin_area %||% "", postal_code = input$person_postal_code %||% "",
        country = input$person_country %||% "", electronic_mail_address = input$email %||% "",
        phone_number = input$phone_number %||% "", online_resource = NA_character_, is_primary = 1L)
      has_person_address <- any(nzchar(trimws(as.character(unlist(
        person_address[1, setdiff(names(person_address), "is_primary")], use.names = FALSE
      )))), na.rm = TRUE)
      replace_party_addresses(
        state$data_pond,
        if (has_person_address) person_address else data.frame(),
        person_id = person_id
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

    observeEvent(input$review_person_merge, {
      access_require("admin", touch = TRUE)
      req(input$person_merge_source, input$person_merge_target)
      source_id <- as.integer(input$person_merge_source)
      target_id <- as.integer(input$person_merge_target)
      if (identical(source_id, target_id)) {
        showNotification(
          "Choose different records for the duplicate and the person to keep.",
          type = "warning"
        )
        return()
      }

      df <- persons()
      person_label <- function(person_id) {
        row <- df[df$id == person_id, , drop = FALSE]
        if (nrow(row) == 0L) {
          return(paste("Person", person_id))
        }
        trimws(paste(row$name_first[1], row$name_last[1]))
      }

      showModal(modalDialog(
        title = "Confirm person merge",
        tags$p(
          "Remove ", tags$strong(person_label(source_id)),
          " and move all of their links and user history to ",
          tags$strong(person_label(target_id)), "?"
        ),
        tags$p("This merge cannot be undone in the app."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_person_merge"),
            "Merge and remove duplicate",
            class = "btn-danger datapond-requires-active-user"
          )
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_person_merge, {
      access_require("admin", touch = TRUE)
      req(input$person_merge_source, input$person_merge_target)
      source_id <- as.integer(input$person_merge_source)
      target_id <- as.integer(input$person_merge_target)

      tryCatch({
        merged <- merge_person_records(
          source_person_id = source_id,
          target_person_id = target_id,
          base_path = state$data_pond
        )
        if (
          !is.na(merged$source_user_id) &&
            !is.na(merged$target_user_id) &&
            identical(
              as.integer(state$active_user_id),
              merged$source_user_id
            )
        ) {
          state$active_user_id <- merged$target_user_id
          save_config(
            state$data_pond,
            state$active_user_id,
            state$dataset_parent_dir
          )
        }
        removeModal()
        refresh_person(refresh_person() + 1L)
        state$refresh_person <- state$refresh_person + 1L
        updateSelectInput(
          session,
          "person_edit_select",
          selected = as.character(target_id)
        )
        showNotification(
          paste0(
            "Duplicate merged. Moved ",
            merged$person_links_moved + merged$user_links_moved,
            " linked record(s)."
          ),
          type = "message"
        )
      }, error = function(error) {
        showNotification(
          paste("The merge was not completed:", conditionMessage(error)),
          type = "error",
          duration = NULL
        )
      })
    })

    # organisation_name_shorttables
    output$org_table <- DT::renderDT({
      access_require("read")
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
      access_require("read")
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
