point_of_contact_role_choices <- function() {
  c(
    "Author \u2014 created or contributed to the dataset" = "author",
    dataset_organisation_role_choices()
  )
}


mod_poc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Point of Contact"),
    selectInput(
      ns("poc_person"),
      metadata_field_label("Select Contact Person", "Conditional", "When adding a contact, select a person, an owning organisation, or both."),
      choices = c("No individual contact" = "")
    ),
    selectizeInput(
      ns("poc_organisation"),
      metadata_field_label("Point of contact organisation", "Conditional", "Required when adding a contact without an individual person."),
      choices = c("No organisation contact" = ""),
      options = list(
        placeholder = "Select an owning organisation..."
      )
    ),
    tags$small(
      style = "display:block; color:#666; margin-bottom:10px;",
      paste(
        "Organisation choices come from the owning organisations in Dataset Intro.",
        "The organisation contact is stored independently of the person contact."
      )
    ),
    selectInput(
      ns("poc_role"),
      metadata_field_label("Role", "Conditional", "Choose a role when adding a contact."),
      choices = point_of_contact_role_choices()
    ),
    actionButton(ns("add_poc"), "Add POC"),
    actionButton(ns("remove_poc"), "Remove last"),
    tableOutput(ns("poc_table"))
  )
}

mod_poc_server <- function(id, state, organisation_rows = NULL) {
  moduleServer(id, function(input, output, session) {
    poc_list <- reactiveVal(data.frame(
      id = integer(),
      name = character(),
      organisation_id = integer(),
      organisation = character(),
      role = character(),
      stringsAsFactors = FALSE
    ))

    # reactive choices (linked to parties)
    persons_choices <- reactive({
      state$refresh_person
      df <- get_persons(state$data_pond)
      if (nrow(df) == 0) return(list())
      setNames(
        df$id,
        paste(df$name_first, df$name_last)
      )
    })
    # populate dropdown
    observe({
      access_require("read")
      updateSelectInput(
        session,
        "poc_person",
        choices = c(
          "No individual contact" = "",
          persons_choices()
        )
      )
    })

    owning_org_choices <- reactive({
      if (is.null(organisation_rows)) {
        return(character())
      }
      df <- organisation_rows()
      if (is.null(df) || nrow(df) == 0L) {
        return(character())
      }
      organisation_select_choices(
        data.frame(
          id = df$id,
          organisation_name_full = df$name,
          organisation_name_short = df$short,
          organisation_code = df$code,
          organisation_department = df$department,
          organisation_address = df$address,
          stringsAsFactors = FALSE
        )
      )
    })

    observe({
      access_require("read")
      updateSelectizeInput(
        session,
        "poc_organisation",
        choices = c(
          "No organisation contact" = "",
          owning_org_choices()
        ),
        server = FALSE
      )
    })

    # add POC
    observeEvent(input$add_poc, {
      access_require("edit", touch = TRUE)
      req(input$poc_role)
      optional_integer <- function(x) {
        if (is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(x[1])) {
          return(NA_integer_)
        }
        suppressWarnings(as.integer(x[1]))
      }
      person_id <- optional_integer(input$poc_person)
      organisation_id <- optional_integer(input$poc_organisation)
      if (is.na(person_id) && is.na(organisation_id)) {
        showNotification(
          "Select a contact person, an owning organisation, or both.",
          type = "warning"
        )
        return()
      }
      choices <- persons_choices()
      org_choices <- owning_org_choices()
      new <- data.frame(
        id = person_id,
        name = if (is.na(person_id)) {
          ""
        } else {
          names(choices)[choices == input$poc_person][1]
        },
        organisation_id = organisation_id,
        organisation = if (is.na(organisation_id)) {
          ""
        } else {
          names(org_choices)[org_choices == input$poc_organisation][1]
        },
        role = input$poc_role,
        stringsAsFactors = FALSE
      )
      poc_list(rbind(poc_list(), new))
    })

    # remove last
    observeEvent(input$remove_poc, {
      access_require("edit", touch = TRUE)
      df <- poc_list()
      if (nrow(df) == 0) return()
      poc_list(head(df, -1))
    })

    # display
    output$poc_table <- renderTable({
      access_require("read")
      df <- poc_list()
      if (nrow(df) == 0) return(NULL)
      df[, c("name", "organisation", "role")]
    })
    # expose to main app
    clear_poc <- function() {
      poc_list(data.frame(
        id = integer(),
        name = character(),
        organisation_id = integer(),
        organisation = character(),
        role = character(),
        stringsAsFactors = FALSE
      ))
      invisible(TRUE)
    }

    load_poc <- function(dataset_id) {
      if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
        clear_poc()
        return(invisible(FALSE))
      }
      df <- get_dataset_poc_links(dataset_id, state$data_pond)
      if (is.null(df) || nrow(df) == 0) {
        clear_poc()
        return(invisible(TRUE))
      }
      organisation_labels <- rep("", nrow(df))
      has_organisation <- !is.na(df$organisation_id)
      if (any(has_organisation)) {
        choices <- organisation_select_choices(
          data.frame(
            id = df$organisation_id[has_organisation],
            organisation_name_full =
              df$organisation_name_full[has_organisation],
            organisation_name_short =
              df$organisation_name_short[has_organisation],
            organisation_code =
              df$organisation_code[has_organisation],
            organisation_department =
              df$organisation_department[has_organisation],
            organisation_address =
              df$organisation_address[has_organisation],
            stringsAsFactors = FALSE
          )
        )
        organisation_labels[has_organisation] <- names(choices)[
          match(
            as.character(df$organisation_id[has_organisation]),
            unname(choices)
          )
        ]
      }
      poc_list(data.frame(
        id = as.integer(df$person_id),
        name = ifelse(is.na(df$name), "", df$name),
        organisation_id = as.integer(df$organisation_id),
        organisation = organisation_labels,
        role = df$role,
        stringsAsFactors = FALSE
      ))
      invisible(TRUE)
    }

    return(list(
      poc = poc_list,
      clear = clear_poc,
      load = load_poc
    ))
  })
}
