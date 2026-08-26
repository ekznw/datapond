mod_poc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Point of Contact"),
    selectInput(
      ns("poc_person"),
      "Select Contact Person",
      choices = c("No individual contact" = "")
    ),
    selectizeInput(
      ns("poc_organisation"),
      "Point of contact organisation",
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
      "Role",
      choices = dataset_organisation_role_choices()
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
      labels <- ifelse(
        !is.na(df$short) & nzchar(df$short),
        df$short,
        df$name
      )
      department <- trimws(as.character(df$department))
      labels[!is.na(department) & nzchar(department)] <- paste0(
        labels[!is.na(department) & nzchar(department)],
        " \u2014 ",
        department[!is.na(department) & nzchar(department)]
      )
      setNames(as.character(df$id), labels)
    })

    observe({
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
      df <- poc_list()
      if (nrow(df) == 0) return()
      poc_list(head(df, -1))
    })

    # display
    output$poc_table <- renderTable({
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
      poc_list(data.frame(
        id = as.integer(df$person_id),
        name = ifelse(is.na(df$name), "", df$name),
        organisation_id = as.integer(df$organisation_id),
        organisation = ifelse(
          is.na(df$organisation_name_short),
          "",
          ifelse(
            is.na(df$organisation_department) |
              !nzchar(df$organisation_department),
            df$organisation_name_short,
            paste0(
              df$organisation_name_short,
              " \u2014 ",
              df$organisation_department
            )
          )
        ),
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
