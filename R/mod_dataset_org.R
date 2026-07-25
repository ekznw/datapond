mod_dataset_org_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Owning Organisation(s)"),
    selectizeInput(
      ns("org_select"),
      "Select organisation",
      choices = NULL,
      options = list(
        placeholder = "Select organisation..."
      )
    ),
    selectInput(
      ns("org_role"),
      "Role",
      choices = c(
        "custodian",
        "owner",
        "publisher",
        "contributor"
      )
    ),
    actionButton(ns("add_org"), "Add"),
    actionButton(ns("remove_org"), "Remove last"),
    tableOutput(ns("org_table"))
  )
}

mod_dataset_org_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # current selection list
    org_list <- reactiveVal(data.frame(
      id = integer(),
      name = character(),
      code = character(),
      address = character(),
      stringsAsFactors = FALSE
    ))
    # load organisations from DB
    org_choices <- reactive({
      df <- get_organisations(state$data_pond)
      if (nrow(df) == 0) return(list())
      setNames(
        as.character(df$id),
        paste0(
          df$organisation_name_short,
          " (", df$organisation_code, ")"
        )
      )
    })
    # populate dropdown
    observe({
      choices <- org_choices()
      req(length(choices) > 0)
      updateSelectizeInput(
        session,
        "org_select",
        choices = choices,
        server = TRUE
      )
    })
    # ADD ORGANISATION
    observeEvent(input$add_org, {
      req(input$org_select)
      current <- org_list()
      selected_id <- as.integer(input$org_select)
      if (selected_id %in% current$id) return()
      df <- get_organisations(state$data_pond)
      selected <- df[df$id == selected_id, ]
      new_row <- data.frame(
        id = selected$id,
        name = selected$organisation_name_full,
        short = selected$organisation_name_short,
        code = selected$organisation_code,
        address = selected$organisation_address,
        role = input$org_role,
        stringsAsFactors = FALSE
      )
      org_list(rbind(current, new_row))
    })
    # REMOVE LAST
    observeEvent(input$remove_org, {
      df <- org_list()
      if (nrow(df) == 0) return()
      org_list(df[-nrow(df), ])
    })
    # DISPLAY TABLE
    output$org_table <- renderTable({
      df <- org_list()
      if (nrow(df) == 0) return(NULL)
      df
    }, striped = TRUE)
    # expose to main app
    clear_orgs <- function() {
      org_list(data.frame(
        id = integer(),
        name = character(),
        short = character(),
        code = character(),
        address = character(),
        role = character(),
        stringsAsFactors = FALSE
      ))
      invisible(TRUE)
    }

    load_orgs <- function(dataset_id) {
      if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
        clear_orgs()
        return(invisible(FALSE))
      }
      df <- get_dataset_org_links(dataset_id, state$data_pond)
      if (is.null(df) || nrow(df) == 0) {
        clear_orgs()
        return(invisible(TRUE))
      }
      org_list(data.frame(
        id = as.integer(df$organisation_id),
        name = df$organisation_name_full,
        short = df$organisation_name_short,
        code = df$organisation_code,
        address = df$organisation_address,
        role = df$role,
        stringsAsFactors = FALSE
      ))
      invisible(TRUE)
    }

    return(list(
      orgs = org_list,
      clear = clear_orgs,
      load = load_orgs
    ))
  })
}
