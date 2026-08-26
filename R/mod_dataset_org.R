organisation_select_choices <- function(df) {
  if (is.null(df) || nrow(as.data.frame(df)) == 0L) {
    return(character())
  }

  df <- as.data.frame(
    df,
    stringsAsFactors = FALSE
  )
  labels <- paste0(
    df$organisation_name_short,
    " (", df$organisation_code, ")"
  )
  departments <- trimws(
    as.character(df$organisation_department)
  )
  has_department <- !is.na(departments) & nzchar(departments)
  labels[has_department] <- paste0(
    labels[has_department],
    " \u2014 ",
    departments[has_department]
  )

  setNames(
    as.character(df$id),
    labels
  )
}


dataset_organisation_role_choices <- function() {
  c(
    "Owner" = "owner",
    "Custodian" = "custodian",
    "Publisher" = "publisher",
    "Distributor" = "distributor"
  )
}


dataset_organisation_role_descriptor <- function() {
  paste(
    "Owner: accountable owner; Custodian: manages and maintains the data;",
    "Publisher: formally issues the dataset; Distributor: provides access",
    "or copies. These roles describe data responsibilities, not app permissions."
  )
}


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
      choices = dataset_organisation_role_choices()
    ),
    tags$small(
      style = "display:block; color:#666; margin-bottom:10px;",
      dataset_organisation_role_descriptor()
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
      short = character(),
      code = character(),
      department = character(),
      address = character(),
      role = character(),
      stringsAsFactors = FALSE
    ))
    # load organisations from DB
    org_choices <- reactive({
      state$refresh_org
      req(state$data_pond)
      df <- get_organisations(state$data_pond)
      organisation_select_choices(df)
    })
    # populate dropdown
    observe({
      choices <- org_choices()
      updateSelectizeInput(
        session,
        "org_select",
        choices = choices,
        server = FALSE
      )
    })
    # ADD ORGANISATION
    observeEvent(input$add_org, {
      req(input$org_select)
      current <- org_list()
      selected_id <- as.integer(input$org_select)
      selected_role <- as.character(input$org_role)
      if (any(
        current$id == selected_id &
          tolower(current$role) == tolower(selected_role)
      )) return()
      df <- get_organisations(state$data_pond)
      selected <- df[df$id == selected_id, ]
      new_row <- data.frame(
        id = selected$id,
        name = selected$organisation_name_full,
        short = selected$organisation_name_short,
        code = selected$organisation_code,
        department = selected$organisation_department,
        address = selected$organisation_address,
        role = selected_role,
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
        department = character(),
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
        department = df$organisation_department,
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
