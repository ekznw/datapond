organisation_select_choices <- function(df) {
  if (is.null(df) || nrow(as.data.frame(df)) == 0L) {
    return(character())
  }

  df <- as.data.frame(
    df,
    stringsAsFactors = FALSE
  )

  field_value <- function(field) {
    if (!field %in% names(df)) {
      return(rep("", nrow(df)))
    }
    value <- as.character(df[[field]])
    value[is.na(value)] <- ""
    trimws(value)
  }

  full_name <- field_value("organisation_name_full")
  short_name <- field_value("organisation_name_short")
  code <- field_value("organisation_code")
  departments <- field_value("organisation_department")
  addresses <- field_value("organisation_address")
  display_name <- ifelse(nzchar(short_name), short_name, full_name)
  missing_name <- !nzchar(display_name)
  display_name[missing_name] <- paste(
    "Organisation",
    df$id[missing_name]
  )

  labels <- display_name
  has_code <- nzchar(code)
  labels[has_code] <- paste0(
    labels[has_code],
    " (", code[has_code], ")"
  )
  has_department <- nzchar(departments)
  labels[has_department] <- paste0(
    labels[has_department],
    " \u2014 ",
    departments[has_department]
  )
  has_address <- nzchar(addresses)
  labels[has_address] <- paste0(
    labels[has_address],
    " \u2014 Address: ",
    addresses[has_address]
  )

  sorted <- order(
    tolower(display_name),
    has_department,
    tolower(departments),
    tolower(code),
    suppressWarnings(as.integer(df$id)),
    na.last = TRUE,
    method = "radix"
  )

  setNames(
    as.character(df$id[sorted]),
    labels[sorted]
  )
}


dataset_organisation_role_choices <- function() {
  c(
    "Owner \u2014 accountable for the dataset and decisions about its use" = "owner",
    "Custodian \u2014 manages, maintains and safeguards the working data" = "custodian",
    "Publisher \u2014 formally issues or releases the dataset or version" = "publisher",
    "Distributor \u2014 provides access, delivery or copies to users" = "distributor"
  )
}


dataset_organisation_role_descriptor <- function() {
  paste(
    "Choose every role that applies to the organisation.",
    "Owner is accountable for the dataset and use decisions; Custodian handles",
    "day-to-day maintenance and safeguarding; Publisher formally releases a",
    "dataset or version; Distributor supplies access or copies.",
    "These are data-responsibility roles, not app permissions."
  )
}


mod_dataset_org_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(metadata_field_label("Owning Organisation(s)", "Mandatory", "Add at least one organisation with a usable code before creating or saving the dataset.")),
    shiny::helpText(paste(
      "Add at least one organisation. The first added organisation's code,",
      "publication date and first theme keyword form the folder name and citation key.",
      "Selecting an organisation alone does not add it; click Add."
    )),
    selectizeInput(
      ns("org_select"),
      metadata_field_label("Select organisation", "Conditional", "Select an organisation when adding it; click Add to include it in the dataset."),
      choices = NULL,
      options = list(
        placeholder = "Select organisation..."
      )
    ),
    selectInput(
      ns("org_role"),
      metadata_field_label("Role", "Conditional", "Choose a role when adding an owning organisation."),
      choices = dataset_organisation_role_choices()
    ),
    tags$small(
      style = "display:block; color:#666; margin-bottom:10px;",
      dataset_organisation_role_descriptor()
    ),
    actionButton(ns("add_org"), "Add"),
    actionButton(ns("remove_org"), "Remove last"),
    DT::DTOutput(ns("org_table"))
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
      access_require("read")
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
      access_require("edit", touch = TRUE)
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
      access_require("edit", touch = TRUE)
      df <- org_list()
      if (nrow(df) == 0) return()
      org_list(df[-nrow(df), ])
    })
    # DISPLAY TABLE
    output$org_table <- DT::renderDT({
      access_require("read")
      df <- org_list()
      if (nrow(df) == 0) return(NULL)
      datapond_summary_table(df)
    })
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
