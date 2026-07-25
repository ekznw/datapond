mod_poc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Point of Contact"),
    selectInput(
      ns("poc_person"),
      "Select Contact Person",
      choices = NULL
    ),
    selectInput(
      ns("poc_role"),
      "Role",
      choices = c("custodian", "owner", "distributor", "author")
    ),
    actionButton(ns("add_poc"), "Add POC"),
    actionButton(ns("remove_poc"), "Remove last"),
    tableOutput(ns("poc_table"))
  )
}

mod_poc_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    poc_list <- reactiveVal(data.frame(
      id = integer(),
      name = character(),
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
        choices = persons_choices()
      )
    })

    # add POC
    observeEvent(input$add_poc, {
      req(input$poc_person, input$poc_role)
      choices <- persons_choices()
      new <- data.frame(
        id = as.integer(input$poc_person),
        name = names(choices)[choices == input$poc_person],
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
      df[, c("name", "role")]
    })
    # expose to main app
    clear_poc <- function() {
      poc_list(data.frame(
        id = integer(),
        name = character(),
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
        name = df$name,
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