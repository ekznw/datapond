mod_authors_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Authors"),

    selectInput(
      ns("author_select"),
      "Select author",
      choices = NULL
    ),

    actionButton(
      ns("add_author"),
      "Add"
    ),

    actionButton(
      ns("move_up"),
      "Up: Up"
    ),

    actionButton(
      ns("move_down"),
      "Down: Down"
    ),

    actionButton(
      ns("remove_author"),
      "Remove"
    ),

    tableOutput(
      ns("author_table")
    )
  )
}


mod_authors_server <- function(id, state) {
  moduleServer(
    id,
    function(input, output, session) {

      empty_author_table <- function() {
        data.frame(
          id = integer(),
          name = character(),
          name_first = character(),
          name_last = character(),
          author_order = integer(),
          stringsAsFactors = FALSE
        )
      }

      normalise_author_table <- function(df) {
        if (
          is.null(df) ||
          nrow(df) == 0L
        ) {
          return(
            empty_author_table()
          )
        }

        df <- as.data.frame(
          df,
          stringsAsFactors = FALSE
        )

        if (!"id" %in% names(df)) {
          stop(
            "Author data requires an id column."
          )
        }

        if (!"name_first" %in% names(df)) {
          df$name_first <- ""
        }

        if (!"name_last" %in% names(df)) {
          df$name_last <- ""
        }

        df$id <- suppressWarnings(
          as.integer(df$id)
        )

        df$name_first <- ifelse(
          is.na(df$name_first),
          "",
          trimws(
            as.character(df$name_first)
          )
        )

        df$name_last <- ifelse(
          is.na(df$name_last),
          "",
          trimws(
            as.character(df$name_last)
          )
        )

        df$name <- trimws(
          paste(
            df$name_first,
            df$name_last
          )
        )

        df$author_order <- seq_len(
          nrow(df)
        )

        df[
          ,
          c(
            "id",
            "name",
            "name_first",
            "name_last",
            "author_order"
          ),
          drop = FALSE
        ]
      }

      # Author order is stored by row order in this reactiveVal.
      author_list <- reactiveVal(
        empty_author_table()
      )

      # Explicit dependency ensures that newly added or edited
      # people refresh the dropdown.
      persons_choices <- reactive({
        state$refresh_person

        df <- get_persons(
          state$data_pond
        )

        if (
          is.null(df) ||
          nrow(df) == 0L
        ) {
          return(
            stats::setNames(
              character(),
              character()
            )
          )
        }

        df <- as.data.frame(
          df,
          stringsAsFactors = FALSE
        )

        df$name_first <- ifelse(
          is.na(df$name_first),
          "",
          trimws(
            as.character(df$name_first)
          )
        )

        df$name_last <- ifelse(
          is.na(df$name_last),
          "",
          trimws(
            as.character(df$name_last)
          )
        )

        display_name <- trimws(
          paste(
            df$name_first,
            df$name_last
          )
        )

        # Use surname alone when the first name is absent.
        display_name[
          !nzchar(display_name)
        ] <- paste0(
          "Person ",
          df$id[
            !nzchar(display_name)
          ]
        )

        stats::setNames(
          as.character(df$id),
          display_name
        )
      })

      observe({
        current_selection <- isolate(
          input$author_select
        )

        choices <- persons_choices()

        selected_value <- if (
          !is.null(current_selection) &&
          length(current_selection) > 0L &&
          current_selection %in% unname(choices)
        ) {
          current_selection
        } else {
          character()
        }

        updateSelectInput(
          session,
          "author_select",
          choices = choices,
          selected = selected_value
        )
      })

      observeEvent(
        input$add_author,
        {
          req(
            input$author_select
          )

          selected_id <- suppressWarnings(
            as.integer(
              input$author_select
            )
          )

          req(
            !is.na(selected_id)
          )

          current <- author_list()

          if (
            selected_id %in%
              current$id
          ) {
            return()
          }

          new_author <- get_persons_by_ids(
            person_ids = selected_id,
            base_path = state$data_pond
          )

          if (
            is.null(new_author) ||
            nrow(new_author) == 0L
          ) {
            showNotification(
              "The selected person could not be loaded.",
              type = "error"
            )

            return()
          }

          new_author <- normalise_author_table(
            new_author
          )

          author_list(
            normalise_author_table(
              rbind(
                current,
                new_author
              )
            )
          )
        }
      )

      observeEvent(
        input$move_up,
        {
          req(
            input$author_select
          )

          selected_id <- suppressWarnings(
            as.integer(
              input$author_select
            )
          )

          df <- author_list()

          if (
            is.na(selected_id) ||
            nrow(df) < 2L
          ) {
            return()
          }

          idx <- match(
            selected_id,
            df$id
          )

          if (
            is.na(idx) ||
            idx <= 1L
          ) {
            return()
          }

          df[
            c(
              idx - 1L,
              idx
            ),
          ] <- df[
            c(
              idx,
              idx - 1L
            ),
          ]

          author_list(
            normalise_author_table(
              df
            )
          )
        }
      )

      observeEvent(
        input$move_down,
        {
          req(
            input$author_select
          )

          selected_id <- suppressWarnings(
            as.integer(
              input$author_select
            )
          )

          df <- author_list()

          if (
            is.na(selected_id) ||
            nrow(df) < 2L
          ) {
            return()
          }

          idx <- match(
            selected_id,
            df$id
          )

          if (
            is.na(idx) ||
            idx >= nrow(df)
          ) {
            return()
          }

          df[
            c(
              idx,
              idx + 1L
            ),
          ] <- df[
            c(
              idx + 1L,
              idx
            ),
          ]

          author_list(
            normalise_author_table(
              df
            )
          )
        }
      )

      observeEvent(
        input$remove_author,
        {
          req(
            input$author_select
          )

          selected_id <- suppressWarnings(
            as.integer(
              input$author_select
            )
          )

          if (is.na(selected_id)) {
            return()
          }

          df <- author_list()

          df <- df[
            df$id != selected_id,
            ,
            drop = FALSE
          ]

          author_list(
            normalise_author_table(
              df
            )
          )
        }
      )

      output$author_table <- renderTable(
        {
          df <- author_list()

          if (nrow(df) == 0L) {
            return(NULL)
          }

          display_df <- data.frame(
            Order = df$author_order,
            Author = df$name,
            stringsAsFactors = FALSE
          )

          display_df
        },
        striped = TRUE,
        bordered = TRUE,
        spacing = "s"
      )

      clear_authors <- function() {
        author_list(
          empty_author_table()
        )

        invisible(TRUE)
      }

      load_authors <- function(dataset_id) {
        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0L ||
          is.na(dataset_id[1])
        ) {
          clear_authors()

          return(
            invisible(FALSE)
          )
        }

        df <- get_dataset_authors(
          dataset_id = as.integer(
            dataset_id[1]
          ),
          base_path = state$data_pond
        )

        if (
          is.null(df) ||
          nrow(df) == 0L
        ) {
          clear_authors()

          return(
            invisible(TRUE)
          )
        }

        author_list(
          normalise_author_table(
            df
          )
        )

        invisible(TRUE)
      }

      authors <- reactive({
        author_list()
      })

      return(
        list(
          authors = authors,
          clear = clear_authors,
          load = load_authors
        )
      )
    }
  )
}