empty_author_editor_table <- function() {
  data.frame(
    id = integer(),
    name = character(),
    name_first = character(),
    name_last = character(),
    author_order = integer(),
    stringsAsFactors = FALSE
  )
}


normalise_author_editor_table <- function(df) {
  if (is.null(df) || nrow(as.data.frame(df)) == 0L) {
    return(empty_author_editor_table())
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!"id" %in% names(df)) stop("Author data requires an id column.")
  if (!"name_first" %in% names(df)) df$name_first <- ""
  if (!"name_last" %in% names(df)) df$name_last <- ""
  df$id <- suppressWarnings(as.integer(df$id))
  df$name_first <- ifelse(
    is.na(df$name_first),
    "",
    trimws(as.character(df$name_first))
  )
  df$name_last <- ifelse(
    is.na(df$name_last),
    "",
    trimws(as.character(df$name_last))
  )
  df$name <- trimws(paste(df$name_first, df$name_last))
  df$author_order <- seq_len(nrow(df))
  df[, c("id", "name", "name_first", "name_last", "author_order"), drop = FALSE]
}


move_author_editor_row <- function(df, selected_row, direction) {
  df <- normalise_author_editor_table(df)
  if (length(selected_row) != 1L) return(df)
  selected_row <- suppressWarnings(as.integer(selected_row[1]))
  direction <- match.arg(direction, c("up", "down"))
  if (is.na(selected_row) || nrow(df) < 2L) return(df)
  target <- if (identical(direction, "up")) selected_row - 1L else selected_row + 1L
  if (selected_row < 1L || selected_row > nrow(df) || target < 1L || target > nrow(df)) {
    return(df)
  }
  df[c(selected_row, target), ] <- df[c(target, selected_row), ]
  normalise_author_editor_table(df)
}


remove_author_editor_row <- function(df, selected_row) {
  df <- normalise_author_editor_table(df)
  if (length(selected_row) != 1L) return(df)
  selected_row <- suppressWarnings(as.integer(selected_row[1]))
  if (is.na(selected_row) || selected_row < 1L || selected_row > nrow(df)) return(df)
  normalise_author_editor_table(df[-selected_row, , drop = FALSE])
}


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
      "Add author"
    ),

    tags$small(
      style = "display:block; color:#666; margin:9px 0 6px 0;",
      "Select an author row below, then adjust its citation order or remove it."
    ),

    actionButton(
      ns("move_up"),
      "Move selected up"
    ),

    actionButton(
      ns("move_down"),
      "Move selected down"
    ),

    actionButton(
      ns("remove_author"),
      "Remove selected"
    ),

    DT::DTOutput(
      ns("author_table")
    )
  )
}


mod_authors_server <- function(id, state) {
  moduleServer(
    id,
    function(input, output, session) {

      empty_author_table <- empty_author_editor_table
      normalise_author_table <- normalise_author_editor_table

      # Author order is stored by row order in this reactiveVal.
      author_list <- reactiveVal(
        empty_author_table()
      )

      author_table_proxy <- DT::dataTableProxy(
        "author_table",
        session = session
      )

      select_author_row <- function(row) {
        session$onFlushed(
          function() {
            DT::selectRows(author_table_proxy, row)
          },
          once = TRUE
        )
        invisible(TRUE)
      }

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
          selected_row <- input$author_table_rows_selected
          df <- author_list()
          if (length(selected_row) != 1L || selected_row <= 1L) return()
          author_list(move_author_editor_row(df, selected_row, "up"))
          select_author_row(selected_row - 1L)
        }
      )

      observeEvent(
        input$move_down,
        {
          selected_row <- input$author_table_rows_selected
          df <- author_list()
          if (length(selected_row) != 1L || selected_row >= nrow(df)) return()
          author_list(move_author_editor_row(df, selected_row, "down"))
          select_author_row(selected_row + 1L)
        }
      )

      observeEvent(
        input$remove_author,
        {
          selected_row <- input$author_table_rows_selected
          df <- author_list()
          if (length(selected_row) != 1L) return()
          updated <- remove_author_editor_row(df, selected_row)
          author_list(updated)
          next_row <- min(selected_row, nrow(updated))
          select_author_row(if (next_row > 0L) next_row else NULL)
        }
      )

      output$author_table <- DT::renderDT({
        df <- author_list()
        display_df <- data.frame(
          Order = df$author_order,
          Author = df$name,
          stringsAsFactors = FALSE
        )
        DT::datatable(
          display_df,
          rownames = FALSE,
          selection = "single",
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = FALSE,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(
              list(width = "70px", targets = 0L),
              list(width = "auto", targets = 1L)
            )
          ),
          class = "compact stripe hover"
        )
      })

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
