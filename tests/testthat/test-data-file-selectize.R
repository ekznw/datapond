test_that("large file selectors search on the server and retain remote selections", {
  shiny::testServer(function(input, output, session) {
    registered <- new.env(parent = emptyenv())
    messages <- new.env(parent = emptyenv())
    session$registerDataObj <- function(name, data, filterFunc) {
      registered[[name]] <- list(data = data, filter = filterFunc)
      paste0("/choices/", name)
    }
    session$sendInputMessage <- function(inputId, message) {
      messages[[inputId]] <- message
    }
    update_select <- datapond:::data_files_select_updater(session)
    search_choices <- function(id, query = "") {
      entry <- registered[[id]]
      response <- entry$filter(entry$data, list(QUERY_STRING = paste0(
        "query=", utils::URLencode(query, reserved = TRUE),
        "&field=%5B%22label%22%5D&value=value&conju=and&maxop=100"
      )))
      jsonlite::fromJSON(response$content)
    }
  }, {
    choices <- stats::setNames(
      as.character(seq_len(10000)),
      sprintf("CHELSA/bio_%05d.tif", seq_len(10000))
    )
    for (id in c("saved_file_select", "spatial_record_select",
                 "manual_extent_data_file_id")) {
      available <- if (id == "manual_extent_data_file_id") {
        c("Dataset-level extent" = "", choices)
      } else {
        choices
      }
      expect_silent(update_select(id, choices = available, selected = "1"))
      expect_null(messages[[id]]$options)
      expect_identical(messages[[id]]$url, paste0("/choices/", id))
      expect_lte(nrow(search_choices(id)), 100L)
      expect_true("9999" %in% search_choices(id, "bio_09999")$value)

      # A table click or saved extent can select a value not yet in the browser.
      expect_silent(update_select(id, selected = "9999"))
      expect_identical(messages[[id]]$value, "9999")
      expect_true("9999" %in% search_choices(id)$value)
      expect_equal(nrow(registered[[id]]$data), length(available))
    }

    update_select("manual_extent_data_file_id", selected = "")
    expect_true("" %in% search_choices("manual_extent_data_file_id")$value)
    expect_identical(messages$manual_extent_data_file_id$value, "")

    # Switching to an empty dataset must also clear the server's choices.
    update_select("saved_file_select", choices = character(), selected = character())
    expect_equal(nrow(registered$saved_file_select$data), 0L)
    expect_length(messages$saved_file_select$value, 0L)
    update_select("saved_file_select", selected = "9999")
    expect_equal(nrow(registered$saved_file_select$data), 0L)
    expect_equal(nrow(registered$spatial_record_select$data), 10000L)
  })
})
