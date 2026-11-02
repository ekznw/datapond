test_that("shared table tools preserve table behaviour and export settings", {
  widget <- datapond_datatable(iris, rownames = FALSE, selection = "single",
    filter = "top", callback = DT::JS("window.originalCallbackRan = true;"),
    options = list(pageLength = 7L, order = list(list(2L, "desc")),
      dom = "frtip", columnDefs = list(list(targets = 0L, visible = FALSE)),
      buttons = list(list(extend = "csv", filename = "my-export"), "print")))
  expect_identical(widget$x$options$pageLength, 7L)
  expect_identical(widget$x$options$order, list(list(2L, "desc")))
  expect_identical(widget$x$selection$mode, "single")
  expect_identical(widget$x$filter, "top")
  expect_false(widget$x$options$columnDefs[[1]]$visible)
  expect_identical(widget$x$options$dom, "Bfrtip")
  expect_length(widget$x$options$buttons, 1L)
  menu <- widget$x$options$buttons[[1L]]
  expect_identical(menu$className, "datapond-table-options")
  expect_identical(menu$buttons[[2L]]$filename, "my-export")
  expect_identical(menu$buttons[[1L]]$exportOptions$columns, ".datapond-table-public")
  expect_identical(menu$buttons[[5L]], "print")
  expect_match(as.character(widget$x$callback), "window.originalCallbackRan = true", fixed = TRUE)
  dependency <- Filter(function(x) x$name == "datapond-table-tools", widget$dependencies)[[1L]]
  expect_true(file.exists(file.path(dependency$src$file, dependency$script)))
  expect_true(file.exists(file.path(dependency$src$file, dependency$stylesheet)))
})

test_that("summary and formerly toolbar-free tables receive the same menu", {
  summary <- datapond_summary_table(data.frame(Name = "Example", Value = "Text"))
  expect_identical(summary$x$options$dom, "Bt")
  expect_false(summary$x$options$paging)
  expect_false(summary$x$options$ordering)
  expect_identical(summary$x$selection$mode, "none")
  expect_null(datapond_summary_table(NULL))
  widget <- clean_dt_widget(data.frame(id = 1, name = "Example"),
    hidden_columns = "id", buttons = FALSE, selection = "multiple")
  expect_length(widget$x$options$buttons, 1L)
  expect_identical(widget$x$selection$mode, "multiple")
  expect_false(widget$x$options$columnDefs[[1L]]$visible)
})
