# One table-options menu for every interactive table in the application.
# Keep the callers' selection, filtering, formatting and row order intact.
datapond_datatable <- function(data, ..., options = list(), extensions = character(),
                              callback = NULL, table_tools = list()) {
  icon <- paste0(
    '<svg viewBox="0 0 20 20" aria-hidden="true" focusable="false" ',
    'fill="none" stroke="currentColor" stroke-width="1.5">',
    '<rect x="2" y="3" width="16" height="14" rx="1"/>',
    '<path d="M2 8h16M2 12.5h16M7.5 8v9M12.5 8v9"/></svg>'
  )
  old_buttons <- options$buttons
  if (is.null(old_buttons) || is.logical(old_buttons)) old_buttons <- list()
  if (is.character(old_buttons)) old_buttons <- as.list(old_buttons)
  # Existing export settings (for example a chosen filename) still apply.
  export_button <- function(kind) {
    matches <- Filter(function(button) {
      identical(if (is.character(button)) button else button$extend, kind)
    }, old_buttons)
    button <- if (length(matches) && is.list(matches[[1L]])) matches[[1L]] else list(extend = kind)
    if (is.null(button$exportOptions)) button$exportOptions <- list()
    if (is.null(button$exportOptions$columns)) button$exportOptions$columns <- ".datapond-table-public"
    if (is.null(button$exportOptions$orthogonal)) button$exportOptions$orthogonal <- "export"
    button
  }
  extras <- Filter(function(button) {
    !((if (is.character(button)) button else button$extend) %in% c("copy", "csv"))
  }, old_buttons)
  options$buttons <- list(list(
    extend = "collection", text = icon, className = "datapond-table-options",
    titleAttr = "Table options", attr = list(`aria-label` = "Table options"),
    collectionLayout = "datapond-table-menu", background = FALSE, autoClose = TRUE,
    buttons = c(list(
      export_button("copy"), export_button("csv"),
      list(extend = "", text = "Column widths", action = DT::JS(
        "function(e, table) { window.datapondTableTools.widths(table); }"
      )),
      list(extend = "", text = "Show / hide columns", className = "datapond-table-visibility", action = DT::JS(
        "function(e, table) { window.datapondTableTools.visibility(table); }"
      ))
    ), extras)
  ))
  dom <- options$dom %||% "lfrtip"
  if (!grepl("B", dom, fixed = TRUE)) dom <- paste0("B", dom)
  options$dom <- dom
  extensions <- unique(c(extensions, "Buttons"))
  # Binding first also supports an existing callback that ends with return.
  callback <- DT::JS(paste0(
    "window.datapondTableTools.bind(table, ",
    jsonlite::toJSON(table_tools, auto_unbox = TRUE), ");\n",
    paste(as.character(callback), collapse = "\n")
  ))
  widget <- DT::datatable(data, ..., options = options,
    extensions = extensions, callback = callback)
  widget$dependencies <- c(widget$dependencies, list(htmltools::htmlDependency(
    name = "datapond-table-tools", version = "1.4.0",
    src = c(file = system.file("www", package = "datapond", mustWork = TRUE)),
    script = "table-tools.js", stylesheet = "table-tools.css", all_files = FALSE
  )))
  widget
}

datapond_summary_table <- function(data) {
  if (is.null(data)) return(NULL)
  datapond_datatable(data, rownames = FALSE, selection = "none",
    class = "compact stripe hover", options = list(
      dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE
    ))
}
