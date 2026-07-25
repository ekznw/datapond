load_schema_grouped <- function(path, output_path = NULL) {
  raw <- as.data.table(
    readxl::read_excel(path, sheet = "metadata_schema", skip = 5)
  )
  raw[, obligation := as.character(obligation)]
  # clean column names explicitly
  setnames(raw, old = c(
    "tbl",
    "field_code",
    "field_name_alias",
    "new_description",
    "in_app",
    "field_list_options",
    "tab",
    "subtab",
    "ui_type"
  ), new = c(
    "table",
    "field",
    "label",
    "description",
    "include",
    "options",
    "tab",
    "subtab",
    "type"
  ), skip_absent = TRUE)
  # filter only active fields + entry table
  schema <- raw[
    include == "T" & table == "entry_tbl",
    .(tab, subtab, field, label, type, description, options, obligation)
  ]
  # remove NA subtabs (important cleanup)
  schema <- schema[!is.na(subtab)]
  # split into nested list: tab -> subtab
  tabs <- unique(schema$tab)
  structured <- list()
  for (t in tabs) {
    subtabs <- unique(schema[tab == t]$subtab)
    structured[[t]] <- list()
    for (s in subtabs) {
      structured[[t]][[s]] <- schema[
        tab == t & subtab == s
      ]
    }
  }
  if (!is.null(output_path)) {
    saveRDS(structured, output_path)
    message("schema_grouped.rds created at ", output_path)
  }
  structured
}
