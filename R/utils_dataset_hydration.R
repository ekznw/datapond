# utils_dataset_hydration.R
# Helpers for loading saved dataset values back into Shiny inputs ----

split_stored_multi_value <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(character())
  x <- as.character(x[1])
  if (trimws(x) == "") return(character())
  trimws(unlist(strsplit(x, ";|,")))
}

hydrate_one_input <- function(session, field, type, value,
                              get_choices_fn = NULL) {
  if (is.null(field) || length(field) == 0 || is.na(field) || field == "") {
    return(invisible(FALSE))
  }

  if (field == "version") {
    set_version_inputs(session, value %||% "0.1.0")
    return(invisible(TRUE))
  }

  if (grepl("^keyword_", field)) {
    selected <- split_stored_multi_value(value)
    vocab_field <- keyword_vocab_field_code(field)

    choices <- if (
      !is.null(vocab_field) &&
      is.function(get_choices_fn)
    ) {
      get_choices_fn(vocab_field)
    } else {
      NULL
    }

    merged <- merge_keyword_choices(
      choices = choices,
      selected = selected
    )

    updateSelectizeInput(
      session,
      field,
      choices = merged$choices,
      selected = merged$selected,
      server = TRUE
    )

    return(invisible(TRUE))
  }

  if (is.null(value) || length(value) == 0 || all(is.na(value))) {
    value <- ""
  } else {
    value <- value[1]
  }

  if (identical(type, "date")) {
    updateDateInput(
      session,
      field,
      value = if (non_empty_scalar(value)) {
        as.Date(value)
      } else {
        NULL
      }
    )
  } else if (identical(type, "textarea")) {
    updateTextAreaInput(session, field, value = as.character(value %||% ""))
  } else if (identical(type, "select")) {
    updateSelectInput(session, field, selected = as.character(value %||% ""))
  } else if (identical(type, "checkbox")) {
    updateCheckboxInput(
      session,
      field,
      value = isTRUE(value == 1 || value == TRUE || value == "TRUE" || value == "true")
    )
  } else {
    updateTextInput(session, field, value = as.character(value %||% ""))
  }

  invisible(TRUE)
}

keyword_vocab_field_code <- function(field) {
  vocab_fields <- c(
    "keyword_object",
    "keyword_discipline",
    "keyword_topic",
    "keyword_geo"
  )

  match <- vocab_fields[startsWith(field, vocab_fields)]

  if (length(match) == 0L) {
    return(NULL)
  }

  match[1]
}


merge_keyword_choices <- function(choices, selected) {
  selected <- unique(
    trimws(as.character(selected))
  )

  selected <- selected[
    !is.na(selected) & nzchar(selected)
  ]

  if (is.null(choices) || length(choices) == 0L) {
    choices <- stats::setNames(character(), character())
  }

  choices <- choices[
    !is.na(choices) & nzchar(as.character(choices))
  ]

  approved_values <- as.character(unname(choices))

  if (length(approved_values) > 0L && length(selected) > 0L) {
    matched <- match(
      tolower(selected),
      tolower(approved_values)
    )

    selected[!is.na(matched)] <- approved_values[
      matched[!is.na(matched)]
    ]
  }

  extra <- selected[
    !tolower(selected) %in% tolower(approved_values)
  ]

  if (length(extra) > 0L) {
    choices <- c(
      choices,
      stats::setNames(extra, extra)
    )
  }

  list(
    choices = choices,
    selected = unique(selected)
  )
}

hydrate_dataset_overview_forms <- function(
  session,
  rec,
  citation_dt,
  about_dt,
  get_choices_fn = NULL
) {
  if (is.null(rec) || nrow(as.data.frame(rec)) == 0) {
    return(invisible(FALSE))
  }

  dt <- data.table::rbindlist(
    list(
      data.table::as.data.table(citation_dt),
      data.table::as.data.table(about_dt)
    ),
    fill = TRUE
  )

  dt <- dt[
    !is.na(field) &
      field != "" &
      !is.na(type) &
      !type %ilike% "hidden"
  ]

  for (i in seq_len(nrow(dt))) {
    f <- dt$field[i]
    typ <- as.character(dt$type[i])

    if (f %in% names(rec)) {
      hydrate_one_input(
        session = session,
        field = f,
        type = typ,
        value = rec[[f]][1],
        get_choices_fn = get_choices_fn
      )
    }
  }

  invisible(TRUE)
}

dataset_overview_clear_defaults <- function(
  current_date = Sys.Date()
) {
  list(
    temporal_coverage_start_date =
      as.Date(current_date),

    temporal_coverage_end_date =
      as.Date(current_date)
  )
}

clear_one_input <- function(
  session,
  field,
  type,
  default_value = NULL
) {
  if (
    is.null(field) ||
    length(field) == 0L ||
    is.na(field[1]) ||
    !nzchar(
      trimws(
        as.character(
          field[1]
        )
      )
    )
  ) {
    return(
      invisible(FALSE)
    )
  }

  field <- as.character(
    field[1]
  )

  type <- if (
    is.null(type) ||
    length(type) == 0L ||
    is.na(type[1])
  ) {
    ""
  } else {
    as.character(
      type[1]
    )
  }

  if (identical(
    field,
    "version"
  )) {
    set_version_inputs(
      session,
      "0.1.0"
    )

    return(
      invisible(TRUE)
    )
  }

  if (grepl(
    "^keyword_",
    field
  )) {
    shiny::updateSelectizeInput(
      session,
      field,
      selected = character(0)
    )

    return(
      invisible(TRUE)
    )
  }

  if (identical(
    type,
    "date"
  )) {
    default_text <- if (
      is.null(default_value) ||
      length(default_value) == 0L ||
      all(is.na(default_value))
    ) {
      ""
    } else {
      trimws(
        as.character(
          default_value[1]
        )
      )
    }

    # A blank date cannot be sent through updateDateInput():
    # NULL is ignored and an NA Date generates a coercion warning.
    # Send JSON null directly to the Shiny date-input binding.
    if (!nzchar(default_text)) {
      session$sendInputMessage(
        field,
        list(
          value = NULL
        )
      )

      return(
        invisible(TRUE)
      )
    }

    date_value <- suppressWarnings(
      as.Date(
        default_text
      )
    )

    # Treat invalid defaults as blank dates.
    if (is.na(date_value)) {
      session$sendInputMessage(
        field,
        list(
          value = NULL
        )
      )
    } else {
      shiny::updateDateInput(
        session,
        field,
        value = date_value
      )
    }

    return(
      invisible(TRUE)
    )
  }

  if (identical(
    type,
    "textarea"
  )) {
    shiny::updateTextAreaInput(
      session,
      field,
      value = ""
    )

    return(
      invisible(TRUE)
    )
  }

  if (identical(
    type,
    "select"
  )) {
    shiny::updateSelectInput(
      session,
      field,
      selected = character(0)
    )

    return(
      invisible(TRUE)
    )
  }

  if (identical(
    type,
    "checkbox"
  )) {
    shiny::updateCheckboxInput(
      session,
      field,
      value = FALSE
    )

    return(
      invisible(TRUE)
    )
  }

  shiny::updateTextInput(
    session,
    field,
    value = ""
  )

  invisible(TRUE)
}

clear_dataset_overview_forms <- function(session, citation_dt, about_dt, keep_fields = character()) {
  dt <- data.table::rbindlist(
    list(
      data.table::as.data.table(citation_dt),
      data.table::as.data.table(about_dt)
    ),
    fill = TRUE
  )

  dt <- dt[
    !is.na(field) &
      field != "" &
      !is.na(type) &
      !type %ilike% "hidden"
  ]

  for (i in seq_len(nrow(dt))) {
    f <- dt$field[i]
    if (f %in% keep_fields) next
    clear_one_input(session, f, as.character(dt$type[i]))
  }

  invisible(TRUE)
}

load_selected_dataset_into_state <- function(dataset_id, base_path, state) {
  if (is.null(dataset_id) ||
      length(dataset_id) == 0 ||
      is.na(dataset_id)) {
    return(FALSE)
  }
  rec <- get_dataset_record(dataset_id, base_path)
  if (is.null(rec)) {
    return(FALSE)
  }
  state$current_dataset_id <- dataset_id
  stable_uuid <- rec$uuid %||% NULL
  if (is.null(stable_uuid) ||
      length(stable_uuid) == 0 ||
      is.na(stable_uuid) ||
      stable_uuid == "") {
    stable_uuid <- get_or_create_dataset_uuid(dataset_id, base_path)
  }
  state$current_dataset_uuid <- stable_uuid
  state$current_citation_key <- rec$citation_key %||%
    extract_citation_key_from_folder_name(rec$dataset_path %||% NULL)
  state$current_folder_key <- folder_key_text(
    rec$folder_key,
    default =
      extract_folder_key_from_folder_name(
        rec$dataset_path %||% NULL
      )
  )
  state$dataset_path <- rec$dataset_path %||% NULL
  state$current_dataset_folder <- rec$dataset_path %||% NULL
  if (!is.null(rec$data_path) &&
      length(rec$data_path) > 0 &&
      !is.na(rec$data_path) &&
      rec$data_path != "") {
    state$data_path <- rec$data_path
  } else if (!is.null(state$dataset_path) &&
            length(state$dataset_path) > 0 &&
            !is.na(state$dataset_path) &&
            state$dataset_path != "") {
    state$data_path <- file.path(state$dataset_path, "data")
  } else {
    state$data_path <- NULL
  }
  state$refresh_lineage <- state$refresh_lineage + 1L
  state$refresh_supporting_docs <- state$refresh_supporting_docs + 1L
  state$refresh_data_files_saved <- state$refresh_data_files_saved + 1L
  state$refresh_attributes <- state$refresh_attributes + 1L
  state$refresh_metadata_reviews <- state$refresh_metadata_reviews + 1L
  invisible(TRUE)
}

clear_current_dataset_context <- function(
  state,
  refresh = TRUE
) {
  state$current_dataset_id <- NULL
  state$current_dataset_uuid <- NULL
  state$current_citation_key <- NULL
  state$current_folder_key <- NULL
  state$current_dataset_folder <- NULL
  state$dataset_path <- NULL
  state$data_path <- NULL

  if (isTRUE(refresh)) {
    state$refresh_lineage <-
      state$refresh_lineage + 1L
    state$refresh_supporting_docs <-
      state$refresh_supporting_docs + 1L
    state$refresh_data_files_saved <-
      state$refresh_data_files_saved + 1L
    state$refresh_attributes <-
      state$refresh_attributes + 1L
    state$refresh_metadata_reviews <-
      state$refresh_metadata_reviews + 1L
    state$refresh_datasets <-
      state$refresh_datasets + 1L
  }

  invisible(TRUE)
}