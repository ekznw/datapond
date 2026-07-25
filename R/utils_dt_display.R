# utils_dt_display.R
#
# Shared user-facing labels and rendering helpers for DT tables.

friendly_field_labels <- function() {
  c(
    selected = "Selected",
    id = "ID",
    dataset_id = "Dataset ID",
    data_pond_id = "Data pond ID",
    dataset_uuid = "Dataset UUID",
    uuid = "Dataset UUID",
    folder_key = "Dataset folder key",
    citation_key = "Citation key",

    title = "Title",
    dataset_title = "Dataset title",
    authors = "Authors",
    author_names = "Authors",
    organisation_name = "Organisation",

    dataset_path = "Dataset folder",
    data_path = "Data folder",
    lifecycle_stage = "Lifecycle stage",
    stage = "Pond location",
    dataset_status = "Dataset status",
    lifecycle_status = "Lifecycle status code",
    lifecycle_label = "Lifecycle status",
    folder_locked = "Folder lock",
    ready_for_archive = "Archive readiness",
    next_action = "Recommended next action",
    lifecycle_stage_mismatch =
      "Location / status mismatch",
    source_dataset_id = "Source dataset ID",
    released_at = "Released at",
    released_by_person_id = "Released by person ID",
    archived_at = "Archived at",
    archived_by_person_id = "Archived by person ID",
    item = "Folder attribute",
    current = "Current value",
    proposed = "Proposed value",

    created_at = "Created",
    updated_at = "Updated",

    review_timestamp = "Review date and time",
    review_date = "Review date",
    reviewer_id = "Reviewer ID",
    reviewer_name = "Reviewer",
    reviewer_role = "Reviewer role",
    update_type = "Update type",
    update_reason = "Reason for update",
    update_summary = "Update summary",
    metadata_section = "Metadata section",
    metadata_status = "Metadata status",
    source_of_change = "Source of change",
    app_version = "Application version",
    notes = "Notes",

    file_name = "File",
    data_file_rel_path_full = "Relative path",
    data_file_format = "Format",
    file_role = "File role",
    file_size_kb = "Size (KB)",
    modified_time = "Modified",
    scan_status = "Scan status",
    active = "Active",
    last_seen_at = "Last seen",
    missing_since = "Missing since",

    layer_name = "Layer",
    spatial_status = "Extraction status",
    spatial_type = "Spatial type",
    crs_auth_code = "CRS authority code",
    crs_name = "CRS name",
    source_crs = "Source CRS",
    geometry_type = "Geometry type",
    feature_count = "Feature count",
    bbox_native = "Native bounding box",
    bbox_wgs84 = "EPSG:4326 bounding box",
    bbox_status = "Bounding-box status",
    include_in_dataset_bbox =
      "Include in dataset bounding box",

    raster_nrow = "Raster rows",
    raster_ncol = "Raster columns",
    raster_nlyr = "Raster layers",
    raster_res_x = "Cell size X",
    raster_res_y = "Cell size Y",

    data_field_name = "Field name",
    data_field_alias = "Field alias",
    data_field_desc = "Field description",
    data_field_units = "Units",
    attribute_type = "Attribute type",
    example_values = "Example values",
    value_count = "Value count",
    missing_count = "Missing values",
    unique_count = "Unique values",
    min_value = "Minimum value",
    max_value = "Maximum value",
    legend_field = "Legend field",
    legend_field_notes = "Legend notes",
    profile_updated_at = "Profile updated"
  )
}


title_case_display_text <- function(x) {
  x <- gsub(
    "_",
    " ",
    as.character(x),
    fixed = TRUE
  )

  x <- tools::toTitleCase(
    tolower(x)
  )

  acronym_replacements <- c(
    "\\bId\\b" = "ID",
    "\\bUuid\\b" = "UUID",
    "\\bUrl\\b" = "URL",
    "\\bCrs\\b" = "CRS",
    "\\bEpsg\\b" = "EPSG",
    "\\bWgs84\\b" = "WGS84",
    "\\bXmin\\b" = "X minimum",
    "\\bXmax\\b" = "X maximum",
    "\\bYmin\\b" = "Y minimum",
    "\\bYmax\\b" = "Y maximum"
  )

  for (
    pattern in names(
      acronym_replacements
    )
  ) {
    x <- gsub(
      pattern,
      acronym_replacements[[pattern]],
      x
    )
  }

  x
}


friendly_field_label <- function(fields) {
  fields <- as.character(
    fields
  )

  label_map <-
    friendly_field_labels()

  mapped <- unname(
    label_map[fields]
  )

  missing_label <- (
    is.na(mapped) |
      !nzchar(mapped)
  )

  mapped[missing_label] <-
    title_case_display_text(
      fields[missing_label]
    )

  mapped
}


humanise_code_value <- function(x) {
  vapply(
    x,
    function(value) {
      if (
        is.null(value) ||
        length(value) == 0L ||
        is.na(value) ||
        !nzchar(
          trimws(
            as.character(value)
          )
        )
      ) {
        return("")
      }

      parts <- strsplit(
        as.character(value),
        ";",
        fixed = TRUE
      )[[1]]

      parts <- trimws(parts)

      parts <- parts[
        nzchar(parts)
      ]

      paste(
        title_case_display_text(
          parts
        ),
        collapse = "; "
      )
    },
    character(1)
  )
}


clean_dt_widget <- function(
  df,
  hidden_columns = character(),
  column_widths = NULL,
  page_length = 10L,
  selection = "none",
  filter = "top",
  buttons = TRUE,
  scroll_x = TRUE
) {
  if (
    is.null(df) ||
    nrow(as.data.frame(df)) == 0L
  ) {
    df <- data.frame(
      message = "No records are available.",
      stringsAsFactors = FALSE
    )
  }

  df <- as.data.frame(
    df,
    stringsAsFactors = FALSE
  )

  is_message_table <- identical(
    names(df),
    "message"
  )

  if (isTRUE(is_message_table)) {
    names(df) <- "Message"

    return(
      DT::datatable(
        df,
        rownames = FALSE,
        selection = "none",
        escape = TRUE,
        options = list(
          dom = "t",
          ordering = FALSE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        )
      )
    )
  }

  original_names <- names(df)

  hidden_targets <- which(
    original_names %in%
      hidden_columns
  ) - 1L

  names(df) <- make.unique(
    friendly_field_label(
      original_names
    )
  )

  column_defs <- list()

  if (length(hidden_targets) > 0L) {
    column_defs[[length(column_defs) + 1L]] <- list(
      visible = FALSE,
      targets = as.integer(
        hidden_targets
      )
    )
  }

  if (
    !is.null(column_widths) &&
    length(column_widths) > 0L
  ) {
    width_fields <- intersect(
      names(column_widths),
      original_names
    )

    for (field in width_fields) {
      column_defs[[length(column_defs) + 1L]] <- list(
        width = unname(
          column_widths[[field]]
        ),
        targets = match(
          field,
          original_names
        ) - 1L
      )
    }
  }

  options <- list(
    pageLength =
      as.integer(
        page_length
      ),
    scrollX =
      isTRUE(
        scroll_x
      ),
    autoWidth = TRUE,
    dom = if (isTRUE(buttons)) {
      "Bfrtip"
    } else {
      "frtip"
    },
    columnDefs =
      column_defs
  )

  args <- list(
    data = df,
    rownames = FALSE,
    selection = selection,
    filter = filter,
    escape = TRUE,
    options = options
  )

  if (isTRUE(buttons)) {
    args$extensions <- "Buttons"
    args$options$buttons <- c(
      "copy",
      "csv"
    )
  }

  do.call(
    DT::datatable,
    args
  )
}

app_earth_palette <- function() {
  c(
    forest = "#526657",
    deep_forest = "#3F5146",
    sage = "#A8B49A",
    pale_sage = "#D1DDC7",
    moss = "#87966C",
    olive = "#A7A06A",
    ochre = "#C3A35B",
    pale_ochre = "#E9DFB5",
    sand = "#E4D6B8",
    clay = "#C98F78",
    terracotta = "#B8755E",
    bark = "#806855",
    tan = "#B99572",
    stone = "#D3CEC4",
    parchment = "#F1E9DA",
    cream = "#F8F4EA",
    ink = "#403B35",
    muted_ink = "#625C52"
  )
}

dataset_status_palette <- function() {
  earth <- app_earth_palette()

  c(
    complete = unname(earth[["sage"]]),
    partial = unname(earth[["pale_ochre"]]),
    needs_review = unname(earth[["clay"]]),
    not_started = unname(earth[["stone"]]),
    not_applicable = unname(earth[["parchment"]]),
    draft = unname(earth[["sand"]]),
    released = unname(earth[["sage"]]),
    archived = unname(earth[["tan"]])
  )
}


dataset_status_border_palette <- function() {
  earth <- app_earth_palette()

  c(
    complete = unname(earth[["forest"]]),
    partial = unname(earth[["ochre"]]),
    needs_review = unname(earth[["terracotta"]]),
    not_started = unname(earth[["bark"]]),
    not_applicable = unname(earth[["olive"]]),
    draft = unname(earth[["ochre"]]),
    released = unname(earth[["forest"]]),
    archived = unname(earth[["bark"]])
  )
}