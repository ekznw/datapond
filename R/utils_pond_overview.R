# utils_pond_overview.R
#
# Dataset-level overview helpers for the active data pond.

pond_overview_palette <- function() {
  c(
    "Archive" =
      "#B99572",

    "Development \u2014 ready for archive" =
      "#A8C49A",

    "Development \u2014 in progress" =
      "#D1DDC7",

    "Source \u2014 fully described" =
      "#C3A35B",

    "Source \u2014 description incomplete" =
      "#E9DFB5",

    "Location / lifecycle mismatch" =
      "#C98F78",

    "Unclassified location" =
      "#D3CEC4"
  )
}


pond_stage_label <- function(stage) {
  stage <- tolower(
    trimws(
      as.character(stage)
    )
  )

  data.table::fcase(
    stage == "src",
    "Source (src/)",

    stage == "dev",
    "Development (dev/)",

    stage == "arc",
    "Archive (arc/)",

    default =
      "Unknown location"
  )
}


pond_overview_class <- function(
  stage,
  ready_for_archive,
  lifecycle_stage_mismatch
) {
  stage <- tolower(
    trimws(
      as.character(stage)
    )
  )

  ready_for_archive <-
    ready_for_archive %in% TRUE

  lifecycle_stage_mismatch <-
    lifecycle_stage_mismatch %in% TRUE

  data.table::fcase(
    lifecycle_stage_mismatch,
    "Location / lifecycle mismatch",

    stage == "arc",
    "Archive",

    stage == "dev" &
      ready_for_archive,
    "Development \u2014 ready for archive",

    stage == "dev",
    "Development \u2014 in progress",

    stage == "src" &
      ready_for_archive,
    "Source \u2014 fully described",

    stage == "src",
    "Source \u2014 description incomplete",

    default =
      "Unclassified location"
  )
}


get_dataset_overview_bbox <- function(
  dataset_id,
  base_path
) {
  summary <- tryCatch(
    get_dataset_spatial_summary(
      dataset_id = dataset_id,
      base_path = base_path
    ),
    error = function(e) {
      NULL
    }
  )

  if (
    is.null(summary) ||
    nrow(as.data.frame(summary)) == 0L
  ) {
    return(
      data.table::data.table(
        id = as.integer(dataset_id),
        bbox_status = "not_available",
        bbox_message =
          "No derived dataset bounding box is available.",
        bbox_wgs84 = NA_character_,
        west = NA_real_,
        south = NA_real_,
        east = NA_real_,
        north = NA_real_,
        bbox_available = FALSE
      )
    )
  }

  status <- as.character(
    summary$bbox_status[1] %||%
      "not_available"
  )

  west <- suppressWarnings(
    as.numeric(
      summary$xmin[1]
    )
  )

  south <- suppressWarnings(
    as.numeric(
      summary$ymin[1]
    )
  )

  east <- suppressWarnings(
    as.numeric(
      summary$xmax[1]
    )
  )

  north <- suppressWarnings(
    as.numeric(
      summary$ymax[1]
    )
  )

  valid_bbox <- (
    identical(status, "valid") &&
      all(
        is.finite(
          c(
            west,
            south,
            east,
            north
          )
        )
      )
  )

  status_message <- if (
    "status_message" %in% names(summary)
  ) {
    as.character(
      summary$status_message[1]
    )
  } else {
    NA_character_
  }

  bbox_text <- if (
    "bbox_wgs84" %in% names(summary)
  ) {
    as.character(
      summary$bbox_wgs84[1]
    )
  } else {
    NA_character_
  }

  data.table::data.table(
    id = as.integer(dataset_id),
    bbox_status = status,
    bbox_message = status_message,
    bbox_wgs84 = bbox_text,
    west = west,
    south = south,
    east = east,
    north = north,
    bbox_available = valid_bbox
  )
}


build_pond_overview_data <- function(
  base_path,
  schema_grouped,
  include_hidden = FALSE
) {
  datasets <- get_datasets_with_authors(
    base_path,
    include_hidden = include_hidden
  )

  if (
    is.null(datasets) ||
    nrow(as.data.frame(datasets)) == 0L
  ) {
    return(
      data.table::data.table()
    )
  }

  datasets <- data.table::as.data.table(
    data.table::copy(datasets)
  )

  lifecycle <- tryCatch(
    get_lifecycle_dashboard(
      base_path = base_path,
      schema_grouped = schema_grouped
    ),
    error = function(e) {
      data.table::data.table()
    }
  )

  lifecycle <- data.table::as.data.table(
    data.table::copy(lifecycle)
  )

  if (nrow(lifecycle) > 0L) {
    overview <- merge(
      datasets,
      lifecycle,
      by = "id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    overview <- data.table::copy(
      datasets
    )
  }

  required_columns <- list(
    title = NA_character_,
    authors = NA_character_,
    citation_date = NA_character_,
    version = NA_character_,
    abstract = NA_character_,
    citation_key = NA_character_,
    dataset_path = NA_character_,
    lifecycle_status = "draft",
    lifecycle_label = "Draft",
    stage = NA_character_,
    ready_for_archive = FALSE,
    next_action = "Complete metadata",
    lifecycle_stage_mismatch = FALSE,
    organisations = NA_character_,
    poc = NA_character_,
    record_owner = NA_character_,
    is_hidden = 0L,
    deleted_at = NA_character_,
    location_status = "not_scanned"
  )

  for (field in names(required_columns)) {
    if (!field %in% names(overview)) {
      overview[
        ,
        (field) :=
          required_columns[[field]]
      ]
    }
  }

  overview[
    ,
    stage := tolower(
      trimws(
        as.character(stage)
      )
    )
  ]

  overview[
    ,
    pond_location :=
      pond_stage_label(stage)
  ]

  overview[
    ,
    metadata_readiness :=
      data.table::fifelse(
        ready_for_archive %in% TRUE,
        "Required metadata complete",
        "Metadata incomplete"
      )
  ]

  overview[
    ,
    overview_class :=
      pond_overview_class(
        stage = stage,
        ready_for_archive =
          ready_for_archive,
        lifecycle_stage_mismatch =
          lifecycle_stage_mismatch
      )
  ]

  overview[
    ,
    display_title :=
      data.table::fcase(
        !is.na(title) &
          nzchar(trimws(title)),
        as.character(title),

        !is.na(citation_key) &
          nzchar(trimws(citation_key)),
        as.character(citation_key),

        default =
          paste(
            "Dataset",
            id
          )
      )
  ]

  bbox_rows <- data.table::rbindlist(
    lapply(
      overview$id,
      function(dataset_id) {
        get_dataset_overview_bbox(
          dataset_id = dataset_id,
          base_path = base_path
        )
      }
    ),
    fill = TRUE
  )

  overview <- merge(
    overview,
    bbox_rows,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )

  overview[
    ,
    spatial_extent :=
      data.table::fifelse(
        bbox_available %in% TRUE,
        "Bounding box available",
        "No valid bounding box"
      )
  ]

  overview[
    ,
    record_visibility := data.table::fcase(
      !is.na(deleted_at) & nzchar(trimws(as.character(deleted_at))),
      "Deleted (recoverable)",
      as.integer(is_hidden) == 1L,
      "Hidden",
      default = "Visible"
    )
  ]

  data.table::setorder(
    overview,
    -id
  )

  overview[]
}


pond_overview_search_index <- function(overview) {
  dt <- data.table::as.data.table(overview)
  if (!nrow(dt)) return(character())
  fields <- intersect(
    c(
      "display_title", "authors", "organisations", "poc",
      "citation_key", "pond_location", "lifecycle_label",
      "metadata_readiness", "next_action", "dataset_path",
      "record_owner", "record_visibility", "location_status",
      "title", "pond_name", "connection_type", "uuid", "stage", "availability"
    ),
    names(dt)
  )
  if (!length(fields)) return(rep("", nrow(dt)))
  tolower(apply(
    as.data.frame(dt[, ..fields]),
    1L,
    function(row) paste(row[!is.na(row)], collapse = " ")
  ))
}

filter_pond_overview_data <- function(overview, search_text = NULL) {
  dt <- data.table::as.data.table(overview)
  if (!nrow(dt)) return(dt)
  search_text <- trimws(as.character(search_text[1] %||% ""))
  if (is.na(search_text) || !nzchar(search_text)) return(dt)
  haystack <- if (".search_text" %in% names(dt)) dt$.search_text else pond_overview_search_index(dt)
  terms <- strsplit(tolower(search_text), "\\s+")[[1L]]
  keep <- rep(TRUE, length(haystack))
  for (term in terms) keep <- keep & grepl(term, haystack, fixed = TRUE)
  dt[keep]
}

# Connected rows intentionally have no active-pond record id: ids are only
# unique within a pond and must never be passed into the current editor.
merge_pond_overview_inventory <- function(current, connected = NULL, pond_name, pond_path) {
  current <- data.table::as.data.table(data.table::copy(current))
  if (nrow(current)) {
    current[, `:=`(active_pond = TRUE, pond_name = pond_name, connection_type = "Current pond",
      open_path = pond_path, availability = "Current pond", refreshed = "Current catalogue")]
    current[, record_key := paste("current", pond_path, id, sep = "|")]
  }
  other <- data.table::as.data.table(data.table::copy(connected))
  if (nrow(other)) {
    other[, `:=`(active_pond = FALSE, id = NA_integer_, display_title = title,
      authors = NA_character_, organisations = NA_character_,
      overview_class = "Connected pond (read-only)",
      lifecycle_label = pond_stage_label(stage), metadata_readiness = "Not assessed here",
      next_action = ifelse(connection_type == "Local pond", "Open pond to view / edit", "Open in Flow to inspect / download"),
      spatial_extent = "Not mapped here")]
    other[, record_key := paste(connection_type, open_path, uuid, dataset_path, sep = "|")]
  }
  rows <- data.table::rbindlist(list(current, other), fill = TRUE, use.names = TRUE)
  if (nrow(rows)) {
    for (field in c("citation_date", "version")) if (!field %in% names(rows)) rows[, (field) := NA_character_]
    rows[, .search_text := pond_overview_search_index(rows)]
  }
  rows
}


pond_overview_author_label <- function(authors) {
  vapply(
    authors,
    function(value) {
      if (is.null(value) || length(value) == 0L || is.na(value[1])) {
        return(NA_character_)
      }
      value <- trimws(as.character(value[1]))
      if (!nzchar(value)) return(NA_character_)
      names <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
      names <- names[nzchar(names)]
      if (length(names) == 0L) return(NA_character_)
      if (length(names) == 1L) names[1] else paste(names[1], "et al.")
    },
    character(1),
    USE.NAMES = FALSE
  )
}


pond_overview_inventory_display <- function(overview) {
  overview <- data.table::as.data.table(
    data.table::copy(overview)
  )
  if (!"active_pond" %in% names(overview)) overview[, active_pond := TRUE]
  if (!"pond_name" %in% names(overview)) overview[, pond_name := "Current pond"]
  for (field in c("availability", "refreshed")) if (!field %in% names(overview)) overview[, (field) := NA_character_]

  overview[
    ,
    .(
      `Dataset ID` = id,
      `Overview class` = overview_class,
      Title = display_title,
      Authors = pond_overview_author_label(authors),
      `Lifecycle status` = lifecycle_label,
      `Metadata readiness` = metadata_readiness,
      `Next action` = next_action,
      `Spatial extent` = spatial_extent,
      Version = version,
      `Citation date` = citation_date,
      Organisations = organisations,
      Pond = ifelse(active_pond, paste0("\u2605 Current \u2014 ", pond_name), pond_name),
      `Files / cache` = availability,
      `Metadata refreshed` = refreshed
    )
  ]
}


pond_overview_responsive_column_defs <- function() {
  list(
    list(
      visible = FALSE,
      targets = c(0L, 1L)
    ),
    list(
      className = "all",
      responsivePriority = 1L,
      width = "300px",
      targets = 2L
    ),
    list(
      className = "all",
      responsivePriority = 2L,
      width = "190px",
      targets = 3L
    ),
    list(
      className = "all",
      responsivePriority = 3L,
      width = "110px",
      targets = 9L
    ),
    list(responsivePriority = 4L, targets = 4L),
    list(responsivePriority = 5L, targets = 5L),
    list(responsivePriority = 6L, targets = 6L),
    list(responsivePriority = 7L, targets = 8L),
    list(responsivePriority = 8L, targets = 10L),
    list(responsivePriority = 9L, targets = 7L),
    list(className = "all", responsivePriority = 4L, targets = 11L),
    list(responsivePriority = 10L, targets = 12L),
    list(responsivePriority = 11L, targets = 13L)
  )
}


pond_overview_responsive_table_options <- function() {
  list(
    pageLength = 15L,
    deferRender = TRUE,
    order = list(),
    scrollX = FALSE,
    autoWidth = FALSE,
    dom = "Bfrtip",
    buttons = c("copy", "csv"),
    responsive = list(details = FALSE),
    columnDefs = pond_overview_responsive_column_defs()
  )
}


pond_overview_table_extensions <- function() {
  c("Buttons", "Responsive")
}

# Fetch only the selected connected record's abstract from local metadata.
# Never fetch dataset files or contact the remote service for a preview.
read_connected_overview_abstract <- function(selected) {
  access_require("read", selected$open_path)
  path <- selected$open_path
  if (identical(selected$connection_type, "Remote cache (read-only)")) {
    current <- movement_cache_current(path)
    if (is.null(current)) stop("The metadata cache is unavailable. Refresh it in Flow.")
    path <- current$directory
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(path), flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(conn))
  fields <- DBI::dbListFields(conn, "entry_tbl")
  if (!"abstract" %in% fields) return(NULL)
  query <- paste0("SELECT ", if ("uuid" %in% fields) "uuid" else "NULL AS uuid", ", abstract FROM entry_tbl WHERE id = ?")
  row <- DBI::dbGetQuery(conn, query,
    params = list(selected$source_record_id))
  if (!nrow(row) || (!is.na(selected$uuid) && !identical(row$uuid[1], selected$uuid)))
    stop("This cached record has changed. Refresh the overview and select it again.")
  row$abstract[1]
}

pond_overview_selection_outline <- function(map, overview, selected_id) {
  if (length(selected_id) != 1L || is.na(selected_id) || !nrow(overview)) return(map)
  row <- overview[overview$id %in% selected_id, ]
  if (nrow(row) != 1L || !isTRUE(row$bbox_available) ||
      !all(is.finite(c(row$west, row$south, row$east, row$north))) ||
      row$east <= row$west || row$north <= row$south) return(map)
  leaflet::addRectangles(map, lng1 = row$west, lat1 = row$south, lng2 = row$east, lat2 = row$north,
    group = "overview_selection", color = "#3F5146", weight = 5, opacity = 1, fillOpacity = 0,
    options = leaflet::pathOptions(pane = "pond_bbox_selected", interactive = FALSE, pointerEvents = "none"))
}
