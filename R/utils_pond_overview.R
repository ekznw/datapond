# utils_pond_overview.R
#
# Dataset-level overview helpers for the active data pond.

pond_overview_palette <- function() {
  c(
    "Archive" =
      "#B99572",

    "Development -- ready for archive" =
      "#A8C49A",

    "Development -- in progress" =
      "#D1DDC7",

    "Source -- fully described" =
      "#C3A35B",

    "Source -- description incomplete" =
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
    "Development -- ready for archive",

    stage == "dev",
    "Development -- in progress",

    stage == "src" &
      ready_for_archive,
    "Source -- fully described",

    stage == "src",
    "Source -- description incomplete",

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
  schema_grouped
) {
  datasets <- get_datasets_with_authors(
    base_path
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
    lifecycle_stage_mismatch = FALSE
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

  data.table::setorder(
    overview,
    -id
  )

  overview[]
}