
# LIFECYCLE EXTENSIONS (status -> lifecycle bridge)

# ---- readiness logic ----
is_dataset_ready_for_archive <- function(dataset_id, base_path, schema_grouped = NULL) {
  if (is.null(schema_grouped)) {
    warning("schema_grouped is NULL -- readiness cannot be evaluated")
    return(FALSE)
  }
  status_dt <- get_dataset_completion_status(
    dataset_id = dataset_id,
    base_path = base_path,
    schema_grouped = schema_grouped
  )
  if (is.null(status_dt) || nrow(status_dt) == 0) {
    return(FALSE)
  }
  optional_sections <- c("Spatial metadata")
  valid_status <- c("complete", "not_applicable")
  incomplete <- status_dt[
    !status %in% valid_status &
    !section %in% optional_sections
  ]
  nrow(incomplete) == 0
}


# DASHBOARD DATA ----
empty_lifecycle_dashboard <- function() {
  data.table::data.table(
    id = integer(),
    uuid = character(),
    citation_key = character(),
    dataset_path = character(),
    lifecycle_status = character(),
    folder_locked = integer(),
    ready_for_archive = logical(),
    lifecycle_label = character(),
    stage = character(),
    next_action = character(),
    lifecycle_stage_mismatch = logical()
  )
}


get_lifecycle_dashboard <- function(
  base_path,
  schema_grouped
) {
  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  required_columns <- c(
    "id",
    "uuid",
    "citation_key",
    "dataset_path",
    "lifecycle_status",
    "folder_locked"
  )

  existing_columns <- DBI::dbListFields(
    conn,
    "entry_tbl"
  )

  missing_columns <- setdiff(
    required_columns,
    existing_columns
  )

  if (length(missing_columns) > 0) {
    warning(
      "Lifecycle migration incomplete. Missing entry_tbl columns: ",
      paste(missing_columns, collapse = ", ")
    )

    return(empty_lifecycle_dashboard())
  }

  datasets <- DBI::dbGetQuery(
    conn,
    "
    SELECT
      id,
      uuid,
      citation_key,
      folder_key,
      dataset_path,
      lifecycle_status,
      folder_locked
    FROM entry_tbl
    "
  )

  if (nrow(datasets) == 0) {
    return(empty_lifecycle_dashboard())
  }

  dt <- data.table::as.data.table(datasets)

  dt[, ready_for_archive := vapply(
    id,
    function(dataset_id) {
      is_dataset_ready_for_archive(
        dataset_id = dataset_id,
        base_path = base_path,
        schema_grouped = schema_grouped
      )
    },
    logical(1)
  )]

  lifecycle_labels <- c(
    draft = "Draft",
    released = "Released",
    archived = "Archived"
  )

  dt[
    ,
    lifecycle_label :=
      unname(lifecycle_labels[lifecycle_status])
  ]

  dt[
    is.na(lifecycle_label) |
      lifecycle_label == "",
    lifecycle_label := lifecycle_status
  ]

  dt[
    ,
    stage := ifelse(
      is.na(dataset_path) |
        dataset_path == "",
      NA_character_,
      basename(dirname(dataset_path))
    )
  ]

  dt[
    ,
    next_action := data.table::fifelse(
      lifecycle_status == "draft" &
        ready_for_archive,
      "Ready to archive",
      data.table::fifelse(
        lifecycle_status == "draft",
        "Complete metadata",
        data.table::fifelse(
          lifecycle_status == "released",
          "No action",
          "Archived"
        )
      )
    )
  ]

  dt[, lifecycle_stage_mismatch := FALSE]

  dt[
    (
      lifecycle_status == "archived" &
        stage != "arc"
    ) |
      (
        lifecycle_status == "released" &
          stage == "arc"
      ),
    lifecycle_stage_mismatch := TRUE
  ]

  dt[]
}


# MOVE DATASET (CORE ENGINE)
move_dataset_folder <- function(dataset_id, base_path, target_stage, user_person_id = NA_integer_) {
    rec <- get_dataset_lifecycle(dataset_id, base_path)
  if (is.null(rec) || nrow(rec) == 0) {
    stop("Dataset not found")
  }
  # ---- safety checks ----
  if (rec$lifecycle_status[1] == "released") {
    stop("Released datasets cannot be moved")
  }
  if (as.integer(rec$folder_locked[1] %||% 0L) == 1L) {
    stop("Dataset folder is locked")
  }
  # ---- paths ----
  old_path <- normalizePath(rec$dataset_path[1], mustWork = FALSE, winslash = "/")
  if (!dir.exists(old_path)) {
    stop("Source folder does not exist")
  }
  parent_root <- dirname(dirname(old_path))  # /src or /dev -> root
  new_path <- file.path(parent_root, target_stage, basename(old_path))
  # ---- ensure target dir ----
  dir.create(dirname(new_path), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(new_path)) {
    stop("Target folder already exists")
  }
  # ---- move ----
  ok <- file.rename(old_path, new_path)
  if (!isTRUE(ok)) {
    stop("Folder move failed (permissions or open files?)")
  }
  # ---- update DB ----
  update_dataset_folder_paths(
    dataset_id = dataset_id,
    base_path = base_path,
    dataset_path = new_path,
    data_path = file.path(new_path, "data")
  )
  # ---- repair cached paths ----
  repair_cached_absolute_paths_after_dataset_move(
    dataset_id = dataset_id,
    base_path = base_path,
    dataset_path = new_path,
    data_path = file.path(new_path, "data")
  )
  TRUE
}