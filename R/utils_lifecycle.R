# utils_lifecycle.R
# Simple dataset lifecycle and folder rename helpers ----

lifecycle_status_label <- function(x) {
  labels <- c(
    draft = "Draft",
    released = "Released",
    archived = "Archived"
  )

  labels[[x]] %||% x %||% "draft"
}

lifecycle_is_draft_unlocked <- function(lifecycle_row) {
  if (is.null(lifecycle_row) || nrow(lifecycle_row) == 0) {
    return(FALSE)
  }

  status <- lifecycle_row$lifecycle_status[1] %||% "draft"
  locked <- as.integer(lifecycle_row$folder_locked[1] %||% 0L)

  identical(status, "draft") && locked == 0L
}

lifecycle_safe_path_slug <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || trimws(x[1]) == "") {
    return("dataset")
  }

  x <- tolower(as.character(x[1]))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  if (x == "") "dataset" else x
}

build_dataset_folder_name <- function(
  folder_key,
  uuid
) {
  paste0(
    folder_key_slug(
      folder_key,
      default = "dataset"
    ),
    "__",
    folder_key_slug(
      uuid,
      default = "uuid",
      max_length = 80L
    )
  )
}

propose_dataset_folder_path <- function(
  dataset_id,
  base_path
) {
  rec <- get_dataset_record(
    dataset_id,
    base_path
  )

  if (is.null(rec)) {
    stop(
      "Dataset record not found."
    )
  }

  vals <- as.list(
    as.data.frame(rec)[
      1,
      ,
      drop = FALSE
    ]
  )

  old_dataset_path <- folder_key_text(
    vals$dataset_path,
    default = NULL
  )

  if (is.null(old_dataset_path)) {
    stop(
      "Current dataset_path is missing."
    )
  }

  old_dataset_path <- normalizePath(
    old_dataset_path,
    mustWork = FALSE,
    winslash = "/"
  )

  folder_key <- folder_key_text(
    vals$folder_key,
    default = NULL
  )

  if (is.null(folder_key)) {
    org_df <- get_dataset_org_links(
      dataset_id = dataset_id,
      base_path = base_path
    )

    folder_key <- generate_folder_key(
      vals = vals,
      org_df = org_df
    )
  }

  uuid <- folder_key_text(
    vals$uuid,
    default = NULL
  )

  if (is.null(uuid)) {
    uuid <- get_or_create_dataset_uuid(
      dataset_id,
      base_path
    )
  }

  parent_path <- dirname(
    old_dataset_path
  )

  new_folder_name <- build_dataset_folder_name(
    folder_key = folder_key,
    uuid = uuid
  )

  file.path(
    parent_path,
    new_folder_name
  )
}

get_dataset_folder_comparison <- function(
  dataset_id,
  base_path
) {
  lifecycle <- get_dataset_lifecycle(
    dataset_id,
    base_path
  )

  if (
    is.null(lifecycle) ||
    nrow(lifecycle) == 0
  ) {
    return(
      data.frame(
        item = "Dataset",
        current = "No dataset selected.",
        proposed = "",
        stringsAsFactors = FALSE
      )
    )
  }

  current_path <- folder_key_text(
    lifecycle$dataset_path[1],
    default = ""
  )

  proposed_path <- tryCatch(
    propose_dataset_folder_path(
      dataset_id,
      base_path
    ),
    error = function(e) {
      NA_character_
    }
  )

  current_folder <- if (
    nzchar(current_path)
  ) {
    basename(
      normalizePath(
        current_path,
        mustWork = FALSE,
        winslash = "/"
      )
    )
  } else {
    ""
  }

  proposed_folder <- if (
    !is.na(proposed_path) &&
    nzchar(proposed_path)
  ) {
    basename(
      normalizePath(
        proposed_path,
        mustWork = FALSE,
        winslash = "/"
      )
    )
  } else {
    ""
  }

  current_key <- if (
    nzchar(current_path)
  ) {
    extract_folder_key_from_folder_name(
      current_path
    )
  } else {
    NULL
  }

  proposed_key <- if (
    !is.na(proposed_path) &&
    nzchar(proposed_path)
  ) {
    extract_folder_key_from_folder_name(
      proposed_path
    )
  } else {
    NULL
  }

  current_uuid <- folder_key_text(
    lifecycle$uuid[1],
    default = ""
  )

  if (!nzchar(current_uuid)) {
    current_uuid <- get_or_create_dataset_uuid(
      dataset_id,
      base_path
    )
  }

  changed <- (
    !is.na(proposed_path) &&
      nzchar(current_path) &&
      normalizePath(
        current_path,
        mustWork = FALSE,
        winslash = "/"
      ) != normalizePath(
        proposed_path,
        mustWork = FALSE,
        winslash = "/"
      )
  )

  data.frame(
    item = c(
      "Folder name",
      "Folder key part",
      "UUID suffix",
      "Full path",
      "Will rename?"
    ),

    current = c(
      current_folder,
      current_key %||% "",
      current_uuid,
      current_path,
      ifelse(
        isTRUE(changed),
        "Yes",
        "No"
      )
    ),

    proposed = c(
      proposed_folder,
      proposed_key %||% "",
      current_uuid,
      proposed_path %||% "",
      ifelse(
        isTRUE(changed),
        "Yes",
        "No"
      )
    ),

    stringsAsFactors = FALSE
  )
}

rename_draft_dataset_folder <- function(dataset_id, base_path) {
  lifecycle <- get_dataset_lifecycle(dataset_id, base_path)

  if (is.null(lifecycle)) {
    stop("Dataset lifecycle record not found.")
  }

  if (!lifecycle_is_draft_unlocked(lifecycle)) {
    stop("Only unlocked draft datasets can be renamed.")
  }

  old_path <- lifecycle$dataset_path[1]

  if (is.null(old_path) ||
      length(old_path) == 0 ||
      is.na(old_path) ||
      old_path == "") {
    stop("Current dataset_path is missing.")
  }

  old_path <- normalizePath(old_path, mustWork = FALSE, winslash = "/")

  if (!dir.exists(old_path)) {
    stop("Dataset folder does not exist: ", old_path)
  }

  new_path <- propose_dataset_folder_path(dataset_id, base_path)
  new_path <- normalizePath(new_path, mustWork = FALSE, winslash = "/")

  if (identical(old_path, new_path)) {
    return(list(
      changed = FALSE,
      old_path = old_path,
      new_path = new_path,
      message = "Folder already matches the current proposed name."
    ))
  }

  if (dir.exists(new_path) || file.exists(new_path)) {
    stop("Target folder already exists: ", new_path)
  }

  ok <- file.rename(old_path, new_path)

  if (!isTRUE(ok)) {
    stop("Folder rename failed. Check file permissions and whether files are open.")
  }

  new_data_path <- file.path(new_path, "data")

  update_dataset_folder_paths(
    dataset_id = dataset_id,
    base_path = base_path,
    dataset_path = new_path,
    data_path = new_data_path
  )

  repair_cached_absolute_paths_after_dataset_move(
    dataset_id = dataset_id,
    base_path = base_path,
    dataset_path = new_path,
    data_path = new_data_path
  )

  list(
    changed = TRUE,
    old_path = old_path,
    new_path = new_path,
    message = "Dataset folder renamed."
  )
}