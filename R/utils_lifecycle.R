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


dataset_marker_filename <- function() {
  ".pond-dataset.json"
}


dataset_marker_path <- function(dataset_path) {
  file.path(dataset_path, dataset_marker_filename())
}


write_dataset_marker <- function(dataset_path, dataset_uuid, data_pond_id) {
  if (!dir.exists(dataset_path)) {
    stop("Dataset root does not exist: ", dataset_path)
  }
  values <- trimws(as.character(c(dataset_uuid, data_pond_id)))
  if (length(values) != 2L || any(is.na(values) | !nzchar(values))) {
    stop("dataset_uuid and data_pond_id are required for the dataset marker.")
  }
  if (any(grepl('["\\\\\r\n]', values))) {
    stop("Dataset marker identifiers contain unsupported characters.")
  }

  writeLines(
    c(
      "{",
      paste0('  "dataset_uuid": "', values[1], '",'),
      paste0('  "data_pond_id": "', values[2], '"'),
      "}"
    ),
    dataset_marker_path(dataset_path),
    useBytes = TRUE
  )
  invisible(dataset_marker_path(dataset_path))
}


read_dataset_marker <- function(dataset_path) {
  path <- dataset_marker_path(dataset_path)
  if (!file.exists(path)) {
    return(list(dataset_uuid = NA_character_, data_pond_id = NA_character_))
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  value <- function(field) {
    match <- regexec(
      paste0('"', field, '"\\s*:\\s*"([^"]+)"'),
      text,
      perl = TRUE
    )
    parts <- regmatches(text, match)[[1L]]
    if (length(parts) < 2L) NA_character_ else parts[2L]
  }
  list(
    dataset_uuid = value("dataset_uuid"),
    data_pond_id = value("data_pond_id")
  )
}


extract_dataset_uuid_from_folder_name <- function(path) {
  folder <- basename(sub("/+$", "", as.character(path[1])))
  if (!grepl("__", folder, fixed = TRUE)) {
    return(NA_character_)
  }
  uuid <- sub("^.*__", "", folder)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{5,79}$", uuid)) {
    return(NA_character_)
  }
  uuid
}


dataset_lifecycle_stage <- function(path, base_path) {
  path <- normalizePath(path, mustWork = FALSE, winslash = "/")
  base_path <- normalizePath(base_path, mustWork = FALSE, winslash = "/")
  for (stage in c("src", "dev", "arc")) {
    root <- normalizePath(
      file.path(base_path, stage),
      mustWork = FALSE,
      winslash = "/"
    )
    if (identical(path, root) || startsWith(path, paste0(root, "/"))) {
      return(stage)
    }
  }
  NA_character_
}


empty_dataset_root_inventory <- function() {
  data.table::data.table(
    dataset_path = character(),
    lifecycle_stage = character(),
    folder_uuid = character(),
    marker_uuid = character(),
    marker_data_pond_id = character(),
    location_status = character()
  )
}


discover_dataset_roots <- function(base_path) {
  base_path <- normalizePath(base_path, mustWork = FALSE, winslash = "/")
  rows <- list()

  for (stage in c("src", "dev", "arc")) {
    root <- file.path(base_path, stage)
    if (!dir.exists(root)) {
      next
    }
    pending <- root
    visited <- character()

    while (length(pending) > 0L) {
      current <- pending[[1L]]
      pending <- pending[-1L]
      key <- normalizePath(current, mustWork = FALSE, winslash = "/")
      if (key %in% visited) {
        next
      }
      visited <- c(visited, key)

      children <- list.dirs(current, full.names = TRUE, recursive = FALSE)
      children <- children[children != current]
      if (length(children) == 0L) {
        next
      }

      for (child in children) {
        folder_uuid <- extract_dataset_uuid_from_folder_name(child)
        has_marker <- file.exists(dataset_marker_path(child))
        if (!is.na(folder_uuid) || has_marker) {
          marker <- read_dataset_marker(child)
          status <- "candidate"
          if (
            !is.na(folder_uuid) && !is.na(marker$dataset_uuid) &&
              !identical(tolower(folder_uuid), tolower(marker$dataset_uuid))
          ) {
            status <- "uuid_marker_conflict"
          }
          rows[[length(rows) + 1L]] <- data.table::data.table(
            dataset_path = normalizePath(
              child,
              mustWork = FALSE,
              winslash = "/"
            ),
            lifecycle_stage = stage,
            folder_uuid = folder_uuid,
            marker_uuid = marker$dataset_uuid,
            marker_data_pond_id = marker$data_pond_id,
            location_status = status
          )
        } else {
          pending <- c(pending, child)
        }
      }
    }
  }

  if (length(rows) == 0L) {
    return(empty_dataset_root_inventory())
  }

  found <- data.table::rbindlist(rows, fill = TRUE)
  claimed <- unique(data.table::rbindlist(list(
    found[!is.na(folder_uuid), .(dataset_path, uuid = tolower(folder_uuid))],
    found[!is.na(marker_uuid), .(dataset_path, uuid = tolower(marker_uuid))]
  )))
  duplicates <- claimed[, .N, by = uuid][N > 1L, uuid]
  found[
    tolower(folder_uuid) %in% duplicates |
      tolower(marker_uuid) %in% duplicates,
    location_status := "duplicate_uuid"
  ]
  found[]
}


reconcile_dataset_locations <- function(
  base_path,
  dataset_ids = NULL
) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lifecycle_columns(conn)
  record_sql <-
    "SELECT id, uuid, dataset_path, data_pond_id FROM entry_tbl"

  record_params <- list()

  if (!is.null(dataset_ids)) {
    dataset_ids <- unique(
      suppressWarnings(
        as.integer(dataset_ids)
      )
    )
    dataset_ids <- dataset_ids[!is.na(dataset_ids)]

    if (length(dataset_ids) == 0L) {
      return(data.table::data.table())
    }

    record_sql <- paste0(
      record_sql,
      " WHERE id IN (",
      paste(rep("?", length(dataset_ids)), collapse = ", "),
      ")"
    )
    record_params <- as.list(dataset_ids)
  }

  records <- if (length(record_params) > 0L) {
    DBI::dbGetQuery(
      conn,
      record_sql,
      params = record_params
    )
  } else {
    DBI::dbGetQuery(
      conn,
      record_sql
    )
  }

  records <- data.table::as.data.table(records)
  if (nrow(records) == 0L) {
    return(data.table::data.table())
  }

  pond_id <- ensure_data_pond_id(base_path, conn = conn)
  roots <- discover_dataset_roots(base_path)
  reports <- lapply(seq_len(nrow(records)), function(i) {
    record <- records[i]
    uuid <- trimws(as.character(record$uuid))
    valid_uuid <- length(uuid) == 1L && !is.na(uuid) && nzchar(uuid)
    stored_path <- normalizePath(
      as.character(record$dataset_path),
      mustWork = FALSE,
      winslash = "/"
    )
    stored_stage <- dataset_lifecycle_stage(stored_path, base_path)
    matches <- if (isTRUE(valid_uuid)) {
      roots[
        tolower(folder_uuid) == tolower(uuid) |
          tolower(marker_uuid) == tolower(uuid)
      ]
    } else {
      roots[0]
    }

    status <- if (isTRUE(valid_uuid)) "missing" else "missing_uuid"
    detected_path <- NA_character_
    detected_stage <- NA_character_

    if (nrow(matches) > 1L || any(matches$location_status == "duplicate_uuid")) {
      status <- "duplicate_uuid"
    } else if (nrow(matches) == 1L) {
      detected_path <- matches$dataset_path[1]
      detected_stage <- matches$lifecycle_stage[1]
      if (matches$location_status[1] == "uuid_marker_conflict") {
        status <- "uuid_marker_conflict"
      } else if (
        !is.na(matches$marker_data_pond_id[1]) &&
          !identical(matches$marker_data_pond_id[1], pond_id)
      ) {
        status <- "data_pond_marker_conflict"
      } else if (identical(stored_path, detected_path)) {
        status <- "ok"
      } else if (dir.exists(stored_path)) {
        status <- "stored_path_conflict"
      } else if (
        is.na(stored_stage) || is.na(detected_stage) ||
          !identical(stored_stage, detected_stage)
      ) {
        status <- "lifecycle_location_conflict"
      } else {
        new_data_path <- file.path(detected_path, "data")
        DBI::dbExecute(
          conn,
          "UPDATE entry_tbl SET dataset_path = ?, data_path = ? WHERE id = ?",
          params = list(detected_path, new_data_path, as.integer(record$id))
        )
        repair_cached_absolute_paths_after_dataset_move(
          dataset_id = record$id,
          base_path = base_path,
          dataset_path = detected_path,
          data_path = new_data_path
        )
        stored_path <- detected_path
        status <- "repaired_within_lifecycle_root"
      }

      if (
        status %in% c("ok", "repaired_within_lifecycle_root") &&
          !file.exists(dataset_marker_path(detected_path))
      ) {
        write_dataset_marker(detected_path, uuid, pond_id)
      }
    }

    data.table::data.table(
      id = as.integer(record$id),
      stored_dataset_path = stored_path,
      detected_dataset_path = detected_path,
      stored_stage = stored_stage,
      detected_stage = detected_stage,
      location_status = status,
      lifecycle_location_conflict = status %in% c(
        "lifecycle_location_conflict",
        "duplicate_uuid",
        "uuid_marker_conflict",
        "data_pond_marker_conflict",
        "stored_path_conflict",
        "missing_uuid"
      )
    )
  })

  data.table::rbindlist(reports, fill = TRUE)
}


backfill_dataset_markers <- function(base_path) {
  report <- reconcile_dataset_locations(base_path)
  invisible(report)
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
