# Read-only connected Data Pond catalogues ----

connected_pond_registry_file <- function() {
  file.path(app_state_dir(), "connected_data_ponds.rds")
}


empty_connected_pond_registry <- function() {
  data.frame(
    data_pond_id = character(),
    data_pond_path = character(),
    pond_name = character(),
    connected_at = character(),
    last_seen_at = character(),
    stringsAsFactors = FALSE
  )
}


normalise_connected_pond_registry <- function(registry) {
  required <- names(empty_connected_pond_registry())
  if (is.null(registry) || nrow(as.data.frame(registry)) == 0L) {
    return(empty_connected_pond_registry())
  }
  registry <- as.data.frame(registry, stringsAsFactors = FALSE)
  for (field in required) {
    if (!field %in% names(registry)) registry[[field]] <- NA_character_
    registry[[field]] <- as.character(registry[[field]])
  }
  registry <- registry[
    !is.na(registry$data_pond_id) & nzchar(trimws(registry$data_pond_id)) &
      !is.na(registry$data_pond_path) & nzchar(trimws(registry$data_pond_path)),
    required,
    drop = FALSE
  ]
  registry$data_pond_path <- vapply(
    registry$data_pond_path,
    function(path) normalizePath(path.expand(path), mustWork = FALSE, winslash = "/"),
    character(1)
  )
  registry$pond_name[
    is.na(registry$pond_name) | !nzchar(trimws(registry$pond_name))
  ] <- basename(registry$data_pond_path[
    is.na(registry$pond_name) | !nzchar(trimws(registry$pond_name))
  ])
  registry <- registry[!duplicated(registry$data_pond_id, fromLast = TRUE), , drop = FALSE]
  rownames(registry) <- NULL
  registry
}


read_connected_pond_registry <- function(
  registry_file = connected_pond_registry_file()
) {
  if (!file.exists(registry_file)) return(empty_connected_pond_registry())
  normalise_connected_pond_registry(tryCatch(
    readRDS(registry_file),
    error = function(e) empty_connected_pond_registry()
  ))
}


write_connected_pond_registry <- function(
  registry,
  registry_file = connected_pond_registry_file()
) {
  registry <- normalise_connected_pond_registry(registry)
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  temporary_file <- tempfile("connected-ponds-", tmpdir = dirname(registry_file))
  on.exit(unlink(temporary_file, force = TRUE), add = TRUE)
  saveRDS(registry, temporary_file)
  if (!isTRUE(file.copy(temporary_file, registry_file, overwrite = TRUE))) {
    stop("The connected Data Pond registry could not be saved.", call. = FALSE)
  }
  invisible(registry)
}


read_data_pond_identity_read_only <- function(path) {
  path <- normalizePath(path.expand(path[1]), mustWork = TRUE, winslash = "/")
  id_file <- data_pond_id_file(path)
  database <- get_db_path(path)
  if (!file.exists(id_file)) {
    stop("The selected folder has no Data Pond identifier.", call. = FALSE)
  }
  if (!file.exists(database)) {
    stop("The selected folder has no metadata.db catalogue.", call. = FALSE)
  }
  data_pond_id <- trimws(readLines(id_file, warn = FALSE, n = 1L))
  if (length(data_pond_id) == 0L || is.na(data_pond_id[1]) || !nzchar(data_pond_id[1])) {
    stop("The selected Data Pond identifier is empty.", call. = FALSE)
  }
  pond_label <- NA_character_
  conn <- tryCatch(
    DBI::dbConnect(
      RSQLite::SQLite(),
      database,
      flags = RSQLite::SQLITE_RO,
      synchronous = NULL
    ),
    error = function(e) NULL
  )
  if (!is.null(conn)) {
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    if (DBI::dbExistsTable(conn, "data_pond_tbl")) {
      label_row <- DBI::dbGetQuery(
        conn,
        paste(
          "SELECT data_pond_label FROM data_pond_tbl",
          "WHERE data_pond_id = ? LIMIT 1"
        ),
        params = list(data_pond_id[1])
      )
      if (nrow(label_row) > 0L && !is.na(label_row$data_pond_label[1]) &&
          nzchar(trimws(label_row$data_pond_label[1]))) {
        pond_label <- trimws(label_row$data_pond_label[1])
      }
    }
  }
  list(
    data_pond_id = data_pond_id[1],
    data_pond_path = path,
    database_path = database,
    pond_label = pond_label
  )
}


set_data_pond_label <- function(path, pond_name) {
  pond_name <- trimws(as.character(pond_name[1]))
  if (!nzchar(pond_name)) stop("A nickname / repository name is required.", call. = FALSE)
  conn <- get_conn(path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_data_pond_id(path, conn = conn, label = pond_name)
  invisible(pond_name)
}


register_connected_pond <- function(
  path,
  pond_name = NULL,
  registry_file = connected_pond_registry_file()
) {
  identity <- read_data_pond_identity_read_only(path)
  registry <- read_connected_pond_registry(registry_file)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  existing <- which(registry$data_pond_id == identity$data_pond_id)
  supplied_name <- if (
    is.null(pond_name) || length(pond_name) == 0L || is.na(pond_name[1]) ||
      !nzchar(trimws(as.character(pond_name[1])))
  ) {
    NA_character_
  } else {
    trimws(as.character(pond_name[1]))
  }

  if (length(existing) > 0L) {
    row <- existing[1]
    registry$data_pond_path[row] <- identity$data_pond_path
    if (!is.na(supplied_name)) {
      registry$pond_name[row] <- supplied_name
    } else if (!is.na(identity$pond_label)) {
      registry$pond_name[row] <- identity$pond_label
    }
    registry$last_seen_at[row] <- now
  } else {
    registry <- rbind(
      registry,
      data.frame(
        data_pond_id = identity$data_pond_id,
        data_pond_path = identity$data_pond_path,
        pond_name = if (!is.na(supplied_name)) {
          supplied_name
        } else if (!is.na(identity$pond_label)) {
          identity$pond_label
        } else {
          basename(identity$data_pond_path)
        },
        connected_at = now,
        last_seen_at = now,
        stringsAsFactors = FALSE
      )
    )
  }
  write_connected_pond_registry(registry, registry_file)
  normalise_connected_pond_registry(registry)
}


rename_connected_pond <- function(
  data_pond_id,
  pond_name,
  registry_file = connected_pond_registry_file()
) {
  pond_name <- trimws(as.character(pond_name[1]))
  if (!nzchar(pond_name)) stop("A nickname / repository name is required.", call. = FALSE)
  registry <- read_connected_pond_registry(registry_file)
  row <- which(registry$data_pond_id == as.character(data_pond_id[1]))
  if (length(row) == 0L) stop("The Data Pond is not connected.", call. = FALSE)
  registry$pond_name[row[1]] <- pond_name
  write_connected_pond_registry(registry, registry_file)
  normalise_connected_pond_registry(registry)
}


remove_connected_pond <- function(
  data_pond_id,
  registry_file = connected_pond_registry_file()
) {
  registry <- read_connected_pond_registry(registry_file)
  registry <- registry[
    registry$data_pond_id != as.character(data_pond_id[1]),
    ,
    drop = FALSE
  ]
  write_connected_pond_registry(registry, registry_file)
  normalise_connected_pond_registry(registry)
}


connected_pond_database_signature <- function(path) {
  database <- get_db_path(path)
  if (!file.exists(database)) return(NA_character_)
  info <- file.info(database)
  paste(as.numeric(info$mtime[1]), info$size[1], sep = ":")
}


connected_pond_stage <- function(dataset_path, pond_path) {
  dataset_path <- gsub("\\\\", "/", as.character(dataset_path))
  pond_path <- sub("/+$", "", gsub("\\\\", "/", as.character(pond_path[1])))
  prefix <- paste0(pond_path, "/")
  relative <- ifelse(startsWith(dataset_path, prefix), substring(dataset_path, nchar(prefix) + 1L), "")
  stage <- sub("/.*$", "", relative)
  stage[!stage %in% c("src", "dev", "arc")] <- "unknown"
  stage
}


read_connected_pond_inventory <- function(path, include_hidden = FALSE) {
  identity <- read_data_pond_identity_read_only(path)
  conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    identity$database_path,
    flags = RSQLite::SQLITE_RO,
    synchronous = NULL
  )
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (!DBI::dbExistsTable(conn, "entry_tbl")) return(data.table::data.table())

  available <- DBI::dbListFields(conn, "entry_tbl")
  wanted <- c(
    "id", "uuid", "title", "version", "citation_date", "citation_key",
    "folder_key", "dataset_path", "lifecycle_status", "is_hidden", "deleted_at"
  )
  expressions <- vapply(
    wanted,
    function(field) {
      if (field %in% available) {
        as.character(DBI::dbQuoteIdentifier(conn, field))
      } else {
        paste("NULL AS", as.character(DBI::dbQuoteIdentifier(conn, field)))
      }
    },
    character(1)
  )
  rows <- DBI::dbGetQuery(
    conn,
    paste0("SELECT ", paste(expressions, collapse = ", "), " FROM entry_tbl")
  )
  rows <- data.table::as.data.table(rows)
  if (nrow(rows) == 0L) return(rows)
  if (!isTRUE(include_hidden)) {
    rows <- rows[(is.na(is_hidden) | as.integer(is_hidden) == 0L) & is.na(deleted_at)]
  }
  rows[, stage := connected_pond_stage(dataset_path, identity$data_pond_path)]
  rows[, data_pond_id := identity$data_pond_id]
  rows[, data_pond_path := identity$data_pond_path]
  rows[]
}


read_connected_pond_inventory_cached <- function(
  path,
  cache,
  force = FALSE,
  include_hidden = FALSE
) {
  path <- normalizePath(path.expand(path[1]), mustWork = FALSE, winslash = "/")
  signature <- connected_pond_database_signature(path)
  key <- paste(path, isTRUE(include_hidden), sep = "|")
  if (
    !isTRUE(force) && exists(key, envir = cache, inherits = FALSE) &&
      identical(get(key, envir = cache, inherits = FALSE)$signature, signature)
  ) {
    return(data.table::copy(get(key, envir = cache, inherits = FALSE)$data))
  }
  data <- read_connected_pond_inventory(path, include_hidden = include_hidden)
  assign(key, list(signature = signature, data = data.table::copy(data)), envir = cache)
  data
}


build_connected_pond_catalogue <- function(
  registry,
  cache,
  active_pond_path = NULL,
  force = FALSE,
  include_hidden = FALSE
) {
  registry <- normalise_connected_pond_registry(registry)
  if (nrow(registry) == 0L) return(data.table::data.table())
  active_pond_path <- if (is.null(active_pond_path)) NA_character_ else
    normalizePath(active_pond_path[1], mustWork = FALSE, winslash = "/")

  rows <- lapply(seq_len(nrow(registry)), function(i) {
    pond <- registry[i, , drop = FALSE]
    tryCatch(
      {
        inventory <- read_connected_pond_inventory_cached(
          pond$data_pond_path,
          cache = cache,
          force = force,
          include_hidden = include_hidden
        )
        if (nrow(inventory) == 0L) return(NULL)
        inventory[, pond_name := pond$pond_name]
        inventory[, active_pond := identical(pond$data_pond_path, active_pond_path)]
        inventory
      },
      error = function(e) NULL
    )
  })
  data.table::rbindlist(rows, fill = TRUE, use.names = TRUE)
}


connected_pond_dataset_rows <- function(catalogue) {
  catalogue <- data.table::as.data.table(data.table::copy(catalogue))
  if (nrow(catalogue) == 0L || !"active_pond" %in% names(catalogue)) {
    return(catalogue)
  }
  catalogue[catalogue$active_pond %in% FALSE]
}


cross_pond_duplicate_datasets <- function(catalogue) {
  catalogue <- data.table::as.data.table(data.table::copy(catalogue))
  if (nrow(catalogue) == 0L) return(data.table::data.table())
  nonempty <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  catalogue[, match_basis := data.table::fcase(
    nonempty(uuid), "Dataset UUID",
    nonempty(citation_key), "Citation key",
    nonempty(folder_key), "Folder key",
    default = NA_character_
  )]
  catalogue[, duplicate_key := data.table::fcase(
    nonempty(uuid), paste0("uuid:", tolower(trimws(uuid))),
    nonempty(citation_key), paste0("citation:", tolower(trimws(citation_key))),
    nonempty(folder_key), paste0("folder:", tolower(trimws(folder_key))),
    default = NA_character_
  )]
  catalogue <- catalogue[!is.na(duplicate_key)]
  if (nrow(catalogue) == 0L) return(data.table::data.table())
  catalogue[, pond_count := data.table::uniqueN(data_pond_id), by = duplicate_key]
  catalogue <- catalogue[pond_count > 1L]
  if (nrow(catalogue) == 0L) return(data.table::data.table())

  catalogue[
    ,
    .(
      match_basis = match_basis[1],
      title = {
        titles <- title[nonempty(title)]
        if (length(titles) == 0L) "Untitled dataset" else titles[1]
      },
      dataset_uuid = {
        values <- uuid[nonempty(uuid)]
        if (length(values) == 0L) NA_character_ else values[1]
      },
      copies = .N,
      ponds = paste(unique(paste0(pond_name, " [", stage, "]")), collapse = "; "),
      stages = paste(sort(unique(stage)), collapse = ", "),
      needs_resolution = any(stage %in% c("src", "arc")),
      status = if (any(stage %in% c("src", "arc"))) {
        "Resolve duplicate source/archive record"
      } else {
        "Tolerated duplicate development copy"
      }
    ),
    by = duplicate_key
  ][order(-needs_resolution, title)]
}
