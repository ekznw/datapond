# app-local pointer to the last opened data pond ----
# This is not pond configuration. It is only a startup pointer.

app_state_dir <- function() {
  tools::R_user_dir("datapond", which = "config")
}

last_data_pond_file <- function() {
  file.path(app_state_dir(), "last_data_pond_path")
}

write_last_data_pond_path <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path[1]) || path[1] == "") {
    return(invisible(FALSE))
  }
  dir.create(app_state_dir(), recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path[1], mustWork = FALSE, winslash = "/")
  writeLines(path, last_data_pond_file(), useBytes = TRUE)
  invisible(TRUE)
}

read_last_data_pond_path <- function() {
  f <- last_data_pond_file()
  if (!file.exists(f)) {
    return(NULL)
  }
  path <- trimws(readLines(f, warn = FALSE)[1])
  if (is.null(path) || length(path) == 0 || is.na(path) || path == "") {
    return(NULL)
  }
  normalizePath(path, mustWork = FALSE, winslash = "/")
}

new_uuid <- function() {
  if (requireNamespace("uuid", quietly = TRUE)) {
    uuid::UUIDgenerate()
  } else {
    paste0(
      format(Sys.time(), "%Y%m%d%H%M%OS6"), "_",
      paste(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
    )
  }
}

metadata_pond_dir <- function(data_pond_path) {
  file.path(data_pond_path, ".metadata_pond")
}

data_pond_id_file <- function(data_pond_path) {
  file.path(metadata_pond_dir(data_pond_path), "data_pond_id")
}

data_pond_config_file <- function(data_pond_path) {
  file.path(metadata_pond_dir(data_pond_path), "config.rds")
}

old_config_migrated_marker <- function(data_pond_path) {
  file.path(metadata_pond_dir(data_pond_path), "old_config_migrated")
}

# Validate before opening so a mistyped path cannot create a new pond.
# Older ponds may have a catalogue but no identifier yet; switching migrates them.
validate_existing_data_pond <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stop("Select an existing Data Pond folder.", call. = FALSE)
  }
  path <- normalizePath(path.expand(trimws(path)), winslash = "/", mustWork = FALSE)
  movement_assert_writable(path)
  database <- get_db_path(path)
  if (!dir.exists(path) || !file.exists(database)) {
    stop("This folder is not an existing Data Pond. Use Create new pond to make one.", call. = FALSE)
  }
  valid <- tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), database, flags = RSQLite::SQLITE_RO, synchronous = NULL)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    DBI::dbExistsTable(conn, "entry_tbl")
  }, error = function(e) FALSE)
  if (!isTRUE(valid)) {
    stop("This folder does not contain a valid Data Pond metadata catalogue.", call. = FALSE)
  }
  path
}

new_data_pond_path <- function(parent_path, name) {
  if (is.null(parent_path) || length(parent_path) != 1L || is.na(parent_path) ||
      !nzchar(trimws(parent_path))) {
    stop("Choose a location for your new pond.", call. = FALSE)
  }
  parent_path <- normalizePath(path.expand(trimws(parent_path)), winslash = "/", mustWork = FALSE)
  if (!dir.exists(parent_path)) {
    stop("The location must be an existing parent folder.", call. = FALSE)
  }
  if (is.null(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    stop("Enter a name for your new pond.", call. = FALSE)
  }
  name <- trimws(name)
  if (name %in% c(".", "..") || grepl('[<>:"/\\\\|?*[:cntrl:]]', name) ||
      grepl("[.]$", name) || grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])([.]|$)", name, ignore.case = TRUE)) {
    stop("Use a folder name without path separators or reserved filename characters.", call. = FALSE)
  }
  path <- file.path(parent_path, name)
  if (file.exists(path) || dir.exists(path)) {
    stop("That name already exists here. Choose another name, or use Open existing pond.", call. = FALSE)
  }
  path
}

create_data_pond_folder <- function(parent_path, name) {
  path <- new_data_pond_path(parent_path, name)
  # A non-recursive create also refuses a destination created since validation.
  if (!dir.create(path, showWarnings = FALSE)) {
    stop("Could not create the pond folder. Check the location, name and write permissions.", call. = FALSE)
  }
  path
}

default_pond_config <- function() {
  list(
    config_version = 1L,
    data_pond = NULL,
    data_pond_id = NULL,
    active_user = NULL,
    dataset_parent_dir = "dev",
    attribute_profile = list(
      enabled = FALSE,
      vector = TRUE,
      raster = FALSE,
      tabular = FALSE,
      max_example_values = 8L,
      max_unique_count_scan = 10000L,
      save_examples = TRUE
    ),
    data_file_review = list(
      enable_user_notes = TRUE,
      enable_content_hint = TRUE
    )
  )
}

normalise_dataset_parent_dir <- function(x, default = "dev") {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !x[1] %in% c("src", "dev")) {
    return(default)
  }
  as.character(x[1])
}

ensure_data_pond_id <- function(data_pond_path, conn = NULL,
                                current_user_id = NULL,
                                label = NULL) {
  dir.create(metadata_pond_dir(data_pond_path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(metadata_pond_dir(data_pond_path))) {
    stop("Cannot access the pond metadata folder. Check that the drive is connected and writable: ",
         data_pond_path, call. = FALSE)
  }

  id_path <- data_pond_id_file(data_pond_path)

  if (file.exists(id_path)) {
    data_pond_id <- trimws(readLines(id_path, warn = FALSE)[1])
  } else {
    data_pond_id <- new_uuid()
    writeLines(data_pond_id, id_path, useBytes = TRUE)
  }

  # DBI/RSQLite parameters must each be length 1.
  # NULL has length 0 and causes: "Parameter X does not have length 1".
  if (is.null(label) || length(label) == 0) {
    label <- NA_character_
  } else {
    label <- as.character(label[1])
  }

  if (is.null(current_user_id) || length(current_user_id) == 0) {
    current_user_id <- NA_integer_
  } else {
    current_user_id <- as.integer(current_user_id[1])
  }

  if (!is.null(conn)) {
    DBI::dbExecute(
      conn,
      "
      INSERT INTO data_pond_tbl
        (data_pond_id, data_pond_path, data_pond_label, created_by_user_id, updated_at)
      VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)
      ON CONFLICT(data_pond_id) DO UPDATE SET
        data_pond_path = excluded.data_pond_path,
        data_pond_label = COALESCE(excluded.data_pond_label, data_pond_tbl.data_pond_label),
        updated_at = CURRENT_TIMESTAMP
      ",
      params = list(
        as.character(data_pond_id[1]),
        normalizePath(data_pond_path, winslash = "/", mustWork = FALSE),
        label,
        current_user_id
      )
    )
  }

  data_pond_id
}

read_pond_config <- function(data_pond_path) {
  cfg <- default_pond_config()
  cfg$data_pond <- data_pond_path

  f <- data_pond_config_file(data_pond_path)
  if (file.exists(f)) {
    stored <- readRDS(f)
    cfg <- utils::modifyList(cfg, stored)
  }

  cfg$dataset_parent_dir <- normalise_dataset_parent_dir(cfg$dataset_parent_dir)
  # Reading startup settings must not create folders or an identity on a
  # disconnected drive. Identity creation belongs to pond initialisation.
  id_path <- data_pond_id_file(data_pond_path)
  cfg$data_pond_id <- if (file.exists(id_path)) {
    trimws(readLines(id_path, warn = FALSE, n = 1L)[1])
  } else {
    NULL
  }
  cfg
}

assert_saved_data_pond_available <- function(path) {
  # An unmounted volume may leave an empty mount-point directory behind.
  # Require its catalogue as well, so startup cannot initialise a replacement.
  if (!dir.exists(path) || !file.exists(get_db_path(path))) {
    stop("The saved Data Pond folder or metadata catalogue is unavailable: ", path,
         ". Reconnect the drive or restore access, then retry.", call. = FALSE)
  }
  invisible(path)
}

write_pond_config <- function(data_pond_path, config) {
  dir.create(metadata_pond_dir(data_pond_path), recursive = TRUE, showWarnings = FALSE)

  config$data_pond <- data_pond_path
  config$data_pond_id <- ensure_data_pond_id(data_pond_path)
  config$dataset_parent_dir <- normalise_dataset_parent_dir(config$dataset_parent_dir)

  saveRDS(config, data_pond_config_file(data_pond_path))
  invisible(config)
}

save_config <- function(path,
                        active_user_id = NULL,
                        dataset_parent_dir = "dev",
                        attribute_profile_enabled = NULL) {
  if (is.null(path) || length(path) == 0 || is.na(path[1]) || path[1] == "") {
    stop("A data pond path is required to save config.")
  }
  path <- normalizePath(path[1], mustWork = FALSE, winslash = "/")
  dataset_parent_dir <- normalise_dataset_parent_dir(dataset_parent_dir)
  cfg <- read_pond_config(path)
  cfg$active_user <- active_user_id
  cfg$dataset_parent_dir <- dataset_parent_dir
  if (!is.null(attribute_profile_enabled)) {
    cfg$attribute_profile$enabled <- isTRUE(attribute_profile_enabled)
  }
  write_pond_config(path, cfg)
  # App-local pointer only. The actual config remains pond-local.
  write_last_data_pond_path(path)
  invisible(cfg)
}

load_config <- function(default_path = normalizePath("~/Documents/data_pond", mustWork = FALSE)) {
  default_path <- normalizePath(default_path, mustWork = FALSE, winslash = "/")
  # 1. Prefer explicit app-local last-used pond pointer.
  last_pond <- read_last_data_pond_path()
  if (!is.null(last_pond) && last_pond != "") {
    assert_saved_data_pond_available(last_pond)
    cfg <- read_pond_config(last_pond)
    return(cfg)
  }

  # 2. When running from the former standalone source tree, migrate its
  # last-used pond pointer before consulting the older config.rds fallback.
  legacy_pointer <- file.path("data", "last_data_pond_path")
  if (file.exists(legacy_pointer)) {
    legacy_pond <- trimws(readLines(legacy_pointer, warn = FALSE, n = 1L))

    if (
      length(legacy_pond) > 0L &&
      !is.na(legacy_pond[1]) &&
      nzchar(legacy_pond[1])
    ) {
      legacy_pond <- normalizePath(
        path.expand(legacy_pond[1]),
        mustWork = FALSE,
        winslash = "/"
      )
      assert_saved_data_pond_available(legacy_pond)
      write_last_data_pond_path(legacy_pond)
      return(read_pond_config(legacy_pond))
    }
  }

  # 3. One-time fallback for older app-local config.
  # This is only used if no last-used pointer exists.
  old <- file.path("data", "config.rds")
  if (file.exists(old)) {
    old_cfg <- readRDS(old)
    pond <- old_cfg$data_pond %||% default_path
    pond <- normalizePath(pond, mustWork = FALSE, winslash = "/")
    assert_saved_data_pond_available(pond)
    cfg <- read_pond_config(pond)
    marker <- old_config_migrated_marker(pond)
    if (!file.exists(marker)) {
      cfg$active_user <- old_cfg$active_user %||% cfg$active_user
      cfg$dataset_parent_dir <- normalise_dataset_parent_dir(
        old_cfg$dataset_parent_dir %||% cfg$dataset_parent_dir
      )
      write_pond_config(pond, cfg)
      writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), marker)
    }
    # Create the new app-local pointer so old config is not repeatedly authoritative.
    write_last_data_pond_path(pond)
    return(read_pond_config(pond))
  }
  # 4. Final fallback.
  write_last_data_pond_path(default_path)
  read_pond_config(default_path)
}
