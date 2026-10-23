# Portable dataset bundles ----

dataset_transfer_format_version <- function() {
  # Version 5 adds metadata review requests/returns and continuous import history.
  5L
}


transfer_scalar_text <- function(x, default = NA_character_) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    is.na(x[1]) ||
    !nzchar(trimws(as.character(x[1])))
  ) {
    return(default)
  }

  as.character(x[1])
}


transfer_scalar_integer <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(x[1]))
  if (length(value) == 0L || is.na(value)) default else value
}


transfer_quote_table <- function(conn, table) {
  as.character(DBI::dbQuoteIdentifier(conn, table))
}


transfer_table_rows <- function(conn, table, dataset_id) {
  DBI::dbGetQuery(
    conn,
    paste0(
      "SELECT * FROM ",
      transfer_quote_table(conn, table),
      " WHERE dataset_id = ?"
    ),
    params = list(as.integer(dataset_id))
  )
}


transfer_ids_from_columns <- function(tables, pattern, exact = character()) {
  values <- integer()

  for (table in tables) {
    if (is.null(table) || nrow(table) == 0L) {
      next
    }

    fields <- names(table)
    selected <- unique(c(
      fields[grepl(pattern, fields)],
      intersect(fields, exact)
    ))

    for (field in selected) {
      values <- c(values, suppressWarnings(as.integer(table[[field]])))
    }
  }

  unique(values[!is.na(values)])
}


transfer_reference_rows <- function(conn, table, ids) {
  if (length(ids) == 0L || !DBI::dbExistsTable(conn, table)) {
    return(data.frame())
  }

  rows <- DBI::dbReadTable(conn, table)
  rows[rows$id %in% ids, , drop = FALSE]
}


transfer_user_name <- function(users, people, user_id) {
  user_id <- transfer_scalar_integer(user_id)
  if (is.na(user_id) || is.null(users) || nrow(users) == 0L) {
    return(NA_character_)
  }

  user <- users[users$id == user_id, , drop = FALSE]
  if (nrow(user) == 0L) {
    return(NA_character_)
  }

  user_name <- transfer_scalar_text(user$user_name)
  if (!is.na(user_name)) {
    return(user_name)
  }

  person <- people[people$id == user$person_id[1], , drop = FALSE]
  if (nrow(person) == 0L) {
    return(NA_character_)
  }

  trimws(paste(person$name_first[1], person$name_last[1]))
}


collect_dataset_transfer_payload <- function(
  dataset_id,
  base_path,
  exporting_user_id = NULL,
  bundle_id = new_uuid(),
  conn = NULL
) {
  if (is.null(conn)) {
    conn <- get_conn(base_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  entry <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM entry_tbl WHERE id = ?",
    params = list(as.integer(dataset_id))
  )

  if (nrow(entry) != 1L) {
    stop("The selected dataset record could not be found.", call. = FALSE)
  }

  # Compatibility views (for example temporal coverage) are derived and must
  # not be copied as mutable dataset tables.
  dataset_tables <- DBI::dbGetQuery(
    conn, "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name"
  )$name
  dataset_tables <- dataset_tables[vapply(
    dataset_tables,
    function(table) "dataset_id" %in% DBI::dbListFields(conn, table),
    logical(1)
  )]
  dataset_tables <- setdiff(dataset_tables, c("dataset_review_exchange_tbl", release_control_tables()))

  scoped <- stats::setNames(
    lapply(dataset_tables, transfer_table_rows, conn = conn, dataset_id = dataset_id),
    dataset_tables
  )

  identity_tables <- c(list(entry), scoped[setdiff(names(scoped), "dataset_import_tbl")])
  thesauri <- if (DBI::dbExistsTable(conn, "keyword_thesaurus_tbl")) {
    DBI::dbReadTable(conn, "keyword_thesaurus_tbl")
  } else data.frame()
  person_ids <- transfer_ids_from_columns(
    c(identity_tables, list(thesauri)),
    pattern = "person_id$",
    exact = "reviewer_id"
  )
  user_ids <- transfer_ids_from_columns(
    identity_tables,
    pattern = "user_id$"
  )
  user_ids <- unique(c(
    user_ids,
    transfer_scalar_integer(exporting_user_id),
    suppressWarnings(as.integer(scoped$dataset_import_tbl$imported_by_user_id))
  ))
  user_ids <- user_ids[!is.na(user_ids)]

  users <- transfer_reference_rows(conn, "user_tbl", user_ids)
  person_ids <- unique(c(person_ids, suppressWarnings(as.integer(users$person_id))))
  person_ids <- person_ids[!is.na(person_ids)]
  people <- transfer_reference_rows(conn, "persons_tbl", person_ids)

  organisation_ids <- transfer_ids_from_columns(
    c(identity_tables, list(thesauri)),
    pattern = "organisation_id$"
  )
  organisation_ids <- unique(c(
    organisation_ids,
    suppressWarnings(as.integer(people$organisation_id))
  ))
  organisation_ids <- organisation_ids[!is.na(organisation_ids)]
  organisations <- transfer_reference_rows(
    conn,
    "organisation_tbl",
    organisation_ids
  )
  # Include all ancestors needed to reconstruct the organisational hierarchy.
  repeat {
    parent_ids <- suppressWarnings(as.integer(organisations$parent_organisation_id))
    parent_ids <- unique(parent_ids[!is.na(parent_ids) & !parent_ids %in% organisations$id])
    if (!length(parent_ids)) break
    organisations <- rbind(organisations, transfer_reference_rows(conn, "organisation_tbl", parent_ids))
  }
  party_addresses <- if (DBI::dbExistsTable(conn, "party_address_tbl")) {
    addresses <- DBI::dbReadTable(conn, "party_address_tbl")
    addresses[
      (!is.na(addresses$person_id) & addresses$person_id %in% people$id) |
        (!is.na(addresses$organisation_id) & addresses$organisation_id %in% organisations$id),
      , drop = FALSE
    ]
  } else data.frame()

  template_ids <- transfer_ids_from_columns(
    identity_tables,
    pattern = "license_template_id$"
  )
  license_templates <- transfer_reference_rows(
    conn,
    "license_template_tbl",
    template_ids
  )

  source_pond_id <- transfer_scalar_text(entry$data_pond_id)
  if (is.na(source_pond_id)) {
    source_pond_id <- ensure_data_pond_id(base_path, conn = conn)
  }

  source_dataset_path <- normalizePath(
    transfer_scalar_text(entry$dataset_path),
    mustWork = TRUE,
    winslash = "/"
  )
  pond_path <- normalizePath(base_path, mustWork = TRUE, winslash = "/")
  relative_dataset_path <- substring(
    source_dataset_path,
    nchar(paste0(sub("/+$", "", pond_path), "/")) + 1L
  )
  relative_parts <- strsplit(relative_dataset_path, "/", fixed = TRUE)[[1]]
  source_lifecycle_root <- relative_parts[1]
  source_relative_parent <- if (length(relative_parts) > 2L) {
    paste(relative_parts[2:(length(relative_parts) - 1L)], collapse = "/")
  } else {
    ""
  }
  source_created_by_user_id <- transfer_scalar_integer(
    entry$created_by_user_id
  )
  source_dataset_uuid <- transfer_scalar_text(entry$uuid)
  if (is.na(source_dataset_uuid)) {
    stop("The selected dataset has no UUID and cannot be bundled.", call. = FALSE)
  }
  exporting_user_id <- transfer_scalar_integer(exporting_user_id)
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  manifest <- data.frame(
    bundle_format = "datapond_dataset_bundle",
    bundle_format_version = dataset_transfer_format_version(),
    bundle_id = as.character(bundle_id),
    app_version = as.character(utils::packageVersion("datapond")),
    exported_at = now,
    source_dataset_id = as.integer(dataset_id),
    source_dataset_uuid = source_dataset_uuid,
    source_data_pond_id = source_pond_id,
    source_created_by_user_id = source_created_by_user_id,
    source_created_by_user_name = transfer_user_name(
      users,
      people,
      source_created_by_user_id
    ),
    source_exported_by_user_id = exporting_user_id,
    source_exported_by_user_name = transfer_user_name(
      users,
      people,
      exporting_user_id
    ),
    source_dataset_path = source_dataset_path,
    source_dataset_folder = basename(source_dataset_path),
    source_parent_dir = source_lifecycle_root,
    source_relative_parent = source_relative_parent,
    dataset_title = transfer_scalar_text(entry$title, "Untitled dataset"),
    stringsAsFactors = FALSE
  )

  list(
    manifest = manifest,
    entry_tbl = entry,
    scoped = scoped,
    organisation_tbl = organisations,
    persons_tbl = people,
    user_tbl = users,
    license_template_tbl = license_templates,
    party_address_tbl = party_addresses,
    keyword_thesaurus_tbl = thesauri
  )
}


write_dataset_transfer_database <- function(payload, path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbWriteTable(conn, "bundle_manifest_tbl", payload$manifest)
  DBI::dbWriteTable(conn, "entry_tbl", payload$entry_tbl)

  for (table in names(payload$scoped)) {
    rows <- payload$scoped[[table]]
    if (!is.null(rows) && nrow(rows) > 0L) {
      DBI::dbWriteTable(conn, table, rows)
    }
  }

  for (table in c(
    "organisation_tbl",
    "persons_tbl",
    "user_tbl",
    "license_template_tbl",
    "party_address_tbl",
    "keyword_thesaurus_tbl"
  )) {
    rows <- payload[[table]]
    if (!is.null(rows) && nrow(rows) > 0L) {
      DBI::dbWriteTable(conn, table, rows)
    }
  }

  invisible(path)
}


create_dataset_transfer_bundle <- function(
  dataset_id,
  base_path,
  exporting_user_id = NULL
) {
  compression_level <- getOption("datapond.bundle_compression_level", 6L)
  if (!is.numeric(compression_level) || length(compression_level) != 1L ||
      is.na(compression_level) || !is.finite(compression_level) ||
      compression_level < 0L || compression_level > 9L ||
      compression_level != as.integer(compression_level)) {
    stop("datapond.bundle_compression_level must be an integer from 0 to 9.", call. = FALSE)
  }
  payload <- collect_dataset_transfer_payload(
    dataset_id = dataset_id,
    base_path = base_path,
    exporting_user_id = exporting_user_id
  )
  dataset_path <- payload$manifest$source_dataset_path[1]

  pond_path <- normalizePath(base_path, mustWork = TRUE, winslash = "/")
  allowed_roots <- normalizePath(
    file.path(pond_path, c("src", "dev", "arc")),
    mustWork = FALSE,
    winslash = "/"
  )
  in_managed_root <- any(vapply(
    allowed_roots,
    function(root) {
      identical(dataset_path, root) || startsWith(dataset_path, paste0(root, "/"))
    },
    logical(1)
  ))
  if (!in_managed_root) {
    stop("The dataset folder is outside this pond's managed lifecycle folders.", call. = FALSE)
  }

  metadata_dir <- file.path(dataset_path, ".datapond_transfer")
  if (file.exists(metadata_dir)) {
    stop(
      "The dataset folder already contains .datapond_transfer. Remove or rename it before exporting.",
      call. = FALSE
    )
  }
  dir.create(metadata_dir, recursive = FALSE)
  on.exit(unlink(metadata_dir, recursive = TRUE, force = TRUE), add = TRUE)

  metadata_path <- file.path(metadata_dir, "dataset_metadata.sqlite")
  write_dataset_transfer_database(payload, metadata_path)

  source_files <- list.files(
    dataset_path,
    recursive = TRUE,
    all.files = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  root_bundle_pattern <- "^[^/]+__datapond_[[:alnum:]_-]+\\.zip$"
  source_files <- source_files[!grepl(root_bundle_pattern, source_files)]
  archive_files <- file.path(basename(dataset_path), source_files)

  archive_name <- paste0(
    basename(dataset_path),
    "__datapond_",
    substr(payload$manifest$bundle_id[1], 1L, 8L),
    ".zip"
  )
  output_path <- file.path(dataset_path, archive_name)
  temp_archive <- tempfile("datapond-bundle-", fileext = ".zip")
  on.exit(unlink(temp_archive, force = TRUE), add = TRUE)

  old_directory <- setwd(dirname(dataset_path))
  on.exit(setwd(old_directory), add = TRUE)
  suppressWarnings(utils::zip(
    zipfile = temp_archive,
    files = archive_files,
    flags = paste0("-r", as.integer(compression_level), "X")
  ))

  if (!file.exists(temp_archive) || file.info(temp_archive)$size <= 0) {
    stop("The ZIP archive could not be created.", call. = FALSE)
  }
  if (!isTRUE(file.copy(temp_archive, output_path, overwrite = FALSE))) {
    stop("The completed bundle could not be placed in the dataset folder.", call. = FALSE)
  }

  list(
    path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
    archive_name = archive_name,
    bundle_id = payload$manifest$bundle_id[1],
    manifest = payload$manifest
  )
}


dataset_transfer_archive_index <- function(path) {
  if (!file.exists(path)) {
    stop("The uploaded bundle file no longer exists.", call. = FALSE)
  }

  index <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) stop("The uploaded file is not a readable ZIP archive.", call. = FALSE)
  )
  names <- gsub("\\\\", "/", as.character(index$Name))
  unsafe <- (
    grepl("^/", names) |
      grepl("^[A-Za-z]:", names) |
      grepl("(^|/)\\.\\.(/|$)", names)
  )
  if (length(names) == 0L || any(unsafe)) {
    stop("The bundle contains an unsafe or empty archive path.", call. = FALSE)
  }

  metadata <- names[grepl(
    "^[^/]+/\\.datapond_transfer/dataset_metadata\\.sqlite$",
    names
  )]
  if (length(metadata) != 1L) {
    stop("This ZIP is not a Data Pond dataset bundle.", call. = FALSE)
  }

  root <- strsplit(metadata[1], "/", fixed = TRUE)[[1]][1]
  if (any(!(names == root | startsWith(names, paste0(root, "/"))))) {
    stop("A dataset bundle must contain exactly one top-level dataset folder.", call. = FALSE)
  }

  list(index = index, names = names, metadata = metadata[1], root = root)
}


read_dataset_transfer_manifest <- function(path) {
  archive <- dataset_transfer_archive_index(path)
  extract_dir <- tempfile("datapond-bundle-inspect-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(path, files = archive$metadata, exdir = extract_dir)
  database_path <- file.path(extract_dir, archive$metadata)
  conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (!DBI::dbExistsTable(conn, "bundle_manifest_tbl")) {
    stop("The bundle manifest is missing.", call. = FALSE)
  }
  manifest <- DBI::dbReadTable(conn, "bundle_manifest_tbl")
  if (nrow(manifest) != 1L) {
    stop("The bundle manifest is invalid.", call. = FALSE)
  }
  if (!identical(
    transfer_scalar_text(manifest$bundle_format),
    "datapond_dataset_bundle"
  )) {
    stop("The uploaded ZIP uses an unknown bundle format.", call. = FALSE)
  }
  version <- transfer_scalar_integer(manifest$bundle_format_version)
  if (is.na(version) || version > dataset_transfer_format_version()) {
    stop("This bundle was created by a newer, unsupported Data Pond format.", call. = FALSE)
  }

  manifest
}


transfer_row_value <- function(row, field, default = NA) {
  if (!field %in% names(row) || length(row[[field]]) == 0L) {
    return(default)
  }
  row[[field]][1]
}


transfer_insert_row <- function(conn, table, row, drop = "id") {
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  fields <- intersect(names(row), DBI::dbListFields(conn, table))
  fields <- setdiff(fields, drop)
  if (length(fields) == 0L) {
    stop("No compatible columns were found for ", table, ".", call. = FALSE)
  }

  sql <- paste0(
    "INSERT INTO ", transfer_quote_table(conn, table), " (",
    paste(as.character(DBI::dbQuoteIdentifier(conn, fields)), collapse = ", "),
    ") VALUES (", paste(rep("?", length(fields)), collapse = ", "), ")"
  )
  values <- lapply(fields, function(field) row[[field]][1])
  DBI::dbExecute(conn, sql, params = values)
  as.integer(DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1])
}


transfer_import_organisation <- function(conn, row) {
  full <- transfer_scalar_text(transfer_row_value(row, "organisation_name_full"))
  code <- transfer_scalar_text(transfer_row_value(row, "organisation_code"))
  department <- transfer_scalar_text(transfer_row_value(row, "organisation_department"), "")

  existing <- if (!is.na(full)) {
    DBI::dbGetQuery(
      conn,
      paste(
        "SELECT id FROM organisation_tbl",
        "WHERE LOWER(TRIM(organisation_name_full)) = LOWER(TRIM(?))",
        "AND COALESCE(TRIM(organisation_department), '') = ? LIMIT 1"
      ),
      params = list(full, department)
    )
  } else if (!is.na(code)) {
    DBI::dbGetQuery(
      conn,
      paste(
        "SELECT id FROM organisation_tbl",
        "WHERE LOWER(TRIM(organisation_code)) = LOWER(TRIM(?))",
        "AND COALESCE(TRIM(organisation_department), '') = ? LIMIT 1"
      ),
      params = list(code, department)
    )
  } else {
    data.frame()
  }

  if (nrow(existing) > 0L) {
    return(as.integer(existing$id[1]))
  }
  # Parent identifiers are source-database keys. They are restored after all
  # organisations have been matched or inserted and the complete map exists.
  if ("parent_organisation_id" %in% names(row)) row$parent_organisation_id <- NA_integer_
  transfer_insert_row(conn, "organisation_tbl", row)
}


transfer_remap_id <- function(value, map) {
  value <- transfer_scalar_integer(value)
  if (is.na(value) || length(map) == 0L || !as.character(value) %in% names(map)) {
    return(NA_integer_)
  }
  as.integer(unname(map[as.character(value)]))
}


transfer_import_person <- function(conn, row, organisation_map) {
  source_org <- transfer_row_value(row, "organisation_id")
  target_org <- transfer_remap_id(source_org, organisation_map)
  row$organisation_id <- target_org
  email <- transfer_scalar_text(transfer_row_value(row, "email"))

  existing <- if (!is.na(email)) {
    DBI::dbGetQuery(
      conn,
      "SELECT id FROM persons_tbl WHERE LOWER(TRIM(email)) = LOWER(TRIM(?)) LIMIT 1",
      params = list(email)
    )
  } else {
    DBI::dbGetQuery(
      conn,
      paste(
        "SELECT id FROM persons_tbl",
        "WHERE LOWER(COALESCE(TRIM(name_first), '')) = LOWER(?)",
        "AND LOWER(COALESCE(TRIM(name_last), '')) = LOWER(?)",
        "AND COALESCE(organisation_id, -1) = COALESCE(?, -1) LIMIT 1"
      ),
      params = list(
        transfer_scalar_text(transfer_row_value(row, "name_first"), ""),
        transfer_scalar_text(transfer_row_value(row, "name_last"), ""),
        target_org
      )
    )
  }

  if (nrow(existing) > 0L) {
    return(as.integer(existing$id[1]))
  }
  transfer_insert_row(conn, "persons_tbl", row)
}


transfer_import_user <- function(conn, row, person_map) {
  target_person <- transfer_remap_id(
    transfer_row_value(row, "person_id"),
    person_map
  )
  if (is.na(target_person)) {
    return(NA_integer_)
  }
  user_name <- transfer_scalar_text(transfer_row_value(row, "user_name"))

  existing <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM user_tbl WHERE person_id = ? LIMIT 1",
    params = list(target_person)
  )
  if (nrow(existing) > 0L) {
    return(as.integer(existing$id[1]))
  }

  if (!is.na(user_name)) {
    name_collision <- DBI::dbGetQuery(
      conn,
      "SELECT id FROM user_tbl WHERE LOWER(TRIM(user_name)) = LOWER(TRIM(?)) LIMIT 1",
      params = list(user_name)
    )
    if (nrow(name_collision) > 0L) {
      user_name <- paste0(user_name, "__imported_", target_person)
    }
  }

  row$person_id <- target_person
  row$user_name <- user_name
  row$is_active <- 1L
  row$is_admin <- 0L
  transfer_insert_row(conn, "user_tbl", row)
}


transfer_map_columns <- function(row, fields, map) {
  for (field in intersect(fields, names(row))) {
    row[[field]] <- transfer_remap_id(row[[field]], map)
  }
  row
}


transfer_rewrite_row_paths <- function(row, source_path, target_path) {
  source_path <- sub("/+$", "", gsub("\\\\", "/", source_path))
  for (field in names(row)) {
    value <- row[[field]][1]
    if (!is.character(value) || is.na(value)) {
      next
    }
    normal <- gsub("\\\\", "/", value)
    if (identical(normal, source_path)) {
      row[[field]] <- target_path
    } else if (startsWith(normal, paste0(source_path, "/"))) {
      row[[field]] <- paste0(target_path, substring(normal, nchar(source_path) + 1L))
    }
  }
  row
}


transfer_import_scoped_table <- function(
  conn,
  table,
  rows,
  dataset_id,
  pond_id,
  source_path,
  target_path,
  person_map,
  organisation_map,
  user_map,
  file_map,
  scan_map,
  reference_maps = list()
) {
  if (is.null(rows) || nrow(rows) == 0L || !DBI::dbExistsTable(conn, table)) {
    return(integer())
  }

  id_map <- integer()
  person_fields <- names(rows)[grepl("person_id$", names(rows))]
  person_fields <- unique(c(person_fields, intersect("reviewer_id", names(rows))))
  organisation_fields <- names(rows)[grepl("organisation_id$", names(rows))]
  user_fields <- names(rows)[grepl("user_id$", names(rows))]

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    source_id <- transfer_scalar_integer(transfer_row_value(row, "id"))
    row$dataset_id <- as.integer(dataset_id)
    if ("data_pond_id" %in% names(row)) row$data_pond_id <- pond_id
    row <- transfer_map_columns(row, person_fields, person_map)
    row <- transfer_map_columns(row, organisation_fields, organisation_map)
    row <- transfer_map_columns(row, user_fields, user_map)
    if ("data_file_id" %in% names(row)) {
      row$data_file_id <- transfer_remap_id(row$data_file_id, file_map)
    }
    if ("latest_scan_id" %in% names(row)) {
      row$latest_scan_id <- transfer_remap_id(row$latest_scan_id, scan_map)
    }
    for (field in intersect(names(reference_maps), names(row))) {
      row[[field]] <- transfer_remap_id(row[[field]], reference_maps[[field]])
    }
    row <- transfer_rewrite_row_paths(row, source_path, target_path)
    new_id <- transfer_insert_row(conn, table, row)
    if (!is.na(source_id)) id_map[as.character(source_id)] <- new_id
  }
  id_map
}


import_dataset_transfer_bundle <- function(
  path,
  base_path,
  importing_user_id,
  .review = NULL
) {
  importing_user_id <- transfer_scalar_integer(importing_user_id)
  if (is.na(importing_user_id)) {
    stop("Select an active user before importing a dataset.", call. = FALSE)
  }

  archive <- dataset_transfer_archive_index(path)
  extract_dir <- tempfile("datapond-bundle-import-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = extract_dir)

  source_dataset_path <- file.path(extract_dir, archive$root)
  database_path <- file.path(
    source_dataset_path,
    ".datapond_transfer",
    "dataset_metadata.sqlite"
  )
  source_conn <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  on.exit({
    if (!is.null(source_conn) && DBI::dbIsValid(source_conn)) {
      DBI::dbDisconnect(source_conn)
    }
  }, add = TRUE)
  if (!DBI::dbExistsTable(source_conn, "bundle_manifest_tbl") ||
      !DBI::dbExistsTable(source_conn, "entry_tbl")) {
    stop("The bundle metadata database is incomplete.", call. = FALSE)
  }

  manifest <- DBI::dbReadTable(source_conn, "bundle_manifest_tbl")
  if (nrow(manifest) != 1L ||
      !identical(transfer_scalar_text(manifest$bundle_format), "datapond_dataset_bundle")) {
    stop("The bundle manifest is invalid.", call. = FALSE)
  }
  version <- transfer_scalar_integer(manifest$bundle_format_version)
  if (is.na(version) || version > dataset_transfer_format_version()) {
    stop("This bundle was created by a newer, unsupported Data Pond format.", call. = FALSE)
  }

  kind <- transfer_scalar_text(manifest$review_kind, "transfer")
  if (kind == "return" && is.null(.review))
    stop("Use Review returned bundle to inspect and accept this return.", call. = FALSE)
  if (kind == "request" && !is.null(.review)) stop("This is a request, not a return.")
  if (!kind %in% c("transfer", "request", "return")) stop("Unknown review bundle purpose.")
  if (kind %in% c("request", "return")) {
    if (version != 5L) stop("Review exchange requires bundle format 5.")
    review_check_archive(archive)
  }

  entry <- DBI::dbReadTable(source_conn, "entry_tbl")
  if (nrow(entry) != 1L) {
    stop("The bundle must contain one dataset database record.", call. = FALSE)
  }
  source_uuid <- transfer_scalar_text(manifest$source_dataset_uuid)
  if (is.na(source_uuid) || !identical(source_uuid, transfer_scalar_text(entry$uuid))) {
    stop("The bundle dataset identity does not match its manifest.", call. = FALSE)
  }

  bundle_tables <- DBI::dbListTables(source_conn)
  read_optional <- function(table) {
    if (table %in% bundle_tables) DBI::dbReadTable(source_conn, table) else data.frame()
  }
  organisations <- read_optional("organisation_tbl")
  people <- read_optional("persons_tbl")
  users <- read_optional("user_tbl")
  license_templates <- read_optional("license_template_tbl")
  party_addresses <- read_optional("party_address_tbl")
  thesauri <- read_optional("keyword_thesaurus_tbl")
  scoped_names <- setdiff(
    bundle_tables,
    c(
      release_control_tables(), "bundle_manifest_tbl", "entry_tbl", "organisation_tbl",
      "persons_tbl", "user_tbl", "license_template_tbl",
      "party_address_tbl", "keyword_thesaurus_tbl"
    )
  )
  scoped <- stats::setNames(lapply(scoped_names, read_optional), scoped_names)
  entry_schema <- DBI::dbGetQuery(source_conn, "PRAGMA table_info(entry_tbl)")
  DBI::dbDisconnect(source_conn)
  source_conn <- NULL

  if (is.null(.review)) init_db(base_path)
  target_conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(target_conn), add = TRUE)
  ensure_dataset_import_table(target_conn)
  # Entry fields are added lazily by the metadata forms. A fresh target pond
  # may never have opened those forms; intersecting its columns with the bundle
  # would silently discard purpose, citation, rights and custom schema fields.
  # Quote identifiers from the bundle and restrict declarations to SQLite types.
  missing_entry_fields <- setdiff(names(entry), DBI::dbListFields(target_conn, "entry_tbl"))
  if (is.null(.review)) for (field in missing_entry_fields) {
    source_type <- toupper(entry_schema$type[match(field, entry_schema$name)])
    column_type <- if (source_type %in% c("INTEGER", "REAL", "TEXT", "BLOB", "NUMERIC")) {
      source_type
    } else {
      "TEXT"
    }
    DBI::dbExecute(target_conn, paste(
      "ALTER TABLE entry_tbl ADD COLUMN",
      DBI::dbQuoteIdentifier(target_conn, field),
      column_type
    ))
  }
  target_pond_id <- ensure_data_pond_id(base_path, conn = target_conn)
  importing_user <- DBI::dbGetQuery(
    target_conn,
    "SELECT id FROM user_tbl WHERE id = ? AND COALESCE(is_active, 1) = 1 LIMIT 1",
    params = list(importing_user_id)
  )
  if (nrow(importing_user) != 1L) {
    stop("The importing user is not an active user in the target pond.", call. = FALSE)
  }

  existing_uuid <- DBI::dbGetQuery(
    target_conn,
    "SELECT id FROM entry_tbl WHERE uuid = ? LIMIT 1",
    params = list(source_uuid)
  )
  if (nrow(existing_uuid) > 0L && is.null(.review)) {
    stop(
      "A dataset with UUID ", source_uuid,
      " already exists in this pond; the bundle was not imported.",
      call. = FALSE
    )
  }
  existing_bundle <- DBI::dbGetQuery(
    target_conn,
    "SELECT id FROM dataset_import_tbl WHERE bundle_id = ? LIMIT 1",
    params = list(transfer_scalar_text(manifest$bundle_id))
  )
  if (nrow(existing_bundle) > 0L) {
    stop("This exact bundle has already been imported.", call. = FALSE)
  }

  if (!is.null(.review)) {
    if (nrow(existing_uuid) != 1L || existing_uuid$id[1] != .review$dataset_id)
      stop("The return does not match the master dataset.")
    master_entry <- DBI::dbGetQuery(target_conn, "SELECT * FROM entry_tbl WHERE id = ?",
      params = list(.review$dataset_id))
    target_dataset_path <- master_entry$dataset_path[1]
  } else {
  source_parent <- transfer_scalar_text(manifest$source_parent_dir, "dev")
  if (!source_parent %in% c("src", "dev", "arc")) source_parent <- "dev"
  relative_parent <- transfer_scalar_text(
    manifest$source_relative_parent,
    ""
  )
  relative_parts <- if (nzchar(relative_parent)) {
    strsplit(gsub("\\\\", "/", relative_parent), "/", fixed = TRUE)[[1]]
  } else {
    character()
  }
  if (
    any(relative_parts %in% c("", ".", "..")) ||
    any(grepl("^[A-Za-z]:$", relative_parts))
  ) {
    stop("The bundle contains an unsafe lifecycle-relative path.", call. = FALSE)
  }
  target_parent <- do.call(file.path, as.list(c(base_path, source_parent, relative_parts)))
  dir.create(target_parent, recursive = TRUE, showWarnings = FALSE)
  target_dataset_path <- file.path(target_parent, archive$root)
  if (file.exists(target_dataset_path)) {
    stop(
      "A dataset folder named ", archive$root,
      " already exists under ", source_parent, "/.",
      call. = FALSE
    )
  }

  copied <- file.copy(
    source_dataset_path,
    target_parent,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!isTRUE(copied) || !dir.exists(target_dataset_path)) {
    stop("The dataset folder could not be copied into the active pond.", call. = FALSE)
  }
  import_complete <- FALSE
  on.exit({
    if (!isTRUE(import_complete) && dir.exists(target_dataset_path)) {
      unlink(target_dataset_path, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  unlink(
    file.path(target_dataset_path, ".datapond_transfer"),
    recursive = TRUE,
    force = TRUE
  )
  for (standard_dir in c("data", "docs", "scripts")) {
    dir.create(
      file.path(target_dataset_path, standard_dir),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  write_dataset_marker(
    target_dataset_path,
    dataset_uuid = source_uuid,
    data_pond_id = target_pond_id
  )

  }

  source_record_path <- transfer_scalar_text(manifest$source_dataset_path)
  import_scoped <- function(conn, table, ...) {
    if (!is.null(.review) && table %in% names(.review$protected_maps)) return(.review$protected_maps[[table]])
    transfer_import_scoped_table(conn, table, ...)
  }
  result <- DBI::dbWithTransaction(target_conn, {
    if (!is.null(.review)) {
      review_begin_accept(target_conn, .review, base_path)
      for (field in missing_entry_fields) {
        type <- toupper(entry_schema$type[match(field, entry_schema$name)])
        if (!type %in% c("INTEGER", "REAL", "TEXT", "BLOB", "NUMERIC")) type <- "TEXT"
        DBI::dbExecute(target_conn, paste("ALTER TABLE entry_tbl ADD COLUMN", DBI::dbQuoteIdentifier(target_conn, field), type))
      }
    }

    organisation_map <- integer()
    existing_organisation_ids <- if (!is.null(.review)) DBI::dbGetQuery(target_conn, "SELECT id FROM organisation_tbl")$id else integer()
    if (nrow(organisations) > 0L) {
      for (i in seq_len(nrow(organisations))) {
        source_id <- as.integer(organisations$id[i])
        organisation_map[as.character(source_id)] <-
          transfer_import_organisation(target_conn, organisations[i, , drop = FALSE])
      }
      if ("parent_organisation_id" %in% names(organisations)) for (i in seq_len(nrow(organisations))) {
          if (!is.null(.review) && organisation_map[as.character(organisations$id[i])] %in% existing_organisation_ids) next
          parent_id <- transfer_remap_id(organisations$parent_organisation_id[i], organisation_map)
          if (!is.na(parent_id)) DBI::dbExecute(
            target_conn, "UPDATE organisation_tbl SET parent_organisation_id = ? WHERE id = ?",
            params = list(parent_id, organisation_map[as.character(organisations$id[i])])
          )
        }
    }

    person_map <- integer()
    if (nrow(people) > 0L) {
      for (i in seq_len(nrow(people))) {
        source_id <- as.integer(people$id[i])
        person_map[as.character(source_id)] <- transfer_import_person(
          target_conn,
          people[i, , drop = FALSE],
          organisation_map
        )
      }
    }

    user_map <- integer()
    if (nrow(users) > 0L) {
      for (i in seq_len(nrow(users))) {
        source_id <- as.integer(users$id[i])
        user_map[as.character(source_id)] <- transfer_import_user(
          target_conn,
          users[i, , drop = FALSE],
          person_map
        )
      }
    }

    if (is.null(.review) && nrow(party_addresses) > 0L) for (i in seq_len(nrow(party_addresses))) {
      address <- party_addresses[i, , drop = FALSE]
      address$person_id <- transfer_remap_id(address$person_id, person_map)
      address$organisation_id <- transfer_remap_id(address$organisation_id, organisation_map)
      if (!is.na(address$person_id) || !is.na(address$organisation_id)) {
        transfer_insert_row(target_conn, "party_address_tbl", address)
      }
    }
    if (is.null(.review) && nrow(thesauri) > 0L) for (i in seq_len(nrow(thesauri))) {
      thesaurus <- thesauri[i, , drop = FALSE]
      thesaurus$field_code <- canonical_keyword_field_code(thesaurus$field_code)
      thesaurus$responsible_person_id <- transfer_remap_id(thesaurus$responsible_person_id, person_map)
      thesaurus$responsible_organisation_id <- transfer_remap_id(thesaurus$responsible_organisation_id, organisation_map)
      existing_thesaurus <- DBI::dbGetQuery(target_conn,
        "SELECT id FROM keyword_thesaurus_tbl WHERE field_code = ? COLLATE NOCASE",
        params = list(thesaurus$field_code[1]))
      target_thesaurus_id <- if (!nrow(existing_thesaurus)) {
        transfer_insert_row(target_conn, "keyword_thesaurus_tbl", thesaurus)
      } else as.integer(existing_thesaurus$id[1])
      DBI::dbExecute(target_conn,
        "UPDATE keyword_vocab_tbl SET thesaurus_id = ? WHERE field_code = ? COLLATE NOCASE",
        params = list(target_thesaurus_id, thesaurus$field_code[1]))
    }

    license_template_map <- integer()
    if (nrow(license_templates) > 0L) {
      for (i in seq_len(nrow(license_templates))) {
        source_id <- as.integer(license_templates$id[i])
        identifier <- transfer_scalar_text(
          transfer_row_value(license_templates[i, , drop = FALSE], "license_identifier")
        )
        if (is.na(identifier)) next
        target_template <- DBI::dbGetQuery(
          target_conn,
          paste(
            "SELECT id FROM license_template_tbl",
            "WHERE LOWER(TRIM(license_identifier)) = LOWER(TRIM(?)) LIMIT 1"
          ),
          params = list(identifier)
        )
        if (nrow(target_template) > 0L) {
          license_template_map[as.character(source_id)] <-
            as.integer(target_template$id[1])
        }
      }
    }

    entry_row <- entry
    entry_row <- transfer_map_columns(
      entry_row,
      names(entry_row)[grepl("person_id$", names(entry_row))],
      person_map
    )
    entry_row <- transfer_map_columns(
      entry_row,
      names(entry_row)[grepl("user_id$", names(entry_row))],
      user_map
    )
    entry_row <- transfer_map_columns(
      entry_row,
      names(entry_row)[grepl("organisation_id$", names(entry_row))],
      organisation_map
    )
    entry_row <- transfer_rewrite_row_paths(
      entry_row,
      source_record_path,
      normalizePath(target_dataset_path, mustWork = TRUE, winslash = "/")
    )
    entry_row$dataset_path <- normalizePath(
      target_dataset_path,
      mustWork = TRUE,
      winslash = "/"
    )
    entry_row$data_path <- file.path(entry_row$dataset_path, "data")
    entry_row$data_pond_id <- target_pond_id
    entry_row$created_by_user_id <- importing_user_id
    if ("source_dataset_id" %in% names(entry_row)) entry_row$source_dataset_id <- NA_integer_
    if (is.null(.review)) {
      new_dataset_id <- transfer_insert_row(target_conn, "entry_tbl", entry_row)
    } else {
      new_dataset_id <- .review$dataset_id
      review_update_entry(target_conn, entry_row, master_entry)
    }

    scan_map <- import_scoped(
      target_conn, "data_file_scan_tbl", scoped[["data_file_scan_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, integer(), integer()
    )
    file_map <- import_scoped(
      target_conn, "data_file_tbl", scoped[["data_file_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, integer(), scan_map
    )
    attribute_map <- import_scoped(
      target_conn, "dataset_attribute_tbl", scoped[["dataset_attribute_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, file_map, scan_map
    )

    supporting_document_map <- import_scoped(
      target_conn, "supporting_document_tbl", scoped[["supporting_document_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, file_map, scan_map
    )
    lineage_source_map <- import_scoped(
      target_conn, "dataset_lineage_source_tbl", scoped[["dataset_lineage_source_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, file_map, scan_map,
      reference_maps = list(supporting_document_id = supporting_document_map)
    )
    lineage_step_map <- import_scoped(
      target_conn, "dataset_lineage_process_step_tbl",
      scoped[["dataset_lineage_process_step_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, file_map, scan_map
    )
    import_scoped(
      target_conn, "dataset_lineage_step_source_tbl",
      scoped[["dataset_lineage_step_source_tbl"]],
      new_dataset_id, target_pond_id, source_record_path, entry_row$dataset_path,
      person_map, organisation_map, user_map, file_map, scan_map,
      reference_maps = list(
        process_step_id = lineage_step_map,
        source_id = lineage_source_map
      )
    )

    imported_first <- c(
      "data_file_scan_tbl", "data_file_tbl", "supporting_document_tbl",
      "dataset_lineage_source_tbl", "dataset_lineage_process_step_tbl",
      "dataset_lineage_step_source_tbl", "dataset_attribute_tbl"
    )
    remaining <- setdiff(names(scoped), c(imported_first, "dataset_import_tbl", "dataset_review_exchange_tbl"))
    preferred <- c(
      "authority_tbl", "dataset_organisation_tbl", "poc_tbl",
      "dataset_license_tbl", "dataset_lineage_tbl", "metadata_review_tbl",
      "data_profile_run_tbl",
      "data_file_spatial_metadata_tbl", "dataset_manual_spatial_extent_tbl",
      "dataset_spatial_summary_tbl"
    )
    remaining <- c(intersect(preferred, remaining), setdiff(remaining, preferred))
    for (table in remaining) {
      table_rows <- scoped[[table]]
      if (!is.null(.review) && table == "metadata_review_tbl")
        table_rows <- review_new_history(table_rows, .review$baseline$scoped$metadata_review_tbl)
      if ("license_template_id" %in% names(table_rows)) {
        table_rows$license_template_id <- vapply(
          table_rows$license_template_id,
          transfer_remap_id,
          integer(1),
          map = license_template_map
        )
      }
      reference_maps <- if (identical(table, "dataset_quality_result_tbl")) {
        list(attribute_id = attribute_map)
      } else {
        list()
      }
      import_scoped(
        target_conn, table, table_rows, new_dataset_id, target_pond_id,
        source_record_path, entry_row$dataset_path, person_map,
        organisation_map, user_map, file_map, scan_map,
        reference_maps = reference_maps
      )
    }

    # Upgrade old bundles only after every structured child row has arrived.
    # Migrated bundles carry their row markers and are not backfilled again.
    if (is.null(.review)) apply_legacy_patch_v1(target_conn)

    review_import_history(target_conn, scoped[["dataset_import_tbl"]], new_dataset_id, user_map)

    DBI::dbExecute(
      target_conn,
      paste(
        "INSERT INTO dataset_import_tbl (",
        "dataset_id, source_dataset_uuid, source_dataset_id, source_data_pond_id,",
        "source_created_by_user_id, source_created_by_user_name,",
        "source_exported_by_user_id, source_exported_by_user_name,",
        "bundle_id, bundle_format_version, source_dataset_folder, archive_name,",
        "exported_at, imported_by_user_id",
        ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        new_dataset_id,
        source_uuid,
        transfer_scalar_integer(manifest$source_dataset_id),
        transfer_scalar_text(manifest$source_data_pond_id),
        transfer_scalar_integer(manifest$source_created_by_user_id),
        transfer_scalar_text(manifest$source_created_by_user_name),
        transfer_scalar_integer(manifest$source_exported_by_user_id),
        transfer_scalar_text(manifest$source_exported_by_user_name),
        transfer_scalar_text(manifest$bundle_id),
        version,
        transfer_scalar_text(manifest$source_dataset_folder),
        basename(path),
        transfer_scalar_text(manifest$exported_at),
        importing_user_id
      )
    )

    if (!is.null(.review)) {
      review_finish_accept(target_conn, .review, importing_user_id, manifest)
    } else if (kind == "request") {
      review_receive_request(target_conn, manifest, new_dataset_id)
    }

    list(
      dataset_id = new_dataset_id,
      dataset_uuid = source_uuid,
      dataset_path = entry_row$dataset_path,
      source_data_pond_id = transfer_scalar_text(manifest$source_data_pond_id),
      source_user_id = transfer_scalar_integer(manifest$source_exported_by_user_id),
      bundle_id = transfer_scalar_text(manifest$bundle_id)
    )
  })

  import_complete <- TRUE
  result
}


get_dataset_import_provenance <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) {
    return(data.frame())
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_import_table(conn)
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM dataset_import_tbl WHERE dataset_id = ? ORDER BY imported_at, id",
    params = list(as.integer(dataset_id[1]))
  )
}
