# Editions are ordinary dataset records with independent identities and versions.
# The scoped relationship travels with metadata; publication records stay local.
ensure_dataset_edition_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_public_derivation_tbl (
    id INTEGER PRIMARY KEY, dataset_id INTEGER NOT NULL UNIQUE,
    baseline_json TEXT NOT NULL, applied_json TEXT NOT NULL, managed_files_json TEXT NOT NULL,
    FOREIGN KEY(dataset_id) REFERENCES entry_tbl(id))")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_public_metadata_policy_tbl (
    id INTEGER PRIMARY KEY, dataset_id INTEGER NOT NULL UNIQUE, excluded_json TEXT NOT NULL,
    FOREIGN KEY(dataset_id) REFERENCES entry_tbl(id))")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS organisation_public_contact_tbl (
    organisation_id INTEGER PRIMARY KEY, person_id INTEGER,
    FOREIGN KEY(organisation_id) REFERENCES organisation_tbl(id),
    FOREIGN KEY(person_id) REFERENCES persons_tbl(id))")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_edition_tbl (
    id INTEGER PRIMARY KEY, dataset_id INTEGER NOT NULL UNIQUE,
    family_uuid TEXT NOT NULL, edition TEXT NOT NULL CHECK(edition IN ('in_house','public')),
    source_dataset_uuid TEXT, source_release_id TEXT, source_version TEXT,
    source_zip_sha256 TEXT, preparation_method TEXT, selection_json TEXT,
    FOREIGN KEY(dataset_id) REFERENCES entry_tbl(id))")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_publication_tbl (
    id INTEGER PRIMARY KEY, dataset_id INTEGER NOT NULL, release_id TEXT NOT NULL,
    action TEXT NOT NULL CHECK(action IN ('published','withdrawn')),
    location TEXT NOT NULL, note TEXT NOT NULL, happened_at TEXT NOT NULL,
    user_id INTEGER NOT NULL, FOREIGN KEY(dataset_id) REFERENCES entry_tbl(id))")
  invisible(TRUE)
}

edition_row <- function(conn, dataset_id) {
  if (!DBI::dbExistsTable(conn, "dataset_edition_tbl")) return(data.frame())
  DBI::dbGetQuery(conn, "SELECT * FROM dataset_edition_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))
}

edition_descriptor <- function(payload) {
  row <- payload$scoped$dataset_edition_tbl
  if (is.null(row) || !nrow(row)) return(NULL)
  as.list(row[1, setdiff(names(row), c("id", "dataset_id")), drop = FALSE])
}

edition_validate <- function(payload) {
  edition <- edition_descriptor(payload)
  if (is.null(edition)) return(invisible(NULL))
  if (!edition$edition %in% c("in_house", "public") || !metadata_present(edition$family_uuid))
    stop("The dataset edition is incomplete.")
  if (edition$edition == "public") {
    required <- c("source_dataset_uuid", "source_release_id", "source_version", "source_zip_sha256", "preparation_method")
    if (any(!vapply(edition[required], metadata_present, logical(1))) ||
        identical(edition$source_dataset_uuid, payload$entry_tbl$uuid[1]))
      stop("Link this public draft to an in-house release and record its preparation method before freezing.")
  }
  invisible(NULL)
}

# An older snapshot may predate designation. Preserve edition identity, but
# never attribute its public files to a source selected for a later draft.
edition_restore_snapshot <- function(conn, snapshot, dataset_id) {
  current <- edition_row(conn, dataset_id)
  saved <- snapshot$scoped$dataset_edition_tbl
  if (!nrow(current)) return(snapshot)
  if (!is.null(saved) && nrow(saved)) {
    if (saved$edition[1] != current$edition[1] || saved$family_uuid[1] != current$family_uuid[1])
      stop("Restoring this snapshot would change the dataset's edition identity or family.")
  } else {
    if (current$edition[1] == "public") {
      for (field in c("source_dataset_uuid", "source_release_id", "source_version", "source_zip_sha256", "preparation_method", "selection_json"))
        current[[field]] <- NA_character_
    }
    snapshot$scoped$dataset_edition_tbl <- current
  }
  snapshot
}

edition_editable <- function(conn, dataset_id, pond, user_id) {
  release_require_user(conn, user_id)
  release_open_review(conn, dataset_id)
  entry <- release_entry(conn, dataset_id)
  release_working_root(pond, entry)
  if (isTRUE(entry$folder_locked[1] == 1L)) stop("Unlock the working draft before changing its edition.")
  entry
}

edition_source <- function(conn, release_id, pond) {
  paths <- verify_dataset_release(release_id, pond)
  source <- edition_row(conn, paths$record$dataset_id[1])
  saved <- release_read_payload(paths$snapshot)$scoped$dataset_edition_tbl
  # A legacy release can be used after its dataset is explicitly designated.
  if (!is.null(saved) && nrow(saved)) source <- saved
  if (!nrow(source) || source$edition[1] != "in_house")
    stop("Choose a frozen release of an in-house edition.")
  list(paths = paths, edition = source)
}

#' @export
#' @noRd
set_dataset_edition <- function(dataset_id, base_path, user_id,
                                edition = c("in_house", "public"),
                                source_release_id = NULL, preparation_method = "") {
  edition <- match.arg(edition)
  lock <- release_lock(base_path, dataset_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_edition_schema(conn)
  DBI::dbWithTransaction(conn, {
    entry <- edition_editable(conn, dataset_id, base_path, user_id)
    old <- edition_row(conn, dataset_id)
    if (nrow(old) && old$edition[1] != edition)
      stop("An edition keeps its identity. Create a separate dataset for the other edition.")
    row <- data.frame(dataset_id = as.integer(dataset_id), family_uuid = entry$uuid[1], edition = edition,
      source_dataset_uuid = NA_character_, source_release_id = NA_character_, source_version = NA_character_,
      source_zip_sha256 = NA_character_, preparation_method = NA_character_, selection_json = NA_character_)
    if (edition == "public") {
      if (!metadata_present(preparation_method)) stop("Describe how the public files and documentation were prepared.")
      source <- edition_source(conn, source_release_id, base_path)
      origin <- source$paths$record
      if (origin$dataset_id[1] == dataset_id) stop("A public edition must have its own dataset identity.")
      row$family_uuid <- source$edition$family_uuid[1]
      if (nrow(old) && old$family_uuid[1] != row$family_uuid)
        stop("The public edition cannot be moved to a different dataset family.")
      row$source_dataset_uuid <- origin$dataset_uuid[1]
      row$source_release_id <- origin$release_id[1]
      row$source_version <- origin$version[1]
      row$source_zip_sha256 <- origin$zip_sha256[1]
      row$preparation_method <- trimws(preparation_method)
      if (nrow(old) && identical(old$source_release_id[1], row$source_release_id)) row$selection_json <- old$selection_json[1]
    }
    if (nrow(old) && !identical(old$source_release_id[1], row$source_release_id))
      DBI::dbExecute(conn, "DELETE FROM dataset_public_derivation_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))
    DBI::dbExecute(conn, "DELETE FROM dataset_edition_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))
    DBI::dbAppendTable(conn, "dataset_edition_tbl", row)
    release_ensure_schema(conn)
    release_event(conn, dataset_id, if (edition == "public") row$source_release_id else "",
      uuid::UUIDgenerate(), "set_edition", user_id, paste("Edition:", edition, preparation_method))
    invisible(row)
  })
}

# Public transport metadata omits local review/import history and application
# accounts. Explicit public author/contact records remain part of the metadata.
edition_transfer_payload <- function(payload) {
  edition <- edition_descriptor(payload)
  if (is.null(edition) || edition$edition != "public") return(payload)
  payload$scoped <- payload$scoped[setdiff(names(payload$scoped), c("metadata_review_tbl", "dataset_import_tbl",
    "dataset_public_derivation_tbl", "dataset_public_metadata_policy_tbl"))]
  clear_users <- function(rows) {
    for (field in names(rows)[grepl("user_id$", names(rows))]) rows[[field]][] <- NA_integer_
    rows
  }
  payload$entry_tbl <- clear_users(payload$entry_tbl)
  payload$scoped <- lapply(payload$scoped, clear_users)
  payload$user_tbl <- payload$user_tbl[FALSE, , drop = FALSE]
  ids <- transfer_ids_from_columns(c(list(payload$entry_tbl), payload$scoped, list(payload$keyword_thesaurus_tbl)), "person_id$", "reviewer_id")
  payload$persons_tbl <- payload$persons_tbl[payload$persons_tbl$id %in% ids, , drop = FALSE]
  org_ids <- transfer_ids_from_columns(c(list(payload$entry_tbl), payload$scoped,
    list(payload$persons_tbl, payload$keyword_thesaurus_tbl)), "organisation_id$")
  repeat {
    parents <- payload$organisation_tbl$parent_organisation_id[payload$organisation_tbl$id %in% org_ids]
    parents <- setdiff(stats::na.omit(parents), org_ids)
    if (!length(parents)) break
    org_ids <- c(org_ids, parents)
  }
  payload$organisation_tbl <- payload$organisation_tbl[payload$organisation_tbl$id %in% org_ids, , drop = FALSE]
  payload$party_address_tbl <- payload$party_address_tbl[
    payload$party_address_tbl$person_id %in% ids | (!is.na(payload$party_address_tbl$organisation_id) &
      payload$party_address_tbl$organisation_id %in% payload$organisation_tbl$id), , drop = FALSE]
  for (field in intersect(names(payload$manifest), c("source_created_by_user_id", "source_exported_by_user_id")))
    payload$manifest[[field]] <- NA_integer_
  for (field in intersect(names(payload$manifest), c("source_created_by_user_name", "source_exported_by_user_name")))
    payload$manifest[[field]] <- NA_character_
  payload
}

# Only ordinary explicitly selected files can seed a draft. Prepared metadata,
# hidden application files and transport archives must be prepared afresh.
edition_selectable <- function(paths) {
  !grepl("(^|/)\\.", paths) & !tolower(basename(paths)) %in% c("metadata.pdf", "metadata.qmd", "citation.txt") &
    !tolower(tools::file_ext(paths)) %in% c("zip", "7z", "gz", "tar", "sqlite", "db")
}

#' @export
#' @noRd
public_draft_files <- function(release_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  source <- edition_source(conn, release_id, base_path)
  rows <- jsonlite::read_json(source$paths$source_manifest, simplifyVector = TRUE)
  rows[edition_selectable(rows$path), , drop = FALSE]
}

edition_read_table <- function(path) {
  sep <- if (tolower(tools::file_ext(path)) == "tsv") "\t" else ","
  # Preserve identifiers, leading zeroes, literal NA strings and empty values.
  utils::read.table(path, header = TRUE, sep = sep, quote = '"', comment.char = "",
    colClasses = "character", check.names = FALSE, na.strings = NULL, stringsAsFactors = FALSE)
}

edition_file_fields <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "tsv")) return(list(table = names(edition_read_table(path))))
  if (ext %in% c("gpkg", "geojson")) {
    layers <- sf::st_layers(path)$name
    return(stats::setNames(lapply(layers, function(layer) {
      data <- sf::st_read(path, layer = layer, quiet = TRUE)
      setdiff(names(data), attr(data, "sf_column"))
    }), layers))
  }
  list()
}

#' @export
#' @noRd
public_draft_fields <- function(release_id, base_path, files) {
  available <- public_draft_files(release_id, base_path)
  if (!length(files) || anyNA(files) || any(!files %in% available$path)) stop("Select files from the frozen release.")
  stage <- tempfile("edition-fields-"); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  verify_dataset_release(release_id, base_path, stage)
  stats::setNames(lapply(files, function(path) edition_file_fields(file.path(stage, "dataset", path))), files)
}

edition_project_file <- function(source, target, fields) {
  available <- edition_file_fields(source)
  if (is.null(fields)) {
    if (!file.copy(source, target, copy.date = TRUE)) stop("Could not copy selected file.")
    return(invisible(NULL))
  }
  if (!length(available) || !is.list(fields) || !length(fields) || is.null(names(fields)) ||
      anyDuplicated(names(fields)) || any(!names(fields) %in% names(available)))
    stop("Attribute selection must name supported tables/layers (CSV, TSV, GeoPackage or GeoJSON).")
  for (layer in names(fields)) {
    chosen <- fields[[layer]]
    if (anyDuplicated(available[[layer]]) || !is.character(chosen) || anyNA(chosen) || anyDuplicated(chosen) || any(!chosen %in% available[[layer]]))
      stop("Selected attributes no longer match the frozen file.")
  }
  ext <- tolower(tools::file_ext(source))
  if (ext %in% c("csv", "tsv")) {
    if (!length(fields$table)) stop("Select at least one column for a text table.")
    data <- edition_read_table(source)[, fields$table, drop = FALSE]
    utils::write.table(data, target, sep = if (ext == "tsv") "\t" else ",", row.names = FALSE, na = "", qmethod = "double")
  } else {
    for (layer in names(fields)) {
      data <- sf::st_read(source, layer = layer, quiet = TRUE)
      # sf retains the geometry when subsetting attribute columns.
      data <- data[, fields[[layer]], drop = FALSE]
      sf::st_write(data, target, layer = layer, quiet = TRUE, append = FALSE)
    }
  }
  invisible(NULL)
}

# Internal atomic placement after public metadata has been prepared.
edition_create_raw <- function(release_id, base_path, user_id, title, version,
                               files, fields = list(), preparation_method, .payload = NULL) {
  if (!metadata_present(title) || !grepl("^[0-9]+[.][0-9]+[.][0-9]+$", version))
    stop("Supply a public title and a major.minor.patch version.")
  if (!metadata_present(preparation_method)) stop("Describe the public preparation method.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_edition_schema(conn)
  source <- edition_source(conn, release_id, base_path)
  origin <- source$paths$record; source_id <- origin$dataset_id[1]
  lock <- release_lock(base_path, source_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_require_user(conn, user_id); release_open_review(conn, source_id)
  available <- public_draft_files(release_id, base_path)
  if (!is.character(files) || !length(files) || anyNA(files) || anyDuplicated(files) || any(!files %in% available$path))
    stop("Select at least one eligible file from the frozen release.")
  if (!is.list(fields) || (length(fields) && (is.null(names(fields)) || anyDuplicated(names(fields)) || any(!names(fields) %in% files))))
    stop("Attribute selections must refer to selected files.")
  id <- uuid::UUIDgenerate()
  directory <- release_safe_directory(base_path, file.path(base_path, ".metadata_pond", "edition-drafts", id), TRUE)
  target <- release_safe_directory(base_path, file.path(base_path, "dev", paste0("public_", substr(id, 1, 8), "__", id)))
  if (file.exists(target)) stop("The public draft destination already exists.")
  op <- list(dataset_id = source_id, source_release_id = release_id, uuid = id, target = target, directory = directory, phase = "prepared")
  journal <- file.path(directory, "edition-operation.json")
  release_operation_write(op, journal)
  complete <- FALSE
  on.exit(if (!complete) {
    # Keep a completed-but-unregistered copy for inspection, never claim it as a dataset.
    if (dir.exists(target)) file.rename(target, file.path(directory, "unregistered-draft"))
    unlink(file.path(directory, "source"), recursive = TRUE)
    op$phase <- "rolled_back"; release_operation_write(op, journal)
  }, add = TRUE)
  extracted <- file.path(directory, "source")
  verify_dataset_release(release_id, base_path, extracted)
  draft <- file.path(directory, "draft"); dir.create(draft)
  for (path in files) {
    output <- file.path(draft, path); dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    edition_project_file(file.path(extracted, "dataset", path), output, fields[[path]])
  }
  snapshot <- release_read_payload(source$paths$snapshot)
  data_relative <- substring(snapshot$entry_tbl$data_path[1], nchar(snapshot$entry_tbl$dataset_path[1]) + 2L)
  if (is.na(data_relative) || !nzchar(data_relative)) data_relative <- "data"
  if (identical(snapshot$entry_tbl$data_path[1], snapshot$entry_tbl$dataset_path[1])) data_relative <- ""
  if (nzchar(data_relative)) consumer_safe_names(data_relative)
  dir.create(file.path(draft, data_relative), recursive = TRUE, showWarnings = FALSE)
  pond_id <- ensure_data_pond_id(base_path, conn = conn)
  write_dataset_marker(draft, id, pond_id)
  selection <- as.character(jsonlite::toJSON(list(files = files, fields = fields), auto_unbox = FALSE))
  DBI::dbWithTransaction(conn, {
    release_require_user(conn, user_id)
    # Recheck the source under the writer transaction, including concurrent removal.
    edition_source(conn, release_id, base_path)
    new_id <- transfer_insert_row(conn, "entry_tbl", list(uuid = id, metadata_uuid = uuid::UUIDgenerate(),
      title = trimws(title), version = version, folder_key = paste0("public_", substr(id, 1, 8)),
      dataset_path = target, data_path = if (nzchar(data_relative)) file.path(target, data_relative) else target,
      data_pond_id = pond_id, lifecycle_status = "draft", folder_locked = 0L, created_by_user_id = as.integer(user_id)))
    transfer_insert_row(conn, "dataset_edition_tbl", list(dataset_id = new_id,
      family_uuid = source$edition$family_uuid[1], edition = "public", source_dataset_uuid = origin$dataset_uuid[1],
      source_release_id = release_id, source_version = origin$version[1], source_zip_sha256 = origin$zip_sha256[1],
      preparation_method = trimws(preparation_method), selection_json = selection))
    transfer_insert_row(conn, "dataset_lineage_tbl", list(dataset_id = new_id, lineage_source_mode = "form",
      lineage_statement = paste("Derived from in-house dataset", origin$dataset_uuid[1], "version", origin$version[1],
        "release", release_id, ".", preparation_method)))
    if (!is.null(.payload)) {
      .payload$entry_tbl$folder_key <- paste0("public_", substr(id, 1, 8))
      release_restore_metadata(conn, .payload, release_entry(conn, new_id), target)
    }
    if (!file.rename(draft, target)) stop("Could not place the public draft in dev/.")
    release_event(conn, new_id, release_id, id, "create_public_draft", user_id, preparation_method)
    release_before_commit()
  })
  complete <- TRUE
  op$phase <- "complete"; release_operation_write(op, journal)
  unlink(extracted, recursive = TRUE)
  list(dataset_id = new_id, dataset_uuid = id, dataset_path = target, source_release_id = release_id)
}

edition_pending_drafts <- function(pond, dataset_id) {
  journals <- release_journals(file.path(pond, ".metadata_pond", "edition-drafts"), "edition-operation.json")
  for (path in journals) {
    op <- jsonlite::read_json(path)
    if (op$dataset_id == dataset_id && !op$phase %in% c("complete", "rolled_back"))
      stop("A public draft operation needs recovery before another lifecycle operation.")
  }
}

edition_recover_drafts <- function(conn, pond, dataset_id) {
  messages <- character()
  for (path in release_journals(file.path(pond, ".metadata_pond", "edition-drafts"), "edition-operation.json")) {
    op <- jsonlite::read_json(path)
    if (op$dataset_id != dataset_id || op$phase %in% c("complete", "rolled_back")) next
    directory <- release_safe_directory(pond, dirname(path))
    target <- release_safe_directory(pond, op$target)
    expected <- file.path(normalizePath(pond), "dev", paste0("public_", substr(op$uuid, 1, 8), "__", op$uuid))
    if (!identical(target, expected) || !identical(directory, op$directory)) stop("Invalid public draft recovery paths.")
    rows <- DBI::dbGetQuery(conn, "SELECT id,dataset_path FROM entry_tbl WHERE uuid=?", params = list(op$uuid))
    if (nrow(rows)) {
      if (rows$dataset_path[1] != target || !dir.exists(target)) stop("The public draft moved; inspect it before recovery.")
      op$phase <- "complete"
    } else {
      if (dir.exists(target) && !file.rename(target, file.path(directory, "unregistered-draft")))
        stop("Could not preserve the unregistered public draft.")
      op$phase <- "rolled_back"
    }
    unlink(file.path(directory, "source"), recursive = TRUE)
    release_operation_write(op, path)
    messages <- c(messages, paste("Public draft:", op$phase))
  }
  messages
}

#' @export
#' @noRd
record_dataset_publication <- function(release_id, base_path, user_id,
                                       action = c("published", "withdrawn"), location, note) {
  action <- match.arg(action)
  if (!metadata_present(note) || !metadata_present(location)) stop("Record the publication location and reason.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_edition_schema(conn)
  row <- release_row(conn, release_id)
  lock <- release_lock(base_path, row$dataset_id[1]); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  DBI::dbWithTransaction(conn, {
    release_require_user(conn, user_id)
    source <- verify_dataset_release(release_id, base_path)
    edition <- edition_descriptor(release_read_payload(source$snapshot))
    if (is.null(edition) || edition$edition != "public") stop("Only a release frozen as a public edition can be recorded as published.")
    if (action == "withdrawn") {
      latest <- edition_current_publication(conn, row$dataset_id[1])
      if (!nrow(latest) || latest$release_id[1] != release_id) stop("Select the currently published release to record withdrawal.")
    }
    transfer_insert_row(conn, "dataset_publication_tbl", list(dataset_id = row$dataset_id[1], release_id = release_id,
      action = action, location = trimws(location), note = trimws(note), happened_at = release_now(), user_id = as.integer(user_id)))
  })
  invisible(TRUE)
}

edition_current_publication <- function(conn, dataset_id) {
  if (!DBI::dbExistsTable(conn, "dataset_publication_tbl")) return(data.frame())
  row <- DBI::dbGetQuery(conn, "SELECT p.*,r.version FROM dataset_publication_tbl p
    JOIN dataset_release_tbl r ON r.release_id=p.release_id WHERE p.dataset_id=? ORDER BY p.id DESC LIMIT 1",
    params = list(as.integer(dataset_id)))
  row[row$action == "published", , drop = FALSE]
}

edition_protect_release <- function(conn, release_id) {
  if (!DBI::dbExistsTable(conn, "dataset_publication_tbl")) return(invisible(NULL))
  rows <- DBI::dbGetQuery(conn, "SELECT p.* FROM dataset_publication_tbl p WHERE p.id IN
    (SELECT MAX(id) FROM dataset_publication_tbl GROUP BY dataset_id) AND p.action='published' AND p.release_id=?",
    params = list(release_id))
  if (nrow(rows)) stop("This is the currently published public release. Record its replacement or withdrawal before removal.")
}

#' @export
#' @noRd
dataset_edition_family <- function(dataset_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  ensure_dataset_edition_schema(conn); release_ensure_schema(conn)
  edition <- edition_row(conn, dataset_id)
  if (!nrow(edition)) return(data.frame())
  rows <- DBI::dbGetQuery(conn, "SELECT e.dataset_id,d.title,d.uuid,e.edition,e.family_uuid,e.source_version,e.source_release_id
    FROM dataset_edition_tbl e JOIN entry_tbl d ON d.id=e.dataset_id WHERE e.family_uuid=? ORDER BY e.edition,e.dataset_id",
    params = list(edition$family_uuid[1]))
  rows$latest_frozen <- rows$published_version <- rows$publication_location <- ""
  for (i in seq_len(nrow(rows))) {
    frozen <- DBI::dbGetQuery(conn, "SELECT version FROM dataset_release_tbl WHERE dataset_id=? AND state='frozen' ORDER BY frozen_at DESC,rowid DESC LIMIT 1",
      params = list(rows$dataset_id[i]))
    if (nrow(frozen)) rows$latest_frozen[i] <- frozen$version[1]
    published <- edition_current_publication(conn, rows$dataset_id[i])
    if (nrow(published)) {
      rows$published_version[i] <- published$version[1]
      rows$publication_location[i] <- published$location[1]
    }
  }
  rows
}
