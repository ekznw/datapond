# Immutable release store. Catalogue records and operation journals are separate
# from working metadata; ordinary transfers must never copy these local controls.
release_control_tables <- function() c("dataset_release_tbl", "dataset_release_event_tbl")

release_ensure_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_release_tbl (
    release_id TEXT PRIMARY KEY, dataset_id INTEGER NOT NULL, dataset_uuid TEXT NOT NULL,
    version TEXT NOT NULL, frozen_at TEXT NOT NULL, frozen_by_user_id INTEGER NOT NULL,
    notes TEXT NOT NULL, relative_path TEXT NOT NULL UNIQUE, zip_sha256 TEXT NOT NULL,
    manifest_sha256 TEXT NOT NULL, snapshot_sha256 TEXT NOT NULL, baseline_hash TEXT NOT NULL,
    source_manifest_sha256 TEXT NOT NULL, state TEXT NOT NULL DEFAULT 'frozen',
    UNIQUE(dataset_id, version))")
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_release_event_tbl (
    id INTEGER PRIMARY KEY, dataset_id INTEGER NOT NULL, release_id TEXT NOT NULL,
    operation_id TEXT NOT NULL UNIQUE, action TEXT NOT NULL, happened_at TEXT NOT NULL,
    user_id INTEGER NOT NULL, note TEXT NOT NULL)")
}

release_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
release_sha <- function(path) digest::digest(file = path, algo = "sha256")
release_json <- function(value, path) jsonlite::write_json(value, path,
  auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null", digits = NA)
release_private <- function(pond, ...) file.path(pond, ".metadata_pond", "releases", ...)
release_store <- function(pond) file.path(pond, "arc", "releases")
release_journals <- function(parent, filename) {
  paths <- file.path(list.dirs(parent, recursive = FALSE, full.names = TRUE), filename)
  paths[file.exists(paths) & !dir.exists(paths)]
}
release_archive_tool <- function(action, source, manifest, output = NULL) {
  python <- document_python_path()
  if (!nzchar(python)) stop("Release packaging requires Python 3 (standard library only).")
  script <- system.file("release", "archive.py", package = "datapond", mustWork = TRUE)
  args <- c(script, action, source, manifest, if (!is.null(output)) c("--output", output))
  result <- processx::run(python, args, error_on_status = FALSE, timeout = Inf)
  if (result$status != 0L) stop(paste("Release file verification failed:", result$stderr), call. = FALSE)
  invisible(output)
}

release_inventory <- function(path) {
  file <- tempfile(fileext = ".json"); on.exit(unlink(file))
  release_archive_tool("inventory", path, file)
  jsonlite::read_json(file, simplifyVector = FALSE)
}

release_safe_directory <- function(pond, path, create = FALSE) {
  pond <- normalizePath(pond, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!startsWith(path, paste0(pond, "/"))) stop("Release path is outside this pond.")
  # Resolve existing ancestors before creating anything, including symlinked parents.
  parent <- path
  while (!file.exists(parent) && parent != dirname(parent)) parent <- dirname(parent)
  resolved <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  if (resolved != pond && !startsWith(resolved, paste0(pond, "/"))) stop("Release path escapes this pond.")
  if (create) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

release_lock <- function(pond, dataset_id, recovery = FALSE) {
  directory <- release_safe_directory(pond, release_private(pond, "locks"), TRUE)
  path <- file.path(directory, as.character(as.integer(dataset_id)))
  if (!dir.create(path, showWarnings = FALSE)) stop("A lifecycle operation is active or interrupted. Use Recover interrupted operation first.")
  acquired <- FALSE
  on.exit(if (!acquired) unlink(path, recursive = TRUE))
  release_json(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]],
    started = as.numeric(ps::ps_create_time(ps::ps_handle()))), file.path(path, "owner.json"))
  if (!recovery) {
    pending <- release_journals(release_private(pond, "operations"), "operation.json")
    for (file in pending) {
      op <- jsonlite::read_json(file)
      if (op$dataset_id == dataset_id && !op$phase %in% c("complete", "rolled_back")) {
        unlink(path, recursive = TRUE)
        stop("A lifecycle operation needs recovery before another operation can start.")
      }
    }
    freezes <- release_journals(release_private(pond), "freeze-operation.json")
    conn <- get_conn(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
    release_ensure_schema(conn)
    for (file in freezes) {
      job <- jsonlite::read_json(file)
      if (job$dataset_id == dataset_id && dir.exists(job$destination) &&
          !nrow(DBI::dbGetQuery(conn, "SELECT release_id FROM dataset_release_tbl WHERE release_id=?", params = list(job$release_id))))
        stop("An interrupted freeze needs recovery before another operation can start.")
    }
  }
  acquired <- TRUE
  path
}

release_require_user <- function(conn, user_id) review_require_user(conn, user_id)
release_entry <- function(conn, dataset_id) {
  rows <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id=?", params = list(as.integer(dataset_id)))
  if (nrow(rows) != 1L) stop("Select a saved dataset.")
  rows
}

release_payload <- function(conn, dataset_id, pond, user_id) {
  payload <- collect_dataset_transfer_payload(dataset_id, pond, exporting_user_id = NULL, conn = conn)
  payload$scoped <- payload$scoped[setdiff(names(payload$scoped), release_control_tables())]
  payload
}

release_open_review <- function(conn, dataset_id) {
  if (DBI::dbExistsTable(conn, "dataset_review_exchange_tbl") && nrow(DBI::dbGetQuery(conn,
      "SELECT review_id FROM dataset_review_exchange_tbl WHERE dataset_id=? AND status='issued'",
      params = list(as.integer(dataset_id))))) stop("Complete or cancel the active metadata review before this lifecycle operation.")
}

release_working_root <- function(pond, record) {
  root <- metadata_text(record$dataset_path, "")
  if (!nzchar(root) || !dir.exists(root)) stop("The working dataset folder is unavailable.")
  root <- release_safe_directory(pond, root)
  if (!dataset_lifecycle_stage(root, pond) %in% c("src", "dev"))
    stop("Freeze an editable dataset in src/ or dev/. Restore an archived release as a draft first.")
  if (root %in% file.path(normalizePath(pond), c("src", "dev"))) stop("Select a dataset folder, not a lifecycle root.")
  if (metadata_present(record$data_path)) {
    data_root <- normalizePath(record$data_path[1], winslash = "/", mustWork = FALSE)
    if (data_root != root && !startsWith(data_root, paste0(root, "/")))
      stop("The data folder must be inside the dataset root before freezing.")
  }
  if (metadata_text(record$lifecycle_status, "draft") != "draft") stop("Only a draft working dataset can be frozen.")
  root
}

#' @export
#' @noRd
preview_dataset_release <- function(dataset_id, base_path, user_id) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  release_ensure_schema(conn); release_require_user(conn, user_id)
  record <- release_entry(conn, dataset_id)
  root <- release_working_root(base_path, record)
  for (field in c("title", "uuid", "version", "citation"))
    if (!metadata_present(record[[field]])) stop(paste("Save the dataset", field, "before freezing a release."))
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]+$", record$uuid[1])) stop("Dataset UUID is not a portable identifier.")
  files <- release_inventory(root)
  if (!length(files)) stop("The working dataset folder contains no files.")
  documents <- metadata_document_ready(dataset_id, base_path, root)
  payload <- release_payload(conn, dataset_id, base_path, user_id)
  list(dataset_id = as.integer(dataset_id), dataset_uuid = record$uuid[1], version = record$version[1],
    source = root, files = files, documents = documents, file_count = length(files),
    bytes = sum(vapply(files, function(x) x$size, numeric(1))),
    metadata_hash = review_fingerprint(payload), files_hash = review_hash(files))
}

#' @export
#' @noRd
dataset_release_history <- function(dataset_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  release_ensure_schema(conn)
  DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_tbl WHERE dataset_id=? ORDER BY frozen_at DESC, release_id",
    params = list(as.integer(dataset_id)))
}

release_row <- function(conn, release_id) {
  release_ensure_schema(conn)
  row <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_tbl WHERE release_id=?", params = list(release_id))
  if (nrow(row) != 1L) stop("Release is not registered in this pond.")
  row
}

release_paths <- function(pond, row) {
  root <- release_safe_directory(pond, file.path(pond, row$relative_path[1]))
  private <- release_safe_directory(pond, release_private(pond, row$release_id[1]))
  list(root = root, zip = file.path(root, "dataset.zip"), manifest = file.path(root, "files.json"),
    snapshot = file.path(private, "snapshot.sqlite"), source_manifest = file.path(private, "source-files.json"))
}

#' @export
#' @noRd
verify_dataset_release <- function(release_id, base_path, extract_to = NULL) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  row <- release_row(conn, release_id); paths <- release_paths(base_path, row)
  expected <- c(zip = row$zip_sha256[1], manifest = row$manifest_sha256[1], snapshot = row$snapshot_sha256[1],
    source_manifest = row$source_manifest_sha256[1])
  for (name in names(expected)) if (!file.exists(paths[[name]]) || !identical(release_sha(paths[[name]]), expected[[name]]))
    stop(paste("Release integrity check failed:", name))
  public <- jsonlite::read_json(file.path(paths$root, "release.json"))
  for (field in c("release_id", "dataset_uuid", "version", "frozen_at", "notes", "zip_sha256", "manifest_sha256", "state"))
    if (!identical(as.character(public[[field]]), as.character(row[[field]][1]))) stop("Release descriptor differs from the registered release.")
  release_archive_tool(if (is.null(extract_to)) "verify" else "extract", paths$zip, paths$manifest, extract_to)
  invisible(c(paths, list(record = row)))
}

release_read_payload <- function(path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(conn))
  read <- function(t) if (DBI::dbExistsTable(conn, t)) DBI::dbReadTable(conn, t) else data.frame()
  refs <- c("organisation_tbl", "persons_tbl", "user_tbl", "license_template_tbl", "party_address_tbl", "keyword_thesaurus_tbl")
  scoped <- setdiff(DBI::dbListTables(conn), c("bundle_manifest_tbl", "entry_tbl", refs))
  c(list(manifest = read("bundle_manifest_tbl"), entry_tbl = read("entry_tbl"),
    scoped = stats::setNames(lapply(scoped, read), scoped)), stats::setNames(lapply(refs, read), refs))
}

release_copy_tree <- function(source, destination, files) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  for (row in files) {
    dest <- file.path(destination, row$path)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(source, row$path), dest, overwrite = FALSE, copy.date = TRUE) ||
        !identical(release_sha(dest), row$sha256)) stop(paste("Source changed or copy failed:", row$path))
  }
}

release_write_documents <- function(context, dataset_id, package, id, frozen_at, notes, iso_policy) {
  snapshot <- collect_metadata_document(dataset_id, context)
  # Human-readable documents were explicitly prepared and reviewed beforehand.
  # Release-specific identity belongs in the sidecars, never a regenerated PDF.
  docs <- snapshot$development_documents
  readme <- c(metadata_text(snapshot$record$title), "", paste("Release:", id),
    paste("Version:", snapshot$record$version), paste("Frozen:", frozen_at), "", notes, "",
    "Start with metadata.pdf. Use citation.txt for citation and LICENSE.txt for recorded terms.",
    "The PDF and citation are the prepared working copies, preserved without regeneration.",
    "The original dataset folder is under dataset/, with its relative paths preserved.",
    "Data-file inventory paths in metadata.pdf are relative to the dataset's data folder.",
    "Development-document paths are relative to the dataset root.", "",
    "Development and lineage documents", if (length(docs)) paste0("dataset/", docs) else "No root QMD/Rmd documents were found.",
    "These documents record methods and the processing environment; they have not been executed during release creation.",
    "", "A frozen release is not automatically approved or published for external distribution.")
  writeLines(readme, file.path(package, "README.txt"), useBytes = TRUE)
  rights <- snapshot$dataset_license_tbl
  terms <- c(metadata_text(snapshot$record$access_constraints, ""), metadata_text(snapshot$record$use_limitations, ""))
  for (i in seq_len(nrow(rights))) for (field in c("license_name", "license_identifier", "license_description", "license_url",
      "license_holder", "use_constraints", "redistribution_permission_code", "attribution_text", "embargo_status",
      "embargo_end_date", "release_statement", "dissemination_constraint_code")) {
    value <- metadata_text(rights[[field]][i], "")
    if (nzchar(value)) terms <- c(terms, paste(gsub("_", " ", field), value, sep = ": "))
  }
  writeLines(c("Recorded rights and use conditions", "", terms[nzchar(terms)], "",
    "Freezing this package does not grant distribution permission."), file.path(package, "LICENSE.txt"), useBytes = TRUE)
  release_json(list(format = "datapond-release-metadata", version = 1L, dataset_uuid = snapshot$record$uuid[1],
    dataset_version = snapshot$record$version[1], release_id = id, frozen_at = frozen_at,
    title = snapshot$record$title[1], citation = metadata_text(snapshot$record$citation),
    description = metadata_text(snapshot$record$abstract), purpose = metadata_text(snapshot$record$purpose),
    development_documents = docs, iso_policy = iso_policy), file.path(package, "metadata.json"))
  if (iso_policy != "none") {
    paths <- write_iso_wave3_documents(dataset_id, context, package, validation_policy = iso_policy)
    if (!length(paths)) stop("Requested ISO package was not generated.")
  }
  unlink(file.path(package, c(".quarto", "metadata_files")), recursive = TRUE)
}

release_event <- function(conn, dataset_id, release_id, op, action, user_id, note) {
  DBI::dbExecute(conn, "INSERT INTO dataset_release_event_tbl
    (dataset_id,release_id,operation_id,action,happened_at,user_id,note) VALUES (?,?,?,?,?,?,?)",
    params = list(as.integer(dataset_id), release_id, op, action, release_now(), as.integer(user_id), note))
}

#' @export
#' @noRd
create_dataset_release <- function(dataset_id, base_path, user_id, notes, preview,
                                   close_working = FALSE, iso_policy = c("none", "pinned", "datapond")) {
  iso_policy <- match.arg(iso_policy)
  if (!metadata_present(notes)) stop("Record release notes before freezing.")
  lock <- release_lock(base_path, dataset_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_ensure_schema(conn); release_require_user(conn, user_id); release_open_review(conn, dataset_id)
  current <- preview_dataset_release(dataset_id, base_path, user_id)
  if (!identical(current, preview)) stop("Dataset changed since preview. Preview the release again.")
  if (nrow(DBI::dbGetQuery(conn, "SELECT release_id FROM dataset_release_tbl WHERE dataset_id=? AND version=?",
      params = list(as.integer(dataset_id), current$version)))) stop("This version is already frozen. Save a new working version first.")
  id <- uuid::UUIDgenerate(); frozen_at <- release_now()
  private <- release_safe_directory(base_path, release_private(base_path, id), TRUE)
  registered <- FALSE
  on.exit(if (!registered && !file.exists(file.path(release_store(base_path), id))) unlink(private, recursive = TRUE), add = TRUE)
  stage <- release_safe_directory(base_path, file.path(release_store(base_path), paste0(".stage-", id)), TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  package <- file.path(stage, "package"); dir.create(package)
  # The backup API captures one committed database generation, including WAL.
  context <- file.path(private, "context"); dir.create(context)
  to <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(context))
  tryCatch(RSQLite::sqliteCopyDatabase(conn, to), finally = DBI::dbDisconnect(to))
  captured <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(context))
  payload <- tryCatch(release_payload(captured, dataset_id, base_path, user_id), finally = DBI::dbDisconnect(captured))
  if (!identical(review_fingerprint(payload), current$metadata_hash)) stop("Metadata changed during snapshot capture.")
  write_dataset_transfer_database(payload, file.path(private, "snapshot.sqlite"))
  release_json(current$files, file.path(private, "source-files.json"))
  release_archive_tool("space", stage, file.path(private, "source-files.json"))
  release_copy_tree(current$source, file.path(package, "dataset"), current$files)
  release_copy_tree(current$documents$directory, package, current$documents$files)
  captured <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(context))
  # Exporters must resolve documents against the staged dataset, never live data.
  staged_entry <- transfer_rewrite_row_paths(payload$entry_tbl, current$source, file.path(package, "dataset"))
  tryCatch(DBI::dbExecute(captured, "UPDATE entry_tbl SET dataset_path=?,data_path=? WHERE id=?",
    params = list(file.path(package, "dataset"), staged_entry$data_path[1], as.integer(dataset_id))),
    finally = DBI::dbDisconnect(captured))
  release_write_documents(context, dataset_id, package, id, frozen_at, notes, iso_policy)
  files <- release_inventory(package)
  release_json(files, file.path(stage, "files.json"))
  release_archive_tool("pack", package, file.path(stage, "files.json"), file.path(stage, "dataset.zip"))
  row <- data.frame(release_id = id, dataset_id = as.integer(dataset_id), dataset_uuid = current$dataset_uuid,
    version = current$version, frozen_at = frozen_at, frozen_by_user_id = as.integer(user_id), notes = notes,
    relative_path = paste0("arc/releases/", id), zip_sha256 = release_sha(file.path(stage, "dataset.zip")),
    manifest_sha256 = release_sha(file.path(stage, "files.json")), snapshot_sha256 = release_sha(file.path(private, "snapshot.sqlite")),
    baseline_hash = current$metadata_hash, source_manifest_sha256 = release_sha(file.path(private, "source-files.json")), state = "frozen")
  release_json(as.list(row[1, c("release_id", "dataset_uuid", "version", "frozen_at", "notes", "zip_sha256", "manifest_sha256", "state")]),
    file.path(stage, "release.json"))
  writeLines(paste(row$zip_sha256, " dataset.zip"), file.path(stage, "checksums.sha256"))
  unlink(package, recursive = TRUE); unlink(context, recursive = TRUE)
  destination <- file.path(base_path, row$relative_path)
  # A prepared journal makes the rename/DB commit gap recoverable after a crash.
  release_json(list(kind = "freeze", dataset_id = dataset_id, release_id = id, row = as.list(row),
    stage = stage, destination = destination), file.path(private, "freeze-operation.json"))
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, "UPDATE entry_tbl SET id=id WHERE id=?", params = list(as.integer(dataset_id)))
    release_require_user(conn, user_id)
    if (!identical(review_fingerprint(release_payload(conn, dataset_id, base_path, user_id)), current$metadata_hash) ||
        !identical(review_hash(release_inventory(current$source)), current$files_hash)) stop("Working dataset changed while freezing; release cancelled.")
    if (file.exists(destination) || !file.rename(stage, destination)) stop("Could not finalise the frozen release.")
    DBI::dbAppendTable(conn, "dataset_release_tbl", row)
    release_event(conn, dataset_id, id, id, "freeze", user_id, notes)
  })
  registered <- TRUE
  if (close_working) release_close_working(conn, base_path, row, user_id, notes)
  row
}

release_operation_write <- function(op, path) {
  temporary <- paste0(path, ".tmp")
  release_json(op, temporary)
  if (!file.rename(temporary, path)) stop("Could not persist the lifecycle operation journal.")
}

release_operation <- function(pond, row, kind, old_path, new_path, user_id, note) {
  id <- uuid::UUIDgenerate()
  root <- release_safe_directory(pond, release_private(pond, "operations", id), TRUE)
  op <- list(operation_id = id, kind = kind, dataset_id = row$dataset_id[1], release_id = row$release_id[1],
    old_path = old_path, new_path = new_path, backup = file.path(root, "prior-working"),
    directory = root, user_id = user_id, note = note, phase = "prepared")
  release_operation_write(op, file.path(root, "operation.json"))
  op
}

release_after_switch <- function() invisible(NULL) # failure-injection boundary
release_before_commit <- function() invisible(NULL)

release_cleanup_operation <- function(op, pond) {
  path <- file.path(op$directory, "operation.json")
  if (dir.exists(op$backup)) {
    if (op$kind == "close") {
      frozen <- verify_dataset_release(op$release_id, pond)
      if (!identical(release_sha(file.path(op$directory, "prior-files.json")), frozen$record$source_manifest_sha256[1]))
        stop("The retirement manifest differs from the frozen release; working files have been retained.")
    } else {
      release_verify_checkpoint(op)
      release_archive_tool("verify", file.path(op$directory, "prior-working.zip"), file.path(op$directory, "prior-files.json"))
    }
    manifest <- jsonlite::read_json(file.path(op$directory, "prior-files.json"))
    if (!identical(review_hash(release_inventory(op$backup)), review_hash(manifest)))
      stop("The retired working folder changed. It has been preserved; inspect it before recovery cleanup.")
    # The archive/checkpoint has been checked before any working bytes are removed.
    unlink(op$backup, recursive = TRUE)
    if (dir.exists(op$backup)) stop("Could not remove the verified retired working folder; recovery can retry.")
  }
  op$phase <- "complete"; release_operation_write(op, path)
  invisible(op)
}

release_rollback_files <- function(op) {
  # On a DB rollback, preserve the uncommitted restored copy for diagnosis and
  # put the prior working directory back. Never delete unrecognised user files.
  if (op$kind == "restore" && dir.exists(op$new_path)) {
    if (!file.rename(op$new_path, file.path(op$directory, "uncommitted-restored")))
      stop("Could not preserve the uncommitted restore. Use lifecycle recovery.")
  }
  if (dir.exists(op$backup)) {
    if (file.exists(op$old_path) || !file.rename(op$backup, op$old_path))
      stop("Could not reinstate the previous working folder. Use lifecycle recovery.")
  }
  op$phase <- "rolled_back"
  release_operation_write(op, file.path(op$directory, "operation.json"))
  invisible(op)
}

release_close_working <- function(conn, pond, row, user_id, note) {
  paths <- verify_dataset_release(row$release_id[1], pond)
  record <- release_entry(conn, row$dataset_id[1]); root <- release_working_root(pond, record)
  snapshot <- release_read_payload(paths$snapshot)
  source_files <- jsonlite::read_json(paths$source_manifest)
  if (!identical(review_fingerprint(release_payload(conn, row$dataset_id[1], pond, user_id)), row$baseline_hash[1]) ||
      !identical(review_hash(release_inventory(root)), review_hash(source_files)))
    stop("The working dataset differs from this release. Freeze the current version before closing development.")
  op <- release_operation(pond, row, "close", root, paths$root, user_id, note)
  release_json(source_files, file.path(op$directory, "prior-files.json"))
  committed <- FALSE
  tryCatch({
    DBI::dbWithTransaction(conn, {
      DBI::dbExecute(conn, "UPDATE entry_tbl SET id=id WHERE id=?", params = list(row$dataset_id[1]))
      if (!identical(review_fingerprint(release_payload(conn, row$dataset_id[1], pond, user_id)), row$baseline_hash[1])) stop("Metadata changed before archive closure.")
      if (!file.rename(root, op$backup)) stop("Could not retire the working directory; it remains in place.")
      release_after_switch()
      if (!identical(review_hash(release_inventory(op$backup)), review_hash(source_files))) stop("Files changed before archive closure.")
      DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path=?,data_path=?,lifecycle_status='archived',folder_locked=1,
        archived_at=?,archived_by_person_id=(SELECT person_id FROM user_tbl WHERE id=?) WHERE id=?",
        params = list(paths$root, file.path(paths$root, "data"), release_now(), as.integer(user_id), row$dataset_id[1]))
      release_event(conn, row$dataset_id[1], row$release_id[1], op$operation_id, "archive_close", user_id, note)
      release_before_commit()
    })
    committed <- TRUE
    op$phase <- "committed"; release_operation_write(op, file.path(op$directory, "operation.json"))
    release_cleanup_operation(op, pond)
  }, error = function(e) {
    if (!committed) release_rollback_files(op)
    stop(conditionMessage(e), call. = FALSE)
  })
  invisible(row)
}

#' @export
#' @noRd
archive_dataset_release <- function(release_id, base_path, user_id, note) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  row <- release_row(conn, release_id)
  lock <- release_lock(base_path, row$dataset_id[1]); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_require_user(conn, user_id); release_open_review(conn, row$dataset_id[1])
  if (!metadata_present(note)) stop("Record why development is being closed.")
  release_close_working(conn, base_path, row, user_id, note)
}

release_restore_metadata <- function(conn, snapshot, current, target) {
  # Local restore preserves the dataset identity and the ongoing audit history.
  # Shared people/vocabulary records are not rolled back for unrelated datasets.
  for (name in c("organisation_tbl", "persons_tbl", "license_template_tbl", "party_address_tbl", "keyword_thesaurus_tbl")) {
    saved <- snapshot[[name]]
    if (is.null(saved) || !nrow(saved)) next
    now <- DBI::dbReadTable(conn, name)
    for (i in seq_len(nrow(saved))) {
      found <- now[now$id == saved$id[i], names(saved), drop = FALSE]
      if (nrow(found) != 1L || !isTRUE(all.equal(unname(as.list(found[1, ])), unname(as.list(saved[i, ])), check.attributes = FALSE)))
        stop(paste("A shared reference changed since release:", name, saved$id[i], "- resolve it before restoring."))
    }
  }
  DBI::dbExecute(conn, "PRAGMA defer_foreign_keys=ON")
  preserved <- c(release_control_tables(), "dataset_review_exchange_tbl", "metadata_review_tbl", "dataset_import_tbl")
  tables <- DBI::dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table'")$name
  tables <- setdiff(tables[vapply(tables, function(t) "dataset_id" %in% DBI::dbListFields(conn, t), logical(1))], preserved)
  maps <- list()
  for (name in tables) {
    rows <- snapshot$scoped[[name]]
    if (!is.null(rows) && nrow(rows) && "id" %in% names(rows)) {
      high <- DBI::dbGetQuery(conn, paste0("SELECT COALESCE(MAX(id),0) AS n FROM ", DBI::dbQuoteIdentifier(conn, name)))$n[1]
      maps[[name]] <- stats::setNames(high + seq_len(nrow(rows)), as.character(rows$id))
    }
  }
  for (name in tables) DBI::dbExecute(conn, paste0("DELETE FROM ", DBI::dbQuoteIdentifier(conn, name), " WHERE dataset_id=?"), params = list(current$id[1]))
  for (name in tables) {
    rows <- snapshot$scoped[[name]]
    if (is.null(rows) || !nrow(rows)) next
    missing <- setdiff(names(rows), DBI::dbListFields(conn, name))
    if (length(missing)) stop(paste("Restore schema no longer supports", name, paste(missing, collapse = ", ")))
    fks <- DBI::dbGetQuery(conn, paste0("PRAGMA foreign_key_list(", DBI::dbQuoteIdentifier(conn, name), ")"))
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      if ("id" %in% names(row)) row$id <- unname(maps[[name]][as.character(row$id)])
      row$dataset_id <- current$id[1]
      for (field in setdiff(names(row), c("id", "dataset_id"))) {
        ref <- review_reference_table(field)
        fk <- fks[fks$from == field, , drop = FALSE]
        if (nrow(fk) == 1L) ref <- fk$table[1]
        if (!is.null(ref) && ref %in% tables && !is.na(row[[field]][1])) {
          value <- unname(maps[[ref]][as.character(row[[field]][1])])
          if (!length(value) || is.na(value)) stop(paste("Unresolved restored reference:", name, field))
          row[[field]] <- value
        }
      }
      row <- transfer_rewrite_row_paths(row, snapshot$entry_tbl$dataset_path[1], target)
      DBI::dbAppendTable(conn, name, row)
    }
  }
  row <- snapshot$entry_tbl
  row <- transfer_rewrite_row_paths(row, row$dataset_path[1], target)
  protected <- c("id", "uuid", "data_pond_id", "created_by_user_id", "source_dataset_id", "is_hidden", "hidden_at",
    "hidden_by_user_id", "deleted_at", "deleted_by_user_id", "archived_at", "archived_by_person_id", "released_at", "released_by_person_id")
  fields <- setdiff(names(row), protected)
  if (length(setdiff(fields, DBI::dbListFields(conn, "entry_tbl")))) stop("The saved entry schema cannot be restored without migration.")
  row$dataset_path <- target
  if (!metadata_present(row$data_path)) row$data_path <- file.path(target, "data")
  row$lifecycle_status <- "draft"; row$folder_locked <- 0L
  DBI::dbExecute(conn, paste0("UPDATE entry_tbl SET ", paste(paste0(DBI::dbQuoteIdentifier(conn, fields), "=?"), collapse = ","), " WHERE id=?"),
    params = c(lapply(fields, function(f) row[[f]][1]), list(current$id[1])))
  if (nrow(DBI::dbGetQuery(conn, "PRAGMA foreign_key_check"))) stop("Restored metadata would break database references.")
}

#' @export
#' @noRd
restore_dataset_release <- function(release_id, base_path, user_id, note) {
  release_restore_impl(release_id, base_path, user_id, note)
}

release_restore_impl <- function(release_id, base_path, user_id, note, .checkpoint = NULL) {
  if (!metadata_present(note)) stop("Record the reason for restoring this release.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  row <- release_row(conn, release_id); dataset_id <- row$dataset_id[1]
  lock <- release_lock(base_path, dataset_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_require_user(conn, user_id); release_open_review(conn, dataset_id)
  paths <- verify_dataset_release(release_id, base_path)
  if (!is.null(.checkpoint)) paths$snapshot <- file.path(.checkpoint$directory, "prior-metadata.sqlite")
  snapshot <- release_read_payload(paths$snapshot)
  current <- release_entry(conn, dataset_id)
  if (!identical(current$uuid[1], row$dataset_uuid[1])) stop("Dataset identity does not match the release.")
  old <- current$dataset_path[1]
  working <- dataset_lifecycle_stage(old, base_path) %in% c("src", "dev")
  if (working) old <- release_working_root(base_path, current)
  else if (!startsWith(normalizePath(old, winslash = "/", mustWork = TRUE), paste0(normalizePath(release_store(base_path)), "/")))
    stop("Restore requires a working draft or a managed archived release.")
  # One current record and UUID, with a distinct draft path that never overwrites.
  target <- release_safe_directory(base_path, file.path(base_path, "dev",
    paste0("restored_", substr(uuid::UUIDgenerate(), 1, 8), "__", current$uuid[1])))
  if (file.exists(target)) stop("The restore destination already exists.")
  op <- release_operation(base_path, row, "restore", old, target, user_id, note)
  op$from_checkpoint <- if (is.null(.checkpoint)) NULL else .checkpoint$operation_id
  release_operation_write(op, file.path(op$directory, "operation.json"))
  extracted <- file.path(op$directory, "extracted")
  if (is.null(.checkpoint)) verify_dataset_release(release_id, base_path, extracted) else
    release_archive_tool("extract", file.path(.checkpoint$directory, "prior-working.zip"),
      file.path(.checkpoint$directory, "prior-files.json"), extracted)
  committed <- FALSE; switched <- FALSE
  tryCatch({
    DBI::dbWithTransaction(conn, {
      DBI::dbExecute(conn, "UPDATE entry_tbl SET id=id WHERE id=?", params = list(dataset_id))
      if (!identical(release_entry(conn, dataset_id), current)) stop("Dataset changed before restore; retry.")
      # Capture the full previous metadata plus files as a private recovery point.
      prior <- release_payload(conn, dataset_id, base_path, user_id)
      write_dataset_transfer_database(prior, file.path(op$directory, "prior-metadata.sqlite"))
      op$prior_snapshot_sha256 <- release_sha(file.path(op$directory, "prior-metadata.sqlite"))
      if (working) {
        files <- release_inventory(old)
        release_json(files, file.path(op$directory, "prior-files.json"))
        release_archive_tool("pack", old, file.path(op$directory, "prior-files.json"), file.path(op$directory, "prior-working.zip"))
        op$prior_manifest_sha256 <- release_sha(file.path(op$directory, "prior-files.json"))
        op$prior_zip_sha256 <- release_sha(file.path(op$directory, "prior-working.zip"))
        release_operation_write(op, file.path(op$directory, "operation.json"))
        if (!file.rename(old, op$backup)) stop("Could not preserve the previous working folder.")
      }
      switched <- TRUE
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      restored_root <- if (is.null(.checkpoint)) file.path(extracted, "dataset") else extracted
      if (!file.rename(restored_root, target)) stop("Could not place the restored dataset in dev/.")
      release_after_switch()
      release_restore_metadata(conn, snapshot, current, target)
      # Marker creation is the sole intentional change to the restored files.
      write_dataset_marker(target, current$uuid[1], current$data_pond_id[1])
      if (working && !identical(review_hash(release_inventory(op$backup)), review_hash(files))) stop("Previous working files changed during restore.")
      release_event(conn, dataset_id, release_id, op$operation_id,
        if (is.null(.checkpoint)) "restore_draft" else "rollback_draft", user_id, note)
      release_before_commit()
    })
    committed <- TRUE
    op$phase <- "committed"; release_operation_write(op, file.path(op$directory, "operation.json"))
    unlink(extracted, recursive = TRUE)
    release_cleanup_operation(op, base_path)
  }, error = function(e) {
    if (!committed) {
      if (switched) release_rollback_files(op) else {
        op$phase <- "rolled_back"; release_operation_write(op, file.path(op$directory, "operation.json"))
      }
    }
    stop(conditionMessage(e), call. = FALSE)
  })
  list(dataset_id = dataset_id, dataset_path = target, checkpoint = op$directory, release_id = release_id)
}

#' @export
#' @noRd
dataset_release_checkpoints <- function(dataset_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  release_ensure_schema(conn)
  rows <- DBI::dbGetQuery(conn, "SELECT operation_id,happened_at,note FROM dataset_release_event_tbl
    WHERE dataset_id=? AND action IN ('restore_draft','rollback_draft') ORDER BY happened_at DESC,id DESC",
    params = list(as.integer(dataset_id)))
  rows[vapply(rows$operation_id, function(id) file.exists(release_private(base_path, "operations", id, "prior-working.zip")), logical(1)), , drop = FALSE]
}

release_verify_checkpoint <- function(op) {
  directory <- op$directory
  files <- c("prior-metadata.sqlite" = "prior_snapshot_sha256", "prior-files.json" = "prior_manifest_sha256", "prior-working.zip" = "prior_zip_sha256")
  for (name in names(files)) {
    value <- op[[files[[name]]]]
    if (is.null(value) || !file.exists(file.path(directory, name)) || !identical(release_sha(file.path(directory, name)), value))
      stop("Recovery checkpoint failed its integrity check.")
  }
  invisible(op)
}

#' @export
#' @noRd
restore_dataset_checkpoint <- function(operation_id, base_path, user_id, note) {
  if (!grepl("^[a-fA-F0-9-]+$", operation_id)) stop("Invalid checkpoint identifier.")
  directory <- release_safe_directory(base_path, release_private(base_path, "operations", operation_id))
  op <- jsonlite::read_json(file.path(directory, "operation.json"))
  if (!identical(op$directory, directory) || !op$phase %in% c("complete", "committed")) stop("Checkpoint operation needs recovery before rollback.")
  release_verify_checkpoint(op)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  if (!nrow(DBI::dbGetQuery(conn, "SELECT id FROM dataset_release_event_tbl WHERE operation_id=? AND dataset_id=?",
    params = list(operation_id, op$dataset_id)))) stop("Checkpoint is not a committed lifecycle operation.")
  release_restore_impl(op$release_id, base_path, user_id, note, .checkpoint = op)
}

# Release cache: unpacked bytes are the usable copy; transport ZIP is optional.
#' @export
#' @noRd
cache_dataset_release <- function(release_id, base_path, cache, keep_zip = FALSE) {
  paths <- verify_dataset_release(release_id, base_path)
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  cache <- normalizePath(cache, winslash = "/", mustWork = TRUE)
  pond <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  if (cache == pond || startsWith(cache, paste0(pond, "/"))) stop("Use a separate cache outside the pond.")
  target <- file.path(cache, release_id)
  if (file.exists(target)) stop("This release is already cached; existing cached versions are never updated in place.")
  stage <- tempfile(".release-download-", tmpdir = cache); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  if (!file.copy(paths$zip, file.path(stage, "download.zip")) ||
      !identical(release_sha(file.path(stage, "download.zip")), paths$record$zip_sha256[1])) stop("Downloaded archive checksum mismatch.")
  release_archive_tool("extract", file.path(stage, "download.zip"), paths$manifest, file.path(stage, "release"))
  release_json(list(release_id = release_id, dataset_uuid = paths$record$dataset_uuid[1],
    version = paths$record$version[1], zip_sha256 = paths$record$zip_sha256[1], verified_at = release_now(),
    manifest_sha256 = paths$record$manifest_sha256[1],
    keep_zip = keep_zip), file.path(stage, "receipt.json"))
  cached_manifest <- file.path(stage, "files.json")
  if (!file.copy(paths$manifest, cached_manifest) ||
      !identical(release_sha(cached_manifest), paths$record$manifest_sha256[1])) stop("Could not preserve the verified cache manifest.")
  if (!keep_zip && unlink(file.path(stage, "download.zip")) != 0L) stop("Could not remove the temporary transport ZIP.")
  if (file.exists(target) || !file.rename(stage, target)) stop("Could not finalise the release cache.")
  target
}

#' @export
#' @noRd
verify_cached_dataset_release <- function(directory) {
  receipt <- jsonlite::read_json(file.path(directory, "receipt.json"))
  if (isTRUE(receipt$keep_zip) && (!file.exists(file.path(directory, "download.zip")) ||
      !identical(release_sha(file.path(directory, "download.zip")), receipt$zip_sha256)))
    stop("Retained cache ZIP failed verification.")
  manifest <- file.path(directory, "files.json")
  if (!identical(release_sha(manifest), receipt$manifest_sha256)) stop("Cached release manifest failed verification.")
  expected <- jsonlite::read_json(manifest)
  if (!identical(review_hash(release_inventory(file.path(directory, "release"))), review_hash(expected)))
    stop("Cached release files differ from the verified release.")
  invisible(receipt)
}

#' @export
#' @noRd
recover_dataset_release_operations <- function(dataset_id, base_path, user_id) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_ensure_schema(conn); release_require_user(conn, user_id)
  lock <- release_safe_directory(base_path, release_private(base_path, "locks", as.character(as.integer(dataset_id))))
  if (dir.exists(lock)) {
    owner_path <- file.path(lock, "owner.json")
    if (!file.exists(owner_path)) stop("The lifecycle lock has no owner record; inspect it before recovery.")
    owner <- jsonlite::read_json(owner_path)
    if (!identical(owner$host, Sys.info()[["nodename"]]) || movement_process_alive(owner$pid, owner$started))
      stop("The lifecycle operation may still be running. Recovery will not interrupt it.")
    unlink(lock, recursive = TRUE)
  }
  lock <- release_lock(base_path, dataset_id, recovery = TRUE); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  private <- release_private(base_path)
  messages <- character()
  journals <- release_journals(private, "freeze-operation.json")
  for (file in journals) {
    job <- jsonlite::read_json(file)
    if (job$dataset_id != dataset_id) next
    id <- job$release_id
    if (nrow(DBI::dbGetQuery(conn, "SELECT release_id FROM dataset_release_tbl WHERE release_id=?", params = list(id)))) next
    row <- as.data.frame(job$row, stringsAsFactors = FALSE)
    destination <- release_safe_directory(base_path, job$destination)
    if (!identical(destination, release_safe_directory(base_path, file.path(base_path, row$relative_path)))) stop("Invalid release recovery destination.")
    if (!dir.exists(destination)) {
      # Nothing was published; retain any interrupted staging for diagnosis.
      messages <- c(messages, paste("Unpublished staging retained for", id))
      next
    }
    paths <- release_paths(base_path, row)
    for (name in c("zip", "manifest", "snapshot", "source_manifest")) {
      expected <- switch(name, zip = row$zip_sha256, manifest = row$manifest_sha256,
        snapshot = row$snapshot_sha256, source_manifest = row$source_manifest_sha256)
      if (!file.exists(paths[[name]]) || !identical(release_sha(paths[[name]]), expected[1])) stop("Interrupted release failed integrity verification.")
    }
    release_archive_tool("verify", paths$zip, paths$manifest)
    if (!identical(release_entry(conn, dataset_id)$uuid[1], row$dataset_uuid[1])) stop("Interrupted release has a different dataset identity.")
    DBI::dbWithTransaction(conn, {
      DBI::dbAppendTable(conn, "dataset_release_tbl", row)
      release_event(conn, dataset_id, id, id, "freeze_recovered", user_id, "Recovered verified release registration after interruption.")
    })
    messages <- c(messages, paste("Recovered frozen release", row$version))
  }
  journals <- release_journals(file.path(private, "operations"), "operation.json")
  for (file in journals) {
    op <- jsonlite::read_json(file)
    if (op$dataset_id != dataset_id || op$phase %in% c("complete", "rolled_back")) next
    if (!identical(normalizePath(dirname(file)), normalizePath(op$directory)) ||
        !identical(op$backup, file.path(op$directory, "prior-working"))) stop("Invalid recovery journal paths.")
    for (p in c(op$old_path, op$new_path, op$backup)) release_safe_directory(base_path, p)
    event <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_event_tbl WHERE operation_id=?", params = list(op$operation_id))
    if (nrow(event)) {
      verify_dataset_release(op$release_id, base_path)
      if (!identical(release_entry(conn, dataset_id)$dataset_path[1], op$new_path)) stop("Dataset moved after the interrupted operation; inspect recovery manually.")
      if (op$kind == "restore" && file.exists(file.path(op$directory, "prior-working.zip")))
        release_archive_tool("verify", file.path(op$directory, "prior-working.zip"), file.path(op$directory, "prior-files.json"))
      release_cleanup_operation(op, base_path)
      messages <- c(messages, paste("Completed cleanup for", op$kind))
    } else {
      # SQLite rolled back. Return the prior folder, leaving the attempted
      # restored copy and checkpoint available for inspection.
      release_rollback_files(op)
      messages <- c(messages, paste("Rolled back interrupted", op$kind))
    }
  }
  if (!length(messages)) messages <- "No interrupted lifecycle operations need recovery."
  messages
}
