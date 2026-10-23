# Metadata-only review exchange. Working datasets remain ordinary transfer bundles;
# review bundles carry an explicit purpose and can never overwrite through import.
review_ensure_schema <- function(conn) {
  DBI::dbExecute(conn, "CREATE TABLE IF NOT EXISTS dataset_review_exchange_tbl (
    review_id TEXT PRIMARY KEY, dataset_id INTEGER NOT NULL, role TEXT NOT NULL,
    status TEXT NOT NULL, master_pond_id TEXT NOT NULL, dataset_uuid TEXT NOT NULL,
    request_bundle_id TEXT NOT NULL, baseline_hash TEXT NOT NULL,
    files_hash TEXT NOT NULL, assignee TEXT NOT NULL, issued_at TEXT NOT NULL,
    issued_by_user_id INTEGER, accepted_by_user_id INTEGER, accepted_at TEXT,
    returned_bundle_id TEXT, decision_note TEXT)")
}

review_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
  na = "null", null = "null", digits = NA, dataframe = "rows"))
review_hash <- function(x) digest::digest(review_json(x), algo = "sha256", serialize = FALSE)
review_directory <- function(pond, id) {
  if (length(id) != 1L || is.na(id) || !grepl("^[A-Za-z0-9_-]+$", id)) stop("Invalid review identifier.")
  file.path(pond, ".metadata_pond", "reviews", id)
}
review_require_user <- function(conn, user_id, admin = FALSE) {
  user_id <- transfer_scalar_integer(user_id)
  if (is.na(user_id)) stop("Select an active application user.")
  rows <- DBI::dbGetQuery(conn, "SELECT * FROM user_tbl WHERE id = ? AND COALESCE(is_active,1)=1",
    params = list(as.integer(user_id)))
  if (nrow(rows) != 1L) stop("Select an active application user.")
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    auth <- access_context()
    access_require(if (admin) "admin" else "edit")
    identity <- access_session(auth$pond, auth$token)
    if (!identical(as.integer(user_id), as.integer(identity$user_id[1]))) stop("The reviewer must be the signed-in user.")
  } else if (admin && !isTRUE(as.integer(rows$is_admin[1]) == 1L)) stop("A pond administrator must accept or cancel a review.")
  rows
}
review_requests <- function(pond, dataset_id = NULL) {
  conn <- get_conn(pond); on.exit(DBI::dbDisconnect(conn))
  review_ensure_schema(conn)
  if (is.null(dataset_id)) return(DBI::dbReadTable(conn, "dataset_review_exchange_tbl"))
  DBI::dbGetQuery(conn, "SELECT * FROM dataset_review_exchange_tbl WHERE dataset_id = ? ORDER BY issued_at",
    params = list(as.integer(dataset_id)))
}
review_request <- function(conn, id) {
  review_ensure_schema(conn)
  row <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_review_exchange_tbl WHERE review_id = ?", params = list(id))
  if (nrow(row) != 1L) stop("This review request is not registered in this pond.")
  row
}

# Canonical comparisons replace pond-local keys with referenced record content.
# Thus re-numbering on import is not presented as a change to the metadata.
review_tables <- function(payload) c(list(entry_tbl = payload$entry_tbl), payload$scoped,
  payload[c("organisation_tbl", "persons_tbl", "user_tbl", "license_template_tbl", "party_address_tbl", "keyword_thesaurus_tbl")])
review_reference_table <- function(field) {
  explicit <- c(reviewer_id = "persons_tbl", parent_organisation_id = "organisation_tbl",
    license_template_id = "license_template_tbl", data_file_id = "data_file_tbl",
    attribute_id = "dataset_attribute_tbl", latest_scan_id = "data_file_scan_tbl",
    supporting_document_id = "supporting_document_tbl", process_step_id = "dataset_lineage_process_step_tbl",
    source_id = "dataset_lineage_source_tbl", thesaurus_id = "keyword_thesaurus_tbl")
  if (field %in% names(explicit)) return(unname(explicit[field]))
  if (grepl("person_id$", field)) return("persons_tbl")
  if (grepl("organisation_id$", field)) return("organisation_tbl")
  if (grepl("user_id$", field)) return("user_tbl")
  NULL
}
review_canonical_row <- function(row, tables, source_path, depth = 0L) {
  ignore <- c("id", "dataset_id", "data_pond_id", "source_dataset_id", "created_by_user_id",
    "updated_at", "updated_by_user_id", "updated_by_person_id", "is_admin", "is_active",
    "legacy_citation_v1_migrated_at", "legacy_wave2_v1_migrated_at",
    "legacy_party_v1_migrated_at", "legacy_licensing_v1_migrated_at")
  result <- list()
  for (field in sort(setdiff(names(row), ignore))) {
    value <- row[[field]][1]
    ref <- review_reference_table(field)
    if (!is.null(ref) && !is.na(value) && depth < 6L) {
      target <- tables[[ref]]
      hit <- if (!is.null(target) && "id" %in% names(target)) target[target$id == value, , drop = FALSE] else data.frame()
      if (nrow(hit) == 1L) value <- review_canonical_row(hit, tables, source_path, depth + 1L)
      else value <- paste0("Unresolved ", ref, " ", value)
    } else if (is.character(value) && !is.na(value) && nzchar(source_path)) {
      value <- gsub("\\\\", "/", value)
      if (identical(value, source_path) || startsWith(value, paste0(source_path, "/")))
        value <- paste0("<dataset>", substring(value, nchar(source_path) + 1L))
    }
    result[[field]] <- value
  }
  result
}
review_canonical <- function(payload) {
  tables <- review_tables(payload)
  path <- gsub("\\\\", "/", payload$manifest$source_dataset_path[1])
  result <- list()
  for (table in sort(names(tables))) {
    rows <- tables[[table]]
    result[[table]] <- if (is.null(rows) || !nrow(rows)) character() else sort(vapply(seq_len(nrow(rows)),
      function(i) review_json(review_canonical_row(rows[i, , drop = FALSE], tables, path)), character(1)))
  }
  result
}
# The conflict fingerprint is deliberately stricter than the human diff: includes
# local IDs and audit timestamps, so any saved master changes invalidate a return.
review_fingerprint <- function(payload) {
  tables <- review_tables(payload)
  tables <- tables[sort(names(tables))]
  for (name in names(tables)) {
    rows <- tables[[name]]
    if (!is.null(rows) && nrow(rows)) {
      rows <- rows[, sort(names(rows)), drop = FALSE]
      keys <- vapply(seq_len(nrow(rows)), function(i) review_json(rows[i, , drop = FALSE]), character(1))
      rows <- rows[order(keys), , drop = FALSE]
      rownames(rows) <- NULL
      tables[[name]] <- rows
    }
  }
  review_hash(tables)
}
review_files <- function(path) {
  files <- sort(list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = FALSE, include.dirs = TRUE))
  if (any(nzchar(Sys.readlink(file.path(path, files))))) stop("Review snapshots do not support symbolic links.")
  files <- files[!file.info(file.path(path, files))$isdir]
  # Ordinary portable bundles are transport artefacts, not dataset content.
  files <- files[!grepl("^[^/]+__datapond_[[:alnum:]_-]+\\.zip$", files)]
  files <- files[!startsWith(files, ".datapond_transfer/")]
  paths <- file.path(path, files)
  if (any(nzchar(Sys.readlink(paths)))) stop("Review snapshots do not support symbolic links.")
  values <- vapply(paths, function(file) digest::digest(file = file, algo = "sha256"), character(1))
  stats::setNames(as.list(unname(values)), files)
}
review_check_archive <- function(archive) {
  if (!identical(archive$names, archive$metadata))
    stop("A metadata review bundle must contain only its metadata database; file payloads are not accepted.")
}
review_write_bundle <- function(payload, directory, kind) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("review-package-"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  root <- payload$manifest$source_dataset_folder[1]
  if (!grepl("^[^/\\\\]+$", root) || root %in% c(".", "..")) stop("Invalid dataset folder name.")
  relative <- file.path(root, ".datapond_transfer", "dataset_metadata.sqlite")
  dir.create(dirname(file.path(stage, relative)), recursive = TRUE)
  write_dataset_transfer_database(payload, file.path(stage, relative))
  dest <- normalizePath(directory, mustWork = TRUE)
  archive <- file.path(dest, paste0(kind, "-", payload$manifest$bundle_id[1], ".zip"))
  old <- setwd(stage); on.exit(setwd(old), add = TRUE)
  status <- utils::zip(archive, relative, flags = "-q6X")
  if (!identical(as.integer(status), 0L) || !file.exists(archive)) stop("Could not write review bundle.")
  review_check_archive(dataset_transfer_archive_index(archive))
  archive
}
review_read_bundle <- function(path) {
  archive <- dataset_transfer_archive_index(path); review_check_archive(archive)
  temp <- tempfile("review-read-"); dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE))
  utils::unzip(path, files = archive$metadata, exdir = temp)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(temp, archive$metadata), flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  tables <- DBI::dbListTables(conn)
  read <- function(name) if (name %in% tables) DBI::dbReadTable(conn, name) else data.frame()
  refs <- c("organisation_tbl", "persons_tbl", "user_tbl", "license_template_tbl", "party_address_tbl", "keyword_thesaurus_tbl")
  payload <- c(list(manifest = read("bundle_manifest_tbl"), entry_tbl = read("entry_tbl"),
    scoped = stats::setNames(lapply(setdiff(tables, c("bundle_manifest_tbl", "entry_tbl", refs)), read),
      setdiff(tables, c("bundle_manifest_tbl", "entry_tbl", refs)))), stats::setNames(lapply(refs, read), refs))
  if (nrow(payload$manifest) != 1L || nrow(payload$entry_tbl) != 1L ||
      !identical(payload$manifest$bundle_format[1], "datapond_dataset_bundle") ||
      is.na(payload$manifest$bundle_format_version[1]) || payload$manifest$bundle_format_version[1] != 5L ||
      !identical(payload$manifest$source_dataset_uuid[1], payload$entry_tbl$uuid[1])) stop("Invalid review bundle identity or format.")
  for (rows in payload$scoped) {
    if (!"dataset_id" %in% names(rows) || any(is.na(rows$dataset_id)) || any(rows$dataset_id != payload$entry_tbl$id[1]))
      stop("Review bundle contains records belonging to another dataset.")
  }
  payload
}
review_manifest <- function(payload, request, kind) {
  for (field in c("review_id", "master_pond_id", "request_bundle_id", "baseline_hash", "files_hash", "assignee", "issued_at"))
    payload$manifest[[field]] <- request[[field]][1]
  payload$manifest$review_kind <- kind
  payload
}

issue_dataset_review <- function(dataset_id, base_path, user_id, assignee) {
  if (!non_empty_scalar(assignee)) stop("Name the colleague responsible for this review.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  review_require_user(conn, user_id); review_ensure_schema(conn)
  id <- new_uuid(); directory <- review_directory(base_path, id)
  success <- FALSE
  on.exit(if (!success) unlink(directory, recursive = TRUE), add = TRUE)
  result <- DBI::dbWithTransaction(conn, {
    # Obtain a write lock before reading the baseline and reserving the request.
    DBI::dbExecute(conn, "UPDATE dataset_review_exchange_tbl SET status=status WHERE 0")
    pending <- DBI::dbGetQuery(conn, "SELECT review_id FROM dataset_review_exchange_tbl WHERE dataset_id=? AND status IN ('issued','working')",
      params = list(as.integer(dataset_id)))
    if (nrow(pending)) stop("This dataset already has an active review. Complete or cancel it first.")
    payload <- collect_dataset_transfer_payload(dataset_id, base_path, user_id, conn = conn)
    roots <- normalizePath(file.path(base_path, c("src", "dev", "arc")), mustWork = FALSE, winslash = "/")
    if (!any(vapply(roots, function(root) startsWith(payload$manifest$source_dataset_path[1], paste0(root, "/")), logical(1))))
      stop("Review datasets must be inside this pond's managed lifecycle folders.")
    request <- data.frame(review_id = id, dataset_id = as.integer(dataset_id), role = "master", status = "issued",
      master_pond_id = payload$manifest$source_data_pond_id[1], dataset_uuid = payload$entry_tbl$uuid[1],
      request_bundle_id = payload$manifest$bundle_id[1], baseline_hash = review_fingerprint(payload),
      files_hash = review_hash(review_files(payload$entry_tbl$dataset_path[1])), assignee = trimws(assignee),
      issued_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), issued_by_user_id = as.integer(user_id))
    dir.create(directory, recursive = TRUE)
    saveRDS(payload, file.path(directory, "baseline.rds"))
    archive <- review_write_bundle(review_manifest(payload, request, "request"), directory, "request")
    if (!identical(request$files_hash[1], review_hash(review_files(payload$entry_tbl$dataset_path[1]))))
      stop("Dataset files changed while issuing the review. Retry after saving and closing file editors.")
    transfer_insert_row(conn, "dataset_review_exchange_tbl", request, drop = character())
    list(path = archive, review_id = id)
  })
  success <- TRUE
  result
}
review_receive_request <- function(conn, manifest, dataset_id) {
  review_ensure_schema(conn)
  fields <- c("review_id", "master_pond_id", "request_bundle_id", "baseline_hash", "files_hash", "assignee", "issued_at")
  if (!all(fields %in% names(manifest)) || any(vapply(manifest[fields], function(x) !non_empty_scalar(x), logical(1))))
    stop("The review request is incomplete.")
  row <- manifest[1, fields, drop = FALSE]
  row$dataset_uuid <- manifest$source_dataset_uuid[1]
  row$dataset_id <- dataset_id; row$role <- "reviewer"; row$status <- "working"
  transfer_insert_row(conn, "dataset_review_exchange_tbl", row, drop = character())
}
create_dataset_review_return <- function(dataset_id, base_path, user_id) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  review_require_user(conn, user_id); review_ensure_schema(conn)
  request <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_review_exchange_tbl WHERE dataset_id=? AND role='reviewer' AND status='working'",
    params = list(as.integer(dataset_id)))
  if (nrow(request) != 1L) stop("Select a dataset imported from a metadata review request.")
  payload <- collect_dataset_transfer_payload(dataset_id, base_path, user_id, conn = conn)
  directory <- review_directory(base_path, request$review_id[1])
  path <- review_write_bundle(review_manifest(payload, request, "return"), directory, "return")
  list(path = path, review_id = request$review_id[1])
}

review_new_history <- function(rows, baseline) {
  if (is.null(rows) || !nrow(rows)) return(rows)
  fields <- setdiff(names(rows), c("id", "dataset_id", "reviewer_id"))
  key <- function(df) if (is.null(df) || !nrow(df)) character() else vapply(seq_len(nrow(df)), function(i) review_json(df[i, intersect(fields, names(df)), drop = FALSE]), character(1))
  rows[!key(rows) %in% key(baseline), , drop = FALSE]
}
review_import_history <- function(conn, rows, dataset_id, user_map) {
  if (is.null(rows) || !nrow(rows)) return(invisible(NULL))
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    existing <- DBI::dbGetQuery(conn, "SELECT id FROM dataset_import_tbl WHERE bundle_id=?", params = list(row$bundle_id[1]))
    if (nrow(existing)) next
    row$dataset_id <- dataset_id
    row$imported_by_user_id <- transfer_remap_id(row$imported_by_user_id, user_map)
    transfer_insert_row(conn, "dataset_import_tbl", row)
  }
}
review_protected_entry <- function() c("uuid", "metadata_uuid", "citation_key", "folder_key", "dataset_path", "data_path",
  "legacy_citation_v1_migrated_at", "legacy_wave2_v1_migrated_at",
  "data_pond_id", "created_by_user_id", "source_dataset_id", "lifecycle_status", "folder_locked",
  "released_at", "released_by_person_id", "archived_at", "archived_by_person_id", "is_hidden", "hidden_at", "hidden_by_user_id", "deleted_at", "deleted_by_user_id")
review_protected_tables <- function() c("data_file_tbl", "data_file_scan_tbl", "data_profile_run_tbl",
  "supporting_document_tbl", "dataset_attribute_tbl", "data_file_spatial_metadata_tbl",
  "dataset_spatial_summary_tbl", "party_address_tbl", "keyword_thesaurus_tbl")
review_diff <- function(before, after) {
  a <- review_canonical(before); b <- review_canonical(after)
  rows <- list()
  label <- function(x) tools::toTitleCase(gsub("_", " ", sub("_tbl$", "", x)))
  # Entry metadata is presented field by field; repeatable rows show exact values.
  aa <- jsonlite::fromJSON(a$entry_tbl[1], simplifyVector = FALSE)
  bb <- jsonlite::fromJSON(b$entry_tbl[1], simplifyVector = FALSE)
  for (field in setdiff(union(names(aa), names(bb)), review_protected_entry())) if (!identical(aa[[field]], bb[[field]]))
    rows[[length(rows)+1L]] <- data.frame(Section = "Dataset", Change = label(field), Before = review_json(aa[[field]]), After = review_json(bb[[field]]))
  for (table in setdiff(union(names(a), names(b)), c("entry_tbl", "dataset_import_tbl", "user_tbl"))) {
    old <- a[[table]]; new <- b[[table]]
    for (value in setdiff(old, new)) rows[[length(rows)+1L]] <- data.frame(Section = label(table), Change = "Removed or changed record", Before = value, After = "")
    for (value in setdiff(new, old)) rows[[length(rows)+1L]] <- data.frame(Section = label(table), Change = "Added or changed record", Before = "", After = value)
  }
  if (!length(rows)) return(data.frame(Section=character(), Change=character(), Before=character(), After=character()))
  do.call(rbind, rows)
}

review_same_rows <- function(a, b) identical(sort(a %||% character()), sort(b %||% character()))
review_validate_metadata <- function(baseline, returned, conn) {
  a <- review_canonical(baseline); b <- review_canonical(returned)
  incoming_tables <- review_tables(returned)
  for (table in setdiff(names(incoming_tables), "dataset_import_tbl")) {
    rows <- incoming_tables[[table]]
    if (is.null(rows) || !nrow(rows)) next
    for (field in names(rows)) {
      ref <- review_reference_table(field)
      if (is.null(ref)) next
      ids <- rows[[field]][!is.na(rows[[field]])]
      target <- incoming_tables[[ref]]
      if (length(ids) && (is.null(target) || !"id" %in% names(target) || any(!ids %in% target$id)))
        stop("Unresolved returned metadata reference: ", table, ".", field)
    }
  }
  for (field in c("uuid", "metadata_uuid", "lifecycle_status", "is_hidden", "deleted_at")) {
    if (!identical(transfer_scalar_text(baseline$entry_tbl[[field]]), transfer_scalar_text(returned$entry_tbl[[field]])))
      stop("Review returns cannot change dataset identity, lifecycle or visibility: ", field)
  }
  for (table in review_protected_tables()) if (!review_same_rows(a[[table]], b[[table]]))
    stop("Metadata-only review cannot change file inventories, supporting documents or shared vocabularies: ", table)
  history <- a$metadata_review_tbl %||% character()
  incoming <- b$metadata_review_tbl %||% character()
  if (length(history) && any(vapply(unique(history), function(x) sum(incoming == x) < sum(history == x), logical(1))))
    stop("The return removed or changed existing review history. Original reviews must be preserved.")
  # Existing shared parties cannot be silently overwritten or silently ignored.
  # Compare against the master, including parties belonging to other datasets.
  shared <- review_tables(baseline)
  for (table in c("organisation_tbl", "persons_tbl", "user_tbl"))
    shared[[table]] <- DBI::dbReadTable(conn, table)
  incoming_tables <- review_tables(returned)
  for (table in c("organisation_tbl", "persons_tbl")) {
    rows <- incoming_tables[[table]]
    existing <- shared[[table]]
    if (is.null(rows) || !nrow(rows)) next
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      if (table == "organisation_tbl") {
        hit <- which(tolower(trimws(existing$organisation_name_full)) == tolower(trimws(row$organisation_name_full[1])) &
          ifelse(is.na(existing$organisation_department), "", trimws(existing$organisation_department)) ==
          transfer_scalar_text(row$organisation_department, ""))
      } else {
        email <- transfer_scalar_text(row$email)
        hit <- if (!is.na(email)) which(tolower(trimws(existing$email)) == tolower(trimws(email))) else
          which(existing$name_first == row$name_first[1] & existing$name_last == row$name_last[1])
      }
      for (j in hit) {
        old <- review_canonical_row(existing[j, , drop = FALSE], shared, baseline$manifest$source_dataset_path[1])
        new <- review_canonical_row(row, incoming_tables, returned$manifest$source_dataset_path[1])
        # Newer app versions may add blank columns; compare all shared columns.
        fields <- union(names(old), names(new))
        unequal <- vapply(fields, function(k) !identical(review_json(old[[k]]), review_json(new[[k]])), logical(1))
        if (any(unequal)) stop("A shared party was edited. Resolve that change in the master before issuing a new review: ", table)
      }
    }
  }
  templates <- returned$license_template_tbl
  if (!is.null(templates) && nrow(templates)) for (i in seq_len(nrow(templates))) {
    old <- DBI::dbGetQuery(conn, "SELECT * FROM license_template_tbl WHERE LOWER(TRIM(license_identifier))=LOWER(TRIM(?))",
      params=list(templates$license_identifier[i]))
    if (nrow(old)!=1L) stop("A returned licence template is not available in the master.")
    cols <- intersect(names(old),names(templates))
    old <- review_canonical_row(old[,cols,drop=FALSE],list(),"")
    new <- review_canonical_row(templates[i,cols,drop=FALSE],list(),"")
    if (!identical(old,new)) stop("A shared licence template was edited. Resolve that in the master first.")
  }
  master_tables <- DBI::dbListTables(conn)
  for (table in names(returned$scoped)) {
    if (!table %in% master_tables || !"dataset_id" %in% DBI::dbListFields(conn, table))
      stop("Unsupported returned metadata table: ", table)
    extra <- setdiff(names(returned$scoped[[table]]), DBI::dbListFields(conn, table))
    if (length(extra)) stop("Unsupported returned fields in ", table, ": ", paste(extra, collapse = ", "))
  }
  # Existing inventory row IDs stay stable in the master. Map return-local keys
  # back to those rows before importing lineage, quality and other references.
  maps <- list()
  for (table in intersect(review_protected_tables(), names(baseline$scoped))) {
    old <- baseline$scoped[[table]]; new <- returned$scoped[[table]]
    if (is.null(old) || !nrow(old)) { maps[[table]] <- integer(); next }
    canon <- function(rows, payload) vapply(seq_len(nrow(rows)), function(i)
      review_json(review_canonical_row(rows[i, , drop = FALSE], review_tables(payload), payload$manifest$source_dataset_path[1])), character(1))
    old_order <- order(canon(old, baseline), old$id)
    new_order <- order(canon(new, returned), new$id)
    maps[[table]] <- stats::setNames(as.integer(old$id[old_order]), as.character(new$id[new_order]))
  }
  maps
}

inspect_dataset_review_return <- function(path, base_path) {
  returned <- review_read_bundle(path)
  if (!identical(transfer_scalar_text(returned$manifest$review_kind), "return")) stop("Choose a returned review bundle, not an outgoing request or ordinary transfer.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  request <- review_request(conn, transfer_scalar_text(returned$manifest$review_id))
  if (request$role[1] != "master" || request$status[1] != "issued") stop("This review is not awaiting acceptance (it may be accepted or cancelled).")
  for (field in c("master_pond_id", "request_bundle_id", "baseline_hash", "files_hash", "assignee", "issued_at"))
    if (!identical(transfer_scalar_text(returned$manifest[[field]]), transfer_scalar_text(request[[field]])))
      stop("The return does not match its registered request: ", field)
  if (!identical(returned$entry_tbl$uuid[1], request$dataset_uuid[1])) stop("Wrong dataset identity.")
  baseline <- readRDS(file.path(review_directory(base_path, request$review_id[1]), "baseline.rds"))
  if (!identical(review_fingerprint(baseline), request$baseline_hash[1])) stop("The saved review baseline is damaged or altered.")
  current <- collect_dataset_transfer_payload(request$dataset_id[1], base_path,
    baseline$manifest$source_exported_by_user_id[1], conn = conn)
  conflicts <- character()
  if (!identical(review_fingerprint(current), request$baseline_hash[1])) conflicts <- c(conflicts, "Master metadata changed since this review was issued.")
  if (!identical(review_hash(review_files(current$entry_tbl$dataset_path[1])), request$files_hash[1])) conflicts <- c(conflicts, "Master files changed since this review was issued.")
  maps <- review_validate_metadata(baseline, returned, conn)
  list(dataset_id = request$dataset_id[1], review_id = request$review_id[1], request = request,
    baseline = baseline, returned = returned, protected_maps = maps,
    changes = review_diff(baseline, returned), conflicts = conflicts,
    can_accept = !length(conflicts), return_hash = digest::digest(file = path, algo = "sha256"))
}

review_begin_accept <- function(conn, assessment, pond) {
  # Acquire the database write lock before rechecking the baseline.
  DBI::dbExecute(conn, "UPDATE dataset_review_exchange_tbl SET status=status WHERE review_id=?", params = list(assessment$review_id))
  request <- review_request(conn, assessment$review_id)
  if (request$status[1] != "issued") stop("This review is no longer awaiting acceptance.")
  current <- collect_dataset_transfer_payload(assessment$dataset_id, pond,
    assessment$baseline$manifest$source_exported_by_user_id[1], conn = conn)
  if (!identical(review_fingerprint(current), request$baseline_hash[1])) stop("Master metadata changed; acceptance cancelled.")
  if (!identical(review_hash(review_files(current$entry_tbl$dataset_path[1])), request$files_hash[1])) stop("Master files changed; acceptance cancelled.")
  review_require_user(conn, assessment$user_id, admin = TRUE)
  write_dataset_transfer_database(current, assessment$backup_path)
  DBI::dbExecute(conn, "PRAGMA defer_foreign_keys=ON")
  replace <- setdiff(names(current$scoped), c(review_protected_tables(), "metadata_review_tbl", "dataset_import_tbl"))
  for (table in replace) DBI::dbExecute(conn, paste0("DELETE FROM ", transfer_quote_table(conn, table), " WHERE dataset_id=?"),
    params = list(assessment$dataset_id))
}
review_update_entry <- function(conn, row, master) {
  fields <- setdiff(intersect(names(row), DBI::dbListFields(conn, "entry_tbl")), c("id", review_protected_entry()))
  if (length(fields)) DBI::dbExecute(conn, paste0("UPDATE entry_tbl SET ",
    paste(paste0(DBI::dbQuoteIdentifier(conn, fields), "=?"), collapse = ","), " WHERE id=?"),
    params = c(lapply(fields, function(field) row[[field]][1]), list(master$id[1])))
}
review_finish_accept <- function(conn, assessment, user_id, manifest) {
  # Recheck file bytes immediately before committing metadata; acceptance never
  # writes files, and local file editors must stay closed during this operation.
  if (!identical(review_hash(review_files(assessment$baseline$entry_tbl$dataset_path[1])), assessment$request$files_hash[1]))
    stop("Master files changed during acceptance; metadata rolled back.")
  user <- DBI::dbGetQuery(conn, "SELECT p.id, p.name_first, p.name_last FROM user_tbl u JOIN persons_tbl p ON p.id=u.person_id WHERE u.id=?", params=list(as.integer(user_id)))
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  transfer_insert_row(conn, "metadata_review_tbl", data.frame(dataset_id=assessment$dataset_id,
    review_timestamp=now, review_date=substr(now,1,10), reviewer_id=user$id[1],
    reviewer_name=paste(user$name_first[1],user$name_last[1]), reviewer_role="master curator",
    update_type="metadata_revision", update_reason=assessment$decision,
    update_summary=paste("Accepted metadata review", assessment$review_id, "bundle", manifest$bundle_id[1]),
    metadata_section="general", metadata_status="reviewed", source_of_change="review_bundle_acceptance",
    app_version=as.character(utils::packageVersion("datapond")), notes=paste("Previous metadata snapshot:",assessment$backup_path)))
  DBI::dbExecute(conn, "UPDATE dataset_review_exchange_tbl SET status='accepted', accepted_by_user_id=?, accepted_at=?, returned_bundle_id=?, decision_note=? WHERE review_id=?",
    params=list(as.integer(user_id),now,manifest$bundle_id[1],assessment$decision,assessment$review_id))
  # Foreign-key failures must be raised before DBI commits the transaction.
  violations <- DBI::dbGetQuery(conn, "PRAGMA foreign_key_check")
  if (nrow(violations)) stop("Returned metadata would break database references; acceptance rolled back.")
}
accept_dataset_review_return <- function(path, base_path, user_id, decision, expected_hash) {
  if (!non_empty_scalar(decision)) stop("Record the curator's acceptance decision.")
  if (!non_empty_scalar(expected_hash)) stop("Inspect the return before accepting it.")
  # Process a private copy so replacing the upload after preview cannot change
  # the accepted bytes. The preview hash binds approval to this exact return.
  private <- tempfile("review-accept-", fileext=".zip")
  on.exit(unlink(private), add=TRUE)
  if (!file.copy(path,private) || !identical(digest::digest(file=private,algo="sha256"), expected_hash))
    stop("The returned bundle changed after inspection. Inspect it again.")
  assessment <- inspect_dataset_review_return(private, base_path)
  if (!assessment$can_accept) stop(paste(assessment$conflicts, collapse=" "))
  assessment$user_id <- user_id; assessment$decision <- trimws(decision)
  assessment$backup_path <- file.path(review_directory(base_path, assessment$review_id), paste0("before-accept-",new_uuid(),".sqlite"))
  success <- FALSE
  on.exit(if (!success) unlink(assessment$backup_path), add=TRUE)
  result <- import_dataset_transfer_bundle(private, base_path, user_id, .review=assessment)
  success <- TRUE
  result$backup_path <- assessment$backup_path
  result
}
cancel_dataset_review <- function(review_id, base_path, user_id, reason) {
  if (!non_empty_scalar(reason)) stop("Record why this review is cancelled.")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  review_require_user(conn,user_id,admin=TRUE)
  request <- review_request(conn,review_id)
  if (request$role[1] != "master" || request$status[1] != "issued") stop("Only an issued master review can be cancelled.")
  count <- DBI::dbExecute(conn,"UPDATE dataset_review_exchange_tbl SET status='cancelled', decision_note=? WHERE review_id=? AND status='issued'", params=list(reason,review_id))
  if (count != 1L) stop("The review changed while cancelling; refresh its status.")
  invisible(TRUE)
}
