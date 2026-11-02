# A derivation baseline is private: it may contain the old in-house wording.
# It participates in local snapshots/rollback, but never public transport.
public_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", dataframe = "rows"))
public_inventory <- function(path) jsonlite::fromJSON(public_json(release_inventory(path)), simplifyVector = TRUE)
public_decode <- function(x) jsonlite::fromJSON(x, simplifyVector = TRUE)
public_entry_fields <- function() c("title", "abstract", "purpose", "keyword_object", "keyword_theme",
  "keyword_place", "keyword_temporal", "keyword_discipline", "keyword_geo", "citation_date", "geographic_extent_name",
  "temporal_coverage_start_date", "temporal_coverage_end_date", "metadata_resource_scope_code",
  "metadata_language_code", "metadata_character_encoding", "resource_language_code", "resource_character_encoding",
  "access_constraints", "use_constraints", "use_limitations", "embargo_status", "embargo_end_date")
public_groups <- function() list(
  authority_tbl = c("person_id", "author_order", "role"),
  dataset_organisation_tbl = c("organisation_id", "role"),
  dataset_citation_date_tbl = c("date_value", "date_type_code", "date_precision", "sort_order"),
  dataset_topic_category_tbl = "topic_category_code",
  dataset_temporal_extent_tbl = c("extent_label", "start_value", "end_value", "start_precision", "end_precision", "start_indeterminate", "end_indeterminate", "sort_order"),
  dataset_license_tbl = c("license_template_id", "license_identifier", "license_name", "license_description", "license_url",
    "license_holder", "access_level", "use_constraints", "sensitive_data", "rights_basis_code", "rights_holder_organisation_id",
    "access_level_code", "redistribution_permission_code", "required_attribution", "attribution_text", "sensitivity_handling_code",
    "embargo_status", "embargo_start_date", "embargo_end_date", "dissemination_constraint_code"))
public_attribute_fields <- function() c("data_field_alias", "data_field_desc", "data_field_units", "legend_field", "legend_field_notes")
public_data_relative <- function(entry) {
  root <- entry$dataset_path[1]; data <- entry$data_path[1]
  if (identical(root, data)) return("")
  if (is.na(data) || !startsWith(data, paste0(root, "/"))) return("data")
  substring(data, nchar(root) + 2L)
}
public_file_path <- function(relative, data_relative) if (nzchar(data_relative)) paste0(data_relative, "/", relative) else relative
public_attr_key <- function(path, layer, field, property) paste("attribute", path, layer, field, property, sep = "\034")
public_metadata <- function(payload, fields, source = FALSE) {
  out <- list()
  for (p in names(fields)) for (l in names(fields[[p]])) for (field in fields[[p]][[l]]) for (property in public_attribute_fields())
    out[[public_attr_key(p, l, field, property)]] <- "null"
  for (f in intersect(public_entry_fields(), names(payload$entry_tbl))) out[[paste0("entry:", f)]] <- public_json(payload$entry_tbl[[f]][1])
  if (source) out[["entry:title"]] <- public_json(paste(metadata_text(payload$entry_tbl$title), "- Public"))
  for (t in names(public_groups())) {
    rows <- payload$scoped[[t]]
    if (is.null(rows) || !nrow(rows)) next
    rows <- rows[, intersect(public_groups()[[t]], names(rows)), drop = FALSE]
    if (nrow(rows) > 1L) rows <- rows[do.call(order, lapply(rows, as.character)), , drop = FALSE]
    out[[paste0("table:", t)]] <- public_json(rows)
  }
  attrs <- payload$scoped$dataset_attribute_tbl; files <- payload$scoped$data_file_tbl
  if (!is.null(attrs) && nrow(attrs) && !is.null(files) && nrow(files)) for (i in seq_len(nrow(attrs))) {
    a <- attrs[i, , drop = FALSE]; file <- files[files$id == a$data_file_id[1], , drop = FALSE]
    if (nrow(file) != 1L) next
    path <- public_file_path(file$data_file_rel_path_full[1], public_data_relative(payload$entry_tbl))
    layer <- metadata_text(a$layer_name, "table")
    if (!a$data_field_name[1] %in% fields[[path]][[layer]]) next
    for (f in intersect(public_attribute_fields(), names(a)))
      out[[public_attr_key(path, layer, a$data_field_name[1], f)]] <- public_json(a[[f]][1])
  }
  policy <- payload$scoped$dataset_public_metadata_policy_tbl
  if (source && !is.null(policy) && nrow(policy)) out[intersect(names(out), unlist(public_decode(policy$excluded_json[1])))] <- NULL
  out
}
public_label <- function(key) {
  labels <- c("table:authority_tbl" = "Authors", "table:dataset_organisation_tbl" = "Owning organisations and roles",
    "table:dataset_citation_date_tbl" = "Citation dates", "table:dataset_topic_category_tbl" = "Topic categories",
    "table:dataset_temporal_extent_tbl" = "Temporal coverage", "table:dataset_license_tbl" = "Rights and licence",
    "entry:keyword_object" = "Theme / subject keywords", "entry:keyword_discipline" = "Knowledge domain",
    "entry:keyword_geo" = "Spatial context", "entry:geographic_extent_name" = "Geographic description")
  if (key %in% names(labels)) return(unname(labels[[key]]))
  if (startsWith(key, "attribute\034")) {
    parts <- strsplit(key, "\034", fixed = TRUE)[[1]]
    properties <- c(data_field_alias = "Alias", data_field_desc = "Definition", data_field_units = "Units", legend_field = "Legend indicator", legend_field_notes = "Legend explanation")
    return(paste(c(parts[2:4], properties[[parts[5]]]), collapse = " / "))
  }
  tools::toTitleCase(gsub("_", " ", sub("^entry:", "", key), fixed = TRUE))
}
public_rights_key <- function(key) grepl("license_tbl$|^entry:(access_constraints|use_constraints|use_limitations|embargo_)", key)

#' @export
#' @noRd
set_public_metadata_defaults <- function(dataset_id, base_path, user_id, excluded_fields = character(),
                                        organisation_id = NULL, person_id = NULL) {
  lock <- release_lock(base_path, dataset_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_edition_schema(conn)
  DBI::dbWithTransaction(conn, {
    edition_editable(conn, dataset_id, base_path, user_id)
    valid <- c(paste0("entry:", public_entry_fields()), paste0("table:", names(public_groups())))
    if (any(!excluded_fields %in% valid) || any(public_rights_key(excluded_fields))) stop("Rights and restrictions cannot be excluded from public defaults.")
    DBI::dbExecute(conn, "DELETE FROM dataset_public_metadata_policy_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))
    transfer_insert_row(conn, "dataset_public_metadata_policy_tbl", list(dataset_id = as.integer(dataset_id), excluded_json = public_json(as.list(excluded_fields))))
    if (!is.null(organisation_id)) {
      if (!organisation_id %in% DBI::dbReadTable(conn, "organisation_tbl")$id) stop("Choose an organisation.")
      if (!is.null(person_id) && !is.na(person_id)) {
        people <- DBI::dbGetQuery(conn, "SELECT id FROM persons_tbl WHERE organisation_id=?", params = list(as.integer(organisation_id)))
        if (!person_id %in% people$id) stop("Choose a contact belonging to that organisation.")
      }
      DBI::dbExecute(conn, "DELETE FROM organisation_public_contact_tbl WHERE organisation_id=?", params = list(as.integer(organisation_id)))
      transfer_insert_row(conn, "organisation_public_contact_tbl", list(organisation_id = as.integer(organisation_id), person_id = if (is.null(person_id)) NA_integer_ else as.integer(person_id)), drop = character())
    }
  })
  invisible(TRUE)
}

#' @export
#' @noRd
public_edition_choices <- function(release_id, base_path, dataset_id = NULL) {
  available <- public_draft_files(release_id, base_path)
  fields <- public_draft_fields(release_id, base_path, available$path)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn))
  recipe <- NULL
  if (!is.null(dataset_id)) {
    row <- edition_row(conn, dataset_id)
    if (!nrow(row) || row$edition[1] != "public") stop("Select a public edition to update.")
    source <- edition_source(conn, release_id, base_path)
    if (row$family_uuid[1] != source$edition$family_uuid[1]) stop("Choose a source in the same dataset family.")
    if (metadata_present(row$selection_json)) recipe <- public_decode(row$selection_json[1])
    if (is.null(recipe)) stop("This draft has no saved selection recipe. Create a public edition with the guided workflow first.")
  }
  # Data defaults are visible and editable; supporting documents remain opt-in.
  selected <- if (is.null(recipe)) available$path[tolower(tools::file_ext(available$path)) %in%
    c("csv", "tsv", "gpkg", "geojson", "shp", "shx", "dbf", "prj", "cpg", "tif", "tiff", "nc", "parquet")] else intersect(unlist(recipe$files), available$path)
  chosen <- fields[selected]
  if (!is.null(recipe)) for (p in names(chosen)) {
    old <- recipe$fields[[p]]
    chosen[[p]] <- chosen[[p]][intersect(names(chosen[[p]]), names(old))]
    for (l in names(chosen[[p]])) chosen[[p]][[l]] <- intersect(unlist(old[[l]]), chosen[[p]][[l]])
  }
  chosen <- chosen[lengths(fields[names(chosen)]) > 0L]
  old_files <- if (is.null(recipe)) character() else unlist(recipe$files)
  changes <- c(if (!is.null(recipe) && length(setdiff(available$path, old_files))) paste("New source file:", setdiff(available$path, old_files)),
    if (length(setdiff(old_files, available$path))) paste("Removed source file:", setdiff(old_files, available$path)))
  if (!is.null(recipe)) for (p in intersect(old_files, names(fields))) for (l in union(names(fields[[p]]), names(recipe$fields[[p]]))) {
    added <- setdiff(fields[[p]][[l]], unlist(recipe$fields[[p]][[l]]))
    removed <- setdiff(unlist(recipe$fields[[p]][[l]]), fields[[p]][[l]])
    if (length(added)) changes <- c(changes, paste("New attributes:", p, l, paste(added, collapse = ", ")))
    if (length(removed)) changes <- c(changes, paste("Removed attributes:", p, l, paste(removed, collapse = ", ")))
  }
  list(files = available, fields = fields, selected_files = selected, selected_fields = chosen, changes = changes)
}

public_fingerprint <- function(conn, id, pond, user) review_hash(list(
  metadata = review_fingerprint(release_payload(conn, id, pond, user)), files = release_inventory(release_entry(conn, id)$dataset_path[1])))

#' @export
#' @noRd
preview_public_dataset_draft <- function(release_id, base_path, user_id, files, fields = list(),
                                         dataset_id = NULL) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_require_user(conn, user_id)
  source <- edition_source(conn, release_id, base_path)
  available <- public_draft_files(release_id, base_path)
  if (!is.character(files) || !length(files) || anyNA(files) || anyDuplicated(files) || any(!files %in% available$path)) stop("Select at least one eligible file from the frozen release.")
  if (!is.list(fields) || (length(fields) && (is.null(names(fields)) || anyDuplicated(names(fields)) || any(!names(fields) %in% files)))) stop("Attribute selections must refer to selected files.")
  stage <- tempfile("public-preview-"); dir.create(stage, mode = "0700")
  ok <- FALSE; on.exit(if (!ok) unlink(stage, recursive = TRUE), add = TRUE)
  extracted <- file.path(stage, "source"); verify_dataset_release(release_id, base_path, extracted)
  snapshot <- release_read_payload(source$paths$snapshot)
  incoming <- list(); output <- file.path(stage, "output"); dir.create(output)
  for (p in files) {
    available_fields <- edition_file_fields(file.path(extracted, "dataset", p))
    if (length(available_fields) && is.null(fields[[p]])) fields[[p]] <- available_fields
    target <- file.path(output, p); dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    edition_project_file(file.path(extracted, "dataset", p), target, fields[[p]])
  }
  incoming <- public_metadata(snapshot, fields, TRUE)
  current <- baseline <- list(); previous <- NULL; fingerprint <- NULL; conflicts <- list(); retained <- character()
  if (!is.null(dataset_id)) {
    entry <- edition_editable(conn, dataset_id, base_path, user_id)
    edition <- edition_row(conn, dataset_id)
    if (!nrow(edition) || edition$edition[1] != "public" || edition$family_uuid[1] != source$edition$family_uuid[1]) stop("Choose a public draft in the same dataset family.")
    saved <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_public_derivation_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))
    if (!nrow(saved)) stop("This public draft has no comparison baseline. Create it with the guided workflow first.")
    baseline <- as.list(public_decode(saved$baseline_json[1]))
    previous <- release_payload(conn, dataset_id, base_path, user_id)
    current <- public_metadata(previous, fields)
    fingerprint <- public_fingerprint(conn, dataset_id, base_path, user_id)
    managed <- public_decode(saved$managed_files_json[1])
    inventory <- public_inventory(entry$dataset_path[1])
    # Public files added outside the generator remain in the next draft. Managed
    # bytes edited since generation require a choice before they can be replaced.
    for (p in inventory$path) {
      if (grepl("(^|/)\\.", p) || tolower(basename(p)) %in% c("metadata.pdf", "metadata.qmd", "citation.txt")) next
      old_hash <- if (is.data.frame(managed)) managed$sha256[match(p, managed$path)] else NA_character_
      now_hash <- inventory$sha256[match(p, inventory$path)]
      changed <- length(old_hash) && !is.na(old_hash) && !identical(old_hash, now_hash)
      unmanaged <- !length(old_hash) || is.na(old_hash)
      source_unchanged <- p %in% files && file.exists(file.path(output, p)) && identical(release_sha(file.path(output, p)), old_hash)
      if ((changed && !source_unchanged) || (unmanaged && p %in% files)) {
        conflicts[[paste0("file:", p)]] <- list(kind = "file", label = p, current = "Keep public file", source = if (p %in% files) "Use new source selection" else "Remove file")
      }
      if ((unmanaged && !p %in% files) || changed) {
        dest <- file.path(output, p); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        if (!file.copy(file.path(entry$dataset_path[1], p), dest, overwrite = TRUE)) stop("Could not preserve public file.")
        retained <- c(retained, p)
      }
    }
  }
  merged <- incoming; kept <- character()
  for (key in union(names(incoming), names(current))) {
    before <- baseline[[key]]; now <- current[[key]]; next_value <- incoming[[key]]
    # Previously excluded/removed attributes must never be resurrected.
    if (startsWith(key, "attribute\034") && is.null(next_value)) next
    if (!is.null(dataset_id)) {
      source_changed <- !identical(before, next_value)
      public_changed <- !identical(now, before)
      if (!source_changed || (public_changed && !identical(now, next_value))) {
        merged[key] <- list(now)
        if (public_changed) kept <- c(kept, key)
      }
      if (source_changed && public_changed && !identical(now, next_value)) conflicts[[key]] <- list(kind = "metadata", label = public_label(key),
        previous = before, current = now, source = next_value)
    }
  }
  ok <- TRUE
  structure(list(base_path = base_path, user_id = user_id, dataset_id = dataset_id, release_id = release_id,
    source = source, source_payload = snapshot, previous = previous, fingerprint = fingerprint,
    stage = stage, output = output, files = files, fields = fields, incoming = incoming, metadata = merged,
    conflicts = conflicts, kept = kept, retained_files = retained,
    selected_hash = review_hash(release_inventory(output)),
    review_items = c("Review the public wording, rights and licence before approval.",
      "Check temporal coverage and geographic description against the selected data.",
      "Refresh file-specific quality assessments and manually entered file extents after updating data.",
      "Geometry simplification and row filtering can be done in the public draft; review coverage afterwards.")), class = "datapond_public_preview")
}

public_apply_metadata <- function(conn, id, values) {
  for (key in names(values)) {
    value <- values[[key]]
    if (startsWith(key, "entry:")) {
      f <- substring(key, 7)
      if (!f %in% DBI::dbListFields(conn, "entry_tbl")) next
      decoded <- if (is.null(value)) NA_character_ else public_decode(value)
      if (is.null(decoded)) decoded <- NA_character_
      DBI::dbExecute(conn, paste0("UPDATE entry_tbl SET ", DBI::dbQuoteIdentifier(conn, f), "=? WHERE id=?"), params = list(decoded, as.integer(id)))
    } else if (startsWith(key, "table:")) {
      table <- substring(key, 7)
      DBI::dbExecute(conn, paste0("DELETE FROM ", table, " WHERE dataset_id=?"), params = list(as.integer(id)))
      rows <- if (is.null(value)) NULL else public_decode(value)
      if (!is.null(rows) && is.data.frame(rows) && nrow(rows)) for (i in seq_len(nrow(rows))) transfer_insert_row(conn, table, c(as.list(rows[i, , drop = FALSE]), list(dataset_id = id)))
    }
  }
}

public_stage_metadata <- function(plan, title, version, method, decisions, edits) {
  if (!metadata_present(title) || !grepl("^[0-9]+[.][0-9]+[.][0-9]+$", version)) stop("Supply a public title and a major.minor.patch version.")
  if (!metadata_present(method)) stop("Describe the public preparation method.")
  if (length(setdiff(names(plan$conflicts), names(decisions)))) stop("Resolve each highlighted conflict before creating the draft.")
  if (any(!unlist(decisions) %in% c("current", "source"))) stop("Choose current public or new source for each conflict.")
  if (any(!names(edits) %in% paste0("entry:", public_entry_fields())) || any(public_rights_key(names(edits)))) stop("Edit descriptive fields here; review rights in the licensing form.")
  if (!identical(review_hash(release_inventory(plan$output)), plan$selected_hash)) stop("Preview files changed; prepare a new preview.")
  resolved <- tempfile("resolved-", tmpdir = plan$stage); dir.create(resolved)
  for (p in public_inventory(plan$output)$path) {
    dest <- file.path(resolved, p); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(plan$output, p), dest)) stop("Could not stage preview files.")
  }
  plan$output <- resolved
  values <- plan$metadata
  for (key in names(plan$conflicts)) {
    conflict <- plan$conflicts[[key]]; decision <- decisions[[key]]
    if (conflict$kind == "metadata") values[key] <- list(conflict[[decision]])
    else {
      p <- substring(key, 6); dest <- file.path(plan$output, p)
      if (decision == "current") {
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        if (!file.copy(file.path(plan$previous$entry_tbl$dataset_path[1], p), dest, overwrite = TRUE)) stop("Could not retain public file.")
      } else {
        if (file.exists(dest)) unlink(dest)
        if (p %in% plan$files) {
          dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
          edition_project_file(file.path(plan$stage, "source", "dataset", p), dest, plan$fields[[p]])
        }
      }
    }
  }
  for (key in names(edits)) values[[key]] <- public_json(edits[[key]])
  values[["entry:title"]] <- public_json(title)
  # Build metadata and scan in a private copy. No live record is partly populated.
  pond <- tempfile("pond-", tmpdir = plan$stage); dir.create(pond)
  ensure_pond_structure(pond)
  original <- get_conn(plan$base_path); on.exit(DBI::dbDisconnect(original), add = TRUE)
  target_conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(pond))
  RSQLite::sqliteCopyDatabase(original, target_conn); DBI::dbDisconnect(target_conn)
  conn <- get_conn(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  data_relative <- public_data_relative(plan$source_payload$entry_tbl)
  draft <- file.path(pond, "dev", "public_preview"); dir.create(dirname(draft), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(plan$output, draft)) stop("Could not stage the public draft.")
  dir.create(file.path(draft, data_relative), recursive = TRUE, showWarnings = FALSE)
  if (is.null(plan$dataset_id)) {
    id <- transfer_insert_row(conn, "entry_tbl", list(uuid = uuid::UUIDgenerate(), metadata_uuid = uuid::UUIDgenerate(), title = title,
      version = version, dataset_path = draft, data_path = file.path(draft, data_relative), lifecycle_status = "draft", folder_locked = 0L,
      data_pond_id = ensure_data_pond_id(plan$base_path, conn = original), created_by_user_id = plan$user_id))
  } else {
    id <- plan$dataset_id
    DBI::dbWithTransaction(conn, release_restore_metadata(conn, plan$previous, release_entry(conn, id), draft))
    DBI::dbExecute(conn, "UPDATE entry_tbl SET data_path=?,version=? WHERE id=?", params = list(file.path(draft, data_relative), version, id))
  }
  public_apply_metadata(conn, id, values)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET version=? WHERE id=?", params = list(version, id))
  if (is.null(plan$dataset_id)) {
    owners <- DBI::dbGetQuery(conn, "SELECT organisation_id FROM dataset_organisation_tbl WHERE dataset_id=? AND role='owner' ORDER BY id", params = list(id))
    if (nrow(owners)) {
      org <- owners$organisation_id[1]
      nominated <- DBI::dbGetQuery(conn, "SELECT person_id FROM organisation_public_contact_tbl WHERE organisation_id=?", params = list(org))
      person <- if (nrow(nominated)) nominated$person_id[1] else NA_integer_
      transfer_insert_row(conn, "poc_tbl", list(dataset_id = id, organisation_id = org, person_id = person, role = "pointOfContact"))
      transfer_insert_row(conn, "metadata_responsibility_tbl", list(dataset_id = id, organisation_id = org, person_id = person, role_code = "pointOfContact", is_primary = 1L))
    }
  }
  source <- plan$source$paths$record
  DBI::dbExecute(conn, "DELETE FROM dataset_edition_tbl WHERE dataset_id=?", params = list(id))
  transfer_insert_row(conn, "dataset_edition_tbl", list(dataset_id = id, family_uuid = plan$source$edition$family_uuid[1], edition = "public",
    source_dataset_uuid = source$dataset_uuid[1], source_release_id = plan$release_id, source_version = source$version[1],
    source_zip_sha256 = source$zip_sha256[1], preparation_method = method,
    selection_json = public_json(list(files = as.list(plan$files), fields = lapply(plan$fields, function(x) lapply(x, as.list))))))
  if (is.null(plan$dataset_id)) transfer_insert_row(conn, "dataset_lineage_tbl", list(dataset_id = id, lineage_source_mode = "form",
    lineage_statement = paste("Derived from in-house dataset", source$dataset_uuid[1], "version", source$version[1], "release", plan$release_id, ".", method)))
  else transfer_insert_row(conn, "dataset_lineage_process_step_tbl", list(dataset_id = id,
    description = paste("Updated from in-house release", plan$release_id, "version", source$version[1], ".", method)))
  # Rebuild the inventory against actual output bytes, never copy private statistics.
  DBI::dbExecute(conn, "DELETE FROM dataset_quality_result_tbl WHERE dataset_id=? AND (data_file_id IS NOT NULL OR attribute_id IS NOT NULL)", params = list(id))
  DBI::dbExecute(conn, "DELETE FROM dataset_manual_spatial_extent_tbl WHERE dataset_id=? AND data_file_id IS NOT NULL", params = list(id))
  for (t in c("data_profile_run_tbl", "dataset_attribute_tbl", "data_file_spatial_metadata_tbl", "dataset_spatial_summary_tbl", "data_file_tbl", "data_file_scan_tbl"))
    DBI::dbExecute(conn, paste0("DELETE FROM ", t, " WHERE dataset_id=?"), params = list(id))
  data_dir <- file.path(draft, data_relative)
  scan <- scan_dataset_files(data_dir)
  if ("message" %in% names(scan)) stop(scan$message[1])
  save_data_file_scan(id, scan, pond, data_dir = data_dir)
  files <- get_data_files_for_dataset(id, pond)
  attributes <- 0L
  for (i in seq_len(nrow(files))) {
    f <- files[i, , drop = FALSE]; p <- public_file_path(f$data_file_rel_path_full[1], data_relative)
    actual <- edition_file_fields(file.path(draft, p))
    for (layer in names(actual)) for (field in actual[[layer]]) {
      a <- list(dataset_id = id, data_file_id = f$id[1], data_pond_id = f$data_pond_id[1],
        data_file_rel_path_full = f$data_file_rel_path_full[1], data_file_name = f$file_name[1], layer_name = if (layer == "table") NA_character_ else layer,
        data_field_name = field, attribute_order = match(field, actual[[layer]]))
      for (property in public_attribute_fields()) {
        value <- values[[public_attr_key(p, layer, field, property)]]
        if (!is.null(value)) a[[property]] <- public_decode(value) %||% NA_character_
      }
      transfer_insert_row(conn, "dataset_attribute_tbl", a); attributes <- attributes + 1L
    }
  }
  spatial <- extract_spatial_metadata(scan, data_dir)
  if (!"message" %in% names(spatial) && nrow(spatial)) save_data_file_spatial_metadata(id, spatial, pond, plan$user_id)
  scan_supporting_documents(id, draft, pond)
  entry <- release_entry(conn, id)
  citation <- generate_citation(as.list(entry[1, , drop = FALSE]), get_dataset_authors(id, pond), get_dataset_org_links(id, pond))
  ensure_columns(conn, "entry_tbl", "citation")
  DBI::dbExecute(conn, "UPDATE entry_tbl SET citation=? WHERE id=?", params = list(citation, id))
  applied <- public_metadata(release_payload(conn, id, pond, plan$user_id), plan$fields)
  managed <- public_inventory(draft); managed <- managed[managed$path %in% plan$files, , drop = FALSE]
  # Retained edited files remain based on the old generated hash, so future updates
  # continue to ask before replacing them.
  if (!is.null(plan$previous)) {
    old <- public_decode(plan$previous$scoped$dataset_public_derivation_tbl$managed_files_json[1])
    for (key in names(plan$conflicts)) if (startsWith(key, "file:") && decisions[[key]] == "current") {
      p <- substring(key, 6); index <- match(p, managed$path); old_index <- match(p, old$path)
      if (!is.na(index) && !is.na(old_index)) managed$sha256[index] <- old$sha256[old_index]
    }
  }
  DBI::dbExecute(conn, "DELETE FROM dataset_public_derivation_tbl WHERE dataset_id=?", params = list(id))
  transfer_insert_row(conn, "dataset_public_derivation_tbl", list(dataset_id = id, baseline_json = public_json(plan$incoming),
    applied_json = public_json(applied), managed_files_json = public_json(managed)))
  list(payload = release_payload(conn, id, pond, plan$user_id), draft = draft, attributes = attributes, files = nrow(files))
}

#' @export
#' @noRd
apply_public_dataset_draft <- function(preview, title, version, preparation_method, decisions = list(), edits = list()) {
  plan <- preview
  if (!inherits(plan, "datapond_public_preview")) stop("Prepare a public draft preview first.")
  conn <- get_conn(plan$base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_require_user(conn, plan$user_id); edition_source(conn, plan$release_id, plan$base_path)
  if (!is.null(plan$dataset_id) && !identical(public_fingerprint(conn, plan$dataset_id, plan$base_path, plan$user_id), plan$fingerprint)) stop("The public draft changed after preview. Preview it again.")
  staged <- public_stage_metadata(plan, title, version, preparation_method, decisions, edits)
  if (is.null(plan$dataset_id)) {
    result <- edition_create_raw(plan$release_id, plan$base_path, plan$user_id, title, version, plan$files, plan$fields, preparation_method, .payload = staged$payload)
  } else result <- public_commit_update(plan, staged, preparation_method)
  result$summary <- paste(staged$files, "data files scanned;", staged$attributes, "retained attributes registered;",
    length(plan$kept), "public metadata edits preserved;", length(plan$retained_files), "public files retained.")
  result$review_items <- plan$review_items
  unlink(plan$stage, recursive = TRUE)
  result
}

#' @export
#' @noRd
create_public_dataset_draft <- function(release_id, base_path, user_id, title, version, files,
                                        fields = list(), preparation_method) {
  plan <- preview_public_dataset_draft(release_id, base_path, user_id, files, fields)
  on.exit(unlink(plan$stage, recursive = TRUE), add = TRUE)
  apply_public_dataset_draft(plan, title, version, preparation_method)
}

public_commit_update <- function(plan, staged, note) {
  pond <- plan$base_path; id <- plan$dataset_id
  lock <- release_lock(pond, id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  conn <- get_conn(pond); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  current <- edition_editable(conn, id, pond, plan$user_id)
  target <- release_safe_directory(pond, file.path(pond, "dev", paste0("public_", substr(uuid::UUIDgenerate(), 1, 8), "__", current$uuid[1])))
  row <- data.frame(dataset_id = id, release_id = plan$release_id)
  op <- release_operation(pond, row, "public_update", current$dataset_path[1], target, plan$user_id, note)
  committed <- switched <- FALSE
  tryCatch({
    DBI::dbWithTransaction(conn, {
      DBI::dbExecute(conn, "UPDATE entry_tbl SET id=id WHERE id=?", params = list(id))
      if (!identical(public_fingerprint(conn, id, pond, plan$user_id), plan$fingerprint)) stop("The public draft changed after preview. Preview it again.")
      edition_source(conn, plan$release_id, pond)
      prior <- release_payload(conn, id, pond, plan$user_id)
      write_dataset_transfer_database(prior, file.path(op$directory, "prior-metadata.sqlite"))
      files <- release_inventory(op$old_path)
      release_json(files, file.path(op$directory, "prior-files.json"))
      release_archive_tool("pack", op$old_path, file.path(op$directory, "prior-files.json"), file.path(op$directory, "prior-working.zip"))
      op$prior_snapshot_sha256 <- release_sha(file.path(op$directory, "prior-metadata.sqlite"))
      op$prior_manifest_sha256 <- release_sha(file.path(op$directory, "prior-files.json"))
      op$prior_zip_sha256 <- release_sha(file.path(op$directory, "prior-working.zip"))
      release_operation_write(op, file.path(op$directory, "operation.json"))
      if (!file.rename(op$old_path, op$backup)) stop("Could not preserve the previous public folder.")
      switched <- TRUE
      if (!file.rename(staged$draft, target)) stop("Could not place the updated public draft.")
      release_after_switch()
      release_restore_metadata(conn, staged$payload, current, target)
      write_dataset_marker(target, current$uuid[1], current$data_pond_id[1])
      if (!identical(review_hash(release_inventory(op$backup)), review_hash(files))) stop("Previous public files changed during update.")
      release_event(conn, id, plan$release_id, op$operation_id, "update_public_draft", plan$user_id, note)
      release_before_commit()
    })
    committed <- TRUE
    op$phase <- "committed"; release_operation_write(op, file.path(op$directory, "operation.json"))
    release_cleanup_operation(op, pond)
  }, error = function(e) {
    if (!committed) {
      if (switched) release_rollback_files(op) else {
        op$phase <- "rolled_back"; release_operation_write(op, file.path(op$directory, "operation.json"))
      }
    }
    stop(conditionMessage(e), call. = FALSE)
  })
  list(dataset_id = id, dataset_uuid = current$uuid[1], dataset_path = target, source_release_id = plan$release_id, checkpoint = op$operation_id)
}
