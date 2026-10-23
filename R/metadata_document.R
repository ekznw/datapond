# Human-readable metadata uses a read-only saved snapshot. It never executes a
# development document, migrates a pond, or treats PDF creation as release approval.
metadata_text <- function(x, default = "Not recorded") {
  if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) return(default)
  trimws(enc2utf8(as.character(x[1])))
}

metadata_present <- function(x) nzchar(metadata_text(x, ""))

# Escape every Markdown control character, including raw HTML/TeX and Quarto
# shortcodes. Saved prose is data, never executable document markup.
metadata_markdown <- function(x) {
  x <- metadata_text(x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("[\u2010-\u2015\u2212]", "-", x)
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  special <- c("\\", "`", "*", "_", "{", "}", "[", "]", "<", ">", "(", ")", "#", "+", "-", ".", "!", "|", "$", "~", "^", "@")
  escaped <- ifelse(chars %in% special, paste0("\\", chars), chars)
  # Permit line breaks in long filenames/identifiers without altering their
  # visible text. The added raw command is fixed, never supplied by metadata.
  escaped[chars %in% c("_", "/")] <- paste0(escaped[chars %in% c("_", "/")], "\\allowbreak{}")
  paste0(escaped, collapse = "")
}

metadata_public_path <- function(x) {
  x <- metadata_text(x, "")
  if (!nzchar(x)) return("")
  x <- gsub("\\", "/", x, fixed = TRUE)
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
  if (grepl("^[/~]|^[A-Za-z][A-Za-z0-9+.-]*:|(^|/)\\.\\.(/|$)", x))
    return("Local reference omitted; record a dataset-relative path")
  x
}

collect_metadata_document <- function(dataset_id, base_path) {
  dataset_id <- suppressWarnings(as.integer(dataset_id))
  if (length(dataset_id) != 1L || is.na(dataset_id)) stop("Select a saved dataset.", call. = FALSE)
  conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(base_path), flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbBegin(conn)
  on.exit(DBI::dbRollback(conn), add = TRUE, after = FALSE)
  read <- function(table, scoped = TRUE) {
    if (!DBI::dbExistsTable(conn, table)) return(data.frame())
    sql <- paste("SELECT * FROM", DBI::dbQuoteIdentifier(conn, table))
    if (scoped) DBI::dbGetQuery(conn, paste(sql, "WHERE dataset_id = ? ORDER BY id"), params = list(dataset_id))
    else DBI::dbGetQuery(conn, sql)
  }
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id = ?", params = list(dataset_id))
  if (nrow(record) != 1L) stop("Select a saved dataset.", call. = FALSE)
  active <- function(rows) {
    if ("active" %in% names(rows)) rows <- rows[is.na(rows$active) | rows$active != 0L, , drop = FALSE]
    rows
  }
  tables <- c("authority_tbl", "dataset_organisation_tbl", "poc_tbl", "metadata_responsibility_tbl",
    "dataset_license_tbl", "dataset_lineage_tbl", "dataset_lineage_source_tbl", "dataset_lineage_process_step_tbl",
    "dataset_quality_result_tbl", "dataset_temporal_extent_tbl", "dataset_citation_date_tbl",
    "dataset_distribution_format_tbl", "dataset_online_resource_tbl", "dataset_spatial_summary_tbl", "dataset_vertical_extent_tbl",
    "dataset_manual_spatial_extent_tbl", "data_file_spatial_metadata_tbl", "metadata_review_tbl")
  values <- stats::setNames(lapply(tables, read), tables)
  values$files <- active(read("data_file_tbl"))
  values$documents <- active(read("supporting_document_tbl"))
  attributes <- read("dataset_attribute_tbl")
  # Attributes must resolve to one active file in this dataset. Do not attach
  # orphan rows or rows linked to inactive files to a released file by guesswork.
  keep <- vapply(seq_len(nrow(attributes)), function(i) {
    id <- attributes$data_file_id[i]
    hits <- if (length(id) && !is.na(id)) which(values$files$id == id) else
      which(values$files$data_file_rel_path_full == metadata_text(attributes$data_file_rel_path_full[i], ""))
    length(hits) == 1L
  }, logical(1))
  values$attributes <- attributes[keep, , drop = FALSE]
  for (i in seq_len(nrow(values$attributes))) {
    if (!metadata_present(values$attributes$data_file_rel_path_full[i])) {
      matched <- which(values$files$id == values$attributes$data_file_id[i])
      if (length(matched) == 1L)
        values$attributes$data_file_rel_path_full[i] <- values$files$data_file_rel_path_full[matched]
    }
  }
  values$unlinked_attribute_count <- sum(!keep)
  values$organisations <- read("organisation_tbl", FALSE)
  values$people <- read("persons_tbl", FALSE)
  root <- metadata_text(record$dataset_path, "")
  values$development_documents <- character()
  if (nzchar(root) && dir.exists(root)) {
    paths <- list.files(root, pattern = "\\.(qmd|rmd)$", ignore.case = TRUE, full.names = TRUE)
    # No symlink traversal or recursive scan into archives/scratch directories.
    paths <- paths[!dir.exists(paths) & !nzchar(Sys.readlink(paths))]
    values$development_documents <- sort(basename(paths))
  }
  values$record <- record
  values$generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  values
}

metadata_document_body <- function(snapshot) {
  s <- snapshot; r <- s$record; lines <- character()
  add <- function(...) lines <<- c(lines, ..., "")
  heading <- function(title) add("```{=latex}", "\\Needspace{7\\baselineskip}", "```", "", paste0("# ", title))
  text <- function(value) add(metadata_markdown(value))
  field <- function(label, value, required = FALSE) {
    if (required || metadata_present(value)) add(paste0("**", metadata_markdown(label), ":** ", metadata_markdown(value)))
  }
  block <- function(rows, fields, empty = "Not recorded.") {
    if (!nrow(rows)) { if (nzchar(empty)) text(empty); return(invisible(NULL)) }
    for (i in seq_len(nrow(rows))) {
      for (label in names(fields)) {
        value <- rows[[fields[[label]]]][i]
        if (grepl("_code$", fields[[label]]) && metadata_present(value)) value <- gsub("_", " ", value, fixed = TRUE)
        field(label, value)
      }
      if (i < nrow(rows)) add("---")
    }
  }
  person <- function(id) {
    row <- s$people[which(s$people$id == id), , drop = FALSE]
    if (!nrow(row)) return("")
    trimws(paste(metadata_text(row$name_first, ""), metadata_text(row$name_last, "")))
  }
  organisation <- function(id) {
    row <- s$organisations[which(s$organisations$id == id), , drop = FALSE]
    metadata_text(row$organisation_name_full, "")
  }
  table <- function(rows, fields, empty = "Not recorded.") {
    if (!nrow(rows)) { text(empty); return(invisible(NULL)) }
    add(paste0("| ", paste(names(fields), collapse = " | "), " |"),
        paste0("| ", paste(rep("---", length(fields)), collapse = " | "), " |"),
        vapply(seq_len(nrow(rows)), function(i) paste0("| ", paste(vapply(fields, function(f)
          metadata_markdown(rows[[f]][i]), character(1)), collapse = " | "), " |"), character(1)))
  }
  field("Dataset version", r$version, TRUE)
  field("Dataset identifier", r$uuid, TRUE)
  field("Release identifier", r$release_uuid)
  field("Release frozen", r$frozen_at)
  field("Document generated", s$generated_at, TRUE)
  text("This document describes saved metadata. Missing information is marked 'Not recorded'. It does not establish permission to distribute the data.")
  heading("Dataset overview")
  field("Description", r$abstract, TRUE)
  field("Purpose and intended use", r$purpose, TRUE)
  field("Credits", r$dataset_credit)
  field("Subject keywords", r$keyword_object)
  field("Topics", r$keyword_topic)
  field("Geographic keywords", r$keyword_geo)
  authors <- s$authority_tbl
  if (nrow(authors)) {
    if ("author_order" %in% names(authors)) authors <- authors[order(authors$author_order, authors$id), , drop = FALSE]
    field("Authors", paste(Filter(nzchar, vapply(authors$person_id, person, character(1))), collapse = "; "))
  }
  orgs <- s$dataset_organisation_tbl
  for (i in seq_len(nrow(orgs))) field(paste0("Organisation (", metadata_text(orgs$role[i]), ")"), organisation(orgs$organisation_id[i]))
  heading("Citation")
  text(r$citation)
  if (!nrow(s$dataset_citation_date_tbl)) field("Citation date", r$citation_date)
  block(s$dataset_citation_date_tbl, c("Date type" = "date_type_code", "Date" = "date_value"), empty = "No additional citation dates recorded.")
  heading("Coverage and spatial reference")
  field("Geographic area", r$geographic_extent_name, TRUE)
  if (nrow(s$dataset_temporal_extent_tbl)) block(s$dataset_temporal_extent_tbl,
    c("Period" = "extent_label", "Start" = "start_value", "Start qualifier" = "start_indeterminate",
      "End" = "end_value", "End qualifier" = "end_indeterminate", "Description" = "description"))
  else {
    field("Temporal coverage start", r$temporal_coverage_start_date, TRUE)
    field("Temporal coverage end", r$temporal_coverage_end_date, TRUE)
  }
  bounds <- s$dataset_spatial_summary_tbl
  if (nrow(bounds) && identical(metadata_text(bounds$bbox_status), "valid")) {
    field("Bounding coordinates reference", bounds$bbox_crs, TRUE)
    field("West / south / east / north", paste(vapply(c("xmin", "ymin", "xmax", "ymax"),
      function(k) metadata_text(bounds[[k]]), character(1)), collapse = " / "))
  } else field("Bounding coordinates", NULL, TRUE)
  field("Equivalent scale denominator", r$equivalent_scale_denominator)
  field("Scale guidance", r$creation_scale_note)
  block(s$dataset_vertical_extent_tbl, c("Vertical extent" = "extent_label", "Minimum" = "minimum_value",
    "Maximum" = "maximum_value", "Vertical units" = "unit_symbol", "Vertical reference" = "vertical_crs_name"), empty = "")
  spatial <- s$data_file_spatial_metadata_tbl
  if (nrow(spatial)) {
    keep <- vapply(seq_len(nrow(spatial)), function(i) {
      id <- spatial$data_file_id[i]
      if (!is.na(id)) id %in% s$files$id else spatial$data_file_rel_path_full[i] %in% s$files$data_file_rel_path_full
    }, logical(1))
    spatial <- spatial[keep, , drop = FALSE]
  }
  for (i in seq_len(nrow(spatial))) {
    row <- spatial[i, , drop = FALSE]
    field("File", metadata_public_path(row$data_file_rel_path_full))
    field("Layer", row$layer_name)
    field("Coordinate reference system", row$crs_auth_code, TRUE)
    field("Coordinate reference name", row$crs_name)
    field("Geometry", row$geometry_type)
    if (metadata_present(row$raster_res_x)) field("Cell size in native CRS units (x / y)",
      paste(metadata_text(row$raster_res_x), metadata_text(row$raster_res_y), sep = " / "))
  }
  manual <- s$dataset_manual_spatial_extent_tbl
  if (nrow(manual)) manual <- manual[!is.na(manual$active) & manual$active == 1L &
    !is.na(manual$include_in_dataset_bbox) & manual$include_in_dataset_bbox == 1L, , drop = FALSE]
  block(manual, c("Recorded extent" = "extent_label", "Coordinate reference system" = "crs_auth_code"), empty = "")
  heading("Data files")
  text(if (metadata_present(r$release_uuid))
    "Active files in the saved inventory. Paths are relative to the dataset's data folder in this release."
    else "Active files in the saved inventory, with paths relative to the dataset's data folder. Downloading this PDF does not package the data files.")
  files <- s$files
  if (nrow(files)) files$data_file_rel_path_full <- vapply(files$data_file_rel_path_full, metadata_public_path, character(1))
  table(files, c("Relative path" = "data_file_rel_path_full", "Format" = "data_file_format", "Description" = "data_file_description"))
  block(s$dataset_distribution_format_tbl, c("Distribution format" = "specification_title", "Format version" = "format_version"), empty = "")
  heading("Lineage and development records")
  lineage <- s$dataset_lineage_tbl
  field("How the data were created", lineage$lineage_statement, TRUE)
  field("Source data", lineage$source_description)
  field("Source datasets", lineage$source_datasets_text)
  field("Processing summary", lineage$process_description)
  field("Outputs", lineage$output_description)
  if (length(s$development_documents)) {
    add("## Development documents")
    text("The following development documents were found in the dataset root. They form supporting development records for the lineage. Consult them for methods, software, package versions and the processing environment. Their presence alone does not verify that they produced this dataset version.")
    for (p in s$development_documents) add(paste0("- ", metadata_markdown(p)))
  }
  docs <- s$documents
  if (nrow(docs)) docs$document_rel_path <- vapply(docs$document_rel_path, metadata_public_path, character(1))
  selected <- if ("include_in_lineage" %in% names(docs)) docs[which(docs$include_in_lineage == 1L), , drop = FALSE] else docs[FALSE, ]
  add("## Recorded lineage references")
  table(selected, c("Document" = "document_name", "Reference" = "document_rel_path"), "No documents explicitly selected as authoritative lineage references.")
  sources <- s$dataset_lineage_source_tbl
  if (nrow(sources)) {
    sources <- sources[order(sources$source_order, sources$id), , drop = FALSE]
    sources$source_uri <- vapply(sources$source_uri, metadata_public_path, character(1))
    add("## Source datasets")
    block(sources, c("Source" = "title", "Description" = "description", "Date" = "citation_date", "Reference" = "source_uri"))
  }
  steps <- s$dataset_lineage_process_step_tbl
  if (nrow(steps)) {
    steps <- steps[order(steps$step_order, steps$id), , drop = FALSE]
    steps$script_reference <- vapply(steps$script_reference, metadata_public_path, character(1))
    add("## Recorded processing steps")
    block(steps, c("Step" = "step_order", "Description" = "description", "Date" = "step_datetime",
      "Method reference" = "script_reference", "Output" = "output_description"))
  }
  heading("Quality and limitations")
  field("Use and fitness limitations", r$use_limitations, TRUE)
  block(s$dataset_quality_result_tbl, c("Measure" = "measure_name", "Scope" = "scope_code",
    "Evaluation method" = "evaluation_method", "Evaluation date" = "evaluation_date", "Result" = "result_value",
    "Units" = "result_unit", "Conformance specification" = "conformance_specification",
    "Conformance result (1 = pass; 0 = fail)" = "pass", "Explanation" = "explanation"), empty = "No structured quality results recorded.")
  heading("Rights, access and redistribution")
  field("Recorded access conditions", r$access_constraints)
  field("Recorded dataset embargo", r$embargo_status)
  field("Recorded dataset embargo end", r$embargo_end_date)
  block(s$dataset_license_tbl, c("Licence" = "license_name", "Licence identifier" = "license_identifier",
    "Licence terms" = "license_description", "Licence URL" = "license_url", "Rights holder" = "license_holder",
    "Rights basis" = "rights_basis_code", "Access level" = "access_level_code",
    "Redistribution permission" = "redistribution_permission_code", "Use conditions" = "use_constraints",
    "Required acknowledgement" = "attribution_text", "Sensitivity handling" = "sensitivity_handling_code",
    "Embargo status" = "embargo_status", "Embargo ends" = "embargo_end_date",
    "Release statement" = "release_statement", "Dissemination condition" = "dissemination_constraint_code",
    "Terms reviewed" = "terms_review_date"),
    empty = "Rights and redistribution terms are not recorded. Permission to redistribute has not been established by this document.")
  for (i in seq_len(nrow(s$dataset_license_tbl))) {
    field("Rights-holder organisation", organisation(s$dataset_license_tbl$rights_holder_organisation_id[i]))
    field("Permission reference", metadata_public_path(s$dataset_license_tbl$permission_reference[i]))
  }
  heading("Contact and maintenance")
  contacts <- s$poc_tbl
  if (!nrow(contacts)) contacts <- s$metadata_responsibility_tbl
  if (!nrow(contacts)) text("Contact not recorded.")
  for (i in seq_len(nrow(contacts))) {
    field("Contact", person(contacts$person_id[i]))
    field("Organisation", organisation(contacts$organisation_id[i]))
    field("Role", if ("role" %in% names(contacts)) contacts$role[i] else contacts$role_code[i])
    p <- s$people[which(s$people$id == contacts$person_id[i]), , drop = FALSE]
    field("Email", p$email)
  }
  field("Update frequency", r$update_frequency, TRUE)
  field("Metadata identifier", r$metadata_uuid)
  reviews <- s$metadata_review_tbl
  dates <- if (nrow(reviews)) sort(reviews$review_timestamp[!is.na(reviews$review_timestamp)]) else character()
  field("Latest recorded metadata review", utils::tail(dates, 1), TRUE)
  field("Metadata creation date", r$metadata_creation_date)
  resources <- s$dataset_online_resource_tbl
  if (nrow(resources)) {
    resources$resource_url <- vapply(resources$resource_url, metadata_public_path, character(1))
    block(resources, c("Online resource" = "resource_name", "URL" = "resource_url"))
  }
  if (nrow(docs)) {
    other <- docs[which(docs$include_in_metadata == 1L & (is.na(docs$include_in_lineage) | docs$include_in_lineage != 1L) &
      !docs$document_rel_path %in% s$development_documents), , drop = FALSE]
    if (nrow(other)) {
      heading("Supporting documentation")
      table(other, c("Document" = "document_name", "Reference" = "document_rel_path"))
    }
  }
  heading("Attribute and band dictionary")
  attrs <- s$attributes
  if (!nrow(attrs)) text("No attribute or band definitions recorded for active data files.")
  if (nrow(attrs)) {
    # Separate each file/layer to avoid confusing fields with identical names.
    keys <- paste(attrs$data_file_id, attrs$data_file_rel_path_full, attrs$layer_name, sep = "\r")
    for (key in unique(keys)) {
      group <- attrs[keys == key, , drop = FALSE]
      field("File", metadata_public_path(group$data_file_rel_path_full[1]), TRUE)
      field("Layer", group$layer_name[1])
      table(group, c("Field / band" = "data_field_name", "Definition" = "data_field_desc", "Units" = "data_field_units"))
      for (i in seq_len(nrow(group))) if (metadata_present(group$legend_field_notes[i]))
        field(paste0("Value meanings for ", group$data_field_name[i]), group$legend_field_notes[i])
    }
  }
  if (s$unlinked_attribute_count > 0L) text(paste(s$unlinked_attribute_count,
    "attribute records could not be linked uniquely to an active file and are omitted from this dictionary."))
  lines
}

write_metadata_document_source <- function(snapshot, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  theme <- datapond_resource("ekznw_document_theme")
  destination <- file.path(directory, "ekznw-theme")
  dir.create(destination, showWarnings = FALSE)
  for (name in list.files(theme, all.files = TRUE, no.. = TRUE)) {
    if (!file.copy(file.path(theme, name), destination, recursive = TRUE)) stop("Could not copy the document theme.")
  }
  title <- as.character(jsonlite::toJSON(metadata_markdown(snapshot$record$title), auto_unbox = TRUE))
  writeLines(c("---", paste0("title: ", title), "subtitle: Dataset metadata", "date: ''",
    "metadata-files: [ekznw-theme/ekznw.yml]", "execute:", "  enabled: false", "---", "",
    metadata_document_body(snapshot)), file.path(directory, "metadata.qmd"), useBytes = TRUE)
  # The exact saved citation is shared by both outputs; no independent rewrite.
  writeLines(metadata_text(snapshot$record$citation), file.path(directory, "citation.txt"), useBytes = TRUE)
  invisible(file.path(directory, "metadata.qmd"))
}

render_metadata_document <- function(directory) {
  quarto <- document_quarto_path()
  if (!nzchar(quarto)) stop("Run datapond::setup_document_tools() to prepare Quarto and XeLaTeX for metadata PDF export.", call. = FALSE)
  result <- processx::run(quarto, c("render", "metadata.qmd", "--to", "pdf", "--no-execute", "--quiet"),
    wd = directory, error_on_status = FALSE, timeout = 180000)
  path <- file.path(directory, "metadata.pdf")
  if (result$status != 0L || !file.exists(path)) stop(paste("Metadata PDF rendering failed.", result$stderr, result$stdout), call. = FALSE)
  path
}

# Prepared documents are a deliberate working-dataset operation. The public
# files live beside the data; source, styling and freshness evidence stay private.
metadata_document_directory <- function(pond, dataset_id) {
  path <- file.path(pond, ".metadata_pond", "documents", as.character(as.integer(dataset_id)))
  link <- Sys.readlink(path)
  if (!is.na(link) && nzchar(link)) stop("The prepared-document folder cannot be a symbolic link.")
  release_safe_directory(pond, path)
}

metadata_document_fingerprint <- function(snapshot) {
  snapshot$generated_at <- ""
  review_hash(list(title = metadata_text(snapshot$record$title), body = metadata_document_body(snapshot)))
}

metadata_document_ready <- function(dataset_id, pond, root) {
  directory <- metadata_document_directory(pond, dataset_id)
  fail <- function() stop("Save and download metadata.pdf from Human-readable metadata, then open it and review the dataset folder before freezing. The prepared documents are missing, changed or out of date.", call. = FALSE)
  if (!file.exists(file.path(directory, "prepared.json"))) fail()
  manifest <- tryCatch(jsonlite::read_json(file.path(directory, "prepared.json")), error = function(e) NULL)
  if (is.null(manifest) || !identical(manifest$fingerprint,
      metadata_document_fingerprint(collect_metadata_document(dataset_id, pond)))) fail()
  files <- release_inventory(directory)
  files <- Filter(function(x) x$path != "prepared.json", files)
  if (!identical(review_hash(files), manifest$files_hash)) fail()
  for (name in c("metadata.pdf", "citation.txt")) {
    path <- file.path(root, name)
    if (!file.exists(path) || dir.exists(path) || nzchar(Sys.readlink(path)) ||
        !identical(release_sha(path), release_sha(file.path(directory, name)))) fail()
  }
  list(directory = directory, files = files, fingerprint = manifest$fingerprint)
}

# Publish several prepared paths together, restoring previous files if any
# rename fails. Backups and replacements are on the same filesystem as targets.
metadata_document_publish <- function(sources, destinations) {
  backups <- rep(NA_character_, length(destinations))
  installed <- rep(FALSE, length(destinations)); complete <- FALSE
  on.exit({
    if (!complete) for (i in rev(seq_along(destinations))) {
      if (installed[i]) unlink(destinations[i], recursive = TRUE)
      if (!is.na(backups[i]) && !file.rename(backups[i], destinations[i]))
        warning(paste("Previous document retained for recovery at", backups[i]))
    }
    if (complete) unlink(backups[!is.na(backups)], recursive = TRUE)
  }, add = TRUE)
  for (i in seq_along(destinations)) {
    if (file.exists(destinations[i])) {
      backup <- tempfile(".previous-document-", tmpdir = dirname(destinations[i]))
      if (!file.rename(destinations[i], backup)) stop("Could not preserve the previous document.")
      backups[i] <- backup
    }
    if (!file.rename(sources[i], destinations[i])) stop("Could not save the prepared document.")
    installed[i] <- TRUE
  }
  complete <- TRUE
  invisible(destinations)
}

save_working_metadata_document <- function(dataset_id, base_path, user_id, pdf = TRUE, download = NULL) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  release_require_user(conn, user_id)
  lock <- release_lock(base_path, dataset_id); on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  release_open_review(conn, dataset_id)
  record <- release_entry(conn, dataset_id)
  root <- release_working_root(base_path, record)
  if (isTRUE(as.integer(record$folder_locked[1]) == 1L)) stop("Unlock the working dataset before saving documents.")
  names <- if (pdf) c("metadata.pdf", "citation.txt") else "citation.txt"
  destinations <- file.path(root, names)
  previous <- function() vapply(destinations, function(path) {
    link <- Sys.readlink(path)
    if ((!is.na(link) && nzchar(link)) || dir.exists(path)) stop("Document destinations must be ordinary files, not folders or symbolic links.")
    if (file.exists(path)) release_sha(path) else "absent"
  }, character(1))
  before <- previous()
  snapshot <- collect_metadata_document(dataset_id, base_path)
  fingerprint <- metadata_document_fingerprint(snapshot)
  directory <- metadata_document_directory(base_path, dataset_id)
  release_safe_directory(base_path, dirname(directory), TRUE)
  stage <- tempfile(".prepare-", tmpdir = dirname(directory)); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  if (pdf) {
    write_metadata_document_source(snapshot, stage)
    render_metadata_document(stage)
    unlink(file.path(stage, c(".quarto", "metadata_files")), recursive = TRUE)
    release_json(list(fingerprint = fingerprint, files_hash = review_hash(release_inventory(stage))),
      file.path(stage, "prepared.json"))
  } else writeLines(metadata_text(snapshot$record$citation), file.path(stage, "citation.txt"), useBytes = TRUE)
  # Deliver the same bytes to the browser and dataset root; finish rendering
  # before touching existing documents. Recheck metadata and external edits.
  pending <- tempfile(".metadata-download-", tmpdir = root); dir.create(pending)
  on.exit(unlink(pending, recursive = TRUE), add = TRUE)
  sources <- file.path(pending, names)
  if (!all(file.copy(file.path(stage, names), sources))) stop("Could not stage the dataset documents.")
  if (!is.null(download) && !file.copy(sources[1], download, overwrite = TRUE)) stop("Could not deliver the document download.")
  release_require_user(conn, user_id)
  latest <- release_entry(conn, dataset_id)
  if (!identical(release_working_root(base_path, latest), root) || isTRUE(as.integer(latest$folder_locked[1]) == 1L) ||
      !identical(fingerprint, metadata_document_fingerprint(collect_metadata_document(dataset_id, base_path))) ||
      !identical(before, previous())) stop("Dataset or documents changed during preparation. Save and download again.")
  release_open_review(conn, dataset_id)
  if (pdf) {
    sources <- c(sources, stage); destinations <- c(destinations, directory)
  }
  metadata_document_publish(sources, destinations)
  invisible(file.path(root, names[1]))
}

#' Write an EKZNW-branded dataset metadata PDF
#'
#' Reads saved metadata without modifying the pond or running development code.
#' Writes metadata.pdf, metadata.qmd, citation.txt and the portable document theme
#' to a new output directory. Existing outputs are never overwritten.
#' @param dataset_id Dataset identifier.
#' @param base_path Data Pond root.
#' @param directory New destination directory, outside the Data Pond.
#' @return Named paths to the PDF, source and citation.
#' @export
write_metadata_pdf <- function(dataset_id, base_path, directory) {
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  pond <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  parent <- normalizePath(dirname(directory), winslash = "/", mustWork = TRUE)
  directory <- file.path(parent, basename(directory))
  if (directory == pond || startsWith(directory, paste0(pond, "/"))) stop("Choose an output folder outside the Data Pond.", call. = FALSE)
  if (file.exists(directory)) stop("Choose a new output directory; existing exports are never overwritten.", call. = FALSE)
  snapshot <- collect_metadata_document(dataset_id, base_path)
  stage <- tempfile(".metadata-stage-", tmpdir = parent)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  write_metadata_document_source(snapshot, stage)
  render_metadata_document(stage)
  # Quarto intermediates belong to the renderer, not the distributable source.
  unlink(file.path(stage, c(".quarto", "metadata_files")), recursive = TRUE)
  if (file.exists(directory) || !file.rename(stage, directory)) stop("Could not publish the metadata document.", call. = FALSE)
  stats::setNames(file.path(directory, c("metadata.pdf", "metadata.qmd", "citation.txt")), c("pdf", "source", "citation"))
}
