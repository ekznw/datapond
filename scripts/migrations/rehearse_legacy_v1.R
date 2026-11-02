# Run from the repository root. Every input is opened read-only; only newly
# created SQLite backup copies are passed to init_db(). Never upgrades a source.
# Rscript scripts/migrations/rehearse_legacy_v1.R /tmp/new-rehearsal DB_OR_POND ...
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Supply a new output directory followed by database files or pond folders.")
output <- normalizePath(path.expand(args[1]), mustWork = FALSE)
sources <- vapply(args[-1], function(path) {
  path <- normalizePath(path.expand(path), mustWork = TRUE)
  if (dir.exists(path)) path <- file.path(path, "metadata.db")
  normalizePath(path, mustWork = TRUE)
}, character(1))
if (file.exists(output)) stop("Use a new output directory; existing results are never overwritten.")
for (source in sources) {
  if (identical(output, dirname(source)) || startsWith(output, paste0(dirname(source), "/"))) {
    stop("The rehearsal directory must be outside all source folders.")
  }
}
pkgload::load_all(export_all = FALSE, helpers = FALSE)
dir.create(output, recursive = TRUE)
hashes <- function(database) {
  files <- c(database, paste0(database, "-wal"))
  files <- files[file.exists(files)]
  stats::setNames(vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE), basename(files))
}
read_table <- function(conn, table) {
  if (DBI::dbExistsTable(conn, table)) DBI::dbReadTable(conn, table) else data.frame()
}
preserved <- function(before, after, fields) {
  fields <- intersect(fields, names(before))
  stopifnot(all(fields %in% names(after)))
  stopifnot(identical(before[, fields, drop = FALSE], after[, fields, drop = FALSE]))
}
results <- vector("list", length(sources))
for (i in seq_along(sources)) {
  source <- sources[i]
  before_hashes <- hashes(source)
  pond <- file.path(output, sprintf("copy-%02d", i))
  dir.create(pond)
  original <- DBI::dbConnect(RSQLite::SQLite(), source, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  DBI::dbExecute(original, "PRAGMA query_only = ON")
  copy <- DBI::dbConnect(RSQLite::SQLite(), file.path(pond, "metadata.db"))
  tryCatch(RSQLite::sqliteCopyDatabase(original, copy), finally = {
    DBI::dbDisconnect(copy)
    DBI::dbDisconnect(original)
  })
  copy <- datapond:::get_conn(pond)
  ids <- read_table(copy, "data_pond_tbl")$data_pond_id
  if (length(ids) == 1L) {
    dir.create(file.path(pond, ".metadata_pond"))
    writeLines(ids, file.path(pond, ".metadata_pond", "data_pond_id"))
  }
  before_entry <- read_table(copy, "entry_tbl")
  before_lineage <- read_table(copy, "dataset_lineage_tbl")
  before_people <- read_table(copy, "persons_tbl")
  before_users <- read_table(copy, "user_tbl")
  before_rights <- read_table(copy, "dataset_license_tbl")
  before_status <- datapond:::legacy_patch_v1_status(copy)
  DBI::dbDisconnect(copy)
  datapond:::init_db(pond)
  copy <- datapond:::get_conn(pond)
  after_entry <- read_table(copy, "entry_tbl")
  preserved(before_entry, after_entry, c("id", "uuid", "title", "software_desc", "epsg_code", "bbox",
    "access_constraints", "use_limitations", "citation_date", "data_distribution_url",
    "temporal_coverage_start_date", "temporal_coverage_end_date", "metadata_creation_date"))
  preserved(before_lineage, read_table(copy, "dataset_lineage_tbl"), c("id", "dataset_id",
    "source_description", "source_datasets_text", "process_description", "processing_tools",
    "processing_scripts", "output_description", "lineage_notes"))
  preserved(before_people, read_table(copy, "persons_tbl"), names(before_people))
  preserved(before_users, read_table(copy, "user_tbl"), names(before_users))
  after_rights <- read_table(copy, "dataset_license_tbl")
  # Existing nonblank rights must remain intact; blanks may be migrated.
  for (field in intersect(names(before_rights), names(after_rights))) {
    if (field %in% c("updated_at", "license_template_id")) next
    selected <- !is.na(before_rights[[field]]) & nzchar(trimws(as.character(before_rights[[field]])))
    matched <- match(before_rights$id[selected], after_rights$id)
    stopifnot(identical(before_rights[[field]][selected], after_rights[[field]][matched]))
  }
  tables <- c("dataset_citation_date_tbl", "dataset_online_resource_tbl", "dataset_temporal_extent_tbl",
    "organisation_tbl", "party_address_tbl", "dataset_license_tbl", "dataset_lineage_tbl",
    "dataset_lineage_source_tbl", "dataset_lineage_process_step_tbl")
  after_first <- stats::setNames(lapply(tables, read_table, conn = copy), tables)
  after_status <- datapond:::legacy_patch_v1_status(copy)
  stopifnot(all(after_status$pending == 0L))
  stopifnot(nrow(DBI::dbGetQuery(copy, "PRAGMA foreign_key_check")) == 0L)
  DBI::dbDisconnect(copy)
  datapond:::init_db(pond)
  copy <- datapond:::get_conn(pond)
  after_second <- stats::setNames(lapply(tables, read_table, conn = copy), tables)
  stopifnot(identical(after_first, after_second))
  DBI::dbDisconnect(copy)
  after_hashes <- hashes(source)
  stopifnot(identical(before_hashes, after_hashes))
  results[[i]] <- list(source_database = source, copy = pond, records = nrow(before_entry),
    source_sha256_before = before_hashes, source_sha256_after = after_hashes,
    source_unchanged = TRUE, source_fields_preserved = TRUE, existing_populated_rights_preserved = TRUE,
    people_and_users_preserved = TRUE, before = before_status, after = after_status,
    repeated_conversion_tables_identical = TRUE,
    resulting_row_counts = as.list(vapply(after_first, nrow, integer(1))))
  jsonlite::write_json(results[seq_len(i)], file.path(output, "results.json"), pretty = TRUE, auto_unbox = TRUE)
  cat(basename(source), ": ", nrow(before_entry), " records; PASS, source unchanged\n", sep = "")
}
