# Startup compatibility patch. Keep the v1 implementation while older colleague
# ponds and transfer bundles are supported. Schema definitions stay in db/profile
# modules; these permanent row markers travel with exported records.
legacy_patch_v1_components <- function() {
  list(
    keywords = list(table = "entry_tbl", marker = NULL),
    citation = list(table = "entry_tbl", marker = "legacy_citation_v1_migrated_at"),
    wave2 = list(table = "entry_tbl", marker = "legacy_wave2_v1_migrated_at"),
    parties = list(table = "organisation_tbl", marker = "legacy_party_v1_migrated_at"),
    licensing = list(table = "dataset_license_tbl", marker = "legacy_licensing_v1_migrated_at"),
    lineage = list(table = "dataset_lineage_tbl", marker = "legacy_lineage_migrated_at")
  )
}

ensure_legacy_patch_v1_marker <- function(conn, component) {
  definition <- legacy_patch_v1_components()[[component]]
  if (is.null(definition)) stop("Unknown legacy patch component: ", component)
  if (!is.null(definition$marker)) {
    ensure_columns(conn, definition$table, definition$marker,
      stats::setNames("TEXT", definition$marker))
  }
  invisible(TRUE)
}

legacy_patch_v1_pending <- function(conn, component) {
  definition <- legacy_patch_v1_components()[[component]]
  if (is.null(definition)) stop("Unknown legacy patch component: ", component)
  if (component == "keywords") {
    counts <- c(0L)
    if ("keyword_topic" %in% DBI::dbListFields(conn, "entry_tbl")) {
      counts <- c(counts, DBI::dbGetQuery(conn, paste(
        "SELECT COUNT(*) AS n FROM entry_tbl",
        "WHERE NULLIF(TRIM(keyword_topic), '') IS NOT NULL"
      ))$n)
    }
    for (table in c("keyword_vocab_tbl", "keyword_thesaurus_tbl")) {
      if (DBI::dbExistsTable(conn, table)) counts <- c(counts, DBI::dbGetQuery(conn,
        paste("SELECT COUNT(*) AS n FROM", table,
          "WHERE field_code = 'keyword_topic' COLLATE NOCASE"))$n)
    }
    return(as.integer(sum(counts)))
  }
  if (!DBI::dbExistsTable(conn, definition$table)) return(NA_integer_)
  predicate <- if (definition$marker %in% DBI::dbListFields(conn, definition$table)) {
    paste("WHERE", DBI::dbQuoteIdentifier(conn, definition$marker), "IS NULL")
  } else ""
  as.integer(DBI::dbGetQuery(conn, paste("SELECT COUNT(*) AS n FROM",
    DBI::dbQuoteIdentifier(conn, definition$table), predicate))$n[1])
}

# Read-only progress inspection; missing marker columns are pending, not done.
legacy_patch_v1_status <- function(conn) {
  components <- names(legacy_patch_v1_components())
  data.frame(patch = "legacy-v1", component = components,
    pending = vapply(components, function(x) legacy_patch_v1_pending(conn, x), integer(1)),
    row.names = NULL)
}

apply_legacy_patch_v1 <- function(conn, components = names(legacy_patch_v1_components())) {
  unknown <- setdiff(components, names(legacy_patch_v1_components()))
  if (length(unknown)) stop("Unknown legacy patch component: ", paste(unknown, collapse = ", "))
  # A savepoint supports both startup and the existing transfer transaction.
  # On failure, all data conversions and completion markers in this call roll back.
  DBI::dbExecute(conn, "SAVEPOINT datapond_legacy_patch_v1")
  finished <- FALSE
  on.exit({
    if (!finished) DBI::dbExecute(conn, "ROLLBACK TO datapond_legacy_patch_v1")
    DBI::dbExecute(conn, "RELEASE datapond_legacy_patch_v1")
  }, add = TRUE)
  migrated <- stats::setNames(integer(length(components)), components)
  for (component in components) {
    ensure_legacy_patch_v1_marker(conn, component)
    pending <- legacy_patch_v1_pending(conn, component)
    if (is.na(pending)) stop("Prepare the current schema before applying the legacy patch.")
    if (pending == 0L) next
    handler <- get(paste0("legacy_patch_v1_", component), mode = "function")
    handler(conn)
    definition <- legacy_patch_v1_components()[[component]]
    if (!is.null(definition$marker)) {
      DBI::dbExecute(conn, paste("UPDATE", DBI::dbQuoteIdentifier(conn, definition$table),
        "SET", DBI::dbQuoteIdentifier(conn, definition$marker), "= ? WHERE",
        DBI::dbQuoteIdentifier(conn, definition$marker), "IS NULL"),
        params = list(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
    }
    migrated[[component]] <- pending
  }
  if (any(migrated > 0L) && nrow(DBI::dbGetQuery(conn, "PRAGMA foreign_key_check"))) {
    stop("Legacy patch could not complete: the catalogue has broken foreign-key references.", call. = FALSE)
  }
  finished <- TRUE
  invisible(migrated)
}
