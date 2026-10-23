# ISO metadata Wave 2 persistence ------------------------------------------

iso_online_resource_function_codes <- function() {
  c(
    "download", "information", "offlineAccess", "order", "search",
    "completeMetadata", "browseGraphic", "upload", "emailService",
    "browsing", "fileAccess"
  )
}

iso_quality_measure_type_codes <- function() {
  c(
    "completeness", "logicalConsistency", "positionalAccuracy",
    "thematicAccuracy", "temporalQuality"
  )
}

# Precise ISO elements supplement the historical broad categories. The editor
# uses one selection; the broad category is derived for compatibility.
iso_quality_elements <- function() {
  data.frame(
    code = c("DQ_CompletenessCommission", "DQ_CompletenessOmission",
      "DQ_ConceptualConsistency", "DQ_DomainConsistency", "DQ_FormatConsistency", "DQ_TopologicalConsistency",
      "DQ_AbsoluteExternalPositionalAccuracy", "DQ_RelativeInternalPositionalAccuracy", "DQ_GriddedDataPositionalAccuracy",
      "DQ_ThematicClassificationCorrectness", "DQ_NonQuantitativeAttributeCorrectness", "DQ_QuantitativeAttributeAccuracy",
      "DQ_AccuracyOfATimeMeasurement", "DQ_TemporalConsistency", "DQ_TemporalValidity"),
    category = rep(iso_quality_measure_type_codes(), c(2, 4, 3, 3, 3)),
    label = c("Completeness: excess or duplicate data", "Completeness: missing data",
      "Consistency: data model rules", "Consistency: allowed values", "Consistency: file format", "Consistency: topology",
      "Position: accuracy against a reference", "Position: relative accuracy", "Position: grid accuracy",
      "Attributes: classification correctness", "Attributes: non-numeric correctness", "Attributes: numeric accuracy",
      "Time: measurement accuracy", "Time: consistency", "Time: validity"), stringsAsFactors = FALSE)
}

iso_quality_element_choices <- function() {
  elements <- iso_quality_elements()
  c(stats::setNames(iso_quality_measure_type_codes(), paste0(
    c("Completeness", "Logical consistency", "Positional accuracy", "Thematic accuracy", "Temporal quality"),
    " (specific test not recorded)")), stats::setNames(elements$code, elements$label))
}

iso_quality_type_selection <- function(selection) {
  elements <- iso_quality_elements()
  index <- match(selection, elements$code)
  if (is.na(index)) list(measure_type_code = selection, quality_element_code = NA_character_)
  else list(measure_type_code = elements$category[index], quality_element_code = elements$code[index])
}

iso_quality_result_type_codes <- function() {
  c("quantitative", "conformance", "descriptive")
}

iso_quality_scope_codes <- function() {
  c(
    "dataset", "series", "service", "software", "model",
    "nonGeographicDataset", "featureType", "attribute"
  )
}

iso_temporal_indeterminate_codes <- function() {
  c("unknown", "now", "before", "after")
}

iso_dissemination_constraint_codes <- function() {
  c("none", "internal", "approvedRecipients", "contractual", "statutory", "confidential")
}

iso_dissemination_restriction_code <- function(code) {
  unname(c(
    none = "unrestricted", internal = "restricted",
    approvedRecipients = "restricted", contractual = "otherRestrictions",
    statutory = "statutory", confidential = "confidential"
  )[as.character(code)[1]])
}

wave2_optional_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NA_character_)
  value <- trimws(as.character(x[1]))
  if (nzchar(value)) value else NA_character_
}

wave2_optional_integer <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NA_integer_)
  value <- suppressWarnings(as.integer(x[1]))
  if (is.na(value)) NA_integer_ else value
}

wave2_valid_uri <- function(x) {
  value <- wave2_optional_text(x)
  !is.na(value) && grepl("^[A-Za-z][A-Za-z0-9+.-]*:[^[:space:]]+$", value)
}

wave2_validate_temporal_value <- function(value, precision, indeterminate, endpoint) {
  value <- wave2_optional_text(value)
  precision <- wave2_optional_text(precision)
  indeterminate <- wave2_optional_text(indeterminate)
  if (!is.na(indeterminate) && !indeterminate %in% iso_temporal_indeterminate_codes()) {
    stop("Unknown temporal indeterminate-position code.", call. = FALSE)
  }
  if (!is.na(value) && !is.na(indeterminate)) {
    stop(paste("A temporal", endpoint, "cannot have both a value and an indeterminate position."), call. = FALSE)
  }
  if (is.na(value) && is.na(indeterminate)) {
    stop(paste("Every temporal", endpoint, "needs a value or indeterminate position."), call. = FALSE)
  }
  if (!is.na(value)) {
    checked <- validate_iso_date_value(value, precision)
    value <- checked$value
    precision <- checked$precision
  } else if (is.na(precision)) {
    precision <- "day"
  }
  list(value = value, precision = precision, indeterminate = indeterminate)
}

ensure_iso_wave2_schema <- function(conn, migrate = TRUE) {
  ensure_columns(
    conn, "entry_tbl",
    fields = c(
      "data_distribution_url", "embargo_status", "embargo_reason", "embargo_end_date",
      "temporal_coverage_start_date", "temporal_coverage_end_date"
    ),
    types = c(
      data_distribution_url = "TEXT", embargo_status = "TEXT", embargo_reason = "TEXT",
      embargo_end_date = "TEXT", temporal_coverage_start_date = "TEXT",
      temporal_coverage_end_date = "TEXT"
    )
  )
  ensure_columns(
    conn, "entry_tbl", "equivalent_scale_denominator",
    types = c(equivalent_scale_denominator = "INTEGER")
  )

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_alternate_title_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      alternate_title TEXT NOT NULL,
      title_type_code TEXT NOT NULL DEFAULT 'alternate',
      sort_order INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_alternate_title_dataset_order
    ON dataset_alternate_title_tbl(dataset_id, sort_order)
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_online_resource_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      resource_url TEXT NOT NULL,
      resource_name TEXT,
      resource_description TEXT,
      protocol TEXT,
      function_code TEXT NOT NULL DEFAULT 'information' CHECK(function_code IN (
        'download', 'information', 'offlineAccess', 'order', 'search',
        'completeMetadata', 'browseGraphic', 'upload', 'emailService',
        'browsing', 'fileAccess'
      )),
      media_type TEXT,
      is_primary INTEGER NOT NULL DEFAULT 0 CHECK(is_primary IN (0, 1)),
      sort_order INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_online_resource_dataset_order
    ON dataset_online_resource_tbl(dataset_id, sort_order)
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_online_resource_primary
    ON dataset_online_resource_tbl(dataset_id) WHERE is_primary = 1
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_distribution_format_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      specification_title TEXT NOT NULL,
      format_version TEXT,
      media_type TEXT,
      format_uri TEXT,
      sort_order INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_distribution_format_dataset_order
    ON dataset_distribution_format_tbl(dataset_id, sort_order)
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_quality_result_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      scope_code TEXT NOT NULL DEFAULT 'dataset' CHECK(scope_code IN (
        'dataset', 'series', 'service', 'software', 'model',
        'nonGeographicDataset', 'featureType', 'attribute'
      )),
      data_file_id INTEGER,
      attribute_id INTEGER,
      measure_identifier TEXT NOT NULL,
      measure_name TEXT,
      measure_type_code TEXT NOT NULL CHECK(measure_type_code IN (
        'completeness', 'logicalConsistency', 'positionalAccuracy',
        'thematicAccuracy', 'temporalQuality'
      )),
      evaluation_method TEXT NOT NULL,
      evaluation_date TEXT,
      result_type_code TEXT NOT NULL CHECK(result_type_code IN (
        'quantitative', 'conformance', 'descriptive'
      )),
      result_value TEXT,
      result_unit TEXT,
      conformance_specification TEXT,
      explanation TEXT,
      pass INTEGER CHECK(pass IN (0, 1) OR pass IS NULL),
      sort_order INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (data_file_id) REFERENCES data_file_tbl(id),
      FOREIGN KEY (attribute_id) REFERENCES dataset_attribute_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  ensure_columns(conn, "dataset_quality_result_tbl", "quality_element_code",
                 types = c(quality_element_code = "TEXT"))
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_quality_result_dataset_order
    ON dataset_quality_result_tbl(dataset_id, sort_order)
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_temporal_extent_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      extent_label TEXT,
      start_value TEXT,
      end_value TEXT,
      start_precision TEXT NOT NULL DEFAULT 'day' CHECK(start_precision IN ('year', 'month', 'day', 'dateTime')),
      end_precision TEXT NOT NULL DEFAULT 'day' CHECK(end_precision IN ('year', 'month', 'day', 'dateTime')),
      start_indeterminate TEXT CHECK(start_indeterminate IN ('unknown', 'now', 'before', 'after') OR start_indeterminate IS NULL),
      end_indeterminate TEXT CHECK(end_indeterminate IN ('unknown', 'now', 'before', 'after') OR end_indeterminate IS NULL),
      description TEXT,
      sort_order INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_temporal_extent_dataset_order
    ON dataset_temporal_extent_tbl(dataset_id, sort_order)
  ")

  ensure_columns(
    conn, "dataset_license_tbl",
    fields = c(
      "embargo_status", "embargo_reason", "embargo_start_date", "embargo_end_date",
      "release_addressee", "release_statement", "dissemination_constraint_code"
    ),
    types = c(
      embargo_status = "TEXT", embargo_reason = "TEXT", embargo_start_date = "TEXT",
      embargo_end_date = "TEXT", release_addressee = "TEXT", release_statement = "TEXT",
      dissemination_constraint_code = "TEXT"
    )
  )

  ensure_legacy_patch_v1_marker(conn, "wave2")
  if (isTRUE(migrate)) apply_legacy_patch_v1(conn, "wave2")
  view_sql <- "CREATE VIEW dataset_temporal_coverage_compat_vw AS
    SELECT e.id AS dataset_id,
      COALESCE(
        (SELECT t.start_value FROM dataset_temporal_extent_tbl t
         WHERE t.dataset_id = e.id ORDER BY t.sort_order, t.id LIMIT 1),
        CASE WHEN e.legacy_wave2_v1_migrated_at IS NULL THEN e.temporal_coverage_start_date END
      ) AS temporal_coverage_start_date,
      COALESCE(
        (SELECT t.end_value FROM dataset_temporal_extent_tbl t
         WHERE t.dataset_id = e.id ORDER BY t.sort_order, t.id LIMIT 1),
        CASE WHEN e.legacy_wave2_v1_migrated_at IS NULL THEN e.temporal_coverage_end_date END
      ) AS temporal_coverage_end_date
    FROM entry_tbl e
  "
  existing_view <- DBI::dbGetQuery(conn,
    "SELECT sql FROM sqlite_master WHERE type = 'view' AND name = 'dataset_temporal_coverage_compat_vw'")
  if (!nrow(existing_view) || !identical(trimws(existing_view$sql[1]), trimws(view_sql))) {
    DBI::dbExecute(conn, "DROP VIEW IF EXISTS dataset_temporal_coverage_compat_vw")
    DBI::dbExecute(conn, view_sql)
  }
  invisible(TRUE)
}

wave2_get_rows <- function(dataset_id, base_path, table) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave2_schema(conn)
  DBI::dbGetQuery(
    conn,
    paste0("SELECT * FROM ", table, " WHERE dataset_id = ? ORDER BY sort_order, id"),
    params = list(as.integer(dataset_id))
  )
}

get_dataset_alternate_titles <- function(dataset_id, base_path) {
  wave2_get_rows(dataset_id, base_path, "dataset_alternate_title_tbl")
}

get_dataset_online_resources <- function(dataset_id, base_path) {
  wave2_get_rows(dataset_id, base_path, "dataset_online_resource_tbl")
}

get_dataset_distribution_formats <- function(dataset_id, base_path) {
  wave2_get_rows(dataset_id, base_path, "dataset_distribution_format_tbl")
}

get_dataset_quality_results <- function(dataset_id, base_path) {
  wave2_get_rows(dataset_id, base_path, "dataset_quality_result_tbl")
}

get_dataset_temporal_extents <- function(dataset_id, base_path) {
  wave2_get_rows(dataset_id, base_path, "dataset_temporal_extent_tbl")
}

wave2_replace_rows <- function(dataset_id, rows, base_path, user_person_id, table, fields) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave2_schema(conn)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  user_person_id <- wave2_optional_integer(user_person_id)
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, paste0("DELETE FROM ", table, " WHERE dataset_id = ?"),
                   params = list(as.integer(dataset_id)))
    if (nrow(rows) > 0L) for (i in seq_len(nrow(rows))) {
      insert_fields <- c("dataset_id", fields, "sort_order", "created_at", "created_by_person_id",
                         "updated_at", "updated_by_person_id")
      sql <- paste0(
        "INSERT INTO ", table, " (", paste(insert_fields, collapse = ", "), ") VALUES (",
        paste(rep("?", length(insert_fields)), collapse = ", "), ")"
      )
      params <- c(list(as.integer(dataset_id)), unname(as.list(rows[i, fields, drop = FALSE])),
                  list(i, now, user_person_id, now, user_person_id))
      DBI::dbExecute(conn, sql, params = params)
    }
  })
  invisible(TRUE)
}

replace_dataset_alternate_titles <- function(dataset_id, titles, base_path, user_person_id = NA_integer_) {
  if (is.data.frame(titles)) {
    rows <- as.data.frame(titles, stringsAsFactors = FALSE)
    values <- if ("alternate_title" %in% names(rows)) rows$alternate_title else character()
  } else {
    values <- titles %||% character()
  }
  values <- unique(trimws(as.character(values)))
  values <- values[!is.na(values) & nzchar(values)]
  rows <- data.frame(alternate_title = values, title_type_code = rep("alternate", length(values)),
                     stringsAsFactors = FALSE)
  wave2_replace_rows(dataset_id, rows, base_path, user_person_id,
                     "dataset_alternate_title_tbl", c("alternate_title", "title_type_code"))
}

replace_dataset_online_resources <- function(dataset_id, resources, base_path, user_person_id = NA_integer_) {
  rows <- as.data.frame(resources %||% data.frame(), stringsAsFactors = FALSE)
  fields <- c("resource_url", "resource_name", "resource_description", "protocol", "function_code", "media_type", "is_primary")
  for (field in setdiff(fields, names(rows))) rows[[field]] <- rep(NA, nrow(rows))
  if (nrow(rows) > 0L) {
    for (i in seq_len(nrow(rows))) {
      if (!wave2_valid_uri(rows$resource_url[i])) stop("Every online resource needs a valid absolute URL or URI.", call. = FALSE)
      rows$resource_url[i] <- wave2_optional_text(rows$resource_url[i])
      rows$function_code[i] <- wave2_optional_text(rows$function_code[i])
      if (is.na(rows$function_code[i])) rows$function_code[i] <- "information"
      if (!rows$function_code[i] %in% iso_online_resource_function_codes()) stop("Unknown online-resource function code.", call. = FALSE)
      rows$is_primary[i] <- as.integer(isTRUE(as.logical(rows$is_primary[i])))
      for (field in c("resource_name", "resource_description", "protocol", "media_type")) {
        rows[[field]][i] <- wave2_optional_text(rows[[field]][i])
      }
    }
    primary <- which(rows$is_primary == 1L)
    if (length(primary) == 0L) rows$is_primary[1] <- 1L
    if (length(primary) > 1L) rows$is_primary[primary[-1L]] <- 0L
  }
  wave2_replace_rows(dataset_id, rows[, fields, drop = FALSE], base_path, user_person_id,
                     "dataset_online_resource_tbl", fields)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  primary_url <- if (nrow(rows) == 0L) NA_character_ else rows$resource_url[which(rows$is_primary == 1L)[1]]
  DBI::dbExecute(conn, "UPDATE entry_tbl SET data_distribution_url = ? WHERE id = ?",
                 params = list(primary_url, as.integer(dataset_id)))
  invisible(TRUE)
}

replace_dataset_distribution_formats <- function(dataset_id, formats, base_path, user_person_id = NA_integer_) {
  rows <- as.data.frame(formats %||% data.frame(), stringsAsFactors = FALSE)
  fields <- c("specification_title", "format_version", "media_type", "format_uri")
  for (field in setdiff(fields, names(rows))) rows[[field]] <- rep(NA_character_, nrow(rows))
  if (nrow(rows) > 0L) for (i in seq_len(nrow(rows))) {
    rows$specification_title[i] <- wave2_optional_text(rows$specification_title[i])
    if (is.na(rows$specification_title[i])) stop("Every distribution format needs a specification title.", call. = FALSE)
    rows$format_version[i] <- wave2_optional_text(rows$format_version[i])
    rows$media_type[i] <- wave2_optional_text(rows$media_type[i])
    rows$format_uri[i] <- wave2_optional_text(rows$format_uri[i])
    if (!is.na(rows$format_uri[i]) && !wave2_valid_uri(rows$format_uri[i])) stop("Format URI must be an absolute URI.", call. = FALSE)
  }
  wave2_replace_rows(dataset_id, rows[, fields, drop = FALSE], base_path, user_person_id,
                     "dataset_distribution_format_tbl", fields)
}

replace_dataset_quality_results <- function(dataset_id, results, base_path, user_person_id = NA_integer_) {
  rows <- as.data.frame(results %||% data.frame(), stringsAsFactors = FALSE)
  fields <- c("scope_code", "data_file_id", "attribute_id", "measure_identifier", "measure_name", "quality_element_code",
              "measure_type_code", "evaluation_method", "evaluation_date", "result_type_code",
              "result_value", "result_unit", "conformance_specification", "explanation", "pass")
  for (field in setdiff(fields, names(rows))) rows[[field]] <- rep(NA, nrow(rows))
  if (nrow(rows) > 0L) for (i in seq_len(nrow(rows))) {
    rows$scope_code[i] <- wave2_optional_text(rows$scope_code[i]); if (is.na(rows$scope_code[i])) rows$scope_code[i] <- "dataset"
    rows$measure_identifier[i] <- wave2_optional_text(rows$measure_identifier[i])
    rows$measure_type_code[i] <- wave2_optional_text(rows$measure_type_code[i])
    rows$quality_element_code[i] <- wave2_optional_text(rows$quality_element_code[i])
    if (!is.na(rows$quality_element_code[i])) {
      elements <- iso_quality_elements()
      index <- match(rows$quality_element_code[i], elements$code)
      if (is.na(index)) stop("Unknown ISO quality element.", call. = FALSE)
      if (!is.na(rows$measure_type_code[i]) && rows$measure_type_code[i] != elements$category[index])
        stop("Quality element does not match the measure category.", call. = FALSE)
      rows$measure_type_code[i] <- elements$category[index]
    }
    rows$evaluation_method[i] <- wave2_optional_text(rows$evaluation_method[i])
    rows$result_type_code[i] <- wave2_optional_text(rows$result_type_code[i])
    if (!rows$scope_code[i] %in% iso_quality_scope_codes()) stop("Unknown quality-result scope.", call. = FALSE)
    if (!rows$measure_type_code[i] %in% iso_quality_measure_type_codes()) stop("Unknown ISO quality-measure type.", call. = FALSE)
    if (!rows$result_type_code[i] %in% iso_quality_result_type_codes()) stop("Unknown quality-result type.", call. = FALSE)
    if (is.na(rows$measure_identifier[i]) || is.na(rows$evaluation_method[i])) stop("Quality results need an explicit measure identifier and evaluation method.", call. = FALSE)
    rows$data_file_id[i] <- wave2_optional_integer(rows$data_file_id[i])
    rows$attribute_id[i] <- wave2_optional_integer(rows$attribute_id[i])
    if (identical(rows$scope_code[i], "attribute") && is.na(rows$attribute_id[i])) stop("Attribute-scoped quality results need an attribute.", call. = FALSE)
    for (field in c("measure_name", "evaluation_date", "result_value", "result_unit", "conformance_specification", "explanation")) {
      rows[[field]][i] <- wave2_optional_text(rows[[field]][i])
    }
    if (!is.na(rows$evaluation_date[i])) validate_iso_date_value(rows$evaluation_date[i], infer_iso_date_precision(rows$evaluation_date[i]))
    rows$pass[i] <- if (is.na(rows$pass[i])) NA_integer_ else as.integer(as.logical(rows$pass[i]))
    if (rows$result_type_code[i] == "quantitative" && is.na(rows$result_value[i])) stop("Quantitative quality results need a result value.", call. = FALSE)
    if (rows$result_type_code[i] == "conformance" && (is.na(rows$conformance_specification[i]) || is.na(rows$pass[i]))) stop("Conformance results need a specification and pass/fail value.", call. = FALSE)
    if (rows$result_type_code[i] == "descriptive" && is.na(rows$explanation[i])) stop("Descriptive quality results need an explanation.", call. = FALSE)
  }
  wave2_replace_rows(dataset_id, rows[, fields, drop = FALSE], base_path, user_person_id,
                     "dataset_quality_result_tbl", fields)
}

replace_dataset_temporal_extents <- function(dataset_id, extents, base_path, user_person_id = NA_integer_) {
  rows <- as.data.frame(extents %||% data.frame(), stringsAsFactors = FALSE)
  fields <- c("extent_label", "start_value", "end_value", "start_precision", "end_precision",
              "start_indeterminate", "end_indeterminate", "description")
  for (field in setdiff(fields, names(rows))) rows[[field]] <- rep(NA_character_, nrow(rows))
  if (nrow(rows) > 0L) for (i in seq_len(nrow(rows))) {
    start <- wave2_validate_temporal_value(rows$start_value[i], rows$start_precision[i], rows$start_indeterminate[i], "start")
    end <- wave2_validate_temporal_value(rows$end_value[i], rows$end_precision[i], rows$end_indeterminate[i], "end")
    rows$start_value[i] <- start$value; rows$start_precision[i] <- start$precision; rows$start_indeterminate[i] <- start$indeterminate
    rows$end_value[i] <- end$value; rows$end_precision[i] <- end$precision; rows$end_indeterminate[i] <- end$indeterminate
    rows$extent_label[i] <- wave2_optional_text(rows$extent_label[i]); rows$description[i] <- wave2_optional_text(rows$description[i])
    if (!is.na(start$value) && !is.na(end$value) && start$precision %in% c("day", "dateTime") && end$precision %in% c("day", "dateTime")) {
      start_time <- suppressWarnings(as.POSIXct(start$value, tz = "UTC")); end_time <- suppressWarnings(as.POSIXct(end$value, tz = "UTC"))
      if (!is.na(start_time) && !is.na(end_time) && start_time > end_time) stop("Temporal extent start cannot be later than its end.", call. = FALSE)
    }
  }
  wave2_replace_rows(dataset_id, rows[, fields, drop = FALSE], base_path, user_person_id,
                     "dataset_temporal_extent_tbl", fields)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  start <- if (nrow(rows) == 0L) NA_character_ else rows$start_value[1]
  end <- if (nrow(rows) == 0L) NA_character_ else rows$end_value[1]
  DBI::dbExecute(conn, "UPDATE entry_tbl SET temporal_coverage_start_date = ?, temporal_coverage_end_date = ? WHERE id = ?",
                 params = list(start, end, as.integer(dataset_id)))
  invisible(TRUE)
}

validate_equivalent_scale_denominator <- function(value) {
  text <- wave2_optional_text(value)
  if (is.na(text)) return(NA_integer_)
  numeric_value <- suppressWarnings(as.numeric(gsub(",", "", text, fixed = TRUE)))
  if (!is.finite(numeric_value) || numeric_value <= 0 || numeric_value != round(numeric_value) ||
      numeric_value > .Machine$integer.max) {
    stop("Equivalent-scale denominator must be a positive integer.", call. = FALSE)
  }
  as.integer(numeric_value)
}

get_dataset_release_rights <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave2_schema(conn)
  rows <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_license_tbl WHERE dataset_id = ?", params = list(as.integer(dataset_id)))
  if (nrow(rows) == 0L) NULL else rows[1, , drop = FALSE]
}

save_dataset_release_rights <- function(dataset_id, data, base_path, user_person_id = NA_integer_) {
  status <- normalise_embargo_status(data$embargo_status, default = "No")
  reason <- wave2_optional_text(data$embargo_reason)
  start <- wave2_optional_text(data$embargo_start_date)
  end <- wave2_optional_text(data$embargo_end_date)
  if (!is.na(start)) validate_iso_date_value(start, "day")
  if (!is.na(end)) validate_iso_date_value(end, "day")
  if (!is.na(start) && !is.na(end) && as.Date(start) > as.Date(end)) stop("Embargo start date cannot be later than the end date.", call. = FALSE)
  if (status == "No") { reason <- NA_character_; start <- NA_character_; end <- NA_character_ }
  dissemination <- wave2_optional_text(data$dissemination_constraint_code)
  if (!is.na(dissemination) && !dissemination %in% iso_dissemination_constraint_codes()) stop("Unknown dissemination-constraint code.", call. = FALSE)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave2_schema(conn)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, "
      INSERT INTO dataset_license_tbl (
        dataset_id, embargo_status, embargo_reason, embargo_start_date, embargo_end_date,
        release_addressee, release_statement, dissemination_constraint_code,
        updated_at, updated_by_person_id
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(dataset_id) DO UPDATE SET
        embargo_status = excluded.embargo_status,
        embargo_reason = excluded.embargo_reason,
        embargo_start_date = excluded.embargo_start_date,
        embargo_end_date = excluded.embargo_end_date,
        release_addressee = excluded.release_addressee,
        release_statement = excluded.release_statement,
        dissemination_constraint_code = excluded.dissemination_constraint_code,
        updated_at = excluded.updated_at,
        updated_by_person_id = excluded.updated_by_person_id
    ", params = list(
      as.integer(dataset_id), status, reason, start, end,
      wave2_optional_text(data$release_addressee), wave2_optional_text(data$release_statement),
      dissemination, now, wave2_optional_integer(user_person_id)
    ))
    DBI::dbExecute(conn, "UPDATE entry_tbl SET embargo_status = ?, embargo_reason = ?, embargo_end_date = ? WHERE id = ?",
                   params = list(status, reason, end, as.integer(dataset_id)))
  })
  invisible(TRUE)
}

save_iso_wave2_sections <- function(dataset_id, alternate_titles, online_resources,
                                    distribution_formats, quality_results, temporal_extents,
                                    base_path, user_person_id = NA_integer_) {
  replace_dataset_alternate_titles(dataset_id, alternate_titles, base_path, user_person_id)
  replace_dataset_online_resources(dataset_id, online_resources, base_path, user_person_id)
  replace_dataset_distribution_formats(dataset_id, distribution_formats, base_path, user_person_id)
  replace_dataset_quality_results(dataset_id, quality_results, base_path, user_person_id)
  replace_dataset_temporal_extents(dataset_id, temporal_extents, base_path, user_person_id)
  invisible(TRUE)
}

iso_wave2_status_rows <- function(conn, dataset_id, rec) {
  ensure_iso_wave2_schema(conn)
  count <- function(table, where = "") {
    DBI::dbGetQuery(
      conn,
      paste0("SELECT COUNT(*) AS n FROM ", table, " WHERE dataset_id = ? ", where),
      params = list(as.integer(dataset_id))
    )$n[1]
  }
  row <- function(section, status, missing = character(), saved = 0L, action) {
    data.table::data.table(
      section = section, status = status, status_label = status_label(status),
      required_missing = length(missing), missing_fields = paste(missing, collapse = "; "),
      saved_records = as.integer(saved), next_action = action
    )
  }

  alternate_n <- count("dataset_alternate_title_tbl")
  online_n <- count("dataset_online_resource_tbl")
  format_n <- count("dataset_distribution_format_tbl")
  quality_n <- count("dataset_quality_result_tbl")
  temporal_n <- count("dataset_temporal_extent_tbl")
  file_n <- count("data_file_tbl", "AND COALESCE(active, 1) = 1")
  legacy_url <- non_empty_scalar(rec$data_distribution_url[1])
  distribution_missing <- c(
    if (legacy_url && online_n == 0L) "Structured online resource",
    if (file_n > 0L && format_n == 0L) "Structured distribution format"
  )
  distribution_status <- if (online_n + format_n == 0L && !legacy_url && file_n == 0L) {
    "not_started"
  } else if (length(distribution_missing) == 0L) {
    "complete"
  } else {
    "partial"
  }

  scale <- suppressWarnings(as.integer(rec$equivalent_scale_denominator[1]))
  legacy_scale <- any(vapply(c("min_scale", "max_scale"), function(field) {
    field %in% names(rec) && non_empty_scalar(rec[[field]][1])
  }, logical(1)))
  scale_status <- if (!is.na(scale) && scale > 0L) "complete" else if (legacy_scale) "partial" else "not_started"

  rights <- DBI::dbGetQuery(
    conn, "SELECT * FROM dataset_license_tbl WHERE dataset_id = ?",
    params = list(as.integer(dataset_id))
  )
  embargoed <- nrow(rights) > 0L && identical(
    normalise_embargo_status(rights$embargo_status[1], default = "No"), "Yes"
  )
  rights_missing <- if (!embargoed) character() else c(
    if (!non_empty_scalar(rights$embargo_start_date[1])) "Embargo start date",
    if (!non_empty_scalar(rights$embargo_end_date[1])) "Embargo end date",
    if (!non_empty_scalar(rights$release_statement[1])) "Release statement",
    if (!non_empty_scalar(rights$dissemination_constraint_code[1])) "Dissemination constraint"
  )
  rights_status <- if (nrow(rights) == 0L) {
    "not_started"
  } else if (length(rights_missing) > 0L) {
    "partial"
  } else {
    "complete"
  }

  list(
    row(
      "Alternate titles", if (alternate_n > 0L) "complete" else "not_started",
      saved = alternate_n,
      action = if (alternate_n > 0L) "Review alternate titles if naming changes." else "Add recognised alternate titles when they exist."
    ),
    row(
      "Structured distribution", distribution_status, distribution_missing,
      online_n + format_n,
      if (distribution_status == "complete") "Review online resources and formats when distribution changes." else "Add explicit online resources and format specifications."
    ),
    row(
      "ISO data quality", if (quality_n > 0L) "complete" else "not_started",
      saved = quality_n,
      action = if (quality_n > 0L) "Review explicit measures when evaluation evidence changes." else "Add quality results only when a measure and evaluation method are known."
    ),
    row(
      "Equivalent scale", scale_status,
      saved = as.integer(!is.na(scale) && scale > 0L),
      action = if (scale_status == "complete") "Review the representative fraction when resolution changes." else if (legacy_scale) "Add one ISO representative-fraction denominator; the saved range remains an extension." else "Add an equivalent-scale denominator when scale is meaningful."
    ),
    row(
      "Temporal extents", if (temporal_n > 0L) "complete" else "not_started",
      saved = temporal_n,
      action = if (temporal_n > 0L) "Review time periods when coverage changes." else "Add one or more temporal extents when the resource has temporal coverage."
    ),
    row(
      "Rights and releasability", rights_status, rights_missing, nrow(rights),
      if (length(rights_missing) > 0L) paste("Complete:", paste(rights_missing, collapse = "; ")) else "Review releasability whenever access conditions change."
    )
  )
}
