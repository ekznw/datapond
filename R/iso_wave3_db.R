# ISO metadata Wave 3 persistence ------------------------------------------

iso_vertical_crs_types <- function() c("geoidal", "depth", "barometric", "other")

wave3_optional_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NA_character_)
  value <- trimws(as.character(x[1]))
  if (nzchar(value)) value else NA_character_
}

wave3_optional_integer <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NA_integer_)
  value <- suppressWarnings(as.integer(x[1]))
  if (is.na(value)) NA_integer_ else value
}

wave3_text_default <- function(x, default) {
  value <- wave3_optional_text(x)
  if (is.na(value)) default else value
}

wave3_valid_uri <- function(x) {
  value <- wave3_optional_text(x)
  is.na(value) || grepl("^[A-Za-z][A-Za-z0-9+.-]*:[^[:space:]]+$", value)
}

canonical_keyword_field_code <- function(field_code) {
  value <- wave3_optional_text(field_code)
  if (is.na(value)) return(value)
  if (identical(tolower(value), "keyword_topic")) "keyword_object" else value
}

merge_keyword_theme_values <- function(object_values, topic_values) {
  values <- c(split_keyword_values(object_values), split_keyword_values(topic_values))
  values <- trimws(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  values <- values[!duplicated(tolower(values))]
  if (length(values)) paste(values, collapse = "; ") else NA_character_
}

ensure_iso_wave3_schema <- function(conn, migrate = TRUE) {
  ensure_columns(
    conn, "organisation_tbl",
    fields = c("parent_organisation_id", "organisational_unit_name"),
    types = c(parent_organisation_id = "INTEGER", organisational_unit_name = "TEXT")
  )
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS party_address_tbl (
      id INTEGER PRIMARY KEY,
      person_id INTEGER,
      organisation_id INTEGER,
      delivery_point TEXT,
      city TEXT,
      administrative_area TEXT,
      postal_code TEXT,
      country TEXT,
      electronic_mail_address TEXT,
      phone_number TEXT,
      online_resource TEXT,
      is_primary INTEGER NOT NULL DEFAULT 0 CHECK(is_primary IN (0, 1)),
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CHECK((person_id IS NOT NULL AND organisation_id IS NULL) OR
            (person_id IS NULL AND organisation_id IS NOT NULL)),
      FOREIGN KEY (person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (organisation_id) REFERENCES organisation_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_party_address_person ON party_address_tbl(person_id)")
  DBI::dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_party_address_org ON party_address_tbl(organisation_id)")
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS keyword_thesaurus_tbl (
      id INTEGER PRIMARY KEY,
      field_code TEXT NOT NULL UNIQUE COLLATE NOCASE,
      citation_title TEXT NOT NULL,
      edition TEXT,
      edition_date TEXT,
      edition_date_type_code TEXT NOT NULL DEFAULT 'revision',
      thesaurus_uri TEXT,
      identifier TEXT,
      responsible_person_id INTEGER,
      responsible_organisation_id INTEGER,
      responsible_role_code TEXT NOT NULL DEFAULT 'publisher',
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (responsible_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (responsible_organisation_id) REFERENCES organisation_tbl(id)
    )
  ")
  ensure_columns(
    conn, "keyword_vocab_tbl", "thesaurus_id",
    types = c(thesaurus_id = "INTEGER")
  )

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_vertical_extent_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      extent_label TEXT,
      minimum_value REAL NOT NULL,
      maximum_value REAL NOT NULL,
      unit_symbol TEXT NOT NULL,
      unit_uri TEXT,
      vertical_crs_name TEXT NOT NULL,
      vertical_crs_identifier TEXT,
      vertical_crs_uri TEXT,
      vertical_crs_type TEXT NOT NULL DEFAULT 'geoidal'
        CHECK(vertical_crs_type IN ('geoidal', 'depth', 'barometric', 'other')),
      description TEXT,
      sort_order INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      CHECK(minimum_value <= maximum_value),
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "CREATE UNIQUE INDEX IF NOT EXISTS idx_vertical_dataset_order ON dataset_vertical_extent_tbl(dataset_id, sort_order)")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_feature_catalogue_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL UNIQUE,
      catalogue_uuid TEXT NOT NULL UNIQUE,
      title TEXT NOT NULL,
      version TEXT,
      version_date TEXT,
      language_code TEXT NOT NULL DEFAULT 'eng',
      producer_person_id INTEGER,
      producer_organisation_id INTEGER,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (producer_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (producer_organisation_id) REFERENCES organisation_tbl(id)
    )
  ")
  ensure_legacy_patch_v1_marker(conn, "parties")
  if (isTRUE(migrate)) apply_legacy_patch_v1(conn, c("keywords", "parties"))
  invisible(TRUE)
}

get_dataset_vertical_extents <- function(dataset_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  DBI::dbGetQuery(conn, "SELECT * FROM dataset_vertical_extent_tbl WHERE dataset_id = ? ORDER BY sort_order, id",
                  params = list(as.integer(dataset_id)))
}

validate_vertical_extent_row <- function(row) {
  minimum <- suppressWarnings(as.numeric(row$minimum_value[1]))
  maximum <- suppressWarnings(as.numeric(row$maximum_value[1]))
  if (!is.finite(minimum) || !is.finite(maximum)) stop("Vertical minimum and maximum must be finite numbers.", call. = FALSE)
  if (minimum > maximum) stop("Vertical minimum cannot exceed the maximum.", call. = FALSE)
  unit <- wave3_optional_text(row$unit_symbol)
  crs <- wave3_optional_text(row$vertical_crs_name)
  if (is.na(unit) || is.na(crs)) stop("Vertical unit and vertical CRS name are required.", call. = FALSE)
  type <- wave3_optional_text(row$vertical_crs_type)
  if (is.na(type)) type <- "geoidal"
  if (!type %in% iso_vertical_crs_types()) stop("Unknown vertical CRS type.", call. = FALSE)
  for (field in c("unit_uri", "vertical_crs_uri")) {
    if (!wave3_valid_uri(row[[field]])) stop(paste(gsub("_", " ", field), "must be an absolute URI."), call. = FALSE)
  }
  row$minimum_value <- minimum; row$maximum_value <- maximum
  row$unit_symbol <- unit; row$vertical_crs_name <- crs; row$vertical_crs_type <- type
  row
}

replace_dataset_vertical_extents <- function(dataset_id, rows, base_path, user_person_id = NA_integer_) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  if (is.null(rows)) rows <- data.frame()
  checked <- lapply(seq_len(nrow(rows)), function(i) validate_vertical_extent_row(rows[i, , drop = FALSE]))
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, "DELETE FROM dataset_vertical_extent_tbl WHERE dataset_id = ?", params = list(as.integer(dataset_id)))
    for (i in seq_along(checked)) {
      row <- checked[[i]]
      DBI::dbExecute(conn, "
        INSERT INTO dataset_vertical_extent_tbl (
          dataset_id, extent_label, minimum_value, maximum_value, unit_symbol,
          unit_uri, vertical_crs_name, vertical_crs_identifier, vertical_crs_uri,
          vertical_crs_type, description, sort_order, created_by_person_id, updated_by_person_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", params = list(
          as.integer(dataset_id), wave3_optional_text(row$extent_label), row$minimum_value,
          row$maximum_value, row$unit_symbol, wave3_optional_text(row$unit_uri),
          row$vertical_crs_name, wave3_optional_text(row$vertical_crs_identifier),
          wave3_optional_text(row$vertical_crs_uri), row$vertical_crs_type,
          wave3_optional_text(row$description), as.integer(i), wave3_optional_integer(user_person_id),
          wave3_optional_integer(user_person_id)
        ))
    }
  })
  invisible(TRUE)
}

get_keyword_thesauri <- function(base_path, field_code = NULL) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  if (is.null(field_code) || is.na(wave3_optional_text(field_code))) {
    return(DBI::dbGetQuery(conn, "SELECT * FROM keyword_thesaurus_tbl ORDER BY field_code"))
  }
  DBI::dbGetQuery(conn, "SELECT * FROM keyword_thesaurus_tbl WHERE field_code = ? COLLATE NOCASE",
                  params = list(canonical_keyword_field_code(field_code)))
}

save_keyword_thesaurus <- function(base_path, data) {
  field <- canonical_keyword_field_code(data$field_code); title <- wave3_optional_text(data$citation_title)
  if (is.na(field) || is.na(title)) stop("Keyword group and thesaurus citation title are required.", call. = FALSE)
  if (!field %in% c("keyword_object", "keyword_discipline", "keyword_geo")) {
    stop("Unknown keyword group for thesaurus citation.", call. = FALSE)
  }
  if (!wave3_valid_uri(data$thesaurus_uri)) stop("Thesaurus URI must be absolute.", call. = FALSE)
  date_type <- wave3_text_default(data$edition_date_type_code, "revision")
  if (!date_type %in% iso_citation_date_type_codes()) stop("Unknown thesaurus citation date type.", call. = FALSE)
  if (!is.na(wave3_optional_text(data$edition_date))) validate_iso_date_value(data$edition_date, "day")
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  DBI::dbExecute(conn, "
    INSERT INTO keyword_thesaurus_tbl (
      field_code, citation_title, edition, edition_date, edition_date_type_code,
      thesaurus_uri, identifier, responsible_person_id, responsible_organisation_id,
      responsible_role_code, updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ON CONFLICT(field_code) DO UPDATE SET
      citation_title=excluded.citation_title, edition=excluded.edition,
      edition_date=excluded.edition_date, edition_date_type_code=excluded.edition_date_type_code,
      thesaurus_uri=excluded.thesaurus_uri, identifier=excluded.identifier,
      responsible_person_id=excluded.responsible_person_id,
      responsible_organisation_id=excluded.responsible_organisation_id,
      responsible_role_code=excluded.responsible_role_code, updated_at=CURRENT_TIMESTAMP
  ", params = list(field, title, wave3_optional_text(data$edition), wave3_optional_text(data$edition_date),
                    date_type,
                    wave3_optional_text(data$thesaurus_uri), wave3_optional_text(data$identifier),
                    wave3_optional_integer(data$responsible_person_id), wave3_optional_integer(data$responsible_organisation_id),
                    wave3_text_default(data$responsible_role_code, "publisher")))
  id <- DBI::dbGetQuery(conn, "SELECT id FROM keyword_thesaurus_tbl WHERE field_code = ? COLLATE NOCASE", params = list(field))$id[1]
  DBI::dbExecute(conn, "UPDATE keyword_vocab_tbl SET thesaurus_id = ? WHERE field_code = ? COLLATE NOCASE", params = list(id, field))
  as.integer(id)
}

get_party_addresses <- function(base_path, person_id = NULL, organisation_id = NULL) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  if (!is.null(person_id) && !is.na(wave3_optional_integer(person_id))) {
    return(DBI::dbGetQuery(conn, "SELECT * FROM party_address_tbl WHERE person_id = ? ORDER BY is_primary DESC, id", params = list(as.integer(person_id))))
  }
  DBI::dbGetQuery(conn, "SELECT * FROM party_address_tbl WHERE organisation_id = ? ORDER BY is_primary DESC, id", params = list(as.integer(organisation_id)))
}

replace_party_addresses <- function(base_path, rows, person_id = NULL, organisation_id = NULL) {
  person_id <- wave3_optional_integer(person_id); organisation_id <- wave3_optional_integer(organisation_id)
  if (is.na(person_id) == is.na(organisation_id)) stop("Supply exactly one person or organisation.", call. = FALSE)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn); if (is.null(rows)) rows <- data.frame()
  DBI::dbWithTransaction(conn, {
    if (!is.na(person_id)) DBI::dbExecute(conn, "DELETE FROM party_address_tbl WHERE person_id = ?", params = list(person_id))
    else DBI::dbExecute(conn, "DELETE FROM party_address_tbl WHERE organisation_id = ?", params = list(organisation_id))
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      if (!wave3_valid_uri(row$online_resource)) stop("Address online resource must be an absolute URI.", call. = FALSE)
      DBI::dbExecute(conn, "INSERT INTO party_address_tbl (
        person_id, organisation_id, delivery_point, city, administrative_area,
        postal_code, country, electronic_mail_address, phone_number, online_resource, is_primary
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", params = list(
        person_id, organisation_id, wave3_optional_text(row$delivery_point), wave3_optional_text(row$city),
        wave3_optional_text(row$administrative_area), wave3_optional_text(row$postal_code), wave3_optional_text(row$country),
        wave3_optional_text(row$electronic_mail_address), wave3_optional_text(row$phone_number),
        wave3_optional_text(row$online_resource), as.integer(i == 1L || isTRUE(row$is_primary[1] == 1L))
      ))
    }
  })
  invisible(TRUE)
}

get_dataset_feature_catalogue <- function(dataset_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  DBI::dbGetQuery(conn, "SELECT * FROM dataset_feature_catalogue_tbl WHERE dataset_id = ?", params = list(as.integer(dataset_id)))
}

save_dataset_feature_catalogue <- function(dataset_id, data, base_path, user_person_id = NA_integer_) {
  title <- wave3_optional_text(data$title)
  if (is.na(title)) stop("Feature catalogue title is required.", call. = FALSE)
  version_date <- wave3_optional_text(data$version_date)
  if (!is.na(version_date)) validate_iso_date_value(version_date, "day")
  language <- wave3_text_default(data$language_code, "eng")
  if (!language %in% iso_metadata_language_codes()) stop("Unknown feature catalogue language.", call. = FALSE)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  existing <- DBI::dbGetQuery(conn, "SELECT catalogue_uuid FROM dataset_feature_catalogue_tbl WHERE dataset_id = ?", params = list(as.integer(dataset_id)))
  catalogue_uuid <- wave3_optional_text(data$catalogue_uuid)
  if (is.na(catalogue_uuid)) catalogue_uuid <- if (nrow(existing)) existing$catalogue_uuid[1] else new_uuid()
  DBI::dbExecute(conn, "INSERT INTO dataset_feature_catalogue_tbl (
    dataset_id, catalogue_uuid, title, version, version_date, language_code,
    producer_person_id, producer_organisation_id, created_by_person_id, updated_by_person_id, updated_at
  ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
  ON CONFLICT(dataset_id) DO UPDATE SET title=excluded.title, version=excluded.version,
    version_date=excluded.version_date, language_code=excluded.language_code,
    producer_person_id=excluded.producer_person_id, producer_organisation_id=excluded.producer_organisation_id,
    updated_by_person_id=excluded.updated_by_person_id, updated_at=CURRENT_TIMESTAMP",
    params = list(as.integer(dataset_id), catalogue_uuid, title, wave3_optional_text(data$version),
                  version_date, language,
                  wave3_optional_integer(data$producer_person_id), wave3_optional_integer(data$producer_organisation_id),
                  wave3_optional_integer(user_person_id), wave3_optional_integer(user_person_id)))
  invisible(catalogue_uuid)
}

save_iso_wave3_sections <- function(dataset_id, vertical_extents, feature_catalogue,
                                    base_path, user_person_id = NA_integer_) {
  replace_dataset_vertical_extents(dataset_id, vertical_extents, base_path, user_person_id)
  title <- wave3_optional_text(feature_catalogue$title)
  if (is.na(title)) {
    conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
    ensure_iso_wave3_schema(conn)
    DBI::dbExecute(conn, "DELETE FROM dataset_feature_catalogue_tbl WHERE dataset_id = ?", params = list(as.integer(dataset_id)))
  } else {
    save_dataset_feature_catalogue(dataset_id, feature_catalogue, base_path, user_person_id)
  }
  invisible(TRUE)
}

dataset_feature_catalogue_attributes <- function(dataset_id, base_path, coverage = FALSE) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  raster <- c("raster", "virtual_raster", "esri_arcinfo_grid")
  placeholders <- paste(rep("?", length(raster)), collapse = ",")
  comparator <- if (isTRUE(coverage)) "IN" else "NOT IN"
  sql <- paste0("SELECT a.*, COALESCE(df.data_file_format, 'attribute_table') AS data_file_format,
    COALESCE(NULLIF(df.file_name, ''), a.data_file_name, a.data_file_rel_path_full, 'Dataset') AS feature_type_name
    FROM dataset_attribute_tbl a LEFT JOIN data_file_tbl df ON df.id = a.data_file_id
    WHERE a.dataset_id = ? AND LOWER(COALESCE(df.data_file_format, 'attribute_table')) ", comparator,
    " (", placeholders, ") ORDER BY COALESCE(a.data_file_id, 0), COALESCE(a.layer_name, ''), COALESCE(a.attribute_order, a.id), a.id")
  DBI::dbGetQuery(conn, sql, params = c(list(as.integer(dataset_id)), as.list(raster)))
}

iso_wave3_status_rows <- function(conn, dataset_id, rec) {
  count <- function(table) DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) AS n FROM ", table, " WHERE dataset_id = ?"), params = list(as.integer(dataset_id)))$n[1]
  row <- function(section, status, saved, action, missing = character()) data.frame(
    section = section, status = status, missing_fields = paste(missing, collapse = "; "),
    saved_records = as.integer(saved), next_action = action, stringsAsFactors = FALSE
  )
  vertical_n <- count("dataset_vertical_extent_tbl")
  feature_n <- count("dataset_feature_catalogue_tbl")
  attrs <- dataset_feature_catalogue_attributes(dataset_id, dirname(DBI::dbGetInfo(conn)$dbname), FALSE)
  keyword_fields <- intersect(c("keyword_object", "keyword_discipline", "keyword_geo"), names(rec))
  used_keyword_fields <- keyword_fields[vapply(keyword_fields, function(field) non_empty_scalar(rec[[field]][1]), logical(1))]
  cited_fields <- if (length(used_keyword_fields)) DBI::dbGetQuery(conn,
    paste0("SELECT field_code FROM keyword_thesaurus_tbl WHERE field_code IN (",
           paste(rep("?", length(used_keyword_fields)), collapse = ","), ")"),
    params = as.list(used_keyword_fields))$field_code else character()
  missing_thesauri <- setdiff(used_keyword_fields, cited_fields)
  contact_detail_n <- DBI::dbGetQuery(conn, "
    WITH linked_people AS (
      SELECT person_id AS id FROM metadata_responsibility_tbl WHERE dataset_id = ? AND person_id IS NOT NULL
      UNION SELECT person_id FROM poc_tbl WHERE dataset_id = ? AND person_id IS NOT NULL
    ), linked_orgs AS (
      SELECT organisation_id AS id FROM metadata_responsibility_tbl WHERE dataset_id = ? AND organisation_id IS NOT NULL
      UNION SELECT organisation_id FROM poc_tbl WHERE dataset_id = ? AND organisation_id IS NOT NULL
      UNION SELECT organisation_id FROM dataset_organisation_tbl WHERE dataset_id = ? AND organisation_id IS NOT NULL
    )
    SELECT
      (SELECT COUNT(*) FROM party_address_tbl a
       WHERE a.person_id IN (SELECT id FROM linked_people) OR a.organisation_id IN (SELECT id FROM linked_orgs)) +
      (SELECT COUNT(*) FROM organisation_tbl o WHERE o.id IN (SELECT id FROM linked_orgs)
       AND (o.parent_organisation_id IS NOT NULL OR NULLIF(TRIM(o.organisational_unit_name), '') IS NOT NULL)) AS n
  ", params = rep(list(as.integer(dataset_id)), 5L))$n[1]
  list(
    row("Vertical extent", if (vertical_n) "complete" else "not_started", vertical_n,
        if (vertical_n) "Review vertical CRS and units when elevation coverage changes." else "Add a vertical extent only when height or depth is meaningful."),
    row("Feature catalogue", if (!nrow(attrs)) "not_applicable" else if (feature_n) "complete" else "partial", feature_n,
        if (!nrow(attrs)) "No vector or tabular attributes require an ISO 19110 catalogue." else if (feature_n) "Regenerate the catalogue after attribute changes." else "Configure the ISO 19110 catalogue for the saved vector or tabular attributes.",
        if (nrow(attrs) && !feature_n) "Catalogue identity" else character()),
    row("Keyword thesauri", if (!length(used_keyword_fields)) "not_applicable" else if (!length(missing_thesauri)) "complete" else "partial",
        length(cited_fields), if (!length(used_keyword_fields)) "No keyword groups currently require a thesaurus citation." else if (!length(missing_thesauri)) "Review citations when controlled vocabularies change." else "Add pond-level thesaurus citations for the populated keyword groups.",
        missing_thesauri),
    row("Structured contact details", if (contact_detail_n > 0L) "complete" else "not_applicable", contact_detail_n,
        if (contact_detail_n > 0L) "Review normalized addresses and organisation hierarchy when contacts change." else "Add structured address or hierarchy details only when detailed contact export requires them.")
  )
}
