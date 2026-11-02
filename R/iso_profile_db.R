# ISO metadata profile, discovery dates/categories, and responsibilities ----

iso_topic_category_codes <- function() {
  c(
    "farming", "biota", "boundaries", "climatologyMeteorologyAtmosphere",
    "economy", "elevation", "environment", "geoscientificInformation",
    "health", "imageryBaseMapsEarthCover", "intelligenceMilitary",
    "inlandWaters", "location", "oceans", "planningCadastre", "society",
    "structure", "transportation", "utilitiesCommunication"
  )
}

iso_metadata_scope_codes <- function() {
  c("dataset", "series", "service", "software", "model", "nonGeographicDataset")
}

iso_metadata_language_codes <- function() c("eng", "afr", "zul")
iso_character_encoding_codes <- function() c("utf8", "utf16")
iso_citation_date_type_codes <- function() c("creation", "publication", "revision")
iso_date_precision_codes <- function() c("year", "month", "day", "dateTime")
metadata_responsibility_role_codes <- function() {
  c("creator", "maintainer", "pointOfContact")
}

metadata_scope_requires_geographic_discovery <- function(scope_code) {
  identical(as.character(scope_code[1] %||% "dataset"), "dataset")
}

iso_optional_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NA_character_)
  value <- trimws(as.character(x[1]))
  if (nzchar(value)) value else NA_character_
}

iso_optional_integer <- function(x) {
  value <- suppressWarnings(as.integer(x[1]))
  if (length(value) == 0L || is.na(value)) NA_integer_ else value
}

infer_iso_date_precision <- function(value) {
  value <- iso_optional_text(value)
  if (is.na(value)) return("day")
  if (grepl("^\\d{4}$", value)) return("year")
  if (grepl("^\\d{4}-\\d{2}$", value)) return("month")
  if (grepl("[T ]\\d{2}:\\d{2}", value)) return("dateTime")
  "day"
}

validate_iso_date_value <- function(value, precision) {
  value <- iso_optional_text(value)
  precision <- iso_optional_text(precision)
  if (is.na(value)) stop("A citation date is required.", call. = FALSE)
  if (is.na(precision)) precision <- infer_iso_date_precision(value)
  patterns <- c(
    year = "^\\d{4}$",
    month = "^\\d{4}-\\d{2}$",
    day = "^\\d{4}-\\d{2}-\\d{2}$",
    dateTime = "^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}"
  )
  if (!precision %in% names(patterns) || !grepl(patterns[[precision]], value)) {
    stop("The citation date does not match its selected precision.", call. = FALSE)
  }
  if (precision %in% c("month", "day", "dateTime")) {
    date_part <- if (precision == "month") paste0(value, "-01") else substr(value, 1L, 10L)
    if (is.na(suppressWarnings(as.Date(date_part)))) {
      stop("The citation date is not a valid calendar date.", call. = FALSE)
    }
  }
  list(value = value, precision = precision)
}

ensure_iso_profile_schema <- function(conn, migrate = TRUE) {
  ensure_columns(
    conn, "entry_tbl",
    fields = c(
      "metadata_uuid", "metadata_resource_scope_code", "geographic_extent_name", "metadata_creation_date",
      "metadata_language_code", "metadata_character_encoding", "resource_language_code", "resource_character_encoding"
    ),
    types = c(
      metadata_uuid = "TEXT", metadata_resource_scope_code = "TEXT DEFAULT 'dataset'",
      geographic_extent_name = "TEXT", metadata_creation_date = "TEXT",
      resource_language_code = "TEXT", resource_character_encoding = "TEXT",
      metadata_language_code = "TEXT DEFAULT 'eng'",
      metadata_character_encoding = "TEXT DEFAULT 'utf8'"
    )
  )
  DBI::dbExecute(conn, "
    UPDATE entry_tbl SET
      metadata_resource_scope_code = COALESCE(NULLIF(TRIM(metadata_resource_scope_code), ''), 'dataset'),
      metadata_language_code = COALESCE(NULLIF(TRIM(metadata_language_code), ''), 'eng'),
      metadata_character_encoding = COALESCE(NULLIF(TRIM(metadata_character_encoding), ''), 'utf8')
  ")
  missing_uuid <- DBI::dbGetQuery(conn, "
    SELECT id, uuid FROM entry_tbl
    WHERE metadata_uuid IS NULL OR TRIM(metadata_uuid) = ''
    ORDER BY id
  ")
  if (nrow(missing_uuid) > 0L) {
    for (i in seq_len(nrow(missing_uuid))) {
      candidate <- uuid::UUIDgenerate()
      while (identical(candidate, missing_uuid$uuid[i])) candidate <- uuid::UUIDgenerate()
      DBI::dbExecute(
        conn, "UPDATE entry_tbl SET metadata_uuid = ? WHERE id = ?",
        params = list(candidate, missing_uuid$id[i])
      )
    }
  }
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_entry_metadata_uuid
    ON entry_tbl(metadata_uuid)
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_topic_category_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      topic_category_code TEXT NOT NULL CHECK(topic_category_code IN (
        'farming', 'biota', 'boundaries', 'climatologyMeteorologyAtmosphere',
        'economy', 'elevation', 'environment', 'geoscientificInformation',
        'health', 'imageryBaseMapsEarthCover', 'intelligenceMilitary',
        'inlandWaters', 'location', 'oceans', 'planningCadastre', 'society',
        'structure', 'transportation', 'utilitiesCommunication'
      )),
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
    CREATE UNIQUE INDEX IF NOT EXISTS idx_topic_category_dataset_code
    ON dataset_topic_category_tbl(dataset_id, topic_category_code)
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_topic_category_order
    ON dataset_topic_category_tbl(dataset_id, sort_order)
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_citation_date_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      date_value TEXT NOT NULL,
      date_type_code TEXT NOT NULL CHECK(date_type_code IN ('creation', 'publication', 'revision')),
      date_precision TEXT NOT NULL DEFAULT 'day' CHECK(date_precision IN ('year', 'month', 'day', 'dateTime')),
      date_note TEXT,
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
    CREATE UNIQUE INDEX IF NOT EXISTS idx_citation_date_order
    ON dataset_citation_date_tbl(dataset_id, sort_order)
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS metadata_responsibility_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      person_id INTEGER,
      organisation_id INTEGER,
      role_code TEXT NOT NULL CHECK(role_code IN ('creator', 'maintainer', 'pointOfContact')),
      is_primary INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (organisation_id) REFERENCES organisation_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_metadata_responsibility_identity
    ON metadata_responsibility_tbl(
      dataset_id, COALESCE(person_id, -1), COALESCE(organisation_id, -1), role_code
    )
  ")
  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_metadata_responsibility_primary_role
    ON metadata_responsibility_tbl(dataset_id, role_code)
    WHERE is_primary = 1
  ")

  ensure_legacy_patch_v1_marker(conn, "citation")
  if (isTRUE(migrate)) apply_legacy_patch_v1(conn, "citation")
  invisible(TRUE)
}

get_dataset_topic_categories <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  DBI::dbGetQuery(conn, "
    SELECT * FROM dataset_topic_category_tbl
    WHERE dataset_id = ? ORDER BY sort_order, id
  ", params = list(as.integer(dataset_id)))
}

replace_dataset_topic_categories <- function(
  dataset_id, codes, base_path, user_person_id = NA_integer_
) {
  codes <- unique(as.character(codes))
  codes <- codes[!is.na(codes) & nzchar(trimws(codes))]
  if (length(setdiff(codes, iso_topic_category_codes())) > 0L) {
    stop("Unknown ISO topic-category code.", call. = FALSE)
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  user_person_id <- iso_optional_integer(user_person_id)
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn, "DELETE FROM dataset_topic_category_tbl WHERE dataset_id = ?",
      params = list(as.integer(dataset_id))
    )
    for (i in seq_along(codes)) DBI::dbExecute(conn, "
      INSERT INTO dataset_topic_category_tbl (
        dataset_id, topic_category_code, sort_order, created_at,
        created_by_person_id, updated_at, updated_by_person_id
      ) VALUES (?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      as.integer(dataset_id), codes[i], i, now, user_person_id, now, user_person_id
    ))
  })
  invisible(TRUE)
}

get_dataset_citation_dates <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  DBI::dbGetQuery(conn, "
    SELECT * FROM dataset_citation_date_tbl
    WHERE dataset_id = ? ORDER BY sort_order, id
  ", params = list(as.integer(dataset_id)))
}

sync_primary_publication_date_conn <- function(conn, dataset_id) {
  primary <- DBI::dbGetQuery(conn, "
    SELECT date_value FROM dataset_citation_date_tbl
    WHERE dataset_id = ? AND date_type_code = 'publication'
    ORDER BY sort_order, id LIMIT 1
  ", params = list(as.integer(dataset_id)))
  value <- if (nrow(primary) == 0L) NA_character_ else primary$date_value[1]
  DBI::dbExecute(
    conn, "UPDATE entry_tbl SET citation_date = ? WHERE id = ?",
    params = list(value, as.integer(dataset_id))
  )
  invisible(value)
}

replace_dataset_citation_dates <- function(
  dataset_id, dates, base_path, user_person_id = NA_integer_,
  primary_publication_date = NULL
) {
  dates <- as.data.frame(dates %||% data.frame(), stringsAsFactors = FALSE)
  primary <- iso_optional_text(primary_publication_date)
  if (!is.na(primary)) {
    primary_valid <- validate_iso_date_value(primary, infer_iso_date_precision(primary))
    primary_row <- data.frame(
      date_value = primary_valid$value, date_type_code = "publication",
      date_precision = primary_valid$precision, date_note = NA_character_,
      stringsAsFactors = FALSE
    )
    dates <- rbind(primary_row, dates)
  }
  required <- c("date_value", "date_type_code", "date_precision", "date_note")
  for (field in setdiff(required, names(dates))) {
    dates[[field]] <- rep(NA_character_, nrow(dates))
  }
  validated <- vector("list", nrow(dates))
  if (nrow(dates) > 0L) for (i in seq_len(nrow(dates))) {
    type <- iso_optional_text(dates$date_type_code[i])
    if (is.na(type) || !type %in% iso_citation_date_type_codes()) {
      stop("Unknown ISO citation-date type.", call. = FALSE)
    }
    date <- validate_iso_date_value(dates$date_value[i], dates$date_precision[i])
    validated[[i]] <- data.frame(
      date_value = date$value, date_type_code = type, date_precision = date$precision,
      date_note = iso_optional_text(dates$date_note[i]), stringsAsFactors = FALSE
    )
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  user_person_id <- iso_optional_integer(user_person_id)
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn, "DELETE FROM dataset_citation_date_tbl WHERE dataset_id = ?",
      params = list(as.integer(dataset_id))
    )
    for (i in seq_along(validated)) {
      row <- validated[[i]]
      DBI::dbExecute(conn, "
        INSERT INTO dataset_citation_date_tbl (
          dataset_id, date_value, date_type_code, date_precision, date_note,
          sort_order, created_at, created_by_person_id, updated_at, updated_by_person_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(
        as.integer(dataset_id), row$date_value, row$date_type_code,
        row$date_precision, row$date_note, i, now, user_person_id, now, user_person_id
      ))
    }
    sync_primary_publication_date_conn(conn, dataset_id)
  })
  invisible(TRUE)
}

get_metadata_responsibilities <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  DBI::dbGetQuery(conn, "
    SELECT r.*,
      TRIM(COALESCE(p.name_first, '') || ' ' || COALESCE(p.name_last, '')) AS person_name,
      COALESCE(o.organisation_name_short, o.organisation_name_full) AS organisation_name
    FROM metadata_responsibility_tbl r
    LEFT JOIN persons_tbl p ON p.id = r.person_id
    LEFT JOIN organisation_tbl o ON o.id = r.organisation_id
    WHERE r.dataset_id = ?
    ORDER BY r.role_code, r.is_primary DESC, r.id
  ", params = list(as.integer(dataset_id)))
}

replace_metadata_responsibilities <- function(
  dataset_id, responsibilities, base_path, user_person_id = NA_integer_
) {
  rows <- as.data.frame(responsibilities %||% data.frame(), stringsAsFactors = FALSE)
  required <- c("person_id", "organisation_id", "role_code", "is_primary")
  for (field in setdiff(required, names(rows))) rows[[field]] <- rep(NA, nrow(rows))
  if (nrow(rows) > 0L) {
    rows$person_id <- vapply(rows$person_id, iso_optional_integer, integer(1))
    rows$organisation_id <- vapply(rows$organisation_id, iso_optional_integer, integer(1))
    rows$role_code <- as.character(rows$role_code)
    rows$is_primary <- as.integer(as.logical(rows$is_primary))
    if (any(is.na(rows$person_id) & is.na(rows$organisation_id))) {
      stop("Every metadata responsibility needs a person, organisation, or both.", call. = FALSE)
    }
    if (any(!rows$role_code %in% metadata_responsibility_role_codes())) {
      stop("Unknown metadata-responsibility role.", call. = FALSE)
    }
    for (role in unique(rows$role_code)) {
      primary <- which(rows$role_code == role & rows$is_primary == 1L)
      if (length(primary) > 1L) rows$is_primary[primary[-1L]] <- 0L
    }
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  user_person_id <- iso_optional_integer(user_person_id)
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn, "DELETE FROM metadata_responsibility_tbl WHERE dataset_id = ?",
      params = list(as.integer(dataset_id))
    )
    if (nrow(rows) > 0L) for (i in seq_len(nrow(rows))) DBI::dbExecute(conn, "
      INSERT INTO metadata_responsibility_tbl (
        dataset_id, person_id, organisation_id, role_code, is_primary,
        created_at, created_by_person_id, updated_at, updated_by_person_id
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      as.integer(dataset_id), rows$person_id[i], rows$organisation_id[i],
      rows$role_code[i], rows$is_primary[i], now, user_person_id, now, user_person_id
    ))
  })
  invisible(TRUE)
}

metadata_responsibilities_with_default <- function(
  responsibilities,
  user_person_id
) {
  rows <- as.data.frame(
    responsibilities %||% data.frame(),
    stringsAsFactors = FALSE
  )
  required <- c(
    "person_id", "organisation_id", "role_code", "is_primary",
    "person_name", "organisation_name"
  )
  for (field in setdiff(required, names(rows))) {
    rows[[field]] <- rep(NA, nrow(rows))
  }
  rows <- rows[, required, drop = FALSE]

  has_primary_contact <- nrow(rows) > 0L && any(
    rows$role_code == "pointOfContact" &
      suppressWarnings(as.integer(rows$is_primary)) == 1L,
    na.rm = TRUE
  )
  person_id <- iso_optional_integer(user_person_id)
  if (has_primary_contact || is.na(person_id)) return(rows)

  rbind(
    rows,
    data.frame(
      person_id = person_id,
      organisation_id = NA_integer_,
      role_code = "pointOfContact",
      is_primary = 1L,
      person_name = "Active editor (automatic)",
      organisation_name = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

save_iso_profile_sections <- function(
  dataset_id, profile, topic_categories, citation_dates, responsibilities,
  primary_publication_date, base_path, user_person_id = NA_integer_
) {
  scope <- iso_optional_text(profile$metadata_resource_scope_code)
  language <- iso_optional_text(profile$metadata_language_code)
  resource_language <- iso_optional_text(profile$resource_language_code)
  if (!is.na(resource_language) && !resource_language %in% c(iso_metadata_language_codes(), "und"))
    stop("Unknown dataset language.", call. = FALSE)
  resource_encoding <- iso_optional_text(profile$resource_character_encoding)
  if (!is.na(resource_encoding) && !resource_encoding %in% iso_character_encoding_codes())
    stop("Unknown dataset character encoding.", call. = FALSE)
  encoding <- iso_optional_text(profile$metadata_character_encoding)
  creation <- iso_optional_text(profile$metadata_creation_date)
  if (!is.na(creation)) creation <- iso_export_date(creation, "Metadata creation date")$value
  metadata_uuid <- iso_optional_text(profile$metadata_uuid)
  if (is.na(metadata_uuid)) metadata_uuid <- uuid::UUIDgenerate()
  if (!grepl(
    "^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$",
    metadata_uuid
  )) {
    stop("Metadata-record UUID must use canonical UUID format.", call. = FALSE)
  }
  if (!scope %in% iso_metadata_scope_codes()) stop("Unknown metadata resource scope.")
  if (!language %in% iso_metadata_language_codes()) stop("Unknown metadata language.")
  if (!encoding %in% iso_character_encoding_codes()) stop("Unknown character encoding.")
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_iso_profile_schema(conn)
  entry_identity <- DBI::dbGetQuery(
    conn, "SELECT uuid FROM entry_tbl WHERE id = ?",
    params = list(as.integer(dataset_id))
  )
  if (nrow(entry_identity) != 1L) stop("Dataset record not found.", call. = FALSE)
  if (identical(metadata_uuid, iso_optional_text(entry_identity$uuid))) {
    stop("The metadata-record UUID must differ from the resource UUID.", call. = FALSE)
  }
  duplicate_uuid <- DBI::dbGetQuery(conn, "
    SELECT id FROM entry_tbl WHERE metadata_uuid = ? AND id != ? LIMIT 1
  ", params = list(metadata_uuid, as.integer(dataset_id)))
  if (nrow(duplicate_uuid) > 0L) {
    stop("The metadata-record UUID is already used by another record.", call. = FALSE)
  }
  DBI::dbExecute(conn, "
    UPDATE entry_tbl SET metadata_uuid = ?, metadata_resource_scope_code = ?,
      metadata_language_code = ?, metadata_character_encoding = ?
    WHERE id = ?
  ", params = list(metadata_uuid, scope, language, encoding, as.integer(dataset_id)))
  if ("resource_language_code" %in% names(profile)) DBI::dbExecute(conn,
    "UPDATE entry_tbl SET resource_language_code = ? WHERE id = ?",
    params = list(resource_language, as.integer(dataset_id)))
  if ("resource_character_encoding" %in% names(profile)) DBI::dbExecute(conn,
    "UPDATE entry_tbl SET resource_character_encoding = ? WHERE id = ?",
    params = list(resource_encoding, as.integer(dataset_id)))
  for (field in intersect(c("geographic_extent_name", "metadata_creation_date"), names(profile))) {
    value <- if (field == "metadata_creation_date") creation else iso_optional_text(profile[[field]])
    DBI::dbExecute(conn, paste0("UPDATE entry_tbl SET ", field, " = ? WHERE id = ?"),
      params = list(value, as.integer(dataset_id)))
  }
  replace_dataset_topic_categories(dataset_id, topic_categories, base_path, user_person_id)
  replace_dataset_citation_dates(
    dataset_id, citation_dates, base_path, user_person_id, primary_publication_date
  )
  replace_metadata_responsibilities(
    dataset_id, responsibilities, base_path, user_person_id
  )
  invisible(metadata_uuid)
}
