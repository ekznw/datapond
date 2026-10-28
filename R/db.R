get_db_path <- function(base_path) {
  file.path(base_path, "metadata.db")
}

get_conn <- function(base_path) {
  movement_assert_writable(base_path)
  conn <- dbConnect(RSQLite::SQLite(), get_db_path(base_path))
  dbExecute(conn, "PRAGMA foreign_keys = ON")
  conn
}

normalise_optional_user_id <- function(user_id) {
  if (
    is.null(user_id) ||
    length(user_id) == 0L ||
    is.na(user_id[1]) ||
    !nzchar(trimws(as.character(user_id[1])))
  ) {
    return(NA_integer_)
  }

  as.integer(user_id[1])
}

ensure_keyword_vocab_table <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS keyword_vocab_tbl (
      id INTEGER PRIMARY KEY,
      field_code TEXT NOT NULL,
      option_value TEXT NOT NULL,
      option_label TEXT,
      sort_order INTEGER,
      is_active INTEGER NOT NULL DEFAULT 1,
      source TEXT NOT NULL DEFAULT 'seed_csv',
      approval_status TEXT NOT NULL DEFAULT 'approved',
      created_at TEXT,
      created_by_user_id INTEGER,
      updated_at TEXT,
      updated_by_user_id INTEGER
    )
  ")

  ensure_columns(
    conn = conn,
    table = "keyword_vocab_tbl",
    fields = c(
      "is_active",
      "source",
      "approval_status",
      "created_at",
      "created_by_user_id",
      "updated_at",
      "updated_by_user_id"
    ),
    types = c(
      is_active = "INTEGER",
      source = "TEXT",
      approval_status = "TEXT",
      created_at = "TEXT",
      created_by_user_id = "INTEGER",
      updated_at = "TEXT",
      updated_by_user_id = "INTEGER"
    )
  )

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(
    conn,
    "
    UPDATE keyword_vocab_tbl
    SET
      field_code = TRIM(field_code),
      option_value = TRIM(option_value),

      option_label = COALESCE(
        NULLIF(TRIM(option_label), ''),
        TRIM(option_value)
      ),

      is_active = COALESCE(is_active, 1),

      source = COALESCE(
        NULLIF(TRIM(source), ''),
        'seed_csv'
      ),

      approval_status = CASE
        WHEN LOWER(
          TRIM(
            COALESCE(approval_status, '')
          )
        ) IN (
          'approved',
          'pending',
          'rejected'
        )
        THEN LOWER(
          TRIM(approval_status)
        )
        ELSE 'approved'
      END,

      created_at = COALESCE(
        created_at,
        ?
      ),

      updated_at = COALESCE(
        updated_at,
        ?
      )
    ",
    params = list(
      now,
      now
    )
  )

  # Retain the oldest row when legacy duplicates exist.
  DBI::dbExecute(
    conn,
    "
    DELETE FROM keyword_vocab_tbl
    WHERE id IN (
      SELECT id
      FROM (
        SELECT
          id,

          ROW_NUMBER() OVER (
            PARTITION BY
              LOWER(TRIM(field_code)),
              LOWER(TRIM(option_value))

            ORDER BY
              CASE
                WHEN source IN (
                  'admin_override',
                  'admin_created'
                )
                THEN 1

                WHEN source = 'seed_csv'
                THEN 2

                ELSE 3
              END,

              id
          ) AS duplicate_number

        FROM keyword_vocab_tbl
      )

      WHERE duplicate_number > 1
    )
    "
  )

  DBI::dbExecute(
    conn,
    "
    DROP INDEX IF EXISTS
      idx_keyword_vocab_field_value
    "
  )

  DBI::dbExecute(
    conn,
    "
    CREATE UNIQUE INDEX
      idx_keyword_vocab_field_value

    ON keyword_vocab_tbl (
      field_code COLLATE NOCASE,
      option_value COLLATE NOCASE
    )
    "
  )

  invisible(TRUE)
}

ensure_organisation_code_not_unique <- function(conn) {
  indexes <- DBI::dbGetQuery(
    conn,
    "PRAGMA index_list('organisation_tbl')"
  )

  unique_indexes <- indexes[
    as.integer(indexes[["unique"]]) == 1L,
    ,
    drop = FALSE
  ]
  code_is_unique <- any(vapply(
    unique_indexes$name,
    function(index_name) {
      index_fields <- DBI::dbGetQuery(
        conn,
        paste0(
          "PRAGMA index_info(",
          DBI::dbQuoteString(conn, index_name),
          ")"
        )
      )
      identical(
        as.character(index_fields$name),
        "organisation_code"
      )
    },
    logical(1)
  ))

  if (!code_is_unique) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "PRAGMA foreign_keys = OFF")
  on.exit(
    DBI::dbExecute(conn, "PRAGMA foreign_keys = ON"),
    add = TRUE
  )

  DBI::dbWithTransaction(
    conn,
    {
      DBI::dbExecute(
        conn,
        "
        CREATE TABLE organisation_tbl_rebuild (
          id INTEGER PRIMARY KEY,
          organisation_name_full TEXT,
          organisation_name_short TEXT,
          organisation_code TEXT NOT NULL,
          organisation_department TEXT,
          organisation_address TEXT,
          parent_organisation_id INTEGER,
          organisational_unit_name TEXT,
          FOREIGN KEY (parent_organisation_id) REFERENCES organisation_tbl_rebuild(id)
        )
        "
      )
      DBI::dbExecute(
        conn,
        "
        INSERT INTO organisation_tbl_rebuild (
          id,
          organisation_name_full,
          organisation_name_short,
          organisation_code,
          organisation_department,
          organisation_address,
          parent_organisation_id,
          organisational_unit_name
        )
        SELECT
          id,
          organisation_name_full,
          organisation_name_short,
          organisation_code,
          organisation_department,
          organisation_address,
          parent_organisation_id,
          organisational_unit_name
        FROM organisation_tbl
        "
      )
      DBI::dbExecute(conn, "DROP TABLE organisation_tbl")
      DBI::dbExecute(
        conn,
        "ALTER TABLE organisation_tbl_rebuild RENAME TO organisation_tbl"
      )
    }
  )

  invisible(TRUE)
}


ensure_dataset_organisation_role_schema <- function(conn) {
  if (!DBI::dbExistsTable(conn, "dataset_organisation_tbl")) {
    return(invisible(FALSE))
  }

  # Preserve distinct legacy roles while removing only exact duplicates.
  DBI::dbExecute(
    conn,
    "
    DELETE FROM dataset_organisation_tbl
    WHERE id NOT IN (
      SELECT MIN(id)
      FROM dataset_organisation_tbl
      GROUP BY dataset_id, organisation_id, LOWER(TRIM(role))
    )
    "
  )
  DBI::dbExecute(
    conn,
    "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_dataset_org_role
    ON dataset_organisation_tbl(
      dataset_id,
      organisation_id,
      LOWER(TRIM(role))
    )
    "
  )

  invisible(TRUE)
}


ensure_dataset_record_control_columns <- function(conn) {
  ensure_columns(
    conn,
    "entry_tbl",
    fields = c(
      "created_by_user_id",
      "is_hidden",
      "hidden_at",
      "hidden_by_user_id",
      "deleted_at",
      "deleted_by_user_id"
    ),
    types = c(
      created_by_user_id = "INTEGER",
      is_hidden = "INTEGER DEFAULT 0",
      hidden_at = "TEXT",
      hidden_by_user_id = "INTEGER",
      deleted_at = "TEXT",
      deleted_by_user_id = "INTEGER"
    )
  )

  DBI::dbExecute(
    conn,
    "UPDATE entry_tbl SET is_hidden = 0 WHERE is_hidden IS NULL"
  )

  invisible(TRUE)
}


init_db <- function(base_path) {
  #  Create folder if it doesn't exist
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE)
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)

  # main dataset table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS entry_tbl (
      id INTEGER PRIMARY KEY,
      uuid TEXT UNIQUE,
      title TEXT,
      citation_date TEXT,
      version TEXT,
      abstract TEXT
    )
  ")
  # organisations table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS organisation_tbl (
      id INTEGER PRIMARY KEY,
      organisation_name_full TEXT,
      organisation_name_short TEXT,
      organisation_code TEXT NOT NULL,
      organisation_department TEXT,
      organisation_address TEXT
    )
  ")
  ensure_columns(
    conn = conn,
    table = "organisation_tbl",
    fields = c("organisation_department", "parent_organisation_id", "organisational_unit_name"),
    types = c(organisation_department = "TEXT", parent_organisation_id = "INTEGER", organisational_unit_name = "TEXT")
  )
  ensure_organisation_code_not_unique(conn)
  DBI::dbExecute(
    conn,
    "
    CREATE INDEX IF NOT EXISTS idx_organisation_code
    ON organisation_tbl(organisation_code)
    "
  )
  # persons table
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE IF NOT EXISTS persons_tbl (
      id INTEGER PRIMARY KEY,
      name_first TEXT,
      name_last TEXT,
      email TEXT,
      role_title TEXT,
      phone_number TEXT,
      organisation_id INTEGER,

      FOREIGN KEY (organisation_id)
        REFERENCES organisation_tbl(id)
    )
    "
  )
  ensure_columns(
    conn = conn,
    table = "persons_tbl",
    fields = c(
      "email",
      "role_title",
      "phone_number"
    ),
    types = c(
      email = "TEXT",
      role_title = "TEXT",
      phone_number = "TEXT"
    )
  )
  ensure_person_unique_indexes(conn)
  # add a user table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS user_tbl (
      id INTEGER PRIMARY KEY,
      person_id INTEGER UNIQUE NOT NULL,
      user_name TEXT UNIQUE,
      is_active INTEGER DEFAULT 1,
      is_admin INTEGER DEFAULT 0,
      FOREIGN KEY (person_id) REFERENCES persons_tbl(id)
    )
  ")

  # authority tbl for multi author datasets
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS authority_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER,
      person_id INTEGER,
      author_order INTEGER,
      role TEXT,

      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (person_id) REFERENCES persons_tbl(id)
    )
  ")
  # table for dataset 'owning' organisations
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_organisation_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER,
      organisation_id INTEGER,
      role TEXT,

      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (organisation_id) REFERENCES organisation_tbl(id)
    )
  ")
  ensure_dataset_organisation_role_schema(conn)
  # generate point of contact table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS poc_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER,
      person_id INTEGER,
      organisation_id INTEGER,
      role TEXT,

      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (organisation_id) REFERENCES organisation_tbl(id)
    )
  ")
  ensure_columns(
    conn,
    "poc_tbl",
    fields = "organisation_id",
    types = c(organisation_id = "INTEGER")
  )
  # keyword table generate
  ensure_keyword_vocab_table(conn)

  ensure_data_pond_tables(conn)
  ensure_dataset_import_table(conn)
  ensure_dataset_citation_key_column(conn)
  ensure_dataset_lifecycle_columns(conn)
  ensure_dataset_record_control_columns(conn)
  ensure_entry_creation_scale_columns_conn(conn)
  ensure_iso_profile_schema(conn, migrate = FALSE)

  ensure_data_pond_id(base_path, conn = conn)

  ensure_data_file_tables(conn)
  ensure_dataset_attribute_table(conn)
  ensure_data_profile_run_table(conn)

  ensure_data_file_spatial_metadata_table(conn)
  ensure_dataset_manual_spatial_extent_table(conn)
  ensure_dataset_spatial_summary_table(conn)
  backfill_missing_dataset_spatial_summaries_conn(conn)

  ensure_metadata_review_table(conn)
  ensure_licensing_v2_schema(conn)
  ensure_iso_wave2_schema(conn, migrate = FALSE)
  ensure_iso_wave3_schema(conn, migrate = FALSE)
  seed_license_templates(conn)
  repair_dataset_license_template_links(conn)

  ensure_supporting_document_table(conn)
  ensure_dataset_lineage_table(conn)
  ensure_dataset_lineage_detail_tables(conn, migrate = FALSE)
  ensure_columns(conn, "metadata_review_tbl", fields = c("reviewer_id"),
    types = c(reviewer_id = "INTEGER")
  )
  ensure_columns(
    conn,
    "dataset_lineage_tbl",
    fields = c(
      "lineage_source_mode",
      "lineage_statement",
      "source_description",
      "source_datasets_text",
      "process_description",
      "processing_tools",
      "processing_scripts",
      "output_description",
      "lineage_notes",
      "lineage_status",
      "updated_at",
      "updated_by_person_id"
    ),
    types = c(
      lineage_source_mode = "TEXT DEFAULT 'form'",
      lineage_statement = "TEXT",
      source_description = "TEXT",
      source_datasets_text = "TEXT",
      process_description = "TEXT",
      processing_tools = "TEXT",
      processing_scripts = "TEXT",
      output_description = "TEXT",
      lineage_notes = "TEXT",
      lineage_status = "TEXT",
      updated_at = "TEXT",
      updated_by_person_id = "INTEGER"
    )
  )

  ensure_columns(
    conn,
    "user_tbl",
    fields = c("is_admin"),
    types = c(is_admin = "INTEGER")
  )
  ensure_columns(
    conn,
    "entry_tbl",
    fields = c(
      "lifecycle_status",
      "folder_locked",
      "source_dataset_id",
      "released_at",
      "archived_at"
    ),
    types = c(
      lifecycle_status = "TEXT",
      folder_locked = "INTEGER",
      source_dataset_id = "INTEGER",
      released_at = "TEXT",
      archived_at = "TEXT"
    )
  )
  ensure_columns(
    conn,
    "supporting_document_tbl",
    fields = c(
      "document_type_auto_code",
      "document_type_user_set",
      "document_type_user_text",
      "include_in_lineage",
      "include_in_metadata",
      "notes"
    ),
    types = c(
      document_type_auto_code = "TEXT",
      document_type_user_set = "INTEGER DEFAULT 0",
      document_type_user_text = "TEXT",
      include_in_lineage = "INTEGER DEFAULT 0",
      include_in_metadata = "INTEGER DEFAULT 1",
      notes = "TEXT"
    )
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE supporting_document_tbl
    SET
      document_type_user_set =
        COALESCE(document_type_user_set, 0),
      include_in_lineage =
        COALESCE(include_in_lineage, 0),
      include_in_metadata =
        COALESCE(include_in_metadata, 1),
      document_type_auto_code =
        COALESCE(
          NULLIF(
            TRIM(document_type_auto_code),
            ''
          ),
          document_type_code
        )
    "
  )
  apply_legacy_patch_v1(conn)
  TRUE
}


ensure_dataset_import_table <- function(conn) {
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE IF NOT EXISTS dataset_import_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      source_dataset_uuid TEXT NOT NULL,
      source_dataset_id INTEGER,
      source_data_pond_id TEXT NOT NULL,
      source_created_by_user_id INTEGER,
      source_created_by_user_name TEXT,
      source_exported_by_user_id INTEGER,
      source_exported_by_user_name TEXT,
      bundle_id TEXT NOT NULL UNIQUE,
      bundle_format_version INTEGER NOT NULL,
      source_dataset_folder TEXT,
      archive_name TEXT,
      exported_at TEXT,
      imported_at TEXT DEFAULT CURRENT_TIMESTAMP,
      imported_by_user_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (imported_by_user_id) REFERENCES user_tbl(id)
    )
    "
  )

  DBI::dbExecute(
    conn,
    "
    CREATE INDEX IF NOT EXISTS idx_dataset_import_dataset
    ON dataset_import_tbl(dataset_id)
    "
  )

  invisible(TRUE)
}

get_dataset_authors <- function(
  dataset_id,
  base_path
) {
  empty_result <- function() {
    data.frame(
      id = integer(),
      name = character(),
      name_first = character(),
      name_last = character(),
      author_order = integer(),
      stringsAsFactors = FALSE
    )
  }

  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    return(
      empty_result()
    )
  }

  links <- get_dataset_author_links(
    dataset_id = as.integer(dataset_id[1]),
    base_path = base_path
  )

  if (
    is.null(links) ||
    nrow(links) == 0L
  ) {
    return(
      empty_result()
    )
  }

  links <- as.data.frame(
    links,
    stringsAsFactors = FALSE
  )

  clean_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }

  name_first <- if (
    "name_first" %in% names(links)
  ) {
    clean_text(links$name_first)
  } else {
    rep("", nrow(links))
  }

  name_last <- if (
    "name_last" %in% names(links)
  ) {
    clean_text(links$name_last)
  } else {
    rep("", nrow(links))
  }

  author_order <- if (
    "author_order" %in% names(links)
  ) {
    suppressWarnings(
      as.integer(links$author_order)
    )
  } else {
    seq_len(nrow(links))
  }

  missing_order <- is.na(author_order)

  if (any(missing_order)) {
    author_order[missing_order] <-
      seq_len(sum(missing_order)) +
      max(
        c(
          0L,
          author_order[!missing_order]
        ),
        na.rm = TRUE
      )
  }

  out <- data.frame(
    id = as.integer(links$person_id),
    name = trimws(
      paste(
        name_first,
        name_last
      )
    ),
    name_first = name_first,
    name_last = name_last,
    author_order = author_order,
    stringsAsFactors = FALSE
  )

  out <- out[
    order(
      out$author_order,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(out) <- NULL

  out
}

get_persons_by_ids <- function(
  person_ids,
  base_path
) {
  person_ids <- suppressWarnings(
    as.integer(person_ids)
  )

  person_ids <- person_ids[
    !is.na(person_ids)
  ]

  if (length(person_ids) == 0L) {
    return(
      data.frame(
        id = integer(),
        name_first = character(),
        name_last = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  placeholders <- paste(
    rep("?", length(person_ids)),
    collapse = ", "
  )

  sql <- paste0(
    "
    SELECT
      id,
      name_first,
      name_last
    FROM persons_tbl
    WHERE id IN (",
    placeholders,
    ")
    "
  )

  people <- DBI::dbGetQuery(
    conn,
    sql,
    params = as.list(person_ids)
  )

  people <- as.data.frame(
    people,
    stringsAsFactors = FALSE
  )

  # SQL IN does not preserve input order.
  people <- people[
    match(person_ids, people$id),
    ,
    drop = FALSE
  ]

  people
}

get_datasets <- function(
  base_path,
  include_hidden = TRUE,
  include_deleted = FALSE
) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  ensure_dataset_record_control_columns(conn)
  conditions <- character()
  if (!isTRUE(include_hidden)) {
    conditions <- c(conditions, "COALESCE(is_hidden, 0) = 0")
  }
  if (!isTRUE(include_deleted)) {
    conditions <- c(conditions, "deleted_at IS NULL")
  }
  where <- if (length(conditions) == 0L) "" else paste(
    "WHERE",
    paste(conditions, collapse = " AND ")
  )
  res <- dbGetQuery(
    conn,
    paste("SELECT * FROM entry_tbl", where, "ORDER BY id DESC")
  )
  res
}

get_datasets_with_authors <- function(base_path, include_hidden = FALSE) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  ensure_dataset_record_control_columns(conn)
  where <- if (isTRUE(include_hidden)) {
    ""
  } else {
    "WHERE COALESCE(e.is_hidden, 0) = 0 AND e.deleted_at IS NULL"
  }
  dbGetQuery(conn, paste0("
    SELECT 
      e.id,
      e.title,
      e.citation_date,
      e.version,
      e.abstract,
      e.created_by_user_id,
      COALESCE(e.is_hidden, 0) AS is_hidden,
      e.hidden_at,
      e.deleted_at,

      owner_p.name_first || ' ' || owner_p.name_last AS record_owner,

      (
        SELECT GROUP_CONCAT(author_name, ', ')
        FROM (
          SELECT p.name_first || ' ' || p.name_last AS author_name
          FROM authority_tbl a
          LEFT JOIN persons_tbl p ON a.person_id = p.id
          WHERE a.dataset_id = e.id
          ORDER BY a.author_order
        )
      ) AS authors,

      (
        SELECT GROUP_CONCAT(
          poc_p.name_first || ' ' || poc_p.name_last || ' (' || poc.role || ')',
          ', '
        )
        FROM poc_tbl poc
        LEFT JOIN persons_tbl poc_p ON poc.person_id = poc_p.id
        WHERE poc.dataset_id = e.id
      ) AS poc

      ,(
        SELECT GROUP_CONCAT(org_name, ', ')
        FROM (
          SELECT DISTINCT
            COALESCE(o.organisation_name_short, o.organisation_name_full) ||
              CASE
                WHEN dorg.role IS NULL OR TRIM(dorg.role) = '' THEN ''
                ELSE ' (' || dorg.role || ')'
              END AS org_name
          FROM dataset_organisation_tbl dorg
          LEFT JOIN organisation_tbl o ON dorg.organisation_id = o.id
          WHERE dorg.dataset_id = e.id
          ORDER BY org_name
        )
      ) AS organisations

    FROM entry_tbl e
    LEFT JOIN user_tbl owner_u ON owner_u.id = e.created_by_user_id
    LEFT JOIN persons_tbl owner_p ON owner_p.id = owner_u.person_id
    ", where, "
    ORDER BY e.id DESC
  "))
}

get_organisations <- function(base_path) {
  conn <- get_conn(base_path)
  res <- dbGetQuery(conn, "
    SELECT *
    FROM organisation_tbl
    ORDER BY
      LOWER(TRIM(COALESCE(
        NULLIF(organisation_name_short, ''),
        organisation_name_full,
        ''
      ))),
      CASE
        WHEN organisation_department IS NULL OR
          TRIM(organisation_department) = ''
        THEN 0
        ELSE 1
      END,
      LOWER(TRIM(COALESCE(organisation_department, ''))),
      LOWER(TRIM(COALESCE(organisation_code, ''))),
      id
  ")
  dbDisconnect(conn)
  res
}

insert_organisation <- function(data, base_path) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  validate_organisation_parent_conn(conn, wave3_optional_integer(data$parent_organisation_id))
  dbExecute(conn, "
    INSERT INTO organisation_tbl (
      organisation_name_full,
      organisation_name_short,
      organisation_code,
      organisation_department,
      organisation_address,
      parent_organisation_id,
      organisational_unit_name
    )
    VALUES (?, ?, ?, ?, ?, ?, ?)
  ", params = list(
      data$full,
      data$short,
      data$code,
      data$department %||% NA_character_,
      data$address,
      wave3_optional_integer(data$parent_organisation_id),
      wave3_text_default(data$organisational_unit_name, data$department %||% NA_character_)
  ))
  organisation_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  upsert_primary_organisation_address_conn(conn, organisation_id, data)
  TRUE
}

update_organisation <- function(
  organisation_id,
  data,
  base_path
) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  ensure_iso_wave3_schema(conn)
  validate_organisation_parent_conn(
    conn, wave3_optional_integer(data$parent_organisation_id), as.integer(organisation_id)
  )

  dbExecute(
    conn,
    "
    UPDATE organisation_tbl
    SET
      organisation_name_full = ?,
      organisation_name_short = ?,
      organisation_code = ?,
      organisation_department = ?,
      organisation_address = ?,
      parent_organisation_id = ?,
      organisational_unit_name = ?
    WHERE id = ?
    ",
    params = list(
      data$full,
      data$short,
      data$code,
      data$department %||% NA_character_,
      data$address,
      wave3_optional_integer(data$parent_organisation_id),
      wave3_text_default(data$organisational_unit_name, data$department %||% NA_character_),
      as.integer(organisation_id)
    )
  )
  upsert_primary_organisation_address_conn(conn, organisation_id, data)
  TRUE
}

validate_organisation_parent_conn <- function(conn, parent_id, organisation_id = NA_integer_) {
  parent_id <- wave3_optional_integer(parent_id); organisation_id <- wave3_optional_integer(organisation_id)
  if (is.na(parent_id)) return(invisible(TRUE))
  if (!is.na(organisation_id) && identical(parent_id, organisation_id)) {
    stop("An organisation cannot be its own parent.", call. = FALSE)
  }
  current <- parent_id; visited <- integer()
  while (!is.na(current)) {
    if (current %in% visited || (!is.na(organisation_id) && identical(current, organisation_id))) {
      stop("The selected parent would create an organisational hierarchy cycle.", call. = FALSE)
    }
    visited <- c(visited, current)
    found <- DBI::dbGetQuery(conn,
      "SELECT parent_organisation_id FROM organisation_tbl WHERE id = ?", params = list(current))
    if (!nrow(found)) stop("The selected parent organisation does not exist.", call. = FALSE)
    current <- wave3_optional_integer(found$parent_organisation_id[1])
  }
  invisible(TRUE)
}

upsert_primary_organisation_address_conn <- function(conn, organisation_id, data) {
  fields <- c("delivery_point", "city", "administrative_area", "postal_code", "country",
              "electronic_mail_address", "address_phone_number", "online_resource")
  values <- lapply(fields, function(field) wave3_optional_text(data[[field]]))
  names(values) <- fields
  if (all(vapply(values, is.na, logical(1)))) {
    legacy <- wave3_optional_text(data$address)
    if (!is.na(legacy)) values$delivery_point <- legacy else return(invisible(FALSE))
  }
  DBI::dbExecute(conn, "DELETE FROM party_address_tbl WHERE organisation_id = ? AND is_primary = 1", params = list(as.integer(organisation_id)))
  DBI::dbExecute(conn, "INSERT INTO party_address_tbl (
    organisation_id, delivery_point, city, administrative_area, postal_code, country,
    electronic_mail_address, phone_number, online_resource, is_primary
  ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1)", params = c(list(as.integer(organisation_id)), unname(values)))
  invisible(TRUE)
}

organisation_reference_columns <- function(conn) {
  references <- data.frame(
    table = character(),
    column = character(),
    stringsAsFactors = FALSE
  )
  quote_identifier <- function(value) {
    as.character(DBI::dbQuoteIdentifier(conn, value))
  }

  for (table in setdiff(DBI::dbListTables(conn), c("organisation_tbl", "party_address_tbl"))) {
    fields <- DBI::dbListFields(conn, table)
    organisation_fields <- fields[
      grepl("(^organisation_id$|_organisation_id$)", fields)
    ]

    foreign_keys <- DBI::dbGetQuery(
      conn,
      paste0("PRAGMA foreign_key_list(", quote_identifier(table), ")")
    )
    if (nrow(foreign_keys) > 0L) {
      organisation_fields <- c(
        organisation_fields,
        foreign_keys$from[
          foreign_keys$table == "organisation_tbl" &
            foreign_keys$to == "id"
        ]
      )
    }

    organisation_fields <- unique(organisation_fields)
    if (length(organisation_fields) > 0L) {
      references <- rbind(
        references,
        data.frame(
          table = rep(table, length(organisation_fields)),
          column = organisation_fields,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # A parent organisation is a genuine hierarchy reference. Addresses are
  # owned components and are deliberately excluded from usage counts.
  if ("parent_organisation_id" %in% DBI::dbListFields(conn, "organisation_tbl")) {
    references <- rbind(references, data.frame(
      table = "organisation_tbl", column = "parent_organisation_id",
      stringsAsFactors = FALSE
    ))
  }

  unique(references)
}

organisation_reference_usage <- function(conn, organisation_id) {
  references <- organisation_reference_columns(conn)
  if (nrow(references) == 0L) {
    references$records <- integer()
    return(references)
  }

  records <- integer(nrow(references))
  for (i in seq_len(nrow(references))) {
    table <- as.character(DBI::dbQuoteIdentifier(conn, references$table[i]))
    column <- as.character(DBI::dbQuoteIdentifier(conn, references$column[i]))
    records[i] <- as.integer(DBI::dbGetQuery(
      conn,
      paste0(
        "SELECT COUNT(*) AS records FROM ", table,
        " WHERE ", column, " = ?"
      ),
      params = list(organisation_id)
    )$records[1])
  }

  references$records <- records
  references[references$records > 0L, , drop = FALSE]
}

get_organisation_usage <- function(organisation_id, base_path) {
  organisation_id <- suppressWarnings(as.integer(organisation_id[1]))
  if (length(organisation_id) != 1L || is.na(organisation_id)) {
    stop("Select an organisation.", call. = FALSE)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  organisation_reference_usage(conn, organisation_id)
}

remove_organisation_record <- function(organisation_id, base_path) {
  if (length(organisation_id) != 1L) {
    stop("Select an organisation to remove.", call. = FALSE)
  }
  organisation_id <- suppressWarnings(as.integer(organisation_id))
  if (is.na(organisation_id)) {
    stop("Select an organisation to remove.", call. = FALSE)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbWithTransaction(conn, {
    exists <- DBI::dbGetQuery(
      conn,
      "SELECT COUNT(*) AS records FROM organisation_tbl WHERE id = ?",
      params = list(organisation_id)
    )$records[1]
    if (as.integer(exists) != 1L) {
      stop("The organisation record no longer exists.", call. = FALSE)
    }

    usage <- organisation_reference_usage(conn, organisation_id)
    if (nrow(usage) > 0L) {
      stop(
        "This organisation is still used by ",
        sum(usage$records),
        " linked record(s). Merge it into another organisation instead.",
        call. = FALSE
      )
    }

    if (DBI::dbExistsTable(conn, "party_address_tbl")) {
      DBI::dbExecute(conn, "DELETE FROM party_address_tbl WHERE organisation_id = ?",
                     params = list(organisation_id))
    }

    DBI::dbExecute(
      conn,
      "DELETE FROM organisation_tbl WHERE id = ?",
      params = list(organisation_id)
    )
  })

  invisible(TRUE)
}

merge_organisation_records <- function(
  source_organisation_id,
  target_organisation_id,
  base_path
) {
  if (
    length(source_organisation_id) != 1L ||
      length(target_organisation_id) != 1L
  ) {
    stop("Both organisations are required.", call. = FALSE)
  }
  source_organisation_id <- suppressWarnings(
    as.integer(source_organisation_id)
  )
  target_organisation_id <- suppressWarnings(
    as.integer(target_organisation_id)
  )
  if (is.na(source_organisation_id) || is.na(target_organisation_id)) {
    stop("Both organisations are required.", call. = FALSE)
  }
  if (identical(source_organisation_id, target_organisation_id)) {
    stop("Choose two different organisations to merge.", call. = FALSE)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  quote_identifier <- function(value) {
    as.character(DBI::dbQuoteIdentifier(conn, value))
  }

  DBI::dbWithTransaction(conn, {
    organisations <- DBI::dbGetQuery(
      conn,
      "SELECT id FROM organisation_tbl WHERE id IN (?, ?)",
      params = list(source_organisation_id, target_organisation_id)
    )
    if (!source_organisation_id %in% organisations$id) {
      stop("The duplicate organisation no longer exists.", call. = FALSE)
    }
    if (!target_organisation_id %in% organisations$id) {
      stop("The organisation to keep no longer exists.", call. = FALSE)
    }

    duplicate_links_removed <- 0L
    if (DBI::dbExistsTable(conn, "dataset_organisation_tbl")) {
      duplicate_links_removed <- as.integer(DBI::dbExecute(
        conn,
        "
        DELETE FROM dataset_organisation_tbl AS source
        WHERE source.organisation_id = ?
          AND EXISTS (
            SELECT 1
            FROM dataset_organisation_tbl AS target
            WHERE target.organisation_id = ?
              AND target.dataset_id = source.dataset_id
              AND LOWER(TRIM(COALESCE(target.role, ''))) =
                LOWER(TRIM(COALESCE(source.role, '')))
          )
        ",
        params = list(source_organisation_id, target_organisation_id)
      ))
    }

    if (DBI::dbExistsTable(conn, "party_address_tbl")) {
      target_has_primary <- DBI::dbGetQuery(conn,
        "SELECT COUNT(*) AS n FROM party_address_tbl WHERE organisation_id = ? AND is_primary = 1",
        params = list(target_organisation_id))$n[1] > 0L
      if (target_has_primary) DBI::dbExecute(conn,
        "UPDATE party_address_tbl SET is_primary = 0 WHERE organisation_id = ?",
        params = list(source_organisation_id))
      DBI::dbExecute(conn,
        "UPDATE party_address_tbl SET organisation_id = ? WHERE organisation_id = ?",
        params = list(target_organisation_id, source_organisation_id))
    }

    references <- organisation_reference_columns(conn)
    links_moved <- 0L
    if (nrow(references) > 0L) {
      for (i in seq_len(nrow(references))) {
        table <- quote_identifier(references$table[i])
        column <- quote_identifier(references$column[i])
        if (identical(references$table[i], "organisation_tbl") &&
            identical(references$column[i], "parent_organisation_id")) {
          source_parent <- DBI::dbGetQuery(conn,
            "SELECT parent_organisation_id FROM organisation_tbl WHERE id = ?",
            params = list(source_organisation_id))$parent_organisation_id[1]
          child_count <- DBI::dbGetQuery(conn,
            "SELECT COUNT(*) AS n FROM organisation_tbl WHERE parent_organisation_id = ?",
            params = list(source_organisation_id))$n[1]
          replacement_for_target <- if (is.na(source_parent) || identical(as.integer(source_parent), target_organisation_id)) NA_integer_ else as.integer(source_parent)
          DBI::dbExecute(conn,
            "UPDATE organisation_tbl SET parent_organisation_id = ? WHERE id = ? AND parent_organisation_id = ?",
            params = list(replacement_for_target, target_organisation_id, source_organisation_id))
          DBI::dbExecute(conn,
            "UPDATE organisation_tbl SET parent_organisation_id = ? WHERE parent_organisation_id = ? AND id != ?",
            params = list(target_organisation_id, source_organisation_id, target_organisation_id))
          links_moved <- links_moved + as.integer(child_count)
          next
        }
        links_moved <- links_moved + as.integer(DBI::dbExecute(
          conn,
          paste0(
            "UPDATE ", table,
            " SET ", column, " = ?",
            " WHERE ", column, " = ?"
          ),
          params = list(target_organisation_id, source_organisation_id)
        ))
      }
    }

    DBI::dbExecute(
      conn,
      "DELETE FROM organisation_tbl WHERE id = ?",
      params = list(source_organisation_id)
    )

    list(
      source_organisation_id = source_organisation_id,
      target_organisation_id = target_organisation_id,
      links_moved = links_moved,
      duplicate_links_removed = duplicate_links_removed
    )
  })
}

ensure_person_unique_indexes <- function(conn) {
  duplicate_emails <- DBI::dbGetQuery(
    conn,
    "
    SELECT
      LOWER(TRIM(email)) AS email_key,
      COUNT(*) AS duplicate_count
    FROM persons_tbl
    WHERE email IS NOT NULL
      AND TRIM(email) != ''
    GROUP BY LOWER(TRIM(email))
    HAVING COUNT(*) > 1
    "
  )

  if (nrow(duplicate_emails) > 0L) {
    warning(
      "The persons table contains ",
      sum(as.integer(duplicate_emails$duplicate_count)),
      " records across ",
      nrow(duplicate_emails),
      " duplicated email address(es). ",
      "The unique email index was not created; existing person records were ",
      "left unchanged.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  DBI::dbExecute(
    conn,
    "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_persons_email_unique
    ON persons_tbl(LOWER(TRIM(email)))
    WHERE email IS NOT NULL
      AND TRIM(email) != ''
    "
  )
  invisible(TRUE)
}

sync_persons_to_db <- function(person_dt, base_path, conn = NULL) {
  if (is.null(person_dt) || nrow(person_dt) == 0) {
    warning("No persons to sync")
    return(invisible(NULL))
  }
  if (is.null(conn)) {
    conn <- get_conn(base_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  for (i in seq_len(nrow(person_dt))) {
    first_name <- trimws(
      as.character(person_dt$name_first[i] %||% "")
    )
    last_name <- trimws(
      as.character(person_dt$name_last[i] %||% "")
    )
    email <- trimws(
      tolower(
        as.character(person_dt$email[i] %||% "")
      )
    )
    role_title <- as.character(
      person_dt$role_title[i] %||% NA_character_
    )
    phone_number <- as.character(
      person_dt$phone_number[i] %||% NA_character_
    )
    organisation_code <- trimws(
      as.character(
        person_dt$organisation_code[i] %||% ""
      )
    )
    organisation_department <- if (
      "organisation_department" %in% names(person_dt)
    ) {
      person_dt$organisation_department[i]
    } else if (
      "department" %in% names(person_dt)
    ) {
      person_dt$department[i]
    } else {
      NA_character_
    }
    organisation_department <- trimws(
      as.character(organisation_department)
    )
    if (
      length(organisation_department) == 0L ||
      is.na(organisation_department)
    ) {
      organisation_department <- ""
    }
    org_id <- NA_integer_
    if (nzchar(organisation_code)) {
      if (nzchar(organisation_department)) {
        org_result <- DBI::dbGetQuery(
          conn,
          "
          SELECT id
          FROM organisation_tbl
          WHERE organisation_code = ?
            AND TRIM(COALESCE(organisation_department, '')) = ?
          ORDER BY id
          LIMIT 1
          ",
          params = list(
            organisation_code,
            organisation_department
          )
        )
      } else {
        org_result <- DBI::dbGetQuery(
          conn,
          "
          SELECT id
          FROM organisation_tbl
          WHERE organisation_code = ?
          ORDER BY
            CASE
              WHEN organisation_department IS NULL
                OR TRIM(organisation_department) = ''
              THEN 0
              ELSE 1
            END,
            id
          LIMIT 1
          ",
          params = list(organisation_code)
        )
      }
      if (nrow(org_result) > 0) {
        org_id <- as.integer(org_result$id[1])
      }
    }
    # Prefer email as the matching key.
    if (nzchar(email)) {
      existing_person <- DBI::dbGetQuery(
        conn,
        "
        SELECT id
        FROM persons_tbl
        WHERE LOWER(TRIM(email)) = ?
        LIMIT 1
        ",
        params = list(email)
      )
    } else {
      # Fallback for records without email.
      existing_person <- DBI::dbGetQuery(
        conn,
        "
        SELECT id
        FROM persons_tbl
        WHERE LOWER(TRIM(name_first)) = LOWER(TRIM(?))
          AND LOWER(TRIM(name_last)) = LOWER(TRIM(?))
        LIMIT 1
        ",
        params = list(
          first_name,
          last_name
        )
      )
    }
    if (nrow(existing_person) > 0) {
      person_id <- as.integer(
        existing_person$id[1]
      )
      DBI::dbExecute(
        conn,
        "
        UPDATE persons_tbl
        SET
          name_first = ?,
          name_last = ?,
          email = ?,
          role_title = ?,
          phone_number = ?,
          organisation_id = ?
        WHERE id = ?
        ",
        params = list(
          first_name,
          last_name,
          if (nzchar(email)) email else NA_character_,
          role_title,
          phone_number,
          org_id,
          person_id
        )
      )
    } else {
      DBI::dbExecute(
        conn,
        "
        INSERT INTO persons_tbl (
          name_first,
          name_last,
          email,
          role_title,
          phone_number,
          organisation_id
        )
        VALUES (?, ?, ?, ?, ?, ?)
        ",
        params = list(
          first_name,
          last_name,
          if (nzchar(email)) email else NA_character_,
          role_title,
          phone_number,
          org_id
        )
      )
      person_id <- DBI::dbGetQuery(
        conn,
        "SELECT last_insert_rowid() AS id"
      )$id[1]
    }
    is_user <- suppressWarnings(
      as.integer(person_dt$is_user[i])
    )
    is_admin <- suppressWarnings(
      as.integer(person_dt$is_admin[i])
    )
    if (!is.na(is_user) && is_user == 1L) {
      DBI::dbExecute(
        conn,
        "
        INSERT INTO user_tbl (
          person_id,
          user_name,
          is_active,
          is_admin
        )
        VALUES (?, ?, 1, ?)
        ON CONFLICT(person_id)
        DO UPDATE SET
          is_active = 1,
          is_admin = excluded.is_admin
        ",
        params = list(
          person_id,
          paste0("user_", person_id),
          ifelse(
            is.na(is_admin),
            0L,
            is_admin
          )
        )
      )
    }
  }
  invisible(TRUE)
}
get_persons <- function(base_path) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, "
    SELECT
      p.id,
      p.name_first,
      p.name_last,
      p.email,
      p.role_title,
      p.phone_number,
      p.organisation_id,
      o.organisation_name_full,
      o.organisation_name_short,
      o.organisation_code,
      o.organisation_department AS department
    FROM persons_tbl p
    LEFT JOIN organisation_tbl o ON p.organisation_id = o.id
    ORDER BY p.name_last, p.name_first
  ")
}

insert_person <- function(data, base_path) {
  conn <- get_conn(base_path)
  dbExecute(conn, "
    INSERT INTO persons_tbl (
      name_first,
      name_last,
      email,
      phone_number,
      organisation_id
    )
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    data$first,
    data$last,
    data$email %||% NA_character_,
    data$phone_number %||% NA_character_,
    data$org_id
  ))
  id <- dbGetQuery(conn, "SELECT last_insert_rowid() as id")$id
  dbDisconnect(conn)
  id
}

update_person <- function(person_id, data, base_path) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbExecute(conn, "
    UPDATE persons_tbl
    SET
      name_first = ?,
      name_last = ?,
      email = ?,
      phone_number = ?,
      organisation_id = ?
    WHERE id = ?
  ", params = list(
    data$first,
    data$last,
    data$email %||% NA_character_,
    data$phone_number %||% NA_character_,
    data$org_id,
    person_id
  ))
}

set_user_status <- function(person_id, is_user, base_path) {
  if (!isTRUE(is_user)) access_assert_unlinked_person(base_path, person_id)
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  if (isTRUE(is_user)) {
    dbExecute(conn, "
      INSERT OR IGNORE INTO user_tbl (person_id, user_name, is_active)
      VALUES (?, ?, 1)
    ", params = list(
      person_id,
      paste0("user_", person_id)
    ))
    dbExecute(conn, "
      UPDATE user_tbl
      SET is_active = 1
      WHERE person_id = ?
    ", params = list(person_id))
  } else {
    dbExecute(conn, "
      UPDATE user_tbl
      SET is_active = 0
      WHERE person_id = ?
    ", params = list(person_id))
  }
}

merge_person_records <- function(source_person_id, target_person_id, base_path) {
  if (length(source_person_id) != 1L || length(target_person_id) != 1L) {
    stop("Both the duplicate and the person to keep are required.", call. = FALSE)
  }
  source_person_id <- suppressWarnings(as.integer(source_person_id))
  target_person_id <- suppressWarnings(as.integer(target_person_id))
  if (is.na(source_person_id) || is.na(target_person_id)) {
    stop("Both the duplicate and the person to keep are required.", call. = FALSE)
  }
  access_assert_unlinked_person(base_path, source_person_id)
  access_assert_unlinked_person(base_path, target_person_id)
  if (identical(source_person_id, target_person_id)) {
    stop("Choose two different person records to merge.", call. = FALSE)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  quote_identifier <- function(value) {
    as.character(DBI::dbQuoteIdentifier(conn, value))
  }

  update_reference <- function(table, column, from_id, to_id) {
    sql <- paste0(
      "UPDATE ", quote_identifier(table),
      " SET ", quote_identifier(column), " = ?",
      " WHERE ", quote_identifier(column), " = ?"
    )
    as.integer(DBI::dbExecute(conn, sql, params = list(to_id, from_id)))
  }

  result <- DBI::dbWithTransaction(conn, {
    people <- DBI::dbGetQuery(
      conn,
      "SELECT id FROM persons_tbl WHERE id IN (?, ?)",
      params = list(source_person_id, target_person_id)
    )
    if (!source_person_id %in% people$id) {
      stop("The duplicate person record no longer exists.", call. = FALSE)
    }
    if (!target_person_id %in% people$id) {
      stop("The person record to keep no longer exists.", call. = FALSE)
    }

    users <- DBI::dbGetQuery(
      conn,
      paste(
        "SELECT id, person_id, COALESCE(is_active, 0) AS is_active,",
        "COALESCE(is_admin, 0) AS is_admin",
        "FROM user_tbl WHERE person_id IN (?, ?)"
      ),
      params = list(source_person_id, target_person_id)
    )
    source_user <- users[users$person_id == source_person_id, , drop = FALSE]
    target_user <- users[users$person_id == target_person_id, , drop = FALSE]
    source_user_id <- if (nrow(source_user) == 1L) {
      as.integer(source_user$id[1])
    } else {
      NA_integer_
    }
    target_user_id <- if (nrow(target_user) == 1L) {
      as.integer(target_user$id[1])
    } else {
      NA_integer_
    }
    user_links_moved <- 0L

    if (!is.na(source_user_id) && is.na(target_user_id)) {
      DBI::dbExecute(
        conn,
        "UPDATE user_tbl SET person_id = ? WHERE id = ?",
        params = list(target_person_id, source_user_id)
      )
      target_user_id <- source_user_id
    } else if (!is.na(source_user_id) && !is.na(target_user_id)) {
      tables <- setdiff(DBI::dbListTables(conn), "user_tbl")
      user_references <- data.frame(
        table = character(),
        column = character(),
        stringsAsFactors = FALSE
      )

      for (table in tables) {
        fields <- DBI::dbListFields(conn, table)
        local_user_fields <- fields[
          grepl("(^user_id$|_user_id$)", fields) &
            !grepl("^source_", fields)
        ]
        if (length(local_user_fields) > 0L) {
          user_references <- rbind(
            user_references,
            data.frame(
              table = rep(table, length(local_user_fields)),
              column = local_user_fields,
              stringsAsFactors = FALSE
            )
          )
        }

        foreign_keys <- DBI::dbGetQuery(
          conn,
          paste0("PRAGMA foreign_key_list(", quote_identifier(table), ")")
        )
        if (nrow(foreign_keys) > 0L) {
          user_fields <- foreign_keys$from[
            foreign_keys$table == "user_tbl" &
              foreign_keys$to == "id"
          ]
          if (length(user_fields) > 0L) {
            user_references <- rbind(
              user_references,
              data.frame(
                table = rep(table, length(user_fields)),
                column = user_fields,
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }

      user_references <- unique(user_references)
      if (nrow(user_references) > 0L) {
        for (i in seq_len(nrow(user_references))) {
          user_links_moved <- user_links_moved + update_reference(
            user_references$table[i],
            user_references$column[i],
            source_user_id,
            target_user_id
          )
        }
      }

      DBI::dbExecute(
        conn,
        paste(
          "UPDATE user_tbl",
          "SET is_active = MAX(COALESCE(is_active, 0), ?),",
          "is_admin = MAX(COALESCE(is_admin, 0), ?)",
          "WHERE id = ?"
        ),
        params = list(
          as.integer(source_user$is_active[1] %||% 0L),
          as.integer(source_user$is_admin[1] %||% 0L),
          target_user_id
        )
      )
      DBI::dbExecute(
        conn,
        "DELETE FROM user_tbl WHERE id = ?",
        params = list(source_user_id)
      )
    }

    person_references <- data.frame(
      table = character(),
      column = character(),
      stringsAsFactors = FALSE
    )
    tables <- setdiff(DBI::dbListTables(conn), "user_tbl")
    for (table in tables) {
      fields <- DBI::dbListFields(conn, table)
      local_person_fields <- fields[
        grepl("(^person_id$|_person_id$|^reviewer_id$)", fields)
      ]
      if (length(local_person_fields) > 0L) {
        person_references <- rbind(
          person_references,
          data.frame(
            table = rep(table, length(local_person_fields)),
            column = local_person_fields,
            stringsAsFactors = FALSE
          )
        )
      }

      foreign_keys <- DBI::dbGetQuery(
        conn,
        paste0("PRAGMA foreign_key_list(", quote_identifier(table), ")")
      )
      if (nrow(foreign_keys) == 0L) {
        next
      }
      person_fields <- foreign_keys$from[
        foreign_keys$table == "persons_tbl" &
          foreign_keys$to == "id"
      ]
      if (length(person_fields) > 0L) {
        person_references <- rbind(
          person_references,
          data.frame(
            table = rep(table, length(person_fields)),
            column = person_fields,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    person_links_moved <- 0L
    person_references <- unique(person_references)
    if (nrow(person_references) > 0L) {
      for (i in seq_len(nrow(person_references))) {
        person_links_moved <- person_links_moved + update_reference(
          person_references$table[i],
          person_references$column[i],
          source_person_id,
          target_person_id
        )
      }
    }

    DBI::dbExecute(
      conn,
      "DELETE FROM persons_tbl WHERE id = ?",
      params = list(source_person_id)
    )

    list(
      source_person_id = source_person_id,
      target_person_id = target_person_id,
      source_user_id = source_user_id,
      target_user_id = target_user_id,
      person_links_moved = person_links_moved,
      user_links_moved = user_links_moved
    )
  })

  result
}

insert_authors <- function(
  dataset_id,
  authors,
  base_path
) {
  if (length(authors) == 0L) {
    return(invisible(TRUE))
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  DBI::dbWithTransaction(
    conn,
    {
      for (i in seq_along(authors)) {
        DBI::dbExecute(
          conn,
          "
          INSERT INTO authority_tbl (
            dataset_id,
            person_id,
            author_order,
            role
          )
          VALUES (?, ?, ?, ?)
          ",
          params = list(
            as.integer(dataset_id),
            as.integer(authors[i]),
            as.integer(i),
            "author"
          )
        )
      }
    }
  )

  invisible(TRUE)
}

get_person_choices <- function(base_path) {
  conn <- get_conn(base_path)
  df <- dbGetQuery(conn, "
    SELECT id,
          name_first || ' ' || name_last AS name
    FROM persons_tbl
  ")
  dbDisconnect(conn)
  setNames(df$id, df$name)
}


# insert the point of contact into the database
insert_poc <- function(
  dataset_id,
  person_id,
  role,
  base_path,
  organisation_id = NA_integer_
) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_columns(
    conn,
    "poc_tbl",
    fields = "organisation_id",
    types = c(organisation_id = "INTEGER")
  )
  dbExecute(conn, "
    INSERT INTO poc_tbl (dataset_id, person_id, organisation_id, role)
    VALUES (?, ?, ?, ?)
  ", params = list(
    dataset_id,
    person_id,
    organisation_id,
    role
  ))
}

insert_dataset_org <- function(dataset_id, org_id, role, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  dbExecute(conn, "
    INSERT INTO dataset_organisation_tbl (dataset_id, organisation_id, role)
    VALUES (?, ?, ?)
  ", params = list(
    dataset_id,
    org_id,
    role
  ))
}

# function to add missing columns to existing tables
# this enables dynamic schema from excel doc
ensure_columns <- function(conn, table, fields, types = NULL) {
  existing <- dbGetQuery(conn, paste0("PRAGMA table_info(", table, ")"))
  existing_fields <- existing$name
  missing <- setdiff(fields, existing_fields)
  for (f in missing) {
    column_type <- "TEXT"
    if (!is.null(types) && f %in% names(types)) {
      column_type <- types[[f]]
    }
    sql <- sprintf(
      "ALTER TABLE %s ADD COLUMN %s %s",
      table,
      f,
      column_type
    )
    dbExecute(conn, sql)
  }
  invisible(TRUE)
}

sync_organisations_to_db <- function(
  org_dt,
  base_path,
  conn = NULL
) {
  if (is.null(org_dt) || nrow(org_dt) == 0L) {
    warning("No organisations to sync.")
    return(invisible(NULL))
  }

  if (is.null(conn)) {
    conn <- get_conn(base_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }
  # A savepoint also works inside atomic first-use party seeding.
  DBI::dbExecute(conn, "SAVEPOINT datapond_seed_organisations")
  finished <- FALSE
  on.exit({
    if (!finished) DBI::dbExecute(conn, "ROLLBACK TO datapond_seed_organisations")
    DBI::dbExecute(conn, "RELEASE datapond_seed_organisations")
  }, add = TRUE, after = FALSE)
  for (i in seq_len(nrow(org_dt))) {
    department <- if (
      "department" %in% names(org_dt)
    ) {
      org_dt$department[i]
    } else if (
      "organisation_department" %in% names(org_dt)
    ) {
      org_dt$organisation_department[i]
    } else {
      NA_character_
    }
    department <- trimws(
      as.character(department)
    )
    if (length(department) == 0L || is.na(department)) {
      department <- ""
    }

    existing <- DBI::dbGetQuery(
      conn,
      "
      SELECT id
      FROM organisation_tbl
      WHERE organisation_code = ?
        AND COALESCE(TRIM(organisation_department), '') = ?
      ORDER BY id
      LIMIT 1
      ",
      params = list(
        org_dt$code[i],
        department
      )
    )

    if (nrow(existing) > 0L) {
      DBI::dbExecute(
        conn,
        "
        UPDATE organisation_tbl
        SET
          organisation_name_full = ?,
          organisation_name_short = ?,
          organisation_department = COALESCE(NULLIF(?, ''), organisation_department),
          organisation_address = ?
        WHERE id = ?
        ",
        params = list(
          org_dt$full[i],
          org_dt$short[i],
          department,
          org_dt$address[i],
          existing$id[1]
        )
      )
    } else {
      DBI::dbExecute(
        conn,
        "
        INSERT INTO organisation_tbl (
          organisation_name_full,
          organisation_name_short,
          organisation_code,
          organisation_department,
          organisation_address
        )
        VALUES (?, ?, ?, ?, ?)
        ",
        params = list(
          org_dt$full[i],
          org_dt$short[i],
          org_dt$code[i],
          if (nzchar(department)) department else NA_character_,
          org_dt$address[i]
        )
      )
    }
  }
  finished <- TRUE
  invisible(TRUE)
}

# function to get keywords from the database
get_keywords <- function(field_code, base_path) {
  field_code <- canonical_keyword_field_code(field_code)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_keyword_vocab_table(conn)

  df <- DBI::dbGetQuery(conn, "
    SELECT option_value, option_label
    FROM keyword_vocab_tbl
    WHERE field_code = ?
      AND COALESCE(is_active, 1) = 1
      AND COALESCE(approval_status, 'approved') = 'approved'
    ORDER BY
      COALESCE(sort_order, 2147483647),
      option_label,
      option_value
  ", params = list(field_code))

  if (nrow(df) == 0L) {
    return(NULL)
  }

  df$option_label[
    is.na(df$option_label) | trimws(df$option_label) == ""
  ] <- df$option_value[
    is.na(df$option_label) | trimws(df$option_label) == ""
  ]

  choices <- stats::setNames(
    as.character(df$option_value),
    as.character(df$option_label)
  )

  choices[
    !is.na(names(choices)) &
      nzchar(names(choices)) &
      !is.na(choices) &
      nzchar(choices)
  ]
}

is_user_admin <- function(user_id, base_path) {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    auth <- access_context()
    if (is.null(auth) || !identical(access_path(base_path), access_path(auth$pond))) return(FALSE)
    identity <- access_session(base_path, auth$token, "admin")
    return(!is.null(identity) && identical(as.integer(user_id), as.integer(identity$user_id[1])))
  }
  if (is.null(user_id) || length(user_id) == 0L ||
      is.na(user_id[1])) {
    return(FALSE)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  row <- DBI::dbGetQuery(conn, "
    SELECT COALESCE(is_admin, 0) AS is_admin
    FROM user_tbl
    WHERE id = ?
      AND COALESCE(is_active, 1) = 1
    LIMIT 1
  ", params = list(as.integer(user_id[1])))

  nrow(row) == 1L && identical(as.integer(row$is_admin[1]), 1L)
}


assert_admin_user <- function(user_id, base_path) {
  if (!is_user_admin(user_id, base_path)) {
    stop("Administrator privileges are required.")
  }

  invisible(TRUE)
}


dataset_record_permissions <- function(dataset_id, user_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_record_control_columns(conn)

  record <- DBI::dbGetQuery(
    conn,
    "
    SELECT created_by_user_id, COALESCE(is_hidden, 0) AS is_hidden,
           deleted_at
    FROM entry_tbl
    WHERE id = ?
    LIMIT 1
    ",
    params = list(as.integer(dataset_id))
  )
  if (nrow(record) == 0L) {
    return(list(exists = FALSE, is_owner = FALSE, is_admin = FALSE,
                can_manage = FALSE))
  }

  user_id <- suppressWarnings(as.integer(user_id[1] %||% NA_integer_))
  admin <- is_user_admin(user_id, base_path)
  owner <- !is.na(user_id) &&
    !is.na(record$created_by_user_id[1]) &&
    identical(user_id, as.integer(record$created_by_user_id[1]))

  list(
    exists = TRUE,
    is_owner = owner,
    is_admin = admin,
    can_manage = owner || admin,
    is_hidden = identical(as.integer(record$is_hidden[1]), 1L),
    is_deleted = !is.na(record$deleted_at[1]) &&
      nzchar(trimws(as.character(record$deleted_at[1])))
  )
}


assert_dataset_record_manager <- function(dataset_id, user_id, base_path) {
  permissions <- dataset_record_permissions(dataset_id, user_id, base_path)
  if (!isTRUE(permissions$exists)) {
    stop("Dataset record not found.")
  }
  if (!isTRUE(permissions$can_manage)) {
    stop("Only the record owner or an administrator may perform this action.")
  }
  invisible(permissions)
}


set_dataset_hidden <- function(dataset_id, hidden, user_id, base_path) {
  assert_dataset_record_manager(dataset_id, user_id, base_path)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_record_control_columns(conn)

  DBI::dbExecute(
    conn,
    "
    UPDATE entry_tbl
    SET is_hidden = ?,
        hidden_at = CASE WHEN ? = 1 THEN CURRENT_TIMESTAMP ELSE NULL END,
        hidden_by_user_id = CASE WHEN ? = 1 THEN ? ELSE NULL END
    WHERE id = ?
    ",
    params = list(
      as.integer(isTRUE(hidden)),
      as.integer(isTRUE(hidden)),
      as.integer(isTRUE(hidden)),
      as.integer(user_id),
      as.integer(dataset_id)
    )
  )
  invisible(TRUE)
}


soft_delete_dataset_record <- function(dataset_id, user_id, base_path) {
  assert_dataset_record_manager(dataset_id, user_id, base_path)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_record_control_columns(conn)
  DBI::dbExecute(
    conn,
    "
    UPDATE entry_tbl
    SET deleted_at = CURRENT_TIMESTAMP,
        deleted_by_user_id = ?,
        is_hidden = 1
    WHERE id = ?
    ",
    params = list(as.integer(user_id), as.integer(dataset_id))
  )
  invisible(TRUE)
}


restore_dataset_record <- function(dataset_id, user_id, base_path) {
  assert_dataset_record_manager(dataset_id, user_id, base_path)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(
    conn,
    "
    UPDATE entry_tbl
    SET deleted_at = NULL,
        deleted_by_user_id = NULL,
        is_hidden = 0,
        hidden_at = NULL,
        hidden_by_user_id = NULL
    WHERE id = ?
    ",
    params = list(as.integer(dataset_id))
  )
  invisible(TRUE)
}


hard_delete_dataset_record <- function(dataset_id, user_id, base_path) {
  assert_admin_user(user_id, base_path)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (DBI::dbExistsTable(conn, "dataset_release_tbl") && nrow(DBI::dbGetQuery(conn,
    "SELECT release_id FROM dataset_release_tbl WHERE dataset_id=?", params = list(as.integer(dataset_id)))))
    stop("This dataset has frozen releases. Hide the record instead of deleting its release history.")

  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, "PRAGMA defer_foreign_keys = ON")

    mutable_tables <- DBI::dbGetQuery(
      conn, "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name"
    )$name
    for (table in setdiff(mutable_tables, "entry_tbl")) {
      fields <- DBI::dbListFields(conn, table)
      if (!"dataset_id" %in% fields) {
        next
      }
      sql <- paste(
        "DELETE FROM",
        DBI::dbQuoteIdentifier(conn, table),
        "WHERE dataset_id = ?"
      )
      DBI::dbExecute(conn, sql, params = list(as.integer(dataset_id)))
    }

    if ("source_dataset_id" %in% DBI::dbListFields(conn, "entry_tbl")) {
      DBI::dbExecute(
        conn,
        "UPDATE entry_tbl SET source_dataset_id = NULL WHERE source_dataset_id = ?",
        params = list(as.integer(dataset_id))
      )
    }
    DBI::dbExecute(
      conn,
      "DELETE FROM entry_tbl WHERE id = ?",
      params = list(as.integer(dataset_id))
    )
  })

  invisible(TRUE)
}


get_keyword_vocab_admin <- function(
  base_path,
  field_code = NULL
) {
  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_keyword_vocab_table(conn)

  sql <- "
    SELECT
      id,
      field_code,
      option_value,
      option_label,
      sort_order,
      is_active,
      source,
      approval_status,
      created_at,
      created_by_user_id,
      updated_at,
      updated_by_user_id
    FROM keyword_vocab_tbl
  "

  has_field_filter <- (
    !is.null(field_code) &&
      length(field_code) > 0L &&
      !is.na(field_code[1]) &&
      nzchar(trimws(as.character(field_code[1])))
  )

  if (isTRUE(has_field_filter)) {
    field_code <- canonical_keyword_field_code(field_code)

    sql <- paste0(
      sql,
      "
      WHERE field_code = ?
      "
    )
  }

  sql <- paste0(
    sql,
    "
    ORDER BY
      field_code,
      COALESCE(sort_order, 2147483647),
      option_label,
      option_value
    "
  )

  vocabulary <- if (isTRUE(has_field_filter)) {
    DBI::dbGetQuery(conn, sql, params = list(field_code))
  } else {
    DBI::dbGetQuery(conn, sql)
  }
  vocabulary$dataset_count <- keyword_vocab_usage(conn, vocabulary)
  vocabulary

}


save_keyword_vocab_term <- function(
  base_path,
  user_id,
  field_code,
  option_value,
  option_label = option_value,
  sort_order = NA_integer_,
  is_active = TRUE,
  approval_status = "approved",
  keyword_id = NULL
) {
  assert_admin_user(user_id, base_path)

  field_code <- canonical_keyword_field_code(field_code)
  option_value <- trimws(as.character(option_value[1]))
  option_label <- trimws(as.character(option_label[1]))

  if (is.na(field_code) || is.na(option_value) || !nzchar(field_code) || !nzchar(option_value)) {
    stop("Field code and option value are required.")
  }

  if (is.na(option_label) || !nzchar(option_label)) {
    option_label <- option_value
  }

  prepare_keyword_vocab_seed(data.frame(field_code = field_code, option_value = option_value,
    option_label = option_label, sort_order = if (is.na(sort_order)) 100L else sort_order))

  approval_status <- match.arg(
    approval_status,
    c("approved", "pending", "rejected")
  )

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_keyword_vocab_table(conn)

  existing <- if (!is.null(keyword_id) && !is.na(keyword_id)) {
    DBI::dbGetQuery(conn, "
      SELECT *
      FROM keyword_vocab_tbl
      WHERE id = ?
      LIMIT 1
    ", params = list(as.integer(keyword_id)))
  } else {
    DBI::dbGetQuery(conn, "
      SELECT *
      FROM keyword_vocab_tbl
      WHERE field_code = ? COLLATE NOCASE
        AND option_value = ? COLLATE NOCASE
      LIMIT 1
    ", params = list(field_code, option_value))
  }

  editing <- !is.null(keyword_id) && !is.na(keyword_id)
  if (!editing && nrow(existing)) {
    stop("This keyword already exists. Select it in the table to edit it.")
  }
  if (editing && !nrow(existing)) stop("This keyword no longer exists. Reload the vocabulary before editing.")
  if (nrow(existing) &&
      (!identical(existing$option_value[1], option_value) || !identical(existing$field_code[1], field_code)) &&
      keyword_vocab_usage(conn, existing)[1] > 0L) {
    stop("This term is used by datasets. Its stored value and group cannot be changed; edit the display label instead.")
  }
  duplicate <- DBI::dbGetQuery(conn, paste(
    "SELECT id FROM keyword_vocab_tbl WHERE field_code = ? COLLATE NOCASE",
    "AND (option_value = ? COLLATE NOCASE OR option_label = ? COLLATE NOCASE) AND id != ?"
  ), params = list(field_code, option_value, option_label, if (nrow(existing)) existing$id[1] else -1L))
  if (nrow(duplicate)) stop("Another term in this group already uses that value or display label.")

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (nrow(existing) == 0L) {
    DBI::dbExecute(conn, "
      INSERT INTO keyword_vocab_tbl (
        field_code,
        option_value,
        option_label,
        sort_order,
        is_active,
        source,
        approval_status,
        created_at,
        created_by_user_id,
        updated_at,
        updated_by_user_id
      )
      VALUES (?, ?, ?, ?, ?, 'admin_created', ?, ?, ?, ?, ?)
    ", params = list(
      field_code,
      option_value,
      option_label,
      suppressWarnings(as.integer(sort_order)),
      as.integer(isTRUE(is_active)),
      approval_status,
      now,
      as.integer(user_id),
      now,
      as.integer(user_id)
    ))

    return(
      DBI::dbGetQuery(
        conn,
        "SELECT last_insert_rowid() AS id"
      )$id[1]
    )
  }

  source <- existing$source[1]

  if (identical(source, "seed_csv")) {
    source <- "admin_override"
  }

  DBI::dbExecute(conn, "
    UPDATE keyword_vocab_tbl
    SET
      field_code = ?,
      option_value = ?,
      option_label = ?,
      sort_order = ?,
      is_active = ?,
      source = ?,
      approval_status = ?,
      updated_at = ?,
      updated_by_user_id = ?
    WHERE id = ?
  ", params = list(
    field_code,
    option_value,
    option_label,
    suppressWarnings(as.integer(sort_order)),
    as.integer(isTRUE(is_active)),
    source,
    approval_status,
    now,
    as.integer(user_id),
    existing$id[1]
  ))

  existing$id[1]
}


set_keyword_vocab_active <- function(keyword_id, is_active,
                                      user_id, base_path) {
  assert_admin_user(user_id, base_path)

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbExecute(conn, "
    UPDATE keyword_vocab_tbl
    SET
      is_active = ?,
      source = CASE WHEN source = 'seed_csv' THEN 'admin_override' ELSE source END,
      updated_at = ?,
      updated_by_user_id = ?
    WHERE id = ?
  ", params = list(
    as.integer(isTRUE(is_active)),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    as.integer(user_id),
    as.integer(keyword_id)
  ))

  invisible(TRUE)
}

ensure_data_file_tables <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS data_file_scan_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      data_dir TEXT,
      scan_timestamp TEXT,
      scan_status TEXT,
      files_detected INTEGER,
      notes TEXT,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id)
    )
  ")
  ensure_columns(
    conn,
    "data_file_scan_tbl",
    fields = c("data_pond_id"),
    types = c(data_pond_id = "TEXT")
  )
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS data_file_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      data_file_rel_path_full TEXT NOT NULL,
      file_name TEXT,
      file_dir TEXT,
      file_stem TEXT,
      file_key TEXT,
      file_path TEXT,
      data_file_extension TEXT,
      data_file_format TEXT,
      file_role TEXT,
      is_container INTEGER,
      container_type TEXT,
      group_family TEXT,
      group_key TEXT,
      primary_dataset INTEGER,
      sidecar_count INTEGER,
      sidecar_exts TEXT,
      sidecar_summary TEXT,
      file_size_kb REAL,
      modified_time TEXT,
      scan_status TEXT,
      active INTEGER,
      first_seen_at TEXT,
      last_seen_at TEXT,
      missing_since TEXT,
      latest_scan_id INTEGER,
      data_file_description TEXT,
      data_file_notes TEXT,
      include_in_dataset_bbox INTEGER,
      include_in_attribute_scan INTEGER,
      user_selected_primary INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (latest_scan_id) REFERENCES data_file_scan_tbl(id)
    )
  ")

  ensure_columns(
    conn = conn,
    table = "data_file_tbl",
    fields = c(
      "data_pond_id",
      "data_file_description",
      "data_file_notes",
      "content_hint",
      "review_status",
      "profile_updated_at",
      "updated_by_user_id",
      "include_in_dataset_bbox",
      "include_in_attribute_scan",
      "user_selected_primary"
    ),
    types = c(
      data_pond_id = "TEXT",
      data_file_description = "TEXT",
      data_file_notes = "TEXT",
      content_hint = "TEXT",
      review_status = "TEXT",
      profile_updated_at = "TEXT",
      updated_by_user_id = "INTEGER",
      include_in_dataset_bbox = "INTEGER DEFAULT 0",
      include_in_attribute_scan = "INTEGER DEFAULT 0",
      user_selected_primary = "INTEGER DEFAULT 0"
    )
  )

  dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_data_file_dataset_relpath
    ON data_file_tbl(dataset_id, data_file_rel_path_full)
  ")
  invisible(TRUE)
}

register_pending_keyword_values <- function(values, base_path,
                                            user_id = NULL) {
  if (is.null(values) || length(values) == 0L) {
    return(0L)
  }

  accepted_fields <- c(
    "keyword_object",
    "keyword_discipline",
    "keyword_topic",
    "keyword_geo"
  )

  fields <- intersect(
    accepted_fields,
    names(values)
  )

  if (length(fields) == 0L) {
    return(0L)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_keyword_vocab_table(conn)

  inserted <- 0L
  user_id <- normalise_optional_user_id(user_id)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbWithTransaction(conn, {
    for (field in fields) {
      field_code <- canonical_keyword_field_code(field)

      vocabulary <- DBI::dbGetQuery(conn,
        "SELECT option_value, option_label FROM keyword_vocab_tbl WHERE field_code = ?",
        params = list(field_code))
      terms <- normalise_keyword_values(values[[field]],
        stats::setNames(vocabulary$option_value, vocabulary$option_label))

      for (term in terms) {
        existing <- DBI::dbGetQuery(conn, "
          SELECT id
          FROM keyword_vocab_tbl
          WHERE field_code = ? COLLATE NOCASE
            AND option_value = ? COLLATE NOCASE
          LIMIT 1
        ", params = list(field_code, term))

        if (nrow(existing) > 0L) {
          next
        }

        DBI::dbExecute(conn, "
          INSERT INTO keyword_vocab_tbl (
            field_code,
            option_value,
            option_label,
            is_active,
            source,
            approval_status,
            created_at,
            created_by_user_id,
            updated_at,
            updated_by_user_id
          )
          VALUES (?, ?, ?, 1, 'dataset_custom', 'pending',
                  ?, ?, ?, ?)
        ", params = list(
          field_code,
          term,
          term,
          now,
          user_id,
          now,
          user_id
        ))

        inserted <- inserted + 1L
      }
    }
  })

  inserted
}

ensure_dataset_citation_key_column <- function(conn) {
  ensure_columns(
    conn,
    "entry_tbl",
    fields = c("citation_key"),
    types = c(citation_key = "TEXT")
  )

  invisible(TRUE)
}

ensure_dataset_license_table <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_license_tbl (
      id INTEGER PRIMARY KEY,

      dataset_id INTEGER NOT NULL UNIQUE,
      license_template_id INTEGER,

      license_identifier TEXT,
      license_name TEXT,
      license_description TEXT,
      license_template_notes TEXT,
      license_url TEXT,

      license_holder TEXT,
      access_level TEXT,
      use_constraints TEXT,

      sensitive_data INTEGER,
      sensitivity_reason TEXT,

      licence_notes TEXT,

      updated_at TEXT,
      updated_by_person_id INTEGER,

      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (license_template_id) REFERENCES license_template_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")

  invisible(TRUE)
}

# Licensing and sharing schema v2 ----

ensure_licensing_v2_schema <- function(conn) {
  ensure_license_template_table(
    conn
  )

  ensure_dataset_license_table(
    conn
  )

  ensure_columns(
    conn = conn,
    table = "license_template_tbl",
    fields = c(
      "ui_group",
      "ui_label",
      "sort_order",
      "is_advanced",
      "is_active"
    ),
    types = c(
      ui_group = "TEXT",
      ui_label = "TEXT",
      sort_order = "INTEGER",
      is_advanced = "INTEGER DEFAULT 0",
      is_active = "INTEGER DEFAULT 1"
    )
  )

  ensure_columns(
    conn = conn,
    table = "dataset_license_tbl",
    fields = c(
      "rights_basis_code",
      "rights_holder_organisation_id",
      "access_level_code",
      "redistribution_permission_code",
      "permission_reference",
      "required_attribution",
      "attribution_text",
      "sensitivity_handling_code",
      "terms_review_date"
    ),
    types = c(
      rights_basis_code = "TEXT",
      rights_holder_organisation_id = "INTEGER",
      access_level_code = "TEXT",
      redistribution_permission_code = "TEXT",
      permission_reference = "TEXT",
      required_attribution = "INTEGER DEFAULT 0",
      attribution_text = "TEXT",
      sensitivity_handling_code = "TEXT",
      terms_review_date = "TEXT"
    )
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE license_template_tbl
    SET
      ui_group = COALESCE(
        NULLIF(TRIM(ui_group), ''),
        'Other'
      ),
      ui_label = COALESCE(
        NULLIF(TRIM(ui_label), ''),
        license_name,
        license_identifier
      ),
      sort_order = COALESCE(
        sort_order,
        999
      ),
      is_advanced = COALESCE(
        is_advanced,
        0
      ),
      is_active = COALESCE(
        is_active,
        1
      )
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_license_tbl
    SET
      required_attribution =
        COALESCE(required_attribution, 0)
    "
  )

  invisible(TRUE)
}

save_data_file_scan <- function(
  dataset_id,
  files_dt,
  base_path,
  data_dir = NA_character_,
  data_pond_id = NA_character_
) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    stop("dataset_id is required to save detected files.")
  }
  if ("message" %in% names(files_dt)) {
    stop(files_dt$message[1])
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  ensure_data_file_tables(conn)
  # Resolve the pond id if it was not supplied by the app state.
  if (is.null(data_pond_id) ||
      length(data_pond_id) == 0 ||
      is.na(data_pond_id) ||
      data_pond_id == "") {
    data_pond_id <- ensure_data_pond_id(base_path, conn = conn)
  }
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dt <- data.table::as.data.table(data.table::copy(files_dt))
  dt[, data_pond_id := data_pond_id]

  # Prefer logical/enriched dataset fields when available.
  if ("logical_rel_path" %in% names(dt)) {
    dt[
      !is.na(logical_rel_path) & logical_rel_path != "",
      data_file_rel_path_full := logical_rel_path
    ]
  }
  if ("logical_file_path" %in% names(dt)) {
    dt[
      !is.na(logical_file_path) & logical_file_path != "",
      file_path := logical_file_path
    ]
  }
  if ("logical_file_name" %in% names(dt)) {
    dt[
      !is.na(logical_file_name) & logical_file_name != "",
      file_name := logical_file_name
    ]
  }
  if ("logical_dir" %in% names(dt)) {
    dt[
      !is.na(logical_dir),
      file_dir := logical_dir
    ]
  }
  if ("logical_stem" %in% names(dt)) {
    dt[
      !is.na(logical_stem) & logical_stem != "",
      file_stem := logical_stem
    ]
  }
  if ("logical_extension" %in% names(dt)) {
    dt[
      !is.na(logical_extension) & logical_extension != "",
      data_file_extension := logical_extension
    ]
  }
  if ("logical_format" %in% names(dt)) {
    dt[
      !is.na(logical_format) & logical_format != "",
      data_file_format := logical_format
    ]
  }
  if ("logical_size_kb" %in% names(dt)) {
    dt[
      !is.na(logical_size_kb),
      file_size_kb := logical_size_kb
    ]
  }
  if ("logical_file_role" %in% names(dt)) {
    dt[
      !is.na(logical_file_role) & logical_file_role != "",
      file_role := logical_file_role
    ]
  }

  optional_cols <- list(
    file_dir = NA_character_,
    file_key = NA_character_,
    group_family = NA_character_,
    group_key = NA_character_,
    sidecar_count = NA_integer_,
    sidecar_exts = NA_character_,
    sidecar_summary = NA_character_,
    primary_dataset = FALSE,
    is_container = FALSE,
    container_type = NA_character_
  )
  for (nm in names(optional_cols)) {
    if (!nm %in% names(dt)) {
      dt[, (nm) := optional_cols[[nm]]]
    }
  }
  dbBegin(conn)
  tryCatch({
    dbExecute(conn, "
      INSERT INTO data_file_scan_tbl (
        dataset_id,
        data_pond_id,
        data_dir,
        scan_timestamp,
        scan_status,
        files_detected,
        notes
      )
      VALUES (?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      dataset_id,
      data_pond_id,
      as.character(data_dir),
      now,
      "completed",
      nrow(dt),
      NA_character_
    ))
    scan_id <- dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
    existing <- dbGetQuery(
      conn,
      "
      SELECT data_file_rel_path_full
      FROM data_file_tbl
      WHERE dataset_id = ?
      ",
      params = list(dataset_id)
    )
    existing_keys <- existing$data_file_rel_path_full
    current_keys <- dt$data_file_rel_path_full
    new_keys <- setdiff(current_keys, existing_keys)
    updated_keys <- intersect(current_keys, existing_keys)
    missing_keys <- setdiff(existing_keys, current_keys)
    upsert_sql <- "
      INSERT INTO data_file_tbl (
        dataset_id,
        data_pond_id,
        data_file_rel_path_full,
        file_name,
        file_dir,
        file_stem,
        file_key,
        file_path,
        data_file_extension,
        data_file_format,
        file_role,
        is_container,
        container_type,
        group_family,
        group_key,
        primary_dataset,
        sidecar_count,
        sidecar_exts,
        sidecar_summary,
        file_size_kb,
        modified_time,
        scan_status,
        active,
        first_seen_at,
        last_seen_at,
        missing_since,
        latest_scan_id
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(dataset_id, data_file_rel_path_full)
      DO UPDATE SET
        data_pond_id = excluded.data_pond_id,
        file_name = excluded.file_name,
        file_dir = excluded.file_dir,
        file_stem = excluded.file_stem,
        file_key = excluded.file_key,
        file_path = excluded.file_path,
        data_file_extension = excluded.data_file_extension,
        data_file_format = excluded.data_file_format,
        file_role = excluded.file_role,
        is_container = excluded.is_container,
        container_type = excluded.container_type,
        group_family = excluded.group_family,
        group_key = excluded.group_key,
        primary_dataset = excluded.primary_dataset,
        sidecar_count = excluded.sidecar_count,
        sidecar_exts = excluded.sidecar_exts,
        sidecar_summary = excluded.sidecar_summary,
        file_size_kb = excluded.file_size_kb,
        modified_time = excluded.modified_time,
        scan_status = 'present',
        active = 1,
        last_seen_at = excluded.last_seen_at,
        missing_since = NULL,
        latest_scan_id = excluded.latest_scan_id
    "
    for (i in seq_len(nrow(dt))) {
      row <- dt[i]
      dbExecute(conn, upsert_sql, params = list(
        dataset_id,
        row$data_pond_id,
        row$data_file_rel_path_full,
        row$file_name,
        row$file_dir,
        row$file_stem,
        row$file_key,
        row$file_path,
        row$data_file_extension,
        row$data_file_format,
        row$file_role,
        as.integer(isTRUE(row$is_container)),
        row$container_type,
        row$group_family,
        row$group_key,
        as.integer(isTRUE(row$primary_dataset)),
        as.integer(row$sidecar_count),
        row$sidecar_exts,
        row$sidecar_summary,
        as.numeric(row$file_size_kb),
        row$modified_time,
        "present",
        1L,
        now,
        now,
        NA_character_,
        scan_id
      ))
    }
    if (length(missing_keys) > 0) {
      for (k in missing_keys) {
        dbExecute(conn, "
          UPDATE data_file_tbl
          SET
            scan_status = 'missing',
            active = 0,
            missing_since = COALESCE(missing_since, ?),
            latest_scan_id = ?
          WHERE dataset_id = ?
            AND data_file_rel_path_full = ?
        ", params = list(
          now,
          scan_id,
          dataset_id,
          k
        ))
      }
    }
    dbCommit(conn)
    data.table::data.table(
      scan_id = scan_id,
      files_scanned = nrow(dt),
      new_files = length(new_keys),
      updated_files = length(updated_keys),
      missing_files = length(missing_keys)
    )
  }, error = function(e) {
    dbRollback(conn)
    stop(e)
  })
}

save_entry_dynamic <- function(table, data, base_path, id = NULL) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  # Only a new application record establishes creation now. Imports use
  # transfer_insert_row; migrations and ordinary updates must never backdate.
  if (identical(table, "entry_tbl") && (is.null(id) || length(id) == 0L || is.na(id))) {
    if (!non_empty_scalar(data$metadata_creation_date))
      data$metadata_creation_date <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  ensure_columns(conn, table, names(data))
  # INSERT
  if (is.null(id) || length(id) == 0 || is.na(id)) {
    fields <- names(data)
    placeholders <- paste(rep("?", length(fields)), collapse = ", ")
    sql <- sprintf(
      "INSERT INTO %s (%s) VALUES (%s)",
      table,
      paste(fields, collapse = ", "),
      placeholders
    )
    dbExecute(conn, sql, params = unname(as.list(data)))
    return(
      dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
    )
  }
  # UPDATE
  fields <- setdiff(names(data), "id")
  if (length(fields) == 0) {
    return(id)
  }
  set_clause <- paste0(fields, " = ?", collapse = ", ")
  sql <- sprintf(
    "UPDATE %s SET %s WHERE id = ?",
    table,
    set_clause
  )
  params <- c(
    unname(as.list(data[fields])),
    list(id)
  )
  dbExecute(conn, sql, params = params)
  id
}

clear_dataset_relationships <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)

  dbExecute(
    conn, "DELETE FROM authority_tbl WHERE dataset_id = ?",
    params = list(dataset_id)
  )
  dbExecute(conn,
    "DELETE FROM poc_tbl WHERE dataset_id = ?", params = list(dataset_id)
  )
  dbExecute(conn,
    "DELETE FROM dataset_organisation_tbl WHERE dataset_id = ?",
    params = list(dataset_id)
  )
  invisible(TRUE)
}

# metadata review & control ----
ensure_metadata_review_table <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS metadata_review_tbl (
      id INTEGER PRIMARY KEY,

      dataset_id INTEGER NOT NULL,

      review_timestamp TEXT NOT NULL,
      review_date TEXT,
      reviewer_id INTEGER,
      reviewer_name TEXT,
      reviewer_role TEXT,

      update_type TEXT,
      update_reason TEXT,
      update_summary TEXT,

      metadata_section TEXT,
      metadata_status TEXT,

      source_of_change TEXT,
      app_version TEXT,

      notes TEXT,

      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (reviewer_id) REFERENCES persons_tbl(id)
    )
  ")
  invisible(TRUE)
}

# metadata control options ----
metadata_update_types <- c(
  "new_metadata_record",
  "metadata_development",
  "metadata_revision",
  "metadata_correction",
  "routine_review",
  "file_inventory_refresh",
  "spatial_metadata_refresh",
  "attribute_metadata_update",
  "contact_update",
  "citation_update",
  "keyword_update",
  "quality_review",
  "other"
)

metadata_sections <- c(
  "dataset_overview",
  "citation",
  "about",
  "parties",
  "licensing",
  "data_access",
  "lineage",
  "data_files",
  "spatial_metadata",
  "attribute_metadata",
  "general"
)

metadata_status_values <- c(
  "draft",
  "reviewed",
  "approved",
  "needs_attention",
  "deprecated"
)

insert_metadata_review <- function(
  dataset_id,
  base_path,
  reviewer_id = NA_character_,
  reviewer_name = NA_character_,
  reviewer_role = NA_character_,
  update_type = "metadata_revision",
  update_reason,
  update_summary = NA_character_,
  metadata_section = "general",
  metadata_status = "reviewed",
  source_of_change = "manual_edit",
  app_version = NA_character_,
  notes = NA_character_
) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    stop("dataset_id is required.")
  }

  if (is.null(update_reason) || length(update_reason) == 0 || is.na(update_reason) ||
      update_reason == "") {
    stop("update_reason is required.")
  }

  collapse_multi <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      return(NA_character_)
    }
    x <- as.character(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) {
      return(NA_character_)
    }
    paste(unique(x), collapse = "; ")
  }
  update_type <- collapse_multi(update_type)
  update_reason <- collapse_multi(update_reason)
  update_summary <- collapse_multi(update_summary)
  metadata_section <- collapse_multi(metadata_section)
  reviewer_name <- collapse_multi(reviewer_name)
  reviewer_role <- collapse_multi(reviewer_role)
  metadata_status <- collapse_multi(metadata_status)
  source_of_change <- collapse_multi(source_of_change)
  app_version <- collapse_multi(app_version)
  notes <- collapse_multi(notes)

  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)

  ensure_metadata_review_table(conn)

  now <- Sys.time()

  dbExecute(conn, "
    INSERT INTO metadata_review_tbl (
      dataset_id,
      review_timestamp,
      review_date,
      reviewer_id,
      reviewer_name,
      reviewer_role,
      update_type,
      update_reason,
      update_summary,
      metadata_section,
      metadata_status,
      source_of_change,
      app_version,
      notes
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    dataset_id,
    format(now, "%Y-%m-%d %H:%M:%S"),
    format(as.Date(now), "%Y-%m-%d"),
    reviewer_id,
    reviewer_name,
    reviewer_role,
    update_type,
    update_reason,
    update_summary,
    metadata_section,
    metadata_status,
    source_of_change,
    app_version,
    notes
  ))

  dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
}

get_metadata_reviews <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(data.frame())
  }

  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)

  dbGetQuery(conn, "
    SELECT
      r.id,
      r.review_timestamp,
      r.reviewer_id,
      p.name_first || ' ' || p.name_last AS reviewer_name,
      r.reviewer_role,
      r.update_type,
      r.metadata_section,
      r.metadata_status,
      r.update_reason,
      r.update_summary,
      r.source_of_change,
      r.notes
    FROM metadata_review_tbl r
    LEFT JOIN persons_tbl p ON r.reviewer_id = p.id
    WHERE r.dataset_id = ?
    ORDER BY r.review_timestamp DESC, r.id DESC
  ", params = list(dataset_id))
}

get_users <- function(base_path) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, "
    SELECT
      u.id,
      u.person_id,
      u.is_admin,
      p.name_first,
      p.name_last,
      p.email,
      p.role_title
    FROM user_tbl u
    LEFT JOIN persons_tbl p ON u.person_id = p.id
    WHERE u.is_active = 1
  ")
}

insert_user <- function(person_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbExecute(conn, "
    INSERT OR IGNORE INTO user_tbl (
      person_id,
      user_name,
      is_active
    )
    VALUES (?, ?, 1)
  ", params = list(
    person_id,
    paste0("user_", person_id)
  ))
}

ensure_license_template_table <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS license_template_tbl (
      id INTEGER PRIMARY KEY,
      license_identifier TEXT UNIQUE NOT NULL,
      license_name TEXT,
      description TEXT,
      notes TEXT,
      url TEXT
    )
  ")
  invisible(TRUE)
}

seed_license_templates <- function(conn) {
  ensure_licensing_v2_schema(
    conn
  )

  templates <- data.frame(
    license_identifier = c(
      "CC-BY-4.0",
      "CC0-1.0",
      "CC-BY-SA-4.0",
      "CC-BY-NC-4.0",
      "ODbL-1.0",
      "PROVIDER-LICENCE",
      "REDISTRIBUTION-PERMISSION",
      "EKZNW-PARTNER",
      "EKZNW-INTERNAL",
      "CUSTOM-TERMS",
      "NO-LICENCE"
    ),

    license_name = c(
      "Creative Commons Attribution 4.0",
      "Creative Commons CC0 1.0",
      "Creative Commons Attribution Share-Alike 4.0",
      "Creative Commons Attribution Non-Commercial 4.0",
      "Open Database Licence 1.0",
      "External provider licence",
      "Redistribution permitted by written agreement",
      "EKZNW approved-partner terms",
      "EKZNW internal-use terms",
      "Custom or mixed sharing terms",
      "Rights unclear or no licence specified"
    ),

    description = c(
      paste(
        "Public redistribution and adaptation are permitted.",
        "Users must acknowledge the data provider."
      ),

      paste(
        "The dataset is dedicated to the public domain",
        "to the greatest extent possible."
      ),

      paste(
        "Public redistribution and adaptation are permitted",
        "with attribution and share-alike requirements."
      ),

      paste(
        "Public non-commercial reuse is permitted",
        "with attribution."
      ),

      paste(
        "Database reuse is permitted under attribution",
        "and share-alike conditions."
      ),

      paste(
        "The external provider's original licence applies.",
        "The supplied licence must accompany redistribution."
      ),

      paste(
        "Redistribution is authorised by an agreement,",
        "permission letter, contract, email or memorandum."
      ),

      paste(
        "Use is restricted to approved partners.",
        "Onward redistribution is prohibited unless authorised."
      ),

      paste(
        "Use is restricted to EKZNW.",
        "External redistribution is prohibited unless authorised."
      ),

      paste(
        "Sharing is governed by custom, mixed or",
        "dataset-specific terms."
      ),

      paste(
        "Rights are unclear or no usable licence has been",
        "identified. Redistribution is not authorised."
      )
    ),

    notes = c(
      "Recommended default for EKZNW-created public datasets requiring acknowledgement.",
      "Use only when no mandatory attribution is intended.",
      "Advanced option. Use only when share-alike is intentional.",
      "Advanced option. Non-commercial terms are not a sensitivity control.",
      "Advanced database-specific option.",
      "Record the provider, licence reference and required attribution.",
      "Record the permission reference and all redistribution conditions.",
      "Controlled sharing rather than an open licence.",
      "Controlled internal access rather than an open licence.",
      "Explain all applicable rights, access and redistribution conditions.",
      "Use until rights have been clarified."
    ),

    url = c(
      "https://creativecommons.org/licenses/by/4.0/",
      "https://creativecommons.org/publicdomain/zero/1.0/",
      "https://creativecommons.org/licenses/by-sa/4.0/",
      "https://creativecommons.org/licenses/by-nc/4.0/",
      "https://opendatacommons.org/licenses/odbl/1-0/",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),

    ui_group = c(
      "Recommended public sharing",
      "Recommended public sharing",
      "Advanced public licences",
      "Advanced public licences",
      "Advanced public licences",
      "External-source terms",
      "External-source terms",
      "Controlled sharing",
      "Controlled sharing",
      "Controlled sharing",
      "Controlled sharing"
    ),

    ui_label = c(
      "Public reuse \u2014 acknowledgement required (CC BY 4.0)",
      "Public reuse \u2014 no mandatory acknowledgement (CC0)",
      "Public reuse \u2014 derivatives must use the same terms",
      "Public non-commercial reuse \u2014 acknowledgement required",
      "Open database reuse \u2014 attribution and share-alike",
      "Use the external provider's licence",
      "Redistribution permitted by written agreement",
      "Approved partners only \u2014 no onward redistribution",
      "EKZNW internal use only",
      "Custom or mixed conditions",
      "Rights unclear \u2014 do not redistribute"
    ),

    sort_order = c(
      10L,
      20L,
      30L,
      40L,
      50L,
      60L,
      70L,
      80L,
      90L,
      100L,
      110L
    ),

    is_advanced = c(
      0L,
      0L,
      1L,
      1L,
      1L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L
    ),

    is_active = rep(
      1L,
      11L
    ),

    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(templates))) {
    DBI::dbExecute(
      conn,
      "
      INSERT INTO license_template_tbl (
        license_identifier,
        license_name,
        description,
        notes,
        url,
        ui_group,
        ui_label,
        sort_order,
        is_advanced,
        is_active
      )
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(license_identifier) DO UPDATE SET
        license_name = excluded.license_name,
        description = excluded.description,
        notes = excluded.notes,
        url = excluded.url,
        ui_group = excluded.ui_group,
        ui_label = excluded.ui_label,
        sort_order = excluded.sort_order,
        is_advanced = excluded.is_advanced,
        is_active = excluded.is_active
      ",
      params = list(
        templates$license_identifier[i],
        templates$license_name[i],
        templates$description[i],
        templates$notes[i],
        templates$url[i],
        templates$ui_group[i],
        templates$ui_label[i],
        templates$sort_order[i],
        templates$is_advanced[i],
        templates$is_active[i]
      )
    )
  }

  invisible(TRUE)
}

get_license_templates <- function(base_path) {
  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_licensing_v2_schema(
    conn
  )

  DBI::dbGetQuery(
    conn,
    "
    SELECT *
    FROM license_template_tbl
    WHERE COALESCE(is_active, 1) = 1
    ORDER BY
      COALESCE(sort_order, 999),
      license_identifier
    "
  )
}

get_dataset_license <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(NULL)
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  res <- dbGetQuery(conn, "
    SELECT *
    FROM dataset_license_tbl
    WHERE dataset_id = ?
  ", params = list(dataset_id))
  if (nrow(res) == 0) return(NULL)
  res[1, , drop = FALSE]
}

licensing_data_value <- function(
  data,
  field,
  default = NA_character_
) {
  if (
    is.null(data) ||
    !field %in% names(data) ||
    is.null(data[[field]]) ||
    length(data[[field]]) == 0L ||
    all(is.na(data[[field]]))
  ) {
    return(default)
  }

  value <- data[[field]][1]

  if (
    is.character(value) &&
    !nzchar(trimws(value))
  ) {
    return(default)
  }

  value
}


licensing_access_level_label <- function(code) {
  switch(
    as.character(code),

    public =
      "Public",

    public_request =
      "Public after request or registration",

    approved_partners =
      "Approved partners",

    internal =
      "EKZNW internal",

    embargoed =
      "Embargoed",

    not_distributable =
      "Not distributable",

    NA_character_
  )
}


save_dataset_license <- function(
  dataset_id,
  template_row,
  data,
  base_path,
  user_person_id = NA_integer_
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    stop(
      "dataset_id is required."
    )
  }

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_licensing_v2_schema(
    conn
  )

  ensure_legacy_patch_v1_marker(conn, "licensing")

  template_value <- function(
    field,
    default = NA_character_
  ) {
    if (
      is.null(template_row) ||
      nrow(as.data.frame(template_row)) == 0L ||
      !field %in% names(template_row) ||
      is.null(template_row[[field]]) ||
      length(template_row[[field]]) == 0L ||
      is.na(template_row[[field]][1])
    ) {
      return(default)
    }

    as.character(
      template_row[[field]][1]
    )
  }

  template_id <- if (
    !is.null(template_row) &&
    nrow(as.data.frame(template_row)) > 0L &&
    "id" %in% names(template_row) &&
    !is.na(template_row$id[1])
  ) {
    as.integer(
      template_row$id[1]
    )
  } else {
    NA_integer_
  }

  rights_holder_organisation_id <-
    suppressWarnings(
      as.integer(
        licensing_data_value(
          data,
          "rights_holder_organisation_id",
          default = NA_integer_
        )
      )
    )

  access_level_code <-
    licensing_data_value(
      data,
      "access_level_code"
    )

  sensitivity_handling_code <-
    licensing_data_value(
      data,
      "sensitivity_handling_code",
      default = "none"
    )

  sensitive_data <- !is.na(
    sensitivity_handling_code
  ) &&
    !sensitivity_handling_code %in%
      c(
        "",
        "none"
      )

  now <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  DBI::dbExecute(
    conn,
    "
    INSERT INTO dataset_license_tbl (
      dataset_id,
      license_template_id,
      license_identifier,
      license_name,
      license_description,
      license_template_notes,
      license_url,

      license_holder,
      rights_holder_organisation_id,
      rights_basis_code,

      access_level,
      access_level_code,
      redistribution_permission_code,
      permission_reference,

      required_attribution,
      attribution_text,
      use_constraints,

      sensitive_data,
      sensitivity_handling_code,
      sensitivity_reason,

      terms_review_date,
      licence_notes,

      updated_at,
      updated_by_person_id,
      legacy_licensing_v1_migrated_at
    )
    VALUES (
      ?, ?, ?, ?, ?, ?, ?,
      ?, ?, ?,
      ?, ?, ?, ?,
      ?, ?, ?,
      ?, ?, ?,
      ?, ?,
      ?, ?, CURRENT_TIMESTAMP
    )
    ON CONFLICT(dataset_id) DO UPDATE SET
      legacy_licensing_v1_migrated_at = COALESCE(
        dataset_license_tbl.legacy_licensing_v1_migrated_at, excluded.legacy_licensing_v1_migrated_at),
      license_template_id =
        excluded.license_template_id,
      license_identifier =
        excluded.license_identifier,
      license_name =
        excluded.license_name,
      license_description =
        excluded.license_description,
      license_template_notes =
        excluded.license_template_notes,
      license_url =
        excluded.license_url,

      license_holder =
        excluded.license_holder,
      rights_holder_organisation_id =
        excluded.rights_holder_organisation_id,
      rights_basis_code =
        excluded.rights_basis_code,

      access_level =
        excluded.access_level,
      access_level_code =
        excluded.access_level_code,
      redistribution_permission_code =
        excluded.redistribution_permission_code,
      permission_reference =
        excluded.permission_reference,

      required_attribution =
        excluded.required_attribution,
      attribution_text =
        excluded.attribution_text,
      use_constraints =
        excluded.use_constraints,

      sensitive_data =
        excluded.sensitive_data,
      sensitivity_handling_code =
        excluded.sensitivity_handling_code,
      sensitivity_reason =
        excluded.sensitivity_reason,

      terms_review_date =
        excluded.terms_review_date,
      licence_notes =
        excluded.licence_notes,

      updated_at =
        excluded.updated_at,
      updated_by_person_id =
        excluded.updated_by_person_id
    ",
    params = list(
      as.integer(dataset_id[1]),
      template_id,
      template_value(
        "license_identifier"
      ),
      template_value(
        "license_name"
      ),
      template_value(
        "description"
      ),
      template_value(
        "notes"
      ),
      template_value(
        "url"
      ),

      licensing_data_value(
        data,
        "license_holder"
      ),
      rights_holder_organisation_id,
      licensing_data_value(
        data,
        "rights_basis_code"
      ),

      licensing_access_level_label(
        access_level_code
      ),
      access_level_code,
      licensing_data_value(
        data,
        "redistribution_permission_code"
      ),
      licensing_data_value(
        data,
        "permission_reference"
      ),

      as.integer(
        isTRUE(
          licensing_data_value(
            data,
            "required_attribution",
            default = FALSE
          )
        )
      ),
      licensing_data_value(
        data,
        "attribution_text"
      ),
      licensing_data_value(
        data,
        "use_constraints"
      ),

      as.integer(
        sensitive_data
      ),
      sensitivity_handling_code,
      licensing_data_value(
        data,
        "sensitivity_reason"
      ),

      licensing_data_value(
        data,
        "terms_review_date"
      ),
      licensing_data_value(
        data,
        "licence_notes"
      ),

      now,
      user_person_id
    )
  )

  invisible(TRUE)
}

# supporting documentation ----
ensure_supporting_document_table <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS supporting_document_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      document_rel_path TEXT NOT NULL,
      document_name TEXT,
      document_dir TEXT,
      document_path TEXT,
      document_extension TEXT,
      document_type_auto_code TEXT,
      document_type_code TEXT,
      document_type_label TEXT,
      document_type_auto_text TEXT,
      document_type_user_set INTEGER DEFAULT 0,
      document_type_user_text TEXT,
      include_in_lineage INTEGER,
      include_in_metadata INTEGER,
      notes TEXT,
      active INTEGER,
      first_seen_at TEXT,
      last_seen_at TEXT,
      missing_since TEXT,
      classified_at TEXT,
      classified_by_person_id INTEGER,
      reviewed_at TEXT,
      reviewed_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (classified_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (reviewed_by_person_id) REFERENCES persons_tbl(id)
    )
  ")

  ensure_columns(
    conn,
    "supporting_document_tbl",
    fields = c(
      "document_type_user_set",
      "classified_at",
      "classified_by_person_id",
      "reviewed_at",
      "reviewed_by_person_id"
    ),
    types = c(
      document_type_user_set = "INTEGER DEFAULT 0",
      classified_at = "TEXT",
      classified_by_person_id = "INTEGER",
      reviewed_at = "TEXT",
      reviewed_by_person_id = "INTEGER"
    )
  )

  # A legacy user override was necessarily an explicit review. Automatic
  # classifications are deliberately left unreviewed for one-pass approval.
  DBI::dbExecute(
    conn,
    "
    UPDATE supporting_document_tbl
    SET
      reviewed_at = COALESCE(
        classified_at,
        last_seen_at,
        first_seen_at,
        datetime('now')
      ),
      reviewed_by_person_id = classified_by_person_id
    WHERE document_type_user_set = 1
      AND reviewed_at IS NULL
    "
  )

  dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_supporting_doc_dataset_relpath
    ON supporting_document_tbl(dataset_id, document_rel_path)
  ")
  invisible(TRUE)
}

update_supporting_document <- function(
  doc_id,
  data,
  base_path,
  user_person_id = NA_integer_
) {
  if (
    is.null(doc_id) ||
    length(doc_id) == 0 ||
    is.na(doc_id)
  ) {
    stop("doc_id is required.")
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  now <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE supporting_document_tbl
    SET
      document_type_code = ?,
      document_type_label = ?,
      document_type_user_set = ?,
      document_type_user_text = ?,
      include_in_lineage = ?,
      include_in_metadata = ?,
      notes = ?,
      classified_at = ?,
      classified_by_person_id = ?,
      reviewed_at = ?,
      reviewed_by_person_id = ?
    WHERE id = ?
    ",
    params = list(
      data$document_type_code,
      data$document_type_label,
      as.integer(
        isTRUE(data$document_type_user_set)
      ),
      data$document_type_user_text,
      as.integer(
        isTRUE(data$include_in_lineage)
      ),
      as.integer(
        isTRUE(data$include_in_metadata)
      ),
      data$notes,
      now,
      user_person_id,
      now,
      user_person_id,
      as.integer(doc_id)
    )
  )

  invisible(TRUE)
}

accept_supporting_document_defaults <- function(
  dataset_id,
  base_path,
  user_person_id = NA_integer_
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    stop("dataset_id is required.")
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  reviewed <- DBI::dbExecute(
    conn,
    "
    UPDATE supporting_document_tbl
    SET
      reviewed_at = ?,
      reviewed_by_person_id = ?
    WHERE dataset_id = ?
      AND active = 1
      AND COALESCE(document_type_user_set, 0) = 0
      AND reviewed_at IS NULL
    ",
    params = list(
      now,
      user_person_id,
      as.integer(dataset_id[1])
    )
  )

  as.integer(reviewed)
}

supporting_document_type_choices <- c(
  "Lineage document" = "lineage",
  "Article" = "article",
  "Technical report" = "technical_report",
  "Accuracy report" = "accuracy_report",
  "Thesis" = "thesis",
  "Method statement" = "method_statement",
  "Script" = "script",
  "Data dictionary" = "data_dictionary",
  "Dataset intake notes" = "dataset_intake_notes",
  "Metadata note" = "metadata_note",
  "Map layout" = "map_layout",
  "Legal document" = "legal_document",
  "Supporting document" = "other"
)

supporting_document_type_label <- function(code) {
  choices <- supporting_document_type_choices
  hit <- names(choices)[choices == code]
  if (length(hit) == 0) return("Supporting document")
  hit[1]
}

upsert_supporting_document <- function(dataset_id, row, class, base_path, user_person_id) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  dbExecute(conn, "
    INSERT INTO supporting_document_tbl (
      dataset_id,
      document_rel_path,
      document_name,
      document_dir,
      document_path,
      document_extension,
      document_type_auto_code,
      document_type_code,
      document_type_label,
      document_type_auto_text,
      document_type_user_set,
      document_type_user_text,
      include_in_lineage,
      include_in_metadata,
      notes,
      active,
      first_seen_at,
      last_seen_at,
      missing_since,
      classified_at,
      classified_by_person_id,
      reviewed_at,
      reviewed_by_person_id
    )
    VALUES (
      ?, ?, ?, ?, ?, ?,
      ?, ?, ?, ?,
      0, NULL,
      ?, ?, ?,
      ?, ?, ?, ?, ?, ?,
      NULL, NULL
    )
    ON CONFLICT(dataset_id, document_rel_path)
    DO UPDATE SET
      document_name = excluded.document_name,
      document_dir = excluded.document_dir,
      document_path = excluded.document_path,
      document_extension = excluded.document_extension,
      document_type_auto_code =
        excluded.document_type_auto_code,

      document_type_code = CASE
        WHEN supporting_document_tbl.document_type_user_set = 1
        THEN supporting_document_tbl.document_type_code
        ELSE excluded.document_type_code
      END,

      document_type_label = CASE
        WHEN supporting_document_tbl.document_type_user_set = 1
        THEN supporting_document_tbl.document_type_label
        ELSE excluded.document_type_label
      END,

      document_type_auto_text =
        excluded.document_type_auto_text,
      active = 1,
      last_seen_at = excluded.last_seen_at,
      missing_since = NULL,
      classified_at = CASE
        WHEN supporting_document_tbl.document_type_user_set = 1
        THEN supporting_document_tbl.classified_at
        ELSE excluded.classified_at
      END,

      classified_by_person_id = CASE
        WHEN supporting_document_tbl.document_type_user_set = 1
        THEN supporting_document_tbl.classified_by_person_id
        ELSE excluded.classified_by_person_id
      END,

      reviewed_at = CASE
        WHEN supporting_document_tbl.document_type_user_set = 1
        THEN supporting_document_tbl.reviewed_at
        WHEN COALESCE(supporting_document_tbl.document_type_auto_code, '') <>
             COALESCE(excluded.document_type_auto_code, '')
        THEN NULL
        ELSE supporting_document_tbl.reviewed_at
      END,

      reviewed_by_person_id = CASE
        WHEN supporting_document_tbl.document_type_user_set = 1
        THEN supporting_document_tbl.reviewed_by_person_id
        WHEN COALESCE(supporting_document_tbl.document_type_auto_code, '') <>
             COALESCE(excluded.document_type_auto_code, '')
        THEN NULL
        ELSE supporting_document_tbl.reviewed_by_person_id
      END
  ", params = list(
    dataset_id,
    row$document_rel_path,
    row$document_name,
    row$document_dir,
    row$document_path,
    row$document_extension,
    class$code,  # document_type_auto_code
    class$code,  # initial effective document_type_code
    class$label,
    class$auto,
    as.integer(identical(class$code, "lineage")),
    1L,
    NA_character_,
    1L,
    now,
    now,
    NA_character_,
    now,
    user_person_id
  ))
}

scan_supporting_documents <- function(dataset_id, dataset_path, base_path, user_person_id = NA_integer_) {
  docs <- list_supporting_document_files(dataset_path)
  if ("message" %in% names(docs)) {
    return(docs)
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  existing <- dbGetQuery(
    conn,
    "
    SELECT document_rel_path
    FROM supporting_document_tbl
    WHERE dataset_id = ?
    ",
    params = list(dataset_id)
  )
  existing_keys <- existing$document_rel_path
  current_keys <- docs$document_rel_path
  missing_keys <- setdiff(existing_keys, current_keys)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  for (i in seq_len(nrow(docs))) {
    row <- docs[i, ]
    class <- classify_supporting_document(
      row$document_name,
      row$document_rel_path,
      row$document_extension
    )
    upsert_supporting_document(
      dataset_id = dataset_id,
      row = row,
      class = class,
      base_path = base_path,
      user_person_id = user_person_id
    )
  }
  if (length(missing_keys) > 0) {
    for (k in missing_keys) {
      dbExecute(conn, "
        UPDATE supporting_document_tbl
        SET active = 0,
            missing_since = COALESCE(missing_since, ?)
        WHERE dataset_id = ?
          AND document_rel_path = ?
      ", params = list(
        now,
        dataset_id,
        k
      ))
    }
  }
  data.table::data.table(
    documents_detected = nrow(docs),
    missing_documents = length(missing_keys)
  )
}

get_supporting_documents <- function(dataset_id, base_path, active_only = TRUE) {
  if (is.null(dataset_id) ||
      length(dataset_id) == 0 ||
      is.na(dataset_id)) {
    return(data.frame())
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  where_active <- if (isTRUE(active_only)) {
    "AND sd.active = 1"
  } else {
    ""
  }
  sql <- paste0("
    SELECT
      sd.*,
      p.name_first || ' ' || p.name_last AS classified_by_name,
      reviewer.name_first || ' ' || reviewer.name_last AS reviewed_by_name
    FROM supporting_document_tbl sd
    LEFT JOIN persons_tbl p ON sd.classified_by_person_id = p.id
    LEFT JOIN persons_tbl reviewer ON sd.reviewed_by_person_id = reviewer.id
    WHERE sd.dataset_id = ?
      ", where_active, "
    ORDER BY
      sd.active DESC,
      sd.document_dir,
      sd.document_name
  ")
  dbGetQuery(conn, sql, params = list(dataset_id))
}

ensure_dataset_lineage_table <- function(conn) {
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_lineage_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL UNIQUE,
      lineage_source_mode TEXT DEFAULT 'form',
      lineage_statement TEXT,
      source_description TEXT,
      source_datasets_text TEXT,
      process_description TEXT,
      processing_tools TEXT,
      processing_scripts TEXT,
      output_description TEXT,
      lineage_notes TEXT,
      lineage_status TEXT,
      updated_at TEXT,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  invisible(TRUE)
}

lineage_value_origin_codes <- function() {
  c("user", "supporting_document", "extracted_metadata", "legacy_backfill")
}

# An empty editor choice means automatic for a new row and unchanged for an
# existing row. Exploring the selector must not erase recorded provenance.
lineage_origin_for_save <- function(conn, table, dataset_id, record_id, value, default) {
  origin <- normalise_lineage_optional_text(value)
  if (is.na(origin) && !is.na(record_id)) {
    saved <- DBI::dbGetQuery(conn, paste0(
      "SELECT value_origin_code FROM ", table, " WHERE id = ? AND dataset_id = ?"
    ), params = list(record_id, as.integer(dataset_id)))
    if (nrow(saved)) origin <- normalise_lineage_optional_text(saved$value_origin_code[1])
  }
  if (is.na(origin)) origin <- default
  if (!origin %in% lineage_value_origin_codes()) stop("Unknown lineage value origin.", call. = FALSE)
  origin
}

lineage_responsible_role_codes <- function() {
  c(
    "resourceProvider", "custodian", "owner", "user", "distributor",
    "originator", "pointOfContact", "principalInvestigator", "processor",
    "publisher", "author", "collaborator"
  )
}

ensure_dataset_lineage_detail_tables <- function(conn, migrate = TRUE) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_lineage_source_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      source_order INTEGER NOT NULL DEFAULT 1,
      title TEXT,
      identifier TEXT,
      identifier_code_space TEXT,
      description TEXT,
      source_uri TEXT,
      citation_date TEXT,
      citation_date_type_code TEXT,
      temporal_start TEXT,
      temporal_end TEXT,
      equivalent_scale_denominator INTEGER,
      spatial_resolution_value REAL,
      spatial_resolution_unit TEXT,
      responsible_person_id INTEGER,
      responsible_organisation_id INTEGER,
      role_code TEXT,
      supporting_document_id INTEGER,
      value_origin_code TEXT NOT NULL DEFAULT 'user',
      is_provisional INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (responsible_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (responsible_organisation_id) REFERENCES organisation_tbl(id),
      FOREIGN KEY (supporting_document_id) REFERENCES supporting_document_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_lineage_process_step_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      step_order INTEGER NOT NULL DEFAULT 1,
      description TEXT,
      rationale TEXT,
      step_datetime TEXT,
      processor_person_id INTEGER,
      processor_organisation_id INTEGER,
      role_code TEXT,
      tool_software_reference TEXT,
      script_reference TEXT,
      output_description TEXT,
      value_origin_code TEXT NOT NULL DEFAULT 'user',
      is_provisional INTEGER NOT NULL DEFAULT 0,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (processor_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (processor_organisation_id) REFERENCES organisation_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id),
      FOREIGN KEY (updated_by_person_id) REFERENCES persons_tbl(id)
    )
  ")
  # dataset_id is deliberately present on the junction table. Besides enforcing
  # dataset boundaries, it lets portable dataset bundles discover and transfer it.
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_lineage_step_source_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      process_step_id INTEGER NOT NULL,
      source_id INTEGER NOT NULL,
      created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
      created_by_person_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (process_step_id) REFERENCES dataset_lineage_process_step_tbl(id),
      FOREIGN KEY (source_id) REFERENCES dataset_lineage_source_tbl(id),
      FOREIGN KEY (created_by_person_id) REFERENCES persons_tbl(id)
    )
  ")

  DBI::dbExecute(conn, paste(
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_lineage_source_order",
    "ON dataset_lineage_source_tbl(dataset_id, source_order)"
  ))
  DBI::dbExecute(conn, paste(
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_lineage_step_order",
    "ON dataset_lineage_process_step_tbl(dataset_id, step_order)"
  ))
  DBI::dbExecute(conn, paste(
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_lineage_step_source",
    "ON dataset_lineage_step_source_tbl(process_step_id, source_id)"
  ))
  DBI::dbExecute(conn, paste(
    "CREATE INDEX IF NOT EXISTS idx_lineage_source_document",
    "ON dataset_lineage_source_tbl(supporting_document_id)"
  ))

  ensure_legacy_patch_v1_marker(conn, "lineage")
  if (isTRUE(migrate)) apply_legacy_patch_v1(conn, "lineage")
  invisible(TRUE)
}

merge_lineage_narrative <- function(statement, notes) {
  statement <- normalise_lineage_optional_text(statement)
  notes <- normalise_lineage_optional_text(notes)
  if (is.na(notes)) return(statement)
  if (is.na(statement)) return(notes)
  if (identical(statement, notes) ||
      endsWith(statement, paste0("\n\nAdditional lineage notes: ", notes))) return(statement)
  paste0(statement, "\n\nAdditional lineage notes: ", notes)
}

get_dataset_lineage <- function(dataset_id, base_path) {
  if (is.null(dataset_id) ||
      length(dataset_id) == 0 ||
      is.na(dataset_id)) {
    return(NULL)
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  res <- dbGetQuery(conn, "
    SELECT *
    FROM dataset_lineage_tbl
    WHERE dataset_id = ?
  ", params = list(dataset_id))
  if (nrow(res) == 0) return(NULL)
  res[1, , drop = FALSE]
}

save_dataset_lineage <- function(dataset_id, data, base_path, user_person_id = NA_integer_) {
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  saved <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_lineage_tbl WHERE dataset_id = ?",
                          params = list(dataset_id))
  for (field in c("source_description", "source_datasets_text", "process_description",
                  "processing_tools", "processing_scripts", "output_description", "lineage_notes")) {
    if (is.null(data[[field]])) {
      data[[field]] <- if (nrow(saved)) saved[[field]][1] else NA_character_
    }
  }
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dbExecute(conn, "
    INSERT INTO dataset_lineage_tbl (
      dataset_id,
      lineage_source_mode,
      lineage_statement,
      source_description,
      source_datasets_text,
      process_description,
      processing_tools,
      processing_scripts,
      output_description,
      lineage_notes,
      lineage_status,
      updated_at,
      updated_by_person_id
    )
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(dataset_id) DO UPDATE SET
      lineage_source_mode = excluded.lineage_source_mode,
      lineage_statement = excluded.lineage_statement,
      source_description = excluded.source_description,
      source_datasets_text = excluded.source_datasets_text,
      process_description = excluded.process_description,
      processing_tools = excluded.processing_tools,
      processing_scripts = excluded.processing_scripts,
      output_description = excluded.output_description,
      lineage_notes = excluded.lineage_notes,
      lineage_status = excluded.lineage_status,
      updated_at = excluded.updated_at,
      updated_by_person_id = excluded.updated_by_person_id
  ", params = list(
    dataset_id,
    data$lineage_source_mode %||% "form",
    data$lineage_statement,
    data$source_description,
    data$source_datasets_text,
    data$process_description,
    data$processing_tools,
    data$processing_scripts,
    data$output_description,
    data$lineage_notes,
    data$lineage_status,
    now,
    user_person_id
  ))
  invisible(TRUE)
}

normalise_lineage_optional_integer <- function(x) {
  value <- suppressWarnings(as.integer(x[1]))
  if (length(value) == 0L || is.na(value)) NA_integer_ else value
}

normalise_lineage_optional_number <- function(x) {
  value <- suppressWarnings(as.numeric(x[1]))
  if (length(value) == 0L || is.na(value)) NA_real_ else value
}

normalise_lineage_optional_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(NA_character_)
  value <- trimws(as.character(x[1]))
  if (!nzchar(value)) NA_character_ else value
}

validate_lineage_parent <- function(conn, dataset_id) {
  found <- DBI::dbGetQuery(
    conn,
    "SELECT id FROM entry_tbl WHERE id = ? LIMIT 1",
    params = list(as.integer(dataset_id))
  )
  if (nrow(found) != 1L) stop("Dataset record not found.", call. = FALSE)
  invisible(TRUE)
}

validate_lineage_reference <- function(conn, table, id, dataset_id = NULL) {
  id <- normalise_lineage_optional_integer(id)
  if (is.na(id)) return(NA_integer_)
  sql <- paste0("SELECT id FROM ", table, " WHERE id = ?")
  params <- list(id)
  if (!is.null(dataset_id)) {
    sql <- paste0(sql, " AND dataset_id = ?")
    params <- c(params, list(as.integer(dataset_id)))
  }
  found <- DBI::dbGetQuery(conn, sql, params = params)
  if (nrow(found) != 1L) {
    stop("A lineage reference does not belong to the selected dataset.", call. = FALSE)
  }
  id
}

get_dataset_lineage_sources <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) {
    return(data.frame())
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  DBI::dbGetQuery(conn, "
    SELECT
      s.*,
      d.document_rel_path AS supporting_document_path,
      TRIM(COALESCE(p.name_first, '') || ' ' || COALESCE(p.name_last, ''))
        AS responsible_person_name,
      COALESCE(o.organisation_name_short, o.organisation_name_full)
        AS responsible_organisation_name
    FROM dataset_lineage_source_tbl s
    LEFT JOIN supporting_document_tbl d
      ON d.id = s.supporting_document_id AND d.active = 1
    LEFT JOIN persons_tbl p ON p.id = s.responsible_person_id
    LEFT JOIN organisation_tbl o ON o.id = s.responsible_organisation_id
    WHERE s.dataset_id = ?
    ORDER BY s.source_order, s.id
  ", params = list(as.integer(dataset_id[1])))
}

save_dataset_lineage_source <- function(
  dataset_id, data, base_path, user_person_id = NA_integer_, source_id = NULL
) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  validate_lineage_parent(conn, dataset_id)
  source_id <- normalise_lineage_optional_integer(source_id)
  supporting_document_id <- normalise_lineage_optional_integer(
    data$supporting_document_id
  )
  if (!is.na(supporting_document_id)) {
    document <- DBI::dbGetQuery(
      conn,
      paste(
        "SELECT id FROM supporting_document_tbl",
        "WHERE id = ? AND dataset_id = ? AND active = 1"
      ),
      params = list(supporting_document_id, as.integer(dataset_id))
    )
    if (nrow(document) != 1L) {
      stop(
        "The authoritative lineage document must be active and belong to the selected dataset.",
        call. = FALSE
      )
    }
  }
  origin <- lineage_origin_for_save(conn, "dataset_lineage_source_tbl", dataset_id,
    source_id, data$value_origin_code,
    if (is.na(supporting_document_id)) "user" else "supporting_document")
  scale_text <- normalise_lineage_optional_text(data$equivalent_scale_denominator)
  scale_number <- normalise_lineage_optional_number(data$equivalent_scale_denominator)
  if (!is.na(scale_text) && is.na(scale_number)) {
    stop("Equivalent scale denominator must be a positive integer.", call. = FALSE)
  }
  if (!is.na(scale_number) && (
    scale_number <= 0 || scale_number != floor(scale_number) ||
      scale_number > .Machine$integer.max
  )) {
    stop("Equivalent scale denominator must be a positive integer.", call. = FALSE)
  }
  scale <- if (is.na(scale_number)) NA_integer_ else as.integer(scale_number)
  resolution_text <- normalise_lineage_optional_text(data$spatial_resolution_value)
  resolution <- normalise_lineage_optional_number(data$spatial_resolution_value)
  if (!is.na(resolution_text) && is.na(resolution)) {
    stop("Spatial resolution must be numeric.", call. = FALSE)
  }
  if (!is.na(resolution) && resolution <= 0) {
    stop("Spatial resolution must be positive.", call. = FALSE)
  }
  role <- normalise_lineage_optional_text(data$role_code)
  if (!is.na(role) && !role %in% lineage_responsible_role_codes()) {
    stop("Unknown ISO responsible-party role.", call. = FALSE)
  }
  citation_date_type <- normalise_lineage_optional_text(data$citation_date_type_code)
  if (!is.na(citation_date_type) &&
      !citation_date_type %in% c("creation", "publication", "revision")) {
    stop("Unknown ISO citation-date type.", call. = FALSE)
  }
  title <- normalise_lineage_optional_text(data$title)
  description <- normalise_lineage_optional_text(data$description)
  if (is.na(title) && is.na(description) && is.na(supporting_document_id)) {
    stop("Provide a source title, description, or authoritative document.", call. = FALSE)
  }
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  user_person_id <- normalise_lineage_optional_integer(user_person_id)

  DBI::dbWithTransaction(conn, {
    if (is.na(source_id)) {
      next_order <- DBI::dbGetQuery(conn, paste(
        "SELECT COALESCE(MAX(source_order), 0) + 1 AS n",
        "FROM dataset_lineage_source_tbl WHERE dataset_id = ?"
      ), params = list(as.integer(dataset_id)))$n[1]
      DBI::dbExecute(conn, "
        INSERT INTO dataset_lineage_source_tbl (
          dataset_id, source_order, title, identifier, identifier_code_space,
          description, source_uri, citation_date, citation_date_type_code,
          temporal_start, temporal_end, equivalent_scale_denominator,
          spatial_resolution_value, spatial_resolution_unit,
          responsible_person_id, responsible_organisation_id, role_code,
          supporting_document_id, value_origin_code, is_provisional,
          created_at, created_by_person_id, updated_at, updated_by_person_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, ?, ?, ?, ?)
      ", params = list(
        as.integer(dataset_id), as.integer(next_order), title,
        normalise_lineage_optional_text(data$identifier),
        normalise_lineage_optional_text(data$identifier_code_space), description,
        normalise_lineage_optional_text(data$source_uri),
        normalise_lineage_optional_text(data$citation_date),
        citation_date_type,
        normalise_lineage_optional_text(data$temporal_start),
        normalise_lineage_optional_text(data$temporal_end), scale, resolution,
        normalise_lineage_optional_text(data$spatial_resolution_unit),
        normalise_lineage_optional_integer(data$responsible_person_id),
        normalise_lineage_optional_integer(data$responsible_organisation_id),
        role, supporting_document_id, origin, now, user_person_id, now, user_person_id
      ))
      source_id <- as.integer(DBI::dbGetQuery(
        conn, "SELECT last_insert_rowid() AS id"
      )$id[1])
    } else {
      validate_lineage_reference(
        conn, "dataset_lineage_source_tbl", source_id, dataset_id
      )
      DBI::dbExecute(conn, "
        UPDATE dataset_lineage_source_tbl SET
          title = ?, identifier = ?, identifier_code_space = ?, description = ?,
          source_uri = ?, citation_date = ?, citation_date_type_code = ?,
          temporal_start = ?, temporal_end = ?, equivalent_scale_denominator = ?,
          spatial_resolution_value = ?, spatial_resolution_unit = ?,
          responsible_person_id = ?, responsible_organisation_id = ?, role_code = ?,
          supporting_document_id = ?, value_origin_code = ?, is_provisional = 0,
          updated_at = ?, updated_by_person_id = ?
        WHERE id = ? AND dataset_id = ?
      ", params = list(
        title, normalise_lineage_optional_text(data$identifier),
        normalise_lineage_optional_text(data$identifier_code_space), description,
        normalise_lineage_optional_text(data$source_uri),
        normalise_lineage_optional_text(data$citation_date),
        citation_date_type,
        normalise_lineage_optional_text(data$temporal_start),
        normalise_lineage_optional_text(data$temporal_end), scale, resolution,
        normalise_lineage_optional_text(data$spatial_resolution_unit),
        normalise_lineage_optional_integer(data$responsible_person_id),
        normalise_lineage_optional_integer(data$responsible_organisation_id),
        role, supporting_document_id, origin, now, user_person_id,
        source_id, as.integer(dataset_id)
      ))
    }
  })
  source_id
}

get_dataset_lineage_process_steps <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0L || is.na(dataset_id[1])) {
    return(data.frame())
  }
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  DBI::dbGetQuery(conn, "
    SELECT
      p.*,
      TRIM(COALESCE(person.name_first, '') || ' ' || COALESCE(person.name_last, ''))
        AS processor_person_name,
      COALESCE(o.organisation_name_short, o.organisation_name_full)
        AS processor_organisation_name,
      GROUP_CONCAT(ss.source_id, ',') AS source_ids
    FROM dataset_lineage_process_step_tbl p
    LEFT JOIN persons_tbl person ON person.id = p.processor_person_id
    LEFT JOIN organisation_tbl o ON o.id = p.processor_organisation_id
    LEFT JOIN dataset_lineage_step_source_tbl ss ON ss.process_step_id = p.id
    WHERE p.dataset_id = ?
    GROUP BY p.id
    ORDER BY p.step_order, p.id
  ", params = list(as.integer(dataset_id[1])))
}

save_dataset_lineage_process_step <- function(
  dataset_id, data, base_path, user_person_id = NA_integer_, process_step_id = NULL
) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  validate_lineage_parent(conn, dataset_id)
  process_step_id <- normalise_lineage_optional_integer(process_step_id)
  description <- normalise_lineage_optional_text(data$description)
  if (is.na(description)) stop("A process-step description is required.", call. = FALSE)
  origin <- lineage_origin_for_save(conn, "dataset_lineage_process_step_tbl", dataset_id,
    process_step_id, data$value_origin_code, "user")
  role <- normalise_lineage_optional_text(data$role_code)
  if (!is.na(role) && !role %in% lineage_responsible_role_codes()) {
    stop("Unknown ISO responsible-party role.", call. = FALSE)
  }
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  user_person_id <- normalise_lineage_optional_integer(user_person_id)

  DBI::dbWithTransaction(conn, {
    if (is.na(process_step_id)) {
      next_order <- DBI::dbGetQuery(conn, paste(
        "SELECT COALESCE(MAX(step_order), 0) + 1 AS n",
        "FROM dataset_lineage_process_step_tbl WHERE dataset_id = ?"
      ), params = list(as.integer(dataset_id)))$n[1]
      DBI::dbExecute(conn, "
        INSERT INTO dataset_lineage_process_step_tbl (
          dataset_id, step_order, description, rationale, step_datetime,
          processor_person_id, processor_organisation_id, role_code,
          tool_software_reference, script_reference, output_description,
          value_origin_code, is_provisional, created_at, created_by_person_id,
          updated_at, updated_by_person_id
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, ?, ?, ?, ?)
      ", params = list(
        as.integer(dataset_id), as.integer(next_order), description,
        normalise_lineage_optional_text(data$rationale),
        normalise_lineage_optional_text(data$step_datetime),
        normalise_lineage_optional_integer(data$processor_person_id),
        normalise_lineage_optional_integer(data$processor_organisation_id), role,
        normalise_lineage_optional_text(data$tool_software_reference),
        normalise_lineage_optional_text(data$script_reference),
        normalise_lineage_optional_text(data$output_description), origin,
        now, user_person_id, now, user_person_id
      ))
      process_step_id <- as.integer(DBI::dbGetQuery(
        conn, "SELECT last_insert_rowid() AS id"
      )$id[1])
    } else {
      validate_lineage_reference(
        conn, "dataset_lineage_process_step_tbl", process_step_id, dataset_id
      )
      DBI::dbExecute(conn, "
        UPDATE dataset_lineage_process_step_tbl SET
          description = ?, rationale = ?, step_datetime = ?, processor_person_id = ?,
          processor_organisation_id = ?, role_code = ?, tool_software_reference = ?,
          script_reference = ?, output_description = ?, value_origin_code = ?,
          is_provisional = 0, updated_at = ?, updated_by_person_id = ?
        WHERE id = ? AND dataset_id = ?
      ", params = list(
        description, normalise_lineage_optional_text(data$rationale),
        normalise_lineage_optional_text(data$step_datetime),
        normalise_lineage_optional_integer(data$processor_person_id),
        normalise_lineage_optional_integer(data$processor_organisation_id), role,
        normalise_lineage_optional_text(data$tool_software_reference),
        normalise_lineage_optional_text(data$script_reference),
        normalise_lineage_optional_text(data$output_description), origin,
        now, user_person_id, process_step_id, as.integer(dataset_id)
      ))
    }
  })
  process_step_id
}

set_dataset_lineage_step_sources <- function(
  dataset_id, process_step_id, source_ids, base_path, user_person_id = NA_integer_
) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  process_step_id <- validate_lineage_reference(
    conn, "dataset_lineage_process_step_tbl", process_step_id, dataset_id
  )
  source_ids <- unique(suppressWarnings(as.integer(source_ids)))
  source_ids <- source_ids[!is.na(source_ids)]
  if (length(source_ids) > 0L) {
    for (source_id in source_ids) {
      validate_lineage_reference(
        conn, "dataset_lineage_source_tbl", source_id, dataset_id
      )
    }
  }
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn, "DELETE FROM dataset_lineage_step_source_tbl WHERE process_step_id = ?",
      params = list(process_step_id)
    )
    for (source_id in source_ids) {
      DBI::dbExecute(conn, "
        INSERT INTO dataset_lineage_step_source_tbl (
          dataset_id, process_step_id, source_id, created_by_person_id
        ) VALUES (?, ?, ?, ?)
      ", params = list(
        as.integer(dataset_id), process_step_id, source_id,
        normalise_lineage_optional_integer(user_person_id)
      ))
    }
  })
  invisible(TRUE)
}

delete_dataset_lineage_source <- function(dataset_id, source_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  validate_lineage_reference(conn, "dataset_lineage_source_tbl", source_id, dataset_id)
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn, "DELETE FROM dataset_lineage_step_source_tbl WHERE source_id = ?",
      params = list(as.integer(source_id))
    )
    DBI::dbExecute(
      conn, "DELETE FROM dataset_lineage_source_tbl WHERE id = ? AND dataset_id = ?",
      params = list(as.integer(source_id), as.integer(dataset_id))
    )
    rows <- DBI::dbGetQuery(conn, paste(
      "SELECT id FROM dataset_lineage_source_tbl WHERE dataset_id = ?",
      "ORDER BY source_order, id"
    ), params = list(as.integer(dataset_id)))
    if (nrow(rows) > 0L) {
      DBI::dbExecute(conn, paste(
        "UPDATE dataset_lineage_source_tbl SET source_order = -source_order",
        "WHERE dataset_id = ?"
      ), params = list(as.integer(dataset_id)))
      for (i in seq_len(nrow(rows))) DBI::dbExecute(
        conn, "UPDATE dataset_lineage_source_tbl SET source_order = ? WHERE id = ?",
        params = list(i, rows$id[i])
      )
    }
  })
  invisible(TRUE)
}

delete_dataset_lineage_process_step <- function(dataset_id, process_step_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  validate_lineage_reference(
    conn, "dataset_lineage_process_step_tbl", process_step_id, dataset_id
  )
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn, "DELETE FROM dataset_lineage_step_source_tbl WHERE process_step_id = ?",
      params = list(as.integer(process_step_id))
    )
    DBI::dbExecute(conn, paste(
      "DELETE FROM dataset_lineage_process_step_tbl",
      "WHERE id = ? AND dataset_id = ?"
    ), params = list(as.integer(process_step_id), as.integer(dataset_id)))
    rows <- DBI::dbGetQuery(conn, paste(
      "SELECT id FROM dataset_lineage_process_step_tbl WHERE dataset_id = ?",
      "ORDER BY step_order, id"
    ), params = list(as.integer(dataset_id)))
    if (nrow(rows) > 0L) {
      DBI::dbExecute(conn, paste(
        "UPDATE dataset_lineage_process_step_tbl SET step_order = -step_order",
        "WHERE dataset_id = ?"
      ), params = list(as.integer(dataset_id)))
      for (i in seq_len(nrow(rows))) DBI::dbExecute(
        conn, "UPDATE dataset_lineage_process_step_tbl SET step_order = ? WHERE id = ?",
        params = list(i, rows$id[i])
      )
    }
  })
  invisible(TRUE)
}

move_dataset_lineage_record <- function(dataset_id, record_id, direction, type, base_path) {
  direction <- match.arg(direction, c("up", "down"))
  type <- match.arg(type, c("source", "process_step"))
  table <- if (type == "source") "dataset_lineage_source_tbl" else "dataset_lineage_process_step_tbl"
  order_field <- if (type == "source") "source_order" else "step_order"
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_dataset_lineage_detail_tables(conn)
  rows <- DBI::dbGetQuery(conn, paste(
    "SELECT id FROM", table, "WHERE dataset_id = ? ORDER BY", order_field, ", id"
  ), params = list(as.integer(dataset_id)))
  position <- match(as.integer(record_id), rows$id)
  target <- position + if (direction == "up") -1L else 1L
  if (is.na(position) || target < 1L || target > nrow(rows)) return(invisible(FALSE))
  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(conn, paste("UPDATE", table, "SET", order_field, "= -", order_field,
      "WHERE dataset_id = ?"), params = list(as.integer(dataset_id)))
    reordered <- rows$id
    reordered[c(position, target)] <- reordered[c(target, position)]
    for (i in seq_along(reordered)) DBI::dbExecute(
      conn, paste("UPDATE", table, "SET", order_field, "= ? WHERE id = ?"),
      params = list(i, reordered[i])
    )
  })
  invisible(TRUE)
}

get_lineage_supporting_documents <- function(dataset_id, base_path) {
  if (is.null(dataset_id) ||
      length(dataset_id) == 0 ||
      is.na(dataset_id)) {
    return(data.frame())
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, "
    SELECT
      id,
      document_rel_path,
      document_extension,
      document_type_label,
      document_type_auto_text,
      notes
    FROM supporting_document_tbl
    WHERE dataset_id = ?
      AND active = 1
      AND include_in_lineage = 1
    ORDER BY document_rel_path
  ", params = list(dataset_id))
}

set_lineage_supporting_documents <- function(
  dataset_id,
  document_ids,
  base_path,
  user_person_id = NA_integer_
) {
  if (is.null(dataset_id) ||
      length(dataset_id) == 0L ||
      is.na(dataset_id[1])) {
    stop("dataset_id is required.")
  }

  document_ids <- unique(as.integer(document_ids))
  document_ids <- document_ids[!is.na(document_ids)]

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (length(document_ids) > 0L) {
    placeholders <- paste(rep("?", length(document_ids)), collapse = ", ")
    matching <- DBI::dbGetQuery(
      conn,
      paste0(
        "SELECT id FROM supporting_document_tbl ",
        "WHERE dataset_id = ? AND active = 1 AND id IN (",
        placeholders,
        ")"
      ),
      params = c(list(as.integer(dataset_id[1])), as.list(document_ids))
    )$id

    if (length(matching) != length(document_ids)) {
      stop(
        paste(
          "Every lineage document must be an active supporting document",
          "belonging to the selected dataset."
        )
      )
    }
  }

  DBI::dbWithTransaction(conn, {
    DBI::dbExecute(
      conn,
      "
      UPDATE supporting_document_tbl
      SET include_in_lineage = 0
      WHERE dataset_id = ?
        AND active = 1
      ",
      params = list(as.integer(dataset_id[1]))
    )

    if (length(document_ids) > 0L) {
      placeholders <- paste(rep("?", length(document_ids)), collapse = ", ")
      DBI::dbExecute(
        conn,
        paste0(
          "UPDATE supporting_document_tbl ",
          "SET include_in_lineage = 1 ",
          "WHERE dataset_id = ? AND active = 1 AND id IN (",
          placeholders,
          ")"
        ),
        params = c(list(as.integer(dataset_id[1])), as.list(document_ids))
      )
    }
  })

  invisible(TRUE)
}

ensure_data_pond_tables <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS data_pond_tbl (
      data_pond_id TEXT PRIMARY KEY,
      data_pond_path TEXT,
      data_pond_label TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
      created_by_user_id INTEGER,
      active INTEGER DEFAULT 1
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS app_config_tbl (
      id INTEGER PRIMARY KEY,
      data_pond_id TEXT NOT NULL,
      user_id INTEGER,
      config_scope TEXT NOT NULL DEFAULT 'pond',
      config_key TEXT NOT NULL,
      config_value TEXT,
      config_type TEXT DEFAULT 'text',
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (data_pond_id) REFERENCES data_pond_tbl(data_pond_id)
    )
  ")

  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_app_config_unique
    ON app_config_tbl(data_pond_id, user_id, config_scope, config_key)
  ")

  ensure_columns(conn, "entry_tbl", c("data_pond_id"), c(data_pond_id = "TEXT"))
  invisible(TRUE)
}

ensure_dataset_attribute_table <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS dataset_attribute_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      data_file_id INTEGER,
      data_pond_id TEXT,
      data_file_rel_path_full TEXT,
      data_file_name TEXT,
      layer_name TEXT,
      data_field_alias TEXT,
      data_field_name TEXT NOT NULL,
      data_field_desc TEXT,
      data_field_units TEXT,
      legend_field INTEGER DEFAULT 0,
      legend_field_notes TEXT,
      attribute_type TEXT,
      attribute_order INTEGER,
      example_values TEXT,
      value_count INTEGER,
      missing_count INTEGER,
      unique_count INTEGER,
      min_value TEXT,
      max_value TEXT,
      profile_updated_at TEXT,
      updated_at TEXT,
      updated_by_user_id INTEGER,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (data_file_id) REFERENCES data_file_tbl(id)
    )
  ")

  DBI::dbExecute(conn, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_dataset_attribute_unique
    ON dataset_attribute_tbl(dataset_id, data_file_rel_path_full, COALESCE(layer_name, ''), data_field_name)
  ")
  ensure_columns(
    conn,
    "dataset_attribute_tbl",
    fields = c("data_field_units"),
    types = c(data_field_units = "TEXT")
  )
  existing <- DBI::dbGetQuery(conn, "PRAGMA table_info(dataset_attribute_tbl)")
  if ("data_field_untis" %in% existing$name && "data_field_units" %in% existing$name) {
    DBI::dbExecute(conn, "
      UPDATE dataset_attribute_tbl
      SET data_field_units = COALESCE(data_field_units, data_field_untis)
      WHERE data_field_untis IS NOT NULL
    ")
  }
  invisible(TRUE)
}

ensure_data_profile_run_table <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS data_profile_run_tbl (
      id INTEGER PRIMARY KEY,
      dataset_id INTEGER NOT NULL,
      data_file_id INTEGER,
      data_pond_id TEXT,
      profile_type TEXT NOT NULL DEFAULT 'attribute_table',
      profile_status TEXT DEFAULT 'pending',
      profile_message TEXT,
      profiled_by_user_id INTEGER,
      started_at TEXT DEFAULT CURRENT_TIMESTAMP,
      completed_at TEXT,
      FOREIGN KEY (dataset_id) REFERENCES entry_tbl(id),
      FOREIGN KEY (data_file_id) REFERENCES data_file_tbl(id)
    )
  ")
  invisible(TRUE)
}

# dataset relationship loading helpers ----

get_dataset_author_links <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(data.frame())
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbGetQuery(conn, "
    SELECT
      a.dataset_id,
      a.person_id,
      a.author_order,
      a.role,
      p.name_first,
      p.name_last,
      p.name_first || ' ' || p.name_last AS name
    FROM authority_tbl a
    LEFT JOIN persons_tbl p ON a.person_id = p.id
    WHERE a.dataset_id = ?
    ORDER BY a.author_order, a.id
  ", params = list(dataset_id))
}

get_dataset_poc_links <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(data.frame())
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_columns(
    conn,
    "poc_tbl",
    fields = "organisation_id",
    types = c(organisation_id = "INTEGER")
  )

  DBI::dbGetQuery(conn, "
    SELECT
      poc.dataset_id,
      poc.person_id,
      poc.organisation_id,
      poc.role,
      p.name_first,
      p.name_last,
      p.name_first || ' ' || p.name_last AS name,
      o.organisation_name_full,
      o.organisation_name_short,
      o.organisation_code,
      o.organisation_department,
      o.organisation_address
    FROM poc_tbl poc
    LEFT JOIN persons_tbl p ON poc.person_id = p.id
    LEFT JOIN organisation_tbl o ON poc.organisation_id = o.id
    WHERE poc.dataset_id = ?
    ORDER BY poc.id
  ", params = list(dataset_id))
}

get_dataset_org_links <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(data.frame())
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  DBI::dbGetQuery(conn, "
    SELECT
      dorg.dataset_id,
      dorg.organisation_id,
      dorg.role,
      o.organisation_name_full,
      o.organisation_name_short,
      o.organisation_code,
      o.organisation_department,
      o.organisation_address
    FROM dataset_organisation_tbl dorg
    LEFT JOIN organisation_tbl o ON dorg.organisation_id = o.id
    WHERE dorg.dataset_id = ?
    ORDER BY dorg.id
  ", params = list(dataset_id))
}

get_active_user_person_id <- function(active_user_id, base_path) {
  if (is.null(active_user_id) ||
      length(active_user_id) == 0 ||
      is.na(active_user_id)) {
    return(NA_integer_)
  }

  users <- get_users(base_path)

  if (is.null(users) || nrow(users) == 0) {
    return(NA_integer_)
  }

  user <- users[
    as.character(users$id) == as.character(active_user_id),
    ,
    drop = FALSE
  ]

  if (nrow(user) == 0) {
    return(NA_integer_)
  }

  as.integer(user$person_id[1])
}

repair_dataset_license_template_links <- function(conn) {
  if (!DBI::dbExistsTable(conn, "dataset_license_tbl") ||
      !DBI::dbExistsTable(conn, "license_template_tbl")) {
    return(invisible(FALSE))
  }
  DBI::dbExecute(conn, "
    UPDATE dataset_license_tbl
    SET license_template_id = (
      SELECT lt.id
      FROM license_template_tbl lt
      WHERE lt.license_identifier = dataset_license_tbl.license_identifier
      LIMIT 1
    )
    WHERE license_identifier IS NOT NULL
      AND license_identifier != ''
      AND (
        license_template_id IS NULL
        OR license_template_id NOT IN (
          SELECT id FROM license_template_tbl
        )
      )
  ")
  invisible(TRUE)
}

# dataset lifecycle / folder lock ----
ensure_dataset_lifecycle_columns <- function(conn) {
  if (!DBI::dbExistsTable(conn, "entry_tbl")) {
    stop(
      "entry_tbl does not exist. Run base database initialisation first."
    )
  }

  ensure_columns(
    conn,
    "entry_tbl",
    fields = c(
      "uuid",
      "citation_key",
      "folder_key",
      "dataset_path",
      "data_path",
      "data_pond_id",
      "lifecycle_status",
      "folder_locked",
      "source_dataset_id",
      "released_at",
      "released_by_person_id",
      "archived_at",
      "archived_by_person_id"
    ),
    types = c(
      uuid = "TEXT",
      citation_key = "TEXT",
      folder_key = "TEXT",
      dataset_path = "TEXT",
      data_path = "TEXT",
      data_pond_id = "TEXT",
      lifecycle_status = "TEXT DEFAULT 'draft'",
      folder_locked = "INTEGER DEFAULT 0",
      source_dataset_id = "INTEGER",
      released_at = "TEXT",
      released_by_person_id = "INTEGER",
      archived_at = "TEXT",
      archived_by_person_id = "INTEGER"
    )
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE entry_tbl
    SET lifecycle_status = 'draft'
    WHERE lifecycle_status IS NULL
        OR TRIM(lifecycle_status) = ''
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE entry_tbl
    SET folder_locked = 0
    WHERE folder_locked IS NULL
    "
  )

  invisible(TRUE)
}

get_dataset_lifecycle <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(NULL)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_dataset_lifecycle_columns(conn)

  res <- DBI::dbGetQuery(conn, "
    SELECT
      id,
      uuid,
      title,
      citation_key,
      folder_key,
      version,
      dataset_path,
      data_path,
      lifecycle_status,
      folder_locked,
      source_dataset_id,
      released_at,
      released_by_person_id,
      archived_at,
      archived_by_person_id
    FROM entry_tbl
    WHERE id = ?
  ", params = list(dataset_id))

  if (nrow(res) == 0) return(NULL)

  res[1, , drop = FALSE]
}

set_dataset_lifecycle <- function(
  dataset_id,
  base_path,
  lifecycle_status,
  folder_locked = NULL,
  user_person_id = NA_integer_
) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    stop("dataset_id is required.")
  }

  if (!lifecycle_status %in% c("draft", "released", "archived")) {
    stop("Invalid lifecycle_status: ", lifecycle_status)
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_dataset_lifecycle_columns(conn)

  current <- release_entry(conn, dataset_id)
  if (metadata_present(current$dataset_path) && startsWith(normalizePath(current$dataset_path[1], winslash = "/", mustWork = FALSE),
                 paste0(normalizePath(release_store(base_path), winslash = "/", mustWork = FALSE), "/")))
    stop("Use Restore as draft for a frozen archive; its lifecycle cannot be changed in place.")

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (is.null(folder_locked)) {
    folder_locked <- if (lifecycle_status %in% c("released", "archived")) 1L else 0L
  }

  if (lifecycle_status == "released") {
    DBI::dbExecute(conn, "
      UPDATE entry_tbl
      SET lifecycle_status = ?,
          folder_locked = ?,
          released_at = ?,
          released_by_person_id = ?
      WHERE id = ?
    ", params = list(
      lifecycle_status,
      as.integer(folder_locked),
      now,
      user_person_id,
      dataset_id
    ))
  } else if (lifecycle_status == "archived") {
    DBI::dbExecute(conn, "
      UPDATE entry_tbl
      SET lifecycle_status = ?,
          folder_locked = ?,
          archived_at = ?,
          archived_by_person_id = ?
      WHERE id = ?
    ", params = list(
      lifecycle_status,
      as.integer(folder_locked),
      now,
      user_person_id,
      dataset_id
    ))
  } else {
    DBI::dbExecute(conn, "
      UPDATE entry_tbl
      SET lifecycle_status = ?,
          folder_locked = ?
      WHERE id = ?
    ", params = list(
      lifecycle_status,
      as.integer(folder_locked),
      dataset_id
    ))
  }

  invisible(TRUE)
}

update_dataset_folder_paths <- function(
  dataset_id,
  base_path,
  dataset_path,
  data_path = file.path(dataset_path, "data")
) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    stop("dataset_id is required.")
  }

  dataset_path <- normalizePath(dataset_path, mustWork = FALSE, winslash = "/")
  data_path <- normalizePath(data_path, mustWork = FALSE, winslash = "/")

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_columns(
    conn,
    "entry_tbl",
    fields = c("dataset_path", "data_path"),
    types = c(dataset_path = "TEXT", data_path = "TEXT")
  )

  DBI::dbExecute(conn, "
    UPDATE entry_tbl
    SET dataset_path = ?,
        data_path = ?
    WHERE id = ?
  ", params = list(
    dataset_path,
    data_path,
    dataset_id
  ))

  invisible(TRUE)
}

repair_cached_absolute_paths_after_dataset_move <- function(
  dataset_id,
  base_path,
  dataset_path,
  data_path = file.path(dataset_path, "data")
) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(invisible(FALSE))
  }
  dataset_path <- normalizePath(dataset_path, mustWork = FALSE, winslash = "/")
  data_path <- normalizePath(data_path, mustWork = FALSE, winslash = "/")
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (DBI::dbExistsTable(conn, "data_file_tbl")) {
    DBI::dbExecute(conn, "
      UPDATE data_file_tbl
      SET file_path = ? || '/' || data_file_rel_path_full
      WHERE dataset_id = ?
        AND data_file_rel_path_full IS NOT NULL
        AND data_file_rel_path_full != ''
    ", params = list(
      data_path,
      dataset_id
    ))
  }
  if (DBI::dbExistsTable(conn, "supporting_document_tbl")) {
    DBI::dbExecute(conn, "
      UPDATE supporting_document_tbl
      SET document_path = ? || '/' || document_rel_path
      WHERE dataset_id = ?
        AND document_rel_path IS NOT NULL
        AND document_rel_path != ''
    ", params = list(
      dataset_path,
      dataset_id
    ))
  }
  invisible(TRUE)
}

backfill_missing_citation_keys <- function(base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_dataset_citation_key_column(conn)

  datasets <- DBI::dbGetQuery(conn, "
    SELECT *
    FROM entry_tbl
    WHERE citation_key IS NULL
      OR citation_key = ''
  ")

  if (nrow(datasets) == 0) {
    return(invisible(TRUE))
  }

  for (i in seq_len(nrow(datasets))) {
    rec <- datasets[i, , drop = FALSE]
    vals <- as.list(rec)

    org_df <- get_dataset_org_links(rec$id[1], base_path)

    existing_key <- extract_citation_key_from_folder_name(
      rec$dataset_path[1] %||% NULL
    )

    citation_key <- generate_citation_key(
      vals,
      org_df,
      existing_key = existing_key
    )

    DBI::dbExecute(conn, "
      UPDATE entry_tbl
      SET citation_key = ?
      WHERE id = ?
    ", params = list(
      citation_key,
      rec$id[1]
    ))
  }

  invisible(TRUE)
}

backfill_missing_folder_keys <- function(
  base_path
) {
  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_dataset_lifecycle_columns(
    conn
  )

  datasets <- DBI::dbGetQuery(
    conn,
    "
    SELECT *
    FROM entry_tbl
    WHERE folder_key IS NULL
        OR TRIM(folder_key) = ''
    "
  )

  if (nrow(datasets) == 0) {
    return(
      invisible(TRUE)
    )
  }

  for (i in seq_len(nrow(datasets))) {
    rec <- datasets[
      i,
      ,
      drop = FALSE
    ]

    dataset_id <- as.integer(
      rec$id[1]
    )

    org_df <- get_dataset_org_links(
      dataset_id = dataset_id,
      base_path = base_path
    )

    folder_key <- generate_folder_key(
      vals = as.list(rec),
      org_df = org_df
    )

    DBI::dbExecute(
      conn,
      "
      UPDATE entry_tbl
      SET folder_key = ?
      WHERE id = ?
      ",
      params = list(
        folder_key,
        dataset_id
      )
    )
  }

  invisible(TRUE)
}

dataset_uuid_exists <- function(
  uuid,
  base_path
) {
  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_dataset_lifecycle_columns(
    conn
  )

  result <- DBI::dbGetQuery(
    conn,
    "
    SELECT COUNT(*) AS n
    FROM entry_tbl
    WHERE uuid = ?
    ",
    params = list(
      uuid
    )
  )

  as.integer(
    result$n[1]
  ) > 0L
}

generate_dataset_uuid <- function() {
  date_part <- format(
    Sys.Date(),
    "%Y%m%d"
  )

  alphabet <- c(
    letters[
      !letters %in% c(
        "i",
        "l",
        "o"
      )
    ],
    as.character(
      2:9
    )
  )

  random_part <- paste0(
    sample(
      alphabet,
      size = 6L,
      replace = TRUE
    ),
    collapse = ""
  )

  paste(
    date_part,
    random_part,
    sep = "_"
  )
}

get_or_create_dataset_uuid <- function(dataset_id, base_path) {
  if (is.null(dataset_id) ||
      length(dataset_id) == 0 ||
      is.na(dataset_id)) {
    stop("dataset_id is required.")
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_columns(
    conn,
    "entry_tbl",
    fields = c("uuid"),
    types = c(uuid = "TEXT")
  )

  rec <- DBI::dbGetQuery(
    conn,
    "SELECT id, uuid FROM entry_tbl WHERE id = ?",
    params = list(dataset_id)
  )

  if (nrow(rec) == 0) {
    stop("Dataset record not found: ", dataset_id)
  }

  existing_uuid <- rec$uuid[1]

  if (!is.null(existing_uuid) &&
      length(existing_uuid) > 0 &&
      !is.na(existing_uuid) &&
      existing_uuid != "") {
    return(existing_uuid)
  }

  new_id <- generate_dataset_uuid()

  DBI::dbExecute(
    conn,
    "UPDATE entry_tbl SET uuid = ? WHERE id = ?",
    params = list(new_id, dataset_id)
  )

  new_id
}

backfill_missing_dataset_uuids <- function(base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  ensure_columns(
    conn,
    "entry_tbl",
    fields = c("uuid"),
    types = c(uuid = "TEXT")
  )

  rows <- DBI::dbGetQuery(conn, "
    SELECT id
    FROM entry_tbl
    WHERE uuid IS NULL
        OR uuid = ''
  ")

  if (nrow(rows) == 0) {
    return(invisible(TRUE))
  }

  for (i in seq_len(nrow(rows))) {
    new_id <- generate_dataset_uuid()

    DBI::dbExecute(
      conn,
      "UPDATE entry_tbl SET uuid = ? WHERE id = ?",
      params = list(new_id, rows$id[i])
    )
  }

  invisible(TRUE)
}

# spatial metadata ----
ensure_data_file_spatial_metadata_table <- function(conn) {
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE IF NOT EXISTS data_file_spatial_metadata_tbl (
      id INTEGER PRIMARY KEY,

      dataset_id INTEGER NOT NULL,
      data_file_id INTEGER,
      data_pond_id TEXT,

      data_file_rel_path_full TEXT NOT NULL,
      file_path TEXT,
      layer_name TEXT,
      layer_key TEXT NOT NULL DEFAULT '',

      spatial_status TEXT,
      spatial_type TEXT,

      crs_auth_code TEXT,
      crs_name TEXT,
      crs_input TEXT,
      crs_status TEXT,

      bbox_native TEXT,
      bbox_status TEXT,
      xmin REAL,
      ymin REAL,
      xmax REAL,
      ymax REAL,

      include_in_dataset_bbox INTEGER NOT NULL DEFAULT 0,
      bbox_inclusion_updated_at TEXT,
      bbox_inclusion_updated_by_user_id INTEGER,

      geometry_type TEXT,
      feature_count INTEGER,

      raster_nrow INTEGER,
      raster_ncol INTEGER,
      raster_nlyr INTEGER,
      raster_res_x REAL,
      raster_res_y REAL,

      spatial_note TEXT,

      extracted_at TEXT,
      extracted_by_user_id INTEGER,

      FOREIGN KEY (dataset_id)
        REFERENCES entry_tbl(id),

      FOREIGN KEY (data_file_id)
        REFERENCES data_file_tbl(id),

      FOREIGN KEY (extracted_by_user_id)
        REFERENCES user_tbl(id)
    )
    "
  )

  ensure_columns(
    conn = conn,
    table = "data_file_spatial_metadata_tbl",
    fields = c(
      "layer_key",
      "bbox_status",
      "include_in_dataset_bbox",
      "bbox_inclusion_updated_at",
      "bbox_inclusion_updated_by_user_id",
      "user_notes",
      "notes_updated_at",
      "notes_updated_by_user_id"
    ),
    types = c(
      layer_key = "TEXT NOT NULL DEFAULT ''",
      bbox_status = "TEXT",
      include_in_dataset_bbox =
        "INTEGER NOT NULL DEFAULT 0",
      bbox_inclusion_updated_at = "TEXT",
      bbox_inclusion_updated_by_user_id = "INTEGER",
      user_notes = "TEXT",
      notes_updated_at = "TEXT",
      notes_updated_by_user_id = "INTEGER"
    )
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE data_file_spatial_metadata_tbl
    SET include_in_dataset_bbox = 0
    WHERE include_in_dataset_bbox IS NULL
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE data_file_spatial_metadata_tbl
    SET layer_key = COALESCE(
      NULLIF(TRIM(layer_name), ''),
      ''
    )
    WHERE layer_key IS NULL
      OR TRIM(layer_key) = ''
    "
  )

  DBI::dbExecute(
    conn,
    "
    DROP INDEX IF EXISTS
      idx_spatial_metadata_file_layer
    "
  )

  DBI::dbExecute(
    conn,
    "
    CREATE UNIQUE INDEX IF NOT EXISTS
      idx_spatial_metadata_file_layer
    ON data_file_spatial_metadata_tbl (
      dataset_id,
      data_file_rel_path_full,
      layer_key
    )
    "
  )

  invisible(TRUE)
}

ensure_dataset_manual_spatial_extent_table <- function(
  conn
) {
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE IF NOT EXISTS
      dataset_manual_spatial_extent_tbl (
        id INTEGER PRIMARY KEY,

        dataset_id INTEGER NOT NULL,
        data_file_id INTEGER,

        extent_scope TEXT NOT NULL DEFAULT 'dataset',
        extent_label TEXT NOT NULL,
        source_note TEXT,

        crs_auth_code TEXT,
        crs_name TEXT,
        crs_input TEXT NOT NULL,

        bbox_native TEXT NOT NULL,
        bbox_status TEXT NOT NULL,

        xmin REAL NOT NULL,
        ymin REAL NOT NULL,
        xmax REAL NOT NULL,
        ymax REAL NOT NULL,

        include_in_dataset_bbox INTEGER
          NOT NULL DEFAULT 1,

        active INTEGER NOT NULL DEFAULT 1,

        created_at TEXT,
        created_by_user_id INTEGER,
        updated_at TEXT,
        updated_by_user_id INTEGER,

        FOREIGN KEY (dataset_id)
          REFERENCES entry_tbl(id),

        FOREIGN KEY (data_file_id)
          REFERENCES data_file_tbl(id),

        FOREIGN KEY (created_by_user_id)
          REFERENCES user_tbl(id),

        FOREIGN KEY (updated_by_user_id)
          REFERENCES user_tbl(id)
      )
    "
  )

  ensure_columns(
    conn = conn,
    table = "dataset_manual_spatial_extent_tbl",
    fields = c(
      "data_file_id",
      "extent_scope",
      "extent_label",
      "source_note",
      "crs_auth_code",
      "crs_name",
      "crs_input",
      "bbox_native",
      "bbox_status",
      "xmin",
      "ymin",
      "xmax",
      "ymax",
      "include_in_dataset_bbox",
      "active",
      "created_at",
      "created_by_user_id",
      "updated_at",
      "updated_by_user_id"
    ),
    types = c(
      data_file_id = "INTEGER",
      extent_scope = "TEXT",
      extent_label = "TEXT",
      source_note = "TEXT",
      crs_auth_code = "TEXT",
      crs_name = "TEXT",
      crs_input = "TEXT",
      bbox_native = "TEXT",
      bbox_status = "TEXT",
      xmin = "REAL",
      ymin = "REAL",
      xmax = "REAL",
      ymax = "REAL",
      include_in_dataset_bbox =
        "INTEGER DEFAULT 1",
      active = "INTEGER DEFAULT 1",
      created_at = "TEXT",
      created_by_user_id = "INTEGER",
      updated_at = "TEXT",
      updated_by_user_id = "INTEGER"
    )
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_manual_spatial_extent_tbl
    SET
      extent_scope = COALESCE(
        NULLIF(TRIM(extent_scope), ''),
        CASE
          WHEN data_file_id IS NULL
          THEN 'dataset'
          ELSE 'data_file'
        END
      ),

      bbox_status = COALESCE(
        NULLIF(TRIM(bbox_status), ''),
        'valid'
      ),

      include_in_dataset_bbox =
        COALESCE(include_in_dataset_bbox, 1),

      active = COALESCE(active, 1)
    "
  )

  DBI::dbExecute(
    conn,
    "
    CREATE INDEX IF NOT EXISTS
      idx_manual_extent_dataset_active

    ON dataset_manual_spatial_extent_tbl (
      dataset_id,
      active,
      include_in_dataset_bbox
    )
    "
  )

  invisible(TRUE)
}

ensure_dataset_spatial_summary_table <- function(conn) {
  DBI::dbExecute(
    conn,
    "
    CREATE TABLE IF NOT EXISTS dataset_spatial_summary_tbl (
      id INTEGER PRIMARY KEY,

      dataset_id INTEGER NOT NULL UNIQUE,

      bbox_status TEXT NOT NULL DEFAULT 'not_derived',
      status_message TEXT,

      bbox_crs TEXT NOT NULL DEFAULT 'EPSG:4326',
      bbox_wgs84 TEXT,

      xmin REAL,
      ymin REAL,
      xmax REAL,
      ymax REAL,

      included_source_count INTEGER NOT NULL DEFAULT 0,
      eligible_source_count INTEGER NOT NULL DEFAULT 0,
      skipped_source_count INTEGER NOT NULL DEFAULT 0,

      contributor_keys TEXT,
      derivation_method TEXT,

      generated_at TEXT,
      generated_by_user_id INTEGER,

      FOREIGN KEY (dataset_id)
        REFERENCES entry_tbl(id),

      FOREIGN KEY (generated_by_user_id)
        REFERENCES user_tbl(id)
    )
    "
  )

  ensure_columns(
    conn = conn,
    table = "dataset_spatial_summary_tbl",
    fields = c(
      "bbox_status",
      "status_message",
      "bbox_crs",
      "bbox_wgs84",
      "xmin",
      "ymin",
      "xmax",
      "ymax",
      "included_source_count",
      "eligible_source_count",
      "skipped_source_count",
      "contributor_keys",
      "derivation_method",
      "generated_at",
      "generated_by_user_id"
    ),
    types = c(
      bbox_status = "TEXT NOT NULL DEFAULT 'not_derived'",
      status_message = "TEXT",
      bbox_crs = "TEXT NOT NULL DEFAULT 'EPSG:4326'",
      bbox_wgs84 = "TEXT",
      xmin = "REAL",
      ymin = "REAL",
      xmax = "REAL",
      ymax = "REAL",
      included_source_count = "INTEGER NOT NULL DEFAULT 0",
      eligible_source_count = "INTEGER NOT NULL DEFAULT 0",
      skipped_source_count = "INTEGER NOT NULL DEFAULT 0",
      contributor_keys = "TEXT",
      derivation_method = "TEXT",
      generated_at = "TEXT",
      generated_by_user_id = "INTEGER"
    )
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_spatial_summary_tbl
    SET
      bbox_status = COALESCE(
        NULLIF(TRIM(bbox_status), ''),
        'not_derived'
      ),
      bbox_crs = COALESCE(
        NULLIF(TRIM(bbox_crs), ''),
        'EPSG:4326'
      ),
      included_source_count =
        COALESCE(included_source_count, 0),
      eligible_source_count =
        COALESCE(eligible_source_count, 0),
      skipped_source_count =
        COALESCE(skipped_source_count, 0)
    "
  )

  invisible(TRUE)
}

get_included_spatial_bbox_records_conn <- function(
  conn,
  dataset_id
) {
  DBI::dbGetQuery(
    conn,
    "
    SELECT
      sm.*,

      df.file_name,
      df.data_file_format,

      'extracted_spatial_record'
        AS source_type,

      sm.id
        AS source_id,

      'spatial:' || sm.id
        AS source_key,

      COALESCE(
        NULLIF(TRIM(df.file_name), ''),
        sm.data_file_rel_path_full
      ) ||

      CASE
        WHEN sm.layer_name IS NOT NULL
          AND TRIM(sm.layer_name) != ''
        THEN ' | ' || sm.layer_name
        ELSE ''
      END
        AS source_label

    FROM data_file_spatial_metadata_tbl sm

    LEFT JOIN data_file_tbl df
      ON df.dataset_id = sm.dataset_id
      AND df.data_file_rel_path_full =
          sm.data_file_rel_path_full

    WHERE sm.dataset_id = ?

      AND COALESCE(
        sm.include_in_dataset_bbox,
        0
      ) = 1

    ORDER BY
      sm.data_file_rel_path_full,
      sm.layer_name,
      sm.id
    ",
    params = list(
      as.integer(dataset_id[1])
    )
  )
}

get_included_manual_bbox_records_conn <- function(
  conn,
  dataset_id
) {
  ensure_dataset_manual_spatial_extent_table(
    conn
  )

  DBI::dbGetQuery(
    conn,
    "
    SELECT
      me.*,

      df.data_file_rel_path_full,
      df.file_path,
      df.file_name,

      COALESCE(
        df.data_file_format,
        'manual_extent'
      ) AS data_file_format,

      NULL AS layer_name,

      'ok' AS spatial_status,
      'manual_extent' AS spatial_type,
      'manual_crs' AS crs_status,

      'manual_extent'
        AS source_type,

      me.id
        AS source_id,

      'manual:' || me.id
        AS source_key,

      'Manual: ' || me.extent_label
        AS source_label

    FROM dataset_manual_spatial_extent_tbl me

    LEFT JOIN data_file_tbl df
      ON df.id = me.data_file_id
      AND df.dataset_id = me.dataset_id

    WHERE me.dataset_id = ?

      AND COALESCE(
        me.active,
        1
      ) = 1

      AND COALESCE(
        me.include_in_dataset_bbox,
        0
      ) = 1

    ORDER BY
      me.extent_label,
      me.id
    ",
    params = list(
      as.integer(dataset_id[1])
    )
  )
}


get_dataset_bbox_source_records_conn <- function(
  conn,
  dataset_id
) {
  extracted <- get_included_spatial_bbox_records_conn(
    conn = conn,
    dataset_id = dataset_id
  )

  manual <- get_included_manual_bbox_records_conn(
    conn = conn,
    dataset_id = dataset_id
  )

  data.table::rbindlist(
    list(
      extracted,
      manual
    ),
    fill = TRUE,
    use.names = TRUE
  )
}

rebuild_dataset_spatial_summary_conn <- function(
  conn,
  dataset_id,
  user_id = NA_integer_
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    stop("dataset_id is required.")
  }

  dataset_id <- as.integer(
    dataset_id[1]
  )

  ensure_dataset_spatial_summary_table(
    conn
  )

  records <- get_dataset_bbox_source_records_conn(
    conn = conn,
    dataset_id = dataset_id
  )

  derive_dataset_bbox_from_records(
    records
  )$contributors

  derived <- derive_dataset_bbox_from_records(
    records
  )

  summary <- derived$summary[
    1,
    ,
    drop = FALSE
  ]

  user_id_value <- if (
    is.null(user_id) ||
    length(user_id) == 0L ||
    is.na(user_id[1])
  ) {
    NA_integer_
  } else {
    as.integer(user_id[1])
  }

  generated_at <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  DBI::dbExecute(
    conn,
    "
    INSERT INTO dataset_spatial_summary_tbl (
      dataset_id,
      bbox_status,
      status_message,
      bbox_crs,
      bbox_wgs84,
      xmin,
      ymin,
      xmax,
      ymax,
      included_source_count,
      eligible_source_count,
      skipped_source_count,
      contributor_keys,
      derivation_method,
      generated_at,
      generated_by_user_id
    )
    VALUES (
      ?, ?, ?, ?, ?, ?, ?, ?, ?,
      ?, ?, ?, ?, ?, ?, ?
    )

    ON CONFLICT(dataset_id)
    DO UPDATE SET
      bbox_status =
        excluded.bbox_status,
      status_message =
        excluded.status_message,
      bbox_crs =
        excluded.bbox_crs,
      bbox_wgs84 =
        excluded.bbox_wgs84,
      xmin =
        excluded.xmin,
      ymin =
        excluded.ymin,
      xmax =
        excluded.xmax,
      ymax =
        excluded.ymax,
      included_source_count =
        excluded.included_source_count,
      eligible_source_count =
        excluded.eligible_source_count,
      skipped_source_count =
        excluded.skipped_source_count,
      contributor_keys =
        excluded.contributor_keys,
      derivation_method =
        excluded.derivation_method,
      generated_at =
        excluded.generated_at,
      generated_by_user_id =
        excluded.generated_by_user_id
    ",
    params = list(
      dataset_id,
      as.character(summary$bbox_status[1]),
      as.character(summary$status_message[1]),
      as.character(summary$bbox_crs[1]),
      as.character(summary$bbox_wgs84[1]),
      as.numeric(summary$xmin[1]),
      as.numeric(summary$ymin[1]),
      as.numeric(summary$xmax[1]),
      as.numeric(summary$ymax[1]),
      as.integer(summary$included_source_count[1]),
      as.integer(summary$eligible_source_count[1]),
      as.integer(summary$skipped_source_count[1]),
      as.character(summary$contributor_keys[1]),
      as.character(summary$derivation_method[1]),
      generated_at,
      user_id_value
    )
  )

  saved_summary <- DBI::dbGetQuery(
    conn,
    "
    SELECT *
    FROM dataset_spatial_summary_tbl
    WHERE dataset_id = ?
    LIMIT 1
    ",
    params = list(
      dataset_id
    )
  )

  list(
    summary = saved_summary,
    contributors = derived$contributors
  )
}

rebuild_dataset_spatial_summary <- function(
  dataset_id,
  base_path,
  user_id = NA_integer_
) {
  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  DBI::dbWithTransaction(
    conn,
    rebuild_dataset_spatial_summary_conn(
      conn = conn,
      dataset_id = dataset_id,
      user_id = user_id
    )
  )
}


get_dataset_spatial_summary <- function(
  dataset_id,
  base_path
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    return(NULL)
  }

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_dataset_spatial_summary_table(
    conn
  )

  result <- DBI::dbGetQuery(
    conn,
    "
    SELECT *
    FROM dataset_spatial_summary_tbl
    WHERE dataset_id = ?
    LIMIT 1
    ",
    params = list(
      as.integer(dataset_id[1])
    )
  )

  if (nrow(result) == 0L) {
    return(NULL)
  }

  result[1, , drop = FALSE]
}


get_dataset_bbox_contributors <- function(
  dataset_id,
  base_path
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    return(data.frame())
  }

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  records <- get_dataset_bbox_source_records_conn(
    conn = conn,
    dataset_id = dataset_id
  )

  derive_dataset_bbox_from_records(
    records
  )$contributors

  derive_dataset_bbox_from_records(
    records
  )$contributors
}

get_manual_spatial_extents <- function(
  dataset_id,
  base_path,
  active_only = FALSE
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    return(data.frame())
  }

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_dataset_manual_spatial_extent_table(
    conn
  )

  active_sql <- if (isTRUE(active_only)) {
    "AND COALESCE(me.active, 1) = 1"
  } else {
    ""
  }

  sql <- paste0(
    "
    SELECT
      me.*,

      df.file_name,
      df.data_file_rel_path_full,
      df.data_file_format,

      created_person.name_first ||
        ' ' ||
        created_person.name_last
        AS created_by_name,

      updated_person.name_first ||
        ' ' ||
        updated_person.name_last
        AS updated_by_name

    FROM dataset_manual_spatial_extent_tbl me

    LEFT JOIN data_file_tbl df
      ON df.id = me.data_file_id
      AND df.dataset_id = me.dataset_id

    LEFT JOIN user_tbl created_user
      ON created_user.id =
          me.created_by_user_id

    LEFT JOIN persons_tbl created_person
      ON created_person.id =
          created_user.person_id

    LEFT JOIN user_tbl updated_user
      ON updated_user.id =
          me.updated_by_user_id

    LEFT JOIN persons_tbl updated_person
      ON updated_person.id =
          updated_user.person_id

    WHERE me.dataset_id = ?
    ",
    active_sql,
    "
    ORDER BY
      me.active DESC,
      me.extent_label,
      me.id
    "
  )

  DBI::dbGetQuery(
    conn,
    sql,
    params = list(
      as.integer(dataset_id[1])
    )
  )
}

save_manual_spatial_extent <- function(
  dataset_id,
  base_path,
  extent_label,
  crs_value,
  xmin,
  ymin,
  xmax,
  ymax,
  data_file_id = NULL,
  source_note = NULL,
  include_in_dataset_bbox = TRUE,
  active = TRUE,
  manual_extent_id = NULL,
  user_id = NA_integer_
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0L ||
    is.na(dataset_id[1])
  ) {
    stop("dataset_id is required.")
  }

  dataset_id <- as.integer(
    dataset_id[1]
  )

  extent_label <- trimws(
    as.character(
      extent_label[1]
    )
  )

  if (!nzchar(extent_label)) {
    stop("A manual extent label is required.")
  }

  validation <- validate_manual_spatial_extent(
    crs_value = crs_value,
    xmin = xmin,
    ymin = ymin,
    xmax = xmax,
    ymax = ymax
  )

  if (!isTRUE(validation$valid)) {
    stop(
      validation$message
    )
  }

  record <- validation$record[
    1,
    ,
    drop = FALSE
  ]

  data_file_id_value <- suppressWarnings(
    as.integer(
      data_file_id[1]
    )
  )

  if (
    is.null(data_file_id) ||
    length(data_file_id) == 0L ||
    is.na(data_file_id_value)
  ) {
    data_file_id_value <- NA_integer_
  }

  user_id_value <- suppressWarnings(
    as.integer(
      user_id[1]
    )
  )

  if (
    is.null(user_id) ||
    length(user_id) == 0L ||
    is.na(user_id_value)
  ) {
    user_id_value <- NA_integer_
  }

  extent_id_value <- suppressWarnings(
    as.integer(
      manual_extent_id[1]
    )
  )

  if (
    is.null(manual_extent_id) ||
    length(manual_extent_id) == 0L ||
    is.na(extent_id_value)
  ) {
    extent_id_value <- NA_integer_
  }

  source_note_value <- if (
    is.null(source_note) ||
    length(source_note) == 0L ||
    is.na(source_note[1]) ||
    !nzchar(trimws(as.character(source_note[1])))
  ) {
    NA_character_
  } else {
    trimws(
      as.character(source_note[1])
    )
  }

  include_value <- as.integer(
    isTRUE(
      include_in_dataset_bbox
    )
  )

  active_value <- as.integer(
    isTRUE(active)
  )

  extent_scope <- if (
    is.na(data_file_id_value)
  ) {
    "dataset"
  } else {
    "data_file"
  }

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_dataset_manual_spatial_extent_table(
    conn
  )

  result <- DBI::dbWithTransaction(
    conn,
    {
      if (!is.na(data_file_id_value)) {
        linked_file <- DBI::dbGetQuery(
          conn,
          "
          SELECT id
          FROM data_file_tbl
          WHERE id = ?
            AND dataset_id = ?
          LIMIT 1
          ",
          params = list(
            data_file_id_value,
            dataset_id
          )
        )

        if (nrow(linked_file) == 0L) {
          stop(
            paste(
              "The selected data file does not belong",
              "to the active dataset."
            )
          )
        }
      }

      now <- format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )

      if (is.na(extent_id_value)) {
        DBI::dbExecute(
          conn,
          "
          INSERT INTO
            dataset_manual_spatial_extent_tbl (
              dataset_id,
              data_file_id,
              extent_scope,
              extent_label,
              source_note,
              crs_auth_code,
              crs_name,
              crs_input,
              bbox_native,
              bbox_status,
              xmin,
              ymin,
              xmax,
              ymax,
              include_in_dataset_bbox,
              active,
              created_at,
              created_by_user_id,
              updated_at,
              updated_by_user_id
            )
          VALUES (
            ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
            ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
          )
          ",
          params = list(
            dataset_id,
            data_file_id_value,
            extent_scope,
            extent_label,
            source_note_value,
            record$crs_auth_code[1],
            record$crs_name[1],
            record$crs_input[1],
            record$bbox_native[1],
            record$bbox_status[1],
            record$xmin[1],
            record$ymin[1],
            record$xmax[1],
            record$ymax[1],
            include_value,
            active_value,
            now,
            user_id_value,
            now,
            user_id_value
          )
        )

        extent_id <- DBI::dbGetQuery(
          conn,
          "SELECT last_insert_rowid() AS id"
        )$id[1]
      } else {
        updated <- DBI::dbExecute(
          conn,
          "
          UPDATE dataset_manual_spatial_extent_tbl
          SET
            data_file_id = ?,
            extent_scope = ?,
            extent_label = ?,
            source_note = ?,
            crs_auth_code = ?,
            crs_name = ?,
            crs_input = ?,
            bbox_native = ?,
            bbox_status = ?,
            xmin = ?,
            ymin = ?,
            xmax = ?,
            ymax = ?,
            include_in_dataset_bbox = ?,
            active = ?,
            updated_at = ?,
            updated_by_user_id = ?
          WHERE id = ?
            AND dataset_id = ?
          ",
          params = list(
            data_file_id_value,
            extent_scope,
            extent_label,
            source_note_value,
            record$crs_auth_code[1],
            record$crs_name[1],
            record$crs_input[1],
            record$bbox_native[1],
            record$bbox_status[1],
            record$xmin[1],
            record$ymin[1],
            record$xmax[1],
            record$ymax[1],
            include_value,
            active_value,
            now,
            user_id_value,
            extent_id_value,
            dataset_id
          )
        )

        if (updated != 1L) {
          stop(
            paste(
              "The selected manual extent could not",
              "be updated."
            )
          )
        }

        extent_id <- extent_id_value
      }

      summary <- rebuild_dataset_spatial_summary_conn(
        conn = conn,
        dataset_id = dataset_id,
        user_id = user_id_value
      )

      list(
        id = as.integer(extent_id),
        validation = validation,
        dataset_summary = summary$summary
      )
    }
  )

  invisible(result)
}

set_manual_spatial_extent_active <- function(
  manual_extent_id,
  active,
  base_path,
  user_id = NA_integer_
) {
  if (
    is.null(manual_extent_id) ||
    length(manual_extent_id) == 0L ||
    is.na(manual_extent_id[1])
  ) {
    stop("manual_extent_id is required.")
  }

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_dataset_manual_spatial_extent_table(
    conn
  )

  record <- DBI::dbGetQuery(
    conn,
    "
    SELECT id, dataset_id
    FROM dataset_manual_spatial_extent_tbl
    WHERE id = ?
    LIMIT 1
    ",
    params = list(
      as.integer(manual_extent_id[1])
    )
  )

  if (nrow(record) == 0L) {
    stop("The manual extent record was not found.")
  }

  user_id_value <- if (
    is.null(user_id) ||
    length(user_id) == 0L ||
    is.na(user_id[1])
  ) {
    NA_integer_
  } else {
    as.integer(user_id[1])
  }

  result <- DBI::dbWithTransaction(
    conn,
    {
      DBI::dbExecute(
        conn,
        "
        UPDATE dataset_manual_spatial_extent_tbl
        SET
          active = ?,
          updated_at = ?,
          updated_by_user_id = ?
        WHERE id = ?
        ",
        params = list(
          as.integer(isTRUE(active)),
          format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S"
          ),
          user_id_value,
          as.integer(manual_extent_id[1])
        )
      )

      rebuild_dataset_spatial_summary_conn(
        conn = conn,
        dataset_id = record$dataset_id[1],
        user_id = user_id_value
      )
    }
  )

  invisible(result)
}

backfill_spatial_bbox_columns <- function(
  base_path,
  dataset_id = NULL
) {
  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_data_file_spatial_metadata_table(
    conn
  )

  has_dataset_filter <- (
    !is.null(dataset_id) &&
      length(dataset_id) > 0L &&
      !is.na(dataset_id[1])
  )

  sql <- "
    SELECT
      id,
      bbox_native,
      xmin,
      ymin,
      xmax,
      ymax
    FROM data_file_spatial_metadata_tbl
  "

  if (isTRUE(has_dataset_filter)) {
    sql <- paste0(
      sql,
      " WHERE dataset_id = ? "
    )

    records <- DBI::dbGetQuery(
      conn,
      sql,
      params = list(
        as.integer(dataset_id[1])
      )
    )
  } else {
    records <- DBI::dbGetQuery(
      conn,
      sql
    )
  }

  if (nrow(records) == 0L) {
    return(
      data.table::data.table(
        bbox_status = character(),
        record_count = integer()
      )
    )
  }

  normalised <- lapply(
    seq_len(nrow(records)),
    function(i) {
      normalise_bbox_record(
        bbox_native = records$bbox_native[i],
        xmin = records$xmin[i],
        ymin = records$ymin[i],
        xmax = records$xmax[i],
        ymax = records$ymax[i]
      )
    }
  )

  DBI::dbWithTransaction(
    conn,
    {
      for (i in seq_len(nrow(records))) {
        bbox <- normalised[[i]]

        DBI::dbExecute(
          conn,
          "
          UPDATE data_file_spatial_metadata_tbl
          SET
            bbox_native = ?,
            bbox_status = ?,
            xmin = ?,
            ymin = ?,
            xmax = ?,
            ymax = ?
          WHERE id = ?
          ",
          params = list(
            bbox$bbox_native,
            bbox$bbox_status,
            bbox$xmin,
            bbox$ymin,
            bbox$xmax,
            bbox$ymax,
            as.integer(records$id[i])
          )
        )
      }
    }
  )

  statuses <- vapply(
    normalised,
    `[[`,
    character(1),
    "bbox_status"
  )

  status_counts <- as.data.frame(
    table(statuses),
    stringsAsFactors = FALSE
  )

  names(status_counts) <- c(
    "bbox_status",
    "record_count"
  )

  data.table::as.data.table(
    status_counts
  )[
    order(bbox_status)
  ]
}

backfill_missing_dataset_spatial_summaries_conn <- function(
  conn
) {
  ensure_dataset_spatial_summary_table(
    conn
  )

  dataset_ids <- DBI::dbGetQuery(
    conn,
    "
    SELECT DISTINCT
      sm.dataset_id

    FROM data_file_spatial_metadata_tbl sm

    LEFT JOIN dataset_spatial_summary_tbl ds
      ON ds.dataset_id = sm.dataset_id

    WHERE COALESCE(
      sm.include_in_dataset_bbox,
      0
    ) = 1

      AND ds.dataset_id IS NULL

    ORDER BY sm.dataset_id
    "
  )

  if (nrow(dataset_ids) == 0L) {
    return(invisible(0L))
  }

  for (dataset_id in dataset_ids$dataset_id) {
    rebuild_dataset_spatial_summary_conn(
      conn = conn,
      dataset_id = dataset_id,
      user_id = NA_integer_
    )
  }

  invisible(
    nrow(dataset_ids)
  )
}

save_data_file_spatial_metadata <- function(
  dataset_id,
  spatial_dt,
  base_path,
  user_id = NA_integer_
) {
  scalar_text <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0 ||
      is.na(x[1])
    ) {
      return("")
    }

    trimws(as.character(x[1]))
  }

  scalar_integer <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0 ||
      is.na(x[1])
    ) {
      return(NA_integer_)
    }

    suppressWarnings(
      as.integer(x[1])
    )
  }

  scalar_numeric <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0 ||
      is.na(x[1])
    ) {
      return(NA_real_)
    }

    suppressWarnings(
      as.numeric(x[1])
    )
  }

  normalise_path_key <- function(x) {
    x <- scalar_text(x)

    if (!nzchar(x)) {
      return("")
    }

    normalizePath(
      x,
      mustWork = FALSE,
      winslash = "/"
    )
  }

  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0 ||
    is.na(dataset_id[1])
  ) {
    stop("dataset_id is required.")
  }

  if (
    is.null(spatial_dt) ||
    nrow(spatial_dt) == 0 ||
    "message" %in% names(spatial_dt)
  ) {
    return(invisible(FALSE))
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_data_file_spatial_metadata_table(conn)

  dt <- data.table::as.data.table(
    data.table::copy(spatial_dt)
  )

  # Compatibility with older spatial extractors that returned
  # cell_size_x and cell_size_y.
  if (!"raster_res_x" %in% names(dt)) {
    dt[
      ,
      raster_res_x := NA_real_
    ]
  }

  if (!"raster_res_y" %in% names(dt)) {
    dt[
      ,
      raster_res_y := NA_real_
    ]
  }

  if ("cell_size_x" %in% names(dt)) {
    dt[
      is.na(raster_res_x),
      raster_res_x := suppressWarnings(
        as.numeric(cell_size_x)
      )
    ]
  }

  if ("cell_size_y" %in% names(dt)) {
    dt[
      is.na(raster_res_y),
      raster_res_y := suppressWarnings(
        as.numeric(cell_size_y)
      )
    ]
  }

  required_columns <- list(
    data_file_rel_path_full = NA_character_,
    logical_rel_path = NA_character_,
    file_name = NA_character_,
    file_path = NA_character_,
    layer_name = NA_character_,
    spatial_status = NA_character_,
    spatial_type = NA_character_,
    crs_auth_code = NA_character_,
    crs_name = NA_character_,
    crs_input = NA_character_,
    crs_status = NA_character_,
    bbox_native = NA_character_,
    xmin = NA_real_,
    ymin = NA_real_,
    xmax = NA_real_,
    ymax = NA_real_,
    geometry_type = NA_character_,
    feature_count = NA_integer_,
    raster_nrow = NA_integer_,
    raster_ncol = NA_integer_,
    raster_nlyr = NA_integer_,
    raster_res_x = NA_real_,
    raster_res_y = NA_real_,
    spatial_note = NA_character_
  )

  for (nm in names(required_columns)) {
    if (!nm %in% names(dt)) {
      dt[, (nm) := required_columns[[nm]]]
    }
  }

  # Normalize again at the persistence boundary. This protects the database
  # if spatial metadata comes from an older extractor or another future source.
  dt <- add_bbox_columns(
    dt
  )

  saved_files <- DBI::dbGetQuery(
    conn,
    "
    SELECT
      id,
      data_file_rel_path_full,
      file_name,
      file_path,
      data_file_format,
      data_pond_id
    FROM data_file_tbl
    WHERE dataset_id = ?
    ",
    params = list(
      as.integer(dataset_id[1])
    )
  )

  saved_files <- data.table::as.data.table(
    saved_files
  )

  if (nrow(saved_files) == 0) {
    stop(
      paste(
        "No saved data-file inventory exists",
        "for dataset",
        dataset_id[1]
      )
    )
  }

  saved_files[
    ,
    normalized_file_path :=
      vapply(
        file_path,
        normalise_path_key,
        character(1)
      )
  ]

  now <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  rows_saved <- 0L
  rows_skipped <- 0L

  DBI::dbWithTransaction(
    conn,
    {
      for (i in seq_len(nrow(dt))) {
        row <- dt[i]

        row_rel_path <- scalar_text(
          row$data_file_rel_path_full
        )

        if (!nzchar(row_rel_path)) {
          row_rel_path <- scalar_text(
            row$logical_rel_path
          )
        }

        row_file_path <- scalar_text(
          row$file_path
        )

        row_file_name <- scalar_text(
          row$file_name
        )

        if (
          !nzchar(row_file_name) &&
          nzchar(row_file_path)
        ) {
          row_file_name <- basename(
            row_file_path
          )
        }

        row_layer_name <- scalar_text(
          row$layer_name
        )

        file_match <- saved_files[0]

        # First choice: exact relative path.
        if (nzchar(row_rel_path)) {
          file_match <- saved_files[
            data_file_rel_path_full ==
              row_rel_path
          ]
        }

        # Second choice: exact normalised absolute path.
        if (
          nrow(file_match) == 0 &&
          nzchar(row_file_path)
        ) {
          normalized_row_path <-
            normalise_path_key(
              row_file_path
            )

          file_match <- saved_files[
            normalized_file_path ==
              normalized_row_path
          ]
        }

        # Third choice: unique filename.
        if (
          nrow(file_match) == 0 &&
          nzchar(row_file_name)
        ) {
          name_match <- saved_files[
            file_name == row_file_name
          ]

          if (nrow(name_match) == 1L) {
            file_match <- name_match
          }
        }

        if (nrow(file_match) > 0) {
          file_match <- file_match[1]

          data_file_id <- as.integer(
            file_match$id[1]
          )

          data_pond_id <- scalar_text(
            file_match$data_pond_id[1]
          )

          if (!nzchar(data_pond_id)) {
            data_pond_id <- NA_character_
          }

          # Canonical relative path from data_file_tbl.
          row_rel_path <- scalar_text(
            file_match$data_file_rel_path_full[1]
          )

          if (!nzchar(row_file_path)) {
            row_file_path <- scalar_text(
              file_match$file_path[1]
            )
          }
        } else {
          data_file_id <- NA_integer_
          data_pond_id <- NA_character_
        }

        if (!nzchar(row_rel_path)) {
          rows_skipped <- rows_skipped + 1L

          warning(
            paste(
              "Skipping unmatched spatial metadata row.",
              "File path:",
              row_file_path,
              "File name:",
              row_file_name,
              "Layer:",
              row_layer_name
            )
          )

          next
        }

        layer_key <- if (
          nzchar(row_layer_name)
        ) {
          row_layer_name
        } else {
          ""
        }

        layer_name_to_save <- if (
          nzchar(row_layer_name)
        ) {
          row_layer_name
        } else {
          NA_character_
        }

        DBI::dbExecute(
          conn,
          "
          INSERT INTO data_file_spatial_metadata_tbl (
            dataset_id,
            data_file_id,
            data_pond_id,
            data_file_rel_path_full,
            file_path,
            layer_name,
            layer_key,
            spatial_status,
            spatial_type,
            crs_auth_code,
            crs_name,
            crs_input,
            crs_status,
            bbox_native,
            bbox_status,
            xmin,
            ymin,
            xmax,
            ymax,
            geometry_type,
            feature_count,
            raster_nrow,
            raster_ncol,
            raster_nlyr,
            raster_res_x,
            raster_res_y,
            spatial_note,
            extracted_at,
            extracted_by_user_id
          )
          VALUES (
            ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
            ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
            ?, ?, ?, ?, ?, ?, ?, ?, ?
          )
          ON CONFLICT (
            dataset_id,
            data_file_rel_path_full,
            layer_key
          )
          DO UPDATE SET
            data_file_id =
              excluded.data_file_id,
            data_pond_id =
              excluded.data_pond_id,
            file_path =
              excluded.file_path,
            layer_name =
              excluded.layer_name,
            spatial_status =
              excluded.spatial_status,
            spatial_type =
              excluded.spatial_type,
            crs_auth_code =
              excluded.crs_auth_code,
            crs_name =
              excluded.crs_name,
            crs_input =
              excluded.crs_input,
            crs_status =
              excluded.crs_status,
            bbox_native =
              excluded.bbox_native,
            bbox_status =
              excluded.bbox_status,
            xmin =
              excluded.xmin,
            ymin =
              excluded.ymin,
            xmax =
              excluded.xmax,
            ymax =
              excluded.ymax,
            geometry_type =
              excluded.geometry_type,
            feature_count =
              excluded.feature_count,
            raster_nrow =
              excluded.raster_nrow,
            raster_ncol =
              excluded.raster_ncol,
            raster_nlyr =
              excluded.raster_nlyr,
            raster_res_x =
              excluded.raster_res_x,
            raster_res_y =
              excluded.raster_res_y,
            spatial_note =
              excluded.spatial_note,
            extracted_at =
              excluded.extracted_at,
            extracted_by_user_id =
              excluded.extracted_by_user_id
          ",
          params = list(
            as.integer(dataset_id[1]),
            data_file_id,
            data_pond_id,

            # Important: use the resolved canonical path.
            row_rel_path,

            if (nzchar(row_file_path)) {
              row_file_path
            } else {
              NA_character_
            },

            layer_name_to_save,
            layer_key,
            scalar_text(row$spatial_status),
            scalar_text(row$spatial_type),
            scalar_text(row$crs_auth_code),
            scalar_text(row$crs_name),
            scalar_text(row$crs_input),
            scalar_text(row$crs_status),
            scalar_text(row$bbox_native),
            scalar_text(row$bbox_status),
            scalar_numeric(row$xmin),
            scalar_numeric(row$ymin),
            scalar_numeric(row$xmax),
            scalar_numeric(row$ymax),
            scalar_text(row$geometry_type),
            scalar_integer(row$feature_count),
            scalar_integer(row$raster_nrow),
            scalar_integer(row$raster_ncol),
            scalar_integer(row$raster_nlyr),
            scalar_numeric(row$raster_res_x),
            scalar_numeric(row$raster_res_y),
            scalar_text(row$spatial_note),
            now,
            if (
              is.null(user_id) ||
              length(user_id) == 0 ||
              is.na(user_id[1])
            ) {
              NA_integer_
            } else {
              as.integer(user_id[1])
            }
          )
        )

        rows_saved <- rows_saved + 1L
      }
    }
  )

  dataset_summary <- DBI::dbWithTransaction(
    conn,
    rebuild_dataset_spatial_summary_conn(
      conn = conn,
      dataset_id = as.integer(dataset_id[1]),
      user_id = user_id
    )
  )

  invisible(
    data.table::data.table(
      rows_received = nrow(dt),
      rows_saved = rows_saved,
      rows_skipped = rows_skipped,
      dataset_bbox_status =
        dataset_summary$summary$bbox_status[1],
      dataset_bbox_sources =
        dataset_summary$summary$eligible_source_count[1]
    )
  )
}

get_saved_data_file_spatial_metadata <- function(
  dataset_id,
  base_path
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0 ||
    is.na(dataset_id[1])
  ) {
    return(data.frame())
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  if (
    !DBI::dbExistsTable(
      conn,
      "data_file_spatial_metadata_tbl"
    )
  ) {
    return(data.frame())
  }

  DBI::dbGetQuery(
    conn,
    "
    SELECT
      sm.*,
      df.file_name,
      df.data_file_format,
      p.name_first || ' ' || p.name_last
        AS notes_updated_by_name
    FROM data_file_spatial_metadata_tbl sm

    LEFT JOIN data_file_tbl df
      ON df.dataset_id = sm.dataset_id
    AND df.data_file_rel_path_full =
        sm.data_file_rel_path_full

    LEFT JOIN user_tbl u
      ON u.id = sm.notes_updated_by_user_id

    LEFT JOIN persons_tbl p
      ON p.id = u.person_id

    WHERE sm.dataset_id = ?

    ORDER BY
      sm.data_file_rel_path_full,
      sm.layer_name
    ",
    params = list(
      as.integer(dataset_id[1])
    )
  )
}

update_data_file_spatial_metadata_notes <- function(
  spatial_metadata_id,
  base_path,
  user_notes = NULL,
  user_id = NA_integer_
) {
  if (
    is.null(spatial_metadata_id) ||
    length(spatial_metadata_id) == 0 ||
    is.na(spatial_metadata_id[1])
  ) {
    stop("spatial_metadata_id is required.")
  }

  notes_value <- if (
    is.null(user_notes) ||
    length(user_notes) == 0 ||
    is.na(user_notes[1]) ||
    trimws(as.character(user_notes[1])) == ""
  ) {
    NA_character_
  } else {
    trimws(as.character(user_notes[1]))
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_data_file_spatial_metadata_table(conn)

  updated_rows <- DBI::dbExecute(
    conn,
    "
    UPDATE data_file_spatial_metadata_tbl
    SET
      user_notes = ?,
      notes_updated_at = ?,
      notes_updated_by_user_id = ?
    WHERE id = ?
    ",
    params = list(
      notes_value,
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      if (
        is.null(user_id) ||
        length(user_id) == 0 ||
        is.na(user_id[1])
      ) {
        NA_integer_
      } else {
        as.integer(user_id[1])
      },
      as.integer(spatial_metadata_id[1])
    )
  )

  if (updated_rows != 1L) {
    stop(
      paste(
        "Spatial metadata record was not updated:",
        spatial_metadata_id[1]
      )
    )
  }

  invisible(TRUE)
}

update_spatial_bbox_inclusion <- function(
  spatial_metadata_id,
  include_in_dataset_bbox,
  base_path,
  user_id = NA_integer_
) {
  if (
    is.null(spatial_metadata_id) ||
    length(spatial_metadata_id) == 0L ||
    is.na(spatial_metadata_id[1])
  ) {
    stop("spatial_metadata_id is required.")
  }

  include_value <- as.integer(
    isTRUE(
      as.logical(
        include_in_dataset_bbox[1]
      )
    )
  )

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_data_file_spatial_metadata_table(
    conn
  )

  record <- DBI::dbGetQuery(
    conn,
    "
    SELECT *
    FROM data_file_spatial_metadata_tbl
    WHERE id = ?
    LIMIT 1
    ",
    params = list(
      as.integer(spatial_metadata_id[1])
    )
  )

  if (nrow(record) == 0L) {
    stop(
      paste(
        "Spatial metadata record was not found:",
        spatial_metadata_id[1]
      )
    )
  }

  eligibility <- spatial_bbox_preview(
    record
  )

  if (
    include_value == 1L &&
    !isTRUE(eligibility$eligible)
  ) {
    stop(
      paste(
        "This spatial record cannot be included:",
        eligibility$message
      )
    )
  }

  user_id_value <- if (
    is.null(user_id) ||
    length(user_id) == 0L ||
    is.na(user_id[1])
  ) {
    NA_integer_
  } else {
    as.integer(user_id[1])
  }

  result <- DBI::dbWithTransaction(
    conn,
    {
      updated <- DBI::dbExecute(
        conn,
        "
        UPDATE data_file_spatial_metadata_tbl
        SET
          include_in_dataset_bbox = ?,
          bbox_inclusion_updated_at = ?,
          bbox_inclusion_updated_by_user_id = ?
        WHERE id = ?
        ",
        params = list(
          include_value,
          format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S"
          ),
          user_id_value,
          as.integer(spatial_metadata_id[1])
        )
      )

      if (updated != 1L) {
        stop(
          paste(
            "Spatial bbox inclusion was not updated:",
            spatial_metadata_id[1]
          )
        )
      }

      dataset_summary <-
        rebuild_dataset_spatial_summary_conn(
          conn = conn,
          dataset_id = record$dataset_id[1],
          user_id = user_id_value
        )

      list(
        included = include_value == 1L,
        eligibility = eligibility,
        dataset_summary =
          dataset_summary$summary
      )
    }
  )

  invisible(
    result
  )
}
