pond_bibliography_path <- function(base_path) {
  file.path(
    normalizePath(
      path.expand(base_path),
      mustWork = FALSE,
      winslash = "/"
    ),
    "datapond.bib"
  )
}


biblatex_scalar <- function(x, default = "") {
  first_non_empty(
    x,
    default = default
  )
}


biblatex_escape <- function(x) {
  x <- biblatex_scalar(
    x,
    default = ""
  )

  x <- gsub(
    "[\r\n\t]+",
    " ",
    as.character(x)
  )

  chars <- strsplit(
    x,
    "",
    fixed = TRUE
  )[[1]]

  if (length(chars) == 0L) {
    return("")
  }

  escaped <- vapply(
    chars,
    function(char) {
      switch(
        char,
        "\\" = "{\\textbackslash{}}",
        "{" = "\\{",
        "}" = "\\}",
        "%" = "\\%",
        "#" = "\\#",
        "&" = "\\&",
        "_" = "\\_",
        "$" = "\\$",
        "^" = "\\^{}",
        "~" = "\\~{}",
        char
      )
    },
    character(1)
  )

  paste0(
    escaped,
    collapse = ""
  )
}


biblatex_verbatim <- function(x) {
  x <- biblatex_scalar(
    x,
    default = ""
  )

  x <- gsub(
    "[\r\n\t]+",
    "",
    as.character(x)
  )

  x <- gsub(
    "{",
    "%7B",
    x,
    fixed = TRUE
  )

  gsub(
    "}",
    "%7D",
    x,
    fixed = TRUE
  )
}


biblatex_field <- function(
  field,
  value,
  protect_case = FALSE,
  verbatim = FALSE,
  preformatted = FALSE
) {
  value <- biblatex_scalar(
    value,
    default = ""
  )

  if (!nzchar(value)) {
    return(NULL)
  }

  value <- if (isTRUE(preformatted)) {
    value
  } else if (isTRUE(verbatim)) {
    biblatex_verbatim(value)
  } else {
    biblatex_escape(value)
  }

  if (isTRUE(protect_case)) {
    value <- paste0(
      "{",
      value,
      "}"
    )
  }

  paste0(
    "  ",
    field,
    " = {",
    value,
    "}"
  )
}


biblatex_person_names <- function(authors_df) {
  if (
    is.null(authors_df) ||
    nrow(as.data.frame(authors_df)) == 0L
  ) {
    return("")
  }

  authors_df <- as.data.frame(
    authors_df,
    stringsAsFactors = FALSE
  )

  names <- vapply(
    seq_len(nrow(authors_df)),
    function(i) {
      first <- biblatex_scalar(
        authors_df$name_first[i],
        default = ""
      )

      last <- biblatex_scalar(
        authors_df$name_last[i],
        default = ""
      )

      if (nzchar(last) && nzchar(first)) {
        paste0(
          biblatex_escape(last),
          ", ",
          biblatex_escape(first)
        )
      } else {
        biblatex_escape(
          paste(
            c(last, first)[
              nzchar(c(last, first))
            ],
            collapse = " "
          )
        )
      }
    },
    character(1)
  )

  names <- names[
    nzchar(names)
  ]

  paste(
    names,
    collapse = " and "
  )
}


biblatex_organisation_names <- function(org_df) {
  if (
    is.null(org_df) ||
    nrow(as.data.frame(org_df)) == 0L
  ) {
    return(character())
  }

  org_df <- as.data.frame(
    org_df,
    stringsAsFactors = FALSE
  )

  full <- if (
    "organisation_name_full" %in%
      names(org_df)
  ) {
    org_df$organisation_name_full
  } else {
    rep("", nrow(org_df))
  }

  short <- if (
    "organisation_name_short" %in%
      names(org_df)
  ) {
    org_df$organisation_name_short
  } else {
    rep("", nrow(org_df))
  }

  full <- trimws(
    as.character(full)
  )

  short <- trimws(
    as.character(short)
  )

  full[is.na(full)] <- ""
  short[is.na(short)] <- ""

  values <- ifelse(
    nzchar(full),
    full,
    short
  )

  unique(
    values[
      nzchar(values)
    ]
  )
}


biblatex_organisation_units <- function(org_df) {
  if (
    is.null(org_df) ||
    nrow(as.data.frame(org_df)) == 0L
  ) {
    return(character())
  }

  org_df <- as.data.frame(
    org_df,
    stringsAsFactors = FALSE
  )

  units <- vapply(
    seq_len(nrow(org_df)),
    function(i) {
      row <- org_df[i, , drop = FALSE]
      organisation <- organisation_field_value(
        row,
        c(
          "organisation_name_full",
          "name",
          "organisation_name_short",
          "short"
        )
      )
      department <- organisation_field_value(
        row,
        c("organisation_department", "department")
      )

      paste(
        c(organisation, department)[
          nzchar(c(organisation, department))
        ],
        collapse = ", "
      )
    },
    character(1)
  )

  unique(units[nzchar(units)])
}


biblatex_corporate_author <- function(org_names) {
  if (length(org_names) == 0L) {
    return("")
  }

  paste(
    paste0(
      "{",
      vapply(
        org_names,
        biblatex_escape,
        character(1)
      ),
      "}"
    ),
    collapse = " and "
  )
}


biblatex_dataset_creator <- function(
  record,
  authors_df,
  org_df
) {
  style <- tolower(
    biblatex_scalar(
      record$citation_author_type,
      default = "organisation"
    )
  )

  people <- biblatex_person_names(
    authors_df
  )

  organisations <- biblatex_corporate_author(
    biblatex_organisation_names(
      org_df
    )
  )

  creator <- if (
    style %in%
      c(
        "author",
        "authors",
        "person",
        "personal"
      )
  ) {
    if (nzchar(people)) {
      people
    } else {
      organisations
    }
  } else {
    if (nzchar(organisations)) {
      organisations
    } else {
      people
    }
  }

  if (nzchar(creator)) {
    creator
  } else {
    "{Unknown creator}"
  }
}


normalise_biblatex_doi <- function(x) {
  doi <- biblatex_scalar(
    x,
    default = ""
  )

  doi <- sub(
    "^https?://(?:dx\\.)?doi\\.org/",
    "",
    doi,
    ignore.case = TRUE
  )

  sub(
    "^doi:\\s*",
    "",
    doi,
    ignore.case = TRUE
  )
}


format_biblatex_dataset <- function(
  record,
  authors_df = NULL,
  org_df = NULL,
  frozen_at = NULL,
  release_id = NULL
) {
  if (
    is.null(record) ||
    nrow(as.data.frame(record)) == 0L
  ) {
    stop(
      "A saved dataset record is required."
    )
  }

  record <- as.data.frame(
    record,
    stringsAsFactors = FALSE
  )[1, , drop = FALSE]

  key <- biblatex_scalar(
    record$citation_key,
    default = ""
  )

  if (!nzchar(key)) {
    stop(
      "The dataset does not have a citation key."
    )
  }

  title <- biblatex_scalar(
    record$title,
    default = "Untitled dataset"
  )

  organisations <- biblatex_organisation_names(
    org_df
  )
  organisation_units <- biblatex_organisation_units(
    org_df
  )

  primary_org <- if (
    length(organisations) > 0L
  ) {
    organisations[1]
  } else {
    ""
  }

  location <- organisation_citation_affiliation(
    org_df
  )

  fields <- Filter(
    Negate(is.null),
    list(
      biblatex_field(
        "author",
        biblatex_dataset_creator(
          record = record,
          authors_df = authors_df,
          org_df = org_df
        ),
        preformatted = TRUE
      ),
      biblatex_field(
        "title",
        title,
        protect_case = TRUE
      ),
      biblatex_field(
        "date",
        record$citation_date
      ),
      biblatex_field(
        "version",
        record$version
      ),
      biblatex_field(
        "organization",
        paste(
          organisation_units,
          collapse = " and "
        )
      ),
      biblatex_field(
        "publisher",
        primary_org
      ),
      biblatex_field(
        "location",
        location
      ),
      biblatex_field(
        "doi",
        normalise_biblatex_doi(
          record$doi
        ),
        verbatim = TRUE
      ),
      biblatex_field(
        "url",
        record$data_distribution_url,
        verbatim = TRUE
      ),
      biblatex_field(
        "abstract",
        record$abstract
      ),
      if (!is.null(release_id)) biblatex_field(
        "note",
        paste0("Frozen ", substr(frozen_at, 1L, 10L),
               "; release ID ", release_id)
      )
    )
  )

  paste0(
    "@dataset{",
    key,
    ",\n",
    paste(
      fields,
      collapse = ",\n"
    ),
    "\n}"
  )
}


release_biblatex_entry <- function(row, base_path) {
  paths <- release_paths(base_path, row)
  if (!file.exists(paths$snapshot) ||
      !identical(release_sha(paths$snapshot), row$snapshot_sha256[1])) {
    stop("Frozen bibliography metadata is missing or changed for release ",
         row$release_id[1], ".")
  }

  payload <- release_read_payload(paths$snapshot)
  record <- payload$entry_tbl
  if (nrow(record) != 1L ||
      !identical(as.character(record$uuid[1]), as.character(row$dataset_uuid[1]))) {
    stop("Frozen bibliography metadata does not match release ", row$release_id[1], ".")
  }
  record$version <- row$version[1]
  base_key <- citation_key_base(record$citation_key[1])
  if (is.null(base_key) || !nzchar(base_key)) {
    base_key <- paste0("dataset_", gsub("[^a-z0-9]", "",
                                      tolower(row$dataset_uuid[1])))
  }
  record$citation_key <- paste0(
    base_key, "_",
    format_citation_key_version(row$version[1])
  )
  if (!nzchar(biblatex_scalar(record$citation_date))) {
    record$citation_date <- substr(row$frozen_at[1], 1L, 10L)
  }

  authors <- payload$scoped$authority_tbl
  people <- payload$persons_tbl
  if (!is.null(authors) && nrow(authors)) {
    people <- people[match(authors$person_id, people$id), , drop = FALSE]
    authors <- data.frame(
      name_first = people$name_first,
      name_last = people$name_last,
      author_order = authors$author_order
    )
    authors <- authors[order(authors$author_order, na.last = TRUE), , drop = FALSE]
  }

  organisations <- payload$scoped$dataset_organisation_tbl
  org_refs <- payload$organisation_tbl
  if (!is.null(organisations) && nrow(organisations)) {
    org_refs <- org_refs[match(organisations$organisation_id, org_refs$id), , drop = FALSE]
    organisations <- cbind(organisations, org_refs)
  }

  list(
    key = record$citation_key[1],
    entry = format_biblatex_dataset(
      record, authors_df = authors, org_df = organisations,
      frozen_at = row$frozen_at[1], release_id = row$release_id[1]
    )
  )
}


dataset_biblatex_entry <- function(
  dataset_id,
  base_path
) {
  record <- get_dataset_record(
    dataset_id = dataset_id,
    base_path = base_path
  )

  if (
    is.null(record) ||
    nrow(as.data.frame(record)) == 0L
  ) {
    stop(
      "The selected dataset record was not found."
    )
  }

  format_biblatex_dataset(
    record = record,
    authors_df = get_dataset_authors(
      dataset_id = dataset_id,
      base_path = base_path
    ),
    org_df = get_dataset_org_links(
      dataset_id = dataset_id,
      base_path = base_path
    )
  )
}


write_pond_bibliography <- function(base_path, update_keys = TRUE) {
  base_path <- normalizePath(
    path.expand(base_path),
    mustWork = FALSE,
    winslash = "/"
  )

  if (!dir.exists(base_path)) {
    stop(
      "The active data pond directory does not exist."
    )
  }

  datasets <- get_datasets(
    base_path
  )

  datasets <- as.data.frame(
    datasets,
    stringsAsFactors = FALSE
  )

  entries <- character()
  keys <- character()
  updated_keys <- 0L
  updated_ids <- integer()
  replacement_keys <- character()

  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  releases <- if (DBI::dbExistsTable(conn, "dataset_release_tbl")) {
    DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_tbl ORDER BY dataset_id, frozen_at, release_id")
  } else {
    data.frame()
  }

  if (nrow(datasets) > 0L) {
    for (i in seq_len(nrow(datasets))) {
      record <- datasets[i, , drop = FALSE]
      dataset_id <- as.integer(
        record$id[1]
      )
      has_release <- nrow(releases) > 0L && dataset_id %in% releases$dataset_id
      archived <- identical(as.character(record$lifecycle_status[1]), "archived")
      release_matches_record <- has_release &&
        isTRUE(citation_key_frozen_state(as.list(record))) &&
        isTRUE(any(releases$dataset_id == dataset_id &
                     releases$version == record$version[1], na.rm = TRUE))
      if ((archived && has_release) || release_matches_record) next

      org_df <- get_dataset_org_links(
        dataset_id = dataset_id,
        base_path = base_path
      )

      key <- generate_citation_key(
        vals = as.list(record),
        org_df = org_df,
        existing_key =
          record$citation_key[1] %||%
            NULL
      )

      old_key <- biblatex_scalar(
        record$citation_key,
        default = ""
      )

      if (!identical(key, old_key)) {
        record$citation_key <- key
        if (isTRUE(update_keys)) {
          updated_keys <- updated_keys + 1L
          updated_ids <- c(updated_ids, dataset_id)
          replacement_keys <- c(replacement_keys, key)
        }
      }

      keys <- c(keys, key)
      entries <- c(entries, format_biblatex_dataset(
        record = record,
        authors_df = get_dataset_authors(dataset_id, base_path),
        org_df = org_df
      ))
    }
  }

  if (nrow(releases)) {
    for (i in seq_len(nrow(releases))) {
      frozen <- release_biblatex_entry(releases[i, , drop = FALSE], base_path)
      keys <- c(keys, frozen$key)
      entries <- c(entries, frozen$entry)
    }
  }

  duplicate_keys <- unique(
    keys[
      duplicated(keys)
    ]
  )

  if (length(duplicate_keys) > 0L) {
    stop(
      "Duplicate citation keys prevent bibliography generation: ",
      paste(
        duplicate_keys,
        collapse = ", "
      )
    )
  }

  if (length(updated_ids) > 0L) {
    DBI::dbWithTransaction(
      conn,
      {
        for (i in seq_along(updated_ids)) {
          DBI::dbExecute(
            conn,
            "
            UPDATE entry_tbl
            SET citation_key = ?
            WHERE id = ?
            ",
            params = list(
              replacement_keys[i],
              updated_ids[i]
            )
          )
        }
      }
    )
  }

  if (length(entries) > 0L) {
    order_index <- order(
      keys
    )

    keys <- keys[
      order_index
    ]

    entries <- entries[
      order_index
    ]
  }

  generated_at <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S %z"
  )

  content <- c(
    "% Generated by Data Pond from metadata.db and frozen release snapshots.",
    paste0(
      "% Refreshed: ",
      generated_at
    ),
    "% This file is derived output; edit dataset metadata in Data Pond.",
    "",
    if (length(entries) > 0L) {
      paste(
        entries,
        collapse = "\n\n"
      )
    } else {
      "% No saved dataset records are available."
    },
    ""
  )

  path <- pond_bibliography_path(
    base_path
  )

  temporary_path <- tempfile(
    pattern = ".datapond-bibliography-",
    tmpdir = base_path,
    fileext = ".tmp"
  )

  on.exit(
    unlink(
      temporary_path,
      force = TRUE
    ),
    add = TRUE
  )

  writeLines(
    content,
    con = temporary_path,
    useBytes = TRUE
  )

  backup_path <- paste0(
    path,
    ".bak"
  )

  if (file.exists(path)) {
    file.copy(
      from = path,
      to = backup_path,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }

  replaced <- file.rename(
    from = temporary_path,
    to = path
  )

  if (!isTRUE(replaced)) {
    replaced <- file.copy(
      from = temporary_path,
      to = path,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }

  if (!isTRUE(replaced)) {
    stop(
      "The pond bibliography could not be written."
    )
  }

  list(
    path = path,
    entries = length(entries),
    keys = keys,
    updated_keys = updated_keys,
    generated_at = generated_at
  )
}
