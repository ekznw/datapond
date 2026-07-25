# helper functions for the app server
collect_inputs <- function(input, dt) {
  fields <- dt$field
  values <- lapply(fields, function(f) {
    if (is.null(input[[f]])) return(NULL)
    input[[f]]
  })
  names(values) <- fields
  values
}

validate_required <- function(dt, input) {
  if (is.null(dt) || nrow(dt) == 0) return(character(0))
  if (!"obligation" %in% names(dt)) return(character(0))
  field_vec <- as.character(dt$field)
  obligation_vec <- as.character(dt$obligation)
  label_vec <- as.character(dt$label)
  type_vec <- as.character(dt$type)
  required_idx <- which(
    obligation_vec == "M" &
      type_vec != "hidden" &
      !is.na(field_vec) &
      field_vec != ""
  )
  if (length(required_idx) == 0) return(character(0))
  required_fields <- field_vec[required_idx]
  required_labels <- label_vec[required_idx]
  missing <- character(0)
  is_missing_value <- function(val) {
    if (is.null(val) || length(val) == 0) {
      return(TRUE)
    }
    if (inherits(val, "Date")) {
      return(all(is.na(val)))
    }
    if (is.character(val)) {
      val <- trimws(val)
      return(length(val) == 0 || all(is.na(val) | val == ""))
    }
    if (is.logical(val)) {
      return(length(val) == 0 || all(is.na(val)))
    }
    if (is.numeric(val) || is.integer(val)) {
      return(length(val) == 0 || all(is.na(val)))
    }
    return(all(is.na(val)))
  }
  for (i in seq_along(required_fields)) {
    f <- required_fields[i]
    lab <- required_labels[i] %||% f
    val <- input[[f]]
    if (is_missing_value(val)) {
      missing <- c(missing, lab)
    }
  }
  missing
}

# helper functions for citation and citation key generator
extract_first_keyword <- function(x) {
  if (is.null(x) || x == "") return(NULL)
  parts <- unlist(strsplit(x, "[;,]"))
  kw <- trimws(parts[1])

  # remove spaces/punctuation + lowercase
  kw <- tolower(gsub("[^A-Za-z0-9]", "", kw))
  if (kw == "") return(NULL)
  substr(kw, 1, 6)
}

title_to_initials <- function(title) {
  title <- first_non_empty(title, default = "")

  if (is.null(title) || is.na(title) || trimws(title) == "") {
    return("xxx")
  }

  words <- unlist(strsplit(tolower(title), "\\s+"))

  words <- words[!is.na(words)]
  words <- gsub("[^a-z0-9]", "", words)
  words <- words[words != ""]

  stopwords <- c(
    "the",
    "and",
    "of",
    "for",
    "with",
    "data",
    "map",
    "analysis"
  )

  words <- words[!words %in% stopwords]

  if (length(words) == 0) {
    return("xxx")
  }

  initials <- substr(words, 1, 1)

  paste(initials[seq_len(min(4, length(initials)))], collapse = "")
}

to_snake_clean <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", x))
}

# random alphanumeric generator --- ignores ambiguous letters
generate_rand_part <- function() {
  pool <- c(
    letters[!letters %in% c("o", "l")],
    setdiff(0:9, c(0, 1))
  )
  paste0(sample(pool, 4, replace = TRUE), collapse = "")
}

extract_rand_part_from_citation_key <- function(citation_key) {
  if (is.null(citation_key) ||
      length(citation_key) == 0 ||
      is.na(citation_key[1]) ||
      citation_key[1] == "") {
    return(NULL)
  }

  citation_key <- as.character(citation_key[1])

  # Expected ending pattern:
  # _abcd_v0_1_0
  # where abcd is the stable 4-character random disambiguator.
  m <- regexec("_([a-z0-9]{4})_v[0-9_]+$", citation_key)
  hit <- regmatches(citation_key, m)[[1]]

  if (length(hit) >= 2) {
    return(hit[2])
  }

  NULL
}

extract_citation_key_from_folder_name <- function(dataset_path) {
  if (is.null(dataset_path) ||
      length(dataset_path) == 0 ||
      is.na(dataset_path[1]) ||
      dataset_path[1] == "") {
    return(NULL)
  }
  folder <- basename(normalizePath(dataset_path[1], mustWork = FALSE, winslash = "/"))
  # Folder pattern is:
  # citation_key__uuid
  if (!grepl("__", folder, fixed = TRUE)) {
    return(NULL)
  }

  sub("__.*$", "", folder)
}

# citetation generator
# if 'style == organisation' then the org will be listed as author in citation
# this is the ezemvelo default.

format_given_name_initials <- function(
  first_name
) {
  if (
    is.null(first_name) ||
    length(first_name) == 0 ||
    is.na(first_name[1]) ||
    !nzchar(trimws(as.character(first_name[1])))
  ) {
    return("")
  }

  first_name <- trimws(
    as.character(first_name[1])
  )

  # Split ordinary compound names on whitespace.
  name_parts <- unlist(
    strsplit(
      first_name,
      "\\s+"
    )
  )

  name_parts <- name_parts[
    nzchar(name_parts)
  ]

  if (length(name_parts) == 0) {
    return("")
  }

  format_part <- function(part) {
    part <- trimws(part)

    if (!nzchar(part)) {
      return("")
    }

    # Preserve hyphenated initials, for example:
    # Jean-Paul -> J.-P.
    hyphen_parts <- unlist(
      strsplit(
        part,
        "-",
        fixed = TRUE
      )
    )

    initials <- vapply(
      hyphen_parts,
      function(x) {
        x <- gsub(
          "[^[:alpha:]]",
          "",
          x
        )

        if (!nzchar(x)) {
          return("")
        }

        paste0(
          toupper(
            substr(
              x,
              1L,
              1L
            )
          ),
          "."
        )
      },
      character(1)
    )

    initials <- initials[
      nzchar(initials)
    ]

    paste(
      initials,
      collapse = "-"
    )
  }

  initials <- vapply(
    name_parts,
    format_part,
    character(1)
  )

  initials <- initials[
    nzchar(initials)
  ]

  paste(
    initials,
    collapse = " "
  )
}

format_citation_display_name <- function(name) {
  if (
    is.null(name) ||
    length(name) == 0L ||
    is.na(name[1]) ||
    !nzchar(trimws(as.character(name[1])))
  ) {
    return("")
  }

  name <- trimws(as.character(name[1]))

  # Already in bibliographic form, for example:
  # Clark, D. J.
  if (grepl(",", name, fixed = TRUE)) {
    parts <- strsplit(
      name,
      ",",
      fixed = TRUE
    )[[1]]

    last_name <- trimws(parts[1])

    given_part <- if (length(parts) > 1L) {
      trimws(paste(parts[-1], collapse = " "))
    } else {
      ""
    }

    initials <- format_given_name_initials(given_part)

    if (nzchar(initials)) {
      return(
        paste0(
          last_name,
          ", ",
          initials
        )
      )
    }

    return(last_name)
  }

  name_parts <- strsplit(
    name,
    "\\s+"
  )[[1]]

  name_parts <- name_parts[
    nzchar(name_parts)
  ]

  if (length(name_parts) == 0L) {
    return("")
  }

  # A surname-only record remains unchanged.
  if (length(name_parts) == 1L) {
    return(name_parts[1])
  }

  last_name <- name_parts[length(name_parts)]

  first_names <- paste(
    name_parts[-length(name_parts)],
    collapse = " "
  )

  initials <- format_given_name_initials(
    first_names
  )

  if (!nzchar(initials)) {
    return(last_name)
  }

  paste0(
    last_name,
    ", ",
    initials
  )
}


citation_text_value <- function(x, default = "") {
  if (
    is.null(x) ||
    length(x) == 0L ||
    all(is.na(x))
  ) {
    return(default)
  }

  x <- trimws(as.character(x))
  x <- x[
    !is.na(x) &
      nzchar(x)
  ]

  if (length(x) == 0L) {
    return(default)
  }

  x[1]
}

format_citation_person <- function(
  first_name,
  last_name
) {
  first_name <- if (
    is.null(first_name) ||
    length(first_name) == 0L ||
    is.na(first_name[1])
  ) {
    ""
  } else {
    trimws(
      as.character(first_name[1])
    )
  }

  last_name <- if (
    is.null(last_name) ||
    length(last_name) == 0L ||
    is.na(last_name[1])
  ) {
    ""
  } else {
    trimws(
      as.character(last_name[1])
    )
  }

  initials <- format_given_name_initials(
    first_name
  )

  if (
    nzchar(last_name) &&
    nzchar(initials)
  ) {
    return(
      paste0(
        last_name,
        ", ",
        initials
      )
    )
  }

  # A person may legitimately have no recorded given name.
  if (nzchar(last_name)) {
    return(last_name)
  }

  if (nzchar(initials)) {
    return(initials)
  }

  ""
}

format_citation_authors <- function(
  authors_df,
  et_al_threshold = 11L
) {
  if (
    is.null(authors_df) ||
    nrow(authors_df) == 0L
  ) {
    return("")
  }

  authors_df <- as.data.frame(
    authors_df,
    stringsAsFactors = FALSE
  )

  # Citation formatting must use the structured surname field.
  # Do not use the derived display column `name`.
  if (!"name_last" %in% names(authors_df)) {
    warning(
      paste0(
        "Citation authors require a name_last column. ",
        "Available columns: ",
        paste(
          names(authors_df),
          collapse = ", "
        )
      )
    )

    return("")
  }

  # Given names are optional.
  if (!"name_first" %in% names(authors_df)) {
    authors_df$name_first <- ""
  }

  # Preserve the dataset-specific author order.
  if ("author_order" %in% names(authors_df)) {
    authors_df <- authors_df[
      order(
        authors_df$author_order,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
  }

  author_names <- vapply(
    seq_len(nrow(authors_df)),
    function(i) {
      format_citation_person(
        first_name = authors_df$name_first[i],
        last_name = authors_df$name_last[i]
      )
    },
    character(1)
  )

  author_names <- author_names[
    !is.na(author_names) &
      nzchar(trimws(author_names))
  ]

  n_authors <- length(author_names)

  if (n_authors == 0L) {
    return("")
  }

  et_al_threshold <- suppressWarnings(
    as.integer(et_al_threshold[1])
  )

  if (
    is.na(et_al_threshold) ||
    et_al_threshold < 2L
  ) {
    et_al_threshold <- 11L
  }

  if (n_authors >= et_al_threshold) {
    return(
      paste0(
        author_names[1],
        " et al."
      )
    )
  }

  if (n_authors == 1L) {
    return(author_names[1])
  }

  if (n_authors == 2L) {
    return(
      paste(
        author_names,
        collapse = " & "
      )
    )
  }

  paste0(
    paste(
      author_names[
        seq_len(n_authors - 1L)
      ],
      collapse = ", "
    ),
    " & ",
    author_names[n_authors]
  )
}

generate_citation <- function(
  vals,
  authors_df,
  org_df,
  style = "organisation"
) {
  if (is.null(vals)) {
    vals <- list()
  }

  # Citation-author style ----
  citation_style <- citation_text_value(
    style,
    default = "organisation"
  )

  citation_style <- tolower(
    citation_style
  )

  # Accept older or alternative stored values.
  if (
    citation_style %in%
      c(
        "authors",
        "person",
        "personal"
      )
  ) {
    citation_style <- "author"
  }

  if (
    !citation_style %in%
      c(
        "organisation",
        "author"
      )
  ) {
    citation_style <- "organisation"
  }

  # Year ----
  citation_date <- citation_text_value(
    vals$citation_date,
    default = ""
  )

  year <- if (nzchar(citation_date)) {
    tryCatch(
      format(
        as.Date(citation_date),
        "%Y"
      ),
      error = function(e) {
        "n.d."
      }
    )
  } else {
    "n.d."
  }

  if (
    is.na(year) ||
    !nzchar(year)
  ) {
    year <- "n.d."
  }

  # Title and version ----
  title <- citation_text_value(
    vals$title,
    default = "Untitled dataset"
  )

  version <- citation_text_value(
    vals$version,
    default = ""
  )

  version_text <- if (nzchar(version)) {
    paste0(
      " (Version ",
      version,
      ")"
    )
  } else {
    ""
  }

  # Organisations ----
  organisation_names <- character()

  if (
    !is.null(org_df) &&
    nrow(org_df) > 0L
  ) {
    org_df <- as.data.frame(
      org_df,
      stringsAsFactors = FALSE
    )

    organisation_names <- if (
      "short" %in% names(org_df)
    ) {
      org_df$short
    } else if (
      "organisation_name_short" %in%
        names(org_df)
    ) {
      org_df$organisation_name_short
    } else if (
      "name" %in% names(org_df)
    ) {
      org_df$name
    } else if (
      "organisation_name" %in%
        names(org_df)
    ) {
      org_df$organisation_name
    } else {
      character()
    }

    organisation_names <- trimws(
      as.character(
        organisation_names
      )
    )

    organisation_names <-
      organisation_names[
        !is.na(organisation_names) &
          nzchar(organisation_names)
      ]

    organisation_names <- unique(
      organisation_names
    )
  }

  organisation_text <- if (
    length(organisation_names) == 0L
  ) {
    ""
  } else if (
    length(organisation_names) <= 2L
  ) {
    paste(
      organisation_names,
      collapse = " & "
    )
  } else {
    paste0(
      organisation_names[1],
      " et al."
    )
  }

  # Personal authors ----
  authors_text <- format_citation_authors(
    authors_df
  )

  # Main citation creator ----
  main_entity <- if (
    identical(
      citation_style,
      "organisation"
    )
  ) {
    if (nzchar(organisation_text)) {
      organisation_text
    } else {
      authors_text
    }
  } else {
    if (nzchar(authors_text)) {
      authors_text
    } else {
      organisation_text
    }
  }

  if (!nzchar(main_entity)) {
    main_entity <- "Unknown creator"
  }

  # Build citation ----
  citation <- paste0(
    main_entity,
    " (",
    year,
    "). ",
    title,
    version_text,
    "."
  )

  # Primary organisation address ----
  if (
    !is.null(org_df) &&
    nrow(org_df) > 0L &&
    "address" %in% names(org_df)
  ) {
    primary_address <- citation_text_value(
      org_df$address,
      default = ""
    )

    if (nzchar(primary_address)) {
      citation <- paste0(
        citation,
        " ",
        primary_address,
        "."
      )
    }
  }

  citation
}

# make a citation key ----
generate_citation_key <- function(vals,
                                  org_df,
                                  existing_key = NULL,
                                  stable_rand_part = NULL,
                                  regenerate_random = FALSE) {
  if (is.null(vals)) {
    vals <- list()
  }

  # ORG ----
  org_part <- "org"

  if (!is.null(org_df) && nrow(org_df) > 0) {
    if ("code" %in% names(org_df)) {
      org_part <- first_non_empty(org_df$code, default = "org")
    } else if ("organisation_code" %in% names(org_df)) {
      org_part <- first_non_empty(org_df$organisation_code, default = "org")
    } else if ("name" %in% names(org_df)) {
      org_part <- first_non_empty(org_df$name, default = "org")
    }
  }

  org_part <- clean_key_part(org_part, default = "org")

  # DATE ----
  date_part <- "000000"

  citation_date <- first_non_empty(vals$citation_date, default = NULL)

  if (!is.null(citation_date)) {
    date_part <- tryCatch(
      format(as.Date(citation_date), "%Y%m"),
      error = function(e) "000000"
    )

    if (is.na(date_part) || date_part == "") {
      date_part <- "000000"
    }
  }

  # OBJECT KEYWORD ----
  object_part <- extract_primary_keyword(vals$keyword_object)

  object_part <- clean_key_part(
    object_part,
    default = "data"
  )

  # TITLE INITIALS ----
  title_value <- first_non_empty(vals$title, default = "")

  title_part <- title_to_initials(title_value)

  title_part <- clean_key_part(
    title_part,
    default = "xxx"
  )

  # RANDOM ----
  rand_part <- NULL

  if (!isTRUE(regenerate_random)) {
    rand_part <- stable_rand_part %||%
      extract_rand_part_from_citation_key(existing_key)
  }

  rand_part <- first_non_empty(rand_part, default = NULL)

  if (is.null(rand_part)) {
    rand_part <- generate_rand_part()
  }

  rand_part <- tolower(gsub("[^a-z0-9]", "", rand_part))
  rand_part <- substr(rand_part, 1, 4)

  if (is.na(rand_part) || nchar(rand_part) < 4) {
    rand_part <- generate_rand_part()
  }

  # VERSION ----
  version_part <- "v0_1_0"

  version_value <- first_non_empty(vals$version, default = NULL)

  if (!is.null(version_value)) {
    version_clean <- gsub("\\.", "_", version_value)
    version_clean <- gsub("[^A-Za-z0-9_]", "", version_clean)

    if (!is.na(version_clean) && version_clean != "") {
      version_part <- paste0("v", version_clean)
    }
  }

  paste(
    org_part,
    date_part,
    object_part,
    title_part,
    rand_part,
    version_part,
    sep = "_"
  )
}

# dataset folder keys ----

folder_key_text <- function(
  x,
  default = NULL
) {
  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(default)
  }

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(default)
  }

  x <- trimws(
    as.character(x)
  )

  x <- x[nzchar(x)]

  if (length(x) == 0) {
    return(default)
  }

  x[1]
}


folder_key_slug <- function(
  x,
  default = "data",
  max_length = 48L
) {
  x <- folder_key_text(
    x,
    default = default
  )

  if (is.null(x)) {
    x <- default
  }

  x <- tolower(
    as.character(x)
  )

  # Preserve word boundaries as underscores.
  x <- gsub(
    "[^a-z0-9]+",
    "_",
    x
  )

  x <- gsub(
    "_+",
    "_",
    x
  )

  x <- gsub(
    "^_|_$",
    "",
    x
  )

  if (!nzchar(x)) {
    x <- default
  }

  x <- substr(
    x,
    1L,
    as.integer(max_length)
  )

  x <- gsub(
    "_+$",
    "",
    x
  )

  if (!nzchar(x)) {
    default
  } else {
    x
  }
}


first_folder_keyword <- function(x) {
  x <- folder_key_text(
    x,
    default = NULL
  )

  if (is.null(x)) {
    return("data")
  }

  # Supports selectize vectors and semicolon/comma strings.
  parts <- unlist(
    strsplit(
      x,
      "[;,]"
    )
  )

  if (length(parts) == 0) {
    return("data")
  }

  folder_key_slug(
    parts[1],
    default = "data"
  )
}


generate_folder_key <- function(
  vals,
  org_df
) {
  if (is.null(vals)) {
    vals <- list()
  }

  # FIRST OBJECT KEYWORD ----

  keyword_part <- first_folder_keyword(
    vals$keyword_object
  )

  # CITATION YEAR ----

  year_part <- "0000"

  citation_date <- folder_key_text(
    vals$citation_date,
    default = NULL
  )

  if (!is.null(citation_date)) {
    year_part <- tryCatch(
      format(
        as.Date(citation_date),
        "%Y"
      ),
      error = function(e) {
        "0000"
      }
    )

    if (
      is.na(year_part) ||
      !nzchar(year_part)
    ) {
      year_part <- "0000"
    }
  }

  # ORGANISATION CODE ----

  org_value <- "org"

  if (
    !is.null(org_df) &&
    nrow(org_df) > 0
  ) {
    if ("code" %in% names(org_df)) {
      org_value <- folder_key_text(
        org_df$code,
        default = "org"
      )
    } else if (
      "organisation_code" %in% names(org_df)
    ) {
      org_value <- folder_key_text(
        org_df$organisation_code,
        default = "org"
      )
    } else if (
      "organisation_name_short" %in%
        names(org_df)
    ) {
      org_value <- folder_key_text(
        org_df$organisation_name_short,
        default = "org"
      )
    } else if (
      "short" %in% names(org_df)
    ) {
      org_value <- folder_key_text(
        org_df$short,
        default = "org"
      )
    } else if (
      "name" %in% names(org_df)
    ) {
      org_value <- folder_key_text(
        org_df$name,
        default = "org"
      )
    }
  }

  org_part <- folder_key_slug(
    org_value,
    default = "org",
    max_length = 24L
  )

  paste(
    keyword_part,
    year_part,
    org_part,
    sep = "_"
  )
}


extract_folder_key_from_folder_name <- function(
  dataset_path
) {
  dataset_path <- folder_key_text(
    dataset_path,
    default = NULL
  )

  if (is.null(dataset_path)) {
    return(NULL)
  }

  folder <- basename(
    normalizePath(
      dataset_path,
      mustWork = FALSE,
      winslash = "/"
    )
  )

  if (
    !grepl(
      "__",
      folder,
      fixed = TRUE
    )
  ) {
    return(NULL)
  }

  sub(
    "__.*$",
    "",
    folder
  )
}

first_non_empty <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(default)
  }

  x <- as.character(x)
  x <- x[trimws(x) != ""]

  if (length(x) == 0) {
    return(default)
  }

  x[1]
}

clean_key_part <- function(x, default) {
  x <- first_non_empty(x, default = default)

  if (is.null(x) || is.na(x) || trimws(x) == "") {
    x <- default
  }

  x <- tolower(gsub("[^A-Za-z0-9]", "", as.character(x)))

  if (is.na(x) || x == "") {
    x <- default
  }

  x
}

# function to create folder structure in the data pond
ensure_pond_structure <- function(base_path) {
  dirs <- c("dev", "src", "arc")
  for (d in dirs) {
    path <- file.path(base_path, d)
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
  TRUE
}

# version conventions ----
# function to validate the version number
validate_version <- function(version_value) {
  if (is.null(version_value) ||
      length(version_value) == 0 ||
      is.na(version_value)) {
    return(FALSE)
  }
  version_value <- trimws(as.character(version_value[1]))
  grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version_value)
}

parse_dataset_version <- function(x) {
  default <- list(major = 0L, minor = 1L, patch = 0L)
  if (is.null(x) ||
      length(x) == 0 ||
      is.na(x) ||
      trimws(x) == "") {
    return(default)
  }
  x <- trimws(as.character(x[1]))
  parts <- strsplit(x, "\\.")[[1]]
  if (length(parts) != 3 ||
      any(!grepl("^[0-9]+$", parts))) {
    return(default)
  }
  list(
    major = as.integer(parts[1]),
    minor = as.integer(parts[2]),
    patch = as.integer(parts[3])
  )
}

make_dataset_version <- function(major, minor, patch) {
  paste(as.integer(major), as.integer(minor), as.integer(patch), sep = ".")
}

set_version_inputs <- function(session, version_value) {
  v <- parse_dataset_version(version_value)
  updateSliderInput(session, "version_major", value = v$major)
  updateSliderInput(session, "version_minor", value = v$minor)
  updateSliderInput(session, "version_patch", value = v$patch)
  updateTextInput(
    session,
    "version",
    value = make_dataset_version(v$major, v$minor, v$patch)
  )
}

# keywords ----
# function to extract first keyword in a string of keywords
# separated by ';' or ','.
extract_primary_keyword <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  x <- as.character(x)
  x <- x[trimws(x) != ""]
  if (length(x) == 0) {
    return(NULL)
  }
  # If multiple values are supplied by selectizeInput, use the first.
  x <- x[1]
  parts <- unlist(strsplit(x, "[;,]"))
  if (length(parts) == 0) {
    return(NULL)
  }
  kw <- trimws(parts[1])
  if (is.na(kw) || kw == "") {
    return(NULL)
  }
  kw <- tolower(gsub("[^A-Za-z0-9]", "", kw))
  if (is.na(kw) || kw == "") {
    return(NULL)
  }
  kw
}

load_vocab <- function(path) {
  if (!file.exists(path)) {
    warning("Vocab file not found: ", path)
    # return safe empty structure
    return(data.table(
      field_code = character(),
      option_value = character(),
      option_label = character()
    ))
  }
  dt <- as.data.table(fread(path, header = TRUE))
  # basic cleaning
  dt <- dt[!is.na(field_code) & field_code != ""]
  # ensure expected columns
  required_cols <- c("field_code", "option_value", "option_label")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    warning(
      "Missing columns in vocab file: ",
      paste(missing_cols, collapse = ", ")
    )
    return(data.table(
      field_code = character(),
      option_value = character(),
      option_label = character()
    ))
  }
  # sort if present
  if ("sort_order" %in% names(dt)) {
    setorder(dt, field_code, sort_order)
  }
  dt
}

# custom function for loading organisations
load_organisations_csv <- function(path) {
  if (!file.exists(path)) {
    warning("Organisation CSV not found: ", path)
    return(data.table())
  }
  dt <- as.data.table(fread(path))
  required <- c("full", "short", "code", "address")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop("Missing columns in organisation CSV: ", paste(missing, collapse = ", "))
  }
  dt
}

generate_unique_dataset_uuid <- function(
  base_path,
  suffix_length = 6L,
  max_attempts = 20L
) {
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

  date_part <- format(
    Sys.Date(),
    "%Y%m%d"
  )

  for (
    attempt in seq_len(
      max_attempts
    )
  ) {
    random_part <- paste0(
      sample(
        alphabet,
        size = suffix_length,
        replace = TRUE
      ),
      collapse = ""
    )

    candidate <- paste(
      date_part,
      random_part,
      sep = "_"
    )

    if (
      !dataset_uuid_exists(
        uuid = candidate,
        base_path = base_path
      )
    ) {
      return(candidate)
    }
  }

  stop(
    paste(
      "Unable to generate a unique dataset UUID after",
      max_attempts,
      "attempts."
    )
  )
}

safe_path_slug <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
    return("dataset")
  }
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (x == "") {
    x <- "dataset"
  }
  x
}

create_dataset_folder_structure <- function(
  base_path,
  folder_key = NULL,
  uuid = NULL,
  parent_dir = "dev",
  subdirs = c(
    "data",
    "docs",
    "scripts"
  ),
  citation_key = NULL
) {
  base_path <- folder_key_text(
    base_path,
    default = NULL
  )

  if (is.null(base_path)) {
    stop("base_path is required.")
  }

  # Temporary compatibility with any older named calls.
  # New calls should provide folder_key.
  folder_key <- folder_key_text(
    folder_key,
    default = folder_key_text(
      citation_key,
      default = NULL
    )
  )

  if (is.null(folder_key)) {
    stop("folder_key is required.")
  }

  uuid <- folder_key_text(
    uuid,
    default = NULL
  )

  if (is.null(uuid)) {
    stop("uuid is required.")
  }

  parent_path <- file.path(
    base_path,
    parent_dir
  )

  if (!dir.exists(parent_path)) {
    dir.create(
      parent_path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  folder_name <- build_dataset_folder_name(
    folder_key = folder_key,
    uuid = uuid
  )

  dataset_path <- file.path(
    parent_path,
    folder_name
  )

  if (
    dir.exists(dataset_path) ||
    file.exists(dataset_path)
  ) {
    stop(
      "Dataset folder already exists: ",
      dataset_path
    )
  }

  dir.create(
    dataset_path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  for (d in subdirs) {
    dir.create(
      file.path(
        dataset_path,
        d
      ),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  list(
    folder_key = folder_key,
    folder_name = folder_name,

    dataset_path = normalizePath(
      dataset_path,
      mustWork = FALSE,
      winslash = "/"
    ),

    data_path = normalizePath(
      file.path(
        dataset_path,
        "data"
      ),
      mustWork = FALSE,
      winslash = "/"
    ),

    docs_path = normalizePath(
      file.path(
        dataset_path,
        "docs"
      ),
      mustWork = FALSE,
      winslash = "/"
    ),

    scripts_path = normalizePath(
      file.path(
        dataset_path,
        "scripts"
      ),
      mustWork = FALSE,
      winslash = "/"
    )
  )
}

open_folder <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || path == "") {
    stop("No folder path is available.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) {
    stop("Folder does not exist: ", path)
  }
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Windows")) {
    shell.exec(path)
  } else if (identical(sys, "Darwin")) {
    system2("open", shQuote(path), wait = FALSE)
  } else {
    system2("xdg-open", shQuote(path), wait = FALSE)
  }
  invisible(TRUE)
}

get_dataset_record <- function(dataset_id, base_path) {
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(NULL)
  }
  conn <- get_conn(base_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  df <- dbGetQuery(
    conn,
    "SELECT * FROM entry_tbl WHERE id = ?",
    params = list(dataset_id)
  )
  if (nrow(df) == 0) {
    return(NULL)
  }
  as.list(df[1, , drop = FALSE])
}

validate_temporal_coverage <- function(
  start_date,
  end_date
) {
  normalise_optional_date <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0L ||
      all(is.na(x))
    ) {
      return(NULL)
    }

    value <- trimws(
      as.character(
        x[1]
      )
    )

    if (!nzchar(value)) {
      return(NULL)
    }

    parsed <- suppressWarnings(
      as.Date(
        value,
        format = "%Y-%m-%d"
      )
    )

    if (is.na(parsed)) {
      return(NULL)
    }

    parsed
  }

  start <- normalise_optional_date(
    start_date
  )

  end <- normalise_optional_date(
    end_date
  )

  # Either or both dates may legitimately be blank.
  if (
    is.null(start) ||
    is.null(end)
  ) {
    return(
      list(
        valid = TRUE,
        message = ""
      )
    )
  }

  if (start > end) {
    return(
      list(
        valid = FALSE,
        message = paste(
          "Temporal coverage start date cannot be",
          "later than the temporal coverage end date."
        )
      )
    )
  }

  list(
    valid = TRUE,
    message = ""
  )
}
