## data file scanner ----

empty_inventory_scan <- function(message = "No file scan has been run.") {
  data.table::data.table(message = message)
}


map_inventory_to_app <- function(inv_dt, data_dir = NA_character_) {
  if (is.null(inv_dt) || nrow(inv_dt) == 0) {
    return(data.table::data.table(
      message = "No files detected."
    ))
  }

  if ("message" %in% names(inv_dt)) {
    return(data.table::as.data.table(inv_dt))
  }

  dt <- data.table::as.data.table(data.table::copy(inv_dt))

  defaults <- list(
    file_name = NA_character_,
    data_file_rel_path_full = NA_character_,
    file_path = NA_character_,
    data_file_extension = NA_character_,
    data_file_format = NA_character_,
    file_role = NA_character_,
    is_container = FALSE,
    container_type = NA_character_,
    file_size_kb = NA_real_,
    modified_time = NA_character_,
    file_stem = NA_character_,
    file_dir = NA_character_,
    file_key = NA_character_,
    primary_dataset = FALSE,
    group_family = NA_character_,
    group_key = NA_character_,
    sidecar_count = 0L,
    sidecar_exts = "",
    sidecar_summary = "None",
    logical_file_name = NA_character_,
    logical_file_path = NA_character_,
    logical_rel_path = NA_character_,
    logical_dir = NA_character_,
    logical_stem = NA_character_,
    logical_extension = NA_character_,
    logical_format = NA_character_,
    logical_file_role = NA_character_,
    logical_size_bytes = NA_real_,
    logical_size_kb = NA_real_,
    logical_size_mb = NA_real_,
    n_components = NA_integer_,
    n_sidecars = NA_integer_,
    n_primary_files = NA_integer_,
    component_exts = NA_character_,
    component_roles = NA_character_,
    component_modtime_latest = NA_character_,
    source_paths = NA_character_,
    logical_primary_reason = NA_character_
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(dt)) {
      dt[, (nm) := defaults[[nm]]]
    }
  }

  # Physical file size for display.
  if (!"file_size_kb" %in% names(dt) || all(is.na(dt$file_size_kb))) {
    if ("size" %in% names(dt)) {
      dt[, file_size_kb := round(as.numeric(size) / 1024, 1)]
    }
  }

  # Modified time for display / database compatibility.
  if (!"modified_time" %in% names(dt) || all(is.na(dt$modified_time))) {
    if ("modtime" %in% names(dt)) {
      dt[, modified_time := as.character(modtime)]
    }
  }

  # Logical size fields.
  if ("logical_size_bytes" %in% names(dt)) {
    dt[, logical_size_kb := round(as.numeric(logical_size_bytes) / 1024, 1)]
    dt[, logical_size_mb := round(as.numeric(logical_size_bytes) / 1024^2, 3)]
  }

  dt[, primary_dataset := primary_dataset %in% TRUE]
  dt[, is_container := is_container %in% TRUE]

  dt[, data_file_rel_path_save := data.table::fifelse(
    !is.na(logical_rel_path) & logical_rel_path != "",
    logical_rel_path,
    data_file_rel_path_full
  )]

  dt[, file_path_save := data.table::fifelse(
    !is.na(logical_file_path) & logical_file_path != "",
    logical_file_path,
    file_path
  )]

  dt[, file_name_save := data.table::fifelse(
    !is.na(logical_file_name) & logical_file_name != "",
    logical_file_name,
    file_name
  )]

  dt[]
}

default_inventory_excluded_dir_names <- function() {
  c(
    "temp",
    "tmp",
    "scratch",
    ".cache",
    "__pycache__",
    ".git"
  )
}


normalise_inventory_excluded_dir_names <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0L
  ) {
    return(character())
  }

  x <- tolower(
    trimws(
      as.character(x)
    )
  )

  unique(
    x[
      !is.na(x) &
        nzchar(x)
    ]
  )
}


normalise_inventory_path <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    is.na(x[1]) ||
    !nzchar(trimws(as.character(x[1])))
  ) {
    return(NA_character_)
  }

  normalizePath(
    path.expand(
      as.character(x[1])
    ),
    winslash = "/",
    mustWork = FALSE
  )
}


inventory_relative_path <- function(
  path,
  base_dir
) {
  base_dir <- normalise_inventory_path(
    base_dir
  )

  if (is.na(base_dir)) {
    return(
      rep(
        NA_character_,
        length(path)
      )
    )
  }

  base_dir <- sub(
    "/+$",
    "",
    base_dir
  )

  base_prefix <- paste0(
    base_dir,
    "/"
  )

  vapply(
    path,
    function(path_value) {
      path_value <- normalise_inventory_path(
        path_value
      )

      if (is.na(path_value)) {
        return(NA_character_)
      }

      if (identical(
        path_value,
        base_dir
      )) {
        return("")
      }

      if (!startsWith(
        path_value,
        base_prefix
      )) {
        return(NA_character_)
      }

      substring(
        path_value,
        nchar(base_prefix) + 1L
      )
    },
    character(1)
  )
}


inventory_path_dirname <- function(x) {
  x <- as.character(x)

  result <- dirname(x)

  result[
    is.na(x) |
      !nzchar(trimws(x)) |
      result == "."
  ] <- ""

  gsub(
    "\\\\",
    "/",
    result
  )
}


rebase_inventory_relative_paths <- function(
  inventory,
  relative_base_dir
) {
  if (
    is.null(inventory) ||
    nrow(inventory) == 0L ||
    "message" %in% names(inventory)
  ) {
    return(inventory)
  }

  dt <- data.table::as.data.table(
    data.table::copy(inventory)
  )

  relative_base_dir <- normalise_inventory_path(
    relative_base_dir
  )

  if (is.na(relative_base_dir)) {
    return(dt[])
  }

  if ("file_path" %in% names(dt)) {
    physical_relative <- inventory_relative_path(
      dt$file_path,
      relative_base_dir
    )

    usable <- (
      !is.na(physical_relative) &
        nzchar(physical_relative)
    )

    dt[
      usable,
      data_file_rel_path_full :=
        physical_relative[usable]
    ]
  }

  if ("logical_file_path" %in% names(dt)) {
    logical_relative <- inventory_relative_path(
      dt$logical_file_path,
      relative_base_dir
    )

    usable <- (
      !is.na(logical_relative) &
        nzchar(logical_relative)
    )

    dt[
      usable,
      logical_rel_path :=
        logical_relative[usable]
    ]
  }

  if (
    "data_file_rel_path_full" %in%
      names(dt)
  ) {
    dt[
      ,
      file_dir :=
        inventory_path_dirname(
          data_file_rel_path_full
        )
    ]
  }

  if ("logical_rel_path" %in% names(dt)) {
    dt[
      ,
      logical_dir :=
        inventory_path_dirname(
          logical_rel_path
        )
    ]
  }

  if (
    all(
      c(
        "logical_rel_path",
        "data_file_rel_path_full"
      ) %in% names(dt)
    )
  ) {
    dt[
      ,
      data_file_rel_path_save :=
        data.table::fifelse(
          !is.na(logical_rel_path) &
            nzchar(logical_rel_path),
          logical_rel_path,
          data_file_rel_path_full
        )
    ]
  }

  if (
    all(
      c(
        "logical_file_path",
        "file_path"
      ) %in% names(dt)
    )
  ) {
    dt[
      ,
      file_path_save :=
        data.table::fifelse(
          !is.na(logical_file_path) &
            nzchar(logical_file_path),
          logical_file_path,
          file_path
        )
    ]
  }

  if (
    all(
      c(
        "logical_file_name",
        "file_name"
      ) %in% names(dt)
    )
  ) {
    dt[
      ,
      file_name_save :=
        data.table::fifelse(
          !is.na(logical_file_name) &
            nzchar(logical_file_name),
          logical_file_name,
          file_name
        )
    ]
  }

  dt[]
}


inventory_excluded_directory_mask <- function(
  inventory,
  excluded_dir_names =
    default_inventory_excluded_dir_names()
) {
  if (
    is.null(inventory) ||
    nrow(inventory) == 0L ||
    "message" %in% names(inventory)
  ) {
    return(
      rep(
        FALSE,
        if (is.null(inventory)) {
          0L
        } else {
          nrow(inventory)
        }
      )
    )
  }

  excluded_dir_names <-
    normalise_inventory_excluded_dir_names(
      excluded_dir_names
    )

  if (length(excluded_dir_names) == 0L) {
    return(
      rep(
        FALSE,
        nrow(inventory)
      )
    )
  }

  dt <- data.table::as.data.table(
    inventory
  )

  relative_paths <- if (
    "data_file_rel_path_full" %in%
      names(dt)
  ) {
    as.character(
      dt$data_file_rel_path_full
    )
  } else {
    rep(
      NA_character_,
      nrow(dt)
    )
  }

  absolute_paths <- if (
    "file_path" %in% names(dt)
  ) {
    as.character(
      dt$file_path
    )
  } else {
    rep(
      NA_character_,
      nrow(dt)
    )
  }

  path_is_directory <- vapply(
    absolute_paths,
    function(path_value) {
      if (
        is.na(path_value) ||
        !nzchar(trimws(path_value))
      ) {
        return(FALSE)
      }

      dir.exists(path_value)
    },
    logical(1)
  )

  vapply(
    seq_len(nrow(dt)),
    function(i) {
      relative_path <- relative_paths[i]

      if (
        is.na(relative_path) ||
        !nzchar(trimws(relative_path))
      ) {
        return(FALSE)
      }

      relative_path <- gsub(
        "\\\\",
        "/",
        relative_path
      )

      relative_path <- sub(
        "^\\./",
        "",
        relative_path
      )

      relative_path <- sub(
        "/+$",
        "",
        relative_path
      )

      segments <- strsplit(
        relative_path,
        "/+",
        perl = TRUE
      )[[1]]

      segments <- segments[
        !is.na(segments) &
          nzchar(segments) &
          segments != "."
      ]

      if (length(segments) == 0L) {
        return(FALSE)
      }

      # For normal files, the final segment is the filename and
      # must not be compared with excluded directory names.
      #
      # For directory-backed containers, the final segment is
      # itself a directory and must be checked.
      if (
        !isTRUE(path_is_directory[i]) &&
        length(segments) > 0L
      ) {
        segments <- head(
          segments,
          -1L
        )
      }

      if (length(segments) == 0L) {
        return(FALSE)
      }

      any(
        tolower(segments) %in%
          excluded_dir_names
      )
    },
    logical(1)
  )
}


attach_inventory_scan_metadata <- function(
  inventory,
  scan_root,
  relative_base_dir,
  total_rows_before_filter,
  ignored_rows,
  ignored_files,
  excluded_dir_names
) {
  attr(
    inventory,
    "scan_root"
  ) <- normalise_inventory_path(
    scan_root
  )

  attr(
    inventory,
    "relative_base_dir"
  ) <- normalise_inventory_path(
    relative_base_dir
  )

  attr(
    inventory,
    "total_rows_before_filter"
  ) <- as.integer(
    total_rows_before_filter
  )

  attr(
    inventory,
    "ignored_rows"
  ) <- as.integer(
    ignored_rows
  )

  attr(
    inventory,
    "ignored_files"
  ) <- as.integer(
    ignored_files
  )

  attr(
    inventory,
    "excluded_dir_names"
  ) <- normalise_inventory_excluded_dir_names(
    excluded_dir_names
  )

  inventory
}

scan_dataset_files <- function(
  data_dir,
  excluded_dir_names =
    default_inventory_excluded_dir_names(),
  relative_base_dir = data_dir
) {
  if (
    is.null(data_dir) ||
    length(data_dir) == 0L ||
    is.na(data_dir[1]) ||
    !nzchar(trimws(as.character(data_dir[1])))
  ) {
    return(
      attach_inventory_scan_metadata(
        inventory = empty_inventory_scan(
          "No valid scan folder was found."
        ),
        scan_root = NA_character_,
        relative_base_dir =
          relative_base_dir,
        total_rows_before_filter = 0L,
        ignored_rows = 0L,
        ignored_files = 0L,
        excluded_dir_names =
          excluded_dir_names
      )
    )
  }

  scan_root <- normalise_inventory_path(
    data_dir
  )

  relative_base_dir <-
    normalise_inventory_path(
      relative_base_dir
    )

  if (is.na(relative_base_dir)) {
    relative_base_dir <- scan_root
  }

  if (
    is.na(scan_root) ||
    !dir.exists(scan_root)
  ) {
    return(
      attach_inventory_scan_metadata(
        inventory = empty_inventory_scan(
          paste(
            "Expected scan folder does not exist:",
            scan_root
          )
        ),
        scan_root = scan_root,
        relative_base_dir =
          relative_base_dir,
        total_rows_before_filter = 0L,
        ignored_rows = 0L,
        ignored_files = 0L,
        excluded_dir_names =
          excluded_dir_names
      )
    )
  }

  if (
    !requireNamespace(
      "ekznwr",
      quietly = TRUE
    )
  ) {
    return(
      attach_inventory_scan_metadata(
        inventory = empty_inventory_scan(
          paste(
            "Package 'ekznwr' is required",
            "for inventory scanning."
          )
        ),
        scan_root = scan_root,
        relative_base_dir =
          relative_base_dir,
        total_rows_before_filter = 0L,
        ignored_rows = 0L,
        ignored_files = 0L,
        excluded_dir_names =
          excluded_dir_names
      )
    )
  }

  raw_inventory <- tryCatch(
    {
      ekznwr::inventory_scan_local(
        scan_root,
        enrich = TRUE
      )
    },
    error = function(e) {
      data.table::data.table(
        message = paste(
          "Inventory scan failed:",
          conditionMessage(e)
        )
      )
    }
  )

  inventory <- map_inventory_to_app(
    raw_inventory,
    data_dir = scan_root
  )

  if ("message" %in% names(inventory)) {
    return(
      attach_inventory_scan_metadata(
        inventory = inventory,
        scan_root = scan_root,
        relative_base_dir =
          relative_base_dir,
        total_rows_before_filter = 0L,
        ignored_rows = 0L,
        ignored_files = 0L,
        excluded_dir_names =
          excluded_dir_names
      )
    )
  }

  # This is important when the actual scan root is
  # data/outputs but database paths must remain relative
  # to the canonical data directory.
  inventory <-
    rebase_inventory_relative_paths(
      inventory = inventory,
      relative_base_dir =
        relative_base_dir
    )

  total_rows_before_filter <- nrow(
    inventory
  )

  excluded_mask <-
    inventory_excluded_directory_mask(
      inventory = inventory,
      excluded_dir_names =
        excluded_dir_names
    )

  ignored_rows <- sum(
    excluded_mask,
    na.rm = TRUE
  )

  ignored_files <- if (
    ignored_rows == 0L
  ) {
    0L
  } else if (
    "file_path" %in% names(inventory)
  ) {
    length(
      unique(
        inventory$file_path[
          excluded_mask &
            !is.na(inventory$file_path) &
            nzchar(inventory$file_path)
        ]
      )
    )
  } else {
    ignored_rows
  }

  inventory <- inventory[
    !excluded_mask
  ]

  if (nrow(inventory) == 0L) {
    inventory <- empty_inventory_scan(
      paste(
        "No files remained after applying",
        "the transient-directory exclusions."
      )
    )
  }

  attach_inventory_scan_metadata(
    inventory = inventory,
    scan_root = scan_root,
    relative_base_dir =
      relative_base_dir,
    total_rows_before_filter =
      total_rows_before_filter,
    ignored_rows = ignored_rows,
    ignored_files = ignored_files,
    excluded_dir_names =
      excluded_dir_names
  )
}

## end of new scanner ----

# function to check that dataset folder has a folder called
# data inside
validate_data_structure <- function(path) {
  required_dirs <- c("data")
  missing <- required_dirs[
    !file.exists(file.path(path, required_dirs))
  ]
  if (length(missing) > 0) {
    return(FALSE)
  }
  TRUE
}

resolve_data_dir <- function(path) {
  if (is.null(path) || length(path) == 0 || path == "") {
    return(NULL)
  }
  path <- path.expand(path)
  # If the user selected the data folder itself, use it directly
  if (basename(normalizePath(path, mustWork = FALSE)) == "data") {
    return(path)
  }
  # Otherwise assume the user selected the dataset root
  file.path(path, "data")
}

# supporting documentation scanner ----
supporting_document_exts <- c(
  # Text and markdown-style documentation
  "txt",
  "md",
  "qmd",
  "rmd",

  # Web / metadata / structured text
  "html",
  "htm",
  "xml",

  # PDF and word-processing documents
  "pdf",
  "docx",
  "doc",
  "odt",

  # Spreadsheets often used as data dictionaries or metadata tables
  "xlsx",
  "xls",
  "ods",
  "csv",

  # Presentations sometimes used as project/method documentation
  "pptx",
  "ppt",
  "odp",

  # Notebooks and scripts
  "ipynb",
  "r",
  "py",
  "sh",
  "pbs",
  "ave",
  "aml"
)

list_supporting_document_files <- function(dataset_path) {
  if (is.null(dataset_path) ||
      length(dataset_path) == 0 ||
      is.na(dataset_path) ||
      dataset_path == "" ||
      !dir.exists(dataset_path)) {
    return(data.table(
      message = "No valid dataset folder selected."
    ))
  }
  dataset_path <- normalizePath(
    dataset_path,
    mustWork = TRUE,
    winslash = "/"
  )
  all_files <- list.files(
    dataset_path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  if (length(all_files) == 0) {
    return(data.table(
      message = "Dataset folder contains no files."
    ))
  }
  all_files <- normalizePath(
    all_files,
    mustWork = FALSE,
    winslash = "/"
  )
  rel_path <- sub(
    paste0("^", gsub("([\\W])", "\\\\\\1", dataset_path), "/?"),
    "",
    all_files
  )
  in_data <- grepl("^data/", rel_path, ignore.case = TRUE)
  ext <- tolower(tools::file_ext(all_files))
  keep <- !in_data & ext %in% supporting_document_exts
  if (!any(keep)) {
    return(data.table(
      message = "No supporting documents detected outside the data folder."
    ))
  }
  dt <- data.table(
    document_name = basename(all_files[keep]),
    document_rel_path = rel_path[keep],
    document_path = all_files[keep],
    document_extension = ext[keep]
  )
  dt[, document_dir := dirname(document_rel_path)]
  dt[is.na(document_dir) | document_dir == ".", document_dir := ""]
  dt[]
}

classify_supporting_document <- function(document_name, document_rel_path, document_extension) {
  f <- tolower(document_name)
  p <- tolower(document_rel_path)
  ext <- tolower(document_extension)
  out <- list(
    code = "other",
    label = "Supporting document",
    auto = "Detected as supporting document; review recommended."
  )
  if (grepl("lineage|process|workflow|provenance", f) ||
      grepl("lineage|process|workflow|provenance", p)) {
    out$code <- "lineage"
    out$label <- "Lineage document"
    out$auto <- "Detected as lineage document from filename or path."
    return(out)
  }
  if (grepl("accuracy|validation|qa|qc", f) ||
      grepl("accuracy|validation|qa|qc", p)) {
    out$code <- "accuracy_report"
    out$label <- "Accuracy report"
    out$auto <- "Detected as accuracy report from filename or path."
    return(out)
  }
  if (grepl("technical|report", f) ||
      grepl("technical|report", p)) {
    out$code <- "technical_report"
    out$label <- "Technical report"
    out$auto <- "Detected as technical report from filename or path."
    return(out)
  }
  if (grepl("thesis|dissertation", f) ||
      grepl("thesis|dissertation", p)) {
    out$code <- "thesis"
    out$label <- "Thesis"
    out$auto <- "Detected as thesis from filename or path."
    return(out)
  }
  if (grepl("article|paper|manuscript|journal", f) ||
      grepl("article|paper|manuscript|journal", p)) {
    out$code <- "article"
    out$label <- "Article"
    out$auto <- "Detected as article from filename or path."
    return(out)
  }
  if (grepl("dictionary|data_dictionary|fields", f) ||
      grepl("dictionary|data_dictionary|fields", p)) {
    out$code <- "data_dictionary"
    out$label <- "Data dictionary"
    out$auto <- "Detected as data dictionary from filename or path."
    return(out)
  }
  if (grepl("method|methods|protocol", f) ||
      grepl("method|methods|protocol", p)) {
    out$code <- "method_statement"
    out$label <- "Method statement"
    out$auto <- "Detected as method statement from filename or path."
    return(out)
  }
  if (ext %in% c("qmd", "rmd", "ipynb")) {
    out$code <- "method_statement"
    out$label <- "Method statement"
    out$auto <- "Detected as method or narrative document from file type."
    return(out)
  }
  if (grepl("readme", f)) {
    out$code <- "metadata_note"
    out$label <- "Metadata note"
    out$auto <- "Detected as metadata note from README filename."
    return(out)
  }
  if (ext %in% c("r", "py", "sh", "bash", "pbs", "ave", "aml" )) {
    out$code <- "script"
    out$label <- "Script"
    out$auto <- paste(
      "Detected as a script",
      "from the file extension."
    )
    return(out)
  }
  if (ext %in% c("qmd", "rmd", "ipynb")) {
    out$code <- "method_statement"
    out$label <- "Method statement"
    out$auto <- paste(
      "Detected as a method or narrative",
      "document from the file extension."
    )
    return(out)
  }

  if (ext %in% c("xlsx", "xls", "ods", "csv")) {
    out$code <- "data_dictionary"
    out$label <- "Data dictionary"
    out$auto <- "Detected as spreadsheet/table document; review whether it is a data dictionary or supporting table."
    return(out)
  }

  if (ext %in% c("odt", "doc", "docx")) {
    out$code <- "other"
    out$label <- "Supporting document"
    out$auto <- "Detected as word-processing document; review document type."
    return(out)
  }

  if (ext %in% c("ppt", "pptx", "odp")) {
    out$code <- "other"
    out$label <- "Supporting document"
    out$auto <- "Detected as presentation document; review document type."
    return(out)
  }
  out
}