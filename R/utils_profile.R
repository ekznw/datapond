profile_vector_attributes <- function(path, layer = NULL, config = list(), format_label = NULL) {
  max_examples <- config$attribute_profile$max_example_values %||% 8L
  max_unique_scan <- config$attribute_profile$max_unique_count_scan %||% 10000L

  if (is.null(path) || length(path) == 0 || is.na(path) || path == "") {
    return(data.table::data.table(
      message = "No file path supplied for attribute profiling."
    ))
  }

  path <- normalizePath(path, mustWork = FALSE, winslash = "/")

  if (!file.exists(path)) {
    return(data.table::data.table(
      message = paste("Attribute profiling path does not exist:", path)
    ))
  }

  profile_one_layer <- function(path, layer_i = NULL) {
    obj <- NULL
    read_messages <- character()

    if (requireNamespace("terra", quietly = TRUE)) {
      obj <- tryCatch(
        {
          if (is.null(layer_i) || length(layer_i) == 0 || is.na(layer_i) || layer_i == "") {
            terra::vect(path)
          } else {
            terra::vect(path, layer = layer_i)
          }
        },
        error = function(e) {
          read_messages <<- c(read_messages, paste("terra:", conditionMessage(e)))
          NULL
        }
      )
    } else {
      read_messages <- c(read_messages, "terra package not available")
    }

    if (is.null(obj) && requireNamespace("sf", quietly = TRUE)) {
      obj <- tryCatch(
        {
          if (is.null(layer_i) || length(layer_i) == 0 || is.na(layer_i) || layer_i == "") {
            sf::st_read(path, quiet = TRUE)
          } else {
            sf::st_read(path, layer = layer_i, quiet = TRUE)
          }
        },
        error = function(e) {
          read_messages <<- c(read_messages, paste("sf:", conditionMessage(e)))
          NULL
        }
      )
    } else if (is.null(obj)) {
      read_messages <- c(read_messages, "sf package not available")
    }

    if (is.null(obj)) {
      return(data.table::data.table(
        message = paste(
          "Could not read vector attribute table.",
          paste(read_messages, collapse = " | ")
        )
      ))
    }

    df <- as.data.frame(obj)

    geom_cols <- names(df)[vapply(df, function(x) inherits(x, "sfc"), logical(1))]
    df <- df[, setdiff(names(df), geom_cols), drop = FALSE]

    terra_geom_cols <- intersect(names(df), c("geom", "geometry"))
    if (length(terra_geom_cols) > 0) {
      df <- df[, setdiff(names(df), terra_geom_cols), drop = FALSE]
    }

    if (ncol(df) == 0) {
      return(data.table::data.table(
        message = paste("No non-geometry attributes detected for layer:", layer_i %||% "")
      ))
    }

    out <- lapply(seq_along(df), function(i) {
      x <- df[[i]]
      non_missing <- x[!is.na(x)]

      unique_preview <- unique(as.character(head(non_missing, max_unique_scan)))
      examples <- head(unique_preview, max_examples)

      data.table::data.table(
        layer_name = layer_i %||% NA_character_,
        data_field_name = names(df)[i],
        data_field_alias = names(df)[i],
        attribute_type = class(x)[1],
        attribute_order = i,
        example_values = paste(examples, collapse = " | "),
        value_count = length(x),
        missing_count = sum(is.na(x)),
        unique_count = length(unique_preview),
        min_value = if (is.numeric(x) && length(non_missing)) as.character(min(non_missing)) else NA_character_,
        max_value = if (is.numeric(x) && length(non_missing)) as.character(max(non_missing)) else NA_character_
      )
    })

    data.table::rbindlist(out, fill = TRUE)
  }

  if (identical(format_label, "esri_file_geodatabase") || grepl("\\.gdb$", path, ignore.case = TRUE)) {
    layers <- character()

    if (requireNamespace("sf", quietly = TRUE)) {
      layers <- tryCatch(
        as.character(sf::st_layers(path)$name),
        error = function(e) character()
      )
    }

    if (length(layers) > 0) {
      out <- data.table::rbindlist(
        lapply(layers, function(layer_i) {
          profile_one_layer(path, layer_i = layer_i)
        }),
        fill = TRUE
      )

      if ("message" %in% names(out) && !"data_field_name" %in% names(out)) {
        return(out)
      }

      return(out)
    }

    return(profile_one_layer(path, layer_i = NULL))
  }

  profile_one_layer(path, layer_i = layer)
}

profile_raster_attributes <- function(
  path,
  config = list(),
  format_label = NULL
) {
  max_examples <- suppressWarnings(
    as.integer(
      config$attribute_profile$max_example_values %||%
        8L
    )
  )

  if (
    is.na(max_examples) ||
    max_examples < 1L
  ) {
    max_examples <- 8L
  }

  if (
    is.null(path) ||
    length(path) == 0L ||
    is.na(path[1]) ||
    !nzchar(trimws(as.character(path[1])))
  ) {
    return(
      data.table::data.table(
        message = paste(
          "No file path was supplied for",
          "raster attribute profiling."
        )
      )
    )
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    return(
      data.table::data.table(
        message = paste(
          "The terra package is required for",
          "raster attribute profiling."
        )
      )
    )
  }

  path <- normalizePath(
    path,
    mustWork = FALSE,
    winslash = "/"
  )

  if (!file.exists(path)) {
    return(
      data.table::data.table(
        message = paste(
          "Raster profiling path does not exist:",
          path
        )
      )
    )
  }

  raster <- tryCatch(
    terra::rast(path),
    error = function(e) {
      structure(
        NULL,
        error_message = conditionMessage(e)
      )
    }
  )

  if (is.null(raster)) {
    error_message <- attr(
      raster,
      "error_message"
    )

    if (
      is.null(error_message) ||
      !nzchar(error_message)
    ) {
      error_message <- "Unknown raster read error."
    }

    return(
      data.table::data.table(
        message = paste(
          "Could not open raster for profiling:",
          error_message
        )
      )
    )
  }

  layer_count <- terra::nlyr(
    raster
  )

  if (
    is.na(layer_count) ||
    layer_count < 1L
  ) {
    return(
      data.table::data.table(
        message = "No raster layers were detected."
      )
    )
  }

  normalise_layer_vector <- function(
    values,
    default = NA_character_
  ) {
    values <- as.character(values)

    if (length(values) == 0L) {
      values <- rep(
        default,
        layer_count
      )
    }

    if (length(values) < layer_count) {
      values <- rep(
        values,
        length.out = layer_count
      )
    }

    values[
      seq_len(layer_count)
    ]
  }

  layer_names <- normalise_layer_vector(
    names(raster)
  )

  missing_names <- (
    is.na(layer_names) |
      !nzchar(trimws(layer_names))
  )

  layer_names[missing_names] <- paste0(
    "layer_",
    which(missing_names)
  )

  # The layer name is part of the database uniqueness key.
  layer_names <- make.unique(
    trimws(layer_names),
    sep = "_"
  )

  layer_units <- tryCatch(
    terra::units(raster),
    error = function(e) {
      rep(
        NA_character_,
        layer_count
      )
    }
  )

  layer_units <- normalise_layer_vector(
    layer_units
  )

  layer_units[
    is.na(layer_units) |
      !nzchar(trimws(layer_units))
  ] <- NA_character_

  storage_datatypes <- tryCatch(
    terra::datatype(
      raster,
      bylyr = TRUE
    ),
    error = function(e) {
      rep(
        NA_character_,
        layer_count
      )
    }
  )

  storage_datatypes <- normalise_layer_vector(
    storage_datatypes
  )

  categorical_flags <- tryCatch(
    as.logical(
      is.factor(raster)
    ),
    error = function(e) {
      rep(
        FALSE,
        layer_count
      )
    }
  )

  if (length(categorical_flags) < layer_count) {
    categorical_flags <- rep(
      categorical_flags,
      length.out = layer_count
    )
  }

  categorical_flags[is.na(categorical_flags)] <-
    FALSE

  range_stats <- tryCatch(
    terra::global(
      raster,
      fun = c(
        "min",
        "max"
      ),
      na.rm = TRUE
    ),
    error = function(e) {
      warning(
        paste(
          "Raster min/max calculation failed:",
          conditionMessage(e)
        )
      )

      NULL
    }
  )

  missing_stats <- tryCatch(
    terra::global(
      raster,
      fun = "isNA"
    ),
    error = function(e) {
      warning(
        paste(
          "Raster missing-cell calculation failed:",
          conditionMessage(e)
        )
      )

      NULL
    }
  )

  data_frame_value <- function(
    x,
    row_number,
    preferred_column
  ) {
    if (
      is.null(x) ||
      nrow(x) < row_number
    ) {
      return(NA_real_)
    }

    column_name <- intersect(
      preferred_column,
      names(x)
    )

    if (length(column_name) == 0L) {
      return(NA_real_)
    }

    suppressWarnings(
      as.numeric(
        x[[column_name[1]]][row_number]
      )
    )
  }

  format_profile_value <- function(x) {
    x <- suppressWarnings(
      as.numeric(x[1])
    )

    if (
      length(x) == 0L ||
      !is.finite(x)
    ) {
      return(NA_character_)
    }

    format(
      signif(
        x,
        digits = 12L
      ),
      scientific = FALSE,
      trim = TRUE,
      big.mark = ""
    )
  }

  category_profile <- function(
    layer,
    is_categorical
  ) {
    if (!isTRUE(is_categorical)) {
      return(
        list(
          unique_count = NA_real_,
          example_values = NA_character_
        )
      )
    }

    categories <- tryCatch(
      levels(layer),
      error = function(e) {
        NULL
      }
    )

    if (
      is.null(categories) ||
      length(categories) == 0L ||
      is.null(categories[[1]]) ||
      nrow(categories[[1]]) == 0L
    ) {
      return(
        list(
          unique_count = NA_real_,
          example_values = NA_character_
        )
      )
    }

    categories <- as.data.frame(
      categories[[1]],
      stringsAsFactors = FALSE
    )

    value_codes <- as.character(
      categories[[1]]
    )

    value_labels <- if (ncol(categories) >= 2L) {
      as.character(
        categories[[2]]
      )
    } else {
      value_codes
    }

    examples <- paste0(
      value_codes,
      " = ",
      value_labels
    )

    examples <- head(
      examples,
      max_examples
    )

    list(
      unique_count = as.numeric(
        nrow(categories)
      ),
      example_values = paste(
        examples,
        collapse = " | "
      )
    )
  }

  total_cells <- as.numeric(
    terra::ncell(raster)
  )

  profile_rows <- lapply(
    seq_len(layer_count),
    function(i) {
      layer_is_categorical <- isTRUE(
        categorical_flags[i]
      )

      datatype <- toupper(
        storage_datatypes[i] %||%
          ""
      )

      attribute_type <- if (
        layer_is_categorical
      ) {
        "factor"
      } else if (
        grepl(
          "^INT",
          datatype
        )
      ) {
        "integer"
      } else {
        "numeric"
      }

      categories <- category_profile(
        layer = raster[[i]],
        is_categorical =
          layer_is_categorical
      )

      minimum <- data_frame_value(
        range_stats,
        row_number = i,
        preferred_column = "min"
      )

      maximum <- data_frame_value(
        range_stats,
        row_number = i,
        preferred_column = "max"
      )

      missing_count <- data_frame_value(
        missing_stats,
        row_number = i,
        preferred_column = "isNA"
      )

      data.table::data.table(
        layer_name = layer_names[i],

        # Each raster layer contains one principal data field:
        # its cell value.
        data_field_name = "cell_value",

        data_field_alias = layer_names[i],

        data_field_units = layer_units[i],

        legend_field = as.integer(
          layer_is_categorical
        ),

        attribute_type = attribute_type,

        attribute_order = as.integer(i),

        example_values =
          categories$example_values,

        # Keep the same meaning as the vector profiler:
        # total observations, including missing values.
        value_count = total_cells,

        missing_count = missing_count,

        # Only populate this where the raster explicitly
        # carries categorical levels.
        unique_count =
          categories$unique_count,

        min_value =
          format_profile_value(minimum),

        max_value =
          format_profile_value(maximum)
      )
    }
  )

  data.table::rbindlist(
    profile_rows,
    fill = TRUE,
    use.names = TRUE
  )
}

profile_data_file_attributes <- function(
  path,
  format_label,
  config = list(),
  layer = NULL
) {
  format_label <- if (
    is.null(format_label) ||
    length(format_label) == 0L ||
    is.na(format_label[1])
  ) {
    ""
  } else {
    trimws(
      as.character(format_label[1])
    )
  }

  raster_formats <- c(
    "raster",
    "virtual_raster",
    "esri_arcinfo_grid"
  )

  vector_formats <- c(
    "vector",
    "esri_file_geodatabase"
  )

  if (format_label %in% raster_formats) {
    return(
      profile_raster_attributes(
        path = path,
        config = config,
        format_label = format_label
      )
    )
  }

  if (format_label %in% vector_formats) {
    return(
      profile_vector_attributes(
        path = path,
        layer = layer,
        config = config,
        format_label = format_label
      )
    )
  }

  data.table::data.table(
    message = paste(
      "Attribute profiling does not currently support",
      "data-file format:",
      format_label
    )
  )
}


attribute_profile_type_for_format <- function(
  format_label
) {
  format_label <- trimws(
    as.character(
      format_label[1] %||% ""
    )
  )

  if (
    format_label %in%
      c(
        "raster",
        "virtual_raster",
        "esri_arcinfo_grid"
      )
  ) {
    return("raster_layers")
  }

  if (
    format_label %in%
      c(
        "vector",
        "esri_file_geodatabase"
      )
  ) {
    return("vector_attributes")
  }

  "attribute_table"
}

save_attribute_profile <- function(
  conn,
  dataset_id,
  data_file_row,
  data_pond_id,
  profile_dt,
  user_id = NA_integer_,
  profile_type = "attribute_table"
) {
  if (
    is.null(profile_dt) ||
    nrow(profile_dt) == 0L
  ) {
    stop(
      "The attribute profiler returned no records."
    )
  }

  if (
    "message" %in% names(profile_dt) &&
    !"data_field_name" %in% names(profile_dt)
  ) {
    stop(
      paste(
        unique(profile_dt$message),
        collapse = " | "
      )
    )
  }

  dt <- data.table::as.data.table(
    data.table::copy(profile_dt)
  )

  required_columns <- list(
    layer_name = NA_character_,
    data_field_name = NA_character_,
    data_field_alias = NA_character_,
    data_field_units = NA_character_,
    legend_field = 0L,
    attribute_type = NA_character_,
    attribute_order = NA_integer_,
    example_values = NA_character_,
    value_count = NA_real_,
    missing_count = NA_real_,
    unique_count = NA_real_,
    min_value = NA_character_,
    max_value = NA_character_
  )

  for (field in names(required_columns)) {
    if (!field %in% names(dt)) {
      dt[
        ,
        (field) := required_columns[[field]]
      ]
    }
  }

  if (
    any(
      is.na(dt$data_field_name) |
        !nzchar(
          trimws(
            as.character(
              dt$data_field_name
            )
          )
        )
    )
  ) {
    stop(
      paste(
        "At least one profile row is missing",
        "data_field_name."
      )
    )
  }

  scalar_text <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0L ||
      is.na(x[1]) ||
      !nzchar(trimws(as.character(x[1])))
    ) {
      return(NA_character_)
    }

    trimws(
      as.character(x[1])
    )
  }

  scalar_number <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0L ||
      is.na(x[1])
    ) {
      return(NA_real_)
    }

    suppressWarnings(
      as.numeric(x[1])
    )
  }

  scalar_integer <- function(x) {
    if (
      is.null(x) ||
      length(x) == 0L ||
      is.na(x[1])
    ) {
      return(NA_integer_)
    }

    suppressWarnings(
      as.integer(x[1])
    )
  }

  profile_type <- scalar_text(
    profile_type
  )

  if (is.na(profile_type)) {
    profile_type <- "attribute_table"
  }

  now <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  user_id_value <- scalar_integer(
    user_id
  )

  data_file_id <- scalar_integer(
    data_file_row$id
  )

  relative_path <- scalar_text(
    data_file_row$data_file_rel_path_full
  )

  file_name <- scalar_text(
    data_file_row$file_name
  )

  if (is.na(data_file_id)) {
    stop("The saved data-file ID is missing.")
  }

  if (is.na(relative_path)) {
    stop(
      paste(
        "The saved data file does not have a",
        "relative-path key."
      )
    )
  }

  DBI::dbExecute(
    conn,
    "
    INSERT INTO data_profile_run_tbl (
      dataset_id,
      data_file_id,
      data_pond_id,
      profile_type,
      profile_status,
      profiled_by_user_id,
      started_at
    )
    VALUES (?, ?, ?, ?, 'running', ?, ?)
    ",
    params = list(
      as.integer(dataset_id),
      data_file_id,
      data_pond_id,
      profile_type,
      user_id_value,
      now
    )
  )

  profile_id <- DBI::dbGetQuery(
    conn,
    "SELECT last_insert_rowid() AS id"
  )$id[1]

  tryCatch(
    {
      DBI::dbWithTransaction(
        conn,
        {
          for (i in seq_len(nrow(dt))) {
            row <- dt[i]

            DBI::dbExecute(
              conn,
              "
              INSERT INTO dataset_attribute_tbl (
                dataset_id,
                data_file_id,
                data_pond_id,

                data_file_rel_path_full,
                data_file_name,
                layer_name,

                data_field_alias,
                data_field_name,
                data_field_units,
                legend_field,

                attribute_type,
                attribute_order,
                example_values,

                value_count,
                missing_count,
                unique_count,
                min_value,
                max_value,

                profile_updated_at,
                updated_at,
                updated_by_user_id
              )
              VALUES (
                ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,
                ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
              )

              ON CONFLICT (
                dataset_id,
                data_file_rel_path_full,
                COALESCE(layer_name, ''),
                data_field_name
              )
              DO UPDATE SET
                data_file_id =
                  excluded.data_file_id,

                data_pond_id =
                  excluded.data_pond_id,

                data_file_name =
                  excluded.data_file_name,

                data_field_alias =
                  COALESCE(
                    NULLIF(
                      TRIM(
                        dataset_attribute_tbl
                          .data_field_alias
                      ),
                      ''
                    ),
                    excluded.data_field_alias
                  ),

                data_field_units =
                  COALESCE(
                    NULLIF(
                      TRIM(
                        dataset_attribute_tbl
                          .data_field_units
                      ),
                      ''
                    ),
                    excluded.data_field_units
                  ),

                attribute_type =
                  excluded.attribute_type,

                attribute_order =
                  excluded.attribute_order,

                example_values =
                  excluded.example_values,

                value_count =
                  excluded.value_count,

                missing_count =
                  excluded.missing_count,

                unique_count =
                  excluded.unique_count,

                min_value =
                  excluded.min_value,

                max_value =
                  excluded.max_value,

                profile_updated_at =
                  excluded.profile_updated_at,

                updated_at =
                  excluded.updated_at,

                updated_by_user_id =
                  excluded.updated_by_user_id
              ",
              params = list(
                as.integer(dataset_id),
                data_file_id,
                data_pond_id,

                relative_path,
                file_name,
                scalar_text(
                  row$layer_name
                ),

                scalar_text(
                  row$data_field_alias
                ),

                scalar_text(
                  row$data_field_name
                ),

                scalar_text(
                  row$data_field_units
                ),

                as.integer(
                  isTRUE(
                    as.logical(
                      row$legend_field[1]
                    )
                  )
                ),

                scalar_text(
                  row$attribute_type
                ),

                scalar_integer(
                  row$attribute_order
                ),

                scalar_text(
                  row$example_values
                ),

                scalar_number(
                  row$value_count
                ),

                scalar_number(
                  row$missing_count
                ),

                scalar_number(
                  row$unique_count
                ),

                scalar_text(
                  row$min_value
                ),

                scalar_text(
                  row$max_value
                ),

                now,
                now,
                user_id_value
              )
            )
          }

          completed_at <- format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S"
          )

          DBI::dbExecute(
            conn,
            "
            UPDATE data_profile_run_tbl
            SET
              profile_status = 'complete',
              profile_message = NULL,
              completed_at = ?
            WHERE id = ?
            ",
            params = list(
              completed_at,
              profile_id
            )
          )

          DBI::dbExecute(
            conn,
            "
            UPDATE data_file_tbl
            SET profile_updated_at = ?
            WHERE id = ?
            ",
            params = list(
              completed_at,
              data_file_id
            )
          )
        }
      )
    },
    error = function(e) {
      DBI::dbExecute(
        conn,
        "
        UPDATE data_profile_run_tbl
        SET
          profile_status = 'failed',
          profile_message = ?,
          completed_at = ?
        WHERE id = ?
        ",
        params = list(
          conditionMessage(e),
          format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S"
          ),
          profile_id
        )
      )

      stop(e)
    }
  )

  invisible(
    list(
      profile_id = as.integer(profile_id),
      profile_type = profile_type,
      records_saved = nrow(dt)
    )
  )
}

get_data_files_for_dataset <- function(dataset_id, base_path, active_only = TRUE) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  where_active <- if (isTRUE(active_only)) "AND active = 1" else ""
  sql <- paste0("
    SELECT *
    FROM data_file_tbl
    WHERE dataset_id = ? ", where_active, "
    ORDER BY data_file_rel_path_full
  ")

  DBI::dbGetQuery(conn, sql, params = list(dataset_id))
}

get_data_file_review_summary <- function(
  dataset_id,
  base_path,
  active_only = FALSE
) {
  if (
    is.null(dataset_id) ||
    length(dataset_id) == 0 ||
    is.na(dataset_id)
  ) {
    return(data.frame())
  }

  conn <- get_conn(base_path)

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  where_active <- if (isTRUE(active_only)) {
    "AND df.active = 1"
  } else {
    ""
  }

  sql <- paste0(
    "
    WITH attribute_summary AS (
      SELECT
        data_file_id,
        COUNT(*) AS attribute_count,
        COUNT(DISTINCT COALESCE(layer_name, '')) AS layer_count,
        MAX(profile_updated_at) AS latest_attribute_profile_at
      FROM dataset_attribute_tbl
      WHERE dataset_id = ?
      GROUP BY data_file_id
    ),

    latest_profile_run AS (
      SELECT
        pr.data_file_id,
        pr.profile_status,
        pr.profile_message,
        pr.started_at,
        pr.completed_at,
        pr.profiled_by_user_id
      FROM data_profile_run_tbl pr
      INNER JOIN (
        SELECT
          data_file_id,
          MAX(id) AS latest_profile_run_id
        FROM data_profile_run_tbl
        WHERE dataset_id = ?
        GROUP BY data_file_id
      ) latest
        ON pr.id = latest.latest_profile_run_id
    )

    SELECT
      df.id AS data_file_id,
      df.file_name,
      df.data_file_rel_path_full,
      df.data_file_format,
      df.file_role,
      df.content_hint,
      df.review_status,
      df.data_file_description,
      df.data_file_notes,
      df.include_in_attribute_scan,
      df.active,
      df.scan_status,
      df.profile_updated_at AS file_profile_updated_at,
      df.updated_by_user_id,

      COALESCE(a.attribute_count, 0) AS attribute_count,
      COALESCE(a.layer_count, 0) AS layer_count,
      a.latest_attribute_profile_at,

      pr.profile_status,
      pr.profile_message,
      pr.started_at AS profile_started_at,
      pr.completed_at AS profile_completed_at,

      u.person_id AS reviewed_by_person_id,
      p.name_first || ' ' || p.name_last AS reviewed_by_name,

      pu.person_id AS profiled_by_person_id,
      pp.name_first || ' ' || pp.name_last AS profiled_by_name

    FROM data_file_tbl df

    LEFT JOIN attribute_summary a
      ON a.data_file_id = df.id

    LEFT JOIN latest_profile_run pr
      ON pr.data_file_id = df.id

    LEFT JOIN user_tbl u
      ON u.id = df.updated_by_user_id

    LEFT JOIN persons_tbl p
      ON p.id = u.person_id

    LEFT JOIN user_tbl pu
      ON pu.id = pr.profiled_by_user_id

    LEFT JOIN persons_tbl pp
      ON pp.id = pu.person_id

    WHERE df.dataset_id = ?
      ",
    where_active,
    "

    ORDER BY
      df.active DESC,
      df.data_file_rel_path_full
    "
  )

  DBI::dbGetQuery(
    conn,
    sql,
    params = list(
      dataset_id,
      dataset_id,
      dataset_id
    )
  )
}

update_data_file_review <- function(
  data_file_id,
  base_path,
  data_file_description = NULL,
  data_file_notes = NULL,
  content_hint = NULL,
  review_status = NULL,
  include_in_attribute_scan = NULL,
  user_id = NA_integer_
) {
  if (
    is.null(data_file_id) ||
    length(data_file_id) == 0 ||
    is.na(data_file_id)
  ) {
    stop("data_file_id is required.")
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
    UPDATE data_file_tbl
    SET
      data_file_description = ?,
      data_file_notes = ?,
      content_hint = ?,
      review_status = ?,
      include_in_attribute_scan = ?,
      updated_by_user_id = ?,
      last_seen_at = COALESCE(last_seen_at, ?)
    WHERE id = ?
    ",
    params = list(
      data_file_description,
      data_file_notes,
      content_hint,
      review_status,
      if (is.null(include_in_attribute_scan)) {
        NA_integer_
      } else {
        as.integer(
          isTRUE(include_in_attribute_scan)
        )
      },
      user_id,
      now,
      as.integer(data_file_id)
    )
  )

  invisible(TRUE)
}

get_dataset_attributes <- function(dataset_id, base_path) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbGetQuery(conn, "
    SELECT *
    FROM dataset_attribute_tbl
    WHERE dataset_id = ?
    ORDER BY data_file_rel_path_full, layer_name, attribute_order, data_field_name
  ", params = list(dataset_id))
}

update_dataset_attribute_note <- function(attribute_id, base_path,
                                          data_field_alias = NULL,
                                          data_field_desc = NULL,
                                          data_field_units = NULL,
                                          legend_field = FALSE,
                                          legend_field_notes = NULL,
                                          user_id = NA_integer_) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbExecute(conn, "
    UPDATE dataset_attribute_tbl
    SET data_field_alias = ?,
        data_field_desc = ?,
        data_field_units = ?,
        legend_field = ?,
        legend_field_notes = ?,
        updated_at = ?,
        updated_by_user_id = ?
    WHERE id = ?
  ", params = list(
    data_field_alias,
    data_field_desc,
    data_field_units,
    as.integer(isTRUE(legend_field)),
    legend_field_notes,
    now,
    user_id,
    attribute_id
  ))

  invisible(TRUE)
}