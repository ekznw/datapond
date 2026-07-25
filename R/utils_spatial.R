spatial_candidate_formats <- c(
  "vector",
  "raster",
  "virtual_raster",
  "esri_file_geodatabase",
  "esri_arcinfo_grid"
)


is_spatial_candidate <- function(dt) {
  if ("message" %in% names(dt)) {
    return(rep(FALSE, nrow(dt)))
  }
  fmt <- dt$data_file_format
  if ("logical_format" %in% names(dt)) {
    fmt <- data.table::fifelse(
      !is.na(dt$logical_format) & dt$logical_format != "",
      dt$logical_format,
      dt$data_file_format
    )
  }
  dt$primary_dataset %in% TRUE &
    fmt %in% spatial_candidate_formats
}


bbox_to_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  vals <- as.numeric(x)
  if (length(vals) < 4 || any(is.na(vals))) return(NA_character_)
  paste(
    round(vals[1], 6),
    round(vals[2], 6),
    round(vals[3], 6),
    round(vals[4], 6),
    sep = "; "
  )
}

bbox_scalar_text <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    is.na(x[1])
  ) {
    return(NA_character_)
  }

  value <- trimws(
    as.character(x[1])
  )

  if (!nzchar(value)) {
    return(NA_character_)
  }

  value
}


format_bbox_text <- function(values, digits = 6L) {
  paste(
    round(
      as.numeric(values),
      digits = digits
    ),
    collapse = "; "
  )
}


empty_bbox_result <- function(status, bbox_native = NA_character_) {
  list(
    bbox_native = bbox_native,
    bbox_status = status,
    xmin = NA_real_,
    ymin = NA_real_,
    xmax = NA_real_,
    ymax = NA_real_
  )
}


normalise_bbox_record <- function(
  bbox_native = NULL,
  xmin = NA_real_,
  ymin = NA_real_,
  xmax = NA_real_,
  ymax = NA_real_,
  digits = 6L
) {
  numeric_values <- suppressWarnings(
    as.numeric(
      c(
        xmin = xmin[1],
        ymin = ymin[1],
        xmax = xmax[1],
        ymax = ymax[1]
      )
    )
  )

  names(numeric_values) <- c(
    "xmin",
    "ymin",
    "xmax",
    "ymax"
  )

  numeric_supplied <- any(
    !is.na(numeric_values) |
      is.nan(numeric_values) |
      is.infinite(numeric_values)
  )

  bbox_text <- bbox_scalar_text(
    bbox_native
  )

  candidate <- NULL
  candidate_text <- bbox_text

  # Prefer complete numeric columns when they are already available.
  if (all(is.finite(numeric_values))) {
    candidate <- numeric_values
    candidate_text <- format_bbox_text(
      numeric_values,
      digits = digits
    )
  } else if (!is.na(bbox_text)) {
    parts <- trimws(
      strsplit(
        bbox_text,
        ";",
        fixed = TRUE
      )[[1]]
    )

    if (length(parts) != 4L) {
      return(
        empty_bbox_result(
          status = "invalid_bbox_format",
          bbox_native = bbox_text
        )
      )
    }

    candidate <- suppressWarnings(
      as.numeric(parts)
    )

    accepted_non_finite <- tolower(parts) %in% c(
      "na",
      "nan",
      "+nan",
      "-nan",
      "inf",
      "+inf",
      "-inf",
      "infinity",
      "+infinity",
      "-infinity"
    )

    conversion_failed <- is.na(candidate) &
      !accepted_non_finite

    if (any(conversion_failed)) {
      return(
        empty_bbox_result(
          status = "invalid_bbox_format",
          bbox_native = bbox_text
        )
      )
    }

    names(candidate) <- c(
      "xmin",
      "ymin",
      "xmax",
      "ymax"
    )
  } else if (isTRUE(numeric_supplied)) {
    status <- if (
      any(is.nan(numeric_values)) ||
      any(is.infinite(numeric_values))
    ) {
      "non_finite_bbox"
    } else {
      "incomplete_bbox"
    }

    return(
      empty_bbox_result(
        status = status
      )
    )
  } else {
    return(
      empty_bbox_result(
        status = "missing_bbox"
      )
    )
  }

  if (any(!is.finite(candidate))) {
    return(
      empty_bbox_result(
        status = "non_finite_bbox",
        bbox_native = candidate_text
      )
    )
  }

  if (
    candidate["xmin"] >= candidate["xmax"] ||
    candidate["ymin"] >= candidate["ymax"]
  ) {
    return(
      empty_bbox_result(
        status = "invalid_bbox_order",
        bbox_native = candidate_text
      )
    )
  }

  list(
    bbox_native = format_bbox_text(
      candidate,
      digits = digits
    ),
    bbox_status = "valid",
    xmin = unname(candidate["xmin"]),
    ymin = unname(candidate["ymin"]),
    xmax = unname(candidate["xmax"]),
    ymax = unname(candidate["ymax"])
  )
}


add_bbox_columns <- function(spatial_dt, digits = 6L) {
  if (
    is.null(spatial_dt) ||
    nrow(spatial_dt) == 0L ||
    "message" %in% names(spatial_dt)
  ) {
    return(spatial_dt)
  }

  dt <- data.table::as.data.table(
    data.table::copy(spatial_dt)
  )

  required_columns <- list(
    bbox_native = NA_character_,
    xmin = NA_real_,
    ymin = NA_real_,
    xmax = NA_real_,
    ymax = NA_real_
  )

  for (field in names(required_columns)) {
    if (!field %in% names(dt)) {
      dt[
        ,
        (field) := required_columns[[field]]
      ]
    }
  }

  bbox_results <- lapply(
    seq_len(nrow(dt)),
    function(i) {
      normalise_bbox_record(
        bbox_native = dt$bbox_native[i],
        xmin = dt$xmin[i],
        ymin = dt$ymin[i],
        xmax = dt$xmax[i],
        ymax = dt$ymax[i],
        digits = digits
      )
    }
  )

  dt[
    ,
    bbox_native := vapply(
      bbox_results,
      `[[`,
      character(1),
      "bbox_native"
    )
  ]

  dt[
    ,
    bbox_status := vapply(
      bbox_results,
      `[[`,
      character(1),
      "bbox_status"
    )
  ]

  dt[
    ,
    xmin := vapply(
      bbox_results,
      `[[`,
      numeric(1),
      "xmin"
    )
  ]

  dt[
    ,
    ymin := vapply(
      bbox_results,
      `[[`,
      numeric(1),
      "ymin"
    )
  ]

  dt[
    ,
    xmax := vapply(
      bbox_results,
      `[[`,
      numeric(1),
      "xmax"
    )
  ]

  dt[
    ,
    ymax := vapply(
      bbox_results,
      `[[`,
      numeric(1),
      "ymax"
    )
  ]

  dt[]
}

spatial_record_value <- function(
  record,
  field,
  default = NA
) {
  if (
    is.null(record) ||
    !field %in% names(record)
  ) {
    return(default)
  }

  value <- record[[field]]

  if (
    is.null(value) ||
    length(value) == 0L ||
    all(is.na(value))
  ) {
    return(default)
  }

  value[1]
}


spatial_record_text <- function(
  record,
  field,
  default = NA_character_
) {
  value <- spatial_record_value(
    record = record,
    field = field,
    default = default
  )

  if (
    is.null(value) ||
    length(value) == 0L ||
    is.na(value[1])
  ) {
    return(default)
  }

  value <- trimws(
    as.character(value[1])
  )

  if (!nzchar(value)) {
    return(default)
  }

  value
}


spatial_record_numeric <- function(
  record,
  field
) {
  value <- spatial_record_value(
    record = record,
    field = field,
    default = NA_real_
  )

  suppressWarnings(
    as.numeric(value[1])
  )
}


spatial_bbox_unavailable <- function(
  status,
  message
) {
  list(
    eligible = FALSE,
    status = status,
    message = message,
    west = NA_real_,
    south = NA_real_,
    east = NA_real_,
    north = NA_real_,
    crs_source = NA_character_,
    source_crs = NA_character_,
    target_crs = "EPSG:4326"
  )
}


resolve_spatial_record_crs <- function(record) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(
      list(
        valid = FALSE,
        status = "sf_not_available",
        message = paste(
          "The sf package is required to validate and transform",
          "the bounding box."
        ),
        crs = NULL,
        crs_source = NA_character_,
        source_text = NA_character_
      )
    )
  }

  crs_input <- spatial_record_text(
    record,
    "crs_input"
  )

  crs_auth_code <- spatial_record_text(
    record,
    "crs_auth_code"
  )

  candidates <- list(
    crs_input = crs_input,
    crs_auth_code = crs_auth_code
  )

  has_candidate <- any(
    vapply(
      candidates,
      function(value) {
        !is.na(value) && nzchar(value)
      },
      logical(1)
    )
  )

  if (!isTRUE(has_candidate)) {
    return(
      list(
        valid = FALSE,
        status = "missing_crs",
        message = paste(
          "The bounding box is valid, but no CRS is explicitly",
          "defined for this spatial record."
        ),
        crs = NULL,
        crs_source = NA_character_,
        source_text = NA_character_
      )
    )
  }

  for (source_name in names(candidates)) {
    candidate <- candidates[[source_name]]

    if (
      is.na(candidate) ||
      !nzchar(candidate)
    ) {
      next
    }

    crs <- tryCatch(
      suppressWarnings(
        sf::st_crs(candidate)
      ),
      error = function(e) {
        NULL
      }
    )

    if (
      !is.null(crs) &&
      !is.na(crs)
    ) {
      return(
        list(
          valid = TRUE,
          status = "valid_crs",
          message = "CRS resolved successfully.",
          crs = crs,
          crs_source = source_name,
          source_text = candidate
        )
      )
    }
  }

  list(
    valid = FALSE,
    status = "invalid_crs",
    message = paste(
      "A CRS value is stored, but it could not be interpreted",
      "by sf and PROJ."
    ),
    crs = NULL,
    crs_source = NA_character_,
    source_text = NA_character_
  )
}


bbox_edge_ring <- function(
  xmin,
  ymin,
  xmax,
  ymax,
  points_per_edge = 25L
) {
  points_per_edge <- max(
    2L,
    as.integer(points_per_edge)
  )

  bottom <- cbind(
    seq(
      xmin,
      xmax,
      length.out = points_per_edge
    ),
    ymin
  )

  right <- cbind(
    xmax,
    seq(
      ymin,
      ymax,
      length.out = points_per_edge
    )[-1]
  )

  top <- cbind(
    seq(
      xmax,
      xmin,
      length.out = points_per_edge
    )[-1],
    ymax
  )

  left <- cbind(
    xmin,
    seq(
      ymax,
      ymin,
      length.out = points_per_edge
    )[-1]
  )

  ring <- rbind(
    bottom,
    right,
    top,
    left
  )

  colnames(ring) <- c(
    "x",
    "y"
  )

  ring
}


spatial_bbox_preview <- function(
  record,
  points_per_edge = 25L
) {
  if (
    is.null(record) ||
    nrow(as.data.frame(record)) == 0L
  ) {
    return(
      spatial_bbox_unavailable(
        status = "no_record",
        message = "No spatial metadata record is selected."
      )
    )
  }

  spatial_status <- tolower(
    spatial_record_text(
      record,
      "spatial_status",
      default = ""
    )
  )

  if (!identical(spatial_status, "ok")) {
    return(
      spatial_bbox_unavailable(
        status = "spatial_status_not_ok",
        message = paste(
          "This record is not eligible because spatial extraction",
          "did not return status 'ok'."
        )
      )
    )
  }

  bbox <- normalise_bbox_record(
    bbox_native = spatial_record_text(
      record,
      "bbox_native"
    ),
    xmin = spatial_record_numeric(
      record,
      "xmin"
    ),
    ymin = spatial_record_numeric(
      record,
      "ymin"
    ),
    xmax = spatial_record_numeric(
      record,
      "xmax"
    ),
    ymax = spatial_record_numeric(
      record,
      "ymax"
    )
  )

  if (!identical(bbox$bbox_status, "valid")) {
    return(
      spatial_bbox_unavailable(
        status = bbox$bbox_status,
        message = paste(
          "This record does not contain a valid finite",
          "bounding box."
        )
      )
    )
  }

  crs_result <- resolve_spatial_record_crs(
    record
  )

  if (!isTRUE(crs_result$valid)) {
    return(
      spatial_bbox_unavailable(
        status = crs_result$status,
        message = crs_result$message
      )
    )
  }

  transformed <- tryCatch(
    {
      ring <- bbox_edge_ring(
        xmin = bbox$xmin,
        ymin = bbox$ymin,
        xmax = bbox$xmax,
        ymax = bbox$ymax,
        points_per_edge = points_per_edge
      )

      polygon <- sf::st_sfc(
        sf::st_polygon(
          list(ring)
        ),
        crs = crs_result$crs
      )

      polygon_wgs84 <- suppressWarnings(
        sf::st_transform(
          polygon,
          crs = 4326
        )
      )

      transformed_bbox <- sf::st_bbox(
        polygon_wgs84
      )

      c(
        west = unname(
          transformed_bbox["xmin"]
        ),
        south = unname(
          transformed_bbox["ymin"]
        ),
        east = unname(
          transformed_bbox["xmax"]
        ),
        north = unname(
          transformed_bbox["ymax"]
        )
      )
    },
    error = function(e) {
      structure(
        rep(NA_real_, 4L),
        names = c(
          "west",
          "south",
          "east",
          "north"
        ),
        error_message = conditionMessage(e)
      )
    }
  )

  if (any(!is.finite(transformed))) {
    error_message <- attr(
      transformed,
      "error_message"
    )

    if (
      is.null(error_message) ||
      !nzchar(error_message)
    ) {
      error_message <- paste(
        "The transformed bounding box contains",
        "non-finite coordinates."
      )
    }

    return(
      spatial_bbox_unavailable(
        status = "transform_failed",
        message = paste(
          "The bounding box could not be transformed to",
          "EPSG:4326:",
          error_message
        )
      )
    )
  }

  if (
    transformed["west"] >= transformed["east"] ||
    transformed["south"] >= transformed["north"]
  ) {
    return(
      spatial_bbox_unavailable(
        status = "invalid_transformed_bbox",
        message = paste(
          "The transformed bounding box has an invalid",
          "coordinate order."
        )
      )
    )
  }

  if (
    transformed["west"] < -180 ||
    transformed["east"] > 180 ||
    transformed["south"] < -90 ||
    transformed["north"] > 90
  ) {
    return(
      spatial_bbox_unavailable(
        status = "invalid_geographic_range",
        message = paste(
          "The transformed coordinates fall outside the",
          "valid EPSG:4326 longitude or latitude range."
        )
      )
    )
  }

  source_label <- spatial_record_text(
    record,
    "crs_auth_code",
    default = "stored CRS"
  )

  list(
    eligible = TRUE,
    status = "eligible",
    message = paste(
      "Eligible for preview and dataset-bbox inclusion.",
      "The native extent was transformed from",
      source_label,
      "to EPSG:4326."
    ),
    west = unname(transformed["west"]),
    south = unname(transformed["south"]),
    east = unname(transformed["east"]),
    north = unname(transformed["north"]),
    crs_source = crs_result$crs_source,
    source_crs = source_label,
    target_crs = "EPSG:4326"
  )
}

validate_manual_spatial_extent <- function(
  crs_value,
  xmin,
  ymin,
  xmax,
  ymax
) {
  bbox <- normalise_bbox_record(
    bbox_native = NULL,
    xmin = xmin,
    ymin = ymin,
    xmax = xmax,
    ymax = ymax
  )

  if (!identical(bbox$bbox_status, "valid")) {
    preview <- spatial_bbox_unavailable(
      status = bbox$bbox_status,
      message = paste(
        "The manually entered extent is invalid:",
        bbox$bbox_status
      )
    )

    return(
      list(
        valid = FALSE,
        status = bbox$bbox_status,
        message = preview$message,
        record = NULL,
        preview = preview
      )
    )
  }

  crs_value <- bbox_scalar_text(
    crs_value
  )

  if (is.na(crs_value)) {
    preview <- spatial_bbox_unavailable(
      status = "missing_crs",
      message = paste(
        "Enter an explicit CRS, for example EPSG:4326,",
        "EPSG:32736 or a valid WKT definition."
      )
    )

    return(
      list(
        valid = FALSE,
        status = "missing_crs",
        message = preview$message,
        record = NULL,
        preview = preview
      )
    )
  }

  authority_input <- if (
    grepl(
      "^[A-Za-z][A-Za-z0-9_]*:[0-9]+$",
      crs_value
    )
  ) {
    toupper(crs_value)
  } else {
    NA_character_
  }

  record <- data.frame(
    spatial_status = "ok",
    spatial_type = "manual_extent",
    crs_auth_code = authority_input,
    crs_name = NA_character_,
    crs_input = crs_value,
    crs_status = "manual_crs",
    bbox_native = bbox$bbox_native,
    bbox_status = bbox$bbox_status,
    xmin = bbox$xmin,
    ymin = bbox$ymin,
    xmax = bbox$xmax,
    ymax = bbox$ymax,
    stringsAsFactors = FALSE
  )

  crs_result <- resolve_spatial_record_crs(
    record
  )

  if (!isTRUE(crs_result$valid)) {
    preview <- spatial_bbox_unavailable(
      status = crs_result$status,
      message = crs_result$message
    )

    return(
      list(
        valid = FALSE,
        status = crs_result$status,
        message = crs_result$message,
        record = record,
        preview = preview
      )
    )
  }

  epsg <- tryCatch(
    crs_result$crs$epsg,
    error = function(e) {
      NA_integer_
    }
  )

  if (
    !is.null(epsg) &&
    length(epsg) > 0L &&
    !is.na(epsg[1])
  ) {
    record$crs_auth_code <- paste0(
      "EPSG:",
      as.integer(epsg[1])
    )
  }

  crs_name <- tryCatch(
    crs_result$crs$Name,
    error = function(e) {
      NA_character_
    }
  )

  if (
    !is.null(crs_name) &&
    length(crs_name) > 0L &&
    !is.na(crs_name[1]) &&
    nzchar(trimws(as.character(crs_name[1])))
  ) {
    record$crs_name <- trimws(
      as.character(crs_name[1])
    )
  }

  preview <- spatial_bbox_preview(
    record
  )

  if (!isTRUE(preview$eligible)) {
    return(
      list(
        valid = FALSE,
        status = preview$status,
        message = preview$message,
        record = record,
        preview = preview
      )
    )
  }

  list(
    valid = TRUE,
    status = "eligible",
    message = paste(
      "The manual extent is valid and can be transformed",
      "to EPSG:4326."
    ),
    record = record,
    preview = preview
  )
}

derive_dataset_bbox_from_records <- function(records) {
  empty_contributors <- function() {
    data.table::data.table(
      source_type = character(),
      source_id = integer(),
      source_key = character(),
      source_label = character(),
      data_file_id = integer(),
      file_name = character(),
      data_file_rel_path_full = character(),
      layer_name = character(),
      data_file_format = character(),
      eligible = logical(),
      eligibility_status = character(),
      eligibility_message = character(),
      source_crs = character(),
      west = numeric(),
      south = numeric(),
      east = numeric(),
      north = numeric(),
      bbox_wgs84 = character()
    )
  }

  empty_summary <- function(
    status,
    message,
    included = 0L,
    eligible = 0L,
    skipped = 0L
  ) {
    data.table::data.table(
      bbox_status = status,
      status_message = message,
      bbox_crs = "EPSG:4326",
      bbox_wgs84 = NA_character_,
      xmin = NA_real_,
      ymin = NA_real_,
      xmax = NA_real_,
      ymax = NA_real_,
      included_source_count = as.integer(included),
      eligible_source_count = as.integer(eligible),
      skipped_source_count = as.integer(skipped),
      contributor_keys = NA_character_,
      derivation_method = "included_source_envelope"
    )
  }

  if (
    is.null(records) ||
    nrow(as.data.frame(records)) == 0L
  ) {
    return(
      list(
        summary = empty_summary(
          status = "no_included_sources",
          message = paste(
            "No spatial file or layer is currently selected",
            "for inclusion in the dataset bounding box."
          )
        ),
        contributors = empty_contributors()
      )
    )
  }

  dt <- data.table::as.data.table(
    data.table::copy(records)
  )

  required_columns <- list(
    id = NA_integer_,
    source_type = NA_character_,
    source_id = NA_integer_,
    source_key = NA_character_,
    source_label = NA_character_,
    data_file_id = NA_integer_,
    file_name = NA_character_,
    data_file_rel_path_full = NA_character_,
    layer_name = NA_character_,
    data_file_format = NA_character_
  )

  for (field in names(required_columns)) {
    if (!field %in% names(dt)) {
      dt[
        ,
        (field) := required_columns[[field]]
      ]
    }
  }

  preview_results <- lapply(
    seq_len(nrow(dt)),
    function(i) {
      spatial_bbox_preview(
        dt[i]
      )
    }
  )

  result_text <- function(result, field) {
    value <- result[[field]]

    if (
      is.null(value) ||
      length(value) == 0L ||
      is.na(value[1]) ||
      !nzchar(trimws(as.character(value[1])))
    ) {
      return(NA_character_)
    }

    as.character(value[1])
  }

  result_numeric <- function(result, field) {
    value <- result[[field]]

    if (
      is.null(value) ||
      length(value) == 0L ||
      is.na(value[1])
    ) {
      return(NA_real_)
    }

    suppressWarnings(
      as.numeric(value[1])
    )
  }

  source_file_label <- ifelse(
    !is.na(dt$file_name) &
      nzchar(trimws(dt$file_name)),
    dt$file_name,
    basename(dt$data_file_rel_path_full)
  )

  default_source_label <- ifelse(
    !is.na(dt$layer_name) &
      nzchar(trimws(dt$layer_name)),
    paste0(
      source_file_label,
      " | ",
      dt$layer_name
    ),
    source_file_label
  )

  dt[
    ,
    source_type := trimws(
      as.character(source_type)
    )
  ]

  dt[
    is.na(source_type) |
      source_type == "",
    source_type := "extracted_spatial_record"
  ]

  dt[
    ,
    source_id := suppressWarnings(
      as.integer(source_id)
    )
  ]

  dt[
    is.na(source_id),
    source_id := suppressWarnings(
      as.integer(id)
    )
  ]

  dt[
    ,
    source_key := trimws(
      as.character(source_key)
    )
  ]

  missing_source_key <- is.na(dt$source_key) |
    dt$source_key == ""

  source_prefix <- ifelse(
    dt$source_type == "manual_extent",
    "manual:",
    "spatial:"
  )

  dt[
    missing_source_key,
    source_key := paste0(
      source_prefix[missing_source_key],
      source_id[missing_source_key]
    )
  ]

  dt[
    ,
    source_label := trimws(
      as.character(source_label)
    )
  ]

  dt[
    is.na(source_label) |
      source_label == "",
    source_label := default_source_label
  ]

  contributors <- data.table::data.table(
    source_type = as.character(
      dt$source_type
    ),

    source_id = suppressWarnings(
      as.integer(dt$source_id)
    ),

    source_key = as.character(
      dt$source_key
    ),

    source_label = as.character(
      dt$source_label
    ),

    data_file_id = suppressWarnings(
      as.integer(dt$data_file_id)
    ),

    file_name = as.character(
      dt$file_name
    ),

    data_file_rel_path_full = as.character(
      dt$data_file_rel_path_full
    ),

    layer_name = as.character(
      dt$layer_name
    ),

    data_file_format = as.character(
      dt$data_file_format
    ),

    eligible = vapply(
      preview_results,
      function(result) {
        isTRUE(result$eligible)
      },
      logical(1)
    ),

    eligibility_status = vapply(
      preview_results,
      result_text,
      character(1),
      field = "status"
    ),

    eligibility_message = vapply(
      preview_results,
      result_text,
      character(1),
      field = "message"
    ),

    source_crs = vapply(
      preview_results,
      result_text,
      character(1),
      field = "source_crs"
    ),

    west = vapply(
      preview_results,
      result_numeric,
      numeric(1),
      field = "west"
    ),

    south = vapply(
      preview_results,
      result_numeric,
      numeric(1),
      field = "south"
    ),

    east = vapply(
      preview_results,
      result_numeric,
      numeric(1),
      field = "east"
    ),

    north = vapply(
      preview_results,
      result_numeric,
      numeric(1),
      field = "north"
    )
  )

  # Store a readable EPSG:4326 bbox for every eligible contributor.
  contributors[
    ,
    bbox_wgs84 := NA_character_
  ]

  valid_bbox <- (
    contributors$eligible %in% TRUE &
      is.finite(contributors$west) &
      is.finite(contributors$south) &
      is.finite(contributors$east) &
      is.finite(contributors$north)
  )

  contributors[
    valid_bbox,
    bbox_wgs84 := paste(
      format(
        round(west, 6L),
        trim = TRUE,
        scientific = FALSE
      ),
      format(
        round(south, 6L),
        trim = TRUE,
        scientific = FALSE
      ),
      format(
        round(east, 6L),
        trim = TRUE,
        scientific = FALSE
      ),
      format(
        round(north, 6L),
        trim = TRUE,
        scientific = FALSE
      ),
      sep = "; "
    )
  ]

  eligible_contributors <- contributors[
    eligible == TRUE &
      is.finite(west) &
      is.finite(south) &
      is.finite(east) &
      is.finite(north)
  ]

  if (nrow(eligible_contributors) == 0L) {
    return(
      list(
        summary = empty_summary(
          status = "no_eligible_sources",
          message = paste(
            nrow(contributors),
            "record(s) are selected for inclusion, but none",
            "currently have an eligible EPSG:4326 extent."
          ),
          included = nrow(contributors),
          eligible = 0L,
          skipped = nrow(contributors)
        ),
        contributors = contributors
      )
    )
  }

  xmin <- min(
    eligible_contributors$west,
    na.rm = TRUE
  )

  ymin <- min(
    eligible_contributors$south,
    na.rm = TRUE
  )

  xmax <- max(
    eligible_contributors$east,
    na.rm = TRUE
  )

  ymax <- max(
    eligible_contributors$north,
    na.rm = TRUE
  )

  skipped_count <- nrow(contributors) -
    nrow(eligible_contributors)

  summary <- data.table::data.table(
    bbox_status = "valid",
    status_message = paste(
      "Derived from",
      nrow(eligible_contributors),
      "eligible included spatial record(s).",
      if (skipped_count > 0L) {
        paste(
          skipped_count,
          "included record(s) were skipped because",
          "they are no longer eligible."
        )
      } else {
        ""
      }
    ),
    bbox_crs = "EPSG:4326",
    bbox_wgs84 = format_bbox_text(
      c(
        xmin,
        ymin,
        xmax,
        ymax
      )
    ),
    xmin = xmin,
    ymin = ymin,
    xmax = xmax,
    ymax = ymax,
    included_source_count = nrow(contributors),
    eligible_source_count = nrow(eligible_contributors),
    skipped_source_count = skipped_count,
    contributor_keys = paste(
      eligible_contributors$source_key,
      collapse = "; "
    ),
    derivation_method = "included_source_envelope"
  )

  list(
    summary = summary,
    contributors = contributors
  )
}

safe_epsg <- function(crs_obj) {
  epsg <- NA_character_
  try({
    if (!is.null(crs_obj$epsg) && !is.na(crs_obj$epsg)) {
      epsg <- as.character(crs_obj$epsg)
    }
  }, silent = TRUE)
  epsg
}

extract_vector_metadata_terra <- function(path, format_label, layer = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    return(data.table(
      file_path = path,
      data_file_format = format_label,
      spatial_status = "terra package not available"
    ))
  }
  res <- tryCatch(
    read_terra_vector_safely(path, layer = layer),
    error = function(e) {
      return(list(
        object = NULL,
        warnings = character(),
        error = conditionMessage(e)
      ))
    }
  )
  if (is.null(res$object)) {
    return(data.table(
      file_path = path,
      data_file_format = format_label,
      spatial_status = paste("error:", res$error),
      spatial_type = NA_character_,
      layer_name = layer %||% NA_character_,
      crs_authority = NA_character_,
      crs_code = NA_character_,
      crs_auth_code = NA_character_,
      epsg_code = NA_character_,
      crs_name = NA_character_,
      crs_input = NA_character_,
      bbox_native = NA_character_,
      bbox_order = "xmin; ymin; xmax; ymax",
      geometry_type = NA_character_,
      feature_count = NA_integer_,
      raster_nrow = NA_integer_,
      raster_ncol = NA_integer_,
      raster_nlyr = NA_integer_,
      raster_res_x = NA_real_,
      raster_res_y = NA_real_,
      spatial_note = NA_character_
    ))
  }
  v <- res$object
  e <- terra::ext(v)
  crs_info <- extract_terra_crs_info(v)
  geom_type <- tryCatch(
    paste(unique(terra::geomtype(v)), collapse = ", "),
    error = function(e) NA_character_
  )
  feature_count <- tryCatch(
    nrow(v),
    error = function(e) NA_integer_
  )
  spatial_note <- normalise_spatial_note(res$warnings)
  data.table(
    file_path = path,
    data_file_format = format_label,
    spatial_status = "ok",
    spatial_type = "vector",
    layer_name = layer %||% NA_character_,
    crs_authority = crs_info$crs_authority,
    crs_code = crs_info$crs_code,
    crs_auth_code = crs_info$crs_auth_code,
    epsg_code = crs_info$epsg_code,
    crs_name = crs_info$crs_name,
    crs_input = crs_info$crs_input,
    bbox_native = paste(
      round(e$xmin, 6),
      round(e$ymin, 6),
      round(e$xmax, 6),
      round(e$ymax, 6),
      sep = "; "
    ),
    bbox_order = "xmin; ymin; xmax; ymax",
    geometry_type = geom_type,
    feature_count = feature_count,
    raster_nrow = NA_integer_,
    raster_ncol = NA_integer_,
    raster_nlyr = NA_integer_,
    raster_res_x = NA_real_,
    raster_res_y = NA_real_,
    spatial_note = spatial_note
  )
}

extract_vector_metadata_sf <- function(path, format_label, layer = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(data.table(
      file_path = path,
      data_file_format = format_label,
      spatial_status = "sf package not available"
    ))
  }
  obj <- if (is.null(layer) || length(layer) == 0 || is.na(layer) || layer == "") {
    sf::st_read(path, quiet = TRUE)
  } else {
    sf::st_read(path, layer = layer, quiet = TRUE)
  }
  crs <- sf::st_crs(obj)
  bbox <- sf::st_bbox(obj)
  geom_types <- unique(as.character(sf::st_geometry_type(obj)))
  epsg <- safe_epsg(crs)
  crs_authority <- if (!is.na(epsg)) "EPSG" else NA_character_
  crs_code <- if (!is.na(epsg)) epsg else NA_character_
  crs_auth_code <- if (!is.na(epsg)) paste0("EPSG:", epsg) else NA_character_
  data.table(
    file_path = path,
    data_file_format = format_label,
    spatial_status = "ok",
    spatial_type = "vector",
    layer_name = layer %||% NA_character_,
    crs_authority = crs_authority,
    crs_code = crs_code,
    crs_auth_code = crs_auth_code,
    epsg_code = epsg,
    crs_name = crs$input %||% NA_character_,
    crs_input = as.character(crs$wkt %||% crs$input %||% NA_character_),
    bbox_native = bbox_to_text(bbox),
    bbox_order = "xmin; ymin; xmax; ymax",
    geometry_type = paste(geom_types, collapse = ", "),
    feature_count = nrow(obj),
    raster_nrow = NA_integer_,
    raster_ncol = NA_integer_,
    raster_nlyr = NA_integer_,
    raster_res_x = NA_real_,
    raster_res_y = NA_real_,
    spatial_note = NA_character_
  )
}

extract_vector_metadata <- function(path, format_label, layer = NULL) {
  terra_result <- tryCatch(
    extract_vector_metadata_terra(path, format_label, layer),
    error = function(e) NULL
  )
  if (!is.null(terra_result)) {
    no_crs <- all(is.na(terra_result$crs_input) | terra_result$crs_input == "") &&
      all(is.na(terra_result$crs_auth_code) | terra_result$crs_auth_code == "")
    if (!no_crs) {
      return(terra_result)
    }
    sf_result <- tryCatch(
      extract_vector_metadata_sf(path, format_label, layer),
      error = function(e) NULL
    )
    if (!is.null(sf_result)) {
      sf_result$spatial_note <- paste(
        terra_result$spatial_note %||% "",
        "CRS read using sf fallback.",
        sep = " "
      )
      return(sf_result)
    }
    return(terra_result)
  }
  extract_vector_metadata_sf(path, format_label, layer)
}

extract_raster_metadata <- function(path, format_label) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    return(data.table(
      file_path = path,
      data_file_format = format_label,
      spatial_status = "terra package not available"
    ))
  }
  r <- terra::rast(path)
  e <- terra::ext(r)
  crs_info <- extract_terra_crs_info(r)
  res <- terra::res(r)
  data.table(
    file_path = path,
    data_file_format = format_label,
    spatial_status = "ok",
    spatial_type = "raster",
    layer_name = NA_character_,
    crs_authority = crs_info$crs_authority,
    crs_code = crs_info$crs_code,
    crs_auth_code = crs_info$crs_auth_code,
    epsg_code = crs_info$epsg_code,
    crs_name = crs_info$crs_name,
    crs_input = crs_info$crs_input,
    bbox_native = paste(
      round(e$xmin, 6),
      round(e$ymin, 6),
      round(e$xmax, 6),
      round(e$ymax, 6),
      sep = "; "
    ),
    bbox_order = "xmin; ymin; xmax; ymax",
    geometry_type = NA_character_,
    feature_count = NA_integer_,
    raster_nrow = terra::nrow(r),
    raster_ncol = terra::ncol(r),
    raster_nlyr = terra::nlyr(r),
    raster_res_x = round(res[1], 8),
    raster_res_y = round(res[2], 8),
    spatial_note = NA_character_
  )
}

normalise_spatial_note <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  txt <- paste(x, collapse = "; ")
  if (grepl("Z coordinates ignored|M coordinates ignored", txt)) {
    return("Z/M coordinates present; extracted as 2D metadata.")
  }
  txt
}

extract_gdb_metadata <- function(path, format_label) {
  path <- normalizePath(path, mustWork = FALSE, winslash = "/")
  if (!dir.exists(path) && !file.exists(path)) {
    return(data.table::data.table(
      file_path = path,
      data_file_format = format_label,
      spatial_status = "no_layers_readable",
      spatial_type = "vector_container",
      layer_name = NA_character_,
      crs_authority = NA_character_,
      crs_code = NA_character_,
      crs_auth_code = NA_character_,
      epsg_code = NA_character_,
      crs_name = NA_character_,
      crs_input = NA_character_,
      bbox_native = NA_character_,
      bbox_order = "xmin; ymin; xmax; ymax",
      geometry_type = NA_character_,
      feature_count = NA_integer_,
      raster_nrow = NA_integer_,
      raster_ncol = NA_integer_,
      raster_nlyr = NA_integer_,
      raster_res_x = NA_real_,
      raster_res_y = NA_real_,
      spatial_note = "No readable layers found in file geodatabase."
    ))
  }
  # First try to list layers.
  layers <- character()
  if (requireNamespace("sf", quietly = TRUE)) {
    layers <- tryCatch(
      {
        x <- sf::st_layers(path)
        as.character(x$name)
      },
      error = function(e) character()
    )
  }
  # If no layer list is available, try terra::vect(path) directly.
  # This matches your known working pattern.
  if (length(layers) == 0) {
    terra_res <- tryCatch(
      {
        extract_vector_metadata_terra(
          path = path,
          format_label = format_label,
          layer = NULL
        )
      },
      error = function(e) NULL
    )
    if (!is.null(terra_res)) {
      terra_res[, spatial_note := paste(
        spatial_note %||% "",
        "Read file geodatabase directly with terra::vect(path)."
      )]
      return(terra_res[])
    }
    sf_res <- tryCatch(
      {
        extract_vector_metadata_sf(
          path = path,
          format_label = format_label,
          layer = NULL
        )
      },
      error = function(e) NULL
    )
    if (!is.null(sf_res)) {
      sf_res[, spatial_note := paste(
        spatial_note %||% "",
        "Read file geodatabase directly with sf::st_read(path)."
      )]
      return(sf_res[])
    }
    return(data.table::data.table(
      file_path = path,
      data_file_format = format_label,
      spatial_status = "no_layers_readable",
      spatial_type = "vector_container",
      layer_name = NA_character_,
      crs_authority = NA_character_,
      crs_code = NA_character_,
      crs_auth_code = NA_character_,
      epsg_code = NA_character_,
      crs_name = NA_character_,
      crs_input = NA_character_,
      bbox_native = NA_character_,
      bbox_order = "xmin; ymin; xmax; ymax",
      geometry_type = NA_character_,
      feature_count = NA_integer_,
      raster_nrow = NA_integer_,
      raster_ncol = NA_integer_,
      raster_nlyr = NA_integer_,
      raster_res_x = NA_real_,
      raster_res_y = NA_real_,
      spatial_note = "No readable layers found in file geodatabase."
    ))
  }
  # Read each layer and extract real CRS/geometry metadata.
  out <- data.table::rbindlist(
    lapply(layers, function(layer_i) {
      terra_res <- tryCatch(
        {
          extract_vector_metadata_terra(
            path = path,
            format_label = format_label,
            layer = layer_i
          )
        },
        error = function(e) NULL
      )
      if (!is.null(terra_res) &&
          "spatial_status" %in% names(terra_res) &&
          any(terra_res$spatial_status == "ok", na.rm = TRUE)) {
        terra_res[, layer_name := layer_i]
        terra_res[, spatial_note := paste(
          spatial_note %||% "",
          "Layer read with terra::vect(path, layer = layer)."
        )]
        return(terra_res[])
      }
      sf_res <- tryCatch(
        {
          extract_vector_metadata_sf(
            path = path,
            format_label = format_label,
            layer = layer_i
          )
        },
        error = function(e) {
          data.table::data.table(
            file_path = path,
            data_file_format = format_label,
            spatial_status = paste("error:", conditionMessage(e)),
            spatial_type = "vector_container",
            layer_name = layer_i,
            crs_authority = NA_character_,
            crs_code = NA_character_,
            crs_auth_code = NA_character_,
            epsg_code = NA_character_,
            crs_name = NA_character_,
            crs_input = NA_character_,
            bbox_native = NA_character_,
            bbox_order = "xmin; ymin; xmax; ymax",
            geometry_type = NA_character_,
            feature_count = NA_integer_,
            raster_nrow = NA_integer_,
            raster_ncol = NA_integer_,
            raster_nlyr = NA_integer_,
            raster_res_x = NA_real_,
            raster_res_y = NA_real_,
            spatial_note = paste("Could not read layer:", layer_i)
          )
        }
      )
      sf_res[, layer_name := layer_i]
      sf_res[, spatial_note := paste(
        spatial_note %||% "",
        "Layer read with sf::st_read(path, layer = layer) fallback."
      )]
      sf_res[]
    }),
    fill = TRUE
  )
  out[]
}

read_terra_vector_safely <- function(path, layer = NULL) {
  warnings_seen <- character()
  v <- withCallingHandlers({
    if (is.null(layer) || length(layer) == 0 || is.na(layer) || layer == "") {
      # Normal vector files: shapefile, gpkg single layer, geojson, etc.
      terra::vect(path)
    } else {
      # Only use layer when explicitly needed
      terra::vect(path, layer = layer)
    }
  }, warning = function(w) {
    msg <- conditionMessage(w)
    warnings_seen <<- c(warnings_seen, msg)
    # Muffle common non-fatal Z/M messages
    if (grepl("Z coordinates ignored|M coordinates ignored", msg)) {
      invokeRestart("muffleWarning")
    }
  })
  list(
    object = v,
    warnings = unique(warnings_seen)
  )
}

extract_spatial_metadata_one <- function(file_path, data_file_format) {
  # message("Spatial read path: ", file_path)
  # message("Spatial format: ", data_file_format)
  tryCatch({
    if (data_file_format %in% c("raster", "esri_arcinfo_grid")) {
      return(extract_raster_metadata(file_path, data_file_format))
    }
    if (data_file_format == "vector") {
      return(extract_vector_metadata(file_path, data_file_format))
    }
    if (data_file_format == "esri_file_geodatabase") {
      return(extract_gdb_metadata(file_path, data_file_format))
    }
    data.table(
      file_path = file_path,
      data_file_format = data_file_format,
      spatial_status = "not_spatial_candidate"
    )
  }, error = function(e) {
    data.table(
      file_path = file_path,
      data_file_format = data_file_format,
      spatial_status = paste("error:", conditionMessage(e))
    )
  })
}

extract_terra_crs_info <- function(x) {
  out <- list(
    crs_authority = NA_character_,
    crs_code = NA_character_,
    crs_auth_code = NA_character_,
    epsg_code = NA_character_,
    crs_name = NA_character_,
    crs_input = NA_character_
  )
  crs_txt <- tryCatch(
    suppressWarnings(terra::crs(x, describe = FALSE)),
    error = function(e) NA_character_
  )
  out$crs_input <- crs_txt
  crs_desc <- tryCatch(
    suppressWarnings(terra::crs(x, describe = TRUE)),
    error = function(e) NULL
  )
  if (is.data.frame(crs_desc) && nrow(crs_desc) > 0) {
    nm <- tolower(names(crs_desc))
    if ("name" %in% nm) {
      out$crs_name <- as.character(crs_desc[[which(nm == "name")[1]]][1])
    }
    auth_col <- which(nm == "authority")[1]
    code_col <- which(nm == "code")[1]
    if (!is.na(auth_col) && !is.na(code_col)) {
      authority <- as.character(crs_desc[[auth_col]][1])
      code <- as.character(crs_desc[[code_col]][1])
      if (!is.na(authority) && !is.na(code) && nzchar(authority) && nzchar(code)) {
        out$crs_authority <- toupper(authority)
        out$crs_code <- code
        out$crs_auth_code <- paste0(out$crs_authority, ":", out$crs_code)
        if (out$crs_authority == "EPSG") {
          out$epsg_code <- code
        }
      }
    }
  }
  # Fallback to WKT parsing if terra did not expose authority/code
  if (is.na(out$crs_auth_code) || out$crs_auth_code == "") {
    auth <- extract_auth_code_from_wkt(crs_txt)
    out$crs_authority <- auth$crs_authority
    out$crs_code <- auth$crs_code
    out$crs_auth_code <- auth$crs_auth_code
    out$epsg_code <- auth$epsg_code
  }
  out
}

extract_sf_crs_info <- function(path) {
  out <- list(
    epsg_code = NA_character_,
    crs_input = NA_character_
  )
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(out)
  }
  crs <- tryCatch(
    sf::st_crs(path),
    error = function(e) NULL
  )
  if (is.null(crs) || is.na(crs)) {
    return(out)
  }
  if (!is.null(crs$epsg) && !is.na(crs$epsg)) {
    out$epsg_code <- as.character(crs$epsg)
  }
  if (!is.null(crs$input) && !is.na(crs$input)) {
    out$crs_input <- as.character(crs$input)
  } else {
    out$crs_input <- as.character(crs$wkt %||% NA_character_)
  }
  out
}

extract_auth_code_from_wkt <- function(crs_txt) {
  out <- list(
    crs_authority = NA_character_,
    crs_code = NA_character_,
    crs_auth_code = NA_character_,
    epsg_code = NA_character_
  )
  if (is.null(crs_txt) || length(crs_txt) == 0 || is.na(crs_txt) || crs_txt == "") {
    return(out)
  }
  # Prefer a CRS-level terminal ID at the end of WKT2, e.g. ID["EPSG",32736]]
  terminal_match <- regmatches(
    crs_txt,
    regexpr('ID\\["[A-Za-z0-9_]+",[0-9]+\\]\\]*\\s*$', crs_txt)
  )
  if (length(terminal_match) == 0 || terminal_match == "") {
    return(out)
  }
  authority <- sub('ID\\["([A-Za-z0-9_]+)",[0-9]+\\].*$', "\\1", terminal_match)
  code <- sub('ID\\["[A-Za-z0-9_]+",([0-9]+)\\].*$', "\\1", terminal_match)
  out$crs_authority <- toupper(authority)
  out$crs_code <- code
  out$crs_auth_code <- paste0(out$crs_authority, ":", out$crs_code)
  if (out$crs_authority == "EPSG") {
    out$epsg_code <- code
  }
  out
}

normalise_spatial_unit_label <- function(unit) {
  if (
    is.null(unit) ||
    length(unit) == 0L ||
    is.na(unit[1]) ||
    !nzchar(trimws(as.character(unit[1])))
  ) {
    return("native units")
  }

  unit <- trimws(
    as.character(unit[1])
  )

  unit_key <- tolower(unit)

  switch(
    unit_key,

    "metre" = "m",
    "metres" = "m",
    "meter" = "m",
    "meters" = "m",

    "kilometre" = "km",
    "kilometres" = "km",
    "kilometer" = "km",
    "kilometers" = "km",

    "degree" = "degrees",
    "degrees" = "degrees",

    "foot" = "ft",
    "feet" = "ft",

    "us survey foot" = "US survey ft",
    "us survey feet" = "US survey ft",

    unit
  )
}


spatial_record_crs_unit <- function(record) {
  crs_result <- resolve_spatial_record_crs(
    record
  )

  if (!isTRUE(crs_result$valid)) {
    return("native units")
  }

  unit <- tryCatch(
    crs_result$crs$units_gdal,
    error = function(e) {
      NA_character_
    }
  )

  normalise_spatial_unit_label(
    unit
  )
}


format_spatial_measurement <- function(
  x,
  significant_digits = 8L
) {
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
      digits = significant_digits
    ),
    scientific = FALSE,
    trim = TRUE,
    big.mark = ""
  )
}


format_raster_cell_size <- function(record) {
  x_resolution <- spatial_record_numeric(
    record,
    "raster_res_x"
  )

  y_resolution <- spatial_record_numeric(
    record,
    "raster_res_y"
  )

  if (
    !is.finite(x_resolution) ||
    !is.finite(y_resolution)
  ) {
    return("Not recorded")
  }

  unit <- spatial_record_crs_unit(
    record
  )

  paste(
    format_spatial_measurement(
      x_resolution
    ),
    "x",
    format_spatial_measurement(
      y_resolution
    ),
    unit
  )
}

extract_spatial_metadata <- function(files_dt, data_dir = NULL) {
  if ("message" %in% names(files_dt)) {
    return(data.table::data.table(
      message = "No scanned files available for spatial extraction."
    ))
  }
  files_dt <- data.table::as.data.table(data.table::copy(files_dt))
  candidates <- files_dt[is_spatial_candidate(files_dt)]
  if (nrow(candidates) == 0) {
    return(data.table::data.table(
      message = "No primary spatial datasets detected."
    ))
  }
  # Prefer logical/enriched path and format.
  if ("logical_file_path" %in% names(candidates)) {
    candidates[
      !is.na(logical_file_path) & logical_file_path != "",
      file_path := logical_file_path
    ]
  }
  if ("logical_format" %in% names(candidates)) {
    candidates[
      !is.na(logical_format) & logical_format != "",
      data_file_format := logical_format
    ]
  }
  # Ensure file_path is character.
  candidates[, file_path := as.character(file_path)]
  # If the path does not exist and we have data_dir + logical_rel_path,
  # reconstruct an absolute path.
  if (!is.null(data_dir) && length(data_dir) > 0 && !is.na(data_dir) && data_dir != "") {
    data_dir_norm <- normalizePath(data_dir, mustWork = FALSE, winslash = "/")
    if ("logical_rel_path" %in% names(candidates)) {
      idx_missing <- is.na(candidates$file_path) |
        candidates$file_path == "" |
        !file.exists(candidates$file_path)
      candidates[
        idx_missing &
          !is.na(logical_rel_path) &
          logical_rel_path != "",
        file_path := file.path(data_dir_norm, logical_rel_path)
      ]
    }
    if ("data_file_rel_path_full" %in% names(candidates)) {
      idx_missing <- is.na(candidates$file_path) |
        candidates$file_path == "" |
        !file.exists(candidates$file_path)
      candidates[
        idx_missing &
          !is.na(data_file_rel_path_full) &
          data_file_rel_path_full != "",
        file_path := file.path(data_dir_norm, data_file_rel_path_full)
      ]
    }
  }
  # Normalise paths after repair.
  candidates[
    !is.na(file_path) & file_path != "",
    file_path := normalizePath(file_path, mustWork = FALSE, winslash = "/")
  ]
  res <- data.table::rbindlist(
    lapply(seq_len(nrow(candidates)), function(i) {
      extract_spatial_metadata_one(
        candidates$file_path[i],
        candidates$data_file_format[i]
      )
    }),
    fill = TRUE
  )
  if (!"crs_auth_code" %in% names(res)) {
    res[, crs_auth_code := NA_character_]
  }
  if (!"crs_input" %in% names(res)) {
    res[, crs_input := NA_character_]
  }
  res[
    ,
    crs_status := mapply(
      derive_crs_status,
      crs_auth_code,
      crs_input,
      USE.NAMES = FALSE
    )
  ]
  res <- add_bbox_columns(
    res
  )
  res[]
}

derive_crs_status <- function(crs_auth_code, crs_input) {
  if (!is.na(crs_auth_code) && crs_auth_code != "") {
    return("authority_code_detected")
  }
  if (!is.na(crs_input) && crs_input != "") {
    return("crs_detected_no_authority_code")
  }
  "missing_or_unknown"
}
