# Remaining spatial mappings for the full export candidate -----------------

iso_legacy_spatial_values <- function(conn, dataset_id) {
  empty <- list(bbox = data.frame(), crs = data.frame())
  # An excluded or failed modern record is a decision, not missing legacy data.
  for (table in c("dataset_spatial_summary_tbl", "data_file_spatial_metadata_tbl",
                  "dataset_manual_spatial_extent_tbl")) {
    count <- DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) AS n FROM ", table,
      " WHERE dataset_id=?"), params = list(as.integer(dataset_id)))$n
    if (count > 0) return(empty)
  }
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id=?",
                          params = list(as.integer(dataset_id)))
  code <- iso_optional_text(record$epsg_code)
  if (is.na(code)) return(empty)
  code <- toupper(code)
  if (grepl("^[0-9]+$", code)) code <- paste0("EPSG:", code)
  if (!grepl("^EPSG:[0-9]+$", code))
    stop("Legacy EPSG code must be a numeric EPSG identifier.", call. = FALSE)
  crs <- tryCatch(suppressWarnings(sf::st_crs(code)), error = function(e) NULL)
  if (is.null(crs) || is.na(crs)) stop("Legacy EPSG code cannot be resolved locally.", call. = FALSE)
  empty$crs <- data.frame(crs_auth_code = code, crs_name = crs$Name, crs_input = code)
  text <- iso_optional_text(record$bbox)
  if (!is.na(text)) {
    if (code != "EPSG:4326") stop("Legacy bounds need an explicit EPSG:4326 CRS; migrate projected bounds before exporting.", call. = FALSE)
    bounds <- normalise_bbox_record(bbox_native = text)
    if (bounds$bbox_status != "valid") stop("Legacy bounds need xmin; ymin; xmax; ymax in that order.", call. = FALSE)
    empty$bbox <- data.frame(xmin = bounds$xmin, ymin = bounds$ymin,
      xmax = bounds$xmax, ymax = bounds$ymax, bbox_crs = code,
      derivation_method = "Recorded legacy WGS84 bounds (not recalculated).",
      contributor_keys = NA_character_)
  }
  empty
}

iso_append_supplemental <- function(identification, text) {
  text <- iso_optional_text(text)
  if (is.na(text)) return(invisible(NULL))
  node <- xml2::xml_find_first(identification, "mri:supplementalInformation/gco:CharacterString", iso_profile_namespaces())
  if (inherits(node, "xml_missing")) iso_profile_add_text(identification, "mri:supplementalInformation", text) else
    xml2::xml_set_text(node, paste(xml2::xml_text(node), text, sep = "\n\n"))
  invisible(identification)
}

iso_spatial_export_rows <- function(conn, dataset_id) {
  rows <- DBI::dbGetQuery(conn, "SELECT * FROM data_file_spatial_metadata_tbl
    WHERE dataset_id = ? AND include_in_dataset_bbox = 1 ORDER BY id", params = list(as.integer(dataset_id)))
  files <- DBI::dbGetQuery(conn, "SELECT * FROM data_file_tbl WHERE dataset_id = ?", params = list(as.integer(dataset_id)))
  keep <- rep(FALSE, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    hits <- if (!is.na(rows$data_file_id[i])) which(files$id == rows$data_file_id[i]) else
      which(files$data_file_rel_path_full == rows$data_file_rel_path_full[i])
    if (length(hits) != 1L) stop("An included spatial record must link to one file in this dataset.", call. = FALSE)
    keep[i] <- is.na(files$active[hits]) || files$active[hits] != 0L
    rows$data_file_id[i] <- files$id[hits]
    rows$data_file_rel_path_full[i] <- files$data_file_rel_path_full[hits]
  }
  rows <- rows[keep, , drop = FALSE]
  key <- paste(rows$data_file_id, ifelse(is.na(rows$layer_name), "", rows$layer_name), sep = "\r")
  if (anyDuplicated(key)) stop("Included spatial records contain duplicate file/layer descriptions.", call. = FALSE)
  rows
}

iso_spatial_integer <- function(value, label, zero = FALSE) {
  if (!non_empty_scalar(value)) return(NA_real_)
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value) || value != floor(value) || value < if (zero) 0 else 1)
    stop(paste(label, "must be a", if (zero) "nonnegative" else "positive", "integer."), call. = FALSE)
  value
}

iso_spatial_crs_unit <- function(row) {
  code <- wave3_text_default(row$crs_auth_code, row$crs_input)
  if (is.na(code) || !grepl("^(EPSG|ESRI|OGC):[A-Za-z0-9_.-]+$", code))
    stop("Raster axis resolution needs a locally resolvable CRS authority code to establish its unit.", call. = FALSE)
  crs <- tryCatch(suppressWarnings(sf::st_crs(code)), error = function(e) NULL)
  if (is.null(crs) || is.na(crs)) stop("Raster resolution CRS could not be resolved locally.", call. = FALSE)
  unit <- iso_optional_text(crs$units_gdal)
  aliases <- c(metre = "m", meter = "m", degree = "deg", radian = "rad", foot = "ft", `US survey foot` = "[ft_us]")
  if (is.na(unit) || !unit %in% names(aliases)) stop("Raster resolution CRS unit is not supported; no unit will be guessed.", call. = FALSE)
  unname(aliases[unit])
}

iso_add_spatial_representation <- function(document, dataset_id, base_path) {
  root <- xml2::xml_root(document)
  xml2::xml_set_attr(root, "xmlns:msr", "https://schemas.isotc211.org/19115/-1/msr/1.3")
  identification <- xml2::xml_find_first(root, "mdb:identificationInfo/mri:MD_DataIdentification", iso_profile_namespaces())
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  rows <- iso_spatial_export_rows(conn, dataset_id)
  representations <- character()
  has_resolution <- FALSE
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    path <- iso_portable_reference(row$data_file_rel_path_full)
    if (is.na(path)) stop("Spatial representation needs a portable file reference.", call. = FALSE)
    label <- paste(c(path, if (non_empty_scalar(row$layer_name)) row$layer_name), collapse = " / ")
    type <- tolower(wave3_text_default(row$spatial_type, ""))
    representation <- switch(type, vector = "vector", vector_container = "vector", raster = "grid",
      grid = "grid", table = "textTable", tabular = "textTable", texttable = "textTable",
      tin = "tin", stereomodel = "stereoModel", video = "video", NA_character_)
    if (is.na(representation)) stop(paste("Unmapped included spatial type:", type), call. = FALSE)
    representations <- union(representations, representation)
    if (representation == "vector") {
      vector <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:spatialRepresentationInfo"), "msr:MD_VectorSpatialRepresentation")
      geometry <- toupper(wave3_text_default(row$geometry_type, ""))
      geometry_types <- trimws(strsplit(geometry, ",", fixed = TRUE)[[1]])
      # terra stores plural family names; sf stores OGC geometry names.
      families <- unname(c(POINT = "point", MULTIPOINT = "point", POINTS = "point",
        LINESTRING = "curve", MULTILINESTRING = "curve", LINES = "curve",
        POLYGON = "surface", MULTIPOLYGON = "surface", POLYGONS = "surface")[geometry_types])
      object_type <- if (length(families) && !anyNA(families) && length(unique(families)) == 1L)
        families[1] else NA_character_
      count <- iso_spatial_integer(row$feature_count, "Feature count", zero = TRUE)
      if (nzchar(geometry) || !is.na(count)) {
        object <- xml2::xml_add_child(xml2::xml_add_child(vector, "msr:geometricObjects"), "msr:MD_GeometricObjects")
        if (is.na(object_type)) iso_content_required(object, "msr:geometricObjectType", NA_character_) else
          iso_profile_add_code(object, "msr:geometricObjectType", "msr:MD_GeometricObjectTypeCode", object_type,
            "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.msr.MD_GeometricObjectTypeCode")
        # A multipart/mixed feature count is not a count of its component primitives.
        if (geometry %in% c("POINT", "LINESTRING", "POLYGON") && !is.na(count))
          iso_profile_add_text(object, "msr:geometricObjectCount", format(count, scientific = FALSE, trim = TRUE), "gco:Integer")
      }
      details <- c(paste("Vector file/layer:", label), if (nzchar(geometry)) paste("Recorded geometry:", row$geometry_type),
        if (!is.na(count)) paste("Recorded feature count:", format(count, scientific = FALSE, trim = TRUE)))
      iso_append_supplemental(identification, paste(details, collapse = "\n"))
    } else if (representation == "grid") {
      grid <- xml2::xml_add_child(xml2::xml_add_child(root, "mdb:spatialRepresentationInfo"), "msr:MD_GridSpatialRepresentation")
      bands <- iso_spatial_integer(row$raster_nlyr, "Raster band/layer count")
      iso_content_required(grid, "msr:numberOfDimensions", if (!is.na(bands) && bands == 1) 2L else NA_integer_, "gco:Integer")
      resolutions <- c(row$raster_res_y, row$raster_res_x)
      populated <- vapply(resolutions, non_empty_scalar, logical(1))
      unit <- if (any(populated)) iso_spatial_crs_unit(row) else NULL
      has_resolution <- has_resolution || any(populated)
      for (axis in seq_len(2L)) {
        dimension <- xml2::xml_add_child(xml2::xml_add_child(grid, "msr:axisDimensionProperties"), "msr:MD_Dimension")
        iso_profile_add_code(dimension, "msr:dimensionName", "msr:MD_DimensionNameTypeCode", c("row", "column")[axis],
          "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.msr.MD_DimensionNameTypeCode")
        size <- iso_spatial_integer(c(row$raster_nrow, row$raster_ncol)[axis], "Raster axis size")
        iso_content_required(dimension, "msr:dimensionSize", if (is.na(size)) NA_character_ else format(size, scientific = FALSE, trim = TRUE), "gco:Integer")
        if (populated[axis]) {
          value <- suppressWarnings(as.numeric(resolutions[axis]))
          if (!is.finite(value) || value <= 0) stop("Raster resolutions must be finite positive values.", call. = FALSE)
          measure <- iso_profile_add_text(dimension, "msr:resolution", format(value, digits = 17, trim = TRUE, decimal.mark = "."), "gco:Measure")
          xml2::xml_set_attr(measure, "uom", unit)
        }
        iso_profile_add_text(dimension, "msr:dimensionTitle", paste(label, c("rows", "columns")[axis]))
      }
      iso_content_required(grid, "msr:cellGeometry", NA_character_)
      iso_content_required(grid, "msr:transformationParameterAvailability", NA_character_)
      if (!is.na(bands)) iso_append_supplemental(identification, paste0("Raster file/layer: ", label, "\nRecorded band/layer count: ", bands))
    }
  }
  for (type in representations) iso_profile_add_code(identification, "mri:spatialRepresentationType", "mcc:MD_SpatialRepresentationTypeCode", type,
    "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.mcc.MD_SpatialRepresentationTypeCode")
  record <- DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE id=?", params = list(as.integer(dataset_id)))
  software <- iso_optional_text(record$software_desc)
  tools <- DBI::dbGetQuery(conn, "SELECT tool_software_reference FROM dataset_lineage_process_step_tbl WHERE dataset_id=?", params=list(as.integer(dataset_id)))
  if (!is.na(software) && !any(vapply(tools$tool_software_reference, non_empty_scalar, logical(1))))
    iso_append_supplemental(identification, paste("Legacy software description:", software))
  cell <- iso_optional_text(record$cell_size)
  if (!is.na(cell) && !has_resolution) {
    parsed <- regmatches(cell, regexec("^([0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?)[[:space:]]*(m|km|cm|mm|ft)$", cell))[[1]]
    if (length(parsed) && is.finite(as.numeric(parsed[2])) && as.numeric(parsed[2]) > 0) {
      resolution <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:spatialResolution"), "mri:MD_Resolution")
      distance <- iso_profile_add_text(resolution, "mri:distance", parsed[2], "gco:Distance")
      xml2::xml_set_attr(distance, "uom", utils::tail(parsed, 1))
    } else iso_append_supplemental(identification, paste("Legacy cell size:", cell))
  }
  iso_core_order_metadata(document)
}

iso_extent_derivation_note <- function(conn, dataset_id, summary) {
  if (!nrow(summary)) return(NULL)
  method <- iso_optional_text(summary$derivation_method[1])
  notes <- if (!is.na(method)) paste("Geographic extent derivation:", if (method == "included_source_envelope")
    "Envelope of the included spatial bounds in WGS84." else method) else character()
  keys <- split_stored_multi_value(summary$contributor_keys[1])
  manual <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_manual_spatial_extent_tbl WHERE dataset_id=? AND active=1
    AND include_in_dataset_bbox=1 AND NOT EXISTS (SELECT 1 FROM data_file_tbl f WHERE f.id=dataset_manual_spatial_extent_tbl.data_file_id AND COALESCE(f.active,1)=0)
    ORDER BY id", params = list(as.integer(dataset_id)))
  for (i in seq_len(nrow(manual))) if (paste0("manual:", manual$id[i]) %in% keys) {
    row <- manual[i, ]
    values <- c(Label = iso_optional_text(row$extent_label), Source = iso_optional_text(row$source_note), CRS = iso_optional_text(row$crs_auth_code))
    values <- values[!is.na(values)]
    if (length(values)) notes <- c(notes, paste("Contributing manual extent:", paste(paste0(names(values), ": ", values), collapse = "; ")))
  }
  if (length(notes)) paste(notes, collapse = "\n") else NULL
}
