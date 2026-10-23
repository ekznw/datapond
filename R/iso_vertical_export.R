# Vertical coverage: explicit references and locally resolved EPSG CRSs -----

iso_vertical_epsg <- function(value) {
  value <- iso_optional_text(value)
  if (is.na(value)) return(NA_character_)
  patterns <- c("^EPSG:([0-9]+)$", "^urn:ogc:def:crs:EPSG::([0-9]+)$",
    "^https?://www[.]opengis[.]net/def/crs/EPSG/0/([0-9]+)$",
    "^https?://epsg[.]io/([0-9]+)/?$")
  for (pattern in patterns) if (grepl(pattern, value, ignore.case = TRUE))
    return(paste0("EPSG:", sub(pattern, "\\1", value, ignore.case = TRUE)))
  if (grepl("^EPSG:", value, ignore.case = TRUE))
    stop("Vertical EPSG identifier must have the form EPSG:<numeric code>.", call. = FALSE)
  NA_character_
}

iso_vertical_public_uri <- function(value, label) {
  value <- iso_optional_text(value)
  if (!is.na(value) && !grepl("^(https?://[^/?#[:space:]]+[^[:space:]]*|urn:[^[:space:]]+)$", value))
    stop(paste(label, "must be an absolute HTTP(S) URI or URN; local file references cannot be exported."), call. = FALSE)
  value
}

iso_vertical_unit <- function(value) {
  # Equivalence only: no conversion, and international/survey feet stay distinct.
  value <- iso_optional_text(value)
  aliases <- list(m = c("m", "metre", "metres", "meter", "meters"),
    ft = c("ft", "foot", "feet", "international foot", "[ft_i]"),
    ftUS = c("ftUS", "US survey foot", "US survey feet", "us-ft", "[ft_us]"))
  for (key in names(aliases)) if (!is.na(value) && value %in% aliases[[key]]) return(key)
  value
}

iso_vertical_uri_unit <- function(uri) {
  # A deliberately small verified vocabulary. Other URIs are preserved but
  # never dereferenced or asserted equivalent to the recorded symbol.
  if (is.na(uri)) return(NA_character_)
  uri <- sub("^https:", "http:", uri)
  unname(c("http://qudt.org/vocab/unit/M" = "m", "http://qudt.org/vocab/unit/FT" = "ft")[uri])
}

iso_vertical_reference <- function(row) {
  identifier <- iso_optional_text(row$vertical_crs_identifier)
  uri <- iso_vertical_public_uri(row$vertical_crs_uri, "Vertical CRS URI")
  unit_uri <- iso_vertical_public_uri(row$unit_uri, "Vertical unit URI")
  symbol <- iso_vertical_unit(row$unit_symbol)
  uri_unit <- iso_vertical_uri_unit(unit_uri)
  if (!is.na(uri_unit) && !identical(symbol, uri_unit))
    stop("Vertical unit symbol conflicts with the recorded unit URI.", call. = FALSE)
  codes <- unique(stats::na.omit(c(iso_vertical_epsg(identifier), iso_vertical_epsg(uri))))
  if (length(codes) > 1L) stop("Vertical CRS identifier and URI refer to different EPSG codes.", call. = FALSE)
  definition <- NULL
  if (length(codes)) {
    # Only canonical authority codes reach GDAL. No saved URL, path or arbitrary
    # user string is opened, and no coordinate transformation/grid download runs.
    crs <- tryCatch(suppressWarnings(sf::st_crs(codes[1])), error = function(e) NULL)
    if (is.null(crs) || is.na(crs)) stop(paste("Vertical CRS cannot be resolved in the local PROJ database:", codes[1]), call. = FALSE)
    json <- tryCatch(jsonlite::fromJSON(crs$ProjJson, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(json)) stop("The local GDAL/PROJ library cannot inspect the vertical CRS definition.", call. = FALSE)
    if (!identical(json$type, "VerticalCRS")) stop(paste(codes[1],
      "is not a standalone vertical CRS. Supply its vertical component, not a horizontal, geographic 3D or compound CRS."), call. = FALSE)
    axes <- json$coordinate_system$axis
    if (length(axes) != 1L) stop("Vertical CRS must have exactly one recorded axis.", call. = FALSE)
    axis <- axes[[1]]
    axis_unit <- if (is.list(axis$unit)) axis$unit$name else axis$unit
    if (is.na(iso_optional_text(axis_unit)) || !identical(symbol, iso_vertical_unit(axis_unit)))
      stop(paste0("Vertical unit '", row$unit_symbol, "' does not match ", codes[1],
        " axis unit '", axis_unit, "'. Correct the units or CRS; export does not convert values."), call. = FALSE)
    type <- iso_optional_text(row$vertical_crs_type)
    if ((identical(type, "depth") && !identical(axis$direction, "down")) ||
        (identical(type, "geoidal") && !identical(axis$direction, "up")) || identical(type, "barometric"))
      stop("Recorded vertical reference type conflicts with the EPSG vertical axis.", call. = FALSE)
    definition <- crs$wkt
    if (is.na(identifier)) identifier <- codes[1]
  }
  list(identifier = identifier, uri = uri, definition = definition, unit_uri = unit_uri)
}

iso_add_vertical_extents <- function(identification, rows) {
  for (i in seq_len(nrow(rows))) {
    # Recheck stored rows: database/import edits may bypass the form validator.
    row <- validate_vertical_extent_row(rows[i, , drop = FALSE])
    reference <- iso_vertical_reference(row)
    extent <- xml2::xml_add_child(xml2::xml_add_child(identification, "mri:extent"), "gex:EX_Extent")
    details <- c(Label = iso_optional_text(row$extent_label), Note = iso_optional_text(row$description),
      Unit = row$unit_symbol, `Unit URI` = reference$unit_uri, `Vertical CRS` = row$vertical_crs_name,
      `Recorded reference type` = iso_optional_text(row$vertical_crs_type))
    details <- details[!is.na(details)]
    iso_profile_add_text(extent, "gex:description", paste(paste0(names(details), ": ", details), collapse = "\n"))
    vertical <- xml2::xml_add_child(xml2::xml_add_child(extent, "gex:verticalElement"), "gex:EX_VerticalExtent")
    properties <- c(minimum_value = "minimumValue", maximum_value = "maximumValue")
    for (field in names(properties)) {
      iso_profile_add_text(vertical, paste0("gex:", properties[[field]]),
        format(row[[field]], digits = 17, trim = TRUE, decimal.mark = "."), "gco:Real")
    }
    if (!is.na(reference$uri)) {
      node <- xml2::xml_add_child(vertical, "gex:verticalCRS")
      xml2::xml_set_attrs(node, c("xlink:href" = reference$uri, "xlink:title" = row$vertical_crs_name))
    }
    if (!is.na(reference$identifier)) {
      system <- xml2::xml_add_child(xml2::xml_add_child(vertical, "gex:verticalCRSId"), "mrs:MD_ReferenceSystem")
      id <- reference$identifier; code_space <- NULL
      # Normalize only the explicit AUTH:CODE form; keep opaque identifiers whole.
      if (grepl("^[A-Za-z][A-Za-z0-9_.-]*:[A-Za-z0-9_.-]+$", id)) {
        parts <- strsplit(id, ":", fixed = TRUE)[[1]]
        code_space <- parts[1]; id <- parts[2]
      }
      iso_core_add_identifier(system, "mrs:referenceSystemIdentifier", id, code_space, row$vertical_crs_name)
      if (!is.null(reference$definition)) {
        crs <- xml2::xml_add_child(xml2::xml_add_child(system, "mrs:crs"), "rbc:RS_CRS")
        iso_profile_add_text(crs, "rbc:definition", reference$definition)
        iso_profile_add_code(system, "mrs:referenceSystemType", "mrs:MD_ReferenceSystemTypeCode", "vertical",
          "https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.mrs.MD_ReferenceSystemTypeCode")
      }
    } else if (is.na(reference$uri)) {
      # A name is retained in the description, not promoted into a fabricated ID.
      iso_content_required(vertical, "gex:verticalCRS", NA_character_)
    }
  }
  invisible(identification)
}

iso_vertical_export_warnings <- function(document) {
  ns <- c(iso_profile_namespaces(), rbc = "https://schemas.isotc211.org/19111/-/rbc/3.1")
  rows <- data.frame(location = character(), reason = character())
  for (node in xml2::xml_find_all(document, "//gex:EX_VerticalExtent", ns)) {
    if (!length(xml2::xml_find_all(node, ".//rbc:definition|gex:verticalCRS[@gco:nilReason]", ns))) {
      rows[nrow(rows) + 1L, ] <- list(xml2::xml_path(node),
        "Vertical CRS reference retained without a locally resolved definition; unit and axis compatibility are unverified.")
    }
  }
  rows
}
