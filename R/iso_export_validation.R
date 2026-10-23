# Offline ISO export conformance guardrails (GAP-018) -----------------------

iso_validation_resource <- function(...) {
  path <- system.file(..., package = "datapond")
  if (!nzchar(path)) path <- file.path("inst", ...)
  path
}

iso_validation_issue <- function(stage, message) {
  data.frame(stage = stage, message = message, location = "", rule = "",
             source = "", line = 0L, stringsAsFactors = FALSE)
}

iso_validation_unavailable <- function(message, stage = "engine") {
  list(valid = FALSE, status = "unavailable", schema_package = "1.3.0",
       checks = data.frame(), issues = iso_validation_issue(stage, message),
       adaptations = data.frame(),
       datapond_profile = list(valid = FALSE, status = "unavailable",
         policy = "DataPond creation-date precision", version = "1.0.0",
         issues = iso_validation_issue(stage, message), accepted_deviations = data.frame()))
}

#' Check a metadata XML document against the pinned ISO validation bundle
#'
#' This checks XML structure, the bundled ISO 19115-1 rules and the ISO 19157
#' MDQ measure-reference rule. It does not
#' certify factual accuracy, code-list membership, complete crosswalk coverage,
#' any linked ISO 19110 feature catalogue or standalone DQM measure catalogue.
#' It does not access a pond.
#' The top-level result retains the pinned rules. `datapond_profile` separately
#' applies version 1.0.0 of the creation-date precision policy, accepting an
#' XSD-valid creation Date as well as DateTime and listing accepted deviations.
#'
#' Python 3.9+ with lxml is required only for this optional check, not to run the
#' app. All schemas and rules are read locally and verified against SHA-256
#' hashes. Missing tooling or damaged resources return `unavailable`, not pass.
#'
#' @param document An xml2 document or path to an existing XML file.
#' @param python Python executable; defaults to DATAPOND_ISO_PYTHON or python3.
#' @param kind Metadata rules by default, or catalogue XSD, embedded citation
#'   rules and local reference checks. This does not validate a document pair.
#' @return A list with valid, status, checks, issues, adaptations and coverage.
#' @keywords internal
validate_iso_metadata_xml <- function(
    document, python = Sys.getenv("DATAPOND_ISO_PYTHON", unset = Sys.which("python3")),
    kind = c("metadata", "catalogue")) {
  kind <- match.arg(kind)
  if (length(python) != 1L || is.na(python) || !nzchar(python)) {
    return(iso_validation_unavailable(
      "Install Python 3.9+ with lxml, then set DATAPOND_ISO_PYTHON to its executable."
    ))
  }
  if (inherits(document, "xml_document") || inherits(document, "xml_node")) {
    path <- tempfile("datapond-iso-", fileext = ".xml")
    on.exit(unlink(path), add = TRUE)
    xml2::write_xml(document, path, options = "format")
  } else if (is.character(document) && length(document) == 1L &&
             !is.na(document) && file.exists(document) && !dir.exists(document)) {
    path <- document
  } else {
    stop("Supply an XML document or an existing XML file.", call. = FALSE)
  }
  runner <- iso_validation_resource("iso", "validate_metadata.py")
  bundle <- iso_validation_resource("extdata", "iso_validation", "1.3.0")
  result <- tryCatch(processx::run(
    python, c(runner, normalizePath(path, mustWork = TRUE), "--bundle", bundle, "--kind", kind),
    error_on_status = FALSE, timeout = 60000, windows_hide_window = TRUE
  ), error = function(error) error)
  if (inherits(result, "error")) {
    return(iso_validation_unavailable(paste("ISO validation could not run:", conditionMessage(result))))
  }
  report <- tryCatch(jsonlite::fromJSON(result$stdout), error = function(error) NULL)
  if (!is.list(report) || !is.character(report$status) || length(report$status) != 1L ||
      !report$status %in% c("pass", "fail", "unavailable") ||
      !is.logical(report$valid) || length(report$valid) != 1L || is.na(report$valid)) {
    return(iso_validation_unavailable(paste(
      "ISO validator did not return a usable report.", trimws(result$stderr)
    )))
  }
  expected_status <- if (isTRUE(report$valid)) 0L else if (report$status == "unavailable") 2L else 1L
  if (result$status != expected_status || (isTRUE(report$valid) != (report$status == "pass"))) {
    return(iso_validation_unavailable("ISO validator exit status disagrees with its report."))
  }
  for (field in c("checks", "issues", "adaptations")) {
    if (!is.data.frame(report[[field]]) && length(report[[field]]) == 0L) report[[field]] <- data.frame()
  }
  for (field in c("issues", "accepted_deviations")) {
    if (!is.data.frame(report$datapond_profile[[field]]) && length(report$datapond_profile[[field]]) == 0L)
      report$datapond_profile[[field]] <- data.frame()
  }
  report
}

#' Assemble existing metadata sections and lineage for export assessment
#'
#' A candidate is not a conformant export. This preserves the current section
#' serializers so validation exposes their defects instead of silently changing
#' namespaces, dropping populated sections, or inventing required values.
#'
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @return An xml2 metadata document for validation and development review.
#' @keywords internal
build_iso_metadata_candidate_xml <- function(dataset_id, base_path) {
  iso_core_attach_lineage(build_iso_wave3_metadata_xml(dataset_id, base_path), dataset_id, base_path)
}

#' Assess the current metadata export candidate without publishing it
#'
#' The existing builders may run their usual schema migrations. Use a test pond
#' or a copy when auditing an older pond. No XML is written to the dataset.
#' The full candidate includes vertical extents, with local EPSG unit/axis
#' checks. Custom references are retained with assessment warnings; external
#' CRS resources and arbitrary unit URI meanings are not verified.
#' A separate `semantics` result checks the declared DataPond export profile
#' against pinned ISO, language and encoding registries and bounded consistency
#' rules. It does not change the pinned XML outcome or imply catalogue acceptance.
#'
#' @param dataset_id Dataset record identifier.
#' @param base_path Data Pond root.
#' @param python Python executable used for validation.
#' @param sections "all" for the provisional complete candidate, or "core" for
#'   identity, discovery, resource status, metadata maintenance and implemented
#'   lineage. Core success does not
#'   establish conformance of omitted specialist sections. "distribution" adds
#'   stored distribution and public legal constraints to core metadata.
#'   "quality" additionally includes explicit quality results. "content" adds
#'   raster content and validates the linked feature catalogue and its link.
#' @return A list containing the candidate document (or NULL), validation report
#'   and crosswalk version, plus a separate semantic report. Assembly errors are
#'   distinct from validation failure.
#' @keywords internal
assess_iso_metadata_export <- function(
    dataset_id, base_path,
    python = Sys.getenv("DATAPOND_ISO_PYTHON", unset = Sys.which("python3")),
    sections = c("all", "core", "distribution", "quality", "content")) {
  sections <- match.arg(sections)
  builder <- switch(sections, core = build_iso_core_metadata_xml,
    distribution = build_iso_distribution_metadata_xml, quality = build_iso_quality_metadata_xml,
    content = build_iso_content_metadata_xml, all = build_iso_metadata_candidate_xml)
  feature_catalogue <- NULL
  feature_catalogue_filename <- NULL
  document <- tryCatch({
    result <- builder(dataset_id, base_path)
    if (sections %in% c("all", "content")) {
      feature_catalogue <- build_iso_19110_feature_catalogue_xml(dataset_id, base_path)
      if (!is.null(feature_catalogue)) feature_catalogue_filename <- paste0("feature-catalogue-",
        xml2::xml_attr(xml2::xml_root(feature_catalogue), "uuid"), ".xml")
    }
    result
  }, error = function(error) error)
  if (inherits(document, "error")) {
    report <- list(valid = FALSE, status = "not-built", schema_package = "1.3.0",
                   checks = data.frame(), adaptations = data.frame(),
                   issues = iso_validation_issue("assembly", conditionMessage(document)),
                   datapond_profile = list(valid = FALSE, status = "not-built",
                     issues = iso_validation_issue("assembly", conditionMessage(document))))
    document <- NULL
  } else {
    report <- if (sections %in% c("all", "content"))
      validate_iso_content_documents(document, feature_catalogue, feature_catalogue_filename, python) else
      validate_iso_metadata_xml(document, python = python)
  }
  report$export_scope <- sections
  if (!is.null(document)) {
    notes <- iso_vertical_export_warnings(document)
    if (nrow(notes)) {
      if (!is.data.frame(report$warnings)) report$warnings <- notes else
        report$warnings <- rbind(report$warnings, notes)
    }
  }
  if (sections == "core") report$coverage <- paste(
    "Core identity, citation, discovery, resource status, metadata maintenance and implemented lineage fields only.",
    "Excludes specialist Wave 2/3 sections and complete crosswalk coverage.", report$coverage %||% ""
  )
  if (sections == "distribution") report$coverage <- paste(
    "Core metadata and implemented lineage plus explicit distribution and public legal constraints.",
    "Excludes quality, content, catalogues, internal licence notes and complete crosswalk coverage.",
    report$coverage %||% ""
  )
  if (sections == "quality") report$coverage <- paste(
    "Core metadata, implemented lineage, explicit distribution, public legal constraints and quality results.",
    "Excludes content, linked catalogues and complete crosswalk coverage.", report$coverage %||% ""
  )
  if (sections == "content") report$coverage <- paste(
    "Core, lineage, distribution, licensing, quality, raster content and linked feature catalogue.",
    "Excludes remaining specialist sections and complete crosswalk coverage.", report$coverage %||% "")
  list(document = document, validation = report, export_scope = sections,
       semantics = validate_iso_export_semantics(document, feature_catalogue, python),
       feature_catalogue = feature_catalogue, feature_catalogue_filename = feature_catalogue_filename,
       crosswalk_version = current_iso_crosswalk_version())
}

# Validate both documents and the local link. Never follow arbitrary URLs.
validate_iso_content_documents <- function(document, catalogue = NULL, filename = NULL,
    python = Sys.getenv("DATAPOND_ISO_PYTHON", unset = Sys.which("python3"))) {
  report <- validate_iso_metadata_xml(document, python = python)
  ns <- c(iso_profile_namespaces(), iso_wave3_namespaces())
  ns <- ns[!duplicated(names(ns))]
  citations <- xml2::xml_find_all(document,
    "//mrc:MD_FeatureCatalogueDescription/mrc:featureCatalogueCitation/cit:CI_Citation", ns)
  errors <- character()
  if (is.null(catalogue)) {
    if (length(citations)) errors <- "Metadata links to a feature catalogue that was not supplied."
  } else {
    catalogue_report <- validate_iso_metadata_xml(catalogue, python = python, kind = "catalogue")
    report$feature_catalogue <- catalogue_report
    if (!isTRUE(catalogue_report$valid)) {
      report$valid <- FALSE
      report$status <- if ("unavailable" %in% c(report$status, catalogue_report$status)) "unavailable" else "fail"
      issues <- catalogue_report$issues
      if (nrow(issues)) issues$stage <- paste0("catalogue-", issues$stage)
      report$issues <- rbind(report$issues, issues)
      report$datapond_profile$issues <- rbind(report$datapond_profile$issues, issues)
    }
    uuid <- xml2::xml_attr(xml2::xml_root(catalogue), "uuid")
    expected <- if (!is.na(uuid)) paste0("feature-catalogue-", uuid, ".xml") else NA_character_
    links <- xml2::xml_text(xml2::xml_find_all(citations, "cit:onlineResource/cit:CI_OnlineResource/cit:linkage/gco:CharacterString", ns))
    identifiers <- xml2::xml_text(xml2::xml_find_all(citations, "cit:identifier/mcc:MD_Identifier/mcc:code/gco:CharacterString", ns))
    catalogue_identifiers <- xml2::xml_text(xml2::xml_find_all(catalogue, "./gfc:identifier/mcc:MD_Identifier/mcc:code/gco:CharacterString", ns))
    if (length(citations) != 1L || is.na(expected) || !identical(filename, expected) ||
        !identical(links, expected) || !identical(identifiers, uuid) || !identical(catalogue_identifiers, uuid))
      errors <- c(errors, "Feature catalogue filename, link and identifiers must agree exactly.")
    catalogue_ns <- c(ns, cat = "https://schemas.isotc211.org/19139/-/cat/1.2")
    for (pair in list(c("cit:title", "cat:name"), c("cit:edition", "cat:versionNumber"),
                      c("cit:date/cit:CI_Date/cit:date", "cat:versionDate"))) {
      a <- xml2::xml_text(xml2::xml_find_all(citations, paste0(pair[1], "/*"), ns))
      b <- xml2::xml_text(xml2::xml_find_all(xml2::xml_root(catalogue), paste0(pair[2], "/*"), catalogue_ns))
      if (!identical(a, b)) errors <- c(errors, paste("Catalogue citation differs from catalogue:", pair[1]))
    }
    report$coverage <- paste(report$coverage, catalogue_report$coverage)
  }
  # checks tables from Python have optional columns; append without dropping them.
  check <- as.data.frame(setNames(lapply(names(report$checks), function(name)
    if (name == "stage") "catalogue-link" else if (name == "valid") !length(errors) else NA), names(report$checks)))
  if (ncol(check)) report$checks <- rbind(report$checks, check)
  if (length(errors)) {
    link_issues <- do.call(rbind, lapply(errors, function(message) iso_validation_issue("catalogue-link", message)))
    report$issues <- rbind(report$issues, link_issues)
    report$datapond_profile$issues <- rbind(report$datapond_profile$issues, link_issues)
    report$valid <- FALSE
    if (report$status != "unavailable") report$status <- "fail"
  }
  report$datapond_profile$valid <- report$status != "unavailable" && nrow(report$datapond_profile$issues) == 0L
  report$datapond_profile$status <- if (report$status == "unavailable") "unavailable" else
    if (report$datapond_profile$valid) "pass" else "fail"
  report$coverage <- paste("Document pair checked, including local catalogue identity and citation agreement.",
    sub("linked feature-catalogue or ", "", report$coverage, fixed = TRUE))
  report
}
