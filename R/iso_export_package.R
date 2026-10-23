# Validated, self-contained metadata packages. Nothing is written to the pond.

iso_package_dependencies <- function(assessment, dataset_id, base_path) {
  files <- data.frame(path = character(), source = character(), md5 = character())
  issues <- iso_validation_issue("package", "")[FALSE, ]
  external <- character()
  fail <- function(message) {
    issues <<- rbind(issues, iso_validation_issue("package", message))
  }
  if (is.null(assessment$document)) return(list(
    valid = FALSE, status = "not-built", files = files, external = external, issues = issues))
  record <- get_dataset_record(dataset_id, base_path)
  root <- iso_optional_text(record$dataset_path)
  pond <- normalizePath(base_path, winslash = "/", mustWork = TRUE)
  if (!is.na(root) && dir.exists(root)) root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  generated <- c("metadata.xml", "validation-report.json", assessment$feature_catalogue_filename)
  ns <- xml2::xml_ns(assessment$document)
  links <- xml2::xml_find_all(assessment$document, "//cit:linkage/gco:CharacterString", ns)
  for (node in links) {
    path <- trimws(xml2::xml_text(node))
    if (identical(path, assessment$feature_catalogue_filename) &&
        length(xml2::xml_find_all(node, "ancestor::mrc:MD_FeatureCatalogueDescription", ns))) next
    if (grepl("^https?://", path, ignore.case = TRUE)) {
      external <- union(external, path)
      next
    }
    # Package only explicitly cited supporting documents and process scripts.
    # Other resources (e.g. dataset downloads) should have external URLs.
    packaged <- length(xml2::xml_find_all(node, paste0(
      "ancestor::cit:CI_Citation[starts-with(@id,'datapond-lineage-document-') or ",
      "starts-with(@id,'datapond-resource-document-')] | ancestor::mrl:reference"), ns)) > 0L
    if (!packaged) {
      if (grepl("^[A-Za-z][A-Za-z0-9+.-]*:", path) && !grepl("^(file|[A-Za-z]):", path, ignore.case = TRUE)) {
        external <- union(external, path)
      } else fail(paste("Local online reference", shQuote(path),
        "must use an external URL or be registered as a supporting document or process script."))
      next
    }
    parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    if (!nzchar(path) || is.na(iso_portable_reference(path)) ||
        grepl("[\\\\%?#:]", path) || any(parts %in% c("", ".", "..")) ||
        startsWith(path, "/") || endsWith(path, "/")) {
      fail(paste("Supporting document or script has an unsafe or ambiguous relative path:", path))
      next
    }
    if (any(tolower(path) == tolower(generated)) ||
        any(startsWith(tolower(path), paste0(tolower(generated), "/")))) {
      fail(paste("Supporting document or script conflicts with a generated package filename:", path))
      next
    }
    if (path %in% files$path) next
    if (tolower(path) %in% tolower(files$path)) {
      fail(paste("Supporting document or script paths differ only by letter case:", path))
      next
    }
    if (is.na(root) || !dir.exists(root) || !startsWith(paste0(root, "/"), paste0(pond, "/"))) {
      fail("Supporting documents need an existing saved dataset folder inside this pond.")
      next
    }
    source <- file.path(root, path)
    if (!utils::file_test("-f", source)) {
      fail(paste("Missing supporting document or process script:", path))
      next
    }
    source <- normalizePath(source, winslash = "/", mustWork = TRUE)
    if (!startsWith(source, paste0(root, "/"))) {
      fail(paste("Supporting document or script resolves outside the dataset folder:", path))
      next
    }
    checksum <- unname(tools::md5sum(source))
    if (is.na(checksum)) {
      fail(paste("Supporting document or script cannot be read:", path))
      next
    }
    files <- rbind(files, data.frame(path = path, source = source, md5 = checksum))
  }
  list(valid = !nrow(issues), status = if (nrow(issues)) "fail" else "pass",
    files = files, external = external, issues = issues)
}

assess_iso_metadata_package <- function(dataset_id, base_path) {
  assessment <- assess_iso_metadata_export(dataset_id, base_path, sections = "all")
  assessment$package <- iso_package_dependencies(assessment, dataset_id, base_path)
  assessment$assessed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  assessment
}

iso_package_selected_result <- function(assessment, policy) {
  if (identical(policy, "datapond")) assessment$validation$datapond_profile else assessment$validation
}

iso_package_report <- function(assessment, policy) {
  package <- assessment$package
  # Reports must be portable: do not expose absolute local source paths.
  if (!is.null(package$files)) package$files$source <- NULL
  list(selected_policy = policy, crosswalk_version = assessment$crosswalk_version,
    assessed_at = assessment$assessed_at, validation = assessment$validation,
    semantics = assessment$semantics,
    receiving_catalogue = list(status = "not-assessed", target = NULL),
    package = package, checksum_algorithm = "MD5",
    scope = paste("Metadata, generated feature catalogue, explicitly cited local supporting documents and scripts.",
      "External URLs are retained without fetching or checking their availability.",
      "Dataset data files are not included. Passing these checks is not ISO certification."))
}

# Small boundaries allow failure-path tests without altering operational files.
iso_package_copy <- function(from, to) file.copy(from, to, overwrite = FALSE)
iso_package_publish <- function(from, to) file.rename(from, to)

iso_publish_metadata_package <- function(assessment, output_dir, policy) {
  selected <- iso_package_selected_result(assessment, policy)
  if (!isTRUE(selected$valid) || !isTRUE(assessment$package$valid) || !isTRUE(assessment$semantics$valid)) stop(paste(
    "ISO export was not written:",
    paste(c(selected$issues$message, assessment$semantics$issues$message, assessment$package$issues$message), collapse = "; ")
  ), call. = FALSE)
  parent <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  stage <- tempfile(".iso-stage-", tmpdir = parent)
  if (!dir.create(stage)) stop("Cannot create the ISO export staging directory.", call. = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  names <- c(metadata = "metadata.xml", validation = "validation-report.json")
  if (!is.null(assessment$feature_catalogue)) {
    names <- c(names, feature_catalogue = assessment$feature_catalogue_filename)
    xml2::write_xml(assessment$feature_catalogue, file.path(stage, names[["feature_catalogue"]]), options = "format")
  }
  xml2::write_xml(assessment$document, file.path(stage, names[["metadata"]]), options = "format")
  files <- assessment$package$files
  for (i in seq_len(nrow(files))) {
    destination <- file.path(stage, files$path[i])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(iso_package_copy(files$source[i], destination)) ||
        !identical(unname(tools::md5sum(destination)), files$md5[i])) {
      stop(paste("ISO export was not written: could not copy unchanged supporting file:", files$path[i]), call. = FALSE)
    }
  }
  # Validate the bytes actually staged, including the generated catalogue pair.
  assessment$validation <- validate_iso_content_documents(
    file.path(stage, names[["metadata"]]) |> xml2::read_xml(),
    if (!is.null(assessment$feature_catalogue)) xml2::read_xml(file.path(stage, names[["feature_catalogue"]])) else NULL,
    assessment$feature_catalogue_filename)
  assessment$validation$export_scope <- "all"
  if (!isTRUE(iso_package_selected_result(assessment, policy)$valid))
    stop("ISO export was not written: staged XML validation failed.", call. = FALSE)
  assessment$semantics <- validate_iso_export_semantics(
    xml2::read_xml(file.path(stage, names[["metadata"]])),
    if (!is.null(assessment$feature_catalogue)) xml2::read_xml(file.path(stage, names[["feature_catalogue"]])) else NULL)
  if (!isTRUE(assessment$semantics$valid))
    stop("ISO export was not written: staged semantic validation failed.", call. = FALSE)
  # Preserve domain warnings from the full assessment as well.
  notes <- iso_vertical_export_warnings(assessment$document)
  if (nrow(notes)) assessment$validation$warnings <- rbind(assessment$validation$warnings, notes)
  jsonlite::write_json(iso_package_report(assessment, policy),
    file.path(stage, names[["validation"]]), pretty = TRUE, auto_unbox = TRUE, na = "null")
  destination <- file.path(parent, paste0("iso-metadata-", uuid::UUIDgenerate()))
  if (file.exists(destination) || !isTRUE(iso_package_publish(stage, destination)))
    stop("ISO export was not written: could not publish the staged package.", call. = FALSE)
  stats::setNames(file.path(destination, names), names(names))
}

iso_download_metadata_package <- function(dataset_id, base_path, file, policy) {
  zip <- Sys.which("zip")
  if (!nzchar(zip)) stop("Install the zip command to download an ISO package.", call. = FALSE)
  parent <- tempfile("datapond-iso-download-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  paths <- write_iso_wave3_documents(dataset_id, base_path, parent, policy)
  folder <- dirname(paths[["metadata"]])
  archive <- tempfile(".iso-archive-", tmpdir = dirname(file), fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  result <- processx::run(zip, c("-q", "-r", normalizePath(archive, mustWork = FALSE), basename(folder)),
    wd = parent, error_on_status = FALSE)
  if (result$status != 0L || !file.exists(archive) || !file.rename(archive, file))
    stop("Could not create the ISO package ZIP.", call. = FALSE)
  invisible(paths)
}
