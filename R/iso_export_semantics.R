# Export-profile semantics remain separate from the pinned ISO rule report.

iso_export_code_reference <- function(element, value, code_list) {
  if (element == "lan:LanguageCode") {
    code_list <- "https://www.loc.gov/standards/iso639-2/"
  } else if (element == "lan:MD_CharacterSetCode") {
    code_list <- "https://www.iana.org/assignments/character-sets/character-sets.xml"
    # Stored/UI compatibility labels stay unchanged.
    aliases <- c(utf8 = "UTF-8", utf16 = "UTF-16")
    if (value %in% names(aliases)) value <- unname(aliases[value])
  } else if (grepl("^(cit|mcc|mri|mco|mmi|mrs|msr):", element)) {
    code_list <- paste0("https://schemas.isotc211.org/resources/codelists/codelists.xml#ISO19115-1.1.",
      sub(":", ".", element, fixed = TRUE))
  }
  list(value = value, code_list = code_list)
}

iso_semantic_unavailable <- function(message, status = "unavailable") {
  list(valid = FALSE, status = status, policy = "DataPond export semantics",
    version = "1.0.0", checks = data.frame(), warnings = data.frame(),
    issues = iso_validation_issue("semantics", message))
}

validate_iso_export_semantics <- function(document, catalogue = NULL,
    python = Sys.getenv("DATAPOND_ISO_PYTHON", unset = Sys.which("python3")),
    bundle = iso_validation_resource("extdata", "iso_semantics", "1.0.0")) {
  if (is.null(document)) return(iso_semantic_unavailable(
    "Build the metadata document before checking export semantics.", "not-built"))
  if (!non_empty_scalar(python)) return(iso_semantic_unavailable(
    "Install Python 3.9+ with lxml and set DATAPOND_ISO_PYTHON to run semantic checks."))
  directory <- tempfile("iso-semantic-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "metadata.xml")
  xml2::write_xml(document, path, options = "format")
  args <- c(iso_validation_resource("iso", "validate_semantics.py"), path, "--bundle", bundle)
  if (!is.null(catalogue)) {
    companion <- file.path(directory, "catalogue.xml")
    xml2::write_xml(catalogue, companion, options = "format")
    args <- c(args, "--catalogue", companion)
  }
  result <- tryCatch(processx::run(python, args, error_on_status = FALSE,
    timeout = 60000, windows_hide_window = TRUE), error = function(e) e)
  if (inherits(result, "error")) return(iso_semantic_unavailable(conditionMessage(result)))
  report <- tryCatch(jsonlite::fromJSON(result$stdout), error = function(e) NULL)
  if (!is.list(report) || length(report$status) != 1L ||
      !report$status %in% c("pass", "fail", "unavailable") ||
      length(report$valid) != 1L || !is.logical(report$valid) || is.na(report$valid) ||
      isTRUE(report$valid) != (report$status == "pass") ||
      result$status != switch(report$status, pass = 0L, fail = 1L, unavailable = 2L))
    return(iso_semantic_unavailable("Semantic validator did not return a consistent report."))
  for (field in c("checks", "issues", "warnings"))
    if (!length(report[[field]])) report[[field]] <- data.frame()
  report
}
