# Versioned ISO crosswalk access -------------------------------------------

iso_crosswalk_root <- function() {
  root <- system.file("extdata", "iso_crosswalk", package = "datapond")
  if (!nzchar(root)) root <- file.path("inst", "extdata", "iso_crosswalk")
  root
}

current_iso_crosswalk_version <- function(prefix = FALSE) {
  pointer <- file.path(iso_crosswalk_root(), "CURRENT")
  if (!file.exists(pointer)) stop("The ISO crosswalk CURRENT pointer is missing.", call. = FALSE)
  value <- trimws(readLines(pointer, warn = FALSE, n = 1L))
  if (!grepl("^v[0-9]+[.][0-9]+[.][0-9]+$", value)) stop("The ISO crosswalk CURRENT pointer is invalid.", call. = FALSE)
  if (isTRUE(prefix)) value else sub("^v", "", value)
}

load_current_iso_crosswalk <- function() {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required to load the ISO crosswalk.", call. = FALSE)
  directory <- file.path(iso_crosswalk_root(), current_iso_crosswalk_version(prefix = TRUE))
  list(
    manifest = yaml::read_yaml(file.path(directory, "manifest.yml")),
    mappings = yaml::read_yaml(file.path(directory, "crosswalk.yml")),
    gaps = yaml::read_yaml(file.path(directory, "profile-gaps.yml")),
    coverage = if (file.exists(file.path(directory, "mapping-coverage.yml"))) {
      yaml::read_yaml(file.path(directory, "mapping-coverage.yml"))
    } else NULL,
    validation_policy = if (file.exists(file.path(directory, "validation-policy.yml"))) {
      yaml::read_yaml(file.path(directory, "validation-policy.yml"))
    } else NULL,
    package_policy = if (file.exists(file.path(directory, "package-policy.yml"))) {
      yaml::read_yaml(file.path(directory, "package-policy.yml"))
    } else NULL,
    semantic_policy = if (file.exists(file.path(directory, "semantic-policy.yml"))) {
      yaml::read_yaml(file.path(directory, "semantic-policy.yml"))
    } else NULL,
    local_acceptance = if (file.exists(file.path(directory, "local-acceptance.yml"))) {
      yaml::read_yaml(file.path(directory, "local-acceptance.yml"))
    } else NULL,
    capture_policy = if (file.exists(file.path(directory, "capture-policy.yml"))) {
      yaml::read_yaml(file.path(directory, "capture-policy.yml"))
    } else NULL
  )
}
