# Public local consumer API. Never open a pond through get_conn(): consumers
# must not migrate catalogues, acquire writer roles, or change lifecycle state.

consumer_local <- function() {
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    stop("The pond consumer API is for trusted local R sessions. A service needs its own authenticated adapter.", call. = FALSE)
  }
}

consumer_text <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop(label, " must be one non-empty string.", call. = FALSE)
  x
}

consumer_safe_names <- function(paths) {
  if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths)) ||
      any(grepl("[\\\\:\x01-\x1f]", paths)) ||
      any(vapply(strsplit(paths, "/", fixed = TRUE), function(p)
        any(p %in% c("", ".", "..")), logical(1))) || any(grepl("/$", paths)))
    stop("Unsafe consumer file path.", call. = FALSE)
  invisible(paths)
}

consumer_path <- function(root, relative, must_work = TRUE) {
  consumer_safe_names(relative)
  if (length(relative) != 1L) stop("Expected one relative path.", call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- root
  for (part in strsplit(relative, "/", fixed = TRUE)[[1]]) {
    path <- file.path(path, part)
    link <- Sys.readlink(path)
    if (!is.na(link) && nzchar(link)) stop("Symbolic links are not supported in consumer paths.", call. = FALSE)
  }
  resolved <- normalizePath(path, winslash = "/", mustWork = must_work)
  if (!startsWith(resolved, paste0(root, "/"))) stop("Consumer path escapes its root.", call. = FALSE)
  resolved
}

consumer_connect <- function(pond) {
  DBI::dbConnect(RSQLite::SQLite(), consumer_path(pond, "metadata.db"),
    flags = RSQLite::SQLITE_RO, synchronous = NULL)
}

consumer_manifest <- function(path, hashed = TRUE) {
  rows <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!is.list(rows)) stop("Invalid consumer manifest.", call. = FALSE)
  out <- data.frame(path = character(), size = numeric(), sha256 = character())
  if (!length(rows)) return(out)
  out <- data.frame(
    path = vapply(rows, function(x) consumer_text(x$path, "Manifest path"), character(1)),
    size = vapply(rows, function(x) {
      if (!is.numeric(x$size) || length(x$size) != 1L || !is.finite(x$size) ||
          x$size < 0 || x$size != floor(x$size)) stop("Invalid manifest size.", call. = FALSE)
      as.numeric(x$size)
    }, numeric(1)),
    sha256 = vapply(rows, function(x) if (hashed) consumer_text(x$sha256, "Manifest hash") else NA_character_, character(1)))
  consumer_safe_names(out$path)
  if (anyDuplicated(tolower(out$path)) ||
      (hashed && any(!grepl("^[a-f0-9]{64}$", out$sha256))))
    stop("Duplicate paths or invalid hashes in consumer manifest.", call. = FALSE)
  out[order(out$path), , drop = FALSE]
}

consumer_check_hash <- function(path, expected) {
  if (!file.exists(path) || dir.exists(path) || !identical(release_sha(path), expected))
    stop("Consumer integrity check failed: ", basename(path), call. = FALSE)
  invisible(path)
}

consumer_empty <- function() {
  out <- data.frame(asset_ref = character(), pond_id = character(), pond_path = character(),
    dataset_uuid = character(), release_id = character(), version = character(),
    title = character(), state = character(), availability = character(),
    relative_path = character(), format = character(), is_container = logical(),
    bytes = numeric(), stringsAsFactors = FALSE)
  out$members <- I(list())
  out
}

#' Connect local ponds for data discovery and extraction
#'
#' Creates a version 1 ekznwr provider. Ponds are opened read-only when queried;
#' creating this object does not initialise or migrate them. Unavailable ponds
#' are reported by search. This API is for trusted local R sessions with file
#' access, not an authenticated Shiny or HTTP endpoint.
#' Through `ekznwr::dta_search()`, this provider searches the latest release of
#' each dataset in each pond by default. Pass `latest = FALSE` for all releases.
#'
#' @param ponds Character vector of local pond root directories.
#' @return A `datapond_source`, also an `ekznwr_dta_source` provider.
#' @export
pond_source <- function(ponds) {
  consumer_local()
  if (!is.character(ponds) || !length(ponds) || anyNA(ponds) || any(!nzchar(ponds)))
    stop("ponds must contain one or more directory paths.", call. = FALSE)
  ponds <- unique(vapply(path.expand(ponds), normalizePath, character(1), winslash = "/", mustWork = FALSE))
  source <- ekznwr::new_dta_source(
    search = function(pattern = NULL, latest = TRUE, ...)
      pond_search(source, pattern = pattern, latest = latest, ...),
    fetch = function(selection, cache) pond_fetch(source, selection, cache))
  source$ponds <- ponds
  class(source) <- c("datapond_source", class(source))
  source
}

consumer_source <- function(source) {
  consumer_local()
  if (!inherits(source, "datapond_source") || !identical(source$protocol_version, 1L) ||
      !is.character(source$ponds) || !length(source$ponds))
    stop("Expected a source created by pond_source().", call. = FALSE)
}

consumer_version <- function(pond, entry, release = NULL) {
  if (is.null(release)) {
    root <- normalizePath(entry$dataset_path[1], winslash = "/", mustWork = TRUE)
    prefix <- paste0(pond, "/")
    if (!startsWith(root, prefix)) stop("Working dataset is outside its pond.", call. = FALSE)
    # Check the recorded path as well as its resolved destination.
    recorded <- gsub("\\\\", "/", entry$dataset_path[1])
    if (!startsWith(recorded, prefix)) stop("Working dataset path must be pond-local.", call. = FALSE)
    root <- consumer_path(pond, substring(recorded, nchar(prefix) + 1L))
    manifest_file <- tempfile(fileext = ".json")
    on.exit(unlink(manifest_file), add = TRUE)
    release_archive_tool("list", root, manifest_file)
    manifest <- consumer_manifest(manifest_file, hashed = FALSE)
    if (nrow(manifest)) manifest$path <- paste0("dataset/", manifest$path)
    return(list(entry = entry, manifest = manifest, root = root, state = "working",
      availability = "available", release_id = NA_character_, version = as.character(entry$version[1])))
  }
  root <- consumer_path(pond, release$relative_path[1])
  manifest_file <- consumer_path(root, "files.json")
  consumer_check_hash(manifest_file, release$manifest_sha256[1])
  manifest <- consumer_manifest(manifest_file)
  snapshot <- consumer_path(pond, paste0(".metadata_pond/releases/", release$release_id[1], "/snapshot.sqlite"))
  consumer_check_hash(snapshot, release$snapshot_sha256[1])
  conn <- DBI::dbConnect(RSQLite::SQLite(), snapshot, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  captured <- DBI::dbReadTable(conn, "entry_tbl")
  if (nrow(captured) != 1L || !identical(captured$uuid[1], release$dataset_uuid[1]))
    stop("Release snapshot identity differs from catalogue.", call. = FALSE)
  zip <- consumer_path(root, "dataset.zip", must_work = FALSE)
  availability <- if (!identical(release$state[1], "frozen")) release$state[1] else
    if (file.exists(zip)) "available" else "missing"
  list(entry = captured, manifest = manifest, manifest_file = manifest_file,
    zip = zip, record = release, state = "frozen", availability = availability,
    release_id = release$release_id[1], version = release$version[1])
}

consumer_assets <- function(context, identity) {
  files <- context$manifest[startsWith(context$manifest$path, "dataset/"), , drop = FALSE]
  if (!nrow(files)) return(consumer_empty())
  relative <- substring(files$path, 9L)
  classified <- as.data.frame(ekznwr::inventory_enrich_groups(ekznwr::inventory_classify(
    data.frame(path = relative, size = files$size, isdir = FALSE))))
  output <- list()
  for (key in unique(classified$group_key)) {
    rows <- which(classified$group_key == key)
    group <- classified[rows, , drop = FALSE]
    roots <- unique(stats::na.omit(group$dataset_root))
    container <- length(roots) == 1L && nzchar(roots[1])
    candidates <- which(!group$file_role %in% c("sidecar", "container_support"))
    if (!length(candidates)) next
    # Each ordinary primary remains selectable even when a group has several.
    paths <- if (container) roots[1] else group$path[candidates]
    for (path in paths) {
      selected <- rows
      if (container && any(group$container_type == "esri_arcinfo_grid", na.rm = TRUE)) {
        support <- paste0(tolower(dirname(path)), "/info/")
        selected <- union(selected, which(startsWith(tolower(relative), support)))
      }
      members <- sort(files$path[selected])
      pos <- if (container) candidates[1] else match(path, group$path)
      availability <- context$availability
      if (availability == "available" && grepl("\\.shp$", path, ignore.case = TRUE)) {
        required <- paste0(sub("\\.shp$", "", tolower(path)), c(".shp", ".shx", ".dbf"))
        if (!all(required %in% tolower(relative[selected]))) availability <- "incomplete"
      }
      if (availability == "available" && group$data_file_format[pos] == "virtual_raster")
        availability <- "unsupported_dependencies"
      ref <- digest::digest(list(identity$data_pond_id, context$entry$uuid[1],
        context$state, context$release_id, path), algo = "sha256")
      item <- data.frame(asset_ref = ref, pond_id = identity$data_pond_id,
        pond_path = identity$data_pond_path, dataset_uuid = context$entry$uuid[1],
        release_id = context$release_id, version = context$version,
        title = as.character(context$entry$title[1]), state = context$state,
        availability = availability, relative_path = path,
        format = group$data_file_format[pos], is_container = container,
        bytes = sum(files$size[selected]), stringsAsFactors = FALSE)
      item$members <- I(list(members))
      output[[length(output) + 1L]] <- item
    }
  }
  if (!length(output)) return(consumer_empty())
  as.data.frame(data.table::rbindlist(output))
}

#' Search files and logical GIS assets across local ponds
#'
#' Frozen manifests and captured metadata are used for frozen releases. Working
#' folders are scanned only when requested. Search never extracts archive data.
#' Results include unavailable releases when their manifests remain readable.
#' Failures are warned about and recorded in the `problems` attribute, separately
#' from an empty result. Direct calls return all versions unless `latest = TRUE`;
#' the ekznwr provider defaults to the latest release.
#'
#' @param source A source created by [pond_source()].
#' @param pattern Optional case-insensitive regular expression matching title,
#'   relative asset path, dataset UUID or version.
#' @param state `"frozen"` (default), `"working"`, or both.
#' @param dataset_uuid Optional dataset UUID filter (one or more values).
#' @param release_id Optional exact release UUID filter (one or more values).
#'   Explicit release IDs take precedence over `latest`.
#' @param latest Logical. If `TRUE`, search only the most recently frozen release
#'   per dataset in each pond, selected before matching `pattern`. Freeze time
#'   determines recency, with descending release ID as a deterministic tie-break.
#'   An unavailable latest release never falls back to an older version. Working
#'   results are unaffected. Defaults to `FALSE` for direct calls; the
#'   `ekznwr::dta_search()` provider defaults to `TRUE`.
#' @return Data frame with exact `asset_ref` identifiers, provenance, availability,
#'   paths, formats, sizes and a list column `members` containing required files.
#' @export
pond_search <- function(source, pattern = NULL, state = "frozen", dataset_uuid = NULL, release_id = NULL, latest = FALSE) {
  consumer_source(source)
  if (!is.logical(latest) || length(latest) != 1L || is.na(latest))
    stop("latest must be one non-missing logical value.", call. = FALSE)
  state <- match.arg(state, c("frozen", "working"), several.ok = TRUE)
  if (!is.null(pattern)) {
    consumer_text(pattern, "pattern")
    grepl(pattern, "", perl = TRUE) # Reject invalid regex even for empty ponds.
  }
  output <- list(); problems <- list()
  problem <- function(pond, release, e) {
    problems[[length(problems) + 1L]] <<- data.frame(pond_path = pond,
      release_id = release, message = conditionMessage(e))
  }
  seen_ids <- character()
  for (pond in source$ponds) tryCatch({
    consumer_path(pond, ".metadata_pond/data_pond_id")
    consumer_path(pond, "metadata.db")
    identity <- read_data_pond_identity_read_only(pond)
    if (identity$data_pond_id %in% seen_ids) stop("Duplicate pond identity in source; choose one location for each pond.")
    seen_ids <- c(seen_ids, identity$data_pond_id)
    conn <- consumer_connect(pond)
    catalogue <- tryCatch({
      list(entries = DBI::dbReadTable(conn, "entry_tbl"),
        releases = if (DBI::dbExistsTable(conn, "dataset_release_tbl"))
          DBI::dbReadTable(conn, "dataset_release_tbl") else data.frame())
    }, finally = DBI::dbDisconnect(conn))
    entries <- catalogue$entries
    if ("is_hidden" %in% names(entries)) entries <- entries[is.na(entries$is_hidden) | entries$is_hidden == 0L, , drop = FALSE]
    if ("deleted_at" %in% names(entries)) entries <- entries[is.na(entries$deleted_at) | !nzchar(entries$deleted_at), , drop = FALSE]
    if (!is.null(dataset_uuid)) entries <- entries[entries$uuid %in% dataset_uuid, , drop = FALSE]
    for (i in seq_len(nrow(entries))) {
      entry <- entries[i, , drop = FALSE]
      if ("working" %in% state && is.null(release_id)) tryCatch({
        if (!is.na(entry$dataset_path[1]) && dir.exists(entry$dataset_path[1]) &&
            !startsWith(gsub("\\\\", "/", entry$dataset_path[1]), paste0(pond, "/arc/releases/"))) {
          context <- consumer_version(pond, entry)
          output[[length(output) + 1L]] <- consumer_assets(context, identity)
        }
      }, error = function(e) problem(pond, NA_character_, e))
      releases <- catalogue$releases
      if (!"frozen" %in% state || !nrow(releases)) next
      releases <- releases[releases$dataset_uuid == entry$uuid[1], , drop = FALSE]
      if (!is.null(release_id)) releases <- releases[releases$release_id %in% release_id, , drop = FALSE]
      if (isTRUE(latest) && is.null(release_id) && nrow(releases)) {
        if (!"frozen_at" %in% names(releases) || anyNA(releases$frozen_at) ||
            any(!nzchar(releases$frozen_at))) stop("Cannot determine latest release: missing freeze time.")
        releases <- releases[order(releases$frozen_at, releases$release_id,
          decreasing = TRUE)[1L], , drop = FALSE]
      }
      for (j in seq_len(nrow(releases))) tryCatch({
        context <- consumer_version(pond, entry, releases[j, , drop = FALSE])
        output[[length(output) + 1L]] <- consumer_assets(context, identity)
      }, error = function(e) problem(pond, releases$release_id[j], e))
    }
  }, error = function(e) problem(pond, NA_character_, e))
  out <- if (length(output)) as.data.frame(data.table::rbindlist(output)) else consumer_empty()
  if (!is.null(pattern) && nrow(out)) out <- out[grepl(pattern,
    paste(out$title, out$relative_path, out$dataset_uuid, out$version), ignore.case = TRUE, perl = TRUE), , drop = FALSE]
  out <- out[order(out$pond_id, out$dataset_uuid, out$release_id, out$relative_path, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "problems") <- if (length(problems)) do.call(rbind, problems) else
    data.frame(pond_path = character(), release_id = character(), message = character())
  if (length(problems)) warning("Some pond sources could not be searched; inspect attr(result, 'problems').", call. = FALSE)
  out
}

consumer_reference <- function(source, selection) {
  consumer_source(source)
  required <- c("asset_ref", "pond_id", "pond_path", "dataset_uuid", "release_id", "state")
  if (!is.data.frame(selection) || !nrow(selection) || !all(required %in% names(selection)))
    stop("Select one or more exact asset rows returned by pond_search().", call. = FALSE)
  for (field in setdiff(required, "asset_ref")) if (length(unique(selection[[field]])) != 1L)
    stop("One retrieval must select assets from one pond and one dataset version.", call. = FALSE)
  if (!selection$pond_path[1] %in% source$ponds) stop("Selection is outside this source.", call. = FALSE)
  # Rebuild authority from disk; caller-supplied paths and member lists are ignored.
  local_source <- source
  local_source$ponds <- selection$pond_path[1]
  found <- pond_search(local_source, state = selection$state[1], dataset_uuid = selection$dataset_uuid[1],
    release_id = if (identical(selection$state[1], "frozen")) selection$release_id[1] else NULL)
  if (anyDuplicated(selection$asset_ref) || any(!selection$asset_ref %in% found$asset_ref) ||
      any(!found$pond_id %in% selection$pond_id))
    stop("Selection is stale or does not identify existing assets in this pond.", call. = FALSE)
  found
}

#' List all assets in the version identified by a search result
#' @param source A source created by [pond_source()].
#' @param reference One or more search rows identifying one dataset version.
#' @return An asset data frame in the same format as [pond_search()].
#' @export
pond_assets <- function(source, reference) consumer_reference(source, reference)

consumer_cache_root <- function(cache, ponds) {
  consumer_text(cache, "cache")
  cache <- path.expand(cache)
  # Resolve the deepest existing ancestor before creating the cache.
  parent <- cache; suffix <- character()
  while (!file.exists(parent) && !dir.exists(parent)) {
    suffix <- c(basename(parent), suffix)
    next_parent <- dirname(parent)
    if (identical(parent, next_parent)) stop("Invalid cache directory.", call. = FALSE)
    parent <- next_parent
  }
  resolved <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  if (length(suffix)) {
    consumer_safe_names(paste(suffix, collapse = "/"))
    resolved <- paste(c(resolved, suffix), collapse = "/")
  }
  if (any(vapply(ponds, function(p) identical(resolved, p) || startsWith(resolved, paste0(p, "/")), logical(1))))
    stop("Use a consumer cache outside all source ponds.", call. = FALSE)
  dir.create(resolved, recursive = TRUE, showWarnings = FALSE)
  normalizePath(resolved, winslash = "/", mustWork = TRUE)
}

consumer_recheck_release <- function(pond, original) {
  conn <- consumer_connect(pond)
  on.exit(DBI::dbDisconnect(conn))
  current <- DBI::dbGetQuery(conn, "SELECT * FROM dataset_release_tbl WHERE release_id=?",
    params = list(original$release_id[1]))
  fields <- c("release_id", "dataset_uuid", "version", "relative_path", "state",
    "zip_sha256", "manifest_sha256", "snapshot_sha256")
  if (nrow(current) != 1L || !identical(current[fields], original[fields]))
    stop("Release changed or became unavailable during retrieval; retry with a fresh selection.", call. = FALSE)
}

#' Verify a partial pond consumer cache
#' @param directory Cache directory returned as `cache_dir` by [pond_fetch()].
#' @return Invisibly, the receipt. Detects missing, modified and extra payload
#'   files. Receipts are integrity records, not signatures or access controls.
#' @export
verify_pond_cache <- function(directory) {
  receipt_path <- consumer_path(directory, "receipt.json")
  receipt <- jsonlite::read_json(receipt_path)
  if (!identical(receipt$format, "datapond-consumer-cache") || !identical(receipt$version, 1L))
    stop("Unsupported consumer cache receipt.", call. = FALSE)
  manifest <- consumer_path(directory, "files.json")
  consumer_check_hash(manifest, receipt$manifest_sha256)
  expected <- consumer_manifest(manifest)
  actual_file <- tempfile(fileext = ".json")
  on.exit(unlink(actual_file), add = TRUE)
  release_archive_tool("inventory", consumer_path(directory, "payload"), actual_file)
  actual <- consumer_manifest(actual_file)
  rownames(actual) <- rownames(expected) <- NULL
  if (!identical(actual, expected)) stop("Consumer cache files differ from the receipt.", call. = FALSE)
  invisible(receipt)
}

#' Retrieve selected pond assets into a verified local cache
#'
#' Selects one dataset version per call. Required companions and entire directory
#' containers are included automatically. Frozen members are verified against
#' the registered manifest and ZIP hash; working files are copied and checked
#' against hashes captured during retrieval. Working copies are not releases.
#' Existing cache entries are verified and reused, never overwritten. A cache
#' can be temporary for R or persistent for saved GIS projects. No automatic
#' eviction or project pin registry is implemented.
#'
#' @param source A source created by [pond_source()].
#' @param selection Exact asset rows from [pond_search()] for one dataset version.
#' @param cache Directory outside the source ponds, defaulting to a temporary
#'   directory. Supply a persistent directory for saved QGIS projects.
#' @return An extraction list compatible with `ekznwr::dta_fp()`, including
#'   `paths` (selected asset paths), `cache_dir`, `receipt`, `selected_members`,
#'   `archive_dir` and `arc_file_contents`. Paths retain a `dataset/` prefix.
#' @export
pond_fetch <- function(source, selection, cache = file.path(tempdir(), "datapond-consumer")) {
  found <- consumer_reference(source, selection)
  selected <- found[match(selection$asset_ref, found$asset_ref), , drop = FALSE]
  if (any(selected$availability != "available")) stop("Selected release is unavailable: ",
    paste(unique(selected$availability), collapse = ", "), call. = FALSE)
  pond <- selected$pond_path[1]
  conn <- consumer_connect(pond)
  records <- tryCatch({
    list(entry = DBI::dbGetQuery(conn, "SELECT * FROM entry_tbl WHERE uuid=?", params = list(selected$dataset_uuid[1])),
      release = if (selected$state[1] == "frozen") DBI::dbGetQuery(conn,
        "SELECT * FROM dataset_release_tbl WHERE release_id=?", params = list(selected$release_id[1])) else NULL)
  }, finally = DBI::dbDisconnect(conn))
  context <- consumer_version(pond, records$entry, records$release)
  if (!identical(context$availability, "available")) stop("Selected version became unavailable.", call. = FALSE)
  members <- sort(unique(unlist(selected$members, use.names = FALSE)))
  expected <- context$manifest[match(members, context$manifest$path), , drop = FALSE]
  if (anyNA(expected$path)) stop("Selected members changed during retrieval.", call. = FALSE)
  if (context$state == "working") expected$sha256 <- vapply(expected$path, function(p)
    release_sha(consumer_path(context$root, substring(p, 9L))), character(1))
  cache <- consumer_cache_root(cache, source$ponds)
  key <- digest::digest(list(selected$pond_id[1], selected$dataset_uuid[1], context$state,
    context$release_id, context$record$manifest_sha256, context$record$zip_sha256,
    expected, sort(selected$asset_ref)), algo = "sha256")
  target <- consumer_path(cache, key, must_work = FALSE)
  lock <- paste0(target, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("This cache entry is busy; retry after the other retrieval finishes.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  reused <- dir.exists(target)
  if (reused) {
    receipt <- verify_pond_cache(target)
    if (!identical(receipt$cache_key, key) || !identical(receipt$pond_id, selected$pond_id[1]) ||
        !identical(receipt$dataset_uuid, selected$dataset_uuid[1]) ||
        !identical(receipt$state, context$state) || !identical(receipt$dataset_version, context$version) ||
        (context$state == "frozen" && (!identical(receipt$release_id, context$release_id) ||
          !identical(receipt$source_manifest_sha256, context$record$manifest_sha256[1]) ||
          !identical(receipt$zip_sha256, context$record$zip_sha256[1]))))
      stop("Consumer cache identity mismatch.", call. = FALSE)
    cached <- consumer_manifest(file.path(target, "files.json"))
    rownames(cached) <- rownames(expected) <- NULL
    if (!identical(cached, expected)) stop("Consumer cache differs from selected source files.", call. = FALSE)
  } else {
    stage <- tempfile(".consumer-", tmpdir = cache)
    dir.create(stage)
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    payload <- file.path(stage, "payload")
    if (context$state == "frozen") {
      consumer_check_hash(context$zip, context$record$zip_sha256[1])
      # Use private copies of the selection and manifest throughout extraction.
      selection_file <- file.path(stage, "members.json")
      release_json(as.list(members), selection_file)
      full_manifest <- file.path(stage, "source-files.json")
      if (!file.copy(context$manifest_file, full_manifest)) stop("Cannot stage release manifest.")
      consumer_check_hash(full_manifest, context$record$manifest_sha256[1])
      script <- system.file("release", "archive.py", package = "datapond", mustWork = TRUE)
      result <- processx::run(document_python_path(), c(script, "extract-selected", context$zip,
        full_manifest, "--members", selection_file, "--output", payload), error_on_status = FALSE)
      if (result$status != 0L) stop("Selected release extraction failed: ", result$stderr, call. = FALSE)
      consumer_check_hash(context$zip, context$record$zip_sha256[1])
      unlink(c(selection_file, full_manifest))
    } else {
      dir.create(payload)
      for (i in seq_len(nrow(expected))) {
        origin <- consumer_path(context$root, substring(expected$path[i], 9L))
        destination <- file.path(payload, expected$path[i])
        dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
        if (!file.copy(origin, destination, overwrite = FALSE)) stop("Cannot copy selected working file.")
        consumer_check_hash(origin, expected$sha256[i])
        consumer_check_hash(destination, expected$sha256[i])
      }
    }
    manifest_file <- file.path(stage, "files.json")
    jsonlite::write_json(expected, manifest_file, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, digits = NA)
    receipt <- list(format = "datapond-consumer-cache", version = 1L, cache_key = key,
      pond_id = selected$pond_id[1], dataset_uuid = selected$dataset_uuid[1],
      release_id = context$release_id, dataset_version = context$version, state = context$state,
      verified_at = release_now(), asset_refs = as.list(sort(selected$asset_ref)),
      manifest_sha256 = release_sha(manifest_file),
      source_manifest_sha256 = if (context$state == "frozen") context$record$manifest_sha256[1] else NULL,
      zip_sha256 = if (context$state == "frozen") context$record$zip_sha256[1] else NULL)
    release_json(receipt, file.path(stage, "receipt.json"))
    verify_pond_cache(stage)
    if (context$state == "frozen") consumer_recheck_release(pond, context$record)
    if (file.exists(target) || !file.rename(stage, target)) stop("Cannot finalise consumer cache.", call. = FALSE)
    receipt <- verify_pond_cache(target)
  }
  if (context$state == "frozen") consumer_recheck_release(pond, context$record)
  payload <- file.path(target, "payload")
  logical_paths <- paste0("dataset/", selected$relative_path)
  names(logical_paths) <- selected$asset_ref
  names_to_return <- unique(c(members, unname(logical_paths)))
  contents <- data.frame(Name = names_to_return,
    Length = expected$size[match(names_to_return, expected$path)], ext = TRUE)
  list(paths = stats::setNames(file.path(payload, logical_paths), names(logical_paths)),
    cache_dir = target, receipt = receipt, parent_dir = pond,
    arc_file = if (context$state == "frozen") paste0(context$release_id, "/dataset.zip") else NA_character_,
    arc_file_path = if (context$state == "frozen") context$zip else NA_character_,
    archive_dir = payload, extract_dir = cache, arc_file_contents = contents,
    metadata_file = file.path(target, "receipt.json"), selected_members = members,
    extracted_members = if (reused) character() else members, arc_extracted = !reused,
    msg = if (reused) "Verified consumer cache reused." else "Selected assets retrieved and verified.")
}
