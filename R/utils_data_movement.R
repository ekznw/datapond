# Direct rclone transfers and portable, read-only catalogue caches.
# No shell commands are constructed; arguments are passed separately to processx.
movement_rclone <- function() {
  executable <- getOption("datapond.rclone", unname(Sys.which("rclone")))
  if (length(executable) != 1L || !nzchar(executable))
    stop("Install rclone and add it to PATH, then restart Data Pond.", call. = FALSE)
  executable
}

# Probe locally without contacting a remote or changing rclone configuration.
movement_rclone_status <- function() {
  tryCatch({
    result <- processx::run(movement_rclone(), "version", error_on_status = FALSE,
                           timeout = 5000, windows_hide_window = TRUE)
    version <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1]][1]
    available <- identical(as.integer(result$status), 0L) &&
      !is.na(version) && grepl("^rclone v", version)
    list(available = available, version = if (available) version else "")
  }, error = function(e) list(available = FALSE, version = ""))
}

movement_remote <- function(path) {
  grepl("^[^/\\\\:]+:", path) && !grepl("^[A-Za-z]:([/\\\\]|$)", path)
}

movement_endpoint <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(trimws(path)) ||
      grepl("[\r\n]", path) || startsWith(trimws(path), "-"))
    stop("Enter a folder path or a configured rclone remote:path.", call. = FALSE)
  path <- trimws(path)
  if (grepl("^https?://|^:", path))
    stop("Use a configured rclone remote, not a SharePoint web URL.", call. = FALSE)
  path <- gsub("\\\\", "/", path)
  if (!movement_remote(path)) {
    path <- path.expand(path)
    # Resolve the nearest existing ancestor, including symlinks, for new folders.
    suffix <- character()
    while (!file.exists(path) && dirname(path) != path) {
      suffix <- c(basename(path), suffix)
      path <- dirname(path)
    }
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (length(suffix)) path <- paste(c(path, suffix), collapse = "/")
  }
  if (any(strsplit(sub("^[^:]+:", "", path), "/", fixed = TRUE)[[1]] %in% c(".", "..")))
    stop("Use a path without . or .. components.", call. = FALSE)
  if (path == "/" || grepl("^[A-Za-z]:/$", path)) path else sub("(?<!:)/+$", "", path, perl = TRUE)
}

movement_join <- function(root, relative) {
  movement_relative(relative)
  paste0(sub("/+$", "", root), if (endsWith(root, ":")) "" else "/", relative)
}

movement_relative <- function(path) {
  if (!is.character(path) || anyNA(path) || any(!nzchar(path)) ||
      any(grepl("^/|[:\\\\\r\n]", path)) ||
      any(vapply(strsplit(path, "/", fixed = TRUE), function(p) any(p %in% c("", ".", "..")), logical(1))))
    stop("Invalid relative path in catalogue.", call. = FALSE)
  invisible(path)
}

movement_cache_marker <- function(path) file.path(path, ".datapond-read-only-cache.json")

movement_assert_writable <- function(path) {
  if (is.null(path) || !length(path) || movement_remote(path)) return(invisible(TRUE))
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(movement_cache_marker(path)))
      stop("This is a read-only remote cache. Browse it in Flow; it cannot be edited or used as a transfer destination.", call. = FALSE)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  invisible(TRUE)
}

movement_overlap <- function(a, b) {
  a <- tolower(sub("/+$", "", a)); b <- tolower(sub("/+$", "", b))
  identical(a, b) || startsWith(a, paste0(b, if (endsWith(b, ":")) "" else "/")) ||
    startsWith(b, paste0(a, if (endsWith(a, ":")) "" else "/"))
}

movement_run <- function(args, timeout = 30000) {
  result <- processx::run(movement_rclone(), args, error_on_status = FALSE,
                         timeout = timeout, echo = FALSE, windows_hide_window = TRUE)
  if (result$status != 0L) stop(paste("rclone:", result$stderr), call. = FALSE)
  result$stdout
}

movement_remotes <- function() {
  # listremotes never returns OAuth tokens or other configuration secrets.
  result <- tryCatch(jsonlite::fromJSON(movement_run(c("listremotes", "--json"))), error = function(e) NULL)
  if (is.data.frame(result)) return(result[, intersect(c("name", "type", "description"), names(result)), drop = FALSE])
  names <- strsplit(trimws(movement_run("listremotes")), "\n")[[1]]
  names <- sub(":$", "", names[nzchar(names)])
  data.frame(name = names, type = rep("configured remote", length(names)), stringsAsFactors = FALSE)
}

movement_list <- function(path) {
  result <- jsonlite::fromJSON(movement_run(c("lsjson", movement_endpoint(path), "--no-mimetype", "--no-modtime")))
  if (!is.data.frame(result)) return(data.frame(Path = character(), IsDir = logical(), Size = numeric()))
  result[, c("Path", "IsDir", "Size"), drop = FALSE]
}

movement_transfer_plan <- function(source, destination, mode = "copy", excludes = character(), dry_run = TRUE) {
  source <- movement_endpoint(source); destination <- movement_endpoint(destination)
  if (!mode %in% c("copy", "sync")) stop("Choose copy or mirror sync.")
  if (movement_overlap(source, destination)) stop("Source and destination must be separate, non-overlapping folders.")
  movement_assert_writable(destination)
  if (mode == "sync" && (endsWith(destination, ":") || dirname(destination) == destination ||
                          grepl("^[A-Za-z]:/?$", destination)))
    stop("Mirror into a dedicated subfolder, not a drive or library root.")
  if (!movement_remote(destination) && file.exists(get_db_path(destination)))
    stop("Do not overwrite a working pond. Choose a dedicated backup folder or publish its catalogue.")
  if (!movement_remote(source) && file.exists(get_db_path(source)))
    stop("For a whole pond, open Flow > Run Flows and use Publish pond. This creates a consistent database snapshot of the active pond.")
  excludes <- trimws(excludes[nzchar(trimws(excludes))])
  if (any(grepl("[\r\n]", excludes))) stop("Enter one exclusion pattern per line.")
  args <- c(mode, source, destination, "--stats", "1s", "--stats-one-line", "--log-level", "INFO",
            "--retries", "3", "--low-level-retries", "10", "--transfers", "4", "--checkers", "4")
  if (mode == "sync") args <- c(args, "--delete-after")
  for (pattern in excludes) args <- c(args, "--exclude", pattern)
  if (dry_run) args <- c(args, "--dry-run")
  list(args = args, source = source, destination = destination, mode = mode, excludes = excludes)
}

movement_job_start <- function(steps, directory = tempfile("datapond-movement-"),
                               control_directory = app_state_dir(), profiles = list(), not_after = NULL) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  job <- new.env(parent = emptyenv())
  job$started <- Sys.time()
  job$steps <- lapply(steps, movement_apply_speed, profiles = profiles)
  job$cloud <- any(vapply(steps, function(step) length(movement_step_remotes(step)) > 0L, logical(1)))
  job$control_directory <- control_directory; job$ticket <- NULL
  job$not_after <- not_after
  job$index <- 0L; job$status <- "running"; job$phase <- "starting"; job$throttle <- ""
  job$directory <- directory; job$process <- NULL; job$logs <- character(); job$outputs <- character()
  tryCatch({
    if (job$cloud) movement_queue_add(job)
    movement_job_next(job)
  }, error = function(e) { movement_job_cancel(job); stop(e) })
  job
}

movement_job_next <- function(job) {
  job$index <- job$index + 1L
  if (job$index > length(job$steps)) {
    movement_queue_release(job)
    job$status <- "complete"; job$phase <- "complete"
    return(invisible(job))
  }
  job$process <- NULL; job$throttle <- ""
  job$phase <- "queued"
  if (job$cloud) movement_queue_admit(job) else movement_job_launch(job)
  invisible(job)
}

movement_job_launch <- function(job) {
  if (job$index == 1L && !is.null(job$not_after) && Sys.time() >= job$not_after)
    stop("The preview expired while waiting. Preview the flow again before running.")
  step <- job$steps[[job$index]]
  log <- file.path(job$directory, paste0("step-", job$index, ".log"))
  job$logs <- c(job$logs, log)
  output <- file.path(job$directory, paste0("step-", job$index, ".out"))
  job$outputs <- c(job$outputs, output)
  job$process <- processx::process$new(movement_rclone(), step, stdout = output, stderr = log,
    cleanup_tree = TRUE, supervise = TRUE, windows_hide_window = TRUE)
  job$phase <- "running"
  invisible(job)
}

movement_job_poll <- function(job) {
  if (job$status != "running") return(job$status)
  tryCatch({
    if (job$phase == "queued") { movement_queue_admit(job); return(job$status) }
    if (job$cloud) {
      message <- movement_throttle_message(movement_job_log(job, current = TRUE, debug = TRUE))
      if (nzchar(message)) job$throttle <- message
    }
    if (job$process$is_alive()) return(job$status)
    status <- job$process$get_exit_status()
    if (!identical(status, 0L)) {
      movement_queue_release(job); job$status <- "failed"
    } else movement_job_next(job)
  }, error = function(e) {
    movement_job_cancel(job); job$status <- "failed"; job$error <- conditionMessage(e)
  })
  job$status
}

movement_job_cancel <- function(job) {
  if (!is.null(job) && job$status == "running") {
    if (!is.null(job$process) && job$process$is_alive()) {
      job$process$kill_tree()
      job$process$wait(1000)
      if (job$process$is_alive()) stop("The process is still stopping. Keep this session open and cancel again.")
    }
    movement_queue_release(job)
    job$status <- "cancelled"; job$phase <- "cancelled"
  }
  invisible(NULL)
}

movement_job_log <- function(job, lines = 100L, current = FALSE, debug = FALSE) {
  # Read only the end of each log: large transfers must not fill Shiny memory.
  tail_text <- function(path) {
    if (!file.exists(path)) return("")
    con <- file(path, "rb"); on.exit(close(con))
    size <- file.info(path)$size
    offset <- max(0, size - 32768)
    seek(con, offset, origin = "start")
    bytes <- readBin(con, "raw", n = min(size, 32768))
    # The bounded read can start inside a UTF-8 filename. Discard that partial
    # line before decoding. Rclone writes UTF-8 even on a non-UTF-8 R locale;
    # replace malformed/in-flight bytes so log display never interrupts a job.
    if (offset > 0) {
      newline <- match(as.raw(10), bytes)
      if (!is.na(newline)) bytes <- bytes[seq_along(bytes) > newline]
    }
    iconv(rawToChar(bytes[bytes != as.raw(0)]), from = "UTF-8", to = "UTF-8", sub = "\uFFFD")
  }
  indices <- if (current) intersect(min(job$index, length(job$steps)), seq_along(job$logs)) else seq_along(job$logs)
  text <- paste(vapply(indices, function(i) {
    label <- if (length(job$labels) >= i) job$labels[i] else paste("Step", i)
    paste0("[", i, "/", length(job$steps), "] ", label, "\n", tail_text(job$logs[i]))
  }, character(1)), collapse = "\n")
  text <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (!debug) text <- text[!grepl(" DEBUG[ :]+", text) |
    grepl("pacer:|retry-after|throttl|too many requests|activityLimitReached|HTTP.*(429|503)", text, ignore.case = TRUE)]
  paste(utils::tail(text, lines), collapse = "\n")
}

movement_normalise_profile <- function(x, new_id = FALSE) {
  if (!is.character(x$name) || length(x$name) != 1L || is.na(x$name) || !nzchar(trimws(x$name)))
    stop("Each flow needs a name.")
  mode <- x$mode %||% "copy"
  if (length(mode) != 1L || !mode %in% c("copy", "sync")) stop("Invalid flow operation.")
  excludes <- as.character(unlist(x$excludes))
  if (anyNA(excludes) || any(grepl("[\r\n]", excludes))) stop("Enter one exclusion pattern per line.")
  if (!is.null(x$id) && (length(x$id) != 1L || !is.character(x$id) || is.na(x$id) || !nzchar(x$id)))
    stop("Invalid flow identifier.")
  list(id = if (new_id || is.null(x$id)) new_uuid() else x$id,
       name = trimws(x$name), src = movement_endpoint(x$src), dest = movement_endpoint(x$dest),
       excludes = excludes, mode = mode)
}

movement_routine_plan <- function(profiles, dry_run = TRUE) {
  if (!length(profiles)) stop("Select at least one saved flow.")
  profiles <- lapply(profiles, movement_normalise_profile)
  plans <- lapply(profiles, function(x) movement_transfer_plan(x$src, x$dest, x$mode, x$excludes, dry_run))
  # Every component is previewed before any writes. Components which change
  # each other's inputs or destinations need separate previews and runs.
  for (i in seq_along(plans)) {
    if (i == 1L) next
    for (j in seq_len(i - 1L)) {
      a <- plans[[i]]; b <- plans[[j]]
      if (movement_overlap(a$destination, b$destination) ||
          movement_overlap(a$source, b$destination) || movement_overlap(b$source, a$destination)) {
        stop("Flows '", profiles[[j]]$name, "' and '", profiles[[i]]$name,
             "' use overlapping transfer locations. Preview and run them separately.")
      }
    }
  }
  list(steps = lapply(plans, `[[`, "args"), profiles = profiles,
       summary = data.frame(Component = vapply(profiles, `[[`, character(1), "name"),
                            Source = vapply(plans, `[[`, character(1), "source"),
                            Destination = vapply(plans, `[[`, character(1), "destination"),
                            Operation = vapply(plans, `[[`, character(1), "mode")))
}

movement_read_profiles <- function(path) {
  value <- jsonlite::fromJSON(path, simplifyVector = FALSE)$syncs
  if (!is.list(value)) stop("Expected a JSON object with a syncs array.")
  lapply(value, movement_normalise_profile, new_id = TRUE)
}

movement_json_write <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
}

movement_catalogue_dir <- function(root) movement_join(root, ".datapond-catalogue")

movement_snapshot <- function(pond, directory = tempfile("datapond-catalogue-")) {
  identity <- read_data_pond_identity_read_only(pond)
  movement_assert_writable(pond)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  from <- DBI::dbConnect(RSQLite::SQLite(), identity$database_path, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(from), add = TRUE)
  to <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(directory))
  on.exit(DBI::dbDisconnect(to), add = TRUE)
  # SQLite's backup API includes committed WAL contents, without touching source.
  RSQLite::sqliteCopyDatabase(from, to)
  rows <- DBI::dbGetQuery(to, "SELECT * FROM entry_tbl")
  datasets <- list(); metadata_files <- character()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    if (("is_hidden" %in% names(row) && isTRUE(as.integer(row$is_hidden) == 1L)) ||
        ("deleted_at" %in% names(row) && !is.na(row$deleted_at) && nzchar(row$deleted_at))) next
    path <- row$dataset_path
    relative <- NULL
    if (length(path) && !is.na(path) && nzchar(path)) {
      path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      prefix <- paste0(identity$data_pond_path, "/")
      if (startsWith(path, prefix)) relative <- substring(path, nchar(prefix) + 1L)
    }
    if (!is.null(relative)) {
      movement_relative(relative)
      if (!grepl("^(src|dev|arc)/[^/]+$", relative)) relative <- NULL
    }
    datasets[[length(datasets) + 1L]] <- list(id = row$id, uuid = row$uuid, title = row$title, path = relative)
    if (!is.null(relative) && dir.exists(path)) {
      # Catalogue exports only; supporting documents and payload stay on demand.
      candidates <- list.files(path, full.names = TRUE, all.files = TRUE, recursive = FALSE)
      candidates <- candidates[grepl("^(metadata([._-].*)?|feature-catalogue-.*\\.xml|\\.pond-dataset\\.json)$", basename(candidates), ignore.case = TRUE)]
      links <- Sys.readlink(candidates)
      candidates <- candidates[!file.info(candidates)$isdir & (is.na(links) | !nzchar(links))]
      for (file in candidates) {
        rel <- paste0(relative, "/", basename(file)); movement_relative(rel)
        dest <- file.path(directory, rel); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        if (!file.copy(file, dest)) stop("Could not stage a metadata file: ", file)
        metadata_files <- c(metadata_files, rel)
      }
    }
  }
  DBI::dbDisconnect(to); on.exit(NULL, add = FALSE); DBI::dbDisconnect(from)
  files <- c("metadata.db", metadata_files)
  manifest <- list(format = "datapond-catalogue", version = 1L, pond_id = identity$data_pond_id,
                   label = identity$pond_label, published = format(Sys.time(), tz = "UTC", usetz = TRUE),
                   datasets = datasets, files = lapply(files, function(p) list(path = p, md5 = unname(tools::md5sum(file.path(directory, p))))))
  movement_json_write(manifest, file.path(directory, "catalogue.json"))
  list(directory = directory, manifest = manifest)
}

movement_publication_destination <- function(pond, destination) {
  destination <- movement_endpoint(destination); pond <- movement_endpoint(pond)
  movement_assert_writable(destination)
  if (movement_overlap(pond, destination)) stop("Publish to a separate folder or library.")
  if (!movement_remote(destination) && file.exists(get_db_path(destination))) stop("Choose a publication folder, not a working pond.")
  destination
}

movement_publish_plan <- function(pond, destination, include_data = FALSE, dry_run = TRUE) {
  pond <- movement_endpoint(pond)
  destination <- movement_publication_destination(pond, destination)
  snapshot <- movement_snapshot(pond)
  remote <- movement_catalogue_dir(destination)
  flags <- c("--stats", "1s", "--log-level", "INFO", if (dry_run) "--dry-run")
  steps <- list(); labels <- character()
  if (isTRUE(include_data)) {
    # Only registered datasets: no DB/WAL, local settings, intake archives or backups.
    for (dataset in snapshot$manifest$datasets) if (!is.null(dataset$path)) {
      source <- file.path(pond, dataset$path)
      if (dir.exists(source)) {
        steps[[length(steps) + 1L]] <- c("copy", source, movement_join(destination, dataset$path), flags)
        labels <- c(labels, paste("Dataset:", dataset$path))
      }
    }
  }
  steps[[length(steps) + 1L]] <- c("copy", snapshot$directory, remote, "--exclude", "/catalogue.json", flags)
  # Publish the checksum manifest last. Readers reject partial/concurrent updates.
  steps[[length(steps) + 1L]] <- c("copyto", file.path(snapshot$directory, "catalogue.json"), movement_join(remote, "catalogue.json"), flags)
  labels <- c(labels, "Pond catalogue and metadata", "Finalise published catalogue")
  summary <- data.frame(Component = labels, Operation = "copy",
    Source = vapply(steps, `[`, character(1), 2L),
    Destination = vapply(steps, `[`, character(1), 3L), stringsAsFactors = FALSE)
  list(steps = steps, snapshot = snapshot, destination = destination, include_data = include_data, summary = summary)
}

movement_validate_manifest <- function(manifest) {
  if (!identical(manifest$format, "datapond-catalogue") || !identical(manifest$version, 1L) ||
      !is.character(manifest$pond_id) || length(manifest$pond_id) != 1L || !nzchar(manifest$pond_id) ||
      !is.list(manifest$files) || !length(manifest$files)) stop("Invalid or unsupported pond catalogue.")
  paths <- vapply(manifest$files, function(x) x$path, character(1))
  movement_relative(paths)
  allowed <- paths == "metadata.db" | grepl("^(src|dev|arc)/[^/]+/(metadata([._-].*)?|feature-catalogue-.*\\.xml|\\.pond-dataset\\.json)$", paths, ignore.case = TRUE)
  if (any(!allowed)) stop("Catalogue file list contains payload files instead of metadata.")
  if (anyDuplicated(paths) || !"metadata.db" %in% paths || "catalogue.json" %in% paths)
    stop("Invalid catalogue file list.")
  if (any(!vapply(manifest$files, function(x) is.character(x$md5) && length(x$md5) == 1L && grepl("^[a-f0-9]{32}$", x$md5), logical(1))))
    stop("Invalid catalogue checksums.")
  for (dataset in manifest$datasets) if (!is.null(dataset$path)) {
    movement_relative(dataset$path)
    if (!grepl("^(src|dev|arc)/[^/]+$", dataset$path)) stop("Invalid dataset folder in catalogue.")
  }
  invisible(manifest)
}

movement_cache_prepare <- function(remote, cache) {
  remote <- movement_endpoint(remote); cache <- movement_endpoint(cache)
  if (movement_remote(cache)) stop("Choose a local cache folder, for example on an external hard drive.")
  if (movement_overlap(remote, cache)) stop("The cache must be separate from the source pond.")
  marker <- movement_cache_marker(cache)
  if (!file.exists(marker)) {
    movement_assert_writable(cache)
    if (dir.exists(cache) && length(list.files(cache, all.files = TRUE, no.. = TRUE)))
      stop("Choose an empty folder for this remote pond cache.")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    movement_json_write(list(remote = remote, read_only = TRUE), marker)
  } else {
    saved <- jsonlite::read_json(marker)
    if (!identical(saved$remote, remote)) stop("This cache belongs to a different remote pond.")
  }
  candidate <- tempfile("catalogue-", tmpdir = cache)
  dir.create(candidate)
  list(remote = remote, cache = cache, directory = candidate,
       steps = list(c("copyto", movement_join(movement_catalogue_dir(remote), "catalogue.json"), file.path(candidate, "catalogue.json"))))
}

movement_cache_fetch_plan <- function(prepared) {
  manifest <- jsonlite::read_json(file.path(prepared$directory, "catalogue.json"))
  movement_validate_manifest(manifest)
  files <- vapply(manifest$files, function(x) x$path, character(1))
  list_file <- tempfile("catalogue-files-"); writeLines(files, list_file, useBytes = TRUE)
  list(steps = list(c("copy", movement_catalogue_dir(prepared$remote), prepared$directory,
                     "--files-from-raw", list_file, "--ignore-times", "--stats", "1s", "--log-level", "INFO")),
       manifest = manifest, list_file = list_file)
}

movement_cache_commit <- function(prepared, manifest) {
  movement_validate_manifest(manifest)
  for (file in manifest$files) {
    path <- file.path(prepared$directory, file$path)
    if (!file.exists(path) || !identical(unname(tools::md5sum(path)), file$md5))
      stop("Catalogue changed during download or is incomplete. The previous cache is unchanged; refresh again.")
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(prepared$directory), flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  if (!identical(DBI::dbGetQuery(conn, "PRAGMA quick_check")[[1]], "ok") || !DBI::dbExistsTable(conn, "entry_tbl"))
    stop("Downloaded catalogue is not a valid metadata database.")
  old <- movement_cache_current(prepared$cache)
  if (!is.null(old) && !identical(old$manifest$pond_id, manifest$pond_id)) stop("The remote pond identity has changed. Choose a new cache.")
  dir.create(file.path(prepared$directory, ".metadata_pond"))
  writeLines(manifest$pond_id, data_pond_id_file(prepared$directory))
  marker <- list(remote = prepared$remote, read_only = TRUE, catalogue = basename(prepared$directory),
                 refreshed = format(Sys.time(), tz = "UTC", usetz = TRUE))
  # Replace only the local pointer after verification; never mutate the source DB.
  temp <- tempfile("pointer-", tmpdir = prepared$cache)
  movement_json_write(marker, temp)
  if (!file.copy(temp, movement_cache_marker(prepared$cache), overwrite = TRUE)) stop("Could not save cache pointer.")
  unlink(temp)
  # Retain one verified metadata generation, never a chain of database backups.
  if (!is.null(old) && !identical(old$directory, prepared$directory) &&
      identical(dirname(old$directory), prepared$cache) && startsWith(basename(old$directory), "catalogue-"))
    unlink(old$directory, recursive = TRUE)
  invisible(marker)
}

movement_cache_current <- function(cache) {
  marker <- movement_cache_marker(cache)
  if (!file.exists(marker)) return(NULL)
  state <- jsonlite::read_json(marker)
  if (is.null(state$catalogue)) return(NULL)
  movement_relative(state$catalogue)
  if (!grepl("^catalogue-[^/]+$", state$catalogue)) stop("Invalid cached catalogue location.")
  directory <- file.path(cache, state$catalogue)
  manifest <- jsonlite::read_json(file.path(directory, "catalogue.json"))
  movement_validate_manifest(manifest)
  list(directory = directory, manifest = manifest, state = state)
}

movement_cache_inventory <- function(cache, include_hidden = FALSE) {
  current <- movement_cache_current(cache)
  if (is.null(current)) return(data.frame())
  rows <- as.data.frame(read_connected_pond_inventory(current$directory, include_hidden = include_hidden))
  rows$relative_path <- vapply(rows$id, function(id) {
    match <- Filter(function(x) identical(as.character(x$id), as.character(id)), current$manifest$datasets)
    if (!length(match) || is.null(match[[1]]$path)) return(NA_character_)
    match[[1]]$path
  }, character(1))
  rows$stage <- ifelse(is.na(rows$relative_path), "unavailable", sub("/.*", "", rows$relative_path))
  rows$cached <- vapply(rows$relative_path, function(p) {
    if (is.na(p)) return("No source folder")
    done <- file.path(cache, "receipts", paste0(gsub("/", "_", p, fixed = TRUE), ".json"))
    if (file.exists(done)) "Downloaded (refresh available)" else "Metadata only"
  }, character(1))
  rows
}

movement_download_plan <- function(cache, dataset_id) {
  current <- movement_cache_current(cache)
  if (is.null(current)) stop("Connect or refresh a remote catalogue first.")
  rows <- movement_cache_inventory(cache)
  row <- rows[rows$id == dataset_id, , drop = FALSE]
  if (nrow(row) != 1L || is.na(row$relative_path)) stop("This record has no portable dataset folder. Republish from the source pond.")
  relative <- row$relative_path; movement_relative(relative)
  destination <- file.path(cache, "datasets", relative)
  # Cache folders must not be replaced with symlinks pointing outside the cache.
  resolved <- movement_endpoint(destination)
  if (!startsWith(resolved, paste0(movement_endpoint(cache), "/"))) stop("Dataset cache points outside the cache folder.")
  list(steps = list(c("copy", movement_join(current$state$remote, relative), destination,
                     "--stats", "1s", "--log-level", "INFO")), destination = destination,
       receipt = file.path(cache, "receipts", paste0(gsub("/", "_", relative, fixed = TRUE), ".json")),
       relative = relative)
}

movement_evict_download <- function(cache, dataset_id) {
  plan <- movement_download_plan(cache, dataset_id)
  if (dir.exists(plan$destination)) {
    if (unlink(plan$destination, recursive = TRUE) != 0L) stop("Could not remove cached files.")
  }
  if (file.exists(plan$receipt)) unlink(plan$receipt)
  invisible(plan$destination)
}

movement_cache_details <- function(cache, dataset_id) {
  current <- movement_cache_current(cache)
  if (is.null(current)) stop("No verified catalogue is cached.")
  conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(current$directory), flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(conn))
  sections <- list()
  for (table in DBI::dbListTables(conn)) {
    if (!"dataset_id" %in% DBI::dbListFields(conn, table)) next
    rows <- DBI::dbGetQuery(conn, paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(conn, table), " WHERE dataset_id = ? LIMIT 201"), params = list(dataset_id))
    if (nrow(rows)) sections[[table]] <- rows
  }
  sections
}
