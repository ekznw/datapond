# Small real catalogues and ZIPs exercise the consumer boundary independently
# of the interactive metadata approval/rendering workflow.
consumer_fixture <- function(pond_id = "pond-a", versions = c("1", "2")) {
  root <- tempfile("consumer-test-"); dir.create(root)
  pond <- file.path(root, "pond"); dir.create(pond)
  dir.create(file.path(pond, ".metadata_pond"))
  writeLines(pond_id, file.path(pond, ".metadata_pond", "data_pond_id"))
  work <- file.path(pond, "dev", "example")
  dir.create(file.path(work, "data"), recursive = TRUE)
  paths <- c("data/observations.csv", "data/habitat.shp", "data/habitat.shx", "data/habitat.dbf",
    "data/habitat.prj", "data/ecology.gdb/a.gdbtable", "data/ecology.gdb/a.gdbtablx",
    "data/elevation/hdr.adf", "data/elevation/w001001.adf", "data/elevation/metadata.xml",
    "data/elevation.ovr", "data/info/arc0000.dat", "data/other.csv")
  for (p in paths) {
    dir.create(dirname(file.path(work, p)), recursive = TRUE, showWarnings = FALSE)
    writeLines(p, file.path(work, p))
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(pond, "metadata.db"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  entry <- data.frame(id = 1L, uuid = "dataset-example", title = "Current working title",
    version = "3", dataset_path = work, is_hidden = 0L, deleted_at = NA_character_)
  DBI::dbWriteTable(conn, "entry_tbl", entry)
  releases <- list()
  for (v in versions) {
    id <- paste0("release-", v)
    relative <- paste0("arc/releases/example-v", v)
    location <- file.path(pond, relative)
    dir.create(location, recursive = TRUE)
    private <- file.path(pond, ".metadata_pond", "releases", id)
    dir.create(private, recursive = TRUE)
    captured <- entry; captured$title <- paste("Frozen title", v); captured$version <- v
    snapshot <- file.path(private, "snapshot.sqlite")
    sconn <- DBI::dbConnect(RSQLite::SQLite(), snapshot)
    DBI::dbWriteTable(sconn, "entry_tbl", captured); DBI::dbDisconnect(sconn)
    package <- file.path(root, paste0("package-", v))
    dir.create(file.path(package, "dataset"), recursive = TRUE)
    file.copy(file.path(work, "data"), file.path(package, "dataset"), recursive = TRUE)
    writeLines(paste("version", v), file.path(package, "dataset", "data", "observations.csv"))
    writeLines("Do not extract this transport metadata", file.path(package, "metadata.json"))
    manifest <- file.path(location, "files.json")
    datapond:::release_archive_tool("inventory", package, manifest)
    zip <- file.path(location, "dataset.zip")
    datapond:::release_archive_tool("pack", package, manifest, zip)
    releases[[v]] <- data.frame(release_id = id, dataset_id = 1L,
      dataset_uuid = entry$uuid, version = v, frozen_at = paste0("2026-01-0", v),
      relative_path = relative, zip_sha256 = datapond:::release_sha(zip),
      manifest_sha256 = datapond:::release_sha(manifest),
      snapshot_sha256 = datapond:::release_sha(snapshot), state = "frozen")
  }
  if (length(releases)) DBI::dbWriteTable(conn, "dataset_release_tbl", do.call(rbind, releases))
  list(root = root, pond = pond, work = work, releases = releases, cache = file.path(root, "cache"))
}

consumer_pick <- function(source, path = "data/observations.csv", release = "release-1", state = "frozen") {
  hits <- pond_search(source, state = state, release_id = if (state == "frozen") release else NULL)
  hits[hits$relative_path == path, , drop = FALSE]
}

test_that("ekznwr extraction options control both pond and ordinary ZIP retrieval", {
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  previous <- options("ekznwr.extract_dir")
  on.exit(options(previous), add = TRUE)
  source <- pond_source(f$pond)
  chosen <- consumer_pick(source)
  configured <- file.path(f$root, "configured extraction folder")
  options(ekznwr.extract_dir = configured)
  pond <- ekznwr::dta_arc_extr(source = source, selection = chosen)
  expect_identical(pond$extract_dir, normalizePath(configured, winslash = "/"))
  expect_true(startsWith(pond$cache_dir, paste0(pond$extract_dir, "/")))
  expect_identical(readLines(ekznwr::dta_fp(pond, name = "observations\\.csv$")), "version 1")

  zip <- ekznwr::dta_arc_extr(parent_dir = f$pond,
    arc_file = "example-v1/dataset\\.zip$", arc_file_filter = "observations\\.csv$")
  expect_identical(zip$extract_dir, pond$extract_dir)
  expect_true(startsWith(zip$archive_dir, paste0(zip$extract_dir, "/")))
  expect_identical(readLines(ekznwr::dta_fp(zip, name = "observations\\.csv$")), "version 1")

  # The option is resolved for every call, even for an existing provider object.
  changed <- file.path(f$root, "changed-option")
  options(ekznwr.extract_dir = changed)
  next_pond <- ekznwr::dta_arc_extr(source = source, selection = chosen)
  next_zip <- ekznwr::dta_arc_extr(parent_dir = f$pond,
    arc_file = "example-v1/dataset\\.zip$", arc_file_filter = "observations\\.csv$")
  expect_identical(next_pond$extract_dir, normalizePath(changed, winslash = "/"))
  expect_identical(next_zip$extract_dir, next_pond$extract_dir)
  expect_false(identical(next_pond$cache_dir, pond$cache_dir))

  explicit <- file.path(f$root, "explicit-override")
  override_pond <- ekznwr::dta_arc_extr(source = source, selection = chosen, extract_dir = explicit)
  override_zip <- ekznwr::dta_arc_extr(parent_dir = f$pond,
    arc_file = "example-v1/dataset\\.zip$", arc_file_filter = "observations\\.csv$", extract_dir = explicit)
  expect_identical(override_pond$extract_dir, normalizePath(explicit, winslash = "/"))
  expect_identical(override_zip$extract_dir, override_pond$extract_dir)
  expect_identical(getOption("ekznwr.extract_dir"), changed)

  options(ekznwr.extract_dir = NULL)
  fallback <- ekznwr::dta_arc_extr(source = source, selection = chosen)
  if (isTRUE(fallback$arc_extracted)) on.exit(unlink(fallback$cache_dir, recursive = TRUE), add = TRUE)
  expect_identical(fallback$extract_dir, normalizePath(file.path(tempdir(), "ekznw_dta"), winslash = "/"))
  expect_true(file.exists(fallback$paths[[1]]))
})

test_that("search is read-only, version-specific and usable through ekznwr", {
  a <- consumer_fixture(); b <- consumer_fixture("pond-b")
  on.exit(unlink(c(a$root, b$root), recursive = TRUE))
  source <- pond_source(c(a$pond, b$pond))
  before <- datapond:::release_inventory(a$pond)
  hits <- pond_search(source, "observations")
  expect_equal(nrow(hits), 4L)
  expect_equal(length(unique(hits$asset_ref)), 4L)
  expect_setequal(hits$title, c("Frozen title 1", "Frozen title 2"))
  expect_true(all(hits$state == "frozen"))
  expect_equal(ekznwr::dta_search(source, "observations", latest = FALSE), hits)
  expect_identical(datapond:::release_inventory(a$pond), before)
  expect_equal(nrow(pond_search(source, "observations", state = "working")), 2L)
  expect_equal(nrow(pond_search(source, "observations", state = c("frozen", "working"))), 6L)
  expect_error(pond_fetch(source, hits, a$cache), "one pond and one dataset version")
  one <- hits[hits$pond_id == "pond-a" & hits$release_id == "release-1", , drop = FALSE]
  result <- ekznwr::dta_arc_extr(source = source, selection = one, extract_dir = a$cache)
  path <- ekznwr::dta_fp(result, name = "observations\\.csv$")
  expect_identical(readLines(path), "version 1")
  expect_length(list.files(result$archive_dir, recursive = TRUE), 1L)
  expect_false(file.exists(file.path(result$archive_dir, "metadata.json")))
  expect_identical(datapond:::release_inventory(a$pond), before)
  expect_equal(result$receipt$release_id, "release-1")
  expect_equal(result$receipt$state, "frozen")
})

test_that("dta_search defaults to the latest freeze per dataset and pond", {
  a <- consumer_fixture(); b <- consumer_fixture("pond-b")
  on.exit(unlink(c(a$root, b$root), recursive = TRUE))
  source <- pond_source(c(a$pond, b$pond))
  latest <- ekznwr::dta_search(source, "observations")
  expect_equal(nrow(latest), 2L)
  expect_true(all(latest$release_id == "release-2"))
  expect_setequal(latest$pond_id, c("pond-a", "pond-b"))
  expect_equal(nrow(ekznwr::dta_search(source, "observations", latest = FALSE)), 4L)
  expect_equal(pond_search(source, "observations", latest = TRUE), latest)
  older <- ekznwr::dta_search(source, "observations", release_id = "release-1")
  expect_equal(nrow(older), 2L)
  expect_true(all(older$release_id == "release-1"))
  expect_equal(nrow(ekznwr::dta_search(source, "observations", state = "working")), 2L)
  # Selection precedes the pattern, so an older matching title is not revived.
  expect_equal(nrow(ekznwr::dta_search(source, "Frozen title 1")), 0L)
  expect_error(ekznwr::dta_search(source, latest = NA), "latest must")
  # Version labels do not define recency: a later freeze with a smaller label wins.
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(a$pond, "metadata.db"))
  DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET frozen_at='2026-02-01' WHERE release_id='release-1'")
  DBI::dbDisconnect(conn)
  latest <- ekznwr::dta_search(source, "observations")
  expect_equal(latest$release_id[latest$pond_id == "pond-a"], "release-1")
  expect_equal(latest$release_id[latest$pond_id == "pond-b"], "release-2")
})

test_that("latest searches never fall back when the newest release is unavailable", {
  f <- consumer_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(f$pond, "metadata.db"))
  DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET state='removed' WHERE release_id='release-2'")
  DBI::dbDisconnect(conn)
  latest <- ekznwr::dta_search(source, "observations")
  expect_equal(latest$release_id, "release-2")
  expect_equal(latest$availability, "removed")
  expect_error(pond_fetch(source, latest, f$cache), "unavailable")
  cat("changed", file = file.path(f$pond, f$releases[["2"]]$relative_path, "files.json"), append = TRUE)
  expect_warning(latest <- ekznwr::dta_search(source, "observations"), "could not be searched")
  expect_equal(nrow(latest), 0L)
  expect_equal(attr(latest, "problems")$release_id, "release-2")
})

test_that("GIS assets include companions and directory containers", {
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond)
  shp <- consumer_pick(source, "data/habitat.shp")
  expect_length(shp$members[[1]], 4L)
  result <- pond_fetch(source, shp, f$cache)
  expect_setequal(basename(result$selected_members), paste0("habitat.", c("shp", "shx", "dbf", "prj")))
  expect_true(file.exists(ekznwr::dta_fp(result, name = "\\.shp$")))
  gdb <- pond_fetch(source, consumer_pick(source, "data/ecology.gdb"), f$cache)
  expect_true(dir.exists(ekznwr::dta_fp(gdb, name = "\\.gdb$")))
  expect_length(gdb$selected_members, 2L)
  grid <- pond_fetch(source, consumer_pick(source, "data/elevation"), f$cache)
  expect_true(dir.exists(ekznwr::dta_fp(grid, name = "/elevation$")))
  expect_true("dataset/data/info/arc0000.dat" %in% grid$selected_members)
  expect_true("dataset/data/elevation.ovr" %in% grid$selected_members)
  expect_true("dataset/data/elevation/metadata.xml" %in% grid$selected_members)
  expect_true(nrow(pond_assets(source, shp)) > 3L)
})

test_that("cache reuse verifies content and never overwrites modified files", {
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond); chosen <- consumer_pick(source)
  first <- pond_fetch(source, chosen, f$cache)
  second <- pond_fetch(source, chosen, f$cache)
  expect_true(first$arc_extracted); expect_false(second$arc_extracted)
  expect_identical(first$paths, second$paths)
  expect_silent(verify_pond_cache(first$cache_dir))
  writeLines("changed", first$paths[[1]])
  expect_error(verify_pond_cache(first$cache_dir), "differ")
  expect_error(pond_fetch(source, chosen, f$cache), "differ")
  expect_identical(readLines(first$paths[[1]]), "changed")
})

test_that("working copies are explicit, checked and isolated from their source", {
  f <- consumer_fixture(versions = character()); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond)
  expect_equal(nrow(pond_search(source)), 0L)
  chosen <- consumer_pick(source, state = "working")
  first <- pond_fetch(source, chosen, f$cache)
  expect_equal(first$receipt$state, "working")
  expect_true(is.na(first$receipt$release_id) || is.null(first$receipt$release_id))
  writeLines("new working data", file.path(f$work, "data", "observations.csv"))
  second <- pond_fetch(source, chosen, f$cache)
  expect_false(identical(first$paths, second$paths))
  expect_identical(readLines(first$paths[[1]]), "data/observations.csv")
  expect_identical(readLines(second$paths[[1]]), "new working data")
  unlink(file.path(f$work, "data", "habitat.shx"))
  shp <- consumer_pick(source, "data/habitat.shp", state = "working")
  expect_equal(shp$availability, "incomplete")
  expect_error(pond_fetch(source, shp, f$cache), "incomplete")
})

test_that("missing ponds and broken releases are not reported as clean empty searches", {
  f <- consumer_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(c(f$pond, file.path(f$root, "offline")))
  expect_warning(hits <- pond_search(source, "observations"), "could not be searched")
  expect_equal(nrow(hits), 2L)
  expect_equal(nrow(attr(hits, "problems")), 1L)
  manifest <- file.path(f$pond, f$releases[["1"]]$relative_path, "files.json")
  cat(" ", file = manifest, append = TRUE)
  expect_warning(hits <- pond_search(pond_source(f$pond), "observations"), "could not be searched")
  expect_equal(hits$release_id, "release-2")
  expect_match(attr(hits, "problems")$message, "integrity")
})

test_that("removed and missing releases cannot be fetched, even with a cached copy", {
  f <- consumer_fixture(); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond); chosen <- consumer_pick(source)
  cached <- pond_fetch(source, chosen, f$cache)
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(f$pond, "metadata.db"))
  DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET state='removed' WHERE release_id='release-1'")
  DBI::dbDisconnect(conn)
  expect_equal(consumer_pick(source)$availability, "removed")
  expect_error(pond_fetch(source, chosen, f$cache), "unavailable.*removed")
  expect_silent(verify_pond_cache(cached$cache_dir))
  unlink(file.path(f$pond, f$releases[["2"]]$relative_path, "dataset.zip"))
  expect_equal(consumer_pick(source, release = "release-2")$availability, "missing")
})

test_that("source authority, integrity and path containment are enforced", {
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond); chosen <- consumer_pick(source)
  tampered <- chosen; tampered$members <- I(list("../../outside")); tampered$relative_path <- "../../outside"
  result <- pond_fetch(source, tampered, f$cache)
  expect_identical(readLines(result$paths[[1]]), "version 1")
  expect_error(pond_fetch(source, chosen, file.path(f$pond, "bad-cache")), "outside")
  expect_false(dir.exists(file.path(f$pond, "bad-cache")))
  bad <- chosen; bad$asset_ref <- "invented"
  expect_error(pond_fetch(source, bad, f$cache), "stale")
  zip <- file.path(f$pond, f$releases[["1"]]$relative_path, "dataset.zip")
  cat("damage", file = zip, append = TRUE)
  expect_error(pond_fetch(source, chosen, file.path(f$root, "new-cache")), "integrity")
  for (p in c("../outside", "/absolute", "C:/outside", "a//b", "a/./b", "a\\b", "a/"))
    expect_error(datapond:::consumer_safe_names(p), "Unsafe")
})

test_that("archived release storage is not rediscovered as a working dataset", {
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(f$pond, "metadata.db"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path=?",
    params = list(file.path(f$pond, f$releases[["1"]]$relative_path)))
  DBI::dbDisconnect(conn)
  expect_equal(nrow(pond_search(pond_source(f$pond), state = "working")), 0L)
  expect_true(nrow(pond_search(pond_source(f$pond))) > 0L)
})

test_that("empty and hidden working datasets do not produce spurious assets", {
  f <- consumer_fixture(versions = character()); on.exit(unlink(f$root, recursive = TRUE))
  unlink(file.path(f$work, "data"), recursive = TRUE)
  expect_silent(hits <- pond_search(pond_source(f$pond), state = "working"))
  expect_equal(nrow(hits), 0L)
  writeLines("private", file.path(f$work, "private.csv"))
  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(f$pond, "metadata.db"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET is_hidden=1")
  DBI::dbDisconnect(conn)
  expect_equal(nrow(pond_search(pond_source(f$pond), state = "working")), 0L)
})

test_that("symlinks and extra cached payload files fail verification", {
  skip_on_os("windows")
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond); chosen <- consumer_pick(source)
  cached <- pond_fetch(source, chosen, f$cache)
  writeLines("extra", file.path(cached$archive_dir, "extra.csv"))
  expect_error(verify_pond_cache(cached$cache_dir), "differ")
  expect_error(pond_fetch(source, chosen, f$cache), "differ")
  link <- file.path(f$root, "redirected-cache")
  expect_true(file.symlink(f$pond, link))
  expect_error(pond_fetch(source, chosen, file.path(link, "nested")), "outside")
  expect_false(dir.exists(file.path(f$pond, "nested")))
  expect_true(file.symlink(file.path(f$work, "data", "observations.csv"), file.path(f$work, "linked.csv")))
  expect_warning(hits <- pond_search(source, state = "working"), "could not be searched")
  expect_match(attr(hits, "problems")$message, "Symbolic links")
})

test_that("a removal beginning during extraction prevents cache publication", {
  f <- consumer_fixture(versions = "1"); on.exit(unlink(f$root, recursive = TRUE))
  source <- pond_source(f$pond); chosen <- consumer_pick(source)
  original <- datapond:::consumer_check_hash
  zip_checks <- 0L
  local_mocked_bindings(consumer_check_hash = function(path, expected) {
    original(path, expected)
    if (basename(path) == "dataset.zip") {
      zip_checks <<- zip_checks + 1L
      if (zip_checks == 2L) {
        conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(f$pond, "metadata.db"))
        DBI::dbExecute(conn, "UPDATE dataset_release_tbl SET state='removing'")
        DBI::dbDisconnect(conn)
      }
    }
    invisible(path)
  }, .package = "datapond")
  expect_error(pond_fetch(source, chosen, f$cache), "became unavailable")
  expect_length(list.files(f$cache, all.files = TRUE, no.. = TRUE), 0L)
})
