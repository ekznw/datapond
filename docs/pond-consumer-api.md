# Local R consumer API

The consumer interface searches one or more Data Ponds and retrieves selected
files into a separate, verified cache. It opens catalogues read-only and does not
restore datasets, create releases, migrate catalogues or change pond metadata.

Use ekznwr **0.0.10 or newer** with this datapond source version. Install or load
ekznwr first, then datapond. Existing `dta_find()` and ordinary ZIP calls to
`dta_arc_extr()` retain their behaviour. The provider integration adds no
dependency from ekznwr back to datapond.

## Discover, select, retrieve

```r
source <- datapond::pond_source(c(
  "/data/biodiversity_pond",
  "/data/spatial_pond"
))

# By default, search the latest frozen release per dataset in each pond.
hits <- ekznwr::dta_search(source, pattern = "wetland")
# Include release history explicitly:
# hits <- ekznwr::dta_search(source, pattern = "wetland", latest = FALSE)
# Direct pond_search() calls also include all releases by default.

hits[c("asset_ref", "pond_id", "dataset_uuid", "release_id", "version",
       "title", "relative_path", "availability")]
attr(hits, "problems")

# Choose these IDs from the results; do not automatically take the first row.
selected <- hits[
  hits$pond_id == chosen_pond_id &
  hits$release_id == chosen_release_id &
  hits$relative_path == "data/wetlands.shp",
  , drop = FALSE
]
stopifnot(nrow(selected) == 1L)

# Uses ekznwr.extract_dir, which defaults to a session temporary directory.
result <- ekznwr::dta_arc_extr(source = source, selection = selected)
wetlands <- ekznwr::dta_fp(result, name = "\\.shp$")

# Alternatively, call datapond directly and choose a persistent cache.
result <- datapond::pond_fetch(source, selected, cache = "~/datapond_cache")
result$paths
result$receipt
datapond::verify_pond_cache(result$cache_dir)
```

`dta_arc_extr()` uses the package extraction option for both ordinary ZIPs and
datapond providers:

```r
options(ekznwr.extract_dir = "/path/to/your/extraction_folder")
result <- ekznwr::dta_arc_extr(source = source, selection = selected)
```

The current option is read on every call. An explicit `extract_dir = ...` takes
precedence for that call without changing the option. If the option is unset,
the fallback is `file.path(tempdir(), "ekznw_dta")`. Datapond places its cache
entries and extraction staging inside that chosen directory. Direct calls to
`datapond::pond_fetch()` use their own `cache` argument; pass
`cache = getOption("ekznwr.extract_dir")` there to use the same configured root.

`dta_search()` with a datapond source searches the latest frozen release of each
dataset in each pond by default. Recency is determined by freeze time, not the
version label; descending release ID breaks ties. Selection happens before the
search expression is matched, so a file or title found only in an older release
is not returned. An unavailable or damaged latest release never causes a silent
fallback to an older version.

Pass `latest = FALSE` to search all releases. Direct `pond_search()` calls retain
that all-release default; use `latest = TRUE` for the same behaviour as
`dta_search()`. The case-insensitive expression matches captured dataset title,
relative asset path, dataset UUID or version. Use `dataset_uuid` and `release_id`
for exact filtering. Explicit release IDs override `latest`, allowing an older
version to be requested directly. A frozen version is not necessarily published
or approved for distribution.

Use `state = "working"` or `state = c("frozen", "working")` to include current
working folders. Working search scans filenames without hashing file contents;
retrieval hashes and copies the selected files, then verifies them. Such a copy
is labelled `working` in its receipt and is not an immutable dataset release.
Close external editors when requesting a consistent working copy, especially
for multi-file datasets and SQLite/GeoPackage databases. Changed selected files
are detected during copying, but the API cannot lock external writers or provide
a transaction spanning a live multi-file dataset.

`pond_assets(source, selected)` lists all logical assets in the selected dataset
version. `members` is a list column describing the physical files required by
each asset. `pond_fetch()` accepts multiple asset rows from **one pond and one
dataset version** per call. Call it separately for different versions or ponds.
Returned paths retain their `dataset/` prefix and relative subdirectories.

## File groups and availability

The API uses ekznwr's classification and companion grouping on release member
paths, without unpacking data during discovery. A shapefile includes its
companions; `.shp`, `.shx` and `.dbf` must all exist. File geodatabases are returned
as directory assets. ArcInfo Grids include all directory contents, recognised
companions and a sibling INFO workspace when present. GeoPackages are currently
retrieved as whole files; layer enumeration is future work.

Search results distinguish available, missing, removed/removing and incomplete
assets. Virtual rasters are listed as `unsupported_dependencies`: resolving
their source references is not implemented, so automatic retrieval is refused.
There is no recursive retrieval of related datasets, nested archives or external
file references in this version. Other files are copied as stored; this is not
a validator for the internal content of every supported GIS format.

Failed ponds/releases produce a warning and rows in `attr(hits, "problems")`.
An empty result with problems is not a complete search. Healthy ponds and other
readable releases remain searchable. Configure one location per pond identity;
duplicate identities are reported rather than silently merged. Hidden/deleted
dataset records are excluded. Retired release storage is never scanned as a
working dataset.

## Integrity and cache behaviour

Frozen search checks the member manifest and captured metadata snapshot against
their registered hashes. It uses the captured title rather than today's working
title. Retrieval resolves the selected asset references afresh; caller-edited
member lists and paths do not control extraction.

For a new frozen extraction, the full ZIP hash is checked, all ZIP member names
and sizes are checked against the complete manifest, and only selected members
are decompressed. Their SHA-256 hashes are checked. The ZIP hash and release
catalogue state are checked again before exposing the completed cache entry.
Selective extraction saves unpacking and storage; ZIP hashing still reads the
whole archive. The source snapshot and `files.json` sidecar must remain present.

Each cache entry contains:

```text
cache/<content-and-selection-key>/
  receipt.json
  files.json
  payload/dataset/...
```

The key includes pond/dataset/release identity, source archive/manifest hashes,
selected asset references and selected file hashes. Identical verified requests
reuse the same entry. A different working-file hash or selection gets a separate
entry. Requests do not append to or overwrite existing entries. The transport ZIP
is not retained. Cache verification checks missing, changed and extra payload
files; this partial-cache format is separate from `cache_dataset_release()` and
`verify_cached_dataset_release()`, which cover complete releases.

The cache must be outside all source ponds. Unsafe paths and symlinks in source
or cache entries are refused. A per-entry lock prevents concurrent writers;
staging is renamed into place only after verification. After a process crash, an
abandoned `.lock` directory may need manual removal after confirming the writer
has stopped. There is no automatic eviction or project pin registry. Keep a
persistent cache for saved QGIS projects, and copy files elsewhere before editing.

`pond_fetch()` revalidates source availability even when an entry is already
cached; it does not silently serve a removed release or an offline source.
An existing cache can still be checked independently with `verify_pond_cache()`.
Receipts and hashes detect changes; they are not digital signatures or filesystem
write protection.

## Provider contract and scope

Ekznwr's `new_dta_source(search, fetch)` creates a protocol version 1 provider:

- `search(pattern = NULL, ...)` returns provider-specific exact asset references.
- `fetch(selection, cache)` validates those references and returns an extraction
  list containing `archive_dir` and an `arc_file_contents` data frame. Its `Name`
  values are safe relative paths to retrieved files or usable directories.
- `dta_search()` delegates discovery; the datapond provider defaults to
  `latest = TRUE`. `dta_arc_extr(source = ..., selection = ...,
  extract_dir = ...)` delegates retrieval. ZIP search arguments and `overwrite`
  cannot be mixed with provider retrieval. `dta_fp()` consumes the result normally.

Callbacks are trusted R code. This datapond provider is explicitly for local R
sessions with filesystem access and rejects calls in a Shiny reactive domain.
It does not enforce the application's account roles or licence/embargo metadata.
An HTTP/QGIS adapter must add explicit authentication and per-pond download
permissions; it must not expose these trusted callbacks directly. A QGIS service,
remote downloads, layer enumeration and related-dataset traversal are future work.
