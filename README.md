# Data Pond

<p align="center">
  <img src="inst/www/datapond-logo.png" alt="DataPond finfoot logo" width="280" />
</p>

Data Pond is an offline-first R Shiny application for managing the local
dataset lifecycle. It combines structured metadata capture, local dataset
organisation, file inventory, spatial inspection, and review history in one
installable R package.

The package is intended for teams that work with datasets in low-connectivity
environments and need a consistent way to organise, describe, review, and
eventually distribute those datasets.

**User access:** the workspace now requires a local login. Existing seeded names
do not grant access. Set up the first administrator from the local R console, then
use Request access and administrator approval for colleagues. See
[account setup, roles, recovery and storage](docs/user-access.md).

## User guide

The editable [Quarto user guide](guide/index.qmd) covers installation, first-pond
setup, metadata, development reports, review and frozen releases. Open the
searchable bundled guide from **Configuration → User guide → Open user guide**,
or use `datapond::open_user_guide()` for its self-contained offline edition.
Reading the guide does not require Quarto. It includes copyable commands,
operating-system tabs and personal progress checklists.

To edit it, change `guide/_content/*.qmd` and run `quarto preview guide` from the
repository root. Run `Rscript scripts/build_user_guide.R` to rebuild the HTML
shipped in `inst/guide/` before releasing the package. Both editions share the
same chapter text; generated HTML is not the editing source.

## Current status

Data Pond is under active internal development. Version `1.4.0` is the current
colleague-facing release on the `main` branch.

The implemented application supports:

- multiple local data ponds;
- pond-local SQLite metadata catalogues;
- structured dataset metadata with shared field definitions in R;
- dataset citation and folder-key generation;
- a pond-level BibLaTeX bibliography with Quarto-ready citation keys;
- source, development, and archive lifecycle locations;
- organisations, people, users, authors, and points of contact;
- controlled keyword vocabularies and keyword administration;
- licensing, distribution, embargo, and access metadata;
- lineage and supporting-document records;
- data-file inventory and review;
- vector and raster metadata inspection;
- attribute and raster-layer profiling;
- manual and derived spatial extents;
- metadata completeness and lifecycle dashboards;
- metadata review history;
- portable dataset ZIP bundles with dataset-only SQLite metadata; and
- drag-and-drop dataset import with source user and Data Pond provenance;
- named, read-only connections to additional local Data Ponds; and
- cross-pond duplicate advisories for source and archive records.

**Dataset lifecycle → Frozen releases** now freezes verified versioned ZIPs,
closes development, restores releases as drafts, and recovers interrupted
operations. [Lifecycle and cache workflow](docs/dataset-release-lifecycle.md).

**Dataset status → Human-readable
metadata** saves an EKZNW-branded `metadata.pdf` and the saved
`citation.txt` at the working dataset folder root and downloads the PDF.
Open it and review the folder before freezing; freezing preserves these documents
without regenerating them and refuses stale or changed copies.
The PDF describes saved dataset metadata and links development
QMD/Rmd documents into the lineage section without executing them. Rendering
requires Quarto and XeLaTeX. The R function `write_metadata_pdf()` also retains
the generated QMD and portable shared theme; see the
[fields, styling and export behaviour](docs/human-readable-metadata.md).
**Dataset status → ISO metadata validation & export** now checks saved metadata
and offers a validated ISO metadata package ZIP. Crosswalk v5.0.0 closes
**GAP-018 for local validation**; see the [closure and scope](docs/iso-metadata-gap-infill-plan.md)
and [acceptance criteria](inst/extdata/iso_crosswalk/v5.0.0/local-acceptance.yml).
Geographic tables can use an explicitly named area; genuinely nongeographic
records need no coordinate extent. New records capture their creation timestamp
automatically. Date-only legacy creation values retain their precision, with
pinned and declared DataPond validation results reported separately.
The report shows pinned and DataPond results separately, with links to relevant
fields and sections. It also checks code membership using pinned ISO/TC 211,
LOC language and IANA encoding registries, plus bounded date, extent and reference
consistency rules. Obsolete code-list URLs are corrected; stored `utf8`/`utf16`
labels become `UTF-8`/`UTF-16` in XML without rewriting saved records.
Download requires the chosen policy, semantic checks and local file checks
to pass, and reruns validation against saved metadata. Packages contain metadata
XML, any generated feature catalogue, cited local supporting documents/scripts,
and the validation report. External URLs are retained without fetching; dataset
data files are excluded. Save current edits before checking. Optional validation
requires Python 3.9+ with `lxml`; ZIP downloads also require the `zip` command.
No files are written into the pond by the download action. Each internal writer
call publishes a separate package folder, preserving previous exports.
No receiving catalogue is selected: import compatibility remains explicitly
**not assessed**, tracked separately as GAP-020. Local success is not universal
ISO certification or verification of the metadata's factual accuracy.

The [2026-10-21 pond rights audit](docs/pond-rights-audit-2026-10-21.md)
records licensing and access/use overlap in 82 dta_scp and BSPI records,
including disagreements needing curator review. It made no pond changes.

Portable working-dataset transfer is implemented separately from immutable
release packaging. A selected dataset can be compressed from the **Dataset
transfer** tab. The ZIP is written inside that dataset's folder and can be
dragged onto the same tab in another Data Pond. The receiving pond creates a
new local record and folder, retains the original dataset UUID and records the
source numeric dataset/user IDs and source Data Pond UUID in
`dataset_import_tbl`. Existing UUIDs or destination folders are never
overwritten.

**Metadata review exchange** adds a controlled return path for metadata-only
reviews. Issue a request, let a colleague import/edit/save it in a separate local
pond, then inspect their return. A pond administrator can accept it into the
existing master only while the issued metadata/file baseline still matches.
Acceptance preserves the dataset identity and files, retains previous metadata,
and appends review/transfer history transactionally. File changes and shared
party/vocabulary edits are outside this first version. See the
[review workflow, safeguards and limits](docs/bundle-review-workflow.md).
Format 5 bundles carry previous import history; older app versions need updating.

Import also carries metadata fields added by the source pond's forms or custom
schema, including purpose, citation, credits and use restrictions, into a fresh
pond. The receiving pond does not need to have opened those forms first.

ZIP compression defaults to level 6. For faster exports of large rasters, set
`options(datapond.bundle_compression_level = 1L)` before exporting. Levels 0–9
are supported; higher levels usually take longer and produce smaller ZIPs.
Compression does not change the extracted data or metadata.

Bundle uploads default to a 2 GiB request limit. For larger compressed
datasets, set a higher byte limit before starting the app, for example:

```r
options(datapond.max_bundle_size = 10 * 1024^3)
datapond::run_data_pond()
```

### Open or create a Data Pond

In **Configuration**, use **Open or create a data pond**:

- **Open existing pond** selects an existing pond folder. You can also enter its
  path under **Open an existing pond by path**.
- **Create new pond** asks for a parent location and a name for a new folder.
  Check the displayed destination, then choose **Create and open pond**. The app
  creates `src/`, `dev/`, `arc/`, the metadata database and pond configuration,
  switches to the new pond, and requires administrator setup and login.

Opening requires an existing pond catalogue. Creating requires a new folder
name; it does not reuse an existing folder or copy datasets from another pond.

If the last opened pond's drive or network folder is unavailable at startup,
the app keeps your selection and shows a recovery message. Reconnect the
storage, then select **Retry opening pond**. Startup does not create a
replacement pond at the missing location or an empty mount point.
Alternatively, choose a connected pond, browse for an existing pond folder, or
enter its path in the recovery dialog, then select **Open selected pond**.
The app validates the selected pond before opening it in the current session.
It becomes the remembered pond for subsequent launches without an explicit
`data_pond` argument; other running sessions keep their active pond.

Both the active-user selection dialog and the unavailable-pond dialog offer
**Exit DataPond**. When launched with `datapond::run_data_pond()`, this stops
the application and returns control to R; you can then close the browser tab.
In a hosted application it ends only the current browser session. You do not
need to sign in or locate a pond before exiting.

### Connected Data Ponds

Additional existing ponds can be connected from the configuration panel and
given a nickname / repository name. Their visible dataset records appear alongside
the current pond in the **Pond overview** inventory. **★ Current** marks the current
pond's rows; **Show current pond datasets only** hides the others. Connected rows
remain read-only. Selecting one does not load it into the metadata forms: use **Open selected pond** first,
then sign in to that pond before editing.

Connected catalogue reads query only `entry_tbl` in each `metadata.db`; they do
not scan dataset files or calculate spatial metadata. Results are cached using
the database and WAL modification times and sizes and can be refreshed manually.
Remote snapshot reads are cached separately from current-pond edits. Search uses
precomputed text in memory with a 250 ms typing debounce; it does not reopen
catalogues or contact remotes. The unified table uses server-side paging and
deferred rendering. Current-only mode skips connected inventory assembly.
The first combined load still grows with the number of connected records;
these controls reduce repeated I/O and browser rendering, not that initial cost.

Each session edits one active pond. If another connected pond is offline, its
records are omitted from combined catalogue results and its connection is
retained. Other available ponds continue to be read. The connected-pond list
shows missing folders or catalogues; refresh the catalogues after reconnecting
storage. Duplicate checks cover only the ponds that could be read.

Cross-pond duplicates are matched by stable dataset UUID where possible, with
citation key and folder key as fallbacks for older records. Duplicate working
copies found only in `dev/` are shown as tolerated. A duplicate group containing
any `src/` or `arc/` record is highlighted with a prompt to switch ponds and
resolve which record is authoritative.

## Installation

Data Pond requires R 4.1 or later. It also requires the packages declared in
[`DESCRIPTION`](DESCRIPTION), including the project-specific `ekznwr` package
used for local file inventory.

**Windows users** will need to install [RTools](https://cran.r-project.org/bin/windows/Rtools/) before following the installation instructions below.

### Install from GitHub

The simplest installation uses `pak`:

```r
install.packages("pak")
pak::pak("ekznw/datapond")
```

Alternatively, use `remotes`:

```r
install.packages("remotes")
remotes::install_github(
  "ekznw/datapond",
  dependencies = TRUE,
  upgrade = "never"
)
```

The GitHub installation resolves the `ekznwr` dependency from the
[`ekznw/ekznwr`](https://github.com/ekznw/ekznwr) repository.

To test features that have not yet been promoted to the stable `main` branch,
install the development branch explicitly:

```r
pak::pak("ekznw/datapond@develop")
```

or:

```r
remotes::install_github(
  "ekznw/datapond",
  ref = "develop",
  dependencies = TRUE,
  upgrade = "never"
)
```

After installation, prepare document tools once while connected to the internet,
then start the application:

```r
datapond::setup_document_tools()
datapond::run_data_pond()
```

Setup detects Quarto, XeLaTeX and Python 3.9+, then renders temporary examples
using both EKZNW themes. It verifies the metadata PDF and the development
template's PDF, Word and HTML outputs without opening or modifying a dataset.
If Quarto or Python is missing, it prints the official installation links;
install the missing program, restart R and rerun setup. These external programs
are not installed by `pak` or `remotes` simply because they are listed in
`SystemRequirements`.

When no XeLaTeX installation exists, setup uses
[`quarto install tinytex`](https://quarto.org/docs/output-formats/pdf-engine.html).
It can provision missing packages in Quarto's TinyTeX installation, including
the EKZNW fonts. An existing system TeX installation is tested but left unchanged;
its administrator can resolve any reported package or font errors. Existing,
incomplete TinyTeX installations are not forcibly replaced. Setup is never run
automatically during package installation, package loading or app startup.

For a check with no software or package downloads:

```r
tools <- datapond::check_document_tools()
tools$ready
# Equivalent setup mode: datapond::setup_document_tools(install = FALSE)
```

The app exposes the same check under **Configuration → Document tools**.
After successful setup, normal document generation keeps automatic TeX-package
downloads disabled. Metadata editing and the supplied Word template remain
available without the rendering tools.

For custom installations, set `DATAPOND_QUARTO` (or `QUARTO_PATH`) to the Quarto
executable and `DATAPOND_RELEASE_PYTHON` to Python. Put these settings in
`.Renviron` if they should persist between R sessions. The checker and the app's
document/release operations use the same executable settings.

### Install from a local checkout

Clone or download the repository, open the repository root in R, and run:

```r
install.packages(
  ".",
  repos = NULL,
  type = "source"
)
```

### Install from a source archive

Build and install a source archive from a terminal:

```sh
R CMD build .
R CMD INSTALL datapond_1.4.0.tar.gz
```

When installing a tarball directly, install `ekznwr` first if it is not
already available:

```r
install.packages("remotes")
remotes::install_github("ekznw/ekznwr", upgrade = "never")
```

## Running the application

Start Data Pond and reopen the last selected pond:

```r
datapond::run_data_pond()
```

Open a particular pond explicitly:

```r
datapond::run_data_pond(
  data_pond = "/path/to/data_pond"
)
```

The root [`app.R`](app.R) file is a development launcher. It loads the package
source with `pkgload` and calls the same exported application function.

## Offline-first model

Each data pond is self-contained. Its datasets, configuration, identifiers,
and SQLite database remain on the local filesystem:

```text
data_pond/
├── src/
├── dev/
├── arc/
├── .metadata_pond/
│   ├── config.rds
│   └── data_pond_id
└── metadata.db
```

The package does not require a remote application server. Dataset records and
file scans remain usable without a network connection.

The current Leaflet maps use an online CartoDB basemap. Spatial records and
bounding boxes remain local, but the background map tiles may be unavailable
when completely offline. A packaged offline basemap is a future enhancement.

## Pond lifecycle locations

### `src` — source and reference data

Use `src` for supplied, authoritative, or reference datasets that are being
catalogued and retained close to their received form.

Typical examples include:

- externally supplied datasets;
- authoritative reference layers;
- source GIS layers; and
- datasets being documented without substantial transformation.

### `dev` — active development

Use `dev` for datasets being created, cleaned, transformed, analysed, or
derived.

Typical examples include:

- work-in-progress datasets;
- transformed or cleaned data;
- analytical and modelling outputs; and
- internally produced GIS products.

### `arc` — archived material

The current application can move eligible, locked lifecycle records to `arc`.
Archived content should be treated as frozen.

The frozen-release workflow stores verified ZIPs in `arc/releases/<release-id>/`
while allowing the working dataset to remain in `dev`. Archive and close removes
the working folder only after verification. Restore returns files and saved
metadata together as a draft with the same dataset identity.

## Dataset folder structure

Data Pond creates a standard folder for each managed dataset:

```text
dataset_folder/
├── data/
├── docs/
└── scripts/
```

### `data/`

Contains the dataset itself and any managed inputs, working files, or outputs.
The Data files module scans this area and stores a logical inventory in
`metadata.db`.

### `docs/`

Contains supporting documents such as methodology notes, data dictionaries,
licence documents, reports, and references.

### `scripts/`

Contains processing, validation, analysis, and reproducibility scripts.

Dataset folders use a readable folder key together with a stable dataset UUID.
The UUID supplies identity even when an unlocked draft folder is renamed.

Maps offer **Street map** (OpenStreetMap, selected by default) and
**Satellite (EOX 2016)**. The satellite background uses EOX's Sentinel-2 cloudless
2016 mosaic under [CC BY 4.0](https://cloudless.eox.at/pricing), with attribution
shown on the map. Both backgrounds load online without an API key. The imagery
is historical context; dataset boundaries remain separate overlays.

## Typical workflow

Choose **Essential — Prickly Porcupine** or **Full — Armoured Pangolin** using
the clickable animal hex icons under **Metadata entry** in Configuration. This session's
choice controls entry visibility, both save actions and Dataset status.
Prickly Porcupine requires title, publication date, version, abstract, theme keywords and
an added organisation with a usable code to create and save a draft; use **Show additional metadata fields** to add detail at any time.
Armoured Pangolin also requires purpose and
scope-dependent ISO topics when saving. Switching keeps entered and saved detail.
Version is required in both modes and defaults to `0.1.0` for new drafts;
use `1.0.0` for the first release. Existing versions are preserved until edited.
Unknown publication dates remain blank and must be supplied before saving.
The first added organisation and first theme keyword determine the naming
components. An active editor is required in both modes.

Saving a draft is separate from completing its contacts, lineage and sharing
assessment, release readiness or ISO validation. Lifecycle and pond overview
still use full completion. See the
[metadata population assessment](docs/metadata-population-assessment.md) for
field consolidation decisions and the remaining readiness work.

1. Select or create a local data pond.
2. Sign in with editor or administrator access.
3. Choose an existing dataset or start a new record.
4. Complete citation and descriptive metadata.
5. Add organisations, authors, and points of contact.
6. Record licensing, distribution, access, and embargo information.
7. Create the standard dataset folder.
8. Scan and review the dataset files.
9. Inspect spatial metadata and dataset extent.
10. Profile selected attributes or raster layers where appropriate.
11. Record lineage and supporting documents.
12. Review Dataset status and Lifecycle readiness.
13. Save metadata changes and their review history.

The configuration panel can be collapsed so the main application occupies the
full browser width.

Use **Configuration → Display → Appearance** to follow the operating-system
theme, use the DataPond earth-dark palette, choose light mode, or use a palette
based on the locally installed VSCodium Noctis Bordo theme. The preference is
remembered in this browser; it does not change the shared pond configuration.

## Main application areas

### Pond overview

Summarises the active pond, maps available dataset locations, and provides the
main dataset inventory and selection interface. The map defaults to location
points at bounding-box centres (not sampling sites), with nearby points clustered
to keep the basemap readable. Click clusters to zoom in or separate overlapping
points. Use **Map display** to switch to extent boxes. Both views retain dataset
status colours and click-to-select; the selected dataset's extent is outlined
without fill. Datasets without a valid derived bounding box remain in the
inventory but are not mapped. The map appears before its display controls and
notes. Map and summary describe the current pond; connected rows in the unified
inventory are not added to the map or subjected to local readiness calculations.

The inventory uses compact 12 px text. Selecting a row or map location previews
saved metadata, including the abstract in a card below the map, without loading
the forms or changing the editor's current dataset. **Open selected dataset in
metadata editor**, or opening **Create / edit dataset** / **Dataset lifecycle**,
loads the previewed current-pond record on demand. Browsing alone preserves any
unsaved editor text. Record actions are available once that record is loaded.
Selection updates only the map's outline layer, preserving its zoom and avoiding
a full map rebuild. Connected abstracts are read on demand from local catalogue
files or verified Flow caches; no cloud request or dataset download is needed.

### Dataset intro

Captures citation and descriptive metadata. The dataset version remains here
because it currently participates in citation, citation-key, and folder-name
generation.

### Dataset status

Reports metadata completeness by section using the shared earth-tone status
palette.

### Dataset lifecycle

Shows lifecycle state, folder location, readiness, locking, and controlled
draft-folder movement or renaming.

### Licensing and data access

Records ownership, redistribution authority, licence conditions, access
levels, distribution details, embargoes, and access contacts.

### Data files and spatial metadata

Scans managed files, reviews logical dataset components, extracts vector and
raster properties, profiles selected attributes, and derives dataset spatial
summaries.

### Supporting documents and lineage

Catalogues documentation and records sources, processing, methods, tools, and
a single lineage narrative. A lineage record can use either the structured lineage fields or
one or more scanned supporting documents as its authoritative detail. Any
supported document type can be linked; only a short lineage summary is then
required in the lineage record itself. Previous flat source/process text migrates
once into structured rows; existing additional notes merge into the narrative.
The old controls are removed. Original values remain hidden compatibility
snapshots, and a migration marker prevents deleted details from reappearing.

### Parties and keywords

Maintains organisations and people and administers controlled keyword
vocabularies. Packaged organisation and person vocabularies initialise an empty
pond once. Existing catalogues keep their local parties and user permissions.

## Metadata schema

The [table dictionary](docs/table-dictionary.md) describes all 38 database
tables and the temporal compatibility view, including their purposes,
relationships, fields, constraints and authoritative versus legacy values.
It also covers dynamically added entry fields and includes a worked example
of resource citation dates.

The generated Citation and About controls use `dataset_overview_schema()` in
[`R/dataset_overview_schema.R`](R/dataset_overview_schema.R). Its 12 field
definitions are shared by UI generation, hydration, saving and validation. Each
session gets fresh tables so local changes cannot affect other sessions.

Sections such as lineage, licensing and spatial information have dedicated editors.
The historical `metadata_schema.xlsx` remains a reference for the original field
dictionary and ISO crosswalk coverage. Runtime CSV/Excel schema loading and the
`schema_path` launcher argument have been retired. Change the R definitions when
updating the generated controls, and review their citation, hydration, saving,
folder naming and completion dependencies together.

Topic keywords and flat temporal dates are handled by the combined theme field
and repeatable temporal editor. The separate CSV vocabularies below still provide
reference choices; they do not define the form layout.

The [legacy-data audit](docs/legacy-data-cleanup-2026-10-21.md) distinguishes
obsolete UI definitions from stored values and migrations that remain necessary.

### Upgrading older ponds

The [retained startup patch](docs/startup-migration-v1-2026-10-21.md) migrates older
records into the current DataPond schema when a pond opens. It records completion
per record, preserves original source information and existing structured values,
and skips completed conversions on later startups. Older portable imports use
the same patch after their related records arrive. A failed conversion rolls back
the patch's data changes and markers so it can be retried.

The patch remains enabled while colleagues update their ponds. Its isolated
implementation can be retired once all supported ponds have upgraded and old
transfer bundles have an explicit upgrade or rejection policy. The linked report
includes a copy-only rehearsal command and the migration-archive inventory.

## Reference vocabularies

Read-only seed resources are installed under `inst/extdata/`:

```text
vocab_keywords.csv
vocab_organisations.csv
vocab_persons.csv
```

Organisations are seeded before people so organisation relationships can be
resolved. First-use party seeding and its `app_config_tbl` marker commit together.
Subsequent startup never overwrites party edits, restores deleted parties, or
reactivates users from the CSV. An existing catalogue without a marker is adopted
without reapplying the seeds. Add further parties through party management.

Administrative keyword changes are stored in the active pond database. The
packaged CSV remains the baseline seed rather than the live editable store.

Keyword inputs accept shared suggestions and custom terms. Press Enter to add a
custom term; it is usable immediately on the dataset. Saving registers unknown
terms for administrator review, and approval makes them shared suggestions.
Unambiguous display labels resolve to existing stored values. Keyword lists use
semicolons as separators; commas are part of a term and survive save/reload and
ISO export. Existing comma-only text is retained as one phrase rather than
guessing whether its commas were intended as separators. The first theme term
remains the primary keyword used for dataset naming and citation keys.

Keyword administration provides group/status filters, dataset usage counts and
a selected-term editor. A term's stored value and group cannot be changed while
datasets reference it; its display label can be edited. Deactivation preserves
saved dataset values and becomes an administrator override. Custom and pending
terms are exported without a thesaurus citation unless they become approved
members of that thesaurus.

CSV maintenance has three explicit operations:

- **Add missing terms only** preserves every existing term, including inactive
  terms and local edits. This is also the behaviour of `merge_keyword_vocab_seed()`.
- **Update CSV terms** adds missing terms and refreshes CSV-managed labels,
  ordering and approval/active state. It preserves administrator overrides and
  custom terms.
- **Reset CSV terms** additionally deactivates CSV-managed terms absent from the
  file. It never deletes terms or rewrites dataset metadata.

Each operation requires a preview and confirmation in the tab. Previews show
previous/proposed labels and ordering; changed CSV files or vocabulary records
invalidate the preview. Validation and transaction rollback prevent partial
updates, including on reset. Empty files, unknown groups, blank terms, invalid
ordering, duplicate values/labels and conflicting local labels are rejected.
The shipped vocabulary has 122 terms across theme/subject, knowledge domain and
spatial context. Legacy `keyword_topic` input is still accepted and mapped to the
theme group; it is no longer present in the packaged CSV.

## Storage and portability

`metadata.db` is the working source of truth for a pond. It contains dataset
records and their related parties, licences, reviews, lineage, inventories,
attributes, and spatial summaries.

### Pond bibliography and Quarto

Data Pond can generate `datapond.bib` in the root of the active pond. The file
is derived from the saved records in `metadata.db` and contains one BibLaTeX
`@dataset` entry per dataset. Refreshing the bibliography rebuilds the complete
file in citation-key order and retains the previous file as
`datapond.bib.bak`.

The Pond overview displays the selected dataset's key and generated BibLaTeX
entry. It provides buttons to copy the key, copy Quarto citation markup such as
`[@ekznw_202607_vegetation_a2b3]`, copy the complete entry, or refresh the pond
bibliography. A successful dataset save also refreshes the bibliography.

Citation keys are stable after creation. Unlocked working datasets omit the
version suffix so evolving metadata and versions do not break citations in
development reports. Locking a dataset adds its current version suffix; released
and archived datasets are always locked.

A Quarto document inside a managed dataset can refer to the pond bibliography
using an appropriate relative path:

```yaml
---
title: "Dataset development report"
bibliography: ../../datapond.bib
---
```

The exact number of `../` path components depends on where the `.qmd` file is
stored. Citations then use standard Quarto syntax:

```markdown
The analysis uses the current vegetation dataset
[@ekznw_202607_vegetation_a2b3].
```

Pond-specific configuration is stored in `.metadata_pond/`. The pointer to the
last opened pond is stored in the user's R configuration directory rather than
inside the installed package.

Absolute application installation paths are not part of the pond model.
Dataset file references are stored or repaired relative to their managed
dataset context where possible.

## Spatial and file dependencies

In **Spatial metadata → Manual extents**, choose **Select extent on map**
and click two opposite corners to fill the coordinate fields in EPSG:4326
(longitude / latitude). Pan and zoom before choosing the corners. Review the
rectangle, give it a label, then choose **Save extent**. **Cancel selection**
discards an unfinished selection without changing the fields. Coordinates can
also be typed directly with an explicit source CRS.

Data Pond uses:

- `ekznwr` for enriched local inventory scanning;
- `sf` and `terra` for vector, raster, CRS, and extent inspection;
- `leaflet` for maps;
- `RSQLite` and `DBI` for pond databases; and
- `DT` for interactive tables.

Supported behavior depends on the GDAL drivers available to the installed
versions of `sf` and `terra`. File geodatabases and less common spatial formats
should therefore be tested on each target operating system.

## Package structure

```text
datapond/
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── README.md
├── app.R
├── R/
│   ├── zzz_app_main.R
│   ├── db.R
│   ├── mod_*.R
│   └── utils_*.R
├── inst/extdata/
├── man/
└── tests/testthat/
```

The public launcher is:

```r
datapond::run_data_pond()
```

Most implementation functions remain internal while the application
architecture is stabilised.

## Development and verification

Regenerate package documentation:

```r
roxygen2::roxygenise()
```

Run the test suite:

```r
devtools::test()
```
## Local build & installation using devtools

Check then build package.

```r
devtools::check()
devtools::build()
devtools::install()
```

Build and check the package:

```sh
./build-package.sh
```

The script writes the source archive and check results to `dist/`. Pass a
directory as the first argument to use a different output location. It returns
a non-zero exit status when the package check reports warnings or errors.

A successful run finishes with `Status: OK`.

The tests cover package resources, shared R field definitions, application
construction, responsive layout behavior, earth-palette status rendering,
database initialization, first-use party seeding and preservation of local edits, and safe
handling of legacy duplicate email records.

## Frozen releases and metadata export

First use **Dataset status → Human-readable metadata** to save/download the PDF
into the working dataset root, then open it and review the folder. Use **Dataset
lifecycle → Frozen releases** to preview saved files, confirm that review, enter
release notes, and freeze a version. A version can be frozen only once per dataset.

```text
working dataset in dev/
        ↓
save/download PDF and review dataset folder
        ↓
saved metadata and file checks
        ↓
metadata snapshot
        ↓
versioned ZIP release in arc/
```

Each release ZIP contains:

```text
dataset_release/
├── metadata.pdf
├── metadata.qmd
├── metadata.json
├── citation.txt
├── README.txt
├── LICENSE.txt
├── ekznw-theme/
└── dataset/             # original files and relative paths
```

Validated ISO export is optional; when requested, validation failure prevents
freezing. Beside the ZIP, `files.json`, `release.json`, and `checksums.sha256`
describe and verify the release. Restoration snapshots and working-draft
checkpoints remain private under `.metadata_pond/releases/`.

Restore preserves any existing draft in a verified checkpoint, which can itself
be restored using the rollback control. The consumer cache keeps the extracted
release and removes its temporary ZIP by default. Publication, supersession,
withdrawal, remote Flow downloads and QGIS integration remain separate future
work. See the [workflow and recovery details](docs/dataset-release-lifecycle.md).

## Data safety

Data Pond performs filesystem operations when creating, renaming, or moving
managed dataset folders. Before using it with production material:

- maintain an independent backup;
- verify the selected pond and dataset;
- test filesystem and GIS behavior on the target operating system;
- review lifecycle readiness before moving folders; and
- treat archived material as immutable.

## Licence

Data Pond is provided under the MIT licence. See [`LICENSE`](LICENSE).

## Flow: file syncing and remote ponds

The **Flow** tab copies and syncs ordinary files and folders as well as datasets.
Neither endpoint needs to be a Data Pond: external hard drives, SharePoint libraries
and other configured locations can be used independently of the pond catalogue.
It uses [rclone](https://rclone.org/install/) directly on
Windows, Linux and macOS; SharePoint does not need to be mounted. Install rclone
on each computer and make it available on `PATH` (or set
`options(datapond.rclone = "/path/to/rclone")`).
Local transfers also use rclone; there is no automatic rsync fallback.

Flow opens on **Run flows**, with **Remote ponds (read-only)** next for
frequent use. **Config (setup)** and **Design flows (setup)** follow in muted
sand-coloured tabs. Before running flows, define connections in Config
and save flows in Design flows. Use **Show configuration tab** to hide or restore
Config without losing its values. Config starts hidden when saved flows exist.

Field labels use a small coloured **asterisk (*)** for mandatory values and a
**diamond (◇)** for conditional values; unmarked fields are optional. A legend,
field guidance and screen-reader text explain the symbols without relying on
colour alone. Input boxes retain their standard appearance.
Requirements apply to the action in that section: saving a flow needs a name,
source, destination and operation; a one-off transfer preview needs no flow name.
Pond publication uses the active pond and destination, without requiring source,
flow name or transfer operation. Exclusions and remote pond nicknames are optional.
Missing fields are reported together before saving or starting an operation.
Mirror deletion acknowledgement appears only when the successful preview includes
a mirror flow. Saved remote ponds hide their connection fields until **Edit
connection settings** is selected; new connections show the required remote and
local cache folders immediately.

**Config (setup)** defines reusable local and remote locations. Use **Choose local
folder** for a folder or external drive, or enter a configured remote folder such
as `team_data:shared/data`. Give it a name and **Save location**. Saved locations
are available in the flow editor and persist across sessions. Editing or removing
a location does not change existing flow paths or delete files.

Use **Sign in and discover libraries** to configure OneDrive/SharePoint through
rclone's Microsoft authentication flow. Choose a SharePoint site or site search,
then a document library when prompted. Repeat for other libraries. Discovery is
limited to the signed-in account's permissions; it is not an organisation-wide
inventory. Microsoft tenant policy can require administrator consent. See
[rclone's SharePoint setup guide](https://rclone.org/onedrive/). Credentials remain
in rclone's configuration, outside pond metadata. Expand **Browse locations and
libraries** to inspect configured remotes, saved locations and local drives, or
to use a browsed folder in a flow.

**Design flows (setup)** lets you choose saved source and destination locations or
enter full paths (`E:/team/dataset`, `/mnt/drive/dataset`,
`team_data:shared/dataset`). You can extend a chosen location's path to a subfolder.
A folder's **contents** are copied into the destination, so include the dataset
folder name in the destination. Swap the direction to download. The current
selected dataset can supply the source. Name the flow, choose copy/update or
mirror sync, set exclusions, then **Save flow**.

Choose a saved flow from the editor dropdown to change its settings. Renaming
updates the existing entry. **Duplicate flow** creates a separate editable copy;
**New flow** clears the editor. Expand **Import / export flows** to import the
controller's `syncs.json` (`name`, `src`, `dest`, `excludes`) or **Export flows**.
Each imported component becomes one independently selectable flow; importing
does not run anything. Export includes saved copy/mirror choices. Older JSON
entries without an operation default to copy/update. Shell scripts and arbitrary
rclone flags are not executed from imported profiles.

**Run flows** lists saved flows. Select one or more rows, choose **Preview selected
flows**, review the source, destination, operation and dry-run log, then **Run flow**. Only those saved flows run. **Select all** selects the
complete list; **Clear selection** deselects it. **Edit selected flow** returns
to the editor. **Remove selected flows** deletes saved settings only after
confirmation, retaining source and destination files.

Flows run sequentially in saved order, regardless of table sorting, and
progress/logs identify each flow. A failed or cancelled flow stops the remaining
queue. Flows with overlapping destinations, or destinations that are another
selected flow's source, must be previewed and run separately so each preview sees
its actual inputs. Save editor changes before a batch preview; **Preview flow**
in Design flows instead previews only the editor's current values, including
unsaved changes, then opens Run flows for review. These are reusable flows run on demand;
this workflow does not schedule unattended transfers.

Every copy, mirror or publication requires a successful dry-run preview. Copy
updates changed files and retains extra destination files. Mirror sync also
removes extra destination files and requires a deletion acknowledgement; it is
refused at drive/library roots. Changing settings requires another preview.
Preview results reflect the files present at that time, not a locked snapshot
of the payload. Jobs run asynchronously with progress, cancellation and retries.
Closing the app cancels an active job; retrying reuses completed files. Use
separate source/destination folders. Different rclone aliases pointing to the
same underlying folder cannot be recognised as overlapping automatically.

**Connection speed** in Config provides **Local**, **Cloud cautious** and
**Custom** profiles. Local defaults to 4 transfers and 4 checks, without API or
bandwidth caps. Cloud cautious defaults to 1 transfer, 2 checks, 2 API requests
per second and a burst of 1, without a bandwidth cap. These are conservative
starting values, not guaranteed SharePoint quotas. Save cloud/local defaults or
override a named remote; **Use default for this connection** removes that
override. Custom separates API request rate from data bandwidth (MiB/s); zero
means unlimited for either limit. Remote-to-remote operations use the stricter
limits at either end. Profiles apply to previews, transfers, listings, pond
publication, metadata refreshes and dataset downloads. Queued and running jobs
retain their captured settings; later jobs use the latest saved settings.

Operations using any configured remote, including aliases and connection setup,
share a first-in, first-out queue across sessions and R processes using the same
local app-state directory. This deliberately serialises different remote names
so aliases for one account cannot bypass the queue. Local paths bypass it.
Queue position and cancellation remain available while waiting. Closing the
session cancels its job; abandoned tickets are reclaimed after their owning
process and child exit. This does not coordinate other computers, other app-state
directories, standalone rclone/rsync, or other clients using the same account.

Previews and listings also generate cloud requests. A successful matching
preview is timestamped and reused for up to 15 minutes; **Request a fresh
preview** forces another scan when files may have changed. Edits or speed changes
require another preview, and an expired preview cannot be run. Reuse does not
lock files or guarantee they are unchanged. Cached pond metadata can still be
browsed offline without refreshing it.

The Flow operation monitor identifies previews versus live transfers, shows the
source/destination and current step, and replaces repeated rclone progress
snapshots with the latest totals. Byte percentage and file percentage can differ;
preview totals are estimates and do not mean files were copied. Recent file
actions, warnings and errors appear separately, with the recent technical log
available in an expandable section. `INFO` is normal rclone status information.

The status area reports throttling/retries when rclone logs them, including
native pacer messages. rclone handles backoff and service `Retry-After` waits;
the app does not launch a competing retry loop. If retries are exhausted the
job fails and later steps stop. Allow waiting jobs to continue instead of
repeatedly restarting them. See [Microsoft's throttling guidance](https://learn.microsoft.com/en-us/sharepoint/dev/general-development/how-to-avoid-getting-throttled-or-blocked-in-sharepoint-online)
and [rclone's API-rate controls](https://rclone.org/docs/#tpslimit-float).

To share a pond with colleagues:

1. Open **Run flows > Publish pond** and enter a dedicated publication destination,
   such as `team_data:published_pond`. **Save publication settings** stores this
   destination and the upload option for the active pond on this computer.
   Previewing saves them too, including when that preview or a later run fails.
   Reopening the same pond restores these choices; other ponds have their own settings.
2. Choose **Preview / retry pond publication**, review the plan, then **Run flow**. Select
   **Also upload registered dataset folders** when their files should be made
   available there too. Publishing copies registered folders in their existing
   `src/`, `dev/`, or `arc/` locations; it does not publish intake backups or local
   application settings. Hidden/deleted records are excluded from the download
   inventory, but the SQLite catalogue is a complete metadata snapshot: publish
   it only to colleagues allowed to read the pond's metadata.
3. Colleagues open **Remote ponds (read-only)**, enter that remote folder and a
   dedicated empty local cache folder, including on an external hard drive.
4. **Connect / refresh metadata only** fetches the database and recognised
   dataset-root metadata exports. Search and select records offline. Data,
   supporting documents and scripts are fetched only with **Download / update
   selected dataset**. Their local path is shown after download.
5. **Remove selected download from local pond cache** releases local space after confirmation,
   retaining remote files and cached metadata. Downloads use copy/update, so
   files removed upstream remain locally until the cached download is removed.

After an interrupted publication, use **Preview / retry pond publication** again.
The saved destination and upload choice remain available after restarting the
app. Rclone checks the destination and skips completed files that still match;
this is file-level retry, not a guarantee of resuming a partially uploaded file.
The catalogue manifest is published last. A new attempt still needs a successful
preview and can generate cloud requests while checking previously uploaded files.

Publications currently run inside the app session. For overnight work, sign in
shortly before starting and keep the app connected and the computer awake.
Closing the session, revoked access or the 12-hour session limit stops the job.
The 15-minute preview expiry only limits when a run can start, not its duration
after starting. Saved publication settings do not provide unattended background
execution or scheduled starts.

Connected datasets also appear in the main **Dataset inventory** on
**Pond overview**, together with the **★ Current** rows. The inventory combines local pond connections
with verified Flow remote caches; selecting a connected row also shows its
file/cache status and metadata refresh time. Search applies across all included
ponds, or use **Show current pond datasets only**. **Open selected pond** switches to a local pond or opens a remote cache in
Flow without changing the active editable pond. Remote records remain available
offline after a successful metadata refresh. Browsing the overview does not
contact remotes or download datasets; use Flow to refresh remote metadata or
fetch files. Missing cache drives are reported without blocking other ponds.

The publisher writes `.datapond-catalogue/metadata.db`, metadata exports and a
checksum manifest. SQLite's backup API includes committed WAL transactions,
so an open pond can publish a consistent catalogue. The manifest is uploaded
last. A reader verifies all metadata checksums and SQLite integrity before
replacing its local catalogue; a failed refresh retains the previous catalogue.
Only one verified metadata generation is retained after a successful refresh.
The original absolute dataset paths in the database are never used to download
files: the manifest provides validated pond-relative paths. Raw live SQLite
files copied by other tools are not accepted as a published catalogue; the
owner must use **Publish pond** once. Catalogue-only publication does not make
unpublished dataset files available remotely.

Remote connections cannot edit or upload through the cache browser. Cache
folders are also refused as active editable ponds and as general transfer
destinations. Their connection settings are stored on this computer, not in the
remote database. The standalone transfer tab remains available for deliberate
uploads to locations for which the user has write access. This is a one-way
copy/mirror facility; it does not merge concurrent metadata edits or provide
two-way conflict resolution. Use Metadata review exchange for explicit acceptance
of returned metadata; Flow itself does not approve or merge it. SharePoint is accessed through the signed-in user's
normal permissions, without checksum/size bypasses from the legacy shell script.
