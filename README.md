# Data Pond

Data Pond is an offline-first R Shiny application for managing the local
dataset lifecycle. It combines structured metadata capture, local dataset
organisation, file inventory, spatial inspection, and review history in one
installable R package.

The package is intended for teams that work with datasets in low-connectivity
environments and need a consistent way to organise, describe, review, and
eventually distribute those datasets.

## Current status

Data Pond is under active internal development. Version `1.0.27` is the current
test build.

The implemented application supports:

- multiple local data ponds;
- pond-local SQLite metadata catalogues;
- structured dataset metadata generated from an XLSX schema;
- dataset citation and folder-key generation;
- source, development, and archive lifecycle locations;
- organisations, people, users, authors, and points of contact;
- controlled keyword vocabularies and keyword administration;
- licensing, distribution, embargo, and access metadata;
- lineage and supporting-document records;
- data-file inventory and review;
- vector and raster metadata inspection;
- attribute and raster-layer profiling;
- manual and derived spatial extents;
- metadata completeness and lifecycle dashboards; and
- metadata review history.

Immutable versioned ZIP releases and portable XML, Markdown, and PDF metadata
exports are planned but are not yet implemented.

## Installation

Data Pond requires R 4.1 or later. It also requires the packages declared in
[`DESCRIPTION`](DESCRIPTION), including the project-specific `ekznwr` package
used for local file inventory.

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

After installation, start the application with:

```r
datapond::run_data_pond()
```

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
R CMD INSTALL datapond_1.0.27.tar.gz
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

During schema development, an alternative metadata workbook can be supplied:

```r
datapond::run_data_pond(
  data_pond = "/path/to/data_pond",
  schema_path = "/path/to/metadata_schema.xlsx"
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

The planned release workflow will extend this model by creating immutable,
versioned ZIP releases in `arc` while allowing the working dataset to remain
in `dev`.

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

## Typical workflow

1. Select or create a local data pond.
2. Select the active application user.
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

## Main application areas

### Pond overview

Summarises the active pond, maps available dataset extents, and provides the
main dataset inventory and selection interface.

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
lineage notes.

### Parties and keywords

Maintains organisations and people and administers controlled keyword
vocabularies. Packaged organisation and person vocabularies seed each newly
selected pond idempotently.

## Metadata schema

The application UI schema is generated at session startup from:

```text
inst/extdata/metadata_schema.xlsx
```

The workbook identifies fields, labels, input types, descriptions,
obligations, controlled options, and their Dataset overview sections.

The bundled `schema_grouped.rds` is retained as a tested reference snapshot.
Package tests verify that the XLSX workbook currently generates equivalent
schema content.

Changing a field's tab or subtab may require corresponding server changes.
In particular, fields used by citation, hydration, saving, folder naming, or
lifecycle validation should not be moved in the workbook without reviewing
those dependencies.

## Reference vocabularies

Read-only seed resources are installed under `inst/extdata/`:

```text
vocab_keywords.csv
vocab_organisations.csv
vocab_persons.csv
```

Organisations are seeded before people so organisation relationships can be
resolved. Repeated seeding updates or reuses existing records rather than
creating duplicates.

Administrative keyword changes are stored in the active pond database. The
packaged CSV remains the baseline seed rather than the live editable store.

## Storage and portability

`metadata.db` is the working source of truth for a pond. It contains dataset
records and their related parties, licences, reviews, lineage, inventories,
attributes, and spatial summaries.

Pond-specific configuration is stored in `.metadata_pond/`. The pointer to the
last opened pond is stored in the user's R configuration directory rather than
inside the installed package.

Absolute application installation paths are not part of the pond model.
Dataset file references are stored or repaired relative to their managed
dataset context where possible.

## Spatial and file dependencies

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

Build and check the package:

```sh
R CMD build .
R CMD check --no-manual datapond_1.0.27.tar.gz
```

The current package check completes with `Status: OK`.

The tests cover package resources, dynamic XLSX schema generation, application
construction, responsive layout behavior, earth-palette status rendering,
database initialization, idempotent party seeding across ponds, and safe
handling of legacy duplicate email records.

## Planned release and metadata export workflow

The next major lifecycle feature is controlled release packaging:

```text
working dataset in dev/
        ↓
readiness validation
        ↓
metadata snapshot
        ↓
versioned ZIP release in arc/
```

Each immutable release is expected to contain:

```text
dataset_release/
├── METADATA.xml
├── METADATA.md
├── METADATA.pdf
├── MANIFEST.json
├── SHA256SUMS.txt
├── data/
├── docs/
└── scripts/
```

The same metadata snapshot should drive the machine-readable XML and the
plain-language Markdown/PDF report. Release history, release UUIDs, checksums,
supersession, and withdrawal controls will be added to the Lifecycle module.

The existing working-version field will remain in Dataset intro for citation
purposes. Lifecycle release controls can later display that value and validate
it against prior immutable releases.

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
