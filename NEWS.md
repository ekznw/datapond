# Data Pond 1.4.0

## R consumer interface (development)

- Added read-only multi-pond discovery and selective verified retrieval through
  `pond_source()`, `pond_search()`, `pond_assets()`, `pond_fetch()` and
  `verify_pond_cache()`.
- Added frozen-manifest discovery, GIS companion/container extraction and
  separate partial consumer caches with provenance receipts.
- Integrated the optional ekznwr provider protocol; requires ekznwr >= 0.0.10.
- Made `ekznwr::dta_search()` select the latest freeze per dataset and pond by
  default, with `latest = FALSE` for release history and exact release overrides.

## Application and access

- Added pond-local user accounts, role-based access, activation and recovery,
  session handling, and administrative audit controls.
- Refined the application layout, pond overview, dataset workflow guidance,
  theme support, and responsive metadata editing interfaces.
- Added an optional African Finfoot milestone sound with visible attribution,
  a remembered browser preference, short action cues, and a fading startup cue.

## Metadata and interoperability

- Added Dataset guide development-report sync with selectable direction and
  fields, change previews, report backups, and imports through the normal
  dataset form. Word and Quarto templates can optionally start populated from
  saved dataset metadata.
- Convert supported Word superscripts/subscripts to Unicode during report sync,
  with explicit notation for unsupported characters and field-specific preview
  warnings for equations, objects and formatting that plain metadata cannot
  preserve. Keep rich Word content in fields not selected for export.
- Preserve Dataset intro fields and unsaved edits when keyword choices refresh,
  including after saving custom keywords during metadata review.
- Added selective removal of missing supporting-document and data-file records
  during review and release preparation, including linked metadata cleanup in
  the working dataset before a fresh approval and freeze.
- Expanded structured ISO 19115/19110 metadata capture, export, semantic
  assessment, pinned validation resources, and downloadable validation reports.
- Added richer citation, responsible-party, temporal, distribution, quality,
  vertical extent, lineage, and feature-catalogue support.
- Added human-readable metadata PDF generation using a shared EKZNW document
  theme, plus saved citation output and document-tool diagnostics.
- Replaced the legacy workbook-backed overview schema with package-native field
  definitions and added startup migration support for legacy ponds.

## Dataset lifecycle and exchange

- Added linked in-house/public editions with independent identities, version
  histories and documentation, and immutable source-release provenance.
- Added public-draft creation from selected frozen files, CSV/TSV columns and
  GeoPackage/GeoJSON layers and attributes, with prefilled public metadata and fresh review.
- Added a three-step public edition workflow, retained-field definitions, public
  contact defaults, internal metadata exclusions, automatic scanning and opening.
- Added repeatable public updates with saved selections, three-way metadata
  comparisons, protection for edited public files, and complete rollback checkpoints.
- Added local publication/withdrawal recording, independent family summaries,
  public transport metadata filtering and protection for currently published ZIPs.

- Added verified frozen releases, manifests, checksums, restoration snapshots,
  rollback checkpoints, archive closure, and interrupted-operation recovery.
- Added controlled metadata review request and return bundles with baseline
  conflict checks and auditable acceptance.
- Added configurable data-movement routines, local release caches, connected
  pond catalogues, cross-pond duplicate advice, and improved dataset transfer
  provenance.
- Added the bundled, searchable user guide and release-oriented operational
  documentation.

## Reliability

- Expanded database migrations, preservation checks, package resources, and
  regression coverage across access, lifecycle, movement, metadata, ISO export,
  spatial metadata, organisations, keywords, and connected ponds.
