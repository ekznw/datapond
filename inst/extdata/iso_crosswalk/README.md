# DataPond database-to-ISO crosswalk

This directory contains the versioned mapping from the effective DataPond
SQLite model to ISO geographic metadata. The effective model comprises:

- tables created and migrated by `R/db.R`;
- current generated `entry_tbl` fields defined in `R/dataset_overview_schema.R`,
  added dynamically when a dataset is saved;
- historical fields documented in `metadata_schema.xlsx`, retained for older
  records and compatibility export.

Runtime CSV/Excel schema loading was retired on 2026-10-21. Existing versioned
manifests retain their original source-model paths and hashes as provenance of
the model observed at that version, including references to the former
`R/schema_loader.R`. They do not configure application startup.

`CURRENT` identifies the default crosswalk. Released versions are immutable:
correcting a mapping creates a patch version, adding backward-compatible
mappings creates a minor version, and changing established export semantics
creates a major version.

The crosswalk separates four cases:

- `direct`: one stored value maps to one ISO property;
- `transform`: conversion, aggregation, fixed code-list values, or fallback
  precedence is required;
- `join`: related party or organisation tables must be joined;
- `extension`: useful DataPond information has no faithful ISO 19115-1 target
  and must not be exported as a misleading ISO property.

From v3.2.0, `capture-policy.yml` identifies canonical fields, compatibility
mirrors, concepts that must remain distinct, and the rough/full assessment
targets. v3.2.1 connects those targets to entry, draft saving and folder creation
and records the Prickly Porcupine / Armoured Pangolin labels. v3.2.2 requires
publication date and an added organisation with a usable naming code in both
modes, so new keys do not silently use unknown-date or missing-organisation
placeholders. These are DataPond
capture policies, not ISO obligations. Mapping
status describes the conceptual mapping; it does not certify serializer
coverage or XSD/Schematron conformance. See
`docs/metadata-population-assessment.md` for the field review and remaining work.

v3.3.0 adds dataset locale mappings and validated raster/GFC content exports.
Unknown cardinality/content type and missing catalogue details remain explicit
nil reasons with warnings. GAP-018 remains open for the remaining mappings and
export workflow.

v3.3.1 adds shared contact serialization, ordered citation authors, source
temporal scope and responsibilities, processors and selected document details.
It corrects the source-extent path and documents the single-resolution constraint.
Maintenance and workflow-derived provenance remain pending.

v3.3.2 implements resource status/update frequency and metadata review history
with shared reviewer contacts. It corrects the proposed lifecycle fallback:
release/archive workflow does not establish production progress. Review reasons,
summaries and section labels are exported; generic notes and workflow audit
fields remain local. Metadata maintenance frequency stays explicitly unknown.
Remaining spatial mappings, workflow-derived provenance and GAP-018 completion
remain pending. No new capture fields are required.

v3.3.3 corrects vertical extent export, preserving ordered bounds, units and
descriptions. Recognised EPSG vertical CRSs resolve offline through the existing
sf/GDAL/PROJ dependency; units and axis type must agree, and their WKT2 definitions
use the pinned RBC wrapper. Custom references have explicit assessment warnings;
name-only references have an unknown CRS nil reason. This applies to the full
candidate, not the narrower subset assessments. No new capture fields are added.

Application identifiers, foreign keys, scan state, UI state, and audit fields
are covered by `source_policy` in the version manifest. A table's default
classification is overridden by any explicit mapping in `crosswalk.yml`.

v3.4.0 resolves the remaining mapping audit and adds
[`mapping-coverage.yml`](v3.4.0/mapping-coverage.yml): 162 explicit decisions,
comprising 119 implemented mappings, nine canonical migrations and 34 local
dispositions. New export coverage includes vector/grid representations,
CRS-derived axis units, extent derivation/manual notes, temporal descriptions
and guarded legacy fallbacks. MCO-018 names the existing legacy access-text
fallback. No new capture fields are introduced.

Import/release/creation audit and inventory sizes do not establish production
lineage, publication, authorship or transfer size. Their former proposed targets
are corrected to local dispositions. Extent calculation describes the metadata
extent rather than resource production. The ledger lists implementation and
regression evidence; it does not certify every value combination or ISO
conformance. Nonspatial discovery, the creation Date/DateTime rule and validated
app publishing remain GAP-018 work.

v4.0.0 implements nongeographic scope names and optional named geographic
extents, and captures real UTC creation timestamps for new app records.
Imports and migrations preserve unknown or partial original dates. The major
version reflects a stricter fallback: ordinary review history no longer
establishes creation; only an explicitly marked initial-record event does.
The [validation policy](v4.0.0/validation-policy.yml) separately reports a
DataPond date-precision result while retaining pinned failures and every accepted
deviation. The internal writer requires explicit policy selection and saves
both results. Its [ledger](v4.0.0/mapping-coverage.yml) has 164 mappings.
Broader semantic checks and app-facing transactional publishing remain GAP-018.

The target conceptual model is ISO 19115-1:2014 with Amendments 1 and 2. XML
paths use ISO 19115-3:2023 implementation namespaces and the current ISO/TC 211
Part 1 schema package (1.3.0). Data-quality targets use ISO 19157-1:2023.

v4.1.0 adds the Dataset status validation report and validated ZIP download.
The 164 mapping identities remain unchanged. Its
[package policy](v4.1.0/package-policy.yml) checks cited local documents/scripts,
stages and revalidates the XML/catalogue pair, then publishes a new package
directory without replacing earlier exports. Both XML outcomes, accepted
deviations, copy checksums and external-reference limitations travel with the
package. Broader semantics and receiving-catalogue compatibility remain GAP-018.

v5.0.0 closes GAP-018 within the agreed **local-validation** scope. Its
[acceptance record](v5.0.0/local-acceptance.yml) and
[semantic profile](v5.0.0/semantic-policy.yml) define the tested limits.
Canonical list URIs and IANA encoding labels replace obsolete export references.
Publication now requires code and consistency checks in addition to either XML
policy. Missing semantic resources block publication. This tightened gate is a
major version; no saved labels or draft requirements change. Receiving-catalogue
compatibility is `not-assessed` and deferred separately as GAP-020.

Authoritative references:

- https://www.iso.org/standard/53798.html
- https://www.iso.org/standard/80874.html
- https://www.iso.org/standard/78900.html
- https://schemas.isotc211.org/19115/-1/
