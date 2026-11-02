# ISO metadata gap-infill plan

Status: Waves 1–3 implemented; GAP-018 resolved for local validation in crosswalk v5.0.0; GAP-020 deferred until a receiving catalogue is selected  
Branch: `dev-iso-metadata-gap-infill`  
Baseline crosswalk: `inst/extdata/iso_crosswalk/v1.0.0`  
Prepared: 2026-08-31

## Population review follow up

The [metadata simplification assessment](metadata-population-assessment.md)
reviews the additive model, establishes canonical versus compatibility fields,
and scopes rough inventory versus full metadata population. Crosswalk v3.2.0
adds the capture policy. v3.2.1 connects the Prickly Porcupine and Armoured
Pangolin choice to entry, draft saving, folder creation and status. The app
also removes duplicate lineage entry; readiness separation remains scoped work. GAP-019 also records supplementary constraints that must not be
lost through legacy-only fallback rules.

## Objective

Close the ISO profile gaps without weakening the current offline workflow or
losing metadata already stored in `entry_tbl`. Work proceeds as additive,
testable vertical slices: database migration, repository functions, app UI,
hydration/status, transfer, crosswalk, and export validation are completed
together for each slice.

## Prioritization method

Ordering considers:

1. whether a gap can make an ISO export invalid or misleading;
2. whether it blocks several later fields or serializers;
3. the user's current lineage and supporting-document workflow pain;
4. whether structured data already exist and only need alignment;
5. migration and UI complexity.

Complexity is relative: S is a focused additive change, M affects a module and
several repository functions, and L introduces repeatable records or a new
export model.

## Original implementation backlog (historical)

This table preserves the original implementation order, not the current open
gap list. The [v5.0.0 register](../inst/extdata/iso_crosswalk/v5.0.0/profile-gaps.yml)
records GAP-018 as resolved locally, GAP-019 as resolved for export, and
GAP-020 as deferred.

| Rank | Gap | Wave | Complexity | Outcome |
|---:|---|---|:---:|---|
| 1 | GAP-018 | 0 | L | ISO serializer and XSD/Schematron validation fixtures establish a measurable definition of done. |
| 2 | GAP-008 | 1 | L | Repeatable lineage sources and process steps replace the current flattened record while preserving its summary. |
| 3 | GAP-003 | 1 | S | Store metadata resource scope instead of assuming every record is a dataset. |
| 4 | GAP-002 | 1 | S | Store metadata language and encoding; retain `eng`/UTF-8 defaults for migrated records. |
| 5 | GAP-004 | 1 | M | Add repeatable controlled ISO topic categories, distinct from free keywords. |
| 6 | GAP-005 | 1 | M | Add repeatable typed citation dates while preserving `entry_tbl.citation_date` as publication-date compatibility data. |
| 7 | GAP-013 | 1 | M | Add explicit metadata responsibilities instead of inferring authorship from record creators and reviewers. |
| 8 | GAP-001 | 1 | S | Persist a metadata-record UUID distinct from the resource UUID. |
| 9 | GAP-011 | 2 | L | Add repeatable online resources and structured distribution formats. |
| 10 | GAP-009 | 2 | L | Add repeatable ISO 19157 quality measures and results; keep scan status as workflow evidence. |
| 11 | GAP-007 | 2 | S | Add an equivalent-scale denominator; retain min/max capture/use scale as a DataPond extension. |
| 12 | GAP-012 | 2 | M | Consolidate embargo data with licensing and add start/release/dissemination semantics. |
| 13 | GAP-017 | 2 | L | Add repeatable temporal extents with precision and indeterminate endpoints. |
| 14 | GAP-006 | 2 | S | Add alternate titles. |
| 15 | GAP-010 | 3 | M | Add optional vertical extent, vertical CRS, and units. |
| 16 | GAP-014 | 3 | M | Add thesaurus citation/version/URI metadata to keyword vocabularies. |
| 17 | GAP-015 | 3 | L | Generate and link ISO 19110 feature catalogues for vector and tabular fields. |
| 18 | GAP-016 | 3 | M | Normalize addresses and organisational units where detailed contact export requires it. |

## Wave 0 — conformance guardrails

### GAP-018 progress — 2026-10-19

These dated milestones preserve the implementation history. **Current status:
resolved for local validation in crosswalk v5.0.0**, with the agreed closure
scope and acceptance criteria recorded below. Catalogue-specific import testing
is deferred separately as GAP-020 because no target is selected.

At this earlier milestone, the validation foundation and core, distribution/licensing,
quality and content/catalogue slices were implemented, but complete-candidate
validation was not yet finished. That work is now complete within the
[v5.0.0 local acceptance scope](#gap-018-local-closure--crosswalk-v500).
Porcupine/Pangolin saving and completion requirements were unchanged.

- `inst/xsd/1.3.0/manifest.json` pins the official ISO/TC 211
  package and its dependency closure by source URL and SHA-256. The original
  upstream files are retained locally, including the official minimal example.
- `validate_iso_metadata_xml()` runs an offline XSD check plus 14 published
  Schematron files (13 metadata rule files and the MDQ measure-reference rule). Its report separates `pass`, `fail`, and `unavailable`, with
  XML locations, rule expressions and source URLs. The reference-system rule
  file contains no assertions; its presence is not a CRS conformance check.
- `build_iso_metadata_candidate_xml()` assembles the existing Wave sections
  and structured lineage, including citations of external lineage documents.
  `assess_iso_metadata_export()` returns that candidate and its report, or
  `not-built` with the assembly error. Neither function publishes an export.
- Regression tests exercise the official minimal example, mandatory contact,
  property ordering, a conditional organisation-name omission, older namespace
  rejection, malformed XML, missing tooling, altered resources, and lineage
  composition. The original vector/raster namespace failure has now been
  resolved for core exports. A nonspatial tabular fixture with `dataset` scope
  still returns `not-built` because preflight demands bounds and CRS. Dataset
  scope is retained; the fixtures do not invent locations or masquerade as services.

The **core export milestone**, tracked by crosswalk **v3.2.4**, provides:

- `build_iso_core_metadata_xml()` and `assess_iso_metadata_export(...,
  sections = "core")`, with an explicit `export_scope` in the assessment.
  The default `sections = "all"` still evaluates the complete provisional
  candidate; core success never changes a complete candidate's failing status.
- Current 1.3.0 metadata/lineage namespaces and schema property order; the
  independent ISO 19110 catalogue and ISO 19157 quality implementation remain
  unfinished specialist work.
- Stored title, abstract, purpose, credit, version, resource UUID/citation key,
  metadata UUID, locale, metadata responsibilities, typed citation dates and
  topics. Metadata creation uses its explicit stored value, otherwise the
  earliest recorded review; the latest review supplies `lastUpdate`. Export
  does not use the current time or resource publication date as metadata creation.
- Validated WGS84 summary bounds and distinct structured native CRS identifiers
  from included spatial records/active manual extents. Inactive files are
  excluded, duplicate authority codes are collapsed, and names/WKT are not
  substituted for authority identifiers.
- Lineage statement, document citations, source descriptions/citations/scale,
  process descriptions/rationale/output/software/script references, recorded
  step dates in `gml:TimeInstant`, and correctly namespaced `xlink:href` source
  references. Step dates retain their precision and timezone.
- Passing vector and raster **core** database fixtures, a structured-lineage
  fixture, value comparisons against the database, partial-date tests,
  conditional source-description failures, invalid-date/bounds/CRS failures,
  and a test proving that an excluded specialist section still fails when
  assessed in the complete candidate. Repeat export produces identical XML.

The core is a defined subset, not a release-ready package. It does not yet
export all descriptive/specialist fields, resource responsibilities, licence
constraints, maintenance, or every structured lineage property (for example
source temporal coverage/responsible parties and process processors).
Those were subsequent crosswalk milestones, now completed within the v5.0.0
local scope. This historical core slice did not include an export button or
package installation.

The **distribution and licensing milestone**, tracked by crosswalk **v3.2.5**,
adds `build_iso_distribution_metadata_xml()` and assessment scope
`sections = "distribution"`. This includes core metadata and implemented lineage,
plus the following stored values:

- Explicit format titles, versions, specification URLs and MIME types. MIME
  types use ISO `gcx:MimeFileType`, not physical `mrd:medium` properties.
- Ordered download/information resources, preserving URL, protocol, name,
  description, function and MIME type. The primary URL mirror is not repeated.
- Explicitly assigned distributor organisations, deduplicating the selected
  distributor and relationship rows. Custodians are not inferred as distributors.
- Licence citations (name, identifier, URL), structured rights holders, access,
  use limitations, required attribution, redistribution and sensitivity statements.
  Internal licence notes and assessment/review workflow fields remain excluded.
- Embargo facts and correctly nested releasability. A dissemination code alone
  still fails the ISO requirement for an addressee or release statement.
- Additive supplementary access/use notes alongside structured terms. Within
  each target property, only whitespace-equivalent text is deduplicated; case,
  punctuation and distinct wording remain. No pond records are rewritten.
  The legacy licence title is a separate fallback when structured licence identity
  is absent, including when the saved rights row contains only embargo details.

This resolves **GAP-019's export omission**. The later
[2026-10-21 real-pond audit](pond-rights-audit-2026-10-21.md) covers licensing and
access/use notes in dta_scp and BSPI xdrive. It found partial overlap and rights
disagreements requiring review; no saved values were changed. No new metadata
entry fields are required.

The complete candidate uses these same serializers. Vector/raster distribution
fixtures pass XSD and the adapted Schematron rules with database-value checks;
a complete vector fixture with alternate title and scale also passes. A broad,
unspecified quality test still blocks the complete candidate while the distribution
subset passes; the quality milestone below handles precisely selected tests.
Detected file-format fallback and inferred transfer sizes were later assigned
explicit mapping dispositions: inventory alone does not identify the distributed
package. Content, linked catalogues and complete crosswalk coverage were
subsequent GAP-018 milestones, now covered by v5.0.0 local closure. This
historical slice added no app export button or installation.

The **quality export milestone**, tracked by crosswalk **v3.2.6**, adds
`build_iso_quality_metadata_xml()` and assessment scope `sections = "quality"`.
It combines core metadata, implemented lineage, distribution, licensing and
explicit quality results. The complete candidate uses the same quality serializer.

- Official [ISO 19157 MDQ 1.2.0 schemas](https://schemas.isotc211.org/19157/-/)
  and the measure-reference rule are pinned alongside metadata 1.3.0, with local
  dependency resolution and checksum checks. No runtime download is needed.
  Standalone DQM measure catalogues and all conceptual ISO 19157-1 requirements
  are outside this milestone's validation claim.
- Fifteen concrete test types cover the existing five broad categories. ISO's
  broad classes are abstract and cannot be instantiated; the exporter must not
  infer omission versus commission, or a particular accuracy test, from a broad
  category. The existing **Measure type** selector now offers precise choices.
  The nullable `quality_element_code` stores that choice; `measure_type_code`
  remains its derived broad compatibility category. There is no second category
  question. Legacy broad records remain saveable and are not automatically
  reclassified; export reports exactly which result needs clarification.
- Measure identifiers and names use `DQ_MeasureReference`; evaluation methods
  retain recorded text. Complete timestamps retain seconds, fractions and time
  zones. Year/month/day-only dates are appended as labelled evaluation text,
  because the dedicated evaluation-time property requires a timestamp. The
  editor accepts the recorded precision and no longer truncates imported times.
- Quantitative values use a structured `gco:Record`, preserving the stored
  lexical value. Unit URIs remain references; other unit labels become local
  GML unit definitions without invented identifiers from a registry or conversion
  factors. Missing units are omitted. Quantitative explanations are accompanying
  descriptive results under the same measure.
- Conformance results preserve specification, explanation and explicit true/false;
  descriptive results preserve their statement. Missing values, incompatible
  populated result fields, category mismatches and invalid dates block assembly
  with an actionable message. File/attribute scope links must belong to this
  dataset; their names are exported without local directory paths.
- Vector and raster combined fixtures cover all three result kinds, failed and
  passed conformance, units, date precision, scope and database-value comparisons.
  A complete vector candidate with these sections populated passes. Additional
  checks cover all fifteen precise choices, missing measure references, invalid
  scope links, editor persistence and dataset transfer.

This slice preserves draft-saving requirements and uses temporary test ponds.
It added neither a public export action nor a package installation. At that
point GAP-018 still required the subsequent content, mapping-coverage,
nonspatial and metadata-date milestones documented below. These are now
complete within the v5.0.0 local acceptance scope.

The **content export milestone**, tracked by crosswalk **v3.3.0**, adds:

- `sections = "content"` builds core, lineage, distribution, public licensing,
  quality and content together. Both `content` and `all` assess the catalogue
  separately and require its filename, UUID, title, version and date to agree
  with the metadata citation. A metadata-only pass cannot hide a bad catalogue.
- Active raster attributes produce one coverage per file/layer, typed band
  names, aliases, descriptions, units and finite numeric minima/maxima.
  Saved counts and profile time are named profiling evidence, not quality
  assertions. DataPond does not currently capture a NoData sentinel; none is
  inferred from `missing_count`.
- GFC 2.2 catalogues use a saved persistent identity, field definitions,
  aliases, types, units and local feature/catalogue references. Missing identity
  blocks assembly. Sample examples and min/max values are not permitted-value
  domains. The unconditional `complianceCode=true` declaration is removed.
- Required but uncaptured properties use legal `nilReason="unknown"`, including
  cardinality, feature-type definitions and raster content type. Validator
  warnings list these locations. Missing catalogue version/date or producer
  also stays explicit. Structural success does **not** mean fully populated
  metadata or semantic ISO certification.
- The offline bundle explicitly imports GFC 2.2.0 (already pinned in the
  dependency closure). Catalogue checks are XSD, embedded citation rules and
  local reference resolution. No official GFC Schematron is claimed.
- Dataset language and character encoding are optional, clearable fields in
  the ISO profile panel, separate from metadata language/encoding. Existing
  records remain blank; older save callers preserve populated values. The
  pinned catalogue-link rule requires dataset language, and the pinned locale
  rules require UTF-8. Those unmet conditions fail assessment without inventing
  values. Porcupine/Pangolin saving requirements remain unchanged.
- Combined vector/raster fixtures compare exported values to database rows.
  A standalone tabular catalogue passes, while complete nonspatial metadata
  still reports its unresolved geographic-discovery requirement. Missing raster
  statistics still fails the pinned sample-dimension rule; none are fabricated.
- The internal `write_iso_wave3_documents()` now assesses both documents before
  writing. Validation failure leaves destination files unchanged. This is not
  yet a transactional publisher: rollback after an I/O failure and the app
  export/report UI remain future work.

For source-loaded development tests:

```r
assessment <- datapond:::assess_iso_metadata_export(
  dataset_id, pond_path, sections = "content"
)
assessment$validation$status
assessment$validation$issues
assessment$validation$warnings
assessment$validation$feature_catalogue$warnings
```

The returned `feature_catalogue` and `feature_catalogue_filename` accompany the
metadata document. No XML is written by assessment. This milestone was tested
on temporary ponds and has not been installed into the user's R library.

The **contact, citation and structured-lineage milestone**, crosswalk **v3.3.1**,
adds a shared contact exporter and combined vector/raster fixtures:

- Resource citation authors now follow `authority_tbl.author_order`. Explicit
  resource contacts and organisation roles are exported separately from metadata
  contacts. Exact repeated party/role links are collapsed, while different roles
  remain distinct. Review events and app users do not automatically become authors.
- Contacts include recorded position, email, phone, postal and online details in
  schema order. Normalized values win per channel; missing channels can use saved
  legacy person fields. Recorded organisation affiliation becomes an embedded
  individual. An explicitly different organisation stays a separate party.
  Organisation names preserve units and ancestors without invalid XML properties.
- The same contact serialization serves metadata contacts, citation authors,
  source responsibilities, processors, thesauri, catalogue producers, linked
  distributors and linked rights holders. Missing parties, unusable names,
  unrecognized roles and cyclic organisation hierarchies produce actionable errors.
- Thesaurus citations retain identifier, edition, date and responsible parties in
  correct order. Resource citation-date notes become labelled citation details.
- Source temporal ranges now use `LI_Source/scope/MD_Scope/extent`, correcting the
  obsolete `sourceExtent` crosswalk path. Endpoint precision is retained; missing
  endpoints and uncaptured scope levels stay explicitly unknown.
- Source distance uses the recorded unit. When both distance and equivalent scale
  exist, the schema permits only one structured resolution: scale is retained
  there and distance is added as labelled source text. Neither saved value is
  changed. Invalid numeric values or missing units block export only.
- Explicit document inclusion flags control resource/lineage citations and their
  type/notes. An authoritative source link also emits a single document citation
  and a local source reference, but does not itself authorize document notes.
  Inactive or foreign authoritative documents block assembly; absolute or file-URI
  document references are rejected. File copying/resolvability remains part of the
  future publication workflow.

Tests compare author order, role/party association, contact channels, identifiers,
source dates/resolutions and processor/document links against populated fixtures.
Existing core, distribution, quality, content, lineage and transfer tests are also
run. This milestone adds no fields, migrations, UI controls or installation.

The **maintenance and resource-status milestone**, crosswalk **v3.3.2**, adds
the following to core and all broader export scopes, using existing fields:

- `data_complete_status`: `complete` becomes `completed`; `started` becomes
  `underDevelopment`. Published progress codes are accepted. Unmatched text is
  preserved as labelled supplemental information with an unknown status nil
  reason. Blank status is omitted. Release/archive workflow states do not
  establish resource progress and are no longer proposed as fallbacks.
- `update_frequency`: published frequency codes and explicit wording aliases
  become ISO maintenance frequency codes. Other wording (for example, “every
  decade; subject to funding”) remains a maintenance note with frequency
  `unknown`. Blank frequency adds no resource-maintenance block.
- A single metadata-maintenance block contains the saved review events as
  repeatable `CI_Date` values of type `lastRevision` and labelled event notes.
  Timestamp precision and time zone are retained. Legacy `review_date` supplies
  a date only when the timestamp is blank; invalid populated dates block export.
  Events retain saved row order, without exposing local row IDs. This does not
  change the separate core creation/last-update date policy.
- Metadata maintenance frequency is explicitly `unknown`: past reviews and the
  resource update schedule do not establish a metadata review schedule. Scope
  is `metadata`; affected app sections stay as labels within their event note.
- Reviewers use shared contact details and recorded affiliations. Contacts are
  deduplicated by person ID and role. A saved legacy name is a name-only fallback
  when no person is linked. Missing linked people block export. Blank roles and
  job titles that are not ISO roles use an unknown role nil reason; recorded role
  wording remains in the event note. Reviews do not create dataset authors.
- Update reasons and summaries are export-facing descriptive text. Generic
  `notes`, update types, metadata workflow status, change source and app version
  remain local audit information. There is no automatic public/private classifier
  for fragments of free text; this policy does not publish generic audit notes.

The mappings were checked against the published ISO/TC 211
[progress](https://schemas.isotc211.org/resources/codelists/ISO19115-1.1.mcc.MD-ProgressCode/),
[frequency](https://schemas.isotc211.org/resources/codelists/ISO19115-1.1.mmi.MD-MaintenanceFrequencyCode/),
[date-type](https://schemas.isotc211.org/resources/codelists/ISO19115-1.1.cit.CI-DateTypeCode/)
and [scope](https://schemas.isotc211.org/resources/codelists/ISO19115-1.1.mcc.MD-ScopeCode/)
catalogues. Runtime export remains offline and uses explicit code allowlists.
Fixtures check value retention, contact ownership, duplicate contacts, precision,
unmapped text, excluded audit fields, malformed dates and broken links, alongside
vector/raster schema and Schematron validation. No fields, UI controls or
installation are added by this milestone.

The **vertical-extent export milestone**, crosswalk **v3.3.3**, corrects the
Wave 3 / `sections = "all"` serializer:

- Bounds use `gco:Real` without the invalid `uom` attributes. Saved double
  precision, negative heights and positive depth ranges are preserved; export
  never converts units, flips signs or rewrites the pond.
- Extent label, note, unit symbol/URI, CRS name and recorded reference type are
  retained in a labelled extent description. Rows retain `sort_order, id` order.
- Saved identifiers use `verticalCRSId/MD_ReferenceSystem`; explicit `AUTH:CODE`
  strings are split into code and code space. Opaque identifiers stay whole.
  Saved HTTP(S)/URN references use `verticalCRS` links; local file URIs fail.
- Recognised EPSG forms (`EPSG:code`, unversioned OGC URN, OGC `/def/crs/EPSG/0/`
  URL and plain `epsg.io/code` URL) resolve through the installed sf/GDAL/PROJ
  database, with no URL fetch or coordinate transformation. Export requires a
  standalone vertical CRS and matching axis unit. Height/depth reference types
  must agree with its up/down axis. Horizontal, compound and geographic 3D CRS
  codes require an appropriate vertical component for this slice.
- The locally resolved WKT2 definition is embedded through
  `mrs:crs/rbc:RS_CRS/rbc:definition`, the CRS wrapper supported by the pinned
  1.3.0 schema. A GML `VerticalCRS` is not substituted into this RBC property.
  The definition supplies datum, axis and unit details from the local registry;
  the exporter does not invent them from a CRS name.
- Metre, international-foot and US-survey-foot aliases are distinguished.
  Conflicting recognised EPSG identifiers/URIs, unit symbols or up/down types
  block assembly. The verified QUDT metre/foot URI aliases are also checked
  against symbols. Other unit URIs are retained as references, not dereferenced
  or certified as equivalent. Unknown EPSG codes require a local registry that
  can resolve them; they are not silently treated as custom identifiers.
- Custom identifiers and URI-only custom references remain available, with an
  assessment warning that unit/axis compatibility is unverified. A CRS name
  alone yields an explicit `nilReason="unknown"`, visible in validation warnings.
  Schema success for these cases does not establish reference completeness.
  Barometric/local coordinate definitions remain reference-only unless supported
  by a future dedicated resolver; they are not invented from the type selector.

This follows the pinned [extent schema](../inst/xsd/1.3.0/schemas.isotc211.org/19115/-1/gex/1.3.0/extent.xsd),
[reference-system schema](../inst/xsd/1.3.0/schemas.isotc211.org/19115/-1/mrs/1.3.0/referenceSystem.xsd)
and [RBC wrapper](../inst/xsd/1.3.0/schemas.isotc211.org/19111/-/rbc/3.1.0/rbcStubs.xsd).
Local CRS inspection uses the existing dependency's
[sf CRS interface](https://r-spatial.github.io/sf/reference/st_crs.html).
Unit aliases follow the QUDT [metre](https://qudt.org/vocab/unit/M.html) and
[foot](https://qudt.org/vocab/unit/FT.html) definitions.

Vector/raster fixtures cover exact numeric round trips, row ordering, all saved
descriptive values, depth and survey-foot definitions, custom/name-only
references, invalid input and excluded subset scopes. Mutation checks prove the
pinned validator rejects illegal bound units and absent CRS properties. The
installed PROJ database supplies the WKT, so its version can affect that text;
tests check semantic properties rather than a byte-for-byte registry snapshot.
No database migration, capture field, UI change or installation is added.

The **remaining mapping audit**, crosswalk **v3.4.0**, resolves those mapping
decisions. Its [coverage ledger](../inst/extdata/iso_crosswalk/v3.4.0/mapping-coverage.yml)
accounts for all **162 mappings**: 119 implemented, nine handled through
canonical migration, and 34 retained locally without an independent ISO property.
Each entry records its decision and implementation/regression evidence. This
is a mapping audit, not proof that every possible record or value combination
passes ISO validation.

The full candidate now includes:

- Vector representation and geometry families, with primitive counts only
  where simple geometry makes them equivalent to the saved feature count.
  Multipart and mixed counts remain labelled descriptive values.
- Raster row/column sizes and separate y/x resolutions, using units resolved
  locally from the recorded CRS. Band counts remain labelled values;
  unknown axes, cell geometry and transformation availability are explicit
  nil reasons, not invented facts.
- Both temporal extent labels and descriptions, with saved endpoint validation.
- Legacy software/cell-size fallbacks, and strictly bounded legacy EPSG/WGS84
  bbox fallback only when no structured spatial records exist. Excluded or
  invalid modern records cannot revive stale legacy bounds.
- Extent derivation and contributing manual extent notes in the geographic
  extent description. These describe metadata bounds, not data-production
  steps.

The audit also corrects proposed mappings that lack evidence: importing a
bundle preserves the same resource UUID and does not establish an upstream
source or production step; record creation/release audit does not establish
authorship, publisher or publication date; inventory formats/sizes do not
establish distribution specifications or transfer sizes. Explicit responsibilities,
citation dates and distribution records remain authoritative. The existing
legacy licence-access text fallback now has its own mapping, MCO-018.

Synthetic vector/raster fixtures check values, units, exclusions and conditional
failures as well as the pinned XSD/Schematron rules. No additional capture fields,
UI changes, installation or operational pond migration are part of this milestone.
The focused ISO, lineage, keyword migration and dataset-transfer regression run
passed **2,343 assertions**, with no failures, errors or skips. Combined
contact/lineage, maintenance, vertical and new spatial/temporal vector and raster
examples also passed the pinned validation checks under crosswalk v3.4.0.
At v3.4.0, nonspatial discovery and date-only metadata creation remained
unresolved. The following milestone implements the agreed handling.

The **nonspatial and creation-date milestone**, crosswalk **v4.0.0**, adds:

- An explicit `entry_tbl.geographic_extent_name`, shown as **Area covered** in
  Advanced metadata profile. Geographic tables can export this as
  `EX_GeographicDescription/geographicIdentifier` without inventing a bounding
  box or requiring a CRS. Supplied coordinate extents retain their validation.
  Keywords are not inferred as coverage.
- `nonGeographicDataset` export with the resource title automatically supplying
  the required scope name. This scope requires neither bounding box nor CRS.
  Scope is selected by the user; a file extension or absence of geometry does
  not establish that the data are nongeographic. Dataset status recognises the
  named-area alternative.
- Automatic UTC creation timestamps on the first application save. The field
  is now part of the startup schema, with no migration-time backfill. Imports
  retain original timestamps, date-only values and unknown creation dates.
  Existing dates can be corrected or cleared in Advanced metadata profile,
  using a text entry that preserves their precision.
- A tighter creation fallback: only review events explicitly tagged
  `new_metadata_record` can supply a missing creation timestamp. Ordinary
  review events continue to supply update/maintenance history. This changes
  previous export eligibility, hence the **major crosswalk version**; an older
  record without creation evidence now needs its actual creation date.
- A separate [DataPond date policy 1.0.0](../inst/extdata/iso_crosswalk/v4.0.0/validation-policy.yml).
  The original pinned result remains at `validation$status`; the separate
  `validation$datapond_profile` accepts an XSD-valid creation `Date` in the two
  root creation assertions. It records every accepted deviation. Missing or
  invalid dates, other assertions, catalogue/link failures and unavailable
  tooling still prevent a profile pass. Pinned schemas and their hashes are
  unchanged.

The coverage ledger now accounts for **164 mappings**: 121 implemented, nine
canonical migrations and 34 local dispositions. No new mandatory draft-entry
requirements are introduced in either population mode.

The internal writer defaults to pinned validation. Explicitly selecting
`validation_policy = "datapond"` allows the declared date policy and writes
`validation-report.json` alongside the XML, preserving both outcomes and the
selected policy. This remains a developer writer, without a new app export
button or transactional publication guarantee.

Verification used only temporary ponds. The repository-wide test sweep passed
apart from the former CRS-only expectation, which was updated for the named-area
alternative. The final affected-suite rerun passed **147 assertions** with no
failures, errors or skips. Combined vector, raster, named-area and nongeographic
examples passed pinned validation; the date-only example retained pinned failure
and DataPond-policy success in its saved report.

The schema permits `gco:Date` values at year/month/day precision, so citation
dates no longer fail merely for lacking a day. An explicit **metadata creation
date** at day precision is also retained as `gco:Date`; however, the bundled
creation-date Schematron rules demand `gco:DateTime`. That case deliberately
reports XSD pass/Schematron fail, with an explanation, rather than inventing a
time or replacing an explicit date with a review timestamp. The separate
DataPond policy above provides a declared compatibility route. It is not an
official correction of ISO's interpretation or a full-conformance claim.

Two upstream 1.3.0 Schematron files (`gex.sch` and `mdb.sch`) bind `gco` to
`https://schemas.isotc211.org/19115/-1/gco/1.3`, whereas their XSDs use
`https://schemas.isotc211.org/19103/-/gco/1.2`. The runner corrects only those
namespace declarations in memory. In addition, `mrl.sch` describes an
`LI_Source` constraint but targets the nonexistent `mrl:LI_Scope`; the runner
corrects that rule context to `mrl:LI_Source`. These three metadata adaptations are
recorded in every report and tested. The MDQ 1.2.0 rule also declares four older
namespace URIs (`mdq`, `cit`, `mcc`, `gco`); these are rebound to its paired XSD
namespaces in memory. Reports now identify seven adaptations, and a deliberately
empty measure reference proves the quality assertion actually runs. The assertion expressions remain unchanged.
The ISO XSLT skeleton accepts the
upstream interleaved diagnostics/multilingual titles; its optional Relax NG
layout gate is disabled. This is an explicitly adapted rule run, not a claim
that the unmodified upstream rules pass. XSD dependency locations are resolved
to canonical local files so HTTP/HTTPS aliases do not load declarations twice.

Run the optional developer assessment from a source checkout:

```r
pkgload::load_all()
result <- datapond:::assess_iso_metadata_export(dataset_id, base_path, sections = "quality")
result$export_scope
result$validation$status
result$validation$issues
result$validation$adaptations
# Separate, declared date-precision policy; does not replace the pinned result:
result$validation$datapond_profile$status
result$validation$datapond_profile$accepted_deviations
# For the complete provisional candidate, use sections = "all".
```

The validator requires Python 3.9+ with `lxml`; set `DATAPOND_ISO_PYTHON` if the
executable is not `python3` on PATH. The app itself has no new Python requirement.
Schemas are bundled: assessment does not download anything. The existing
builders can perform their normal schema migrations, so development audits of
older ponds should use copies. No operational pond was used for this slice.

### App-facing validation and portable packages (crosswalk v4.1.0)

Dataset status now exposes **ISO metadata validation & export**, available in
both population modes. It checks saved metadata, reports pinned and declared
DataPond date-policy outcomes separately, and lists local-file readiness as a
third result. Issue cards open the relevant editing section; recognised profile
issues also reveal and focus the field. Opening optional detail does not switch
population mode. Technical rule output, unknown values, adaptations and accepted
date deviations remain inspectable and downloadable as JSON.

Save current edits before checking. Reports are cleared on dataset/pond changes
and ordinary save/refresh events. A report records its assessment time; external
changes to files are checked again on download. A failed assembly currently
reports the first blocking assembly error, so resolve it and rerun the check.

**Validate & download ISO package** is available only when the selected policy
and local dependency checks pass. It independently reassesses the saved record
at download time. The ZIP includes metadata XML, any generated feature catalogue,
the report, and local supporting documents/process scripts actually cited in XML.
Active/inclusion flags and explicit source links retain their existing selection
meaning. Unselected documents and dataset data files are not copied. HTTP(S)
references remain external and are not fetched or checked for availability.

Local files must exist inside the saved dataset folder. Traversal, escaping
symlinks, ambiguous URI paths and generated-filename conflicts block publication.
The staging copy must match each assessed source checksum. XML and catalogue
bytes are validated again inside the staging directory, then a single rename
publishes a new uniquely named package. Failure removes only that staging
directory. The internal `write_iso_wave3_documents()` now treats `output_dir` as
the **parent** of a new package folder and returns its actual file paths; it
does not write loose XML into that parent or replace earlier packages. The app
uses temporary storage and downloads a ZIP without writing exports into the pond.
Checksums support copy integrity, not signatures or authenticity guarantees.

The [package policy](../inst/extdata/iso_crosswalk/v4.1.0/package-policy.yml)
records these boundaries. Python/lxml remain optional until validation is
requested; the `zip` executable is needed only for ZIP download.

Regression coverage in `test-iso-export-package.R` exercises dependency
selection, missing and escaping files, filename conflicts, copy failures,
changed source bytes, staged-validation and publication failures, ZIP contents,
missing tools, policy selection and Shiny report invalidation/navigation.
Fixtures use temporary ponds; operational records are not rewritten.

### GAP-018 local closure — crosswalk v5.0.0

The agreed scope is **complete local validation, with no receiving catalogue
selected**. GAP-018 is resolved within that scope. The versioned
[acceptance record](../inst/extdata/iso_crosswalk/v5.0.0/local-acceptance.yml)
ties each deliverable to executable tests. All 164 mapping dispositions remain
accounted for; this does not claim exhaustive correctness for every possible
combination of saved values.

The new **Export semantics / code lists** result is separate from the pinned
XSD/Schematron and explicit DataPond date-policy results. Both export policies
require this semantic check and local supporting-file checks to pass. The writer
repeats semantic validation on the staged XML/catalogue pair before publishing.
Missing tooling, altered registries or an incomplete check block publication.
The app displays semantic errors with editing guidance and includes all results
in the downloadable report. Receiving-catalogue compatibility is always shown
as `not-assessed` until a concrete target can be tested.

Local code checks use unmodified, SHA-256-pinned snapshots of:

- [ISO/TC 211's canonical code-list catalogue](https://schemas.isotc211.org/resources/codelists/).
- [Library of Congress ISO 639-2 language codes](https://www.loc.gov/standards/iso639-2/).
- [IANA character sets](https://www.iana.org/assignments/character-sets/character-sets.xml).

The old `19115/resources/Codelist/cat/codelists.xml` export references returned
404 during verification. Serialization now uses canonical list references.
Encoding compatibility labels are translated only in XML: `utf8` → `UTF-8`
and `utf16` → `UTF-16`. This exporter writes UTF-8 metadata, so a contradictory
metadata-locale encoding blocks export; the dataset's own encoding is independent.
No database fields, values or Porcupine/Pangolin draft-save requirements change.

The bounded [semantic profile](../inst/extdata/iso_crosswalk/v5.0.0/semantic-policy.yml)
checks registered codes, list URIs, topic and catalogue language values, empty
text, unique XML IDs and local references, calendar-valid dates, extent ordering,
finite numeric bounds and positive spatial measurements. Year/month dates remain
intervals. A definitely reversed interval fails; overlapping partial dates,
indeterminate endpoints and mixed zoned/unzoned times remain explicitly qualified.
Antimeridian-crossing longitude bounds are allowed. Custom code extensions need
an explicitly extended profile; these checks do not assert that ISO itself
forbids code-list extensions.

End-to-end package fixtures cover vector and raster records, geographic tables
with a named area, nongeographic tables and year-only historical creation.
They include all three quality result types, retain an explicitly false
conformance result, preserve dataset encoding and validate generated catalogues.
Richer contact/lineage, maintenance, vertical and spatial fixtures assert both
pinned and semantic outcomes. Negative cases demonstrate export blocking and
preservation of prior packages; Shiny tests cover the report and download gate.
Run the source regression suite with:

```r
pkgload::load_all()
testthat::test_dir("tests/testthat")
```

Crosswalk v5.0.0 is a major version because the semantic gate can reject records
that previously passed XML checks. The original pinned rule resources and
date-precision policy are unchanged. This closes the local engineering gap,
not factual accuracy, every ISO 19157 conceptual condition, exhaustive combination
coverage, remote-URL availability, signed-document authenticity or ISO certification.

**GAP-020** tracks receiving-catalogue compatibility. When a catalogue, version
and import profile are selected, test representative packages against those
requirements. Until then no acceptance by an external catalogue is claimed.
Neither population mode means ISO certification.

### Deliverables

- Add an internal crosswalk loader resolving `iso_crosswalk/CURRENT`.
- Add an ISO 19115-3 XML serializer with small, composable builders for
  citation, parties, identification, constraints, extent, distribution,
  lineage, maintenance, and content.
- Add three fixtures: vector, raster, and nonspatial/tabular.
- Validate fixtures against the official ISO/TC 211 XSD and Schematron rules.
- Record the crosswalk version in every generated metadata document.
- Fail export with actionable messages when required profile values are absent;
  do not fabricate values beyond documented profile constants.

### Primary code areas

- New `R/iso_crosswalk.R` and `R/iso_export.R`.
- New tests under `tests/testthat/test-iso-export-*.R`.
- Existing crosswalk validation in `tests/testthat/test-iso-crosswalk.R`.

## Wave 1 — core identity, discovery, responsibility, and lineage

### Slice 1A: repeatable lineage and document authority

Database additions:

- `dataset_lineage_source_tbl`
  - source order, title, identifier/code space, description, source URI;
  - citation date/type, temporal extent, equivalent scale/resolution;
  - responsible person/organisation and role;
  - optional authoritative `supporting_document_id`;
  - audit fields.
- `dataset_lineage_process_step_tbl`
  - step order, description, rationale and date/time;
  - processor person/organisation and role;
  - tool/software reference, script reference and output description;
  - audit fields.
- `dataset_lineage_step_source_tbl` for many-to-many step inputs.

Migration:

- Preserve `dataset_lineage_tbl` as the one-row dataset summary.
- Backfill its source text into one provisional source and its process text into
  one provisional process step only when structured rows do not already exist.
- Never attempt to split multiple sources from punctuation or line breaks.

App changes:

- Extend `mod_lineage.R` with ordered source and process-step editors.
- Allow an included supporting document to become an authoritative source
  citation without requiring its content to be copied into every field.
- Show which values came from documents, user entry, or extracted metadata.
- Keep the concise overall lineage statement visible above the detailed lists.

Integration:

- Include all new tables in dataset transfer/import.
- Update dataset completion status to assess the summary, sources, and process
  steps conditionally rather than requiring every possible lineage field.
- Export `LI_Lineage`, repeatable `LI_Source`, `LI_ProcessStep`, and
  `additionalDocumentation`.

### Slice 1B: metadata identity and defaults

Add to `entry_tbl` through an idempotent migration:

- `metadata_uuid`;
- `metadata_resource_scope_code`, default `dataset` for existing records;
- `metadata_language_code`, default `eng`;
- `metadata_character_encoding`, default `utf8`.

The app should place these in an advanced metadata-profile block rather than
crowding Dataset intro. Scope changes must drive conditional validation: bbox,
CRS, and topic-category requirements apply only where the selected profile
requires them.

### Slice 1C: topic categories and typed dates

Add:

- `dataset_topic_category_tbl(dataset_id, topic_category_code, sort_order,
  audit...)` with ISO code-list validation;
- `dataset_citation_date_tbl(dataset_id, date_value, date_type_code,
  date_precision, date_note, sort_order, audit...)`.

Compatibility:

- Backfill `entry_tbl.citation_date` as a `publication` date.
- Continue updating `entry_tbl.citation_date` from the primary publication date
  while older reports and citation-key logic depend on it.
- Do not infer topic category from `keyword_topic`.

App placement:

- Topic categories belong in Dataset intro/About, directly above descriptive
  keywords.
- Typed dates belong in Dataset intro/Citation. The common publication date
  remains prominent; additional creation/revision dates use a repeatable editor.

### Slice 1D: explicit metadata responsibilities

Add `metadata_responsibility_tbl(dataset_id, person_id, organisation_id,
role_code, is_primary, audit...)` and manage it using the existing people and
organisation selectors. Metadata creator, maintainer, and point of contact are
distinct responsibilities. Existing review records remain revision history and
are not rewritten as metadata authorship.

## Wave 2 — distribution, quality, spatial resolution, rights, and time

### Distribution

- Add `dataset_online_resource_tbl` with URL, name, description, protocol,
  function code, media type, and order.
- Add structured format records with specification title, version, media type,
  and optional format URI.
- Backfill the current distribution URL as the primary download/information
  resource without deleting the compatibility field.

### Data quality

- Add `dataset_quality_result_tbl` with resource/attribute scope, ISO 19157
  measure identifier/type, evaluation method/date, result type, value, unit,
  conformance specification, explanation, and pass flag.
- Provide templates for completeness, logical consistency, positional,
  thematic, and temporal quality, but keep records optional unless a resource
  profile makes them conditional.
- Never promote scan success, CRS detection, or profile counts to ISO quality
  results without an explicit measure and method.

### Scale and extent

- Add `equivalent_scale_denominator` as a positive integer.
- Replace the single temporal start/end UI with repeatable extents only after a
  compatibility view is available for existing status and reports.
- Keep dataset WGS84 bounds derived from `dataset_spatial_summary_tbl`.

### Rights and embargo

- Move authoritative embargo start/end, release statement, and dissemination
  constraint into `dataset_license_tbl`.
- Dual-read/dual-write the current `entry_tbl` embargo fields during migration.
- Keep existing licence and distribution modules separate in the UI, but use
  one repository-level rights model.

## Wave 3 — specialist completeness

Implementation status: storage/UI completed in crosswalk v3.0.0; representative
serializer and document validation completed under the v5.0.0 local acceptance
scope. Receiving-catalogue compatibility remains deferred as GAP-020.

- Add vertical extents only for resources where they are meaningful.
- Add vocabulary/thesaurus citations and use them for keyword groups.
- Generate ISO 19110 feature catalogues from `dataset_attribute_tbl`; coverage
  sample dimensions remain in ISO 19115 content information.
- Normalize contact addresses and organisational hierarchy without breaking
  existing free-text addresses.

## Migration pattern for every slice

1. Add idempotent `ensure_*` tables/columns and indexes in `R/db.R`.
2. Backfill only deterministic values; retain original fields.
3. Add repository read/write functions and transaction tests.
4. Add UI, hydration, validation, and completion-status behavior.
5. Add tables to dataset transfer/import and verify round trips.
6. Add or update crosswalk mappings in a new semantic version; do not edit a
   previously adopted crosswalk version.
7. Add serializer fixtures and ISO validation.
8. Remove compatibility reads only in a later major migration after real pond
   data have been audited.

## Definition of done

A gap is closed only when:

- new and existing ponds migrate idempotently;
- create, update, reload, and delete behavior is tested;
- inactive/read-only user protections still apply;
- dataset transfer round-trips the new records;
- completion status uses the field conditionally and explains failures;
- the crosswalk points to real database sources;
- representative ISO XML validates against XSD and Schematron;
- legacy data remain readable with no destructive migration.

## First implementation increment

Begin with Wave 0 and Slice 1A. The serializer guardrails make mapping errors 
visible, while normalized lineage directly addresses the highest-friction user
workflow. Slice 1B can proceed immediately afterward because its additive
columns are low risk and unblock scope-aware validation for all later work.
