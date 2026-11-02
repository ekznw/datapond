# DataPond metadata simplification assessment

Assessed: 2026-10-08  
Baseline: ISO crosswalk v1.0.0 versus the working repository at v3.1.0  
Result: capture policy v3.2.3 and a shared entry / assessment population choice

For the purpose, fields and relationships of each database object, see the
[table dictionary](table-dictionary.md).

The ISO gap work increased what DataPond can describe, but also increased the
amount of metadata presented to an editor. Keep the richer storage model and
reduce duplicate entry: each fact needs one editable source, compatibility
fields should be retained as mirrors or historical evidence, and detailed
metadata should be conditional. A local inventory record and a record ready
for external metadata exchange need different completion targets.

The original assessment reviewed the repository and migration model, without
operational pond records. Those model counts are historical. A subsequent
[read-only rights audit of dta_scp and BSPI](pond-rights-audit-2026-10-21.md)
examines licensing and access/use notes in 82 saved records; neither pond was
modified. Existing uncommitted changes were treated as the current implementation.

## Impact of the ISO gap plan

The crosswalk manifests grow from **23 to 39 database objects**: 15 additional
tables and one temporal compatibility view. Mappings grow from **129 to 157**.
The additional tables support repeatable lineage, dates, topics, metadata
responsibilities, titles, distribution, quality, extents, vocabularies, feature
catalogues and addresses. These counts are reproducible from the manifests'
`source_policy.table_defaults` and the crosswalks' `mappings` lists.

The plan deliberately retained older columns and backfilled deterministic
values. That protects older reports and transfers, but the cost is multiple
representations of the same fact. Repeatable editors also expose identifiers,
roles, precision, ordering and audit context that most inventory records do
not need users to enter.

The working app already merges theme/topic keywords, uses one repeatable
temporal editor, makes the legacy URL read-only, supplies metadata identity
defaults, and hides some specialist fields. Those improvements predate this
assessment. At the start of the review, friction included six editable flat lineage fields
alongside structured sources/steps and repeated publication-date checks. The
workbook also marks resource progress mandatory under Data information/About,
although the current intro form does not render that section.

The plan's Waves 1–3 are implemented as storage and UI slices. Crosswalk v5.0.0
accounts for all 164 mapping dispositions and closes GAP-018 for the agreed
local-validation scope: pinned XSD/Schematron, paired catalogues, code-list and
bounded semantic checks, actionable reports and transactional packages.
The existing Measure type selector records
the specific test; its broad category is derived, avoiding a second question.
Older broad quality records remain saveable but need a precise test for export. GAP-019's supplementary-constraint export omission is resolved; distinct
notes are retained and whitespace-equivalent duplicates are removed from XML.
Receiving-catalogue compatibility is explicitly not assessed and deferred as
GAP-020 until a target is selected. Exhaustive combinations and factual accuracy
are not claimed. See the
[GAP-018 local closure](iso-metadata-gap-infill-plan.md#gap-018-local-closure--crosswalk-v500).
The official [ISO/TC 211 implementation package](https://schemas.isotc211.org/19115/-1/)
provides XSDs, Schematron rules and example documents for that validation.
“Mapping ready”, “record complete”, and “ISO validated” must remain separate.

## Field decisions

| Field family | Authoritative value and action | Population consequence |
| --- | --- | --- |
| Publication date | `dataset_citation_date_tbl`; `entry_tbl.citation_date` is the primary-publication mirror. Keep the prominent Citation input. | Assess the date once; add creation/revision dates only when known. |
| Theme and former topic keywords | `entry_tbl.keyword_object`; merge `keyword_topic` as a legacy alias. | One theme selector. ISO topic categories remain separate. |
| Temporal coverage | `dataset_temporal_extent_tbl`; legacy start/end columns are projections of the first period. | One period editor; multiple periods and uncertain endpoints are optional detail. |
| Distribution URL | `dataset_online_resource_tbl`; primary URL mirrors into `entry_tbl`. | One editable URL list; saved primary URL is a collapsed read-only summary. |
| Embargo | `dataset_license_tbl`; legacy entry embargo values are compatibility mirrors. | Keep one editor. Start, release audience and release statement add detail, not extra versions of the end date. |
| Lineage source text | `dataset_lineage_source_tbl`, with supporting-document citations where appropriate. | Migrate old `source_description` and `source_datasets_text` once; remove their controls. |
| Lineage process text | `dataset_lineage_process_step_tbl`. | Migrate old process description, tools, scripts and output description once; remove their controls. Existing notes join the single editable narrative. |
| General software text | `entry_tbl.software_desc` overlaps process references when it describes production, but may describe software needed to use data. | Retain and audit meaning before migration; do not discard or automatically merge it. |
| Resource progress | `entry_tbl.data_complete_status` describes progress of the resource. | Keep available but optional in the runtime schema; exclude from required descriptive completion. |
| Resource and metadata identifiers | `uuid` identifies the resource; `metadata_uuid` identifies the metadata record. | Keep both, generated and normally not manually entered. |
| People and roles | Authors, resource contacts, metadata contacts and reviewers can share person records but express different responsibilities. | Reuse selectors and saved people; do not infer authorship from review history. A future reuse action should require an explicit choice of role. |
| Distributor organisation | Distribution organisation and organisation-role links can refer to the same party. | Reuse organisation identity; audit conflicting role links before consolidating editors. |
| Abstract and purpose | Abstract describes the data; purpose explains why they were created. | Abstract is enough for rough inventory; purpose remains part of full population. |
| Scale and resolution | Representative denominator, capture/use scale range and raster cell size are distinct. | Keep meaningful values; never manufacture a denominator from a range or pixel size. |
| Bounds and CRS | Dataset spatial summary plus file/manual spatial metadata are the authoritative sources. | Derive from scans where possible. Avoid retyping legacy bbox/EPSG text. |
| Formats | Scanned file types describe inventory; distribution specifications can include edition/version and media type. | Explicit specifications are optional for inventory. The current ISO serializer does not automatically export detected file types. |
| Attributes and feature catalogue | Attribute descriptions and units feed the generated catalogue. | Describe a field once; catalogue identity and citation are separate optional details. |
| Contacts and addresses | Structured addresses support detailed contact export; older free text remains evidence. | Maintain shared people/organisations once. Address normalization is specialist work. |
| Quality and lineage | Production history and measured fitness are different. | Quality results need evidence and a method; scan success is not a quality assertion. |
| Licensing and extra restrictions | Structured licence terms plus genuinely additional operational access/use notes. | Avoid asking users to repeat licence text. Do not discard additional restrictions merely because a licence exists. |

Canonical storage does not justify deleting compatibility columns immediately.
Migrations, old reports, transfer and some completion helpers still read them.
The current workbook is retained; the runtime loader overrides the progress
field's obligation. The crosswalk capture policy records the effective app
model and compatibility relationships.

## Prickly Porcupine and Armoured Pangolin

| Area | Prickly Porcupine (essential inventory) | Armoured Pangolin (full metadata) |
| --- | --- | --- |
| Description | Title, publication date, short abstract, theme keywords | Also purpose and citation version |
| Responsibility | A resource point of contact and an added organisation with a usable code | Also authors and explicit metadata responsibilities |
| Provenance | Short source/production summary | Summary plus source/process detail or authoritative documentation |
| Sharing | Existing sharing assessment and embargo checks remain | Also detailed releasability where applicable |
| Discovery | Add location/time when useful; retain scan-derived values | Scope-aware topics, geographic discovery, applicable temporal extent |
| Specialist detail | Optional | Quality, vertical extent, thesaurus, catalogue and contact detail when relevant |
| Completion meaning | Usable local inventory under DataPond's chosen policy | DataPond metadata assessment; not ISO certification |

The rough target currently keeps the existing structured sharing assessment
intact. It therefore does not make a record publishable merely because it has
a title and summary. Unknown rights should remain unresolved; they must not
be defaulted to permission to distribute. A future shorter rights form needs
an explicit “not assessed” state distinct from permission.

Full population should mean all *applicable* metadata, not filling every
possible control. Optional specialist sections can remain unstarted. If a
user explicitly chooses document-backed lineage, a missing document is still
an inconsistency in either target.

## Changes implemented in this review

- Dataset status now offers **Prickly Porcupine** and **Armoured Pangolin**
  assessments. Prickly Porcupine is the session default; other completion consumers
  retain the existing full default. Switching assessments does not write data.
  Prickly Porcupine is the lighter inventory assessment (internal code `rough`).
  Armoured Pangolin is the full metadata assessment (internal code `full`).
- Rough entry checks assess title, abstract and theme; full entry checks also
  assess purpose/version. Both modes now require a publication date and added
  organisation with a usable naming code. The typed citation-date section owns the full date
  check. The runtime schema now labels resource progress optional, preventing
  future schema-generated forms from treating it as required description.
- A short lineage summary satisfies rough completion. Full completion retains
  the existing detail requirement. Both retain document-authority checks.
- Detailed source/step editors are collapsed. Six old lineage inputs are
  removed in the lineage follow-up below; saving a summary retrieves their previous values from storage,
  preserving them independently of hidden browser controls.
- The primary-URL mirror is collapsed, and format help no longer promises an
  unimplemented detected-format export fallback.
- Crosswalk **v3.2.0** adds a machine-readable capture policy and corrects the
  software-reference guidance. Earlier crosswalk versions remain unchanged.
  At that version, GAP-018 tracked partial serializers and GAP-019 tracked
  supplementary access notes omitted by legacy-only constraint mapping.
  GAP-019 was resolved for export in v3.2.5; GAP-018 was closed for the
  agreed local-validation scope in v5.0.0.

## First implementation scope item — implemented

The clickable animal hex icons under **Metadata population** in Configuration control entry
visibility, draft saving, folder creation and Dataset status. It defaults to
Prickly Porcupine for the session; it is a working preference, not a stored
completeness flag. Changing it does not change dataset identity or write data.

- **Prickly Porcupine:** title, publication date, version, abstract, theme keywords and
  an added organisation with a usable code are required to create a folder and
  save a draft. Version starts at `0.1.0` for new drafts. Purpose, authors and ISO topics can be added through
  **Show additional metadata fields**. Contacts, lineage, files and sharing
  editors remain available.
- **Armoured Pangolin:** the full intro fields are shown. Saving also requires
  purpose and the existing scope-dependent ISO
  topics. The status panel shows the additional missing requirements.
- Both entry points share validation. Version is mandatory in both modes and needs
  valid major.minor.patch syntax; temporal ranges, scale and sharing/access consistency checks
  remain active. The active editor and metadata review history are retained.
- Hiding fields leaves their input bindings and module state in place. Saves
  include detailed fields and repeatable rows in either mode; switching does
  not rebuild forms or clear unsaved inputs. Optional specialist sections keep
  their existing disclosure controls.
- Publication date starts blank. Loading an undated draft explicitly clears
  a previous date rather than retaining it or substituting today's date.
- Storage and ISO mappings are unchanged. Capture policy **v3.2.3** records the
  implemented behavior; earlier crosswalk versions remain unchanged.

The naming follow-up (2026-10-15) makes date and organisation visible in
Porcupine mode. Both save actions reject an absent date, an invalid date, a
missing organisation, or an unusable first organisation code / theme keyword.
Selecting an organisation without clicking **Add** is insufficient. For example,
`biota`, publication date `2024-02-03`, organisation code `EXAMPLE` and title
`Minimal observations` produce folder key `biota_2024_example` and citation key
`example_202402_biota_mo_<random>`. The UUID is appended to the folder key.
Existing physical folders and citation keys remain stable on later edits; the
proposed folder key can update for a later explicit rename.
No date or organisation is invented to replace missing metadata.

A saved draft need not yet satisfy the Prickly Porcupine completion assessment:
contacts, lineage and sharing can still be missing. Unknown rights are not
changed to permission to distribute. Lifecycle readiness and pond overview
continue using full completion; separating the three readiness concepts is
the next scope item. Neither mode certifies ISO conformance.

## Lineage simplification — implemented 2026-10-19

The old flat text fields predate the repeatable source/process tables; they are
DataPond compatibility storage, not additional ISO obligations. The form now
contains one lineage narrative, structured sources/processes under optional
details, and supporting-document links. Both the legacy section and separate
additional-notes input are removed.

On opening an existing pond, a transactional migration preserves unmatched flat
text in provisional source/process rows, without splitting text or overwriting
structured records. Existing backfill rows are recognised. Additional notes
join the narrative once. Originals remain hidden in the old columns for
recovery; no compatibility columns are dropped. A per-record timestamp travels
with transfers and prevents later deletions or narrative edits from being
reversed by another migration. Deletions made before this timestamp existed
cannot be distinguished from never-migrated text; provisional rows need review
in that case. Completion now uses canonical details, not archived text.

Crosswalk v3.2.3 documents this policy. Export uses the consolidated narrative,
structured sources/processes and document citations; distinct output descriptions
are included in process descriptions. GAP-018 is now resolved for local
validation in v5.0.0; target-catalogue compatibility is deferred as GAP-020.

## Remaining implementation scope

| Priority | Work | Acceptance evidence | Rough effort |
| --- | --- | --- | --- |
| 1 | Separate inventory readiness, lifecycle readiness and export readiness throughout overview and workflow | A rough-complete record can remain export-incomplete without contradictory labels | 1–2 days |
| 1 | Audit real pond compatibility values before removing columns or further editors | Per-dataset conflict report for dates, URL, time, embargo, lineage, software and distributor roles; no automatic overwrite of disagreements | 1–3 days plus curator review |
| 2 | Implemented in v3.2.5: MCO-013 additive operational constraints and MCO-017 legacy licence fallback | Passing serializer fixtures preserve licence plus extra restrictions and remove whitespace-equivalent duplicates; [real-pond audit](pond-rights-audit-2026-10-21.md) completed; curator decisions and data cleanup remain separate | Complete for export |
| 2 | Add deliberate reuse actions for parties and document citations, infer date precision, and warn about unadded editor rows | No duplicate typing and no silent loss when saving a partially filled repeatable editor | 2–3 days |
| 2 | Review mirror precedence and deletion behavior | Structured unknown temporal endpoints cannot revive stale legacy dates; lineage deletion is now covered by the migration marker and transfer tests | 1–2 days |
| Complete | Serializer coverage and Wave 0 local validation (v5.0.0) | Vector, raster and nonspatial fixtures; pinned XSD/Schematron results, separate date-only policy, code-list/semantic checks and transactional package tests | GAP-018 closed locally |
| Deferred | Receiving-catalogue compatibility | Import representative packages against a selected catalogue/version/profile | GAP-020; no target selected |

These are engineering estimates, not measured delivery commitments. Priorities
1 and 2 reduce population effort; adding more specialist fields should wait
until this workflow is consistent. A later major migration can remove audited
compatibility storage only after transfer and historical-report behavior is
verified.

## Verification

The first scope item adds shared validation and full Shiny-server integration
coverage for both **Save Dataset** and **Create Folder & Save Dataset** in isolated
temporary ponds. Checks cover minimal draft persistence, folder creation, full-mode
rejection, status switching, detailed scalar / typed-date / topic / alternate-title
retention, reload, unchanged identity and explicit blank-date hydration. UI
structure checks confirm a single selector and controls hidden in place.


Regression coverage exercises rough/full differences, canonical publication
assessment with a deliberately stale mirror, preservation of legacy lineage
when its summary changes, removal of legacy controls, document-authority checks and
crosswalk policy references against the effective database model. Existing
ISO, lineage, migration and dataset-transfer tests remain part of the check.
These tests establish app behavior and data retention, not ISO conformance.

Validation result: the 12-file workflow, population, ISO, lineage and transfer
regression run passed. Additional active-user, keyword-merge, purpose-control
and package-smoke checks passed; the latter found and prompted regeneration of
the bundled schema snapshot to match the existing optional resource-progress
policy. `git diff --check` passed. Browser automation was unavailable in this
session, so live browser interaction was not verified.

Naming follow-up verification: both save routes reject incomplete naming
metadata and generate the expected folder/citation components when date and
organisation have been supplied. Tests also cover invalid dates, unusable
codes, the organisation Add action, and stable physical folder / citation
identity when the publication date is edited. Related crosswalk, organisation,
ISO profile, population assessment, spatial status and identity checks passed.

Lineage simplification verification (2026-10-19): structured-lineage, population-profile,
document-authority, dataset-transfer, crosswalk and full entry-integration tests passed.
Checks cover transactional rollback, preservation alongside existing structured rows,
notes consolidation, XML output, and deletion persistence across reopening and transfer.
All fixtures used temporary ponds; no operational pond was opened or migrated.
