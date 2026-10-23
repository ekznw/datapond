# Legacy data and UI seeding cleanup — 2026-10-21

Follow-up: the [startup migration patch](startup-migration-v1-2026-10-21.md) now
isolates the older record conversions, retains them for colleagues' upgrades,
and prevents repeated backfills. That report also explains the 6.55 MB migration
archive and records tests against eight copied catalogues.

The active pond is `/mnt/pjg_dta/bspi_data/bspi_xdrive`. This audit also covers
`/home/paulg/dta_scp`, the other pond in the earlier rights audit. Together they
contain **82 dataset records**. Both catalogues were read with SQLite `mode=ro`,
`query_only`, and read transactions. Database and WAL hashes were unchanged.
Neither pond was opened through application initialisation or modified.

The [machine-readable evidence](legacy-data-audit-2026-10-21.json) records populated
legacy fields by dataset ID, migration markers, table counts, rights overlap flags,
foreign-key checks, and migration artefact paths/sizes. The repeatable
[audit script](../scripts/audit_legacy_data.py) does not execute migration scripts.

## What is still needed

| Finding | dta_scp | BSPI xdrive | Cleanup implication |
| --- | ---: | ---: | --- |
| Dataset records | 16 | 66 | Preserve identifiers and relationships |
| Foreign-key violations | 0 | 0 | No broken references found |
| Lineage parent records | 4 | 66 | Keep historical snapshots |
| Pending lineage marker | 4 | 0 | dta_scp still needs the newer migration |
| Structured lineage sources / steps | 1 / 0 | 67 / 68 | Structured detail already exists alongside old text |
| Legacy software descriptions | Column absent | 64 | Export still uses these descriptions |
| Legacy EPSG values | Column absent | 6 | Check against structured spatial metadata before retiring |
| Older access / use notes | 0 / 1 | 64 / 64 | Some wording is distinct and must be preserved |
| Structured use equals older access + use | 0 | 59 | Review duplicate terms against source evidence |
| Structured licensing rows absent | 0 | 5 | Populate from evidence, not guessed defaults |
| Migration artefacts | 0 | 40 | Preserve migration provenance and recovery material |

dta_scp has no `legacy_lineage_migrated_at` column yet. Its four parent rows
(dataset IDs 1, 2, 3, 9) therefore still need the current marker-based migration.
It already has one provisional source from an earlier backfill. Removing migration
code merely because BSPI has completed it would strand this pond and older imports.

BSPI has 66 provisional sources and 66 provisional process steps originating from
legacy backfill. The counts do not prove that someone has reviewed their meaning.
Original flat lineage text remains an audit trail, protected by the migration
marker so deleted structured detail does not reappear.

The 59 repeated rights concatenations match the
[earlier rights audit](pond-rights-audit-2026-10-21.md). BSPI dataset 1 contains
additional older use guidance; dta_scp dataset 16 has distinct scale/fitness and
redistribution notes. Do not delete legacy rights columns or remove arbitrary
substrings. Licensing disagreements require source review before consolidation.

## Workbook and startup findings

Three different resources had become conflated:

1. `inst/extdata/metadata_schema.xlsx` is the original field dictionary. Before
   cleanup, startup read 33 active entry-field definitions, although only Citation
   and About used the generic UI generator. These sections had 15 rows, with topic
   keywords and two temporal dates then suppressed in server code.
2. `inst/extdata/schema_grouped.rds` duplicated the workbook-derived schema. It
   was a test snapshot, not a startup cache. Keeping it required synchronising two
   definitions and a binary file.
3. The XLS archive form and DOCX template under `word_metadata/eg_word_entries/`
   are reference examples. They do not seed the UI or database at startup.

Keywords, organisations and people come from separate packaged CSV vocabularies.
The previous party seeding ran on each startup/pond switch, updating matched party
rows and reactivating seeded users while resetting their administrator flags from
the CSV. Idempotent row counts did not protect local edits or permissions.

## Code changes made

- Defined the 12 current generated fields in
  [`dataset_overview_schema()`](../R/dataset_overview_schema.R), shared by UI
  generation, hydration, saving and validation. Each call returns fresh tables.
  The initial cleanup introduced `ui_schema.csv` as an intermediate step; the
  follow-up retires that file and all runtime CSV/Excel schema loading.
- Removed the `schema_path` launcher argument and option, the schema loader and
  its workbook-specific keyword adapter. The original workbook remains useful
  for historical field and crosswalk documentation. `readxl` is now an optional
  test dependency, not an application startup requirement.
- Removed the redundant RDS snapshot, its write path, the unused
  `sync_vocab_to_db()` wrapper, and the unused `citation_key` fallback argument
  to `create_dataset_folder_structure()`. All current folder callers use
  `folder_key` or the corresponding positional argument.
- Removed redundant server-side filtering of obsolete generated controls.
- Changed party startup seeding to initialise an empty catalogue once, atomically
  with a pond-scoped marker in `app_config_tbl`. Existing catalogues are adopted
  without reseeding. Local edits, deletions and user permissions survive startup.
  Explicit party sync helpers remain for deliberate imports and tests.
- Updated package documentation and added regressions for party edits, deleted
  parties, permission changes, rollback and workbook-independent default loading.

Database migrations, legacy import/export readers, and the old startup-pointer
recovery path remain. They have current compatibility uses; none was removed
solely because its name contains “legacy”. No database columns were dropped.

## Recommended data cleanup

1. Keep the BSPI migration evidence. The 40 files total **6,551,727 bytes**
   (6.55 MB), including six pre-intake database backups. This is small; deleting it
   offers little benefit. If tidying is needed, archive the scripts/manifests and
   backups together with checksums and a restoration note, preserving dataset UUIDs
   and source-path mappings. They are historical execution records, not runtime
   application code.
2. Upgrade dta_scp with the current package after a fresh backup. The copy rehearsal
   below confirms that repeat initialisation creates no extra lineage rows and
   preserves its older source description.
3. Review the 59 rights concatenations and five missing BSPI licensing rows using
   the per-record rights report. Preserve every distinct condition and record the
   provenance of any replacement. Only then make one field authoritative and add
   a one-time marker preventing old mirrors from reviving cleared values.
4. Retire a legacy column only after all supported ponds/import formats have a
   versioned upgrade path and its exporter/transfer dependencies are removed.
   In particular, software descriptions, temporal mirrors, and original lineage
   snapshots are not safe candidates for blanket deletion.

Package build archives, `dist/`, `*.Rcheck/`, and `tmp/` are separate development
artefacts, not pond migration data. Remove those by an explicit retention policy;
this cleanup preserves the existing working tree and its prior work.

## Validation

Sixteen focused test files passed, covering schema loading, startup/recovery,
pond creation, saving/completion, identities, transfers, keyword migration,
party management, lineage, crosswalk coverage and maintenance export. New seed
regressions verify preserved edits and user permissions, no resurrection after
deletion, and full rollback when person seeding fails. This was a targeted test
run, not a full package check or browser interaction test.

The schema-file retirement follow-up passed 13 focused test files covering the
R definitions, rendered controls, session isolation, startup/exit/recovery,
population and completion workflows, transfers, and crosswalk coverage. A
before/after comparison confirmed that all 12 definitions, including labels,
descriptions, types and obligations, match the former CSV. Historical crosswalk
manifests retain their original source-model references as versioned provenance.

SQLite backups of both real catalogues were migrated twice in temporary folders.
All 82 dataset identities/titles and the audited older entry/lineage fields were
preserved. Organisation, person and user rows were unchanged. After the first
upgrade, a second initialisation produced identical lineage tables. dta_scp kept
its one existing source without duplication; BSPI retained 67 sources and 68
steps. Both copies had no pending lineage markers or foreign-key violations.
See the [rehearsal results](legacy-migration-rehearsal-2026-10-21.json).

To repeat the audit, choose a new output filename outside both ponds:

```bash
python3 scripts/audit_legacy_data.py --output /tmp/legacy-data-audit.json \
  /home/paulg/dta_scp /mnt/pjg_dta/bspi_data/bspi_xdrive
```
