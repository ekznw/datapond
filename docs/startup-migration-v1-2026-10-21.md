# Startup migration patch v1

The patch is retained in the package and runs when `init_db()` opens a pond,
including application startup and pond switching. It upgrades older records to
the current DataPond tables. Install/use the updated package to enable it; the
operational ponds were not opened or modified during development and testing.

## Scope and behaviour

Schema setup remains in the normal database modules. The historical data
conversions are isolated in [legacy_patch_v1.R](../R/legacy_patch_v1.R), with a
small [dispatcher and progress reader](../R/legacy_patch.R). Startup first
prepares the required tables and columns, then applies all pending components
under one SQLite savepoint. A conversion error rolls back the patch's data
changes and completion markers so the next startup can retry. Additive schema
preparation preceding the savepoint can remain and is safe to repeat.

| Older information | Current destination | Completion tracking |
| --- | --- | --- |
| Topic keywords and old topic vocabulary | Combined theme keywords/vocabulary | No old aliases remain after successful normalization |
| Publication date | Typed citation-date rows, retaining date precision | `entry_tbl.legacy_citation_v1_migrated_at` |
| Distribution URL, temporal bounds, embargo mirrors | Online-resource, temporal-extent and licensing rows | `entry_tbl.legacy_wave2_v1_migrated_at` |
| Organisation department/free-text address | Organisational unit and structured address row | `organisation_tbl.legacy_party_v1_migrated_at` |
| Older licensing selections | Missing structured licensing codes | `dataset_license_tbl.legacy_licensing_v1_migrated_at` |
| Flat source/process text and notes | Structured lineage and one narrative | Existing `dataset_lineage_tbl.legacy_lineage_migrated_at` |

Migration is recorded per record/component, not only per pond. Later-arriving
older records remain eligible. Portable imports run the patch after the related
tables have arrived, inside the import transaction. Current bundles retain their
completion markers, including when structured rows were deliberately deleted.

Ordinary section reads still handle unmarked older records during this transition.
They invoke the same guarded components; completed records skip the conversion
bodies. The temporal compatibility view respects completion, so a cleared
structured endpoint cannot fall back to an obsolete snapshot.

Existing structured values take precedence. Original dates, URLs, free-text
addresses, lineage snapshots and access/use wording remain stored. Keyword
aliases are folded into the current field, as in the existing keyword upgrade.
The patch does not split prose into guessed sources or parse an address into
guessed city/country components. Licensing inference retains the existing mapping
for missing codes, while preserving explicit populated choices, including an
explicit false attribution flag. New saves through the licensing editor are
marked current and are not subsequently reinterpreted as old selections.

The 59 overlapping rights notes still need substantive review. The patch neither
deletes their wording nor resolves the documented licensing disagreements.
Software descriptions and other still-supported stored extension fields remain
available. No source columns are dropped, and party/user seeds are not reapplied.

The new completion timestamps are internal bookkeeping. Review exchanges ignore
them when comparing metadata and protect entry markers from being replaced by a
review return. Existing substantive checks for edited shared parties remain.

## Tests and real-catalogue rehearsal

The final targeted run passed all 16 test files, covering startup/recovery,
schema resources, the new patch, record transfers, review exchanges, frozen
releases, organisation management, lineage, keywords and the affected ISO
persistence/export paths. No full package check or interactive browser test was
run for this patch.

[Regression tests](../tests/testthat/test-legacy-startup-patch.R) exercise:

- Automatic startup conversion, preservation of raw source information, and
  precedence of existing structured metadata.
- Full patch rollback on an injected late failure, followed by a successful retry.
- Repeat startup with converter functions replaced by failures, proving completed
  records do not call them.
- Deleted dates, resources, addresses and lineage staying deleted despite retained
  original text; the compatibility view also remains empty.
- Older records arriving after the first startup.
- Actual bundle imports without the new marker columns, and retransfer of upgraded
  records without reviving deliberately deleted values.
- Current licensing saves preserving explicit choices on the next startup.

The [rehearsal results](startup-migration-v1-rehearsal-2026-10-21.json) cover fresh
SQLite backup copies of both current catalogues and all six historical intake
backups: **eight database snapshots, 111 record instances**. These are overlapping
historical snapshots, not 111 distinct datasets. The live catalogues contain 82
records between them.

All eight copies upgraded successfully, had zero pending patch components and no
foreign-key violations afterward, and produced identical migration destination
tables on a second startup. Dataset identity and audited original source fields,
existing populated rights, people and users were preserved. Source database and
WAL hashes were unchanged. This validates the available versions; it does not
establish coverage of every possible colleague-specific schema variation.

Run the [copy-only rehearsal](../scripts/migrations/rehearse_legacy_v1.R) from the
repository root with a new output directory:

```bash
Rscript scripts/migrations/rehearse_legacy_v1.R /tmp/pond-upgrade-rehearsal \
  /path/to/first_pond /path/to/second_pond
```

It uses SQLite's backup API on read-only source connections, then upgrades only
the copies. Source dataset payloads are not copied or changed. The output contains
the copies and a JSON report. Database file paths can also be supplied directly.

## When it can be retired

Keep the patch enabled until colleagues have installed the updated package and
opened each pond they maintain. Collect a zero-pending status for each pond after
successful startup, including disconnected/archive ponds that are still in use.
The progress check itself is read-only:

```r
conn <- DBI::dbConnect(RSQLite::SQLite(), "/path/to/pond/metadata.db",
  flags = RSQLite::SQLITE_RO, synchronous = NULL)
datapond:::legacy_patch_v1_status(conn)
DBI::dbDisconnect(conn)
```

Retirement also requires a policy for old portable bundles: either reject formats
requiring this upgrade, or retain an explicit offline upgrader before accepting
them. Then remove the v1 conversion implementations and their startup/reader/import
invocations together. Keep current table definitions, completion columns, source
snapshots, review handling and tests that prevent old values reappearing. Do not
remove converters on a calendar date or merely because one pond reports complete.

Schema defaults, immutable identifier creation, integrity repairs and derived
spatial summaries still needed by current records remain normal application
maintenance; they are not all obsolete data-conversion code.

## Contents of the 6.55 MB migration archive

This is a collection of **40 files**, not a single compressed archive, in the
BSPI pond's `.metadata_pond` directory. The exact total is **6,551,727 bytes**
(6.55 decimal MB, about 6.25 MiB). A
[per-file inventory with SHA-256 hashes](migration-archive-inventory-2026-10-21.json)
records every file.

| Contents | Files | Bytes | Purpose |
| --- | ---: | ---: | --- |
| SQLite database backups in `intake_backups/` | 6 | 3,260,416 | Metadata checkpoints before stages of the October 16 intake |
| JSON manifests and verification evidence in `imports/` | 16 | 3,188,637 | Source/destination mappings, hashes, archive inventories, deduplication decisions and checks |
| Python/R scripts under `imports/temp2-20261016/` | 15 | 81,128 | Historical extraction, metadata population, projection checks, deduplication and application routines |
| Intake result summaries at `.metadata_pond/` | 3 | 21,546 | Final dataset IDs, UUIDs, paths, citation keys and titles |
| **Total** | **40** | **6,551,727** | |

The six backups are before soils/earthworms (0 records), the ARC soils update
(2), marine CBA (2), remaining datasets (3), storage cleanup (11), and temp2 intake
(11). Each passed SQLite's `quick_check`. They are metadata-only checkpoints, not
current full backups of the 66-record pond and not copies of the raster/vector
payloads. Restoring one wholesale would revert later catalogue additions.

The largest evidence files are the temp2 manifest (1.01 MB), leaf audit (0.61 MB),
extracted metadata (0.52 MB), and storage-cleanup manifest (0.46 MB). They explain
what was imported, retained, moved or removed, and why. Several manifests record
source removal after verification, making this evidence useful for provenance.
Keep these files together. They are not executed by the new startup patch.
