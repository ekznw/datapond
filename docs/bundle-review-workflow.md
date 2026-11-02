# Metadata review exchange

Implemented 2026-10-21. Working transfer format **5** adds an explicit metadata
review request/return cycle and carries previous import provenance forward.
Older ordinary bundles remain importable; older DataPond versions must be
updated before using format 5. ISO metadata packages remain separate.

## Workflow

1. In the master's **Pond links / Dataset transfer → Metadata review exchange**,
   select the dataset and save its metadata. Enter the colleague responsible and
   choose **Create metadata review request**. Download the ZIP or use the saved
   path. One active request is allowed per dataset.
2. Send the ZIP through your SharePoint review-outgoing folder. It contains
   metadata only. The colleague uses **Import dataset** in their own local pond,
   which must not already contain that dataset UUID. For repeated assignments,
   use a fresh review pond; updating an existing review workspace is not part of
   this first version.
3. The colleague selects their active application user, edits metadata, records
   the reason/status in **Metadata review**, and saves. They then choose
   **Create metadata review return** and upload the ZIP to your review inbox.
4. In the master, choose the returned ZIP and **Inspect return**. The report
   compares it with the issued baseline, shows entry fields and repeatable rows
   added/removed/changed, and checks whether the master has changed meanwhile.
   Local database-ID reassignment is removed from the human comparison.
5. An active **pond administrator** records an acceptance decision and chooses
   **Accept reviewed metadata into master**. The same dataset record and UUID
   are updated. The action repeats the checks under a database write lock,
   preserves the prior metadata, appends the colleague's new review entries and
   a curator acceptance entry, and marks the request accepted.
6. Publish the accepted master through **Flow → Publish pond**. Return uploads
   by themselves never update the master or the published catalogue.

If the master changed, acceptance stops. Review the competing work, cancel the
old request with a reason and issue a fresh request. There is no automatic merge
or override of a stale baseline. Accepted/cancelled returns cannot be replayed.

## Scope and preservation

This first version supports dataset descriptive metadata and editable repeatable
records such as citation dates, topics, temporal extents, licensing, lineage and
quality. Existing inventory IDs remain stable and links from lineage/quality
are remapped to the correct master records. Referencing existing licence
choices is supported. Existing shared people/organisations and shared licence
or vocabulary definitions cannot be edited through a return; resolve those
changes centrally first. New referenced people/organisations can be imported.

Data files, supporting-document records, file inventories, attribute inventories
and extracted file/spatial summaries cannot be changed through acceptance.
Local file edits are not included in the returned ZIP. The metadata-only working
folder therefore does not contain the actual data or supporting files. Avoid
scanning this empty working folder: changed inventory/document records will
block acceptance. Use the published read-only data separately when needed for
reference. Dataset identity, folder placement and ownership of the local record
remain with the master; this is not a lifecycle release action.

Existing metadata review entries must be present and unchanged. They retain
their original master row IDs. Only new colleague review entries are appended,
followed by a curator entry marked `reviewed`; imported `approved` labels do not
bypass the curator's decision or certify ISO conformance. Format 5 also carries
the earlier transfer history, and the provenance panel displays all recorded
transfers. Older bundles cannot supply history that was never packaged.

## Baselines, checks and rollback

- Request records live in `dataset_review_exchange_tbl` (local workflow metadata,
  not an ISO obligation). Request, return and dataset UUIDs bind the exchange.
- The private baseline lives under `.metadata_pond/reviews/<review-id>/baseline.rds`.
  A SHA-256 fingerprint includes the saved metadata and its referenced parties.
  Changes to relevant shared records can therefore also invalidate a request.
- A file manifest fingerprint includes SHA-256 content hashes and relative paths
  of files in the master dataset folder. Ordinary root-level DataPond ZIPs are
  excluded as transport artefacts. Symbolic links are refused. Hashing a large
  dataset can take time even though its review ZIP contains no payload files.
- Inspection binds acceptance to the exact uploaded ZIP hash. Acceptance works
  from a private copy and repeats the master checks under the database lock.
- Before applying changes, acceptance writes a dataset-specific SQLite snapshot
  named `before-accept-<uuid>.sqlite` beside the baseline. Successful snapshots
  are retained; the curator review entry records their paths. They are recovery
  evidence, not a one-click restore or an independent full-pond backup.
- Metadata, history and request status commit in one transaction. Any failure
  rolls them back, leaving the request pending. Files are never overwritten.
  Keep file editors closed: SQLite cannot lock external dataset files. File
  hashes are checked again immediately before commit.

Requests reserve a baseline; they do not prevent someone editing the master.
Assignee names and active-user selections support a trusted internal workflow;
these are not authenticated signatures or centrally enforced SharePoint locks.
No online reviewer assignment, notification or cloud upload is automatic.

**Back up the curator's review directory as well as the pond.** Publish pond
publishes the catalogue and optionally dataset folders; it does not include the
private `.metadata_pond/reviews` baseline/recovery directory. Losing that local
baseline prevents accepting an outstanding return. SharePoint should have an
accepted publication, outgoing requests and an incoming review area; several
people should not upload competing live SQLite databases to the same path.

## Verification

`tests/testthat/test-review-exchange.R` covers request/import/return/accept,
stale metadata/files, repeated acceptance, cancellation, modified history,
archive identity/payload checks, hash-bound approval, transactional failure,
linked lineage/quality/inventory preservation and Shiny inspect/accept controls.
Existing ordinary transfer tests continue to exercise full-file transfer.

Use temporary fixtures for testing. No operational pond is migrated or edited
as part of development tests. File-bearing returns, partial field acceptance,
three-way merging and concurrent review branches remain future milestones.
