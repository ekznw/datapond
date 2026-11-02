# Frozen dataset lifecycle

Implemented 2026-10-21. Use **Dataset lifecycle → Frozen releases**. Freezing
creates a fixed version in the local archive; publishing and distribution
approval are separate steps. Edition controls record local publication/withdrawal
history but do not perform remote distribution.

## Working, frozen and restored

1. Save the dataset's metadata, including title, dataset UUID, version and
   citation. Select an active application user. Finish or cancel any active
   metadata review request before changing its lifecycle.
   Select **Essential** or **Full** in Configuration and complete every required
   section shown by Dataset status for that mode. A named reviewer must save the
   latest metadata review as **Approved**. Approval is bound to the saved metadata;
   subsequent edits require fresh approval, and preparing a PDF does not approve it.
   Older approval history without this evidence must be approved again before a
   new freeze. Existing frozen releases are preserved.
2. Under **Dataset status → Human-readable metadata**, choose **Save PDF to dataset
   folder & download**. This saves `metadata.pdf` and `citation.txt` at the working
   dataset root and downloads the same PDF. Open it and inspect the dataset folder.
   Document preparation is an explicit step; freezing and archiving never render
   or refresh these documents. Repeat this step after changes to document content.
3. Preview the saved dataset. The preview checks completeness in the selected mode
   and current reviewer approval, and shows the mode and reviewer. Switching modes
   clears the preview. Both checks are repeated when freezing. It also checks that the prepared documents are
   current and unchanged, then hashes every file in its root,
   including hidden files, development documents and any old transport bundles.
   Remove unwanted temporary files yourself, then preview again. Symbolic links,
   unsafe paths and names differing only by case are refused.
4. Confirm that you opened the PDF and reviewed the previewed dataset folder, and
   enter release notes. **Freeze version; keep working copy** leaves development
   in place. **Freeze version and close development** retires that working folder
   after the ZIP and metadata snapshot have been verified. A frozen version
   cannot be overwritten: save a different version for the next release.
5. Select a release to verify or download its ZIP. **Close development using
   selected release** succeeds only if the current files and saved metadata
   still match that release.
6. **Restore selected release as draft** restores files and saved dataset
   metadata together into a new folder under `dev/`. The catalogue ID and dataset
   UUID stay the same. The release remains unchanged. Any current working draft
   is first preserved as a verified private ZIP and metadata checkpoint.
7. **Roll back to selected working-draft checkpoint** restores that previous
   draft and creates another checkpoint of the draft it replaces. Save a new
   version before freezing an edited or restored draft again.

Pond overview does not report metadata readiness for frozen releases. It identifies
an unchanged frozen version (including a retained working copy) as **Frozen release**.
Later working edits are shown as development; the frozen ZIP is never reassessed.
The release manifest records the assessment mode and approval used at freeze time.
The pond summary counts datasets with retained frozen releases and the number of
frozen versions independently of the current working folder. The selected dataset
summary and inventory show its current version and archived release versions,
including after a draft has been edited or restored.

Quarto and XeLaTeX are required for the explicit PDF preparation step, with
Python 3 used to verify the prepared files. Freezing
uses the prepared documents and requires Python 3, using only its standard
library. Set `DATAPOND_RELEASE_PYTHON` if Python is not on PATH.
Keep external writers such as QGIS and document editors closed during lifecycle
operations. File and metadata checks detect changes, but Data Pond cannot lock
other applications out of the filesystem. Preview and final verification read
the full dataset, so large releases take time and require staging space.

## Storage and distributable files

```text
pond/
  dev/<working-dataset>/
    metadata.pdf
    citation.txt
  .metadata_pond/documents/<dataset-id>/
    metadata.pdf
    citation.txt
    metadata.qmd
    ekznw-theme/...
    prepared.json
  arc/releases/<dataset-name>__v<version>__<release-id>/
    dataset.zip
    release.json
    files.json
    checksums.sha256
  .metadata_pond/releases/
    <release-id>/snapshot.sqlite
    <release-id>/source-files.json
    <release-id>/freeze-operation.json
    operations/<operation-id>/...
    locks/...
```

`dataset.zip` contains the original dataset under `dataset/`, plus a portable
`.datapond_transfer/dataset_metadata.sqlite` database, `metadata.pdf`,
`metadata.qmd`, `metadata.json`, `citation.txt`, `README.txt`, `LICENSE.txt` and the
portable EKZNW theme. Root development QMD/Rmd documents are acknowledged in the
README and PDF; their contents are preserved without execution. Their software
and package records remain in those documents. The PDF identifies the dataset
version and its document-generation time. It is preserved byte for byte from the
prepared working copy, together with its source and theme. Release UUID and
freeze time are recorded separately in the README and release metadata, since
they do not exist when the user reviews the PDF. Saved missing metadata is reported as
missing, not invented. Empty directories and operating-system permissions are
not preserved; file contents and relative paths are checked.

Choose an ISO validation policy if the release also needs a validated ISO
package. The default is no ISO export. A requested ISO export failure or missing,
changed or stale prepared documents stops the freeze, leaving the working dataset
in place. Release-specific README, licence summary, JSON sidecars and requested
ISO exports are still assembled when freezing. A local freeze does not
certify metadata completeness or permission to distribute the files.

`files.json` lists SHA-256 hashes and sizes for all ZIP members. The catalogue
also records hashes of the ZIP, manifest and private dataset metadata snapshot.
Keep the three sidecars with the ZIP when distributing a release outside the
application. The UI download currently downloads the ZIP alone.

The private snapshot contains dataset-scoped metadata and required shared
references. New frozen ZIPs also include a transfer database built from the same
captured metadata using the normal dataset bundling module. For in-house and unclassified editions it includes review
history and referenced contacts. Public transport metadata omits local review/import
history and application accounts; inspect the explicitly recorded public contacts
and selected documents when sharing a release.
No full catalogue or source-pond release-control tables are distributed.
Back up the catalogue, working datasets, `arc/`,
`.metadata_pond/documents/` and `.metadata_pond/releases/`
together. The ZIP supports importing into another pond; the private snapshot
and catalogue still support restoration and release history in the original pond.

### Import a frozen release into another pond

In the receiving pond, use **Portable dataset transfer → Import dataset** and
select the frozen `dataset.zip`. The existing bundle importer restores the saved
dataset metadata and files, remaps related records and local paths, and records
the source pond and release UUID in import provenance. The original dataset UUID,
version and folder name are retained. A duplicate dataset UUID is refused rather
than overwriting a record. The imported record is the working state captured at
freeze time; it does not register a frozen release in the receiving pond.

The transfer database sits outside `dataset/`, so it is not added to a restored
working folder. Existing frozen ZIPs made before this integration remain valid
for the original restore workflow, but cannot be imported this way unless they
already contain transfer metadata. Existing archives are not rewritten.

Application operations do not overwrite frozen artifacts. Filesystem write
permissions still matter: checksums detect external modification; they are not
digital signatures or operating-system write protection. The editable catalogue
and its shared contacts may evolve independently of a frozen snapshot. Downloads
always use the frozen ZIP, not a fresh report from current catalogue values.

## Recovery and retention

Each operation has a journal, a per-dataset lock and an append-only catalogue
event. A failure before database commit reinstates the previous working folder.
After commit, recovery finishes cleanup only after rechecking the preserved
bytes. Changed retired files are retained for inspection instead of deleted.

Use **Recover interrupted lifecycle operation** after an interruption. Recovery
can register a verified frozen ZIP if the process stopped between its final
filesystem rename and catalogue commit. It can also reconcile a file move with
SQLite's committed or rolled-back transaction. It refuses to interrupt a live
process or a lock owned by another computer. An incomplete unpublished staging
folder may remain for inspection; it is never treated as a release.

Restoration preserves existing review/import history and remaps linked metadata
IDs. It refuses restoration when required shared people, organisations, licence
templates or vocabularies have changed, rather than overwriting shared records
used by other datasets. Resolve those differences before restoring. Checkpoints
and releases do not expire automatically. Superseded release ZIPs can be removed
explicitly as described below. Permanent catalogue deletion remains blocked while
release history exists, including records whose ZIPs have been removed.

## Remove stale release ZIPs

In **Frozen releases**, select an older release, enter a reason, and choose
**Remove selected stale release ZIP...**. A pond administrator sees the selected
version, ZIP size and retained version, then confirms permanent removal. The
preview verifies both archives. Changes to the preview or release state require
a fresh preview before removal.

Only `arc/releases/<release-id>/dataset.zip` is deleted. Its row remains with
state `removed`; manifests, hashes, metadata snapshots and the removal event
(user, time and reason) remain available for audit. The release version cannot
be reused. Removed versions cannot be verified, downloaded or restored from this
pond. The newest frozen release is always protected, as are releases backing a
current archived dataset or a private working-draft recovery checkpoint.
Checkpoint cleanup and automatic retention schedules are not part of this action.

Removal commits its intent before unlinking the ZIP. State `removing` blocks
downloads/restores and other lifecycle actions until **Recover interrupted
lifecycle operation** completes the deletion. A failure before intent commit
leaves the ZIP intact; recovery cancels that uncommitted intent. Recovery also
requires an administrator for pending removals. Symbolic links and redirected
release paths are refused.

```r
plan <- preview_dataset_release_removal(old_release_id, pond, admin_user_id)
# Inspect plan$version, plan$bytes and plan$retained_version first.
remove_dataset_release(old_release_id, pond, admin_user_id,
  note = "Superseded by the retained baseline", preview = plan)
```

This action frees local storage only. `rclone copy` does not remove remote files;
`rclone sync` can propagate a local deletion to SharePoint. Preview the intended
sync scope using `--dry-run` before propagating removals. SharePoint recycle-bin,
version-history and retention policies can delay space reclamation. Existing
external consumer caches are independent and are not removed.
See the [rclone copy](https://rclone.org/commands/rclone_copy/) and
[rclone sync](https://rclone.org/commands/rclone_sync/) documentation.

## One consumer cache

The R cache helper currently reads a release from a local pond. It uses this
layout outside the pond:

```text
cache/<release-id>/
  receipt.json
  files.json
  release/
    metadata.pdf
    dataset/data/...
```

The ZIP is copied into temporary staging, checked, extracted and checked again.
Only then is the cache folder made available at its final path. The transport
ZIP is removed by default; `keep_zip = TRUE` explicitly retains it. Each release
has its own stable path and is never replaced in place, so a future QGIS client
can keep projects tied to a specific release. Treat cached files as read-only;
`verify_cached_dataset_release()` detects changes. Copy data into a working
dataset before editing it.

```r
# First save/download and review the working PDF in the app.
preview <- preview_dataset_release(dataset_id, pond, user_id)
release <- create_dataset_release(dataset_id, pond, user_id,
  notes = "Reviewed baseline", preview = preview)
cached <- cache_dataset_release(release$release_id, pond, cache = cache_root)
verify_cached_dataset_release(cached)
# QGIS can open files below file.path(cached, "release", "dataset", "data").
```

Remote Flow downloads, a remote published-current pointer, remote publication/withdrawal,
cache eviction with project pinning and a QGIS plugin are future work. This step
provides the local freeze, restoration and cache primitives they can use.

For searching multiple ponds and extracting selected files with their GIS
companions, use the [local R consumer API](pond-consumer-api.md). Its partial
cache has a separate receipt and verifier and integrates with ekznwr's
`dta_arc_extr()` / `dta_fp()` workflow.

## Removing obsolete file metadata before freezing

Use **Remove missing documents and data-file records** in **Metadata review**,
**Supporting documents**, **Data files → File review**, or **Frozen releases**.
Choose **Check for missing files**, select the records you intentionally removed,
then choose **Remove selected missing records**. This checks the saved working
folder directly, so a separate scan is unnecessary. Existing files and logical
file directories are retained, even if an earlier scan marked them missing.

Removal is saved immediately in the working dev/src metadata. It also removes
linked attribute descriptions, profiles, file spatial extents and quality
results, and lineage source records linked to removed documents. Dataset-wide
manual extents and unrelated metadata remain. Review lineage and spatial
coverage after cleanup, save a fresh metadata review and prepare metadata.pdf
again. Subsequent frozen metadata bundles contain the cleaned inventories.
Existing frozen releases remain immutable; create a new version to distribute
the corrected record. Cleanup is unavailable during an open review exchange.


## In-house and public editions

Use **Dataset lifecycle → In-house and public editions**. `dataset_edition_tbl`
is dataset-scoped metadata with one row per designated record. Each edition is
an ordinary dataset with its own UUID and version sequence; `family_uuid` groups
in-house and public records. Unclassified datasets retain their existing behaviour.
The public row pins `source_dataset_uuid`, `source_release_id`, `source_version`
and `source_zip_sha256`, and records `preparation_method` and the builder's initial
`selection_json`. This row participates in approval/document fingerprints, frozen
snapshots, transfer, and restoration. Restoring a pre-designation snapshot retains
the current edition identity and clears any public source link that the old
snapshot cannot establish. `release.json` and `metadata.json` also
identify the frozen edition; verification checks the release descriptor against
the hashed snapshot. Human-readable metadata describes the edition and source.

`set_dataset_edition()` designates an unlocked draft or links an existing public
draft to a verified in-house release. Identity and family cannot be switched.
Older releases can seed a derivative after their current dataset is designated
in-house; those archives are never rewritten. Subsequent metadata review and
PDF preparation are required before another freeze.

`public_draft_files()` and `public_draft_fields()` inspect a verified frozen ZIP.
`public_edition_choices()` suggests common data files on first creation and reuses
previous file/layer/attribute selections on update, reporting additions/removals.
The Shiny **Make public edition** workflow has content, documentation and summary
steps and opens the completed draft through the main dataset hydration callback.

`preview_public_dataset_draft()` captures source/current metadata, projects the
selected bytes, and compares the previous source baseline with current public
values and the incoming source. `apply_public_dataset_draft()` requires decisions
for conflicting metadata or changed public files. Nonconflicting source changes
flow through; public edits and unselected public supporting files survive. The
convenience `create_public_dataset_draft()` uses this pipeline for a new UUID.
CSV/TSV and GeoPackage/GeoJSON support field/layer projection; other formats copy
intact. Row filters and geometry transformations remain external preparation steps.

Prefilling uses an explicit allowlist of descriptive fields, authors, ownership,
citation dates, topics, temporal extents, rights and retained attribute definitions.
Internal notes, source review/approval and lineage narratives are not cloned.
`set_public_metadata_defaults()` saves frozen-source exclusion policy and optional
organisation contact nomination. Rights cannot be excluded. New drafts use the
first owner's public contact; updates retain existing contacts. Citation, inventory
and supported spatial extents are generated from the public result. File-specific
quality results/manual extents are invalidated for review after rebuilding data.

Private staging uses a temporary database to scan and populate all metadata before
committing the live record. Creation uses the source lifecycle lock, transactional
metadata insertion and `.metadata_pond/edition-drafts/` journals. Updates check a
stable metadata/file fingerprint under the writer transaction, retain the dataset
UUID, and use `public_update` lifecycle journals and full prior-draft checkpoints.
Rollback works without requiring an earlier public freeze or retaining the source
release. Filesystem switches roll back on ordinary transaction failure; interrupted
operations are reconciled by normal lifecycle recovery.

`dataset_public_derivation_tbl` stores private source/applied metadata and generated
file hashes. `dataset_public_metadata_policy_tbl` stores exclusions. These scoped
tables participate in local approval hashes, snapshots and restoration but are
explicitly stripped from public transport, along with local review/import history
and application accounts. Portable public imports consequently have no automatic
update baseline. Manually changing the edition's source also clears the baseline.
`organisation_public_contact_tbl` is pond-local configuration, not dataset metadata.

`record_dataset_publication()` appends a local event to
`dataset_publication_tbl`; the latest event determines the currently published
version. It requires a frozen public edition and records the location, reason,
time and active user. It does not distribute data or grant permission.
`dataset_edition_family()` displays independent frozen and published versions.
Publication history is local control data: transfers and draft restoration do not
copy or roll it back, and it does not invalidate metadata approval. A currently
published release is protected from ZIP removal until replaced or withdrawn.

Public transfer payloads exclude local metadata-review/import history and
application accounts. Explicit public contacts remain distributable metadata.
Private snapshots retain the full local metadata for restoration. Normal
unclassified and in-house transfers preserve their existing behaviour.

See the user guide's **Keep in-house and public editions** section for the full
workflow, including later public versions and interrupted-operation recovery.
