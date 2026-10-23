# Frozen dataset lifecycle

Implemented 2026-10-21. Use **Dataset lifecycle → Frozen releases**. Freezing
creates a fixed version in the local archive; publishing and distribution
approval are separate steps and are not implemented by these controls.

## Working, frozen and restored

1. Save the dataset's metadata, including title, dataset UUID, version and
   citation. Select an active application user. Finish or cancel any active
   metadata review request before changing its lifecycle.
2. Under **Dataset status → Human-readable metadata**, choose **Save PDF to dataset
   folder & download**. This saves `metadata.pdf` and `citation.txt` at the working
   dataset root and downloads the same PDF. Open it and inspect the dataset folder.
   Document preparation is an explicit step; freezing and archiving never render
   or refresh these documents. Repeat this step after changes to document content.
3. Preview the saved dataset. The preview checks that the prepared documents are
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
  arc/releases/<release-id>/
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

`dataset.zip` contains the original dataset under `dataset/`, plus `metadata.pdf`,
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
references. It is not included in the public ZIP, and no full catalogue is
distributed. Back up the catalogue, working datasets, `arc/`,
`.metadata_pond/documents/` and `.metadata_pond/releases/`
together. The public ZIP alone supports external data use; it is not a complete
replacement for the private restoration state.

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
and releases have no automatic expiry or garbage collection in this version.
Permanent catalogue deletion is blocked while frozen releases exist.

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

Remote Flow downloads, a published-current pointer, publication/withdrawal,
cache eviction with project pinning and a QGIS plugin are future work. This step
provides the local freeze, restoration and cache primitives they can use.
