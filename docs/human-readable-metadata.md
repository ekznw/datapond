# Human-readable dataset metadata

`Dataset status → Human-readable metadata → Save PDF to dataset folder & download`
creates **metadata.pdf** and **citation.txt** from saved metadata in the working
dataset folder root, and downloads the identical PDF to the browser. The separate
citation download also saves `citation.txt` at the dataset root, without rendering
a PDF. Select an active user and use an unlocked draft in `src/` or `dev/`.
Save edits first; these actions replace previous working copies only after
successful preparation. Rendering or delivery failure preserves the previous
copies. Frozen releases are never modified.

Open the downloaded PDF and inspect the dataset folder before freezing. Freezing
requires current, unmodified prepared documents and a review confirmation in the
app; it copies the prepared PDF and citation without regenerating them. If saved
metadata changes the document, download and review it again. A citation-only
download does not make an old PDF current. The app can check freshness and file
integrity, but the review confirmation is the user's statement that they opened
and inspected the documents.

The PDF uses the EKZNW corporate logo,
cream banner, olive headings and brown accents derived from the existing
`~/.ekznw_quarto` HTML theme. It has A4 pages, repeating headers and page numbers.
Quarto and XeLaTeX are required for PDF rendering, and Python 3.9+ for verifying the
prepared files in the app. Run `datapond::setup_document_tools()` once after
installation to check these tools, provision TinyTeX if needed, and test both
EKZNW themes. Missing Quarto or Python gets official installation guidance.
Use **Configuration → Document tools → Check document tools**, or
`datapond::check_document_tools()`, to verify an existing setup without downloads.
Ordinary rendering does
not automatically install TeX packages or execute dataset processing code.

The collector reads saved metadata in one read-only SQLite transaction. The app
saves the generated documents to the working folder and retains their source,
theme and freshness evidence under `.metadata_pond/documents/<dataset-id>/`.
Source and styling are copied into frozen releases; preparation evidence remains
private. Missing essential values remain
visible as **Not recorded**; optional empty sections are omitted where useful.
Generating the PDF is independent of ISO validation and release approval.

## Fields and their authority

| PDF section | Saved source and interpretation |
| --- | --- |
| Title and identity | Dataset title, version and UUID from `entry_tbl`; document generation time is separate from dataset and metadata dates. |
| Overview | Abstract, purpose, credits and keywords; ordered authors and organisation roles. |
| Citation | Exact saved citation, also written to `citation.txt`; typed citation dates where recorded. |
| Coverage | Named geographic area, typed temporal extents (legacy start/end fallback), valid saved bounds, scale guidance, active-file CRS/resolution and selected manual extents; vertical reference where recorded. |
| Data files | Active saved inventory with relative paths, formats and descriptions; recorded distribution-format versions. No rescan or claim that the PDF contains these files. |
| Lineage | Saved lineage statement, sources and processing summaries; source citations and ordered process steps; explicitly selected active lineage documents. |
| Development documents | Root-level QMD/Rmd files, including `data_dev*` and project-specific names, case-insensitive. Listed as supporting development records separately from explicitly selected authoritative lineage references. No recursive archive/scratch discovery or execution. |
| Quality | Saved use/fitness limitations and quality evaluations, values, units and conformance results. |
| Rights | Saved licence, holder, access, redistribution, attribution, sensitivity handling, embargo and release terms. Conflicting saved conditions are not silently resolved. |
| Contact and maintenance | Designated dataset contacts (metadata responsibilities as fallback), update frequency, metadata UUID, creation date where present and last recorded review timestamp. |
| Supporting documents | Active documents selected for metadata, excluding documents already listed as lineage references or root development documents. |
| Attribute dictionary | Definitions and units grouped by active file and layer, with recorded legend notes. Orphan/ambiguous or inactive-file attributes are excluded and counted. Profile examples/minima/maxima are not presented as permitted values. |

Software, package versions and processing-environment details belong in the
development QMD/Rmd as part of lineage. The metadata document points there;
the README should acknowledge the development documents without duplicating
their software inventory. Existing development files and lineage selection
flags are not changed by this export.

The exporter does not infer missing NoData conventions, units, geographic
names, processing environments or data quality. Supply these in the appropriate
saved descriptions/dictionary and development document. Local absolute paths
are not exported as file references. Internal review narratives, user identities,
lineage notes and licence-review notes are not included. Saved public prose and
designated contact details still require normal curator review for distribution.

## Portable source and shared styling

The R API creates a new output folder containing the PDF, its generated QMD,
`citation.txt`, and `ekznw-theme/`:

```r
datapond::write_metadata_pdf(
  dataset_id = 4,
  base_path = "/path/to/pond",
  directory = "/existing/output-parent/pisr-metadata"
)
```

This independent R export remains read-only with respect to the pond; it does
not prepare the working folder for freezing. The destination must be new and
outside the pond. Rendering happens in a
temporary sibling folder; failure leaves no partial published output and
existing exports are never overwritten.

To reuse the PDF theme in a development document, copy the returned
`ekznw-theme/` folder next to that document and add:

```yaml
metadata-files: [ekznw-theme/ekznw.yml]
```

Remove the old absolute HTML `theme` and `css` settings when adopting the shared
theme; document-local YAML overrides inherited settings. Keep the development
document's own execution settings. The generated metadata QMD disables execution
because it contains saved descriptive text only. Do not apply that setting to a
development workflow that intentionally runs its code.

The shared configuration supports HTML and PDF, following [Quarto's PDF
configuration](https://quarto.org/docs/output-formats/pdf-basics). The metadata
export's PDF path is rendered and checked. Existing QMD/Rmd documents are not
migrated automatically. The [frozen-release workflow](dataset-release-lifecycle.md)
now packages the PDF with the dataset. R Markdown would
need its own output configuration to reuse the same LaTeX styling.
