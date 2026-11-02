# EKZNW dataset development template

Template version **1.0.0**. One common structure for a methods and lineage report,
with HTML, PDF and editable Microsoft Word outputs. Instructional callouts are
deliberate author guidance: keep them during development and remove them when
the report is complete.

## Add a template from Data Pond

In Dataset workspace, open **Lineage → Lineage authority and supporting
documents → Add data development template**. Choose Word or Quarto, optionally
enter a short project name such as `pisr`, then select **Add template to dataset
folder**. The dataset must be a saved, unlocked draft in `src/` or `dev/`.

The app adds `data_dev_pisr.docx` or `data_dev_pisr.qmd` to the dataset root.
Quarto also gets its own portable `data_dev_pisr_theme/` folder. Existing files
and styles are never replaced. No rendering software is needed to add either
template. The app registers the report under Supporting documents but does not
select the blank template as authoritative lineage. Complete the report, then
select it in Lineage and save the summary and authority choice.

## For Word users

Open `data_dev.docx` and immediately save a project copy as
`data_dev_<project>.docx` in the dataset folder root. Replace the bracketed prompts,
add table rows and duplicate the method subsection for further processing steps.
Use Word's existing heading styles so new headings appear in its navigation pane.
Tables, headings and body text are editable; there are no macros or locked fields.
No Quarto, R or Python installation is needed to write this report in Word.

Keep this Word document as the editable source for your project. Export a PDF
from Word when it is ready to review or distribute. Changes made in Word do not
automatically update the QMD or HTML. Do not rerender the QMD over a Word-authored
project document. Add the completed Word report to the dataset's supporting
documents and select it as a lineage reference in Data Pond; root-level Word
documents are not discovered by the QMD/Rmd-specific lineage listing.

## For Quarto users

Copy `data_dev.qmd` and the complete `ekznw-theme/` folder into the dataset root.
Rename the source to `data_dev_<project>.qmd` and edit its title, subtitle and
report fields. Keep theme paths relative. The styles are a portable copy of the
theme used by `metadata.pdf`, extended with a Word reference document.

From that folder, render the formats you need:

```sh
quarto render data_dev.qmd --to html
quarto render data_dev.qmd --to pdf
quarto render data_dev.qmd --to docx
```

Substitute your renamed QMD in these commands. Rendered files use its basename.
Each render replaces an existing output with that name, so keep Word-authored
documents separately. HTML embeds its display resources. PDF requires XeLaTeX
and the document packages used by the existing EKZNW theme. Quarto and its
bundled Pandoc create DOCX without requiring Microsoft Word. Normal rendering
does not require R or Python: this template has execution disabled and contains
no executable processing chunks.

Data Pond users can run `datapond::setup_document_tools()` once to prepare and
verify the rendering tools, or use **Configuration → Document tools** for a
check without downloads. Recipients using this template outside Data Pond can
install [Quarto](https://quarto.org/docs/get-started/) and its recommended
[TinyTeX](https://quarto.org/docs/output-formats/pdf-engine.html) directly.

Describe and cite processing code or models under Data processing; retain the
code itself and actual run evidence with the dataset. If you later choose to
make the QMD executable, review execution settings and every added chunk before
rendering. Printing a report should not inadvertently install packages, submit
jobs or reproduce a long processing run.

For automatic Quarto citations, add `bibliography: references.bib` to the YAML
and keep the cited bibliography beside the document. Replace the References
placeholder with citation-generated references. The template itself deliberately
does not depend on a personal bibliography or on external data.

## Content and identity

The template retains the shared structure of the PISR and planning-unit reports:
revision history; project summary; main steps; datasets used; data processing;
products; conclusion; appendices for metadata, supporting documentation, folder
structure, processing environment and project decisions; and references.

Quality assessment is made explicit within Products. The repeatable method
section supports both scripted and manual workflows. Environment details belong
here and in saved run records, rather than being duplicated in the dataset README.
For multiple processing environments, identify which steps used each one.

Keep these identifiers separate:

- **Template version:** the structure and style bundle used to start the report.
- **Report version:** revisions to the project narrative and evidence.
- **Dataset UUID and version:** the data product documented by the report.
- **Run identifier and processing dates:** the execution that produced the data.
- **Release identifier:** the frozen archive reference, if already assigned.

Unassigned values should say Not yet assigned, and inapplicable fields should say
Not applicable. Do not imply that a processing run, review or release exists
merely because a placeholder is present. Link to `metadata.pdf`, `citation.txt`
and file manifests rather than manually duplicating those records.

## Provenance and format choices

The structure was extracted from the working `data_dev_pisr_chpc.qmd` and
`data_dev_pu_v0.5.qmd` in the `dta_scp` pond, reviewed on 2026-10-21. Their
unfinished instructional notes were treated as intended template guidance.
Project-specific results, names, connection details, absolute paths and commands
have not been carried into this blank template. The source reports were not
changed. The original theme derives from `~/.ekznw_quarto`; this bundle uses only
portable copies and relative paths.

The HTML, PDF and Word files share the outline, content, logo and earth palette.
They need not paginate identically. Page breaks organise the blank print/Word
template; adjust them as your report grows. HTML has a contents sidebar; Word
has native headings; the PDF has numbered main sections.

The Word theme uses `reference.docx`, following the
[Quarto Word-template mechanism](https://quarto.org/docs/output-formats/ms-word-templates.html).
Instructional notes use [Quarto callouts](https://quarto.org/docs/authoring/callouts.html).
The included `word-guidance.lua` filter turns them into shaded, editable Word
paragraphs using the Guidance Heading and Guidance Text styles. This keeps
guidance separate from the data tables so authors can remove it cleanly.
Updating this template does not automatically restyle or rewrite existing
project reports.


## Maintaining the style bundle

Change `ekznw-theme/style/_ekznw_theme.scss` and `development.css` for HTML,
`ekznw-theme/pdf-header.tex` and `ekznw.yml` for PDF, and the styles in
`ekznw-theme/reference.docx` for Word. Keep the logo, cream background, olive
headings and brown accents consistent across the three. Retain the Lua filter
beside the reference document. These files travel with the QMD; no home-folder
style installation is needed. Recheck all three outputs after changing styles.

The repository's `scripts/build_data_dev_reference.py` can rebuild the Word
styles from a fresh Pandoc `reference.docx` using python-docx. It is a maintainer
tool and is not needed by report authors or for normal Quarto rendering.
