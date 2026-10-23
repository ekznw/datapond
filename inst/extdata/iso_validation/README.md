# Pinned ISO validation resources

`1.3.0/manifest.json` records the upstream URLs and SHA-256 hashes used by
DataPond's optional offline XML validator. The source index is
<https://schemas.isotc211.org/19115/-1/>. Resources under `upstream/` are the
original downloaded files, including their attribution and copyright notices;
they are third-party resources, not DataPond-authored schemas. ISO/TC 211,
OGC and W3C remain their respective publishers. Upstream resource notices and
terms apply; DataPond's MIT licence does not replace them.

The manifest pins bytes, not just URL directory names. For example, the OGC
`gml/3.2.1/` URLs currently serve schemas declaring version 3.2.2. The dependency
closure also contains files imported from other ISO standards/editions. This
does not change the selected ISO 19115 root schema package, 1.3.0.

Maintainer refresh (network required, run from repository root):

```
python3 scripts/iso/fetch_validation_resources.py --add-quality --add-content
```

The additive quality command retains existing pinned bytes and adds the ISO 19157
MDQ 1.2.0 result schema closure and rule. Standalone DQM measure-catalogue rules
are not run (the downloaded DQM rule uses XSLT2); those catalogues are outside
this exporter. A full refresh without the flag resets to the metadata-only bundle
and requires deliberate review.

Review every changed hash and rerun the conformance tests before accepting a
refresh. Tests and app assessments never run this downloader. The 1.3.1 rules
also listed on the current upstream site are deliberately not substituted.

The validation runner lives at `inst/iso/validate_metadata.py` in the source
tree and requires Python 3.9+ with lxml. It reads only pinned local resources,
ignores document-supplied schema locations, and rejects DTDs. Its two `gco`
namespace adaptations, the corrected `LI_Source` rule context, four MDQ rule
namespace corrections, and Schematron layout compatibility handling are
documented in `docs/iso-metadata-gap-infill-plan.md`. Original files are never
patched. A passing report covers only the checks identified in that report.
The metadata creation-date rule still requires DateTime even though its XSD
permits Date. That mismatch is reported; a missing time is never manufactured.

GFC 2.2.0 is now an explicit schema root (its bytes were already in the pinned
closure). `--add-content` retains existing hashes and quality resources. The
catalogue runner uses `--kind catalogue`: XSD, embedded CIT rules and local
fragment references, without claiming an official GFC Schematron. The R pair
validator additionally checks filename, UUID and citation agreement. Legal nil
reasons appear as warnings; a pass does not establish semantic completeness.
