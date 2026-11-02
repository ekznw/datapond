# Saved licensing and access/use audit — 2026-10-21

Audited `/home/paulg/dta_scp` and `/mnt/pjg_dta/bspi_data/bspi_xdrive`:
**82 saved records, with no pond changes.** The main redundancy is in BSPI:
59 structured use notes are exactly the older access note followed by the older
use note, after whitespace normalisation. Rights disagreements also need review;
removing repeated text alone would not resolve them.

The [per-record evidence](pond-rights-audit-2026-10-21.json) retains the saved
licensing rows, older notes, dataset IDs/UUIDs, overlap flags, and database hashes.
The [audit script](../scripts/iso/audit_pond_rights.py) opens SQLite with `mode=ro`
and `query_only`, using a read transaction per pond. It does not call application
initialisation or migrations. Database/WAL hashes were unchanged across both
reads. All records were included; none were hidden or marked deleted, and no
orphan licensing rows were found. This is a comparison of recorded metadata,
not verification of the permissions in underlying agreements or source documents.

## Coverage and redundancy

| Finding | dta_scp | BSPI xdrive |
| --- | ---: | ---: |
| Dataset records | 16 | 66 |
| Saved licensing rows | 16 | 61 |
| Older access notes populated | 0 | 64 |
| Older use notes populated | 1 | 64 |
| Structured use notes populated | 3 | 60 |
| Identical complete older/structured use notes | 0 | 0 |
| Structured use equals older access + older use | 0 | 59 |
| Older use contains the entire structured use plus additional text | 0 | 1 |
| Explicit unknown redistribution code | 2 | 48 |
| Rights basis marked unknown/review required | 3 | 60 |
| Rights basis blank (including absent licensing rows) | 2 | 5 |
| Conflicting populated embargo mirrors | 0 | 0 |

These counts overlap; they are not numbers of distinct defective records.
Blank or unknown rights do not establish permission to distribute. Different
access and redistribution codes are not inherently contradictory: access to
the local copy and permission to pass it on describe different things.

The 59 BSPI concatenations are IDs **5–7 and 9–64**. The current exporter
removes whole-value whitespace-equivalent duplicates within one XML property;
it deliberately retains partial overlaps and text across different properties.
Consequently these concatenations can repeat older use wording in
`useLimitation`, and access wording across `useLimitation` and `otherConstraints`.
Do not broaden deduplication to arbitrary substrings of rights text.

BSPI **ID 1** (semi-detailed soil mapping) is the reverse case: the older use
note contains the structured conditions plus the additional warning that regional
products do not replace detailed soil surveys. Removing it would lose that warning.
In dta_scp **ID 16**, the older use note gives scale/fitness guidance while the
structured use note contains redistribution and liability wording; they are distinct.

## Records requiring a decision

| Pond / ID | Dataset | Recorded issue and next action |
| --- | --- | --- |
| BSPI / 2 | Earthworm soil classification | Structured CC-BY-4.0, EKZNW ownership and attribution-based redistribution disagree with the older statement that licence/reuse rights are unknown. Verify the basis for the licence selection; then update the outdated statement or correct the selection. Preserve the separate warning about classification accuracy and occurrence interpretation. |
| dta_scp / 1 | Comparison of baseline climate data | `PROVIDER-LICENCE` requires the provider licence to accompany redistribution, but redistribution is `unrestricted` and rights basis is `review_required`. No licence URL or permission reference is recorded. Verify provider terms before aligning these fields. |
| dta_scp / 2 | Multi-scale water resource assessments | Rights basis says `ekznw_owned`, while the saved holder is University of KwaZulu-Natal and the selected licence is EKZNW internal. Confirm ownership or permission basis. An internal access policy does not itself establish ownership. |
| dta_scp / 11 | NBA2018_Dunebase | Permission-reference text combines prior permission/no resale requirements with a public-domain/acknowledgement statement. Retain both pending source clarification. |
| dta_scp / 16 | EstuarineEcosystemMap2018_19092019 | Structured use text combines prior permission/no resale requirements with a public-domain/acknowledgement statement. Retain both pending source clarification; retain the separate older scale/fitness note. |
| BSPI / 5–6 | Coastal Biodiversity Priority Areas; Seashore and Estuary CBA | Notes explicitly identify inconsistent source permissions. Current `not_distributable`/unknown settings retain uncertainty. Resolve against the authoritative source; deduplication is not resolution. |
| BSPI / 60–64 | KZN land-cover versions, 2005–2011 | Notes report encouraged external distribution versus project-specific terms prohibiting third-party redistribution. Keep restrictions and source evidence until clarified. |
| dta_scp / 3 | Hydrological planning units | CC-BY-SA-4.0 is selected but rights basis remains `review_required`; attribution is required with no attribution text. Confirm the selection and add the intended acknowledgement. |
| dta_scp / 5, 7 | Cost estimation; DWS Dams | Licensing rows exist but have no licence identity, rights basis or access code; redistribution remains unknown. Populate from evidence when available. |
| BSPI / 3, 4, 8 | Marine CBA v1.2; EBSAs; invasive plant survey | No structured licensing row. Existing access/use notes retain caveats and uncertainty. Use them as evidence for review, without treating absence of a row as permission. |
| BSPI / 65–66 | Amajuba Dannhauser; Emamdlangeni | No structured licensing row and no access/use notes. Rights are unrecorded. |

Five dta_scp provider-licence records (**1, 8, 9, 14, 16**) have neither a
licence URL nor permission reference. This is a missing recorded reference,
not proof that permission or supporting documents do not exist. ID 16 already
has substantive terms in its use note. Other BSPI rows deliberately record
unknown/review-required rights; those are population follow-ups, not exporter gaps.

## Proposed cleanup sequence

1. Resolve the conflicting licence selections and source statements above.
   Keep a record of the authority used and the superseded wording.
2. For the 59 BSPI concatenations, keep access conditions in their access field
   and use/fitness conditions in their use field. Make the structured use field
   canonical only after confirming that every distinct older statement is retained.
   A migration should record its provenance and prevent old mirrors from reviving
   deleted text. Do not delete columns or apply substring removal globally.
3. Keep internal review notes separate from exported terms. Where use notes also
   describe processing or quality, move those statements only with traceable
   preservation in lineage/quality and confirmation that no use caveat is lost.
4. Test the proposed migration and export against copies of these records before
   any operational update. This audit has not changed saved records or the crosswalk.

GAP-019 remains resolved for export; this audit supplies the previously missing
real-record evidence for subsequent cleanup. GAP-018 remains closed for the
agreed local-validation scope. GAP-020 remains deferred until a receiving
catalogue is selected.

To repeat the audit, use a new report filename outside both ponds:

```sh
python3 scripts/iso/audit_pond_rights.py --output /tmp/pond-rights-audit.json \
  /home/paulg/dta_scp /mnt/pjg_dta/bspi_data/bspi_xdrive
```

## Per-record review index

The following index covers every saved record. Full wording is in the linked
JSON evidence. Flags describe recorded values, not a finding that every flagged
record is incorrect.

### dta_scp

| ID | Dataset | Recorded flags |
| ---: | --- | --- |
| 1 | Comparison of baseline climate data | Rights unknown / review required; Provider reference absent |
| 2 | Development of datasets for multi-scale water resource assessments towards a water secure South Africa | No automated overlap/completeness flag; see decisions above |
| 3 | Hydologically delineated planning units development for the KwaZulu-Natal Systematic Conservation Plan | Rights unknown / review required; Attribution text absent |
| 4 | Potential Incoming Solar Radiation dataset | No automated overlap/completeness flag; see decisions above |
| 5 | Cost estimation for the KwaZulu-Natal Systematic Conservation Plan | Rights basis blank; Redistribution unknown |
| 7 | DWS Dams (1:50 000) | Rights basis blank; Redistribution unknown |
| 8 | Inventory of Inland Aquatic Ecosystems | Provider reference absent |
| 9 | South African National Land Cover, 2022 (SANLC) | Provider reference absent |
| 10 | Estuarine Function Zones of KwaZulu-Natal, South Africa | Rights unknown / review required |
| 11 | NBA2018_Dunebase | No automated overlap/completeness flag; see decisions above |
| 12 | Multiscalar terrain ruggedness using Vector Ruggedness Measure | No automated overlap/completeness flag; see decisions above |
| 13 | KZN Modified DEM Decimal Degrees (derived from 30m SRTM DEM) | No automated overlap/completeness flag; see decisions above |
| 14 | Topographic perennial and non-perennial rivers | Provider reference absent |
| 15 | Vegetation coverage for KZN under development for the SCP revision | No automated overlap/completeness flag; see decisions above |
| 16 | EstuarineEcosystemMap2018_19092019 | Provider reference absent |
| 17 | Digital Elevation Model construction using spot heights, contour data, and SRTM for gap infilling | No automated overlap/completeness flag; see decisions above |

### BSPI xdrive

| ID | Dataset | Recorded flags |
| ---: | --- | --- |
| 1 | Spatial modelling for semi-detailed soil mapping in KwaZulu-Natal | Structured use repeated inside older use; Rights unknown / review required |
| 2 | KwaZulu-Natal earthworm soil classification - broad-soils layer and classification workbook | No automated overlap/completeness flag; see decisions above |
| 3 | National Coastal and Marine Spatial Biodiversity Plan - CBA Map, Version 1.2 | No licensing row; Rights basis blank |
| 4 | South African Ecologically or Biologically Significant Marine Areas - zones and descriptions | No licensing row; Rights basis blank |
| 5 | Coastal Biodiversity Priority Areas 2023 - Version 1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 6 | National Seashore and Estuary CBA Map - Version 1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 7 | National Wetland Map 6 - 20240330 delivery | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 8 | National Invasive Alien Plant Survey 2023 - raster layers | No licensing row; Rights basis blank |
| 9 | DWS KwaZulu-Natal dams - acquired August 2026 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 10 | KwaZulu-Natal boreholes - DWS database extract August 2026 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 11 | KwaZulu-Natal wastewater facilities - DWS database extract August 2026 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 12 | KwaZulu-Natal mean annual precipitation - Schulze atlas derivative, 200 m | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 13 | KwaZulu-Natal mean annual temperature - Schulze atlas derivative, 200 m | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 14 | KwaZulu-Natal estuarine conservation plan - legacy estuary-cplan layer | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 15 | KwaZulu-Natal Freshwater Systematic Conservation Plan - best selection surface 2007 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 16 | KwaZulu-Natal Terrestrial Systematic Conservation Plan - public minimum selection 2010 v1.2 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 17 | KwaZulu-Natal Terrestrial Systematic Conservation Plan - internal 2010 v1.1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 18 | KwaZulu-Natal terrestrial conservation planning - 2004 irreplaceability and minimum selection | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 19 | KwaZulu-Natal terrestrial conservation planning - 2007 irreplaceability and minimum selection | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 20 | KwaZulu-Natal terrestrial conservation planning - resource planning units 2010 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 21 | KwaZulu-Natal BSP landscape ecological corridors - 2014 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 22 | KwaZulu-Natal BSP landscape ecological corridors - 2016 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 23 | KwaZulu-Natal BSP local ecological corridors - 2014 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 24 | KwaZulu-Natal BSP local-knowledge CBAs - internal 2016-02-01 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 25 | KwaZulu-Natal BSP local-knowledge CBAs - public 2016-02-01 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 26 | KwaZulu-Natal BSP local-knowledge CBAs - legacy 2016-01-13 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 27 | KwaZulu-Natal macro-ecological corridors - version 3 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 28 | KwaZulu-Natal macro-ecological corridors - version 3.1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 29 | KwaZulu-Natal macro-ecological corridors - version 2 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 30 | KwaZulu-Natal macro-ecological corridors - version 1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 31 | KwaZulu-Natal primary catchments - March 2010 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 32 | KwaZulu-Natal secondary catchments - March 2010 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 33 | KwaZulu-Natal River Disturbance Index - internal 2010 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 34 | KwaZulu-Natal River Disturbance Index - public 2010 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 35 | KwaZulu-Natal river condition/disturbance assessment - legacy RCI_w31 delivery | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 36 | KwaZulu-Natal top ten free-flowing rivers - 2017 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 37 | KwaZulu-Natal non-perennial rivers - supplied historical coverage | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 38 | KwaZulu-Natal perennial rivers - supplied historical coverage | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 39 | KwaZulu-Natal mainstem and free-flowing rivers - 2008 assessment | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 40 | KwaZulu-Natal river polygons - 2007 source, January 2008 compilation | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 41 | KwaZulu-Natal modelled priority water-production surface - 2007 v1.0 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 42 | uMgungundlovu points of interest - MapIT 2010 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 43 | South African Transnet railway network and stations - historical delivery | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 44 | KwaZulu-Natal March 2011 roads, surface and bridges | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 45 | KwaZulu-Natal September 2014 roads and RCAM surfaces | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 46 | KwaZulu-Natal July 2017 road network, classification and surfaces | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 47 | KwaZulu-Natal December 2017 roads and surfaces (January 2018 delivery) | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 48 | KwaZulu-Natal July 2019 roads and RCAM surfaces | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 49 | KwaZulu-Natal January 2021 roads and RCAM surfaces | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 50 | KwaZulu-Natal September 2021 roads and RCAM surfaces | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 51 | KwaZulu-Natal March 2024 provincial road network and surface classifications | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 52 | KwaZulu-Natal October 2010 RISFSA roads, district networks and uMhlathuze urban roads | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 53 | KwaZulu-Natal January 2009 road network v1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 54 | KwaZulu-Natal January 2009 segmented road network v2 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 55 | KwaZulu-Natal schools and contacts - 2010 coverage | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 56 | KwaZulu-Natal school outreach selections and protected-area context | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 57 | KwaZulu-Natal schools - 2006 delivery | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 58 | KwaZulu-Natal towns - legacy nattown_wdd coverage | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 59 | Ezemvelo protected-area immovable assets - 2011 verification project | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required |
| 60 | KwaZulu-Natal 2005 land cover v2.0 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 61 | KwaZulu-Natal 2005 land cover v3.1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 62 | KwaZulu-Natal 2008 land cover v1.1 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 63 | KwaZulu-Natal 2008 land cover v2.0 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 64 | KwaZulu-Natal 2011 land cover v1.0 | Older use repeated inside structured use; Access repeated inside structured use; Rights unknown / review required; Redistribution unknown |
| 65 | AmajubaDM DannhauserLM 05v109 w31 | No licensing row; Rights basis blank |
| 66 | AmajubaDM EmamdlangeniLM 05v109 w31 | No licensing row; Rights basis blank |
