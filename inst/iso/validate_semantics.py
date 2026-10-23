#!/usr/bin/env python3
"""DataPond export-profile checks, distinct from the pinned ISO rule outcome."""
import argparse
import calendar
from collections import Counter
from datetime import datetime, timedelta, timezone
import hashlib
import json
import math
from pathlib import Path
import re
import sys

BASE = "https://schemas.isotc211.org/19115/-1/"
NS = {p: BASE + p + "/1.3" for p in
      ("mdb", "cit", "mcc", "mri", "mrl", "gex", "mrc", "msr", "lan", "mco")}
NS.update(gco="https://schemas.isotc211.org/19103/-/gco/1.2",
          gml="http://www.opengis.net/gml/3.2", xlink="http://www.w3.org/1999/xlink",
          cat="https://schemas.isotc211.org/19139/-/cat/1.2",
          gfc="https://schemas.isotc211.org/19110/-/gfc/2.2")
CATALOGUE = "https://schemas.isotc211.org/resources/codelists/codelists.xml"
LANGUAGES = "https://www.loc.gov/standards/iso639-2/"
ENCODINGS = "https://www.iana.org/assignments/character-sets/character-sets.xml"


def issue(rule, message, location=""):
    return dict(stage="semantics", message=message, location=location, rule=rule,
                source="DataPond export semantics 1.0.0", line=0)


def date_interval(text):
    """Recorded precision becomes an interval, never an invented exact date."""
    if re.fullmatch(r"\d{4}", text):
        year = int(text)
        return datetime(year, 1, 1), datetime(year, 12, 31, 23, 59, 59, 999999)
    if re.fullmatch(r"\d{4}-\d{2}", text):
        year, month = map(int, text.split("-"))
        return datetime(year, month, 1), datetime(year, month, calendar.monthrange(year, month)[1], 23, 59, 59, 999999)
    if re.fullmatch(r"\d{4}-\d{2}-\d{2}", text):
        start = datetime.strptime(text, "%Y-%m-%d")
        return start, start.replace(hour=23, minute=59, second=59, microsecond=999999)
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})?", text):
        raise ValueError("Unsupported date/time lexical form")
    value = datetime.fromisoformat(text.replace("Z", "+00:00"))
    if value.utcoffset() is not None and abs(value.utcoffset()) > timedelta(hours=14):
        raise ValueError("Timezone outside ISO range")
    return value, value


def validate(path, bundle, catalogue=None):
    report = dict(valid=False, status="unavailable", policy="DataPond export semantics",
                  version="1.0.0", checks=[], issues=[], warnings=[],
                  coverage="Pinned code membership and canonical registry references; local identifiers, "
                  "recorded date/extent ordering, numeric domains, empty text and metadata encoding. "
                  "Not factual accuracy, every ISO condition, or receiving-catalogue acceptance.")
    try:
        from lxml import etree
        root = Path(bundle).resolve()
        manifest = json.loads((root / "manifest.json").read_text(encoding="utf-8"))
        if manifest["version"] != "1.0.0":
            raise ValueError("Unsupported semantic resource version")
        expected = {"iso-codelists.xml", "character-sets.xml", "iso-639-2.txt"}
        if {item["path"] for item in manifest["resources"]} != expected:
            raise ValueError("Incomplete semantic resource manifest")
        for item in manifest["resources"]:
            file = (root / item["path"]).resolve()
            if not file.is_relative_to(root) or hashlib.sha256(file.read_bytes()).hexdigest() != item["sha256"]:
                raise ValueError("Semantic resource checksum mismatch: " + item["path"])
        report["resources"] = manifest
        parser = etree.XMLParser(resolve_entities=False, load_dtd=False, no_network=True)
        lists = etree.parse(str(root / "iso-codelists.xml"), parser)
        codes = {}
        for node in lists.findall(".//cat:CT_Codelist", NS):
            identifier = node.get("id", "")
            if not identifier.startswith("ISO19115-1.1."):
                continue
            module, name = identifier.split(".")[-2:]
            codes["{" + BASE + module + "/1.3}" + name] = (
                set(node.xpath("cat:codeEntry/cat:CT_CodelistValue/cat:identifier/*/text()", namespaces=NS)),
                CATALOGUE + "#" + identifier)
        languages = set()
        for line in (root / "iso-639-2.txt").read_text(encoding="utf-8-sig").splitlines():
            for code in line.split("|")[:2]:
                if re.fullmatch(r"[a-z]{3}", code):
                    languages.add(code)
        charset = etree.parse(str(root / "character-sets.xml"), parser)
        encodings = set(charset.xpath("//*[local-name()='record']/*[local-name()='name' or local-name()='alias']/text()"))
        codes["{" + NS["lan"] + "}LanguageCode"] = (languages, LANGUAGES)
        codes["{" + NS["lan"] + "}MD_CharacterSetCode"] = (encodings, ENCODINGS)
    except Exception as error:
        report["issues"].append(issue("SEM-RESOURCES", str(error)))
        return report

    def check_document(filename, label):
        tree = etree.parse(str(filename), parser)
        if tree.docinfo.doctype:
            raise ValueError("DTD/entity declarations are not allowed")
        expected = "{" + (NS["gfc"] if label else NS["mdb"]) + "}" + ("FC_FeatureCatalogue" if label else "MD_Metadata")
        if tree.getroot().tag != expected:
            report["issues"].append(issue("SEM-DOCUMENT", "Unexpected document root for the export profile.", label + "/"))
            return

        def fail(rule, message, node):
            report["issues"].append(issue(rule, message, label + tree.getpath(node)))

        def warn(message, node):
            report["warnings"].append(dict(reason=message, location=label + tree.getpath(node)))

        def nodes(xpath):
            return tree.xpath(xpath, namespaces=NS)

        def check(rule, selected, action):
            before = len(report["issues"])
            for node in selected:
                try:
                    action(node)
                except (ValueError, OverflowError):
                    fail(rule, "Invalid numeric or date value for this property.", node)
            report["checks"].append(dict(stage=rule, document=label or "metadata",
                                         checked=len(selected), valid=len(report["issues"]) == before))

        def code(node):
            entry = codes.get(node.tag)
            if entry is None:
                fail("SEM-CODE", "Code list is outside the supported export profile: " + etree.QName(node).localname, node)
            elif node.get("codeListValue") not in entry[0]:
                fail("SEM-CODE", "Unknown " + etree.QName(node).localname + " code: " + str(node.get("codeListValue")), node)
            if entry and node.get("codeList") != entry[1]:
                fail("SEM-CODE-URI", "Code list must reference the pinned canonical registry: " + entry[1], node)
        check("SEM-CODE", nodes("//*[@codeList or @codeListValue]"), code)
        topics = codes["{" + NS["mri"] + "}MD_TopicCategoryCode"][0]
        check("SEM-TOPIC", nodes("//mri:MD_TopicCategoryCode"),
              lambda n: fail("SEM-TOPIC", "Unknown ISO topic category: " + str(n.text), n) if n.text not in topics else None)
        check("SEM-LANGUAGE", nodes("/gfc:FC_FeatureCatalogue/cat:language/gco:CharacterString"),
              lambda n: fail("SEM-LANGUAGE", "Unknown feature catalogue language code.", n) if n.text not in languages else None)
        check("SEM-TEXT", nodes("//gco:CharacterString"),
              lambda n: fail("SEM-TEXT", "Empty text must be omitted or use an appropriate nil reason.", n)
              if not (n.text or "").strip() else None)

        ids = Counter(nodes("//@id | //@gml:id"))
        check("SEM-ID", nodes("//*[@id or @gml:id]"),
              lambda n: fail("SEM-ID", "Duplicate local XML identifier.", n)
              if any(ids[v] > 1 for k, v in n.attrib.items() if k in ("id", "{" + NS["gml"] + "}id")) else None)
        check("SEM-LINK", nodes("//*[@xlink:href]"),
              lambda n: fail("SEM-LINK", "Unresolved or ambiguous local XML reference: " + n.get("{" + NS["xlink"] + "}href"), n)
              if n.get("{" + NS["xlink"] + "}href").startswith("#")
              and ids[n.get("{" + NS["xlink"] + "}href")[1:]] != 1 else None)

        def date(n):
            if not (n.text or "").strip() and n.get("indeterminatePosition"):
                return
            try:
                date_interval(n.text or "")
            except (ValueError, OverflowError):
                fail("SEM-DATE", "Invalid recorded date/time: " + str(n.text), n)
        check("SEM-DATE", nodes("//gco:Date | //gco:DateTime | //gml:beginPosition | //gml:endPosition | //gml:timePosition"), date)

        def ordered(first, last, owner, description):
            if first is None or last is None or not first.text or not last.text:
                return
            if first.get("indeterminatePosition") or last.get("indeterminatePosition"):
                warn(description + " order is indeterminate; qualifiers are preserved.", owner)
                return
            try:
                a, b = date_interval(first.text), date_interval(last.text)
            except (ValueError, OverflowError):
                return  # SEM-DATE already explains invalid dates.
            if (a[0].tzinfo is None) != (b[0].tzinfo is None):
                warn(description + " order cannot be compared exactly across zoned and unzoned dates.", owner)
            elif a[0] > b[1]:
                fail("SEM-ORDER", description + " starts later than it ends.", owner)
            elif a[1] > b[0]:
                warn(description + " intervals overlap at the recorded precision; exact order is unverified.", owner)

        periods = nodes("//gml:TimePeriod")
        check("SEM-ORDER", periods, lambda n: ordered(n.find("gml:beginPosition", NS), n.find("gml:endPosition", NS), n, "Temporal extent"))
        creations = nodes("/mdb:MD_Metadata/mdb:dateInfo/cit:CI_Date[cit:dateType/*/@codeListValue='creation']/cit:date/*")
        updates = nodes("/mdb:MD_Metadata/mdb:dateInfo/cit:CI_Date[cit:dateType/*/@codeListValue='lastUpdate']/cit:date/*")
        check("SEM-METADATA-DATES", updates, lambda n: ordered(creations[0] if creations else None, n, n, "Metadata creation / last update"))

        def number(node, xpath):
            found = node.xpath(xpath, namespaces=NS)
            return float(found[0].text) if found else None

        def bounds(node):
            if etree.QName(node).localname == "EX_GeographicBoundingBox":
                names = ["westBoundLongitude", "eastBoundLongitude", "southBoundLatitude", "northBoundLatitude"]
                values = [number(node, "gex:" + name + "/*") for name in names]
                if any(v is None for v in values):
                    return  # XSD owns missing values.
                w, e, s, n = values
                valid = all(math.isfinite(v) for v in values) and -180 <= w <= 180 and -180 <= e <= 180 and -90 <= s <= n <= 90
                # west > east is a legitimate antimeridian-crossing extent.
            else:
                prefix = "gex" if etree.QName(node).localname == "EX_VerticalExtent" else "mrc"
                low = number(node, prefix + ":minimumValue/*" if prefix == "gex" else "mrc:minValue/*")
                high = number(node, prefix + ":maximumValue/*" if prefix == "gex" else "mrc:maxValue/*")
                valid = all(math.isfinite(v) for v in (low, high) if v is not None) and (low is None or high is None or low <= high)
            if not valid:
                fail("SEM-BOUNDS", "Extent or sample bounds must be finite, ordered and within their coordinate domain.", node)
        check("SEM-BOUNDS", nodes("//gex:EX_GeographicBoundingBox | //gex:EX_VerticalExtent | //mrc:MD_SampleDimension"), bounds)
        def positive(n):
            value = float(n.text)
            if not math.isfinite(value) or value <= 0:
                fail("SEM-POSITIVE", "Spatial resolution, dimension size and scale denominator must be finite and positive.", n)
        check("SEM-POSITIVE", nodes("//mri:distance/gco:Distance | //mri:denominator/gco:Integer | //msr:resolution/gco:Measure | //msr:dimensionSize/gco:Integer"), positive)
        check("SEM-ENCODING", nodes("/mdb:MD_Metadata/mdb:defaultLocale/lan:PT_Locale/lan:characterEncoding/lan:MD_CharacterSetCode"),
              lambda n: fail("SEM-ENCODING", "Metadata encoding must be UTF-8 for this UTF-8 XML exporter; dataset encoding is independent.", n)
              if n.get("codeListValue") != "UTF-8" else None)

    try:
        check_document(path, "")
        if catalogue:
            check_document(catalogue, "catalogue:")
    except Exception as error:
        report["issues"].append(issue("SEM-DOCUMENT", "Semantic checks could not complete: " + str(error)))
        return report  # unavailable, never an incomplete pass
    report["valid"] = not report["issues"]
    report["status"] = "pass" if report["valid"] else "fail"
    return report


if __name__ == "__main__":
    args = argparse.ArgumentParser()
    args.add_argument("document")
    args.add_argument("--bundle", required=True)
    args.add_argument("--catalogue")
    opts = args.parse_args()
    result = validate(opts.document, opts.bundle, opts.catalogue)
    print(json.dumps(result, ensure_ascii=False))
    sys.exit(0 if result["valid"] else 2 if result["status"] == "unavailable" else 1)
