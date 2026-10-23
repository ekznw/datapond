#!/usr/bin/env python3
"""Offline ISO metadata validation. JSON on stdout; no pond/database access."""
import argparse
import hashlib
import json
from pathlib import Path
import sys

XSD = "http://www.w3.org/2001/XMLSchema"
SCH = "http://purl.oclc.org/dsdl/schematron"
SVRL = "http://purl.oclc.org/dsdl/svrl"
MDB = "https://schemas.isotc211.org/19115/-1/mdb/1.3"
GCO = "https://schemas.isotc211.org/19103/-/gco/1.2"


def issue(stage, message, location="", rule="", source="", line=0):
    return dict(stage=stage, message=message, location=location,
                rule=rule, source=source, line=line or 0)


def _validate(path, bundle, kind="metadata"):
    report = dict(valid=False, status="unavailable", schema_package="1.3.0",
                  document_kind=kind, checks=[], issues=[], adaptations=[],
                  coverage="XML structure and bundled ISO 19115-1 / ISO 19157 MDQ rules; "
                           "not crosswalk coverage, code-list membership, linked "
                           "feature-catalogue or standalone quality-measure catalogue validity, or factual accuracy.")
    if sys.version_info < (3, 9):
        report["issues"].append(issue("engine", "ISO validation requires Python 3.9 or later with lxml."))
        return report
    try:
        from lxml import etree, isoschematron
    except ImportError:
        report["issues"].append(issue("engine", "Install Python 3 with lxml to run ISO validation."))
        return report
    try:
        bundle = Path(bundle).resolve()
        manifest = json.loads((bundle / "manifest.json").read_text(encoding="utf-8"))
        if manifest["schema_package"] != "1.3.0":
            raise ValueError("The validation bundle must pin schema package 1.3.0.")
        report["quality_schema_package"] = manifest.get("quality_schema_package")
        report["feature_catalogue_schema_package"] = manifest.get("feature_catalogue_schema_package")
        catalog = {}
        for resource in manifest["resources"]:
            local = (bundle / resource["path"]).resolve()
            if not local.is_relative_to(bundle):
                raise ValueError("Validation resource escapes the bundle directory.")
            if hashlib.sha256(local.read_bytes()).hexdigest() != resource["sha256"]:
                raise ValueError(f"Validation resource checksum mismatch: {resource['path']}")
            catalog[resource["url"]] = local
        allowed_paths = set(catalog.values())

        class OfflineResolver(etree.Resolver):
            def resolve(self, url, public_id, context):
                local = catalog.get(url)
                if local is None:
                    # Relative includes are resolved by libxml against the local file.
                    from urllib.parse import unquote, urlparse
                    parsed = urlparse(url)
                    if parsed.scheme in ("", "file"):
                        candidate = Path(unquote(parsed.path)).resolve()
                        if candidate in allowed_paths:
                            local = candidate
                if local is None:
                    raise OSError(f"Unpinned external resource refused: {url}")
                # Canonical local import/include URIs prevent libxml loading
                # the same declaration twice via HTTP, HTTPS and relative URLs.
                # Only locations change in memory; schema declarations do not.
                from urllib.parse import urljoin
                tree = etree.fromstring(local.read_bytes())
                for node in tree.iter():
                    if node.tag in {f"{{{XSD}}}import", f"{{{XSD}}}include", f"{{{XSD}}}redefine"}:
                        ref = node.get("schemaLocation")
                        if ref:
                            target = catalog.get(urljoin(url, ref))
                            if target is None:
                                candidate = (local.parent / ref).resolve()
                                if candidate in allowed_paths:
                                    target = candidate
                            if target is None:
                                raise OSError(f"Unpinned schema dependency refused: {ref}")
                            node.set("schemaLocation", target.as_uri())
                return self.resolve_string(etree.tostring(tree), context, base_url=local.as_uri())

        parser = etree.XMLParser(resolve_entities=False, load_dtd=False, no_network=True)
        parser.resolvers.add(OfflineResolver())
        # A small import wrapper supplies all concrete types used by abstract
        # metadata properties. It does not alter any upstream XSD.
        wrapper = etree.Element(f"{{{XSD}}}schema", nsmap={"xs": XSD})
        for url in manifest["schemas"]:
            schema = etree.parse(str(catalog[url]), parser)
            etree.SubElement(wrapper, f"{{{XSD}}}import",
                             namespace=schema.getroot().get("targetNamespace"),
                             schemaLocation=catalog[url].as_uri())
        schema_tree = etree.fromstring(etree.tostring(wrapper), parser)
        xsd = etree.XMLSchema(schema_tree)
        report["engine"] = dict(python=sys.version.split()[0], lxml=etree.LXML_VERSION,
                                libxml=etree.LIBXML_VERSION)
    except Exception as error:
        report["issues"].append(issue("resources", str(error)))
        return report

    try:
        document = etree.parse(str(path), etree.XMLParser(
            resolve_entities=False, load_dtd=False, no_network=True))
        if document.docinfo.doctype:
            raise ValueError("Metadata XML must not contain a DTD or entity declarations.")
    except (etree.XMLSyntaxError, ValueError, OSError) as error:
        report.update(status="fail")
        report["issues"].append(issue("xml", str(error)))
        return report

    gfc = "https://schemas.isotc211.org/19110/-/gfc/2.2"
    expected = f"{{{gfc}}}FC_FeatureCatalogue" if kind == "catalogue" else f"{{{MDB}}}MD_Metadata"
    if document.getroot().tag != expected:
        report.update(status="fail")
        report["checks"].append(dict(stage="namespace", valid=False))
        report["issues"].append(issue(
            "namespace", "Expected " + expected +
            ". Older Wave exporters use different namespaces; update the serializer, "
            "not the dataset's metadata fields.", location="/"))
        return report

    try:
        xsd_ok = xsd.validate(document)
    except Exception as error:
        report["issues"].append(issue("engine", "XSD validation could not run: " + str(error)))
        return report
    report["checks"].append(dict(stage="xsd", valid=xsd_ok))
    for error in xsd.error_log:
        report["issues"].append(issue("xsd", error.message, error.path or "",
                                       error.type_name, line=error.line))

    engine_failed = False
    report["date_policy_exceptions"] = []
    rules_to_run = manifest["schematron"]
    if kind == "catalogue":
        report["coverage"] = ("ISO 19110 GFC 2.2 XSD, embedded citation rules and DataPond local "
                              "fragment-reference checks. No official GFC Schematron is bundled; "
                              "not semantic completeness, permitted-value verification or factual accuracy.")
        rules_to_run = [url for url in rules_to_run if url.endswith("/cit/1.3.0/cit.sch")]
        ids = document.xpath("//@id")
        links = document.xpath("//@xlink:href", namespaces={"xlink": "http://www.w3.org/1999/xlink"})
        invalid = [ref for ref in links if ref.startswith("#") and ids.count(ref[1:]) != 1]
        for ref in invalid:
            report["issues"].append(issue("catalogue-references", "Unresolved or ambiguous local reference: " + ref))
        root_id = document.getroot().get("id")
        feature_names = []
        for feature in document.findall(f"{{{gfc}}}featureType/{{{gfc}}}FC_FeatureType"):
            feature_names.append(feature.findtext(f"{{{gfc}}}typeName"))
            backlink = feature.find(f"{{{gfc}}}featureCatalogue")
            if backlink is None or not root_id or backlink.get("{http://www.w3.org/1999/xlink}href") != "#" + root_id:
                report["issues"].append(issue("catalogue-references", "Feature type must reference its containing catalogue."))
            for attribute in feature.findall(f"{{{gfc}}}carrierOfCharacteristics/{{{gfc}}}FC_FeatureAttribute"):
                backlink = attribute.find(f"{{{gfc}}}featureType")
                feature_id = feature.get("id")
                if backlink is None or not feature_id or backlink.get("{http://www.w3.org/1999/xlink}href") != "#" + feature_id:
                    report["issues"].append(issue("catalogue-references", "Attribute must reference its containing feature type."))
        if len(feature_names) != len(set(feature_names)):
            report["issues"].append(issue("catalogue-references", "Feature type names must be unique within the catalogue."))
        report["checks"].append(dict(stage="catalogue-references", valid=not any(
            item["stage"] == "catalogue-references" for item in report["issues"])))
    # Nil reasons are legal XML but are visible limitations, never a claim of
    # fully populated metadata. Warnings do not turn structural validity into failure.
    report["warnings"] = [dict(location=document.getpath(node), reason=node.get(f"{{{GCO}}}nilReason"))
                          for node in document.xpath("//*[@gco:nilReason]", namespaces={"gco": GCO})]
    for url in rules_to_run:
        try:
            rules = etree.parse(str(catalog[url]), parser)
            # Two published 1.3.0 files bind gco to a nonexistent 19115 URI.
            # Adapt only the declaration, retaining every rule/test verbatim.
            # The original files and hashes remain untouched and the report
            # always identifies the adaptation (this is not an unmodified run).
            for ns in rules.findall(f"{{{SCH}}}ns"):
                if ns.get("prefix") == "gco" and ns.get("uri") == "https://schemas.isotc211.org/19115/-1/gco/1.3":
                    ns.set("uri", GCO)
                    report["adaptations"].append(dict(source=url, change="Bind gco to " + GCO))
            # The 1.2.0 MDQ rule still declares the older HTTP namespaces.
            # Bind to the namespaces of its paired XSD so the rule actually runs.
            if url.endswith("/mdq/1.2.0/mdq.sch"):
                namespaces = dict(mdq="https://schemas.isotc211.org/19157/-/mdq/1.2",
                                  cit="https://schemas.isotc211.org/19115/-1/cit/1.3",
                                  mcc="https://schemas.isotc211.org/19115/-1/mcc/1.3", gco=GCO)
                for ns in rules.findall(f"{{{SCH}}}ns"):
                    prefix = ns.get("prefix")
                    if prefix in namespaces and ns.get("uri") != namespaces[prefix]:
                        ns.set("uri", namespaces[prefix])
                        report["adaptations"].append(dict(source=url, change="Bind " + prefix + " to " + namespaces[prefix]))
            # mrl.sch describes the LI_Source constraint but its published
            # context names the nonexistent LI_Scope class. Correct only the
            # context so missing source descriptions/scopes are actually tested.
            if url.endswith("/mrl/1.3.0/mrl.sch"):
                for rule in rules.findall(f".//{{{SCH}}}rule"):
                    if rule.get("context") == "//mrl:LI_Scope":
                        rule.set("context", "//mrl:LI_Source")
                        report["adaptations"].append(dict(source=url, change="Apply LI_Source constraint to //mrl:LI_Source"))
            # Upstream uses interleaved diagnostics and multilingual titles;
            # the ISO skeleton accepts these but its optional Relax NG gate
            # does not. Compile all supplied assertions, without that gate.
            validator = isoschematron.Schematron(
                rules, store_report=True, validate_schema=False)
            valid = validator.validate(document)
            report["checks"].append(dict(stage="schematron", source=url, valid=valid,
                assertions=len(rules.findall(f".//{{{SCH}}}assert"))))
            for failure in validator.validation_report.findall(f".//{{{SVRL}}}failed-assert"):
                diagnostics = failure.findall(f"{{{SVRL}}}diagnostic-reference")
                english = [n for n in diagnostics if n.get("diagnostic", "").endswith("-en")]
                nodes = english or diagnostics or failure.findall(f"{{{SVRL}}}text")
                message = " ".join(" ".join("".join(n.itertext()).split()) for n in nodes)
                date_only_creation = failure.get("test") == "$hasAtLeastOneCreationDate" and document.xpath(
                    "/mdb:MD_Metadata/mdb:dateInfo/cit:CI_Date["
                    "cit:dateType/cit:CI_DateTypeCode/@codeListValue='creation']/cit:date/gco:Date",
                    namespaces={"mdb": MDB, "cit": "https://schemas.isotc211.org/19115/-1/cit/1.3", "gco": GCO}
                )
                if date_only_creation:
                    message += (" A creation date is present as gco:Date. The bundled rule requires "
                                "gco:DateTime, although XSD permits gco:Date. The stored precision "
                                "has been preserved; no time has been invented.")
                item = issue("schematron", message or "ISO rule failed.",
                             failure.get("location", ""), failure.get("test", ""), url)
                report["issues"].append(item)
                # Explicit DataPond policy, separate from the pinned result.
                # Only these two creation assertions on the root can differ;
                # an XSD failure, missing date, or any other assertion still fails.
                if (kind == "metadata" and xsd_ok and date_only_creation and
                        url in {f"https://schemas.isotc211.org/19115/-1/mdb/1.3.0/{name}"
                                for name in ("mdb.sch", "metadata-minimal.sch")} and
                        document.xpath(failure.get("location", "")) == [document.getroot()]):
                    report["date_policy_exceptions"].append(item)
        except Exception as error:
            engine_failed = True
            report["checks"].append(dict(stage="schematron", source=url, valid=None))
            report["issues"].append(issue("engine", str(error), source=url))
    report["valid"] = not report["issues"] and not engine_failed
    report["status"] = "unavailable" if engine_failed else ("pass" if report["valid"] else "fail")
    report["schematron_dialect"] = "ISO XSLT skeleton; upstream layout accepted without Relax NG gate"
    return report


def validate(path, bundle, kind="metadata"):
    report = _validate(path, bundle, kind)
    accepted = report.pop("date_policy_exceptions", [])
    remaining = [item for item in report["issues"] if item not in accepted]
    unavailable = report["status"] == "unavailable"
    report["datapond_profile"] = dict(
        policy="DataPond creation-date precision", version="1.0.0",
        valid=not unavailable and not remaining,
        status="unavailable" if unavailable else ("fail" if remaining else "pass"),
        issues=remaining, accepted_deviations=accepted,
        coverage="Same checks as the pinned report, except a schema-valid root metadata "
                 "creation gco:Date satisfies the creation-date assertion. "
                 "This declared compatibility policy is not an upstream ISO rule correction "
                 "or certification; the pinned result remains unchanged.")
    return report


if __name__ == "__main__":
    arguments = argparse.ArgumentParser()
    arguments.add_argument("document")
    arguments.add_argument("--bundle", required=True)
    arguments.add_argument("--kind", choices=("metadata", "catalogue"), default="metadata")
    args = arguments.parse_args()
    result = validate(args.document, args.bundle, args.kind)
    print(json.dumps(result, ensure_ascii=False))
    sys.exit(0 if result["valid"] else (2 if result["status"] == "unavailable" else 1))
