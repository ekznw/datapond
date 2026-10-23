#!/usr/bin/env python3
"""Maintainer-only fetch of the pinned ISO 1.3.0 validation dependency closure.

Run from the repository root. Downloads are never performed by the app or tests.
Original upstream bytes are retained; manifest.json pins every SHA-256 digest.
"""
import concurrent.futures
import argparse
import hashlib
import json
from pathlib import Path
from urllib.parse import urljoin, urlparse
from urllib.request import urlopen
import xml.etree.ElementTree as ET

ROOT = Path("inst/extdata/iso_validation/1.3.0")
BASE = "https://schemas.isotc211.org/19115/-1/"
MODULES = "cit gex lan mas mcc mco mda mdb mex mmi mpc mrc mrd mri mrl mrs msr srv".split()
RULE_MODULES = "cit gex mco mdb mex mmi mrc mrd mri mrl mrs srv".split()
SCHEMAS = [f"{BASE}{m}/1.3.0/{m}.xsd" for m in MODULES]
RULES = [f"{BASE}{m}/1.3.0/{m}.sch" for m in RULE_MODULES]
RULES += [f"{BASE}mdb/1.3.0/metadata-minimal.sch"]
EXAMPLES = [f"{BASE}mdb/1.3.0/examples/D.1Minimal.xml"]
HOSTS = {"schemas.isotc211.org", "schemas.opengis.net", "www.w3.org"}
PINNED_PATHS = {}


def fetch(url):
    parsed = urlparse(url)
    if parsed.hostname not in HOSTS or parsed.query or parsed.fragment or ".." in parsed.path.split("/"):
        raise ValueError(f"Unexpected upstream dependency: {url}")
    # Some official imports still use HTTP. Fetch securely without changing bytes.
    download_url = parsed._replace(scheme="https").geturl()
    relative = Path("upstream") / parsed.hostname / parsed.path.lstrip("/")
    destination = ROOT / relative
    if relative.as_posix() in PINNED_PATHS:
        data = destination.read_bytes()
        if hashlib.sha256(data).hexdigest() != PINNED_PATHS[relative.as_posix()]:
            raise ValueError(f"Existing resource checksum mismatch: {relative}")
    else:
        with urlopen(download_url, timeout=30) as response:
            data = response.read()
    try:
        tree = ET.fromstring(data)
    except ET.ParseError as error:
        raise ValueError(f"Invalid upstream XML at {url}: {error}") from error
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_bytes(data)
    dependencies = set()
    for node in tree.iter():
        if node.tag in {
            "{http://www.w3.org/2001/XMLSchema}import",
            "{http://www.w3.org/2001/XMLSchema}include",
            "{http://www.w3.org/2001/XMLSchema}redefine",
        }:
            ref = node.get("schemaLocation")
        elif node.tag == "{http://purl.oclc.org/dsdl/schematron}include":
            ref = node.get("href")
        else:
            continue
        if ref:
            dependencies.add(urljoin(url, ref))
    return {"url": url, "path": relative.as_posix(),
            "sha256": hashlib.sha256(data).hexdigest()}, dependencies


def main():
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--add-quality", action="store_true",
                           help="Extend the existing bundle with ISO 19157 1.2.0; preserve all pinned bytes")
    arguments.add_argument("--add-content", action="store_true", help="Add the pinned GFC 2.2 root, preserving existing resources")
    args = arguments.parse_args()
    schemas, rules = list(SCHEMAS), list(RULES)
    records = {}
    existing = {}
    if args.add_quality or args.add_content:
        existing = json.loads((ROOT / "manifest.json").read_text(encoding="utf-8"))
        for record in existing["resources"]:
            data = (ROOT / record["path"]).read_bytes()
            if hashlib.sha256(data).hexdigest() != record["sha256"]:
                raise ValueError(f"Existing resource checksum mismatch: {record['path']}")
            records[record["url"]] = record
            PINNED_PATHS[record["path"]] = record["sha256"]
        schemas = list(existing["schemas"])
        rules = list(existing["schematron"])
    if args.add_quality:
        for module in ("mdq",):
            base = f"https://schemas.isotc211.org/19157/-/{module}/1.2.0/{module}"
            if base + ".xsd" not in schemas:
                schemas.append(base + ".xsd")
            if base + ".sch" not in rules:
                rules.append(base + ".sch")
    if args.add_content:
        gfc = "https://schemas.isotc211.org/19110/-/gfc/2.2.0/gfc.xsd"
        if gfc not in schemas:
            schemas.append(gfc)
    pending = set(schemas + rules + EXAMPLES)
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
        while pending:
            current = sorted(pending - records.keys())
            if not current:
                break
            pending = set()
            for record, dependencies in pool.map(fetch, current):
                records[record["url"]] = record
                pending.update(dependencies - records.keys())
            print(f"Pinned {len(records)} resources; {len(pending)} dependencies pending", flush=True)
    manifest = {
        "schema_package": "1.3.0",
        "source_index": BASE,
        "schemas": schemas,
        "schematron": rules,
        "examples": EXAMPLES,
        "resources": [records[url] for url in sorted(records)],
    }
    for key in ("quality_schema_package", "quality_scope", "feature_catalogue_schema_package"):
        if key in existing:
            manifest[key] = existing[key]
    if args.add_content:
        manifest["feature_catalogue_schema_package"] = "ISO 19110 GFC 2.2.0"
    if args.add_quality:
        manifest["quality_schema_package"] = "ISO 19157 1.2.0"
        manifest["quality_scope"] = "MDQ result schemas and measure-reference rule; standalone DQM measure catalogues are not assessed."
    (ROOT / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
