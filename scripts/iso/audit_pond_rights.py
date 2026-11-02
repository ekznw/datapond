#!/usr/bin/env python3
"""Read saved rights without running DataPond migrations or changing either pond.

Flags describe text overlap or incomplete recorded evidence, not legal conclusions.
Usage: python3 scripts/iso/audit_pond_rights.py --output /tmp/rights.json POND ...
"""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import sqlite3


def normalise(value):
    return re.sub(r"\s+", " ", str(value or "")).strip()


def hashes(database):
    result = {}
    for path in (database, Path(str(database) + "-wal")):
        if path.exists():
            digest = hashlib.sha256()
            with path.open("rb") as stream:
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    digest.update(chunk)
            result[path.name] = digest.hexdigest()
    return result


def audit(pond):
    database = (Path(pond).expanduser().resolve() / "metadata.db")
    before = hashes(database)
    conn = sqlite3.connect(database.as_uri() + "?mode=ro", uri=True)
    conn.row_factory = sqlite3.Row
    try:
        conn.execute("PRAGMA query_only=ON")
        conn.execute("BEGIN")
        entries = [dict(row) for row in conn.execute("SELECT * FROM entry_tbl ORDER BY id")]
        rights = {row["dataset_id"]: dict(row) for row in conn.execute(
            "SELECT * FROM dataset_license_tbl ORDER BY dataset_id")}
        conn.rollback()
    finally:
        conn.close()
    records = []
    for entry in entries:
        row = rights.get(entry["id"], {})
        access = normalise(entry.get("access_constraints"))
        use = normalise(entry.get("use_limitations"))
        structured = normalise(row.get("use_constraints"))
        flags = []
        conditions = {
            "missing_rights_row": not row,
            "access_note": bool(access),
            "use_note": bool(use),
            "structured_use_note": bool(structured),
            "exact_use_duplicate": bool(use and use == structured),
            "use_contained_in_structured": bool(use and use != structured and use in structured),
            "structured_contained_in_use": bool(structured and use != structured and structured in use),
            "access_contained_in_structured": bool(access and access in structured),
            "structured_is_access_plus_use": bool(access and use and structured == access + " " + use),
            "rights_basis_missing": not normalise(row.get("rights_basis_code")),
            "rights_basis_needs_review": row.get("rights_basis_code") in ("review_required", "rights_unknown"),
            "redistribution_unknown": row.get("redistribution_permission_code") == "unknown",
            "provider_reference_missing": row.get("license_identifier") == "PROVIDER-LICENCE"
                and not normalise(row.get("license_url")) and not normalise(row.get("permission_reference")),
            "attribution_text_missing": row.get("required_attribution") == 1
                and not normalise(row.get("attribution_text")),
        }
        for flag, matched in conditions.items():
            if matched:
                flags.append(flag)
        embargo_differences = []
        for field in ("embargo_status", "embargo_reason", "embargo_end_date"):
            a, b = normalise(entry.get(field)), normalise(row.get(field))
            if a and b and a != b:
                embargo_differences.append(field)
        if embargo_differences:
            flags.append("embargo_disagreement")
        records.append({
            "dataset_id": entry["id"], "uuid": entry.get("uuid"),
            "title": entry.get("title"), "dataset_path": entry.get("dataset_path"),
            "hidden": entry.get("is_hidden"), "deleted_at": entry.get("deleted_at"),
            "entry_notes": {key: entry.get(key) for key in (
                "license_type", "access_constraints", "use_limitations",
                "embargo_status", "embargo_reason", "embargo_end_date")},
            "saved_rights": row, "flags": flags,
            "embargo_disagreements": embargo_differences,
        })
    after = hashes(database)
    return {
        "pond": str(database.parent), "database_sha256_before": before,
        "database_sha256_after": after, "database_files_unchanged": before == after,
        "dataset_count": len(entries), "rights_row_count": len(rights),
        "orphan_rights_dataset_ids": sorted(set(rights) - {e["id"] for e in entries}),
        "flag_counts": dict(sorted(Counter(f for r in records for f in r["flags"]).items())),
        "code_counts": {field: dict(Counter(str(row.get(field) or "(blank)")
            for row in rights.values())) for field in (
                "license_identifier", "rights_basis_code", "access_level_code", "redistribution_permission_code")},
        "records": records,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ponds", nargs="+")
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    # Reports must never overwrite an operational database or be written in a pond.
    output = args.output.expanduser().resolve()
    for pond in args.ponds:
        root = Path(pond).expanduser().resolve()
        if output == root or root in output.parents:
            parser.error("Write the audit report outside the audited ponds.")
    report = {
        "audited_at_utc": datetime.now(timezone.utc).isoformat(),
        "method": "SQLite mode=ro and query_only; one read transaction per pond; no migrations or writes. "
            "Case-sensitive whitespace-normalised text comparisons; substring overlap is a review flag, "
            "not permission to delete. All saved records included. Source documents not adjudicated.",
        "ponds": [audit(pond) for pond in args.ponds],
    }
    # Refuse to overwrite an earlier audit artefact.
    with output.open("x", encoding="utf-8") as stream:
        json.dump(report, stream, ensure_ascii=False, indent=2)
        stream.write("\n")
    for pond in report["ponds"]:
        print(pond["pond"], pond["dataset_count"], "records; unchanged:", pond["database_files_unchanged"])
        print(json.dumps(pond["flag_counts"], sort_keys=True))


if __name__ == "__main__":
    main()
