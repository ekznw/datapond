#!/usr/bin/env python3
"""Audit saved legacy fields and migration artefacts without opening the app.

Usage: python3 scripts/audit_legacy_data.py --output REPORT.json POND ...
No initialisation, migration, seeding or dataset-file writes are performed.
"""
import argparse
from datetime import datetime, timezone
import json
from pathlib import Path
import sqlite3

from iso.audit_pond_rights import audit as audit_rights, hashes


LEGACY_FIELDS = {
    "entry_tbl": [
        "keyword_topic", "bbox", "epsg_code", "software_desc", "cell_size",
        "scale_factor", "license_type", "access_constraints", "use_limitations",
        "data_distribution_url", "data_distributor", "data_distributor_other",
        "temporal_coverage_start_date", "temporal_coverage_end_date",
    ],
    "dataset_lineage_tbl": [
        "source_description", "source_datasets_text", "process_description",
        "processing_tools", "processing_scripts", "output_description", "lineage_notes",
    ],
}


def quote(identifier):
    return '"' + identifier.replace('"', '""') + '"'


def audit(pond):
    root = Path(pond).expanduser().resolve()
    database = root / "metadata.db"
    before = hashes(database)
    conn = sqlite3.connect(database.as_uri() + "?mode=ro", uri=True)
    conn.row_factory = sqlite3.Row
    try:
        conn.execute("PRAGMA query_only=ON")
        conn.execute("BEGIN")
        tables = [r[0] for r in conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")]
        columns = {t: [r[1] for r in conn.execute("PRAGMA table_info(" + quote(t) + ")")]
                   for t in tables}
        counts = {t: conn.execute("SELECT COUNT(*) FROM " + quote(t)).fetchone()[0]
                  for t in tables}
        legacy = {}
        for table, fields in LEGACY_FIELDS.items():
            key = "id" if table == "entry_tbl" else "dataset_id"
            legacy[table] = {}
            for field in fields:
                ids = None
                if field in columns.get(table, []):
                    ids = [r[0] for r in conn.execute(
                        f"SELECT {quote(key)} FROM {quote(table)} "
                        f"WHERE TRIM(COALESCE({quote(field)}, '')) != '' ORDER BY {quote(key)}")]
                legacy[table][field] = {"column_exists": ids is not None,
                                        "dataset_ids": ids, "populated": len(ids or [])}
        marker = "legacy_lineage_migrated_at"
        has_marker = marker in columns.get("dataset_lineage_tbl", [])
        pending = [r[0] for r in conn.execute(
            "SELECT dataset_id FROM dataset_lineage_tbl" +
            (f" WHERE {marker} IS NULL" if has_marker else "") + " ORDER BY dataset_id")]
        origins = {}
        for table in ("dataset_lineage_source_tbl", "dataset_lineage_process_step_tbl"):
            if "value_origin_code" in columns.get(table, []):
                origins[table] = [dict(r) for r in conn.execute(
                    f"SELECT value_origin_code, is_provisional, COUNT(*) AS count FROM {table} "
                    "GROUP BY value_origin_code, is_provisional")]
        foreign_keys = [dict(r) for r in conn.execute("PRAGMA foreign_key_check")]
        config_keys = [dict(r) for r in conn.execute(
            "SELECT config_scope, config_key FROM app_config_tbl ORDER BY id")]
        conn.rollback()
    finally:
        conn.close()
    artefacts = []
    state = root / ".metadata_pond"
    for directory in (state / "imports", state / "intake_backups"):
        for path in sorted(directory.rglob("*")):
            if path.is_file():
                artefacts.append({"path": str(path.relative_to(root)), "bytes": path.stat().st_size})
    for path in sorted(state.glob("*intake-result.json")):
        artefacts.append({"path": str(path.relative_to(root)), "bytes": path.stat().st_size})
    rights = audit_rights(root)
    after = hashes(database)
    return {
        "pond": str(root), "database_sha256_before": before,
        "database_sha256_after": after, "database_files_unchanged": before == after,
        "table_counts": counts, "legacy_fields": legacy,
        "lineage_marker_column_exists": has_marker, "lineage_pending_dataset_ids": pending,
        "lineage_origins": origins, "foreign_key_violations": foreign_keys,
        "configuration_keys": config_keys,
        "rights_flag_counts": rights["flag_counts"],
        "rights_flags_by_dataset": {str(r["dataset_id"]): r["flags"] for r in rights["records"]},
        "migration_artefacts": artefacts,
        "migration_artefacts_bytes": sum(r["bytes"] for r in artefacts),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ponds", nargs="+")
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    output = args.output.expanduser().resolve()
    for pond in args.ponds:
        root = Path(pond).expanduser().resolve()
        if output == root or root in output.parents:
            parser.error("Write the audit report outside the audited ponds.")
    report = {
        "audited_at_utc": datetime.now(timezone.utc).isoformat(),
        "method": "SQLite mode=ro, query_only and read transactions; no app initialisation. "
            "Database and WAL hashes compared before and after. Rights read in a separate transaction. "
            "Migration artefacts inventoried by path and size, not executed or deleted.",
        "ponds": [audit(p) for p in args.ponds],
    }
    with output.open("x", encoding="utf-8") as stream:
        json.dump(report, stream, indent=2, ensure_ascii=False)
        stream.write("\n")
    for pond in report["ponds"]:
        print(pond["pond"], pond["table_counts"]["entry_tbl"], "records; unchanged:",
              pond["database_files_unchanged"], "; artefact bytes:", pond["migration_artefacts_bytes"])


if __name__ == "__main__":
    main()
