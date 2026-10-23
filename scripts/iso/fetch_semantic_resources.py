#!/usr/bin/env python3
"""Maintainer-only fetch. Runtime validation never contacts these registries."""
import hashlib
import json
from pathlib import Path
from urllib.request import urlopen, Request
from datetime import datetime, timezone

ROOT = Path("inst/extdata/iso_semantics/1.0.0")
SOURCES = {
    "iso-codelists.xml": "https://schemas.isotc211.org/resources/codelists/codelists.xml",
    "character-sets.xml": "https://www.iana.org/assignments/character-sets/character-sets.xml",
    "iso-639-2.txt": "https://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt",
}


def main():
    if (ROOT / "manifest.json").exists():
        raise SystemExit("This resource version is already pinned. Create a new version to refresh it.")
    resources = []
    for name, url in SOURCES.items():
        # LOC's public text endpoint rejects urllib's default user agent.
        with urlopen(Request(url, headers={"User-Agent": "Mozilla/5.0"}), timeout=60) as response:
            data = response.read()
        ROOT.mkdir(parents=True, exist_ok=True)
        (ROOT / name).write_bytes(data)
        resources.append(dict(path=name, url=url, sha256=hashlib.sha256(data).hexdigest()))
    (ROOT / "manifest.json").write_text(json.dumps(dict(
        version="1.0.0", retrieved_at=datetime.now(timezone.utc).isoformat(),
        resources=resources), indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
