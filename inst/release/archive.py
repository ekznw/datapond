#!/usr/bin/env python3
"""Release transport: only stdlib; safe ZIP creation, verification and extraction."""
import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import stat
import zipfile


def digest(stream):
    h = hashlib.sha256()
    while True:
        chunk = stream.read(1024 * 1024)
        if not chunk:
            return h.hexdigest()
        h.update(chunk)


def safe_name(name):
    p = PurePosixPath(name)
    if (not name or p.is_absolute() or "\\" in name or ":" in name
            or any(x in ("", ".", "..") for x in name.split("/"))
            or any(ord(c) < 32 for c in name)):
        raise ValueError("Unsafe release path: " + repr(name))
    return name


def inventory(root):
    root = Path(root)
    if root.is_symlink() or not root.is_dir():
        raise ValueError("Dataset must be a real directory")
    files, names = [], set()
    for parent, dirs, leaves in os.walk(root, followlinks=False):
        for name in dirs + leaves:
            path = Path(parent) / name
            rel = safe_name(path.relative_to(root).as_posix())
            if path.is_symlink():
                raise ValueError("Symbolic links cannot be frozen: " + rel)
            if rel.casefold() in names:
                raise ValueError("Paths differ only by case: " + rel)
            names.add(rel.casefold())
            if path.is_file():
                with path.open("rb") as f:
                    value = digest(f)
                files.append(dict(path=rel, size=path.stat().st_size, sha256=value))
            elif not path.is_dir():
                raise ValueError("Unsupported file type: " + rel)
    return sorted(files, key=lambda x: x["path"])


def check_archive(archive, manifest, output=None):
    expected = {safe_name(f["path"]): f for f in manifest}
    if len(expected) != len(manifest) or len({p.casefold() for p in expected}) != len(expected):
        raise ValueError("Duplicate manifest paths")
    for row in manifest:
        if not isinstance(row["size"], int) or row["size"] < 0 or len(row["sha256"]) != 64:
            raise ValueError("Invalid file manifest")
    with zipfile.ZipFile(archive) as z:
        entries = z.infolist()
        names = [safe_name(i.filename) for i in entries]
        if len(names) != len(set(names)) or set(names) != set(expected):
            raise ValueError("ZIP members do not match the manifest")
        for i in entries:
            mode = i.external_attr >> 16
            if i.is_dir() or stat.S_ISLNK(mode) or (stat.S_IFMT(mode) not in (0, stat.S_IFREG)):
                raise ValueError("Unsupported ZIP member: " + i.filename)
            if i.file_size != expected[i.filename]["size"]:
                raise ValueError("ZIP size mismatch: " + i.filename)
        if output:
            output = Path(output)
            if output.exists():
                raise ValueError("Extraction requires a new directory")
            if shutil.disk_usage(output.parent).free < sum(i.file_size for i in entries) + 16 * 1024**2:
                raise ValueError("Insufficient free space for extraction")
            output.mkdir()
        try:
            for i in entries:
                with z.open(i) as src:
                    if output:
                        dest = output / i.filename
                        dest.parent.mkdir(parents=True, exist_ok=True)
                        with dest.open("xb") as dst:
                            h = hashlib.sha256()
                            while True:
                                chunk = src.read(1024 * 1024)
                                if not chunk:
                                    break
                                h.update(chunk)
                                dst.write(chunk)
                            actual = h.hexdigest()
                    else:
                        actual = digest(src)
                if actual != expected[i.filename]["sha256"]:
                    raise ValueError("Checksum mismatch: " + i.filename)
        except BaseException:
            if output and output.exists():
                shutil.rmtree(output)
            raise


def main():
    p = argparse.ArgumentParser()
    p.add_argument("action", choices=["inventory", "space", "pack", "verify", "extract"])
    p.add_argument("source")
    p.add_argument("manifest")
    p.add_argument("--output")
    args = p.parse_args()
    if args.action == "inventory":
        Path(args.manifest).write_text(json.dumps(inventory(args.source), indent=2), encoding="utf-8")
        return
    manifest = json.loads(Path(args.manifest).read_text(encoding="utf-8"))
    if args.action == "space":
        if shutil.disk_usage(args.source).free < 2 * sum(f["size"] for f in manifest) + 32 * 1024**2:
            raise ValueError("Insufficient free space for staging and the release ZIP")
        return
    if args.action == "pack":
        if inventory(args.source) != manifest:
            raise ValueError("Files changed before packaging")
        if Path(args.output).exists():
            raise ValueError("An existing archive cannot be overwritten")
        with zipfile.ZipFile(args.output, "x", compression=zipfile.ZIP_DEFLATED,
                             compresslevel=6, allowZip64=True) as z:
            for row in manifest:
                z.write(Path(args.source) / row["path"], row["path"])
        check_archive(args.output, manifest)
    else:
        check_archive(args.source, manifest, args.output if args.action == "extract" else None)


if __name__ == "__main__":
    main()
