"""Selective extraction must retain the full archive's structural checks."""
import hashlib
import importlib.util
from pathlib import Path
import stat
import tempfile
import unittest
import zipfile

spec = importlib.util.spec_from_file_location(
    "release_archive", Path(__file__).resolve().parents[2] / "inst/release/archive.py")
archive = importlib.util.module_from_spec(spec)
spec.loader.exec_module(archive)


class SelectedExtractionTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.zip = self.root / "dataset.zip"
        self.files = {"dataset/a.csv": b"a\n", "dataset/b.csv": b"b\n"}
        self.manifest = [dict(path=p, size=len(value), sha256=hashlib.sha256(value).hexdigest())
                         for p, value in self.files.items()]
        with zipfile.ZipFile(self.zip, "w") as z:
            for p, value in self.files.items():
                z.writestr(p, value)

    def test_only_selected_members_are_materialised(self):
        output = self.root / "payload"
        archive.check_archive(self.zip, self.manifest, output, ["dataset/a.csv"])
        self.assertEqual((output / "dataset/a.csv").read_bytes(), b"a\n")
        self.assertFalse((output / "dataset/b.csv").exists())
        archive.check_archive(self.zip, self.manifest)  # Full verification unchanged.

    def test_invalid_selections_leave_no_output(self):
        for selection in ([], ["../outside"], ["dataset/a.csv"] * 2, ["missing"]):
            output = self.root / "payload"
            with self.assertRaises(ValueError):
                archive.check_archive(self.zip, self.manifest, output, selection)
            self.assertFalse(output.exists())

    def test_unselected_unsafe_members_are_rejected(self):
        with zipfile.ZipFile(self.zip, "a") as z:
            z.writestr("../outside", b"bad")
        with self.assertRaisesRegex(ValueError, "Unsafe"):
            archive.check_archive(self.zip, self.manifest, self.root / "payload", ["dataset/a.csv"])
        self.assertFalse((self.root / "payload").exists())

    def test_symlinks_are_rejected_even_when_not_selected(self):
        with zipfile.ZipFile(self.zip, "w") as z:
            z.writestr("dataset/a.csv", b"a\n")
            info = zipfile.ZipInfo("dataset/b.csv")
            info.create_system = 3
            info.external_attr = (stat.S_IFLNK | 0o777) << 16
            z.writestr(info, b"b\n")
        with self.assertRaisesRegex(ValueError, "Unsupported"):
            archive.check_archive(self.zip, self.manifest, self.root / "payload", ["dataset/a.csv"])

    def test_checksum_failure_removes_partial_output(self):
        self.manifest[0]["sha256"] = "0" * 64
        output = self.root / "payload"
        with self.assertRaisesRegex(ValueError, "Checksum mismatch"):
            archive.check_archive(self.zip, self.manifest, output, ["dataset/a.csv"])
        self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
