"""Scientific-text regression tests using real template ZIPs and Word runs."""
import importlib.util
from pathlib import Path
import sys
import tempfile
import unittest
from xml.dom import minidom
import zipfile

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('sync', ROOT / 'inst/templates/sync_data_dev.py')
sync = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sync)
W = sync.W
M = sync.M
VALUES = {'title': 'Scientific text fixture', 'version': '1.2.3',
          'abstract': 'Initial abstract', 'purpose': 'Initial purpose', 'uuid': ''}


def replace_cell(path, field, xml, styles=None):
    """Change a real report as an author would in Word; keep other ZIP members."""
    with zipfile.ZipFile(path) as archive:
        members = [(info, archive.read(info.filename)) for info in archive.infolist()]
    document = minidom.parseString(dict((i.filename, b) for i, b in members)['word/document.xml'])
    block = next(s for s in sync.descendants(document, 'sdt')
                 if any(t.getAttributeNS(W, 'val') == 'datapond.metadata' for t in sync.descendants(s, 'tag')))
    row = next(r for r in sync.descendants(block, 'tr')
               if sync.WordText().content(sync.descendants(r, 'tc')[0]) == sync.FIELDS[field])
    cell = sync.descendants(row, 'tc')[1]
    for node in list(cell.childNodes):
        if not (node.nodeType == node.ELEMENT_NODE and node.localName == 'tcPr'):
            cell.removeChild(node)
    wrapper = minidom.parseString(f'<root xmlns:w="{W}" xmlns:m="{M}">{xml}</root>')
    for node in list(wrapper.documentElement.childNodes):
        cell.appendChild(document.importNode(node, True))
    with zipfile.ZipFile(path, 'w') as archive:
        for info, data in members:
            if info.filename == 'word/document.xml':
                data = document.toxml(encoding='utf-8')
            elif info.filename == 'word/styles.xml' and styles is not None:
                data = styles.encode()
            archive.writestr(info, data)


def scientific_fixture(path):
    replace_cell(path, 'abstract', '''<w:p>
      <w:r><w:t>1.25 × 10</w:t></w:r>
      <w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>−6</w:t></w:r>
      <w:r><w:t xml:space="preserve">; CO</w:t></w:r>
      <w:r><w:rPr><w:vertAlign w:val="subscript"/></w:rPr><w:t>2</w:t></w:r>
      <w:r><w:t xml:space="preserve">; x</w:t></w:r>
      <w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>β</w:t></w:r>
    </w:p>''')


class ScientificTextTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name) / 'data_dev.docx'
        sync.word(ROOT / 'inst/extdata/data_dev_template/data_dev.docx', self.path, VALUES)

    def inspect(self):
        warnings = []
        values = sync.word(self.path, warnings=warnings)
        return values, warnings

    def test_scientific_notation_unicode_and_split_runs(self):
        replace_cell(self.path, 'abstract', '''<w:p>
          <w:r><w:t>1.25e-6; 1.25 × 10</w:t></w:r>
          <w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>−</w:t></w:r>
          <w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>6</w:t></w:r>
          <w:r><w:t>; CO</w:t></w:r>
          <w:r><w:rPr><w:vertAlign w:val="subscript"/></w:rPr><w:t>2</w:t></w:r>
          <w:r><w:t>; m</w:t></w:r>
          <w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>2</w:t></w:r>
          <w:r><w:br/><w:t>H₂O; 10⁻⁶</w:t></w:r></w:p>''')
        before = self.path.read_bytes()
        values, warnings = self.inspect()
        self.assertEqual(values['abstract'], '1.25e-6; 1.25 × 10⁻⁶; CO₂; m²\nH₂O; 10⁻⁶')
        self.assertEqual(warnings, [])
        self.assertEqual(self.path.read_bytes(), before)
        output = self.path.with_name('roundtrip.docx')
        sync.word(self.path, output, values)
        self.assertEqual(sync.word(output), values)

    def test_character_and_paragraph_style_inheritance_and_baseline_override(self):
        styles = f'''<w:styles xmlns:w="{W}">
          <w:style w:styleId="Exponent"><w:rPr><w:vertAlign w:val="superscript"/></w:rPr></w:style>
          <w:style w:styleId="Derived"><w:basedOn w:val="Exponent"/></w:style>
          <w:style w:styleId="Chemical"><w:rPr><w:vertAlign w:val="subscript"/></w:rPr></w:style>
        </w:styles>'''
        replace_cell(self.path, 'abstract', '''<w:p><w:pPr><w:pStyle w:val="Chemical"/></w:pPr>
          <w:r><w:rPr><w:vertAlign w:val="baseline"/></w:rPr><w:t>CO</w:t></w:r>
          <w:r><w:t>2</w:t></w:r>
          <w:r><w:rPr><w:vertAlign w:val="baseline"/></w:rPr><w:t>; 10</w:t></w:r>
          <w:r><w:rPr><w:rStyle w:val="Derived"/></w:rPr><w:t>-6</w:t></w:r>
        </w:p>''', styles)
        values, warnings = self.inspect()
        self.assertEqual(values['abstract'], 'CO₂; 10⁻⁶')
        self.assertEqual(warnings, [])

    def test_unrepresentable_spans_are_explicit_not_flattened(self):
        scientific_fixture(self.path)
        values, warnings = self.inspect()
        self.assertEqual(values['abstract'], '1.25 × 10⁻⁶; CO₂; x^{β}')
        self.assertEqual(len(warnings), 1)
        self.assertEqual(warnings[0]['field'], 'abstract')
        self.assertIn('Unicode equivalent', warnings[0]['message'])
        replace_cell(self.path, 'purpose', '''<w:p>
          <w:r><w:rPr><w:vertAlign w:val="subscript"/></w:rPr><w:t>Qβ</w:t></w:r>
        </w:p>''')
        self.assertEqual(self.inspect()[0]['purpose'], '_{Qβ}')

    def test_equations_and_objects_are_visible_even_without_any_plain_text(self):
        replace_cell(self.path, 'abstract', '''<w:p><m:oMath><m:f><m:num><m:r><m:t>a</m:t></m:r></m:num>
          <m:den><m:r><m:t>b</m:t></m:r></m:den></m:f></m:oMath><w:r><w:object/></w:r></w:p>''')
        values, warnings = self.inspect()
        self.assertIn('[Word equation', values['abstract'])
        self.assertIn('[Word object', values['abstract'])
        self.assertEqual(len(warnings), 2)
        self.assertNotIn('ab', values['abstract'])

    def test_manual_position_and_emphasis_and_tracked_changes_warn(self):
        replace_cell(self.path, 'abstract', '''<w:p>
          <w:r><w:rPr><w:i/></w:rPr><w:t>Species</w:t></w:r>
          <w:r><w:rPr><w:position w:val="6"/></w:rPr><w:t>2</w:t></w:r>
          <w:del><w:r><w:delText>old</w:delText></w:r></w:del>
          <w:ins><w:r><w:t>new</w:t></w:r></w:ins></w:p>''')
        values, warnings = self.inspect()
        self.assertEqual(values['abstract'], 'Species[raised: 2]new')
        self.assertTrue(any('Character styling' in w['message'] for w in warnings))
        self.assertTrue(any('Manually' in w['message'] for w in warnings))
        self.assertTrue(any('Tracked changes' in w['message'] for w in warnings))

    def test_export_preserves_unselected_rich_cells(self):
        scientific_fixture(self.path)
        replace_cell(self.path, 'purpose', '<w:p><m:oMath><m:r><m:t>x</m:t></m:r></m:oMath></w:p>')
        output = self.path.with_name('selected.docx')
        sync.word(self.path, output, {'title': 'New title'})
        with zipfile.ZipFile(output) as archive:
            doc = minidom.parseString(archive.read('word/document.xml'))
        self.assertEqual(len(doc.getElementsByTagNameNS(M, 'oMath')), 1)
        self.assertEqual(len(sync.descendants(doc, 'vertAlign')), 3)
        self.assertEqual(sync.word(output)['abstract'], '1.25 × 10⁻⁶; CO₂; x^{β}')

    def test_symbol_fonts_do_not_silently_become_latin_letters(self):
        replace_cell(self.path, 'abstract', '''<w:p><w:r>
          <w:rPr><w:rFonts w:ascii="Symbol" w:hAnsi="Symbol"/></w:rPr>
          <w:t>a</w:t></w:r></w:p>''')
        values, warnings = self.inspect()
        self.assertEqual(values['abstract'], '[Symbol-font text: a]')
        self.assertIn('different glyphs', warnings[0]['message'])

    def test_qmd_math_warns_but_is_not_evaluated_or_changed(self):
        warnings = []
        values = {'title': 'CO₂ and m²', 'abstract': r'$10^{-6}$; H~2~O; <sup>2</sup>',
                  'purpose': '1.25e-6'}
        sync.qmd_warnings(values, warnings)
        self.assertEqual([w['field'] for w in warnings], ['abstract'])
        self.assertIn('literal source text', warnings[0]['message'])
        path = self.path.with_suffix('.qmd')
        path.write_text(sync.qmd(ROOT / 'inst/extdata/data_dev_template/data_dev.qmd', dict(VALUES, **values)))
        self.assertEqual(sync.qmd(path)['abstract'], values['abstract'])


if __name__ == '__main__':
    if len(sys.argv) == 3 and sys.argv[1] == 'fixture':
        scientific_fixture(Path(sys.argv[2]))
    else:
        unittest.main()
