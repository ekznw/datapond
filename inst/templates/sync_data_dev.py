"""Round-trip an explicit metadata section without rendering or executing a report.

Python standard library only. Word ZIP members other than document.xml are copied
unchanged; no macros, fields, links or Quarto chunks are evaluated.
"""
import json
import re
import sys
import zipfile
from pathlib import Path
from xml.dom import minidom

FIELDS = {'title': 'Dataset title', 'version': 'Dataset version',
          'abstract': 'Abstract', 'purpose': 'Purpose', 'uuid': 'Dataset UUID'}
START = '<!-- datapond-metadata:start -->'
END = '<!-- datapond-metadata:end -->'
W = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'


def qmd(path, values=None):
    text = Path(path).read_text(encoding='utf-8')
    if text.count(START) != text.count(END) or text.count(START) > 1:
        raise ValueError('The DataPond metadata section is damaged or duplicated.')
    match = re.search(re.escape(START) + r'.*?' + re.escape(END), text, re.S)
    if START in text and match is None:
        raise ValueError('The DataPond metadata section markers are out of order.')
    found = {}
    if match:
        for key, label in FIELDS.items():
            part = re.search(r'<!-- datapond:' + key + r' -->\n(.*?)\n<!-- /datapond:' + key + r' -->', match[0], re.S)
            if not part:
                raise ValueError('Missing metadata field marker: ' + key)
            found[key] = part[1]
    if values is None:
        return found
    found.update(values)
    for value in found.values():
        if '<!--' in value or '-->' in value:
            raise ValueError('Metadata cannot contain HTML comment markers.')
    block = START + '\n\n## Dataset metadata {.unnumbered}\n\n'
    block += 'Edit the values in this section to import them into DataPond. Keep the field markers.\n\n'
    for key, label in FIELDS.items():
        block += f'**{label}**\n\n<!-- datapond:{key} -->\n{found.get(key, "")}\n<!-- /datapond:{key} -->\n\n'
    block += END
    if match:
        text = text[:match.start()] + block + text[match.end():]
    else:
        front = re.match(r'\A---\r?\n.*?\r?\n---(?:\r?\n|$)', text, re.S)
        if not front:
            raise ValueError('The Quarto report needs YAML front matter.')
        # Give metadata its own page without pushing the cover's page break
        # onto an otherwise empty page in Word/PDF output.
        pagebreak = '{{< pagebreak >}}'
        pos = text.find(pagebreak, front.end())
        if pos >= 0:
            pos += len(pagebreak)
            text = text[:pos] + '\n\n' + block + '\n\n' + pagebreak + text[pos:]
        else:
            text += '\n\n' + pagebreak + '\n\n' + block + '\n'
    if 'title' in values:
        front = re.match(r'\A---\r?\n.*?\r?\n---', text, re.S)
        if not front:
            raise ValueError('The Quarto report needs YAML front matter.')
        # Replace the complete YAML title value, including block scalar lines.
        header, count = re.subn(r'^title:[^\n]*(?:\n[ \t]+[^\n]*)*',
                                lambda _: 'title: ' + json.dumps(values['title'], ensure_ascii=False),
                                front[0], count=1, flags=re.M)
        if not count:
            header = header[:4] + 'title: ' + json.dumps(values['title']) + '\n' + header[4:]
        text = header + text[front.end():]
        text = re.sub(r'^\*\*Dataset or project title:\*\*.*$',
                      lambda _: '**Dataset or project title:** ' + values['title'].replace('\n', ' '), text, flags=re.M)
    if 'uuid' in found or 'version' in found:
        text = re.sub(r'^\*\*Dataset UUID and version:\*\*.*$',
                      lambda _: '**Dataset UUID and version:** ' + found.get('uuid', '') + ' / ' + found.get('version', ''), text, flags=re.M)
    return text


def descendants(node, tag):
    return node.getElementsByTagNameNS(W, tag)


# Only use characters with established Unicode superscript/subscript forms.
# A whole unsupported span uses explicit ^{...}/_{...} notation, never baseline
# digits that could change an exponent or chemical formula's meaning.
SUPERSCRIPT = dict(zip('0123456789+-=()in', '⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁱⁿ'))
SUBSCRIPT = dict(zip('0123456789+-=()aehijklmnoprstuvx', '₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓ'))
for mapping in (SUPERSCRIPT, SUBSCRIPT):
    mapping['−'] = mapping['-']
    mapping.update({v: v for v in list(mapping.values())})
M = 'http://schemas.openxmlformats.org/officeDocument/2006/math'


def child(node, tag):
    return next((n for n in node.childNodes if n.nodeType == n.ELEMENT_NODE
                 and n.namespaceURI == W and n.localName == tag), None)


def properties(node):
    if node is None:
        return {}
    result = {n.localName: n.getAttributeNS(W, 'val') for n in node.childNodes
              if n.nodeType == n.ELEMENT_NODE and n.namespaceURI == W}
    fonts = child(node, 'rFonts')
    if fonts is not None:
        result['rFonts'] = ' '.join(fonts.getAttributeNS(W, key) for key in ('ascii', 'hAnsi', 'eastAsia', 'cs'))
    return result


def add_warning(warnings, field, message):
    item = {'field': field, 'message': message}
    if warnings is not None and item not in warnings:
        warnings.append(item)


class WordText:
    def __init__(self, styles=None):
        self.styles = {}
        self.defaults = {}
        self.default_paragraph = ''
        if styles is not None:
            for style in descendants(styles, 'style'):
                self.styles[style.getAttributeNS(W, 'styleId')] = style
                if style.getAttributeNS(W, 'type') == 'paragraph' and style.getAttributeNS(W, 'default') in ('1', 'true'):
                    self.default_paragraph = style.getAttributeNS(W, 'styleId')
            defaults = descendants(styles, 'rPrDefault')
            if defaults:
                self.defaults = properties(child(defaults[0], 'rPr'))

    def style_properties(self, name, seen=None):
        seen = set() if seen is None else seen
        if name not in self.styles or name in seen:
            return {}
        seen.add(name)
        style = self.styles[name]
        base = child(style, 'basedOn')
        result = self.style_properties(base.getAttributeNS(W, 'val'), seen) if base else {}
        result.update(properties(child(style, 'rPr')))
        return result

    def content(self, node, field='', warnings=None):
        def warn(message):
            add_warning(warnings, field, message)

        def convert(text, script):
            if not text or not script:
                return text
            if script == 'symbol-font':
                warn('Symbol-font characters may represent different glyphs from their stored text; they are marked explicitly. Replace them with Unicode in the source report.')
                return '[Symbol-font text: ' + text + ']'
            if script in ('raised', 'lowered'):
                warn('Manually raised/lowered text has no reliable plain-text equivalent; it is marked explicitly. Check the source report.')
                return '[' + script + ': ' + text + ']'
            mapping = SUPERSCRIPT if script == 'superscript' else SUBSCRIPT
            if all(c in mapping or c.isspace() for c in text):
                return ''.join(mapping.get(c, c) for c in text)
            warn('Some ' + script + ' characters have no supported Unicode equivalent; explicit ^{...} or _{...} notation is used. Check the proposed text.')
            return ('^{' if script == 'superscript' else '_{') + text + '}'

        paragraphs = []
        for p in descendants(node, 'p'):
            # Do not duplicate text in nested text-box paragraphs.
            ancestor = p.parentNode
            nested = False
            while ancestor is not None and ancestor != node:
                if ancestor.nodeType == ancestor.ELEMENT_NODE and ancestor.namespaceURI == W and ancestor.localName == 'p':
                    nested = True
                ancestor = ancestor.parentNode
            if nested:
                continue
            base = dict(self.defaults)
            ppr = properties(child(p, 'pPr'))
            base.update(self.style_properties(ppr.get('pStyle', self.default_paragraph)))
            fragments = []

            def emit(text, script=''):
                if fragments and fragments[-1][1] == script:
                    fragments[-1] = (fragments[-1][0] + text, script)
                else:
                    fragments.append((text, script))

            def walk(n, script=''):
                if n.nodeType != n.ELEMENT_NODE:
                    return
                tag = n.localName
                if n.namespaceURI == M and tag in ('oMath', 'oMathPara'):
                    warn('Word equations cannot be converted reliably; a visible placeholder replaces each equation. Consult the source report or deselect this field.')
                    emit('[Word equation — consult source report]')
                    return
                if n.namespaceURI == W:
                    if tag in ('rPr', 'pPr', 'sdtPr'):
                        return
                    if tag in ('drawing', 'object', 'pict', 'sym', 'footnoteReference', 'endnoteReference'):
                        warn('Embedded graphics, symbols or note references cannot be converted reliably; they are marked with a placeholder. Consult the source report.')
                        emit('[Word object or reference — consult source report]')
                        return
                    if tag in ('del', 'moveFrom'):
                        warn('Tracked changes are present; deleted or moved-from text is excluded. Resolve the changes in Word before relying on the import.')
                        return
                    if tag in ('ins', 'moveTo'):
                        warn('Tracked changes are present; inserted or moved-to text is included. Resolve the changes in Word before relying on the import.')
                    if tag in ('fldChar', 'fldSimple', 'instrText'):
                        warn('Word fields use their stored text result; field calculations and instructions are not imported or refreshed.')
                        if tag != 'fldSimple':
                            return
                    if tag == 'hyperlink':
                        warn('Hyperlink text is retained, but its link target is not part of the imported text.')
                    if tag == 'r':
                        direct = properties(child(n, 'rPr'))
                        props = dict(base)
                        props.update(self.style_properties(direct.get('rStyle', '')))
                        props.update(direct)
                        align = props.get('vertAlign', '')
                        script = align if align in ('superscript', 'subscript') else ''
                        position = props.get('position', '0')
                        if not script and position not in ('', '0'):
                            script = 'lowered' if position.startswith('-') else 'raised'
                        if re.search(r'\b(symbol|wingdings|webdings|mt extra)\b', props.get('rFonts', ''), re.I):
                            script = 'symbol-font'
                        if any(k in props and props[k] not in ('0', 'false', 'off', 'none')
                               for k in ('b', 'i', 'bCs', 'iCs', 'u', 'strike', 'dstrike', 'smallCaps', 'caps', 'vanish')):
                            warn('Character styling such as bold, italic, underline, strikeout or hidden text is not retained in plain metadata. Check the proposed text.')
                    if tag == 't':
                        emit(''.join(c.data for c in n.childNodes if c.nodeType == c.TEXT_NODE), script)
                        return
                    if tag in ('br', 'cr', 'tab'):
                        emit('\t' if tag == 'tab' else '\n')
                        return
                for c in n.childNodes:
                    walk(c, script)

            walk(p)
            paragraphs.append(''.join(convert(text, script) for text, script in fragments))
        return '\n'.join(paragraphs)


def qmd_warnings(values, warnings):
    for key, value in values.items():
        if re.search(r'\$[^\n]+?\$|\\[([]|<[^>]+>|\^.+?\^|~[^~\n]+~|[_^]\{', value):
            add_warning(warnings, key, 'Quarto math or markup is retained as literal source text, not rendered formatting. Check the proposed text; Unicode superscripts/subscripts remain unchanged.')


def word(path, output=None, values=None, warnings=None):
    with zipfile.ZipFile(path) as archive:
        raw = archive.read('word/document.xml')
        if b'<!DOCTYPE' in raw or b'<!ENTITY' in raw:
            raise ValueError('Unsupported Word XML declarations.')
        doc = minidom.parseString(raw)
        styles = None
        if 'word/styles.xml' in archive.namelist():
            style_xml = archive.read('word/styles.xml')
            if b'<!DOCTYPE' in style_xml or b'<!ENTITY' in style_xml:
                raise ValueError('Unsupported Word style XML declarations.')
            styles = minidom.parseString(style_xml)
        reader = WordText(styles)
        body = descendants(doc, 'body')[0]
        blocks = [s for s in descendants(doc, 'sdt')
                  if any(t.getAttributeNS(W, 'val') == 'datapond.metadata' for t in descendants(s, 'tag'))]
        if len(blocks) > 1:
            raise ValueError('The DataPond metadata section is duplicated.')
        found = {}
        field_cells = {}
        if blocks:
            for row in descendants(blocks[0], 'tr'):
                cells = descendants(row, 'tc')
                if len(cells) != 2:
                    raise ValueError('The metadata table must have two columns.')
                label = reader.content(cells[0])
                key = next((k for k, v in FIELDS.items() if v == label), None)
                if key:
                    if key in found:
                        raise ValueError('Duplicate metadata field: ' + key)
                    found[key] = reader.content(cells[1], key, warnings)
                    field_cells[key] = cells[1]
            if set(found) != set(FIELDS):
                raise ValueError('Keep all labels in the DataPond metadata table.')
        if values is None:
            return found
        found.update(values)

        def element(tag, attrs=None):
            node = doc.createElementNS(W, 'w:' + tag)
            for key, value in (attrs or {}).items():
                node.setAttributeNS(W, 'w:' + key, value)
            return node

        def paragraph(text):
            p = element('p')
            for i, line in enumerate(text.split('\n')):
                r = element('r')
                if i:
                    r.appendChild(element('br'))
                t = element('t'); t.setAttribute('xml:space', 'preserve')
                t.appendChild(doc.createTextNode(line)); r.appendChild(t); p.appendChild(r)
            return p

        def replace_paragraph(p, text):
            for child in list(p.childNodes):
                if not (child.nodeType == child.ELEMENT_NODE and child.localName == 'pPr'):
                    p.removeChild(child)
            new = paragraph(text)
            for child in list(new.childNodes):
                p.appendChild(child)

        for p in list(descendants(body, 'p')):
            text = ''.join(t.firstChild.data if t.firstChild else '' for t in descendants(p, 't'))
            styles = descendants(p, 'pStyle')
            if 'title' in values and styles and styles[0].getAttributeNS(W, 'val') == 'Title':
                replace_paragraph(p, values['title'])
            elif 'title' in values and text.startswith('Dataset or project title:'):
                replace_paragraph(p, 'Dataset or project title: ' + values['title'])
            elif text.startswith('Dataset UUID and version:'):
                replace_paragraph(p, 'Dataset UUID and version: ' + found.get('uuid', '') + ' / ' + found.get('version', ''))
        sdt = element('sdt'); props = element('sdtPr')
        props.appendChild(element('tag', {'val': 'datapond.metadata'})); sdt.appendChild(props)
        inner = element('sdtContent'); sdt.appendChild(inner)
        inner.appendChild(paragraph('Dataset metadata'))
        inner.appendChild(paragraph('Edit these values to import them into DataPond. Keep the field labels.'))
        table = element('tbl'); inner.appendChild(table)
        pr = element('tblPr'); table.appendChild(pr)
        pr.appendChild(element('tblW', {'w': '5000', 'type': 'pct'}))
        borders = element('tblBorders'); pr.appendChild(borders)
        for edge in ('top', 'left', 'bottom', 'right', 'insideH', 'insideV'):
            borders.appendChild(element(edge, {'val': 'single', 'sz': '4', 'color': 'D9D9D9'}))
        grid = element('tblGrid'); table.appendChild(grid)
        for width in ('2200', '6800'):
            grid.appendChild(element('gridCol', {'w': width}))
        for key, label in FIELDS.items():
            row = element('tr'); table.appendChild(row)
            for value, width in ((label, '2200'), (found.get(key, ''), '6800')):
                # An export must not flatten rich text in fields not selected.
                if width == '6800' and key not in values and key in field_cells:
                    row.appendChild(field_cells[key].cloneNode(True))
                    continue
                cell = element('tc'); row.appendChild(cell)
                cp = element('tcPr'); cell.appendChild(cp)
                cp.appendChild(element('tcW', {'w': width, 'type': 'dxa'}))
                cell.appendChild(paragraph(value))
        pagebreak = element('p'); run = element('r')
        run.appendChild(element('br', {'type': 'page'})); pagebreak.appendChild(run)
        inner.appendChild(pagebreak)
        if blocks:
            blocks[0].parentNode.replaceChild(sdt, blocks[0])
        else:
            anchor = next((p for p in descendants(body, 'p') if p.parentNode == body
                           and any(b.getAttributeNS(W, 'type') == 'page' for b in descendants(p, 'br'))), None)
            if anchor is not None:
                body.insertBefore(sdt, anchor.nextSibling)
            else:
                # Reports with a customised cover can still receive a separate
                # metadata page at the end, ahead of the section properties.
                inner.insertBefore(pagebreak.cloneNode(True), inner.firstChild)
                inner.removeChild(pagebreak)
                section = next((n for n in body.childNodes if n.nodeType == n.ELEMENT_NODE
                                and n.localName == 'sectPr'), None)
                body.insertBefore(sdt, section)
        with zipfile.ZipFile(output, 'w') as dest:
            for item in archive.infolist():
                dest.writestr(item, doc.toxml(encoding='utf-8') if item.filename == 'word/document.xml' else archive.read(item.filename))
    return found


def main():
    mode, path = sys.argv[1:3]
    values = json.loads(Path(sys.argv[4]).read_text(encoding='utf-8')) if mode == 'write' else None
    warnings = []
    if Path(path).suffix.lower() == '.qmd':
        result = qmd(path, values)
        if mode == 'write':
            Path(sys.argv[3]).write_text(result, encoding='utf-8')
        else:
            qmd_warnings(result, warnings)
    else:
        result = word(path, sys.argv[3] if mode == 'write' else None, values, warnings)
    if mode in ('read', 'inspect'):
        print(json.dumps({'values': result, 'warnings': warnings} if mode == 'inspect' else result, ensure_ascii=False))


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        sys.exit(str(error))
