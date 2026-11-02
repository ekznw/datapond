-- Pandoc's default inline \texttt does not break long identifiers. Let Pandoc
-- escape each character before inserting fixed, invisible break opportunities.
-- HTML uses the theme's CSS instead. Code remains text and is never executed.
function Code(code)
  if not FORMAT:match("latex") then return nil end
  local characters = pandoc.Inlines({})
  for _, character in utf8.codes(code.text) do
    if #characters > 0 then
      characters:insert(pandoc.RawInline("latex", "\\allowbreak{}"))
    end
    characters:insert(pandoc.Str(utf8.char(character)))
  end
  local escaped = pandoc.write(pandoc.Pandoc({pandoc.Plain(characters)}), "latex")
  return pandoc.RawInline("latex", "\\texttt{" .. escaped .. "}")
end
