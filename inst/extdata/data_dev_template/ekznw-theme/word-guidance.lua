-- Keep author guidance as editable, separately styled Word paragraphs.
-- Native paragraphs avoid Word joining a callout table to the next data table.
-- HTML and PDF retain Quarto's standard callout rendering.
function Callout(el)
  if not quarto.doc.is_format("docx") or el.type ~= "tip" then
    return nil
  end
  local blocks = pandoc.Blocks({})
  blocks:insert(pandoc.Div(
    {pandoc.Para({pandoc.Str(pandoc.utils.stringify(el.title))})},
    pandoc.Attr("", {}, {["custom-style"] = "Guidance Heading"})
  ))
  blocks:insert(pandoc.Div(
    el.content,
    pandoc.Attr("", {}, {["custom-style"] = "Guidance Text"})
  ))
  return blocks
end
