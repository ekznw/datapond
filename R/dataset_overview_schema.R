# Shared definitions for generated controls, hydration and metadata validation.
# Build fresh tables per session so data.table edits cannot affect another user.
dataset_overview_schema <- function() {
  field <- function(name, label, type, description, obligation = "M") {
    list(field = name, label = label, type = type, description = description,
         options = NA_character_, obligation = obligation)
  }
  section <- function(subtab, ...) {
    data.table::data.table(
      tab = "Dataset overview", subtab = subtab,
      data.table::rbindlist(list(...))
    )
  }

  list(`Dataset overview` = list(
    Citation = section("Citation",
      field("uuid", "Dataset identifier", "hidden", "Auto generated primary key."),
      field("title", "Title", "textarea", ""),
      field("citation_date", "Citation date", "date", paste(
        "When this version was published. Record the period covered by the data under Temporal extents."
      )),
      field("version", "Version", "text", paste(
        "Start at 0.1.0 for a working draft; use 1.0.0 for the first release.",
        "Use major.minor.patch, with the text field or sliders. Change the version deliberately; lifecycle status is managed separately."
      )),
      field("citation", "Citation", "hidden", "Check the generated citation."),
      field("citation_key", "Dataset citation key", "hidden", paste(
        "Auto generated, unique persistent identifier generated using citation",
        "information plus auto generated unique number."
      ))
    ),
    About = section("About",
      field("purpose", "Purpose", "textarea", "Describe the original reason the dataset was created."),
      field("abstract", "Abstract", "textarea", paste(
        "Description of the dataset including context, aims and objectives,",
        "input data sources, notes on dataset products and how they can be used."
      )),
      field("keyword_object", "Theme / subject keywords", "select", paste(
        "What the dataset is about, e.g. wetlands; fire; species distributions."
      )),
      field("keyword_discipline", "Knowledge domain", "select", paste(
        "Field of study, e.g. soil science; hydrology; vegetation ecology."
      ), obligation = "O"),
      field("keyword_geo", "Spatial context", "select", paste(
        "Places described, from local to broad, e.g. Cathedral Peak;",
        "uKhahlamba Drakensberg; KwaZulu-Natal."
      ), obligation = "O"),
      field("update_frequency", "Update Frequency", "text", paste(
        "How often the dataset is expected to change, e.g. annually; not planned; subject to funding. This is the data update schedule, not the metadata review schedule. Leave blank if unknown."
      ), obligation = "O")
    )
  ))
}
