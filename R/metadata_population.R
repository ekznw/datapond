# One session-wide population choice controls entry and assessment. It is a
# working preference, not a stored assertion about a dataset's completeness.
metadata_population_choices <- function() {
  c(
    "Essential \u2014 Prickly Porcupine" = "rough",
    "Full \u2014 Armoured Pangolin" = "full"
  )
}

metadata_inventory_fields <- function() c("title", "citation_date", "version", "abstract", "keyword_object")

dataset_naming_organisation_code <- function(organisations) {
  if (is.null(organisations) || nrow(organisations) == 0L) return("")
  field <- intersect(c("code", "organisation_code"), names(organisations))
  if (!length(field)) return("")
  code <- organisations[[field[1]]][1]
  if (!non_empty_scalar(code) || !grepl("[A-Za-z0-9]", code)) return("")
  trimws(as.character(code))
}

# Small inline vector badges remain sharp at icon size and need no asset server.
metadata_population_icon <- function(profile, width = 64L) {
  profile <- match.arg(profile, c("rough", "full"))
  animal <- if (profile == "rough") {
    '<g fill="#f6e8c9" stroke="#f6e8c9" stroke-linejoin="round">
      <path d="M33 67 12 56 37 60 Z M36 61 11 44 43 55 Z M42 57 14 34 49 51 Z M48 54 21 27 55 49 Z M54 52 30 24 61 50 Z M60 54 41 29 66 54 Z" stroke="none"/>
      <path d="M36 61 23 52 39 57 Z M48 54 34 40 51 51 Z M60 54 50 40 63 53 Z" fill="#343638" stroke="none"/>
      <path d="M28 65 18 70" fill="none" stroke="#343638" stroke-width="4" stroke-linecap="round"/>
      <path d="M24 65 C23 52 37 44 52 46 C63 47 67 54 71 59 L81 66 Q84 70 78 72 L68 72 L64 78 L59 78 L59 71 L40 72 L36 79 L30 79 L31 70 Z" fill="#343638" stroke-width=".8"/>
      <path d="m30 52 13 12 m-6-16 14 13 m-5-15 13 12" fill="none" stroke="#f6e8c9" stroke-width="1.8"/>
      <path d="M62 58 Q58 53 61 52 Q65 52 65 57" fill="#666867" stroke="none"/>
      <circle cx="71" cy="64" r="1.6" fill="#f6e8c9" stroke="none"/>
      <circle cx="81" cy="69" r="1.5" fill="#17191b" stroke="none"/>
    </g>'
  } else {
    # Temminck's ground pangolin (Smutsia temminckii): heavy tapering tail,
    # small conical head, overlapping scales and curled digging foreclaws.
    # Reference: https://www.sanbi.org/animal-of-the-week/temmincks-ground-pangolin/
    '<g stroke-linejoin="round" stroke-linecap="round">
      <path d="M51 62 57 70 56 77 61 79 51 79 47 69 Z M65 58 72 64 70 69 66 68 Z" fill="#ac935f"/>
      <path d="M39 55 C29 62 23 72 12 77 Q29 80 44 69 L51 61 Z" fill="#c9b575" stroke="#ecdfb5" stroke-width="1.2"/>
      <path d="M32 62 C32 49 40 39 52 39 C64 38 72 48 76 58 L69 65 Q59 72 43 69 Z" fill="#c9b575" stroke="#ecdfb5" stroke-width="1.2"/>
      <path d="M43 43 39 48 44 52 40 57 45 61 42 66 M52 41 48 46 53 50 49 55 54 59 50 64 55 68 M61 43 57 48 62 52 58 57 63 61 59 67 M69 49 65 54 70 58 66 63 M34 61 37 66 32 69 28 67 M26 70 29 74 23 75 M44 52 49 55 M53 50 58 53 M54 59 59 62 M62 52 67 55" fill="none" stroke="#64704d" stroke-width="1.4"/>
      <path d="M69 52 Q76 54 80 60 L87 68 Q86 71 82 69 L72 65 67 60 Z" fill="#ecdfb5"/>
      <path d="M43 65 Q49 66 49 71 L46 77 51 80 41 80 39 76 40 70" fill="#c9b575" stroke="#ecdfb5" stroke-width="1.2"/>
      <path d="M63 62 67 67 65 71 M65 69 68 70 M64 71 67 72" fill="none" stroke="#ecdfb5" stroke-width="2"/>
      <path d="m44 78 1 2 m3-2 1 2" fill="none" stroke="#64704d" stroke-width="1"/>
      <circle cx="77" cy="61" r="1.4" fill="#303632"/>
      <circle cx="86" cy="68" r="1.1" fill="#303632"/>
    </g>'
  }
  htmltools::HTML(paste0(
    '<svg class="population-hex" width="', as.integer(width), '" height="',
    as.integer(round(width * 1.125)), '" viewBox="0 0 100 112" ',
    'aria-hidden="true" focusable="false" xmlns="http://www.w3.org/2000/svg">',
    '<path class="population-hex-border" d="M50 4 95 30 95 82 50 108 5 82 5 30 Z" ',
    'fill="', if (profile == "rough") '#785235' else '#36594a',
    '" stroke="currentColor" stroke-width="3"/>',
    '<path d="M50 11 89 34 89 78 50 101 11 78 11 34 Z" fill="none" ',
    'stroke="#f6e8c9" stroke-opacity=".3" stroke-width="1"/>',
    animal, '</svg>'
  ))
}

metadata_population_title_badge <- function(profile) {
  profile <- match.arg(profile, c("rough", "full"))
  choices <- metadata_population_choices()
  label <- names(choices)[match(profile, choices)]
  shiny::tags$span(
    class = "population-title-badge",
    title = paste(label, "mode. Change mode in Configuration."),
    style = paste(
      "display:inline-flex; align-items:center; gap:6px;",
      "font-size:11px; font-weight:500; line-height:1.2; color:#596452;"
    ),
    metadata_population_icon(profile, width = 28L),
    shiny::tags$span(label)
  )
}

metadata_population_controls <- function() {
  shiny::tagList(
    shiny::tags$style(htmltools::HTML("
      #population_profile { width:100%; margin-bottom:8px; }
      #population_profile .shiny-options-group { display:flex; gap:8px; }
      #population_profile .radio-inline { flex:1; min-width:0; padding:0; margin:0; }
      #population_profile input[type=radio] {
        position:absolute; width:1px; height:1px; padding:0;
        overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap;
      }
      #population_profile .radio-inline > span {
        display:flex; flex-direction:column; align-items:center;
        padding:8px 3px 5px; border:2px solid transparent; border-radius:8px;
        color:#776f61; cursor:pointer; text-align:center;
      }
      #population_profile .population-name {
        display:block; font-size:12px; font-weight:600; line-height:1.25; margin-top:5px;
      }
      #population_profile .population-selection {
        display:block; height:16px; font-size:10px; font-weight:600; visibility:hidden;
      }
      #population_profile input:checked + span {
        border-color:#496557; background:#eceee3; color:#294635;
      }
      #population_profile input:checked + span .population-selection { visibility:visible; }
      #population_profile input:focus-visible + span { outline:3px solid #88651f; outline-offset:2px; }
      #population_profile .radio-inline:hover > span { background:#f0eadc; }
      #population_profile .population-hex { display:block; flex-shrink:0; }
    ")),
    shiny::radioButtons(
      "population_profile", "Metadata population mode",
      choiceNames = lapply(names(metadata_population_choices()), function(label) {
        profile <- metadata_population_choices()[[label]]
        shiny::tagList(
          metadata_population_icon(profile),
          shiny::tags$span(class = "population-name", label),
          shiny::tags$span(class = "population-selection", `aria-hidden` = "true", "Selected")
        )
      }),
      choiceValues = unname(metadata_population_choices()),
      selected = "rough", inline = TRUE
    ),
    shiny::conditionalPanel(
      "input.population_profile === 'rough'",
      shiny::helpText(paste(
        "Essential inventory: title, publication date, version, organisation, abstract and theme keywords."
      )),
      shiny::checkboxInput(
        "show_population_detail", "Show additional metadata fields", FALSE
      )
    ),
    shiny::conditionalPanel(
      "input.population_profile === 'full'",
      shiny::helpText(paste(
        "Full metadata: Dataset status identifies missing detail."
      ))
    ),
    shiny::helpText(paste(
      "Applies to entry, saving and status. Switching keeps your values."
    ))
  )
}

metadata_population_detail <- function(...) {
  # Hide in place: removing/re-rendering inputs on a mode change loses unsaved
  # values and can empty repeatable editors on the following save.
  shiny::conditionalPanel(
    "input.population_profile === 'full' || input.show_population_detail === true",
    ...
  )
}

validate_metadata_population <- function(values, citation_dt, about_dt,
                                         profile = c("full", "rough"),
                                         scope = "dataset", topics = character(),
                                         organisations = NULL) {
  profile <- match.arg(profile)
  required_schema <- function(dt) {
    dt <- data.table::copy(data.table::as.data.table(dt))
    if (!"field" %in% names(dt)) return(dt)
    dt <- dt[!get("field") %in% c("keyword_topic", "data_complete_status")]
    if (profile == "rough") dt <- dt[get("field") %in% metadata_inventory_fields()]
    dt
  }
  missing <- c(
    validate_required(required_schema(citation_dt), values),
    validate_required(required_schema(about_dt), values)
  )
  if (profile == "full" && metadata_scope_requires_geographic_discovery(scope) &&
      length(topics) == 0L) missing <- c(missing, "ISO topic category")
  if (!nzchar(dataset_naming_organisation_code(organisations))) {
    missing <- c(missing, "Add an organisation with a usable code (the first added organisation supplies the naming code)")
  }

  errors <- character()
  if (length(missing)) errors <- c(errors, paste(
    "Missing required fields:", paste(missing, collapse = ", ")
  ))
  if (non_empty_scalar(values$citation_date)) {
    date_text <- as.character(values$citation_date[1])
    parsed <- tryCatch(suppressWarnings(as.Date(date_text)), error = function(e) as.Date(NA))
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_text) ||
        is.na(parsed) || format(parsed, "%Y-%m-%d") != date_text ||
        substr(date_text, 1, 4) == "0000") {
      errors <- c(errors, "Enter a valid publication date to generate the folder year and citation-key month.")
    }
  }
  if (non_empty_scalar(values$keyword_object) &&
      is.null(extract_primary_keyword(values$keyword_object))) {
    errors <- c(errors, "The first theme keyword must contain letters or numbers for the folder name and citation key.")
  }
  # Optional means absent is allowed, not that malformed supplied values pass.
  if (non_empty_scalar(values$version) && !validate_version(values$version)) {
    errors <- c(errors, "Version must use major.minor.patch format, e.g. 0.1.0 or 1.0.0.")
  }
  temporal <- validate_temporal_coverage(
    values$temporal_coverage_start_date, values$temporal_coverage_end_date
  )
  if (!isTRUE(temporal$valid)) errors <- c(errors, temporal$message)
  list(valid = length(errors) == 0L, missing = missing,
       message = paste(errors, collapse = "\n"))
}
