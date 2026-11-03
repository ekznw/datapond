# utils_pond_overview.R
#
# Dataset-level overview helpers for the active data pond.

pond_overview_folder_path <- function(rows, selected_id, selection_cleared = FALSE,
                                      connected_selection = NULL) {
  if (!is.null(connected_selection)) {
    stop("Open the selected connected pond before opening its dataset folder.", call. = FALSE)
  }
  if (isTRUE(selection_cleared) || length(selected_id) != 1L || is.na(selected_id)) {
    stop("Select a dataset in the overview table or map first.", call. = FALSE)
  }
  rows <- as.data.frame(rows)
  selected <- rows[!is.na(rows$id) & rows$id == selected_id, , drop = FALSE]
  if (nrow(selected) != 1L) {
    stop("Select a dataset in the current overview search results first.", call. = FALSE)
  }
  path <- selected$dataset_path
  if (length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stop("No folder path is recorded for the selected dataset.", call. = FALSE)
  }
  path
}

pond_overview_palette <- function() {
  c(
    "Frozen release" = "#B99572",
    "Archive" =
      "#B99572",

    "Development \u2014 ready for archive" =
      "#A8C49A",

    "Development \u2014 in progress" =
      "#D1DDC7",

    "Source \u2014 fully described" =
      "#C3A35B",

    "Source \u2014 description incomplete" =
      "#E9DFB5",

    "Location / lifecycle mismatch" =
      "#C98F78",

    "Unclassified location" =
      "#D3CEC4"
  )
}


pond_stage_label <- function(stage) {
  stage <- tolower(
    trimws(
      as.character(stage)
    )
  )

  data.table::fcase(
    stage == "src",
    "Source (src/)",

    stage == "dev",
    "Development (dev/)",

    stage == "arc",
    "Archive (arc/)",

    default =
      "Unknown location"
  )
}


pond_overview_class <- function(
  stage,
  ready_for_archive,
  lifecycle_stage_mismatch
) {
  stage <- tolower(
    trimws(
      as.character(stage)
    )
  )

  ready_for_archive <-
    ready_for_archive %in% TRUE

  lifecycle_stage_mismatch <-
    lifecycle_stage_mismatch %in% TRUE

  data.table::fcase(
    lifecycle_stage_mismatch,
    "Location / lifecycle mismatch",

    stage == "arc",
    "Archive",

    stage == "dev" &
      ready_for_archive,
    "Development \u2014 ready for archive",

    stage == "dev",
    "Development \u2014 in progress",

    stage == "src" &
      ready_for_archive,
    "Source \u2014 fully described",

    stage == "src",
    "Source \u2014 description incomplete",

    default =
      "Unclassified location"
  )
}


get_dataset_overview_bbox <- function(
  dataset_id,
  base_path
) {
  summary <- tryCatch(
    get_dataset_spatial_summary(
      dataset_id = dataset_id,
      base_path = base_path
    ),
    error = function(e) {
      NULL
    }
  )

  if (
    is.null(summary) ||
    nrow(as.data.frame(summary)) == 0L
  ) {
    return(
      data.table::data.table(
        id = as.integer(dataset_id),
        bbox_status = "not_available",
        bbox_message =
          "No derived dataset bounding box is available.",
        bbox_wgs84 = NA_character_,
        west = NA_real_,
        south = NA_real_,
        east = NA_real_,
        north = NA_real_,
        bbox_available = FALSE
      )
    )
  }

  status <- as.character(
    summary$bbox_status[1] %||%
      "not_available"
  )

  west <- suppressWarnings(
    as.numeric(
      summary$xmin[1]
    )
  )

  south <- suppressWarnings(
    as.numeric(
      summary$ymin[1]
    )
  )

  east <- suppressWarnings(
    as.numeric(
      summary$xmax[1]
    )
  )

  north <- suppressWarnings(
    as.numeric(
      summary$ymax[1]
    )
  )

  valid_bbox <- (
    identical(status, "valid") &&
      all(
        is.finite(
          c(
            west,
            south,
            east,
            north
          )
        )
      )
  )

  status_message <- if (
    "status_message" %in% names(summary)
  ) {
    as.character(
      summary$status_message[1]
    )
  } else {
    NA_character_
  }

  bbox_text <- if (
    "bbox_wgs84" %in% names(summary)
  ) {
    as.character(
      summary$bbox_wgs84[1]
    )
  } else {
    NA_character_
  }

  data.table::data.table(
    id = as.integer(dataset_id),
    bbox_status = status,
    bbox_message = status_message,
    bbox_wgs84 = bbox_text,
    west = west,
    south = south,
    east = east,
    north = north,
    bbox_available = valid_bbox
  )
}


build_pond_overview_data <- function(
  base_path,
  schema_grouped,
  include_hidden = FALSE
) {
  datasets <- get_datasets_with_authors(
    base_path,
    include_hidden = include_hidden
  )

  if (
    is.null(datasets) ||
    nrow(as.data.frame(datasets)) == 0L
  ) {
    return(
      data.table::data.table()
    )
  }

  datasets <- data.table::as.data.table(
    data.table::copy(datasets)
  )

  lifecycle <- tryCatch(
    get_lifecycle_dashboard(
      base_path = base_path,
      schema_grouped = schema_grouped
    ),
    error = function(e) {
      data.table::data.table()
    }
  )

  lifecycle <- data.table::as.data.table(
    data.table::copy(lifecycle)
  )

  if (nrow(lifecycle) > 0L) {
    overview <- merge(
      datasets,
      lifecycle,
      by = "id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    overview <- data.table::copy(
      datasets
    )
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  overview <- pond_overview_discovery_metadata(conn, overview)

  required_columns <- list(
    title = NA_character_,
    authors = NA_character_,
    citation_date = NA_character_,
    version = NA_character_,
    abstract = NA_character_,
    citation_key = NA_character_,
    dataset_path = NA_character_,
    lifecycle_status = "draft",
    lifecycle_label = "Draft",
    stage = NA_character_,
    ready_for_archive = FALSE,
    next_action = "Complete metadata",
    lifecycle_stage_mismatch = FALSE,
    organisations = NA_character_,
    poc = NA_character_,
    record_owner = NA_character_,
    is_hidden = 0L,
    deleted_at = NA_character_,
    location_status = "not_scanned"
  )

  for (field in names(required_columns)) {
    if (!field %in% names(overview)) {
      overview[
        ,
        (field) :=
          required_columns[[field]]
      ]
    }
  }

  overview[
    ,
    stage := tolower(
      trimws(
        as.character(stage)
      )
    )
  ]

  overview[
    ,
    pond_location :=
      pond_stage_label(stage)
  ]

  overview[
    ,
    metadata_readiness :=
      data.table::fifelse(
        ready_for_archive %in% TRUE,
        "Required metadata complete",
        "Metadata incomplete"
      )
  ]

  overview[
    ,
    overview_class :=
      pond_overview_class(
        stage = stage,
        ready_for_archive =
          ready_for_archive,
        lifecycle_stage_mismatch =
          lifecycle_stage_mismatch
      )
  ]

  # A retained working copy can still match a frozen version. Describe that
  # release without applying today's full-profile assessment to its snapshot.
  if (DBI::dbExistsTable(conn, "dataset_release_tbl")) {
    releases <- DBI::dbGetQuery(conn, "SELECT dataset_id, version, baseline_hash, relative_path
      FROM dataset_release_tbl WHERE state='frozen'")
    for (i in seq_len(nrow(overview))) {
      candidates <- releases[which(releases$dataset_id == overview$id[i] &
        releases$version == overview$version[i]), , drop = FALSE]
      if (!nrow(candidates)) next
      archived <- identical(overview$location_status[i], "frozen_release")
      unchanged <- archived || tryCatch(
        review_fingerprint(release_payload(conn, overview$id[i], base_path, NULL)) %in% candidates$baseline_hash,
        error = function(e) FALSE)
      if (unchanged) overview[i, `:=`(overview_class = "Frozen release",
        metadata_readiness = "", next_action = if (archived) "Frozen release" else "Working copy retained")]
    }
  }

  overview[
    ,
    display_title :=
      data.table::fcase(
        !is.na(title) &
          nzchar(trimws(title)),
        as.character(title),

        !is.na(citation_key) &
          nzchar(trimws(citation_key)),
        as.character(citation_key),

        default =
          paste(
            "Dataset",
            id
          )
      )
  ]

  bbox_rows <- data.table::rbindlist(
    lapply(
      overview$id,
      function(dataset_id) {
        get_dataset_overview_bbox(
          dataset_id = dataset_id,
          base_path = base_path
        )
      }
    ),
    fill = TRUE
  )

  overview <- merge(
    overview,
    bbox_rows,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )

  overview[
    ,
    spatial_extent :=
      data.table::fifelse(
        bbox_available %in% TRUE,
        "Bounding box available",
        "No valid bounding box"
      )
  ]

  overview[
    ,
    record_visibility := data.table::fcase(
      !is.na(deleted_at) & nzchar(trimws(as.character(deleted_at))),
      "Deleted (recoverable)",
      as.integer(is_hidden) == 1L,
      "Hidden",
      default = "Visible"
    )
  ]

  data.table::setorder(
    overview,
    -id
  )

  overview[]
}


# These fields are also carried through local connections and offline Flow caches.
pond_overview_discovery_fields <- function() {
  c("abstract", "purpose", "citation", "citation_key", "citation_date", "version",
    "uuid", "folder_key", "keyword_object", "keyword_topic", "keyword_discipline",
    "keyword_geo", "keyword_labels", "organisation_search", "release_count",
    "release_versions", "latest_release_version", "release_summary", "dataset_edition",
    "dataset_family_uuid", "published_version")
}

# A working record can change or be restored after freezing. Release existence
# and version history come from the release register, not its current class.
pond_overview_release_metadata <- function(conn, rows) {
  rows <- data.table::as.data.table(data.table::copy(rows))
  for (field in c("dataset_edition", "dataset_family_uuid", "published_version"))
    data.table::set(rows, j = field, value = rep(NA_character_, nrow(rows)))
  if (nrow(rows) && DBI::dbExistsTable(conn, "dataset_edition_tbl")) {
    editions <- DBI::dbGetQuery(conn, "SELECT dataset_id,edition,family_uuid FROM dataset_edition_tbl")
    matched <- match(rows$id, editions$dataset_id)
    data.table::set(rows, j = "dataset_edition", value = editions$edition[matched])
    data.table::set(rows, j = "dataset_family_uuid", value = editions$family_uuid[matched])
    if (DBI::dbExistsTable(conn, "dataset_release_tbl") && DBI::dbExistsTable(conn, "dataset_publication_tbl")) {
      published <- DBI::dbGetQuery(conn, "SELECT p.dataset_id,r.version FROM dataset_publication_tbl p
        JOIN dataset_release_tbl r ON r.release_id=p.release_id WHERE p.action='published'
        AND p.id IN (SELECT MAX(id) FROM dataset_publication_tbl GROUP BY dataset_id)")
      data.table::set(rows, j = "published_version", value = published$version[match(rows$id, published$dataset_id)])
    }
  }
  rows[, `:=`(release_count = 0L, release_versions = NA_character_,
    latest_release_version = NA_character_, release_summary = "No frozen releases")]
  if (!nrow(rows) || !DBI::dbExistsTable(conn, "dataset_release_tbl")) return(rows)
  releases <- data.table::as.data.table(DBI::dbGetQuery(conn,
    "SELECT dataset_id, version FROM dataset_release_tbl WHERE state='frozen' ORDER BY frozen_at DESC, rowid DESC"))
  if (!nrow(releases)) return(rows)
  totals <- releases[, .(release_count = .N, release_versions = paste(version, collapse = ", "),
    latest_release_version = version[1]), by = dataset_id]
  matched <- match(rows$id, totals$dataset_id)
  for (field in c("release_count", "release_versions", "latest_release_version")) {
    found <- which(!is.na(matched))
    data.table::set(rows, i = found, j = field, value = totals[[field]][matched[found]])
  }
  rows[release_count > 0L, release_summary := paste0(release_count,
    ifelse(release_count == 1L, " frozen release: ", " frozen releases: "), release_versions)]
  rows[]
}

pond_overview_version_summary <- function(selected) {
  value <- function(field, fallback) {
    x <- selected[[field]]
    if (!length(x) || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) fallback else as.character(x[1])
  }
  shiny::p(class = "help-block",
    shiny::strong("Current version: "), value("version", "Not recorded"),
    shiny::tags$br(), shiny::strong("Archived releases: "),
    value("release_summary", "Not available in this catalogue"))
}

# Read in bulk without schema migrations: connected catalogues can be older and
# are opened read-only. Only metadata assigned to each dataset enters its index.
pond_overview_discovery_metadata <- function(conn, rows) {
  rows <- data.table::as.data.table(data.table::copy(rows))
  if (!nrow(rows)) return(rows)
  fields <- intersect(pond_overview_discovery_fields(), DBI::dbListFields(conn, "entry_tbl"))
  if (length(fields)) {
    records <- DBI::dbGetQuery(conn, paste("SELECT id,",
      paste(DBI::dbQuoteIdentifier(conn, fields), collapse = ", "), "FROM entry_tbl"))
    matched <- match(rows$id, records$id)
    for (field in fields) rows[, (field) := records[[field]][matched]]
  }

  rows[, organisation_search := NA_character_]
  if (DBI::dbExistsTable(conn, "organisation_tbl") &&
      DBI::dbExistsTable(conn, "dataset_organisation_tbl")) {
    org_fields <- intersect(c("organisation_name_full", "organisation_name_short",
      "organisation_code", "organisation_department", "organisational_unit_name"),
      DBI::dbListFields(conn, "organisation_tbl"))
    expressions <- if (length(org_fields)) {
      paste0("COALESCE(o.", DBI::dbQuoteIdentifier(conn, org_fields), ", '')")
    } else character()
    if ("role" %in% DBI::dbListFields(conn, "dataset_organisation_tbl")) {
      expressions <- c(expressions, "COALESCE(dorg.role, '')")
    }
    if (length(expressions)) {
      orgs <- DBI::dbGetQuery(conn, paste0(
        "SELECT dorg.dataset_id, GROUP_CONCAT(", paste(expressions, collapse = " || ' ' || "),
        ", ' ') AS organisation_search FROM dataset_organisation_tbl dorg ",
        "LEFT JOIN organisation_tbl o ON o.id = dorg.organisation_id GROUP BY dorg.dataset_id"))
      rows[, organisation_search := orgs$organisation_search[match(id, orgs$dataset_id)]]
    }
  }

  rows[, keyword_labels := NA_character_]
  if (DBI::dbExistsTable(conn, "keyword_vocab_tbl") &&
      all(c("field_code", "option_value", "option_label") %in%
        DBI::dbListFields(conn, "keyword_vocab_tbl"))) {
    vocab <- DBI::dbGetQuery(conn, "SELECT field_code, option_value, option_label FROM keyword_vocab_tbl")
    vocab$field_code[vocab$field_code == "keyword_topic"] <- "keyword_object"
    keyword_fields <- intersect(c("keyword_object", "keyword_topic", "keyword_discipline", "keyword_geo"), names(rows))
    labels <- lapply(seq_len(nrow(rows)), function(i) {
      selected <- lapply(keyword_fields, function(field) {
        canonical <- if (field == "keyword_topic") "keyword_object" else field
        values <- tolower(split_keyword_values(rows[[field]][i]))
        vocab$option_label[vocab$field_code == canonical & tolower(vocab$option_value) %in% values]
      })
      selected <- unlist(selected, use.names = FALSE)
      paste(unique(selected[!is.na(selected)]), collapse = " ")
    })
    rows[, keyword_labels := unlist(labels, use.names = FALSE)]
  }
  pond_overview_release_metadata(conn, rows)
}

pond_overview_search_index <- function(overview) {
  dt <- data.table::as.data.table(overview)
  if (!nrow(dt)) return(character())
  fields <- intersect(
    c(
      "display_title", "authors", "organisations", "poc",
      "citation_key", "pond_location", "lifecycle_label",
      "metadata_readiness", "next_action", "dataset_path",
      "record_owner", "record_visibility", "location_status",
      "title", "pond_name", "connection_type", "uuid", "stage", "availability",
      setdiff(pond_overview_discovery_fields(), "release_count")
    ),
    names(dt)
  )
  if (!length(fields)) return(rep("", nrow(dt)))
  search_data <- as.data.frame(dt[, ..fields])
  # "No frozen releases" is a display message, not evidence of a release.
  if ("release_summary" %in% fields && "release_count" %in% names(dt)) {
    search_data$release_summary[dt$release_count %in% 0L] <- ""
  }
  tolower(apply(
    search_data,
    1L,
    function(row) paste(row[!is.na(row)], collapse = " ")
  ))
}

pond_overview_search_input <- function(input_id) {
  shiny::textInput(input_id, "Search pond datasets", width = "100%",
    placeholder = paste(
      "Title, author, full organisation name, keywords, abstract,",
      "purpose, contact, identifier, date, version, status or folder"
    ))
}

# Both controls edit one query. Keep them outside the DT widget so typing and
# clearing a no-match search continue to work when the table is redrawn.
pond_overview_search_server <- function(input, session) {
  query <- shiny::reactiveVal("")
  sync <- function(value, other_id) {
    value <- as.character(value %||% "")
    if (identical(value, query())) return(invisible(NULL))
    query(value)
    shiny::updateTextInput(session, other_id, value = value)
  }
  shiny::observeEvent(input$pond_dataset_search, {
    access_require("read")
    sync(input$pond_dataset_search, "pond_inventory_search")
  })
  shiny::observeEvent(input$pond_inventory_search, {
    access_require("read")
    sync(input$pond_inventory_search, "pond_dataset_search")
  })
  shiny::debounce(shiny::reactive(query()), 250)
}

filter_pond_overview_data <- function(overview, search_text = NULL) {
  dt <- data.table::as.data.table(overview)
  if (!nrow(dt)) return(dt)
  search_text <- trimws(as.character(search_text[1] %||% ""))
  if (is.na(search_text) || !nzchar(search_text)) return(dt)
  haystack <- if (".search_text" %in% names(dt)) dt$.search_text else pond_overview_search_index(dt)
  terms <- strsplit(tolower(search_text), "\\s+")[[1L]]
  keep <- rep(TRUE, length(haystack))
  for (term in terms) keep <- keep & grepl(term, haystack, fixed = TRUE)
  dt[keep]
}

# Connected rows intentionally have no active-pond record id: ids are only
# unique within a pond and must never be passed into the current editor.
merge_pond_overview_inventory <- function(current, connected = NULL, pond_name, pond_path) {
  current <- data.table::as.data.table(data.table::copy(current))
  if (nrow(current)) {
    current[, `:=`(active_pond = TRUE, pond_name = pond_name, connection_type = "Current pond",
      open_path = pond_path, availability = "Current pond", refreshed = "Current catalogue")]
    current[, record_key := paste("current", pond_path, id, sep = "|")]
  }
  other <- data.table::as.data.table(data.table::copy(connected))
  if (nrow(other)) {
    other[, `:=`(active_pond = FALSE, id = NA_integer_, display_title = title,
      authors = NA_character_, organisations = NA_character_,
      overview_class = "Connected pond (read-only)",
      lifecycle_label = pond_stage_label(stage), metadata_readiness = "Not assessed here",
      next_action = ifelse(connection_type == "Local pond", "Open pond to view / edit", "Open in Flow to inspect / download"),
      spatial_extent = "Not mapped here")]
    other[, record_key := paste(connection_type, open_path, uuid, dataset_path, sep = "|")]
  }
  rows <- data.table::rbindlist(list(current, other), fill = TRUE, use.names = TRUE)
  if (nrow(rows)) {
    for (field in c("citation_date", "version")) if (!field %in% names(rows)) rows[, (field) := NA_character_]
    rows[, .search_text := pond_overview_search_index(rows)]
  }
  rows
}


pond_overview_author_label <- function(authors) {
  vapply(
    authors,
    function(value) {
      if (is.null(value) || length(value) == 0L || is.na(value[1])) {
        return(NA_character_)
      }
      value <- trimws(as.character(value[1]))
      if (!nzchar(value)) return(NA_character_)
      names <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
      names <- names[nzchar(names)]
      if (length(names) == 0L) return(NA_character_)
      if (length(names) == 1L) names[1] else paste(names[1], "et al.")
    },
    character(1),
    USE.NAMES = FALSE
  )
}


pond_overview_inventory_display <- function(overview) {
  overview <- data.table::as.data.table(
    data.table::copy(overview)
  )
  if (!"active_pond" %in% names(overview)) overview[, active_pond := TRUE]
  if (!"pond_name" %in% names(overview)) overview[, pond_name := "Current pond"]
  for (field in c("availability", "refreshed")) if (!field %in% names(overview)) overview[, (field) := NA_character_]
  for (field in c("dataset_edition", "published_version")) if (!field %in% names(overview)) overview[, (field) := NA_character_]
  if (!"release_summary" %in% names(overview)) overview[, release_summary := "Not available in this catalogue"]

  overview[
    ,
    .(
      `Dataset ID` = id,
      `Overview class` = overview_class,
      Title = display_title,
      Authors = pond_overview_author_label(authors),
      `Lifecycle status` = lifecycle_label,
      `Next action` = next_action,
      `Spatial extent` = spatial_extent,
      `Current version` = version,
      `Citation date` = citation_date,
      Organisations = organisations,
      Pond = ifelse(active_pond, paste0("\u2605 Current \u2014 ", pond_name), pond_name),
      `Files / cache` = availability,
      `Metadata refreshed` = refreshed,
      `Archived releases` = release_summary,
      Edition = ifelse(dataset_edition == "in_house", "In-house", ifelse(dataset_edition == "public", "Public", "")),
      `Published version` = published_version
    )
  ]
}


pond_overview_responsive_column_defs <- function() {
  list(
    list(
      visible = FALSE,
      targets = c(0L, 1L)
    ),
    list(
      className = "all",
      responsivePriority = 1L,
      width = "300px",
      targets = 2L
    ),
    list(
      className = "all",
      responsivePriority = 2L,
      width = "190px",
      targets = 3L
    ),
    list(
      className = "all",
      responsivePriority = 3L,
      width = "110px",
      targets = 8L
    ),
    list(responsivePriority = 4L, targets = 4L),
    list(responsivePriority = 5L, targets = 5L),
    list(responsivePriority = 7L, targets = 7L),
    list(responsivePriority = 8L, targets = 9L),
    list(responsivePriority = 9L, targets = 6L),
    list(className = "all", responsivePriority = 4L, targets = 10L),
    list(responsivePriority = 10L, targets = 11L),
    list(responsivePriority = 11L, targets = 12L),
    list(responsivePriority = 6L, targets = 13L)
  )
}


pond_overview_responsive_table_options <- function() {
  list(
    pageLength = 15L,
    deferRender = TRUE,
    order = list(),
    scrollX = FALSE,
    autoWidth = FALSE,
    # The inventory's Shiny search uses the same query as the map. Retain DT's
    # per-column filters, but omit its independent global search box (f).
    dom = "Brtip",
    buttons = c("copy", "csv"),
    responsive = list(details = FALSE),
    columnDefs = pond_overview_responsive_column_defs()
  )
}


pond_overview_table_extensions <- function() {
  c("Buttons", "Responsive")
}

# Fetch only the selected connected record's abstract from local metadata.
# Never fetch dataset files or contact the remote service for a preview.
read_connected_overview_abstract <- function(selected) {
  if (identical(selected$connection_type, "Remote cache (read-only)")) {
    access_require("read")
  } else connected_pond_require_read(selected$open_path)
  path <- selected$open_path
  if (identical(selected$connection_type, "Remote cache (read-only)")) {
    current <- movement_cache_current(path)
    if (is.null(current)) stop("The metadata cache is unavailable. Refresh it in Flow.")
    path <- current$directory
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), get_db_path(path), flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(DBI::dbDisconnect(conn))
  fields <- DBI::dbListFields(conn, "entry_tbl")
  if (!"abstract" %in% fields) return(NULL)
  query <- paste0("SELECT ", if ("uuid" %in% fields) "uuid" else "NULL AS uuid", ", abstract FROM entry_tbl WHERE id = ?")
  row <- DBI::dbGetQuery(conn, query,
    params = list(selected$source_record_id))
  if (!nrow(row) || (!is.na(selected$uuid) && !identical(row$uuid[1], selected$uuid)))
    stop("This cached record has changed. Refresh the overview and select it again.")
  row$abstract[1]
}

pond_overview_selection_outline <- function(map, overview, selected_id) {
  if (length(selected_id) != 1L || is.na(selected_id) || !nrow(overview)) return(map)
  row <- overview[overview$id %in% selected_id, ]
  if (nrow(row) != 1L || !isTRUE(row$bbox_available) ||
      !all(is.finite(c(row$west, row$south, row$east, row$north))) ||
      row$east <= row$west || row$north <= row$south) return(map)
  leaflet::addRectangles(map, lng1 = row$west, lat1 = row$south, lng2 = row$east, lat2 = row$north,
    group = "overview_selection", color = "#3F5146", weight = 5, opacity = 1, fillOpacity = 0,
    options = leaflet::pathOptions(pane = "pond_bbox_selected", interactive = FALSE, pointerEvents = "none"))
}
