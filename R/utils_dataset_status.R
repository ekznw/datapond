# utils_dataset_status.R
# Dataset completion/status helpers ----

status_label <- function(x) {
  labels <- c(
    not_started = "Not started",
    partial = "Partially complete",
    complete = "Complete",
    needs_review = "Needs review",
    not_applicable = "Not applicable"
  )

  labels[[x]] %||% x
}

non_empty_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  x <- x[1]
  if (is.na(x)) return(FALSE)
  trimws(as.character(x)) != ""
}

schema_section <- function(schema_grouped, tab, subtab) {
  if (is.null(schema_grouped)) return(data.table())
  if (!tab %in% names(schema_grouped)) return(data.table())
  if (!subtab %in% names(schema_grouped[[tab]])) return(data.table())
  as.data.table(schema_grouped[[tab]][[subtab]])
}

check_entry_required_fields <- function(rec, section_dt) {
  if (is.null(rec) || nrow(section_dt) == 0) {
    return(list(
      missing = character(),
      missing_count = 0L,
      populated_count = 0L,
      required_count = 0L
    ))
  }

  dt <- as.data.table(section_dt)

  if (!"obligation" %in% names(dt)) {
    return(list(
      missing = character(),
      missing_count = 0L,
      populated_count = 0L,
      required_count = 0L
    ))
  }

  required <- dt[
    as.character(obligation) == "M" &
      as.character(type) != "hidden" &
      !is.na(field) &
      field != ""
  ]

  if (nrow(required) == 0) {
    return(list(
      missing = character(),
      missing_count = 0L,
      populated_count = 0L,
      required_count = 0L
    ))
  }

  missing <- character()
  populated_count <- 0L

  for (i in seq_len(nrow(required))) {
    f <- required$field[i]
    lab <- required$label[i] %||% f

    if (!f %in% names(rec)) {
      missing <- c(missing, lab)
      next
    }

    val <- rec[[f]][1]

    if (non_empty_scalar(val)) {
      populated_count <- populated_count + 1L
    } else {
      missing <- c(missing, lab)
    }
  }

  list(
    missing = missing,
    missing_count = length(missing),
    populated_count = populated_count,
    required_count = nrow(required)
  )
}

entry_section_status <- function(section_name, rec, section_dt, next_action) {
  chk <- check_entry_required_fields(rec, section_dt)

  status <- if (chk$required_count == 0L) {
    if (!is.null(rec) && nrow(as.data.frame(rec)) > 0) "complete" else "not_started"
  } else if (chk$missing_count == 0L) {
    "complete"
  } else if (chk$populated_count > 0L) {
    "partial"
  } else {
    "not_started"
  }

  data.table(
    section = section_name,
    status = status,
    status_label = status_label(status),
    required_missing = chk$missing_count,
    missing_fields = paste(chk$missing, collapse = "; "),
    saved_records = if (!is.null(rec)) 1L else 0L,
    next_action = if (status == "complete") "Review if needed." else next_action
  )
}

count_query <- function(conn, sql, params = list()) {
  res <- DBI::dbGetQuery(conn, sql, params = params)
  if (nrow(res) == 0) return(0L)
  as.integer(res[[1]][1] %||% 0L)
}

text_query <- function(conn, sql, params = list()) {
  res <- DBI::dbGetQuery(conn, sql, params = params)
  res
}

related_count_status <- function(section, n, complete_action, missing_action) {
  status <- if (n > 0L) "complete" else "not_started"

  data.table(
    section = section,
    status = status,
    status_label = status_label(status),
    required_missing = if (n > 0L) 0L else 1L,
    missing_fields = if (n > 0L) "" else section,
    saved_records = n,
    next_action = if (n > 0L) complete_action else missing_action
  )
}

metadata_population_schema <- function(section_dt, profile = c("full", "rough")) {
  profile <- match.arg(profile)
  dt <- data.table::copy(data.table::as.data.table(section_dt))
  if (!"field" %in% names(dt)) return(dt)
  # Publication is assessed once against its canonical typed-date rows.
  # Resource progress is workflow information, not descriptive completeness.
  dt <- dt[!get("field") %in% c("citation_date", "data_complete_status", "keyword_topic")]
  if (profile == "rough") {
    dt <- dt[get("field") %in% metadata_inventory_fields()]
  }
  dt
}

get_dataset_completion_status <- function(dataset_id, base_path, schema_grouped,
                                          population_profile = c("full", "rough")) {
  population_profile <- match.arg(population_profile)
  if (is.null(dataset_id) || length(dataset_id) == 0 || is.na(dataset_id)) {
    return(data.table(
      section = "Dataset",
      status = "not_started",
      status_label = status_label("not_started"),
      required_missing = NA_integer_,
      missing_fields = "",
      saved_records = 0L,
      next_action = "Select or save a dataset record."
    ))
  }

  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  rec <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM entry_tbl WHERE id = ?",
    params = list(dataset_id)
  )

  if (nrow(rec) == 0) {
    return(data.table(
      section = "Dataset",
      status = "not_started",
      status_label = status_label("not_started"),
      required_missing = NA_integer_,
      missing_fields = "",
      saved_records = 0L,
      next_action = "Selected dataset record was not found."
    ))
  }

  citation_dt <- metadata_population_schema(
    schema_section(schema_grouped, "Dataset overview", "Citation"), population_profile
  )
  about_dt <- metadata_population_schema(
    schema_section(schema_grouped, "Dataset overview", "About"), population_profile
  )

  lifecycle_status <- rec$lifecycle_status[1] %||% "draft"
  folder_locked <- as.integer(rec$folder_locked[1] %||% 0L)

  lifecycle_row <- data.table(
    section = "Lifecycle",
    status = lifecycle_status,
    status_label = lifecycle_status_label(lifecycle_status),
    required_missing = 0L,
    missing_fields = "",
    saved_records = 1L,
    next_action = if (folder_locked == 1L) {
      "Folder is locked. Normal metadata edits will not rename or move the folder."
    } else {
      "Draft folder is unlocked. Use Dataset lifecycle tools for explicit folder rename if needed."
    }
  )

  out <- list(
    lifecycle_row,
    entry_section_status(
      section_name = "Citation",
      rec = rec,
      section_dt = citation_dt,
      next_action = "Complete required citation fields."
    ),
    entry_section_status(
      section_name = "About",
      rec = rec,
      section_dt = about_dt,
      next_action = "Complete required descriptive fields."
    )
  )

  scope_code <- as.character(rec$metadata_resource_scope_code[1] %||% "dataset")
  profile_fields <- c(
    metadata_uuid = "Metadata-record UUID",
    metadata_resource_scope_code = "Metadata resource scope",
    metadata_language_code = "Metadata language",
    metadata_character_encoding = "Character encoding"
  )
  profile_missing <- unname(profile_fields[!vapply(
    names(profile_fields),
    function(field) field %in% names(rec) && non_empty_scalar(rec[[field]][1]),
    logical(1)
  )])
  profile_status <- if (length(profile_missing) == 0L) "complete" else "partial"
  out <- c(out, list(data.table::data.table(
    section = "Metadata profile",
    status = profile_status,
    status_label = status_label(profile_status),
    required_missing = length(profile_missing),
    missing_fields = paste(profile_missing, collapse = "; "),
    saved_records = 1L,
    next_action = if (profile_status == "complete") {
      "Review the advanced metadata profile if scope or language changes."
    } else {
      "Complete the advanced metadata profile."
    }
  )))

  n_topic_categories <- count_query(
    conn,
    "SELECT COUNT(*) AS n FROM dataset_topic_category_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )
  topic_required <- metadata_scope_requires_geographic_discovery(scope_code)
  topic_status <- if (!topic_required) {
    "not_applicable"
  } else if (n_topic_categories > 0L) {
    "complete"
  } else {
    "not_started"
  }
  out <- c(out, list(data.table::data.table(
    section = "ISO topic categories",
    status = topic_status,
    status_label = status_label(topic_status),
    required_missing = if (topic_required && n_topic_categories == 0L) 1L else 0L,
    missing_fields = if (topic_required && n_topic_categories == 0L) "ISO topic category" else "",
    saved_records = n_topic_categories,
    next_action = if (!topic_required) {
      "Topic categories are not required for the selected resource scope."
    } else if (n_topic_categories > 0L) {
      "Review topic categories if the resource changes."
    } else {
      "Select at least one controlled ISO topic category."
    }
  )))

  valid_dataset_bbox <- count_query(
    conn,
    paste(
      "SELECT COUNT(*) AS n FROM dataset_spatial_summary_tbl",
      "WHERE dataset_id = ? AND LOWER(COALESCE(bbox_status, '')) = 'valid'"
    ),
    list(dataset_id)
  ) > 0L
  valid_file_crs <- count_query(
    conn,
    paste(
      "SELECT COUNT(*) AS n FROM data_file_spatial_metadata_tbl",
      "WHERE dataset_id = ?",
      "AND LOWER(COALESCE(crs_status, '')) NOT IN ('', 'missing_crs', 'missing_or_unknown')",
      "AND COALESCE(NULLIF(TRIM(crs_auth_code), ''), NULLIF(TRIM(crs_input), ''), NULLIF(TRIM(crs_name), '')) IS NOT NULL"
    ),
    list(dataset_id)
  )
  valid_manual_crs <- count_query(
    conn,
    paste(
      "SELECT COUNT(*) AS n FROM dataset_manual_spatial_extent_tbl",
      "WHERE dataset_id = ? AND COALESCE(active, 1) = 1",
      "AND COALESCE(NULLIF(TRIM(crs_auth_code), ''), NULLIF(TRIM(crs_input), ''), NULLIF(TRIM(crs_name), '')) IS NOT NULL"
    ),
    list(dataset_id)
  )
  valid_dataset_crs <- valid_file_crs + valid_manual_crs > 0L
  has_named_area <- non_empty_scalar(rec$geographic_extent_name)
  geographic_missing <- if (!topic_required) character() else c(
    if (!valid_dataset_bbox && !has_named_area) "Dataset bounding box or area covered",
    if (valid_dataset_bbox && !valid_dataset_crs) "Coordinate reference system"
  )
  geographic_status <- if (!topic_required) {
    "not_applicable"
  } else if (length(geographic_missing) == 0L) {
    "complete"
  } else if (valid_dataset_bbox || valid_dataset_crs || has_named_area) {
    "partial"
  } else {
    "not_started"
  }
  out <- c(out, list(data.table::data.table(
    section = "Geographic discovery",
    status = geographic_status,
    status_label = status_label(geographic_status),
    required_missing = length(geographic_missing),
    missing_fields = paste(geographic_missing, collapse = "; "),
    saved_records = as.integer(valid_dataset_bbox) + as.integer(valid_dataset_crs) + as.integer(has_named_area),
    next_action = if (!topic_required) {
      "Bounding box and CRS are not required for the selected resource scope."
    } else if (length(geographic_missing) == 0L) {
      "Review the area covered or coordinate extent when source data change."
    } else {
      paste("Complete:", paste(geographic_missing, collapse = "; "))
    }
  )))

  citation_dates <- text_query(
    conn,
    "SELECT date_type_code FROM dataset_citation_date_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )
  has_publication_date <- nrow(citation_dates) > 0L &&
    any(citation_dates$date_type_code == "publication")
  out <- c(out, list(data.table::data.table(
    section = "Citation dates",
    status = if (has_publication_date) "complete" else "not_started",
    status_label = status_label(if (has_publication_date) "complete" else "not_started"),
    required_missing = if (has_publication_date) 0L else 1L,
    missing_fields = if (has_publication_date) "" else "Primary publication date",
    saved_records = nrow(citation_dates),
    next_action = if (has_publication_date) {
      "Add creation or revision dates where they are known."
    } else {
      "Save the primary publication date in Dataset intro."
    }
  )))

  responsibilities <- text_query(
    conn,
    "SELECT role_code, is_primary FROM metadata_responsibility_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )
  has_metadata_contact <- nrow(responsibilities) > 0L && any(
    responsibilities$role_code == "pointOfContact" &
      as.integer(responsibilities$is_primary) == 1L
  )
  responsibility_status <- if (has_metadata_contact) {
    "complete"
  } else if (nrow(responsibilities) > 0L) {
    "partial"
  } else {
    "not_started"
  }
  out <- c(out, list(data.table::data.table(
    section = "Metadata responsibilities",
    status = responsibility_status,
    status_label = status_label(responsibility_status),
    required_missing = if (has_metadata_contact) 0L else 1L,
    missing_fields = if (has_metadata_contact) "" else "Primary metadata point of contact",
    saved_records = nrow(responsibilities),
    next_action = if (has_metadata_contact) {
      "Review metadata creators and maintainers when responsibilities change."
    } else {
      "Add a primary metadata point of contact."
    }
  )))

  n_authors <- count_query(
    conn,
    "SELECT COUNT(*) AS n FROM authority_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  out <- c(out, list(
    related_count_status(
      section = "Authors",
      n = n_authors,
      complete_action = "Review authors if needed.",
      missing_action = "Add at least one author."
    )
  ))

  n_poc <- count_query(
    conn,
    "SELECT COUNT(*) AS n FROM poc_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  out <- c(out, list(
    related_count_status(
      section = "Point of contact",
      n = n_poc,
      complete_action = "Review contacts if needed.",
      missing_action = "Add at least one point of contact."
    )
  ))

  org_links <- text_query(
    conn,
    "SELECT d.role, o.organisation_code
     FROM dataset_organisation_tbl d LEFT JOIN organisation_tbl o ON o.id = d.organisation_id
     WHERE d.dataset_id = ? ORDER BY d.id",
    list(dataset_id)
  )

  n_org <- nrow(org_links)
  has_owner_or_custodian <- n_org > 0 &&
    any(org_links$role %in% c("owner", "custodian"), na.rm = TRUE)
  has_naming_code <- nzchar(dataset_naming_organisation_code(org_links))
  org_missing <- c(
    if (!has_owner_or_custodian) "Owner or custodian organisation",
    if (!has_naming_code) "Code for the first added organisation"
  )

  org_status <- if (n_org == 0) {
    "not_started"
  } else if (has_owner_or_custodian && has_naming_code) {
    "complete"
  } else {
    "partial"
  }

  out <- c(out, list(data.table(
    section = "Organisations",
    status = org_status,
    status_label = status_label(org_status),
    required_missing = length(org_missing),
    missing_fields = paste(org_missing, collapse = "; "),
    saved_records = n_org,
    next_action = if (org_status == "complete") {
      "Review organisations if needed."
    } else {
      paste("Complete:", paste(org_missing, collapse = "; "))
    }
  )))

  lic <- text_query(
    conn,
    "
    SELECT *
    FROM dataset_license_tbl
    WHERE dataset_id = ?
    ",
    list(dataset_id)
  )

  lic_missing <- character()
  licensing_assessment_fields <- c(
    "rights_basis_code", "license_holder", "access_level_code",
    "redistribution_permission_code", "license_identifier"
  )
  has_licensing_assessment <- nrow(lic) > 0L && any(vapply(
    intersect(licensing_assessment_fields, names(lic)),
    function(field) non_empty_scalar(lic[[field]][1]),
    logical(1)
  ))

  if (has_licensing_assessment) {
    lic_value <- function(
      field,
      default = ""
    ) {
      if (
        !field %in% names(lic) ||
        is.null(lic[[field]]) ||
        length(lic[[field]]) == 0L ||
        is.na(lic[[field]][1]) ||
        !nzchar(
          trimws(
            as.character(
              lic[[field]][1]
            )
          )
        )
      ) {
        return(default)
      }

      trimws(
        as.character(
          lic[[field]][1]
        )
      )
    }

    rights_basis <-
      lic_value(
        "rights_basis_code"
      )

    if (
      !nzchar(rights_basis) ||
      rights_basis ==
        "review_required"
    ) {
      lic_missing <- c(
        lic_missing,
        "Sharing authority"
      )
    }

    if (!nzchar(
      lic_value(
        "license_holder"
      )
    )) {
      lic_missing <- c(
        lic_missing,
        "Rights holder"
      )
    }

    if (!nzchar(
      lic_value(
        "access_level_code"
      )
    )) {
      lic_missing <- c(
        lic_missing,
        "Intended access"
      )
    }

    if (!nzchar(
      lic_value(
        "redistribution_permission_code"
      )
    )) {
      lic_missing <- c(
        lic_missing,
        "Redistribution permission"
      )
    }

    if (!nzchar(
      lic_value(
        "license_identifier"
      )
    )) {
      lic_missing <- c(
        lic_missing,
        "Licence or sharing terms"
      )
    }

    if (
      rights_basis %in%
        c(
          "external_licence",
          "external_permission"
        ) &&
      !nzchar(
        lic_value(
          "permission_reference"
        )
      )
    ) {
      lic_missing <- c(
        lic_missing,
        "Licence or permission reference"
      )
    }

    attribution_required <- (
      "required_attribution" %in%
        names(lic) &&
      !is.na(
        lic$required_attribution[1]
      ) &&
      as.integer(
        lic$required_attribution[1]
      ) == 1L
    )

    if (
      attribution_required &&
      !nzchar(
        lic_value(
          "attribution_text"
        )
      )
    ) {
      lic_missing <- c(
        lic_missing,
        "Attribution text"
      )
    }

    sensitivity_code <-
      lic_value(
        "sensitivity_handling_code",
        default = "none"
      )

    if (
      sensitivity_code != "none" &&
      !nzchar(
        lic_value(
          "sensitivity_reason"
        )
      )
    ) {
      lic_missing <- c(
        lic_missing,
        "Sensitivity reason"
      )
    }
  }

  lic_status <- if (!has_licensing_assessment) {
    "not_started"
  } else if (length(lic_missing) == 0L) {
    "complete"
  } else {
    "partial"
  }

  out <- c(
    out,
    list(
      data.table::data.table(
        section =
          "Licensing and sharing",

        status =
          lic_status,

        status_label =
          status_label(
            lic_status
          ),

        required_missing =
          length(
            lic_missing
          ),

        missing_fields =
          paste(
            unique(
              lic_missing
            ),
            collapse = "; "
          ),

        saved_records =
          as.integer(has_licensing_assessment),

        next_action = switch(
          lic_status,

          complete =
            "Review sharing terms if circumstances change.",

          partial =
            "Complete or review the sharing assessment.",

          "Complete the sharing-rights and licence assessment."
        )
      )
    )
  )

  out <- c(out, list(data_access_status_row(rec)))
  out <- c(out, iso_wave2_status_rows(conn, dataset_id, rec))
  out <- c(out, iso_wave3_status_rows(conn, dataset_id, rec))

  n_data_files <- count_query(
    conn,
    "SELECT COUNT(*) AS n FROM data_file_tbl WHERE dataset_id = ? AND active = 1",
    list(dataset_id)
  )

  out <- c(out, list(
    related_count_status(
      section = "Data files",
      n = n_data_files,
      complete_action = "Review saved file notes.",
      missing_action = "Run and save a data file scan."
    )
  ))

  attrs <- text_query(
    conn,
    "SELECT data_field_desc, data_field_units FROM dataset_attribute_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  attr_status <- if (nrow(attrs) == 0) {
    "not_started"
  } else {
    missing_desc <- sum(
      is.na(attrs$data_field_desc) |
        trimws(as.character(attrs$data_field_desc)) == ""
    )

    if (missing_desc == 0L) "complete" else "partial"
  }

  out <- c(out, list(data.table(
    section = "Attribute metadata",
    status = attr_status,
    status_label = status_label(attr_status),
    required_missing = if (nrow(attrs) == 0) 1L else sum(
      is.na(attrs$data_field_desc) |
        trimws(as.character(attrs$data_field_desc)) == ""
    ),
    missing_fields = if (attr_status == "complete") "" else "Field descriptions",
    saved_records = nrow(attrs),
    next_action = if (attr_status == "complete") {
      "Review attribute descriptions if needed."
    } else if (nrow(attrs) > 0) {
      "Add field descriptions and units where needed."
    } else {
      "Profile a selected vector/geodatabase file."
    }
  )))

  n_supporting <- count_query(
    conn,
    "SELECT COUNT(*) AS n FROM supporting_document_tbl WHERE dataset_id = ? AND active = 1",
    list(dataset_id)
  )

  reviewed_supporting <- count_query(
    conn,
    "
    SELECT COUNT(*) AS n
    FROM supporting_document_tbl
    WHERE dataset_id = ?
      AND active = 1
      AND reviewed_at IS NOT NULL
    ",
    list(dataset_id)
  )

  supp_status <- if (n_supporting == 0) {
    "not_started"
  } else if (reviewed_supporting >= n_supporting) {
    "complete"
  } else {
    "partial"
  }

  out <- c(out, list(data.table(
    section = "Supporting documents",
    status = supp_status,
    status_label = status_label(supp_status),
    required_missing = if (supp_status == "complete") 0L else max(n_supporting - reviewed_supporting, 0L),
    missing_fields = if (supp_status == "complete") "" else "Unreviewed supporting documents",
    saved_records = n_supporting,
    next_action = if (supp_status == "complete") {
      "Review supporting documents if needed."
    } else {
      "Review the detected document types or accept the defaults."
    }
  )))

  ensure_dataset_lineage_detail_tables(conn)
  lin <- text_query(
    conn,
    "SELECT * FROM dataset_lineage_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  linked_lineage_documents <- count_query(
    conn,
    paste(
      "SELECT COUNT(*) AS n FROM supporting_document_tbl",
      "WHERE dataset_id = ? AND active = 1 AND include_in_lineage = 1"
    ),
    list(dataset_id)
  )

  lineage_sources <- text_query(
    conn,
    "
    SELECT s.title, s.description, d.document_rel_path AS supporting_document_path
    FROM dataset_lineage_source_tbl s
    LEFT JOIN supporting_document_tbl d
      ON d.id = s.supporting_document_id AND d.active = 1
    WHERE s.dataset_id = ?
    ",
    list(dataset_id)
  )
  lineage_steps <- text_query(
    conn,
    "
    SELECT description
    FROM dataset_lineage_process_step_tbl
    WHERE dataset_id = ?
    ",
    list(dataset_id)
  )

  valid_sources <- if (nrow(lineage_sources) == 0L) 0L else sum(
    vapply(seq_len(nrow(lineage_sources)), function(i) {
      non_empty_scalar(lineage_sources$title[i]) ||
        non_empty_scalar(lineage_sources$description[i]) ||
        non_empty_scalar(lineage_sources$supporting_document_path[i])
    }, logical(1))
  )
  valid_steps <- if (nrow(lineage_steps) == 0L) 0L else sum(
    vapply(lineage_steps$description, non_empty_scalar, logical(1))
  )

  lineage_source_mode <- if (nrow(lin) == 0L) {
    "form"
  } else {
    as.character(lin$lineage_source_mode[1] %||% "form")
  }

  lineage_detail_complete <- if (nrow(lin) == 0L) {
    FALSE
  } else if (identical(lineage_source_mode, "supporting_documents")) {
    linked_lineage_documents > 0L || valid_sources > 0L
  } else {
    valid_sources > 0L || valid_steps > 0L
  }

  lineage_complete <- nrow(lin) > 0 &&
    non_empty_scalar(lin$lineage_statement[1]) &&
    (lineage_detail_complete ||
       (population_profile == "rough" && lineage_source_mode != "supporting_documents"))

  lineage_status <- if (nrow(lin) == 0) {
    "not_started"
  } else if (lineage_complete) {
    "complete"
  } else {
    "partial"
  }

  out <- c(out, list(data.table(
    section = "Lineage",
    status = lineage_status,
    status_label = status_label(lineage_status),
    required_missing = if (lineage_status == "complete") 0L else 1L,
    missing_fields = if (lineage_status == "complete") {
      ""
    } else if (identical(lineage_source_mode, "supporting_documents")) {
      "Lineage statement plus at least one linked supporting document"
    } else if (population_profile == "rough") {
      "Short lineage summary"
    } else {
      "Lineage statement plus source/process description"
    },
    saved_records = nrow(lin) + nrow(lineage_sources) + nrow(lineage_steps),
    next_action = if (lineage_status == "complete") {
      "Review lineage if needed."
    } else {
      if (identical(lineage_source_mode, "supporting_documents")) {
        "Add a short summary and link a supporting lineage document."
      } else if (population_profile == "rough") {
        "Add a short summary of the source and how the dataset was produced."
      } else {
        "Add a short summary and at least one source or process step."
      }
    }
  )))

  n_reviews <- count_query(
    conn,
    "SELECT COUNT(*) AS n FROM metadata_review_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  out <- c(out, list(
    related_count_status(
      section = "Metadata review",
      n = n_reviews,
      complete_action = "Review history if needed.",
      missing_action = "Save a metadata review/update record."
    )
  ))

  # Extracted records require an explicit notes or bbox-inclusion review.
  # A valid active manual extent is reviewed by virtue of its explicit save.
  n_spatial_records <- count_query(
    conn,
    "
    SELECT COUNT(*) AS n
    FROM data_file_spatial_metadata_tbl
    WHERE dataset_id = ?
    ",
    list(dataset_id)
  )

  reviewed_spatial_records <- count_query(
    conn,
    "
    SELECT COUNT(*) AS n
    FROM data_file_spatial_metadata_tbl
    WHERE dataset_id = ?
      AND (
        notes_updated_at IS NOT NULL
        OR bbox_inclusion_updated_at IS NOT NULL
      )
    ",
    list(dataset_id)
  )

  n_manual_extents <- count_query(
    conn,
    "
    SELECT COUNT(*) AS n
    FROM dataset_manual_spatial_extent_tbl
    WHERE dataset_id = ?
      AND COALESCE(active, 1) = 1
      AND LOWER(COALESCE(bbox_status, '')) = 'valid'
    ",
    list(dataset_id)
  )

  n_spatial_saved <-
    n_spatial_records + n_manual_extents

  n_spatial_reviewed <-
    reviewed_spatial_records + n_manual_extents

  spatial_required <- metadata_scope_requires_geographic_discovery(scope_code) &&
    !(has_named_area && n_spatial_saved == 0L)
  spatial_status <- if (!spatial_required) {
    "not_applicable"
  } else if (n_spatial_saved == 0L) {
    "not_started"
  } else if (n_spatial_reviewed >= n_spatial_saved) {
    "complete"
  } else if (n_spatial_reviewed == 0L) {
    "needs_review"
  } else {
    "partial"
  }

  spatial_missing <- if (!spatial_required) 0L else max(
    n_spatial_saved - n_spatial_reviewed, 0L
  )

  out <- c(out, list(data.table(
    section = "Spatial metadata",
    status = spatial_status,
    status_label = status_label(spatial_status),
    required_missing = spatial_missing,
    missing_fields = if (spatial_missing == 0L) {
      ""
    } else {
      "Unreviewed extracted spatial records"
    },
    saved_records = n_spatial_saved,
    next_action = switch(
      spatial_status,
      complete = "Review spatial records or extents if circumstances change.",
      partial = "Review the remaining extracted spatial records.",
      needs_review = "Review extracted spatial records and bbox inclusion.",
      not_applicable = if (has_named_area && topic_required)
        "A named area supplies geographic discovery; coordinate metadata is not required for this record."
        else "Spatial metadata is not required for the selected resource scope.",
      "Extract spatial metadata or save a manual extent."
    )
  )))

  data.table::rbindlist(out, fill = TRUE)
}
