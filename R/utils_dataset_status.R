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

get_dataset_completion_status <- function(dataset_id, base_path, schema_grouped) {
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

  citation_dt <- schema_section(schema_grouped, "Dataset overview", "Citation")
  about_dt <- schema_section(schema_grouped, "Dataset overview", "About")

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
    "SELECT role FROM dataset_organisation_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  n_org <- nrow(org_links)
  has_owner_or_custodian <- n_org > 0 &&
    any(org_links$role %in% c("owner", "custodian"), na.rm = TRUE)

  org_status <- if (n_org == 0) {
    "not_started"
  } else if (has_owner_or_custodian) {
    "complete"
  } else {
    "partial"
  }

  out <- c(out, list(data.table(
    section = "Organisations",
    status = org_status,
    status_label = status_label(org_status),
    required_missing = if (org_status == "complete") 0L else 1L,
    missing_fields = if (org_status == "complete") "" else "Owner or custodian organisation",
    saved_records = n_org,
    next_action = if (org_status == "complete") {
      "Review organisations if needed."
    } else {
      "Add an owner or custodian organisation."
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

  if (nrow(lic) > 0L) {
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

  lic_status <- if (nrow(lic) == 0L) {
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
          nrow(lic),

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
      AND document_type_user_set = 1
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
    missing_fields = if (supp_status == "complete") "" else "Unreviewed document classifications",
    saved_records = n_supporting,
    next_action = if (supp_status == "complete") {
      "Review supporting documents if needed."
    } else {
      "Scan or classify supporting documents."
    }
  )))

  lin <- text_query(
    conn,
    "SELECT * FROM dataset_lineage_tbl WHERE dataset_id = ?",
    list(dataset_id)
  )

  lineage_complete <- nrow(lin) > 0 &&
    non_empty_scalar(lin$lineage_statement[1]) &&
    (
      non_empty_scalar(lin$source_description[1]) ||
        non_empty_scalar(lin$process_description[1])
    )

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
    missing_fields = if (lineage_status == "complete") "" else "Lineage statement plus source/process description",
    saved_records = nrow(lin),
    next_action = if (lineage_status == "complete") {
      "Review lineage if needed."
    } else {
      "Complete and save lineage."
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

  # Spatial metadata is currently extracted in-memory in the data files module.
  # There is no dedicated saved spatial metadata table in the attached schema yet.
  out <- c(out, list(data.table(
    section = "Spatial metadata",
    status = "not_started",
    status_label = status_label("not_started"),
    required_missing = NA_integer_,
    missing_fields = "No saved spatial metadata table yet",
    saved_records = 0L,
    next_action = "Run spatial extraction in Data files. Add persistence later if required."
  )))

  data.table::rbindlist(out, fill = TRUE)
}
