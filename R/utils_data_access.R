# utils_data_access.R
# Shared helpers for dataset-level data access metadata stored in entry_tbl.

# Canonical entry_tbl fields owned by mod_data_access.
data_access_entry_fields <- function() {
  c(
    "data_distribution_url",
    "data_distributor_organisation_id",
    "data_distributor",
    "data_distributor_other",
    "access_constraints",
    "use_limitations",
    "embargo_status",
    "embargo_reason",
    "embargo_end_date"
  )
}

# Ensure the data-access fields exist for each active data pond.
# This is safe to call repeatedly.
ensure_entry_data_access_columns <- function(base_path) {
  if (
    is.null(base_path) ||
    length(base_path) == 0L ||
    is.na(base_path[1]) ||
    !nzchar(trimws(as.character(base_path[1])))
  ) {
    return(invisible(FALSE))
  }

  conn <- get_conn(as.character(base_path[1]))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  fields <- data_access_entry_fields()
  types <- stats::setNames(
    rep("TEXT", length(fields)),
    fields
  )
  types["data_distributor_organisation_id"] <- "INTEGER"

  ensure_columns(
    conn = conn,
    table = "entry_tbl",
    fields = fields,
    types = types
  )

  invisible(TRUE)
}

# Return the first usable scalar character value.
data_access_scalar <- function(x, default = "") {
  if (
    is.null(x) ||
    length(x) == 0L ||
    all(is.na(x))
  ) {
    return(default)
  }

  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return(default)
  }

  x[1]
}

# Safely read a field from a one-row entry_tbl record.
data_access_record_value <- function(rec, field, default = "") {
  if (
    is.null(rec) ||
    nrow(as.data.frame(rec)) == 0L ||
    !field %in% names(rec)
  ) {
    return(default)
  }

  data_access_scalar(
    rec[[field]],
    default = default
  )
}

# Normalize the stored embargo selection to the UI values used by the module.
normalise_embargo_status <- function(x, default = "No") {
  x <- tolower(
    data_access_scalar(
      x,
      default = default
    )
  )

  if (x %in% c("yes", "y", "true", "1")) {
    return("Yes")
  }

  if (x %in% c("no", "n", "false", "0")) {
    return("No")
  }

  default
}

# Build the row inserted into the Dataset status table.
# Required according to the current metadata schema:
#   access_constraints, use_limitations, embargo_status
# Conditional:
#   embargo_end_date when embargo_status == Yes
# Distribution fields and embargo_reason remain optional.
data_access_status_row <- function(rec) {
  fields <- data_access_entry_fields()

  has_any_access_value <- any(
    vapply(
      fields,
      function(field) {
        nzchar(
          data_access_record_value(
            rec,
            field,
            default = ""
          )
        )
      },
      logical(1)
    )
  )

  embargo_status_raw <-
    data_access_record_value(
      rec,
      "embargo_status",
      default = ""
    )

  embargo_status <- if (
    nzchar(embargo_status_raw)
  ) {
    normalise_embargo_status(
      embargo_status_raw,
      default = ""
    )
  } else {
    ""
  }

  embargo_end_date <-
    data_access_record_value(
      rec,
      "embargo_end_date",
      default = ""
    )

  distribution_url <-
    data_access_record_value(
      rec,
      "data_distribution_url",
      default = ""
    )

  distributor_id <-
    data_access_record_value(
      rec,
      "data_distributor_organisation_id",
      default = ""
    )

  distributor_other <-
    data_access_record_value(
      rec,
      "data_distributor_other",
      default = ""
    )

  missing <- character()

  if (!embargo_status %in% c(
    "Yes",
    "No"
  )) {
    missing <- c(
      missing,
      "Embargo status"
    )
  }

  if (
    identical(
      embargo_status,
      "Yes"
    ) &&
    !nzchar(
      embargo_end_date
    )
  ) {
    missing <- c(
      missing,
      "Embargo end date"
    )
  }

  if (
    nzchar(distribution_url) &&
    !nzchar(distributor_id) &&
    !nzchar(distributor_other)
  ) {
    missing <- c(
      missing,
      "Distribution organisation"
    )
  }

  status <- if (!has_any_access_value) {
    "not_started"
  } else if (length(missing) == 0L) {
    "complete"
  } else {
    "partial"
  }

  data.table::data.table(
    section =
      "Distribution and embargo",

    status =
      status,

    status_label =
      status_label(
        status
      ),

    required_missing =
      length(missing),

    missing_fields =
      paste(
        missing,
        collapse = "; "
      ),

    saved_records =
      if (has_any_access_value) {
        1L
      } else {
        0L
      },

    next_action = switch(
      status,

      complete =
        "Review distribution or embargo details if needed.",

      partial =
        "Complete the missing distribution or embargo fields.",

      "Record whether an embargo applies."
    )
  )
}
