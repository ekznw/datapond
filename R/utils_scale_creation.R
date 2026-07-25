# Dataset creation / compilation scale metadata ----
#
# Existing metadata-schema meanings:
#
#   max_scale:
#     Largest cartographic scale, therefore the smallest denominator.
#     Example: 5000 for 1:5,000.
#
#   min_scale:
#     Smallest cartographic scale, therefore the largest denominator.
#     Example: 50000 for 1:50,000.
#
# Do not reverse this mapping without also migrating the original schema.

creation_scale_entry_fields <- function() {
  c(
    "max_scale",
    "min_scale",
    "creation_scale_note"
  )
}


ensure_entry_creation_scale_columns_conn <- function(conn) {
  ensure_columns(
    conn = conn,
    table = "entry_tbl",
    fields = creation_scale_entry_fields(),
    types = c(
      max_scale = "INTEGER",
      min_scale = "INTEGER",
      creation_scale_note = "TEXT"
    )
  )

  invisible(TRUE)
}


ensure_entry_creation_scale_columns <- function(base_path) {
  conn <- get_conn(
    base_path
  )

  on.exit(
    DBI::dbDisconnect(conn),
    add = TRUE
  )

  ensure_entry_creation_scale_columns_conn(
    conn
  )
}


creation_scale_numeric <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }

  value <- trimws(
    as.character(x[1])
  )

  if (!nzchar(value)) {
    return(NA_real_)
  }

  value <- gsub(
    ",",
    "",
    value,
    fixed = TRUE
  )

  value <- suppressWarnings(
    as.numeric(value)
  )

  if (
    length(value) == 0L ||
    !is.finite(value)
  ) {
    return(NA_real_)
  }

  value
}


creation_scale_text <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    all(is.na(x))
  ) {
    return(NA_character_)
  }

  value <- trimws(
    as.character(x[1])
  )

  if (!nzchar(value)) {
    return(NA_character_)
  }

  value
}


format_scale_denominator <- function(x) {
  x <- creation_scale_numeric(
    x
  )

  if (!is.finite(x)) {
    return(NA_character_)
  }

  paste0(
    "1:",
    format(
      as.integer(round(x)),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}


format_creation_scale_range <- function(
  max_scale,
  min_scale
) {
  max_scale <- creation_scale_numeric(
    max_scale
  )

  min_scale <- creation_scale_numeric(
    min_scale
  )

  if (
    !is.finite(max_scale) ||
    !is.finite(min_scale)
  ) {
    return("Not recorded")
  }

  largest_scale <- format_scale_denominator(
    max_scale
  )

  smallest_scale <- format_scale_denominator(
    min_scale
  )

  if (identical(
    as.integer(round(max_scale)),
    as.integer(round(min_scale))
  )) {
    return(largest_scale)
  }

  paste(
    largest_scale,
    "to",
    smallest_scale
  )
}


validate_creation_scale_metadata <- function(
  max_scale,
  min_scale,
  creation_scale_note
) {
  max_scale <- creation_scale_numeric(
    max_scale
  )

  min_scale <- creation_scale_numeric(
    min_scale
  )

  note <- creation_scale_text(
    creation_scale_note
  )

  has_max_scale <- is.finite(
    max_scale
  )

  has_min_scale <- is.finite(
    min_scale
  )

  has_note <- !is.na(note)

  # Completely empty is allowed for an incomplete/draft record.
  if (
    !has_max_scale &&
    !has_min_scale &&
    !has_note
  ) {
    return(
      list(
        valid = TRUE,
        status = "not_recorded",
        message = paste(
          "Creation or compilation scale has not been recorded."
        ),
        values = list(
          max_scale = NA_integer_,
          min_scale = NA_integer_,
          creation_scale_note = NA_character_
        )
      )
    )
  }

  # A note may be saved without numeric scales for cases where scale is
  # unknown, mixed, inherited or not applicable.
  if (
    !has_max_scale &&
    !has_min_scale &&
    has_note
  ) {
    return(
      list(
        valid = TRUE,
        status = "note_only",
        message = paste(
          "A scale note will be saved without numeric",
          "creation-scale denominators."
        ),
        values = list(
          max_scale = NA_integer_,
          min_scale = NA_integer_,
          creation_scale_note = note
        )
      )
    )
  }

  if (xor(
    has_max_scale,
    has_min_scale
  )) {
    return(
      list(
        valid = FALSE,
        status = "incomplete_range",
        message = paste(
          "Enter both creation-scale denominators.",
          "For a single creation scale, enter the same",
          "denominator in both fields."
        ),
        values = NULL
      )
    )
  }

  if (
    max_scale <= 0 ||
    min_scale <= 0
  ) {
    return(
      list(
        valid = FALSE,
        status = "invalid_denominator",
        message = paste(
          "Scale denominators must be positive numbers."
        ),
        values = NULL
      )
    )
  }

  whole_numbers <- all(
    abs(
      c(
        max_scale,
        min_scale
      ) -
        round(
          c(
            max_scale,
            min_scale
          )
        )
    ) <
      sqrt(.Machine$double.eps)
  )

  if (!isTRUE(whole_numbers)) {
    return(
      list(
        valid = FALSE,
        status = "non_integer_denominator",
        message = paste(
          "Scale denominators must be whole numbers."
        ),
        values = NULL
      )
    )
  }

  # max_scale is the largest cartographic scale and therefore has
  # the numerically smaller denominator.
  if (max_scale > min_scale) {
    return(
      list(
        valid = FALSE,
        status = "reversed_scale_range",
        message = paste(
          "The largest creation scale must have the smaller",
          "denominator. For example, enter 5,000 in the first",
          "field and 50,000 in the second field."
        ),
        values = NULL
      )
    )
  }

  if (!has_note) {
    return(
      list(
        valid = FALSE,
        status = "missing_scale_note",
        message = paste(
          "Add a creation-scale note explaining how the scale",
          "or scale range was determined."
        ),
        values = NULL
      )
    )
  }

  max_scale <- as.integer(
    round(max_scale)
  )

  min_scale <- as.integer(
    round(min_scale)
  )

  list(
    valid = TRUE,
    status = "valid",
    message = paste(
      "Creation / compilation scale:",
      format_creation_scale_range(
        max_scale,
        min_scale
      )
    ),
    values = list(
      max_scale = max_scale,
      min_scale = min_scale,
      creation_scale_note = note
    )
  )
}


creation_scale_record_value <- function(
  rec,
  field,
  default = NA
) {
  if (
    is.null(rec) ||
    nrow(as.data.frame(rec)) == 0L ||
    !field %in% names(rec)
  ) {
    return(default)
  }

  value <- rec[[field]]

  if (
    is.null(value) ||
    length(value) == 0L ||
    all(is.na(value))
  ) {
    return(default)
  }

  value[1]
}