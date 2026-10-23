# Keyword text uses semicolons as separators; commas belong to the term.
split_keyword_values <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  x <- trimws(unlist(strsplit(x[!is.na(x)], ";", fixed = TRUE)))
  x <- x[nzchar(x)]
  x[!duplicated(tolower(x))]
}

normalise_keyword_values <- function(x, choices = NULL) {
  selected <- split_keyword_values(x)
  if (!length(choices)) return(selected)
  values <- unname(as.character(choices))
  labels <- names(choices)
  matched <- match(tolower(selected), tolower(values))
  # Only resolve unambiguous display labels, and prefer exact stored values.
  if (!is.null(labels)) {
    unique_label <- !duplicated(tolower(labels)) &
      !duplicated(tolower(labels), fromLast = TRUE)
    label_match <- match(tolower(selected), tolower(labels))
    use_label <- is.na(matched) & !is.na(label_match)
    use_label[use_label] <- unique_label[label_match[use_label]]
    matched[use_label] <- label_match[use_label]
  }
  selected[!is.na(matched)] <- values[matched[!is.na(matched)]]
  selected[!duplicated(tolower(selected))]
}

prepare_keyword_vocab_seed <- function(vocab_dt) {
  if (is.null(vocab_dt) || !nrow(vocab_dt)) stop("Keyword CSV must contain at least one term.")
  dt <- as.data.frame(vocab_dt, stringsAsFactors = FALSE)
  required <- c("field_code", "option_value", "option_label")
  missing <- setdiff(required, names(dt))
  if (length(missing)) stop("Keyword seed is missing columns: ", paste(missing, collapse = ", "))
  for (field in required) dt[[field]] <- trimws(as.character(dt[[field]]))
  original_fields <- dt$field_code
  dt$field_code <- vapply(dt$field_code, canonical_keyword_field_code, character(1))
  if (anyNA(dt[required]) || any(!nzchar(as.matrix(dt[required])))) {
    stop("Keyword CSV fields, values and labels must not be blank.")
  }
  if (any(!dt$field_code %in% c("keyword_object", "keyword_discipline", "keyword_geo"))) {
    stop("Keyword CSV contains an unsupported vocabulary group.")
  }
  if (any(grepl("[;\r\n]", dt$option_value)) || any(grepl("[;\r\n]", dt$option_label))) {
    stop("Keyword CSV terms must not contain semicolons or line breaks.")
  }
  if (!"sort_order" %in% names(dt)) {
    dt$sort_order <- ave(seq_len(nrow(dt)), dt$field_code, FUN = seq_along)
  }
  order <- suppressWarnings(as.numeric(dt$sort_order))
  if (anyNA(order) || any(!is.finite(order) | order < 0 | order > .Machine$integer.max | order != floor(order))) {
    stop("Keyword CSV sort order must contain non-negative whole numbers.")
  }
  dt$sort_order <- as.integer(order)
  key <- paste(dt$field_code, tolower(dt$option_value), sep = "\r")
  # Continue accepting the former topic group, with canonical themes winning.
  for (i in which(duplicated(key))) {
    first <- match(key[i], key)
    if (identical(original_fields[i], original_fields[first]) ||
        !"keyword_topic" %in% c(original_fields[i], original_fields[first])) {
      stop("Keyword CSV contains duplicate stored values: ", dt$option_value[i])
    }
  }
  dt <- dt[order(original_fields == "keyword_topic"), , drop = FALSE]
  key <- paste(dt$field_code, tolower(dt$option_value), sep = "\r")
  dt <- dt[!duplicated(key), c(required, "sort_order"), drop = FALSE]
  label_key <- paste(dt$field_code, tolower(dt$option_label), sep = "\r")
  if (anyDuplicated(label_key)) stop("Keyword CSV contains duplicate display labels within a group.")
  rownames(dt) <- NULL
  dt
}

keyword_seed_snapshot <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT * FROM keyword_vocab_tbl ORDER BY id")
}

keyword_seed_plan <- function(seed, existing, mode) {
  key <- function(x) paste(tolower(x$field_code), tolower(x$option_value), sep = "\r")
  matched <- match(key(seed), key(existing))
  action <- rep("Add", nrow(seed))
  found <- !is.na(matched)
  action[found] <- "Preserve"
  if (mode != "add") {
    managed <- found
    managed[found] <- existing$source[matched[found]] == "seed_csv"
    action[managed] <- "Update / reactivate"
    unchanged <- managed
    indices <- which(managed)
    unchanged[indices] <- vapply(indices, function(i) {
      row <- existing[matched[i], ]
      identical(row$option_label, seed$option_label[i]) &&
        isTRUE(row$sort_order == seed$sort_order[i]) &&
        isTRUE(row$is_active == 1L) && identical(row$approval_status, "approved")
    }, logical(1))
    action[unchanged] <- "Unchanged"
  }
  plan <- data.frame(seed, action = action, id = existing$id[matched])
  if (mode == "reset") {
    removed <- existing[existing$source == "seed_csv" & existing$is_active == 1L &
      !key(existing) %in% key(seed), , drop = FALSE]
    if (nrow(removed)) plan <- rbind(plan, data.frame(
      removed[names(seed)], action = "Deactivate", id = removed$id
    ))
  }
  for (i in which(plan$action %in% c("Add", "Update / reactivate"))) {
    conflict <- existing$field_code == plan$field_code[i] &
      tolower(existing$option_label) == tolower(plan$option_label[i]) &
      tolower(existing$option_value) != tolower(plan$option_value[i])
    if (any(conflict, na.rm = TRUE)) plan$action[i] <- "Conflict: label already used"
  }
  previous <- match(plan$id, existing$id)
  plan$previous_label <- existing$option_label[previous]
  plan$previous_order <- existing$sort_order[previous]
  plan
}

preview_keyword_vocab_seed <- function(vocab_dt, base_path, mode = c("add", "update", "reset")) {
  mode <- match.arg(mode)
  seed <- prepare_keyword_vocab_seed(vocab_dt)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  existing <- keyword_seed_snapshot(conn)
  list(seed = seed, existing = existing, mode = mode,
       plan = keyword_seed_plan(seed, existing, mode),
       pond = normalizePath(base_path, mustWork = TRUE))
}

apply_keyword_vocab_seed <- function(vocab_dt, base_path, user_id = NULL,
                                     mode = c("add", "update", "reset"), preview = NULL) {
  mode <- match.arg(mode)
  # Validate before opening the transaction or changing any terms.
  seed <- prepare_keyword_vocab_seed(vocab_dt)
  user_id <- normalise_optional_user_id(user_id)
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbWithTransaction(conn, {
    existing <- keyword_seed_snapshot(conn)
    if (!is.null(preview) && (!identical(seed, preview$seed) ||
        !identical(existing, preview$existing) || !identical(mode, preview$mode) ||
        !identical(normalizePath(base_path, mustWork = TRUE), preview$pond))) {
      stop("The vocabulary or CSV changed. Preview the changes again before applying them.")
    }
    plan <- keyword_seed_plan(seed, existing, mode)
    if (any(startsWith(plan$action, "Conflict"))) {
      stop("The CSV has display labels already used by other local terms. Resolve the conflicts before applying it.")
    }
    for (i in seq_len(nrow(plan))) {
      row <- plan[i, ]
      if (row$action == "Add") {
        DBI::dbExecute(conn, paste(
          "INSERT INTO keyword_vocab_tbl (field_code, option_value, option_label, sort_order,",
          "is_active, source, approval_status, created_at, created_by_user_id, updated_at, updated_by_user_id)",
          "VALUES (?, ?, ?, ?, 1, 'seed_csv', 'approved', ?, ?, ?, ?)"
        ), params = list(row$field_code, row$option_value, row$option_label,
          row$sort_order, now, user_id, now, user_id))
      } else if (row$action == "Update / reactivate") {
        DBI::dbExecute(conn, paste(
          "UPDATE keyword_vocab_tbl SET option_label = ?, sort_order = ?, is_active = 1,",
          "approval_status = 'approved', updated_at = ?, updated_by_user_id = ? WHERE id = ?"
        ), params = list(row$option_label, row$sort_order, now, user_id, row$id))
      } else if (row$action == "Deactivate") {
        DBI::dbExecute(conn, paste(
          "UPDATE keyword_vocab_tbl SET is_active = 0, updated_at = ?,",
          "updated_by_user_id = ? WHERE id = ?"
        ), params = list(now, user_id, row$id))
      }
    }
    data.frame(inserted = sum(plan$action == "Add"),
      updated = sum(plan$action == "Update / reactivate"),
      preserved = sum(plan$action %in% c("Preserve", "Unchanged")),
      deactivated = sum(plan$action == "Deactivate"))
  })
}

merge_keyword_vocab_seed <- function(vocab_dt, base_path, user_id = NULL) {
  apply_keyword_vocab_seed(vocab_dt, base_path, user_id, mode = "add")
}

reset_keyword_vocab_seed <- function(vocab_dt, base_path, user_id = NULL) {
  apply_keyword_vocab_seed(vocab_dt, base_path, user_id, mode = "reset")
}

seed_keyword_vocab_if_empty <- function(vocab_dt, base_path, user_id = NULL) {
  conn <- get_conn(base_path)
  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM keyword_vocab_tbl")$n[1]
  DBI::dbDisconnect(conn)
  if (n > 0L) return(data.frame(inserted = 0L, updated = 0L, preserved = n, deactivated = 0L))
  merge_keyword_vocab_seed(vocab_dt, base_path, user_id)
}

keyword_vocab_usage <- function(conn, vocabulary) {
  counts <- integer(nrow(vocabulary))
  fields <- intersect(c("keyword_object", "keyword_topic", "keyword_discipline", "keyword_geo"),
    DBI::dbListFields(conn, "entry_tbl"))
  for (field in fields) {
    entries <- DBI::dbGetQuery(conn, paste("SELECT", field, "FROM entry_tbl"))[[field]]
    indices <- which(vocabulary$field_code == canonical_keyword_field_code(field))
    if (!length(indices)) next
    choices <- stats::setNames(vocabulary$option_value[indices], vocabulary$option_label[indices])
    for (entry in entries) {
      terms <- normalise_keyword_values(entry, choices)
      counts[indices] <- counts[indices] + as.integer(tolower(vocabulary$option_value[indices]) %in% tolower(terms))
    }
  }
  counts
}
