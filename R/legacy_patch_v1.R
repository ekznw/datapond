# Retirable data conversions for legacy-v1. Keep original source columns.
# Permanent schema setup and row completion markers live outside this file.

legacy_patch_v1_keywords <- function(conn) {
  entry_fields <- DBI::dbListFields(conn, "entry_tbl")
  if ("keyword_topic" %in% entry_fields && !"keyword_object" %in% entry_fields) {
    ensure_columns(conn, "entry_tbl", "keyword_object")
    entry_fields <- c(entry_fields, "keyword_object")
  }
  if (all(c("keyword_object", "keyword_topic") %in% entry_fields)) {
    records <- DBI::dbGetQuery(conn,
      "SELECT id, keyword_object, keyword_topic FROM entry_tbl
       WHERE NULLIF(TRIM(COALESCE(keyword_topic, '')), '') IS NOT NULL")
    for (i in seq_len(nrow(records))) DBI::dbExecute(conn,
      "UPDATE entry_tbl SET keyword_object = ?, keyword_topic = NULL WHERE id = ?",
      params = list(merge_keyword_theme_values(records$keyword_object[i], records$keyword_topic[i]), records$id[i]))
  }

  object_thesaurus <- DBI::dbGetQuery(conn,
    "SELECT id FROM keyword_thesaurus_tbl WHERE field_code = 'keyword_object' COLLATE NOCASE LIMIT 1")
  topic_thesaurus <- DBI::dbGetQuery(conn,
    "SELECT id FROM keyword_thesaurus_tbl WHERE field_code = 'keyword_topic' COLLATE NOCASE LIMIT 1")
  if (!nrow(object_thesaurus) && nrow(topic_thesaurus)) {
    DBI::dbExecute(conn,
      "UPDATE keyword_thesaurus_tbl SET field_code = 'keyword_object', updated_at = CURRENT_TIMESTAMP WHERE id = ?",
      params = list(topic_thesaurus$id[1]))
    object_thesaurus <- topic_thesaurus
  }
  canonical_thesaurus_id <- if (nrow(object_thesaurus)) as.integer(object_thesaurus$id[1]) else NA_integer_
  if (nrow(object_thesaurus) && nrow(topic_thesaurus) &&
      object_thesaurus$id[1] != topic_thesaurus$id[1]) {
    DBI::dbExecute(conn,
      "UPDATE keyword_vocab_tbl SET thesaurus_id = ? WHERE thesaurus_id = ?",
      params = list(canonical_thesaurus_id, as.integer(topic_thesaurus$id[1])))
    DBI::dbExecute(conn,
      "DELETE FROM keyword_thesaurus_tbl WHERE id = ?",
      params = list(as.integer(topic_thesaurus$id[1])))
  }

  topic_rows <- DBI::dbGetQuery(conn,
    "SELECT * FROM keyword_vocab_tbl WHERE field_code = 'keyword_topic' COLLATE NOCASE ORDER BY id")
  for (i in seq_len(nrow(topic_rows))) {
    existing <- DBI::dbGetQuery(conn,
      "SELECT id, is_active, approval_status FROM keyword_vocab_tbl
       WHERE field_code = 'keyword_object' COLLATE NOCASE AND option_value = ? COLLATE NOCASE LIMIT 1",
      params = list(topic_rows$option_value[i]))
    if (nrow(existing)) {
      approval_rank <- c(rejected = 1L, pending = 2L, approved = 3L)
      statuses <- c(existing$approval_status[1], topic_rows$approval_status[i])
      status <- statuses[which.max(unname(approval_rank[statuses]))]
      DBI::dbExecute(conn,
        "UPDATE keyword_vocab_tbl SET is_active = ?, approval_status = ?,
         thesaurus_id = COALESCE(?, thesaurus_id), updated_at = CURRENT_TIMESTAMP WHERE id = ?",
        params = list(max(as.integer(existing$is_active[1]), as.integer(topic_rows$is_active[i]), na.rm = TRUE),
                      status, canonical_thesaurus_id, existing$id[1]))
      DBI::dbExecute(conn, "DELETE FROM keyword_vocab_tbl WHERE id = ?", params = list(topic_rows$id[i]))
    } else {
      DBI::dbExecute(conn,
        "UPDATE keyword_vocab_tbl SET field_code = 'keyword_object', thesaurus_id = COALESCE(?, thesaurus_id),
         updated_at = CURRENT_TIMESTAMP WHERE id = ?",
        params = list(canonical_thesaurus_id, topic_rows$id[i]))
    }
  }
  if (!is.na(canonical_thesaurus_id)) DBI::dbExecute(conn,
    "UPDATE keyword_vocab_tbl SET thesaurus_id = ? WHERE field_code = 'keyword_object' COLLATE NOCASE",
    params = list(canonical_thesaurus_id))
  invisible(TRUE)
}

legacy_patch_v1_citation <- function(conn) {
  rows <- DBI::dbGetQuery(conn, "
    SELECT e.id, e.citation_date
    FROM entry_tbl e
    WHERE e.legacy_citation_v1_migrated_at IS NULL AND e.citation_date IS NOT NULL AND TRIM(e.citation_date) != ''
      AND NOT EXISTS (
        SELECT 1 FROM dataset_citation_date_tbl d
        WHERE d.dataset_id = e.id AND d.date_type_code = 'publication'
      )
    ORDER BY e.id
  ")
  if (nrow(rows) > 0L) {
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    for (i in seq_len(nrow(rows))) {
      next_order <- DBI::dbGetQuery(
        conn,
        "SELECT COALESCE(MAX(sort_order), 0) + 1 AS n FROM dataset_citation_date_tbl WHERE dataset_id = ?",
        params = list(rows$id[i])
      )$n[1]
      DBI::dbExecute(conn, "
        INSERT INTO dataset_citation_date_tbl (
          dataset_id, date_value, date_type_code, date_precision, sort_order,
          created_at, updated_at
        ) VALUES (?, ?, 'publication', ?, ?, ?, ?)
      ", params = list(
        rows$id[i], rows$citation_date[i], infer_iso_date_precision(rows$citation_date[i]),
        next_order, now, now
      ))
    }
  }
  invisible(TRUE)
}

legacy_patch_v1_wave2 <- function(conn) {
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (all(c("data_distribution_url", "temporal_coverage_start_date", "temporal_coverage_end_date") %in%
          DBI::dbListFields(conn, "entry_tbl"))) {
    DBI::dbExecute(conn, "
      INSERT INTO dataset_online_resource_tbl (
        dataset_id, resource_url, resource_name, function_code, is_primary,
        sort_order, created_at, updated_at
      )
      SELECT e.id, TRIM(e.data_distribution_url), 'Primary distribution resource',
        'information', 1, 1, ?, ?
      FROM entry_tbl e
      WHERE e.legacy_wave2_v1_migrated_at IS NULL AND e.data_distribution_url IS NOT NULL AND TRIM(e.data_distribution_url) != ''
        AND NOT EXISTS (
          SELECT 1 FROM dataset_online_resource_tbl r WHERE r.dataset_id = e.id
        )
    ", params = list(now, now))
    DBI::dbExecute(conn, "
      INSERT INTO dataset_temporal_extent_tbl (
        dataset_id, extent_label, start_value, end_value, start_precision,
        end_precision, start_indeterminate, end_indeterminate,
        sort_order, created_at, updated_at
      )
      SELECT e.id, 'Primary temporal extent',
        NULLIF(TRIM(e.temporal_coverage_start_date), ''),
        NULLIF(TRIM(e.temporal_coverage_end_date), ''),
        CASE
          WHEN LENGTH(TRIM(e.temporal_coverage_start_date)) = 4 THEN 'year'
          WHEN LENGTH(TRIM(e.temporal_coverage_start_date)) = 7 THEN 'month'
          WHEN INSTR(TRIM(e.temporal_coverage_start_date), 'T') > 0 THEN 'dateTime'
          ELSE 'day'
        END,
        CASE
          WHEN LENGTH(TRIM(e.temporal_coverage_end_date)) = 4 THEN 'year'
          WHEN LENGTH(TRIM(e.temporal_coverage_end_date)) = 7 THEN 'month'
          WHEN INSTR(TRIM(e.temporal_coverage_end_date), 'T') > 0 THEN 'dateTime'
          ELSE 'day'
        END,
        CASE WHEN NULLIF(TRIM(e.temporal_coverage_start_date), '') IS NULL THEN 'unknown' END,
        CASE WHEN NULLIF(TRIM(e.temporal_coverage_end_date), '') IS NULL THEN 'unknown' END,
        1, ?, ?
      FROM entry_tbl e
      WHERE e.legacy_wave2_v1_migrated_at IS NULL AND (NULLIF(TRIM(e.temporal_coverage_start_date), '') IS NOT NULL
          OR NULLIF(TRIM(e.temporal_coverage_end_date), '') IS NOT NULL)
        AND NOT EXISTS (
          SELECT 1 FROM dataset_temporal_extent_tbl t WHERE t.dataset_id = e.id
        )
    ", params = list(now, now))
  }
  if (all(c("embargo_status", "embargo_reason", "embargo_end_date") %in%
          DBI::dbListFields(conn, "entry_tbl"))) {
    DBI::dbExecute(conn, "
      INSERT INTO dataset_license_tbl (
        dataset_id, embargo_status, embargo_reason, embargo_end_date, updated_at
      )
      SELECT e.id, e.embargo_status, e.embargo_reason, e.embargo_end_date, ?
      FROM entry_tbl e
      WHERE e.legacy_wave2_v1_migrated_at IS NULL AND COALESCE(TRIM(e.embargo_status), '') != ''
        AND NOT EXISTS (SELECT 1 FROM dataset_license_tbl l WHERE l.dataset_id = e.id)
    ", params = list(now))
    DBI::dbExecute(conn, "
      UPDATE dataset_license_tbl
      SET embargo_status = COALESCE(NULLIF(TRIM(embargo_status), ''),
            (SELECT e.embargo_status FROM entry_tbl e WHERE e.id = dataset_license_tbl.dataset_id)),
          embargo_reason = COALESCE(NULLIF(TRIM(embargo_reason), ''),
            (SELECT e.embargo_reason FROM entry_tbl e WHERE e.id = dataset_license_tbl.dataset_id)),
          embargo_end_date = COALESCE(NULLIF(TRIM(embargo_end_date), ''),
            (SELECT e.embargo_end_date FROM entry_tbl e WHERE e.id = dataset_license_tbl.dataset_id))
      WHERE dataset_id IN (SELECT id FROM entry_tbl WHERE legacy_wave2_v1_migrated_at IS NULL)
    ")
  }
  invisible(TRUE)
}

legacy_patch_v1_parties <- function(conn) {
  DBI::dbExecute(conn, "
    UPDATE organisation_tbl
    SET organisational_unit_name = organisation_department
    WHERE legacy_party_v1_migrated_at IS NULL AND (organisational_unit_name IS NULL OR TRIM(organisational_unit_name) = '')
      AND organisation_department IS NOT NULL AND TRIM(organisation_department) != ''
  ")
  # A legacy address is retained verbatim and copied only to the least
  # interpretive normalized component. Never attempt to parse its punctuation.
  DBI::dbExecute(conn, "
    INSERT INTO party_address_tbl (organisation_id, delivery_point, is_primary)
    SELECT o.id, o.organisation_address, 1
    FROM organisation_tbl o
    WHERE o.legacy_party_v1_migrated_at IS NULL AND o.organisation_address IS NOT NULL AND TRIM(o.organisation_address) != ''
      AND NOT EXISTS (
        SELECT 1 FROM party_address_tbl a WHERE a.organisation_id = o.id
      )
  ")
  invisible(TRUE)
}

legacy_patch_v1_licensing <- function(
  conn
) {
  # Existing open-licence records cannot reliably tell us
  # whether EKZNW or an external provider controlled the rights.
  # Mark those for user review rather than guessing ownership.
  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_license_tbl
    SET rights_basis_code =
      CASE
        WHEN license_identifier = 'PROVIDER-LICENCE'
          THEN 'external_licence'

        WHEN license_identifier =
          'REDISTRIBUTION-PERMISSION'
          THEN 'external_permission'

        WHEN license_identifier = 'NO-LICENCE'
          THEN 'rights_unknown'

        WHEN license_identifier IN (
          'EKZNW-INTERNAL',
          'EKZNW-PARTNER'
        )
          THEN 'ekznw_owned'

        WHEN license_identifier IS NOT NULL
          AND TRIM(license_identifier) != ''
          THEN 'review_required'

        ELSE NULL
      END
    WHERE legacy_licensing_v1_migrated_at IS NULL AND (rights_basis_code IS NULL OR TRIM(rights_basis_code) = '')
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_license_tbl
    SET access_level_code =
      CASE
        WHEN license_identifier = 'EKZNW-INTERNAL'
          THEN 'internal'

        WHEN license_identifier = 'EKZNW-PARTNER'
          THEN 'approved_partners'

        WHEN license_identifier = 'NO-LICENCE'
          THEN 'not_distributable'

        WHEN license_identifier IN (
          'CC-BY-4.0',
          'CC0-1.0',
          'CC-BY-SA-4.0',
          'CC-BY-NC-4.0',
          'ODbL-1.0'
        )
          THEN 'public'

        WHEN LOWER(COALESCE(access_level, ''))
          LIKE '%partner%'
          THEN 'approved_partners'

        WHEN LOWER(COALESCE(access_level, ''))
          LIKE '%internal%'
          THEN 'internal'

        WHEN LOWER(COALESCE(access_level, ''))
          LIKE '%embargo%'
          THEN 'embargoed'

        WHEN LOWER(COALESCE(access_level, ''))
          LIKE '%public%'
          THEN 'public'

        ELSE NULL
      END
    WHERE legacy_licensing_v1_migrated_at IS NULL AND (access_level_code IS NULL OR TRIM(access_level_code) = '')
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_license_tbl
    SET redistribution_permission_code =
      CASE
        WHEN license_identifier = 'CC0-1.0'
          THEN 'unrestricted'

        WHEN license_identifier = 'CC-BY-4.0'
          THEN 'permitted_with_attribution'

        WHEN license_identifier IN (
          'CC-BY-SA-4.0',
          'CC-BY-NC-4.0',
          'ODbL-1.0',
          'PROVIDER-LICENCE',
          'REDISTRIBUTION-PERMISSION',
          'CUSTOM-TERMS'
        )
          THEN 'permitted_with_conditions'

        WHEN license_identifier = 'EKZNW-PARTNER'
          THEN 'approved_recipients_only'

        WHEN license_identifier IN (
          'EKZNW-INTERNAL',
          'NO-LICENCE'
        )
          THEN 'prohibited'

        ELSE 'unknown'
      END
    WHERE legacy_licensing_v1_migrated_at IS NULL AND (redistribution_permission_code IS NULL OR TRIM(redistribution_permission_code) = '')
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_license_tbl
    SET required_attribution =
      CASE
        WHEN license_identifier IN (
          'CC-BY-4.0',
          'CC-BY-SA-4.0',
          'CC-BY-NC-4.0',
          'ODbL-1.0',
          'EKZNW-PARTNER'
        )
          THEN 1

        WHEN license_identifier = 'CC0-1.0'
          THEN 0

        ELSE 0
      END
    WHERE legacy_licensing_v1_migrated_at IS NULL AND required_attribution IS NULL
    "
  )

  DBI::dbExecute(
    conn,
    "
    UPDATE dataset_license_tbl
    SET sensitivity_handling_code =
      CASE
        WHEN COALESCE(sensitive_data, 0) = 1
          THEN 'review_required'

        ELSE 'none'
      END
    WHERE legacy_licensing_v1_migrated_at IS NULL AND (sensitivity_handling_code IS NULL OR TRIM(sensitivity_handling_code) = '')
    "
  )

  invisible(TRUE)
}

legacy_patch_v1_lineage <- function(conn) {
  records <- DBI::dbGetQuery(conn, "
    SELECT * FROM dataset_lineage_tbl WHERE legacy_lineage_migrated_at IS NULL
  ")
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  groups <- list(
    list(table = "dataset_lineage_source_tbl", order = "source_order",
         fields = c(title = "source_datasets_text", description = "source_description")),
    list(table = "dataset_lineage_process_step_tbl", order = "step_order",
         fields = c(description = "process_description", tool_software_reference = "processing_tools",
                    script_reference = "processing_scripts", output_description = "output_description"))
  )
  for (i in seq_len(nrow(records))) {
    record <- records[i, , drop = FALSE]
    for (group in groups) {
      values <- lapply(group$fields, function(field) normalise_lineage_optional_text(record[[field]]))
      if (all(vapply(values, is.na, logical(1)))) next
      existing <- DBI::dbGetQuery(conn, paste0("SELECT * FROM ", group$table, " WHERE dataset_id = ?"),
                                 params = list(record$dataset_id))
      # Previous releases already copied these records, possibly subsequently edited.
      already_copied <- any(existing$value_origin_code == "legacy_backfill", na.rm = TRUE)
      exact_copy <- any(vapply(seq_len(nrow(existing)), function(j) {
        all(vapply(names(values), function(field) {
          is.na(values[[field]]) || identical(normalise_lineage_optional_text(existing[[field]][j]), values[[field]])
        }, logical(1)))
      }, logical(1)))
      if (!already_copied && !exact_copy) {
        next_order <- if (nrow(existing)) max(existing[[group$order]]) + 1L else 1L
        row <- c(list(dataset_id = record$dataset_id), setNames(list(next_order), group$order), values,
                 list(value_origin_code = "legacy_backfill", is_provisional = 1L,
                      created_at = now, updated_at = now, updated_by_person_id = record$updated_by_person_id))
        DBI::dbAppendTable(conn, group$table, as.data.frame(row, stringsAsFactors = FALSE))
      }
    }
    statement <- merge_lineage_narrative(record$lineage_statement, record$lineage_notes)
    DBI::dbExecute(conn, "UPDATE dataset_lineage_tbl
      SET lineage_statement = ?, legacy_lineage_migrated_at = ? WHERE dataset_id = ?",
      params = list(statement, now, record$dataset_id))
  }
  invisible(TRUE)
}
