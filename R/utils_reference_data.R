seed_pond_parties <- function(
  base_path,
  organisation_dt = NULL,
  person_dt = NULL
) {
  conn <- get_conn(base_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  pond_id <- ensure_data_pond_id(base_path, conn = conn)

  # Packaged parties initialise a new catalogue once. A pre-existing catalogue
  # owns its party edits, deletions and user permissions, even without a marker.
  result <- DBI::dbWithTransaction(conn, {
    marked <- nrow(DBI::dbGetQuery(conn, paste(
      "SELECT id FROM app_config_tbl WHERE data_pond_id = ?",
      "AND user_id IS NULL AND config_scope = 'pond'",
      "AND config_key = 'party_seed_initialised'"
    ), params = list(pond_id))) > 0L
    existing <- any(vapply(c("entry_tbl", "organisation_tbl", "persons_tbl", "user_tbl"),
      function(table) nrow(DBI::dbGetQuery(conn, paste(
        "SELECT 1 FROM", DBI::dbQuoteIdentifier(conn, table), "LIMIT 1"
      ))) > 0L, logical(1)))
    counts <- list(organisations = 0L, persons = 0L)
    if (!marked && !existing) {
      if (is.null(organisation_dt)) {
        organisation_dt <- load_organisations_csv(
          datapond_resource("vocab_organisations.csv")
        )
      }

      if (is.null(person_dt)) {
        person_dt <- data.table::fread(
          datapond_resource("vocab_persons.csv")
        )
      }

      sync_organisations_to_db(
        organisation_dt,
        base_path,
        conn = conn
      )

      sync_persons_to_db(
        person_dt,
        base_path,
        conn = conn
      )
      counts <- list(
        organisations = nrow(organisation_dt),
        persons = nrow(person_dt)
      )
    }
    if (!marked) {
      DBI::dbExecute(conn, paste(
        "INSERT INTO app_config_tbl",
        "(data_pond_id, config_scope, config_key, config_value)",
        "VALUES (?, 'pond', 'party_seed_initialised', ?)"
      ), params = list(pond_id, if (existing) "existing_catalogue" else "packaged_csv"))
    }
    counts
  })
  invisible(result)
}
