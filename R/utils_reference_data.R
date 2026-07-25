seed_pond_parties <- function(
  base_path,
  organisation_dt = NULL,
  person_dt = NULL
) {
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
    base_path
  )

  sync_persons_to_db(
    person_dt,
    base_path
  )

  invisible(
    list(
      organisations = nrow(organisation_dt),
      persons = nrow(person_dt)
    )
  )
}
