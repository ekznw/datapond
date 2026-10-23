# Remember publication choices locally, independently of temporary jobs/previews.
# Key by canonical pond path so another pond (including a copied pond) cannot
# silently inherit this destination. The shared transaction avoids lost updates
# when two app sessions save different ponds.
movement_publication_read <- function(directory, pond) {
  key <- movement_endpoint(pond)
  if (!file.exists(file.path(directory, "movement-control.sqlite"))) return(NULL)
  movement_control(directory, function(con) {
    if (!DBI::dbExistsTable(con, "publication_settings")) return(NULL)
    rows <- DBI::dbGetQuery(con, "SELECT * FROM publication_settings WHERE pond = ?", params = list(key))
    if (!nrow(rows)) return(NULL)
    list(pond = rows$pond[1], destination = rows$destination[1],
      include_data = as.logical(rows$include_data[1]), saved_at = rows$saved_at[1])
  })
}

movement_publication_save <- function(directory, pond, destination, include_data = FALSE) {
  key <- movement_endpoint(pond)
  destination <- movement_publication_destination(key, destination)
  if (!is.logical(include_data) || length(include_data) != 1L || is.na(include_data))
    stop("Choose whether to upload registered dataset folders.")
  settings <- list(pond = key, destination = destination, include_data = include_data,
    saved_at = as.numeric(Sys.time()))
  movement_control(directory, function(con) {
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS publication_settings (
      pond TEXT PRIMARY KEY, destination TEXT NOT NULL, include_data INTEGER NOT NULL,
      saved_at REAL NOT NULL)")
    DBI::dbExecute(con, "INSERT INTO publication_settings (pond, destination, include_data, saved_at)
      VALUES (?, ?, ?, ?) ON CONFLICT(pond) DO UPDATE SET
      destination=excluded.destination, include_data=excluded.include_data, saved_at=excluded.saved_at",
      params = list(key, destination, as.integer(include_data), settings$saved_at))
  })
  settings
}
