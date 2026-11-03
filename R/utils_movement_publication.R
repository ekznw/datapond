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
      include_data = as.logical(rows$include_data[1]),
      sharepoint_compat = if ("sharepoint_compat" %in% names(rows)) as.logical(rows$sharepoint_compat[1]) else FALSE,
      saved_at = rows$saved_at[1])
  })
}

movement_publication_save <- function(directory, pond, destination, include_data = FALSE,
                                      sharepoint_compat = FALSE) {
  key <- movement_endpoint(pond)
  destination <- movement_publication_destination(key, destination)
  if (!is.logical(include_data) || length(include_data) != 1L || is.na(include_data))
    stop("Choose whether to upload registered dataset folders.")
  if (!is.logical(sharepoint_compat) || length(sharepoint_compat) != 1L || is.na(sharepoint_compat))
    stop("Choose whether to use SharePoint file compatibility.")
  settings <- list(pond = key, destination = destination, include_data = include_data,
    sharepoint_compat = sharepoint_compat,
    saved_at = as.numeric(Sys.time()))
  movement_control(directory, function(con) {
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS publication_settings (
      pond TEXT PRIMARY KEY, destination TEXT NOT NULL, include_data INTEGER NOT NULL,
      sharepoint_compat INTEGER NOT NULL DEFAULT 0, saved_at REAL NOT NULL)")
    if (!"sharepoint_compat" %in% DBI::dbGetQuery(con, "PRAGMA table_info(publication_settings)")$name)
      DBI::dbExecute(con, "ALTER TABLE publication_settings ADD COLUMN sharepoint_compat INTEGER NOT NULL DEFAULT 0")
    DBI::dbExecute(con, "INSERT INTO publication_settings (pond, destination, include_data, sharepoint_compat, saved_at)
      VALUES (?, ?, ?, ?, ?) ON CONFLICT(pond) DO UPDATE SET
      destination=excluded.destination, include_data=excluded.include_data,
      sharepoint_compat=excluded.sharepoint_compat, saved_at=excluded.saved_at",
      params = list(key, destination, as.integer(include_data), as.integer(sharepoint_compat), settings$saved_at))
  })
  settings
}
