# Explicitly selected supporting-document citations -----------------------

iso_document_citation <- function(parent, property, row, id_prefix = NULL, details = TRUE) {
  citation <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "cit:CI_Citation")
  if (!is.null(id_prefix)) xml2::xml_set_attr(citation, "id", paste0(id_prefix, row$id[1]))
  iso_content_required(citation, "cit:title", row$document_name[1])
  if (details) {
    parts <- c(if (non_empty_scalar(row$document_type_label[1])) paste0("Document type: ", row$document_type_label[1]),
               if (non_empty_scalar(row$notes[1])) paste0("Notes: ", row$notes[1]))
    if (length(parts)) iso_profile_add_text(citation, "cit:otherCitationDetails", paste(parts, collapse = "\n"))
  }
  path <- iso_portable_reference(row$document_rel_path[1])
  if (non_empty_scalar(row$document_rel_path[1]) && is.na(path))
    stop(paste("Supporting document", row$id[1], "needs a portable relative reference before ISO export."), call. = FALSE)
  if (!is.na(path)) {
    online <- xml2::xml_add_child(xml2::xml_add_child(citation, "cit:onlineResource"), "cit:CI_OnlineResource")
    iso_profile_add_text(online, "cit:linkage", path)
  }
  invisible(citation)
}

iso_add_supporting_documents <- function(parent, property, dataset_id, base_path, flag) {
  stopifnot(flag %in% c("include_in_metadata", "include_in_lineage"))
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  rows <- DBI::dbGetQuery(conn, paste0("SELECT * FROM supporting_document_tbl
    WHERE dataset_id = ? AND active = 1 AND ", flag, " = 1 ORDER BY document_rel_path, id"),
    params = list(as.integer(dataset_id)))
  for (i in seq_len(nrow(rows))) iso_document_citation(parent, property, rows[i, ],
    id_prefix = if (flag == "include_in_lineage") "datapond-lineage-document-" else "datapond-resource-document-")
  invisible(rows)
}
