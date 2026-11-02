# Shared citation/contact serialization against the pinned CIT schema -------

iso_contact_role <- function(value, default = NULL) {
  value <- iso_optional_text(value)
  if (is.na(value)) value <- iso_optional_text(default)
  aliases <- c(creator = "author", maintainer = "custodian", contact = "pointOfContact")
  if (!is.na(value) && value %in% names(aliases)) value <- unname(aliases[value])
  allowed <- c(lineage_responsible_role_codes(), "collaborator", "editor", "mediator", "rightsHolder", "sponsor", "stakeholder")
  if (!is.na(value) && !value %in% allowed) stop(paste("Unmapped contact role:", value), call. = FALSE)
  value
}

iso_contact_record <- function(conn, table, id) {
  id <- wave3_optional_integer(id)
  if (is.na(id)) return(NULL)
  row <- DBI::dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE id = ?"), params = list(id))
  if (nrow(row) != 1L) stop(paste("ISO contact link was not found:", table, id), call. = FALSE)
  row
}

iso_contact_addresses <- function(party, rows, email = NULL, phone = NULL, legacy_address = NULL) {
  # Normalized components take precedence per contact method. A legacy phone
  # can still fill a missing phone when a normalized postal address exists.
  fields <- c("delivery_point", "city", "administrative_area", "postal_code", "country",
              "electronic_mail_address", "phone_number", "online_resource")
  for (field in setdiff(fields, names(rows))) rows[[field]] <- rep(NA_character_, nrow(rows))
  if (!any(vapply(rows$electronic_mail_address, non_empty_scalar, logical(1))) && non_empty_scalar(email)) {
    extra <- as.data.frame(setNames(as.list(rep(NA_character_, length(fields))), fields))
    extra$electronic_mail_address <- email
    rows <- rbind(rows[fields], extra)
  }
  if (!any(vapply(rows$phone_number, non_empty_scalar, logical(1))) && non_empty_scalar(phone)) {
    extra <- as.data.frame(setNames(as.list(rep(NA_character_, length(fields))), fields))
    extra$phone_number <- phone
    rows <- rbind(rows[fields], extra)
  }
  postal_fields <- c("delivery_point", "city", "administrative_area", "postal_code", "country")
  if (!any(vapply(unlist(rows[postal_fields], use.names = FALSE), non_empty_scalar, logical(1))) && non_empty_scalar(legacy_address)) {
    extra <- as.data.frame(setNames(as.list(rep(NA_character_, length(fields))), fields))
    extra$delivery_point <- legacy_address
    rows <- rbind(rows[fields], extra)
  }
  rows <- unique(rows[fields])
  if (!nrow(rows)) return(invisible(NULL))
  populated <- apply(rows, 1L, function(row) any(vapply(row, non_empty_scalar, logical(1))))
  rows <- rows[populated, , drop = FALSE]
  if (!nrow(rows)) return(invisible(NULL))
  contact <- xml2::xml_add_child(xml2::xml_add_child(party, "cit:contactInfo"), "cit:CI_Contact")
  for (value in unique(rows$phone_number)) if (non_empty_scalar(value)) {
    telephone <- xml2::xml_add_child(xml2::xml_add_child(contact, "cit:phone"), "cit:CI_Telephone")
    iso_profile_add_text(telephone, "cit:number", value)
  }
  postal <- c(delivery_point = "deliveryPoint", city = "city", administrative_area = "administrativeArea",
              postal_code = "postalCode", country = "country", electronic_mail_address = "electronicMailAddress")
  addresses <- unique(rows[names(postal)])
  for (i in seq_len(nrow(addresses))) {
    if (!any(vapply(addresses[i, ], non_empty_scalar, logical(1)))) next
    address <- xml2::xml_add_child(xml2::xml_add_child(contact, "cit:address"), "cit:CI_Address")
    for (field in names(postal)) iso_profile_add_text(address, paste0("cit:", postal[[field]]), addresses[[field]][i])
  }
  for (value in unique(rows$online_resource)) if (non_empty_scalar(value)) {
    online <- xml2::xml_add_child(xml2::xml_add_child(contact, "cit:onlineResource"), "cit:CI_OnlineResource")
    iso_profile_add_text(online, "cit:linkage", value)
  }
  invisible(contact)
}

iso_contact_individual <- function(parent, person, base_path) {
  names <- c(person$name_first[1], person$name_last[1])
  name <- paste(names[vapply(names, non_empty_scalar, logical(1))], collapse = " ")
  if (!nzchar(name)) stop("ISO contact person has no usable name.", call. = FALSE)
  node <- xml2::xml_add_child(parent, "cit:CI_Individual")
  iso_profile_add_text(node, "cit:name", name)
  iso_contact_addresses(node, get_party_addresses(base_path, person_id = person$id[1]),
                        person$email[1], person$phone_number[1])
  iso_profile_add_text(node, "cit:positionName", person$role_title[1])
  invisible(node)
}

iso_contact_organisation <- function(parent, organisation, conn, base_path, person = NULL) {
  if (!non_empty_scalar(organisation$organisation_name_full[1])) stop("ISO contact organisation has no full name.", call. = FALSE)
  names <- c(organisation$organisational_unit_name[1], organisation$organisation_name_full[1])
  seen <- organisation$id[1]
  ancestor <- organisation$parent_organisation_id[1]
  while (!is.na(ancestor)) {
    if (ancestor %in% seen) stop("ISO contact organisation hierarchy contains a cycle.", call. = FALSE)
    row <- iso_contact_record(conn, "organisation_tbl", ancestor)
    names <- c(names, row$organisation_name_full[1]); seen <- c(seen, ancestor)
    ancestor <- row$parent_organisation_id[1]
  }
  node <- xml2::xml_add_child(parent, "cit:CI_Organisation")
  iso_profile_add_text(node, "cit:name", paste(unique(names[vapply(names, non_empty_scalar, logical(1))]), collapse = ", "))
  iso_contact_addresses(node, get_party_addresses(base_path, organisation_id = organisation$id[1]),
                        legacy_address = organisation$organisation_address[1])
  if (!is.null(person)) iso_contact_individual(xml2::xml_add_child(node, "cit:individual"), person, base_path)
  invisible(node)
}

iso_contact_add_responsibility <- function(parent, property, person_id = NULL, organisation_id = NULL,
                                         role = NULL, base_path, default_role = NULL) {
  person_id <- wave3_optional_integer(person_id); organisation_id <- wave3_optional_integer(organisation_id)
  if (is.na(person_id) && is.na(organisation_id)) return(invisible(NULL))
  role <- iso_contact_role(role, default_role)
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  person <- iso_contact_record(conn, "persons_tbl", person_id)
  # Use recorded affiliation only; an explicitly different organisation remains
  # a separate co-responsible party rather than an invented employer.
  if (is.na(organisation_id) && !is.null(person)) organisation_id <- person$organisation_id[1]
  organisation <- iso_contact_record(conn, "organisation_tbl", organisation_id)
  responsibility <- xml2::xml_add_child(xml2::xml_add_child(parent, property), "cit:CI_Responsibility")
  if (is.na(role)) iso_content_required(responsibility, "cit:role", NA_character_) else
    iso_profile_add_code(responsibility, "cit:role", "cit:CI_RoleCode", role,
      "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#CI_RoleCode")
  affiliated <- !is.null(person) && !is.null(organisation) && !is.na(person$organisation_id[1]) &&
    identical(as.integer(person$organisation_id[1]), as.integer(organisation$id[1]))
  if (!is.null(organisation)) iso_contact_organisation(xml2::xml_add_child(responsibility, "cit:party"),
    organisation, conn, base_path, if (affiliated) person else NULL)
  if (!is.null(person) && !affiliated) iso_contact_individual(xml2::xml_add_child(responsibility, "cit:party"), person, base_path)
  invisible(responsibility)
}

iso_contact_add_resource_parties <- function(citation, identification, dataset_id, base_path) {
  conn <- get_conn(base_path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  authors <- DBI::dbGetQuery(conn, "SELECT * FROM authority_tbl WHERE dataset_id = ? ORDER BY author_order, id",
                            params = list(as.integer(dataset_id)))
  for (i in seq_len(nrow(authors))) {
    if (is.na(authors$person_id[i])) stop("A citation author has no linked person.", call. = FALSE)
    iso_contact_add_responsibility(citation, "cit:citedResponsibleParty", authors$person_id[i],
      role = authors$role[i], base_path = base_path, default_role = "author")
  }
  contacts <- DBI::dbGetQuery(conn, "SELECT person_id, organisation_id, role FROM poc_tbl WHERE dataset_id = ? ORDER BY id",
                             params = list(as.integer(dataset_id)))
  organisations <- DBI::dbGetQuery(conn, "SELECT NULL AS person_id, organisation_id, role FROM dataset_organisation_tbl
    WHERE dataset_id = ? ORDER BY id", params = list(as.integer(dataset_id)))
  contacts <- rbind(contacts, organisations)
  if (nrow(contacts)) contacts$role <- vapply(contacts$role, iso_contact_role, character(1))
  contacts <- unique(contacts)
  for (i in seq_len(nrow(contacts))) {
    if (is.na(contacts$person_id[i]) && is.na(contacts$organisation_id[i])) stop("A resource responsibility has no linked party.", call. = FALSE)
    iso_contact_add_responsibility(identification, "mri:pointOfContact", contacts$person_id[i],
      contacts$organisation_id[i], contacts$role[i], base_path)
  }
  invisible(NULL)
}

iso_order_citation <- function(citation) {
  iso_core_order_children(citation, c("title", "alternateTitle", "date", "edition", "editionDate", "identifier",
    "citedResponsibleParty", "presentationForm", "series", "otherCitationDetails", "ISBN", "ISSN", "onlineResource", "graphic"))
}
