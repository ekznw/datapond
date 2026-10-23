# Minimal database fixtures for the GAP-018 assessment baseline. These are
# deliberately still datasets, including the nonspatial table. Do not change
# their scope to service or fabricate bounds to bypass exporter preflight.
iso_export_candidate_pond <- function(kind = c("vector", "raster", "tabular")) {
  kind <- match.arg(kind)
  pond <- tempfile(paste0("iso-candidate-", kind, "-"))
  dir.create(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  datapond:::ensure_columns(conn, "entry_tbl", c("abstract", "keyword_object", "metadata_creation_date", "purpose", "version", "dataset_credit"))
  DBI::dbExecute(conn, "INSERT INTO organisation_tbl
    (organisation_name_full, organisation_code) VALUES ('Fixture Metadata Office', 'TEST')")
  org_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO entry_tbl
    (uuid, title, abstract, citation_date, keyword_object, metadata_resource_scope_code)
    VALUES (?, ?, 'Synthetic validation fixture, not operational data.', '2026-10-01', 'ecology', 'dataset')",
    params = list(paste0("fixture-", kind), paste("ISO", kind, "fixture")))
  dataset_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "UPDATE entry_tbl SET metadata_creation_date = '2026-09-30 08:30:00',
    purpose = 'Exercise the metadata exporter.', version = '1.2', dataset_credit = 'Fixture survey team',
    citation_key = ? WHERE id = ?", params = list(paste0("fixture_", kind, "_2026"), dataset_id))
  # Backfill identity and typed dates using the actual migration path.
  datapond:::ensure_iso_profile_schema(conn)
  DBI::dbExecute(conn, "INSERT INTO metadata_responsibility_tbl
    (dataset_id, organisation_id, role_code, is_primary) VALUES (?, ?, 'pointOfContact', 1)",
    params = list(dataset_id, org_id))
  DBI::dbExecute(conn, "INSERT INTO dataset_topic_category_tbl
    (dataset_id, topic_category_code) VALUES (?, 'environment')", params = list(dataset_id))
  filename <- switch(kind, vector = "sites.gpkg", raster = "elevation.tif", tabular = "lookup.csv")
  relative <- paste0("data/", filename)
  DBI::dbExecute(conn, "INSERT INTO data_file_tbl
    (dataset_id, data_file_rel_path_full, file_name, data_file_format, active)
    VALUES (?, ?, ?, ?, 1)", params = list(dataset_id, relative, filename, kind))
  file_id <- DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id[1]
  DBI::dbExecute(conn, "INSERT INTO dataset_attribute_tbl
    (dataset_id, data_file_id, data_file_rel_path_full, data_file_name,
     data_field_name, data_field_desc, attribute_type, attribute_order)
    VALUES (?, ?, ?, ?, 'value', 'Documented fixture measurement', 'numeric', 1)",
    params = list(dataset_id, file_id, relative, filename))
  if (kind != "tabular") {
    DBI::dbExecute(conn, "INSERT INTO dataset_spatial_summary_tbl
      (dataset_id, bbox_status, xmin, ymin, xmax, ymax)
      VALUES (?, 'valid', 29, -30, 31, -28)", params = list(dataset_id))
    DBI::dbExecute(conn, "INSERT INTO dataset_manual_spatial_extent_tbl
      (dataset_id, extent_label, crs_auth_code, crs_input, bbox_native,
       bbox_status, xmin, ymin, xmax, ymax)
      VALUES (?, 'Fixture extent', 'EPSG:4326', 'EPSG:4326', '29,-30,31,-28',
              'valid', 29, -30, 31, -28)", params = list(dataset_id))
  }
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_tbl
    (dataset_id, lineage_source_mode, lineage_statement, lineage_status)
    VALUES (?, 'document', 'Methods are described in the accompanying survey protocol.', 'reviewed')",
    params = list(dataset_id))
  DBI::dbExecute(conn, "INSERT INTO supporting_document_tbl
    (dataset_id, document_rel_path, document_name, active, include_in_lineage)
    VALUES (?, 'documents/survey-protocol.pdf', 'Survey protocol', 1, 1)", params = list(dataset_id))
  list(path = pond, dataset_id = as.integer(dataset_id), kind = kind)
}

core_validation_engine <- function() {
  python <- Sys.getenv("DATAPOND_ISO_PYTHON", unset = Sys.which("python3"))
  skip_if(!nzchar(python), "Core ISO checks require Python 3.9+ with lxml")
  probe <- tryCatch(processx::run(python, c("-c", "import lxml.etree, lxml.isoschematron"),
                                error_on_status = FALSE), error = function(e) NULL)
  skip_if(is.null(probe) || probe$status != 0L, "Core ISO checks require lxml")
}

core_xpath_text <- function(document, xpath) {
  xml2::xml_text(xml2::xml_find_all(document, xpath, xml2::xml_ns(document)))
}

# Physical dependencies are needed only for package-publishing tests.
iso_package_fixture_files <- function(fixture) {
  root <- file.path(fixture$path, "dataset")
  dir.create(file.path(root, "documents"), recursive = TRUE, showWarnings = FALSE)
  writeLines("Synthetic survey protocol for package tests.",
    file.path(root, "documents", "survey-protocol.pdf"))
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET dataset_path=? WHERE id=?",
    params = list(root, fixture$dataset_id))
  root
}

distribution_fixture <- function(kind = "vector") {
  fixture <- iso_export_candidate_pond(kind)
  conn <- datapond:::get_conn(fixture$path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  datapond:::ensure_columns(conn, "entry_tbl", c("access_constraints", "use_limitations", "license_type", "data_distributor_organisation_id"))
  DBI::dbExecute(conn, "UPDATE entry_tbl SET access_constraints = 'Request exact site locations from the custodian.',
    use_limitations = 'Not suitable for navigation.', license_type = 'Obsolete licence snapshot',
    data_distributor_organisation_id = 1")
  DBI::dbExecute(conn, "INSERT INTO dataset_organisation_tbl(dataset_id, organisation_id, role) VALUES (?, 1, 'distributor')",
                 params = list(fixture$dataset_id))
  DBI::dbExecute(conn, "INSERT INTO dataset_license_tbl(dataset_id, license_identifier, license_name, license_description,
    license_url, license_holder, rights_holder_organisation_id, access_level_code, use_constraints,
    required_attribution, attribution_text, redistribution_permission_code, permission_reference,
    sensitivity_handling_code, sensitivity_reason, licence_notes, rights_basis_code, terms_review_date)
    VALUES (?, 'CC-BY-4.0', 'Creative Commons Attribution 4.0', 'Credit the source.',
    'https://creativecommons.org/licenses/by/4.0/', 'Stale holder snapshot', 1, 'public_request',
    'Not suitable for navigation.', 1, 'Survey team (2026).', 'permitted_with_attribution',
    'Permission letter ABC-123', 'removed_or_generalised', 'Exact nest locations were removed.',
    'PRIVATE INTERNAL NOTE', 'ekznw_owned', '2026-12-01')", params = list(fixture$dataset_id))
  datapond:::save_dataset_release_rights(fixture$dataset_id, list(
    embargo_status = "Yes", embargo_reason = "Publication review", embargo_start_date = "2026-10-01",
    embargo_end_date = "2027-01-31", release_addressee = "Approved research partners",
    release_statement = "Release following approval.", dissemination_constraint_code = "approvedRecipients"), fixture$path)
  datapond:::replace_dataset_distribution_formats(fixture$dataset_id, data.frame(
    specification_title = c("GeoPackage", "CSV"), format_version = c("1.3", NA),
    media_type = c("application/geopackage+sqlite3", "text/csv"),
    format_uri = c("https://www.geopackage.org/spec/", NA)), fixture$path)
  datapond:::replace_dataset_online_resources(fixture$dataset_id, data.frame(
    resource_url = c("https://example.test/data.gpkg", "https://example.test/about"),
    resource_name = c("Download observations", "About this survey"),
    resource_description = c("Generalised observations", "Survey documentation"),
    protocol = c("HTTPS", "HTTPS"), function_code = c("download", "information"),
    media_type = c("application/geopackage+sqlite3", "text/html"), is_primary = c(1L, 0L)), fixture$path)
  fixture
}
