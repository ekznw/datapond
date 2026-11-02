contact_lineage_fixture <- function(kind = "vector") {
  f <- distribution_fixture(kind)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbExecute(conn, "UPDATE entry_tbl SET resource_language_code='eng', resource_character_encoding='utf8'")
  DBI::dbExecute(conn, "UPDATE dataset_citation_date_tbl SET date_note='First public release'")
  DBI::dbExecute(conn, "UPDATE dataset_attribute_tbl SET min_value='1', max_value='10', data_field_units='m'")
  DBI::dbExecute(conn, "INSERT INTO organisation_tbl(organisation_name_full,organisation_code) VALUES
    ('Parent Conservation Authority','PARENT'),('Independent Survey Office','ISO')")
  DBI::dbExecute(conn, "UPDATE organisation_tbl SET parent_organisation_id=2, organisational_unit_name='Wetlands unit' WHERE id=1")
  DBI::dbExecute(conn, "INSERT INTO persons_tbl(name_first,name_last,organisation_id,role_title,email,phone_number) VALUES
    ('Ada','Surveyor',1,'Field ecologist','legacy@example.test','+27 10 111 1111'),
    ('Bongi',NULL,3,'Data analyst','bongi@example.test','+27 10 222 2222')")
  DBI::dbExecute(conn, "INSERT INTO authority_tbl(dataset_id,person_id,author_order,role) VALUES
    (1,1,2,'author'),(1,2,1,'author')")
  DBI::dbExecute(conn, "INSERT INTO poc_tbl(dataset_id,person_id,organisation_id,role) VALUES
    (1,1,1,'custodian'),(1,NULL,1,'owner')")
  DBI::dbExecute(conn, "INSERT INTO dataset_organisation_tbl(dataset_id,organisation_id,role) VALUES (1,1,'owner')")
  DBI::dbExecute(conn, "INSERT INTO metadata_responsibility_tbl(dataset_id,person_id,organisation_id,role_code,is_primary)
    VALUES (1,2,3,'creator',0)")
  DBI::dbExecute(conn, "INSERT INTO party_address_tbl(person_id,delivery_point,city,electronic_mail_address,online_resource,is_primary)
    VALUES (1,'1 Field Road','Town A','ada@example.test','https://example.test/ada',1)")
  DBI::dbExecute(conn, "INSERT INTO party_address_tbl(organisation_id,delivery_point,city,phone_number,online_resource,is_primary)
    VALUES (1,'2 Office Road','Town B','+27 10 333 3333','https://example.test/office',1),
           (1,'3 Office Road','Town C','+27 10 444 4444','https://example.test/branch',0)")
  DBI::dbExecute(conn, "UPDATE supporting_document_tbl SET document_type_label='Methods report',notes='Approved protocol',include_in_metadata=1")
  DBI::dbExecute(conn, "INSERT INTO supporting_document_tbl(dataset_id,document_name,document_rel_path,active,include_in_metadata,include_in_lineage,notes)
    VALUES (1,'Internal notes','documents/private.txt',1,0,0,'PRIVATE NOTE'),
           (1,'Old protocol','documents/retired.pdf',0,1,1,'RETIRED NOTE')")
  datapond:::save_dataset_feature_catalogue(1,list(title='Fixture catalogue',producer_person_id=1,producer_organisation_id=1),f$path)
  # A citation describes vocabulary members, not arbitrary dataset keywords.
  DBI::dbExecute(conn, "INSERT INTO keyword_vocab_tbl (field_code, option_value, option_label)
    VALUES ('keyword_object', 'ecology', 'Ecology')")
  datapond:::save_keyword_thesaurus(f$path,list(field_code='keyword_object',citation_title='Ecology thesaurus',
    identifier='ECO-2026',edition='2',edition_date='2026-01-01',edition_date_type_code='revision',
    thesaurus_uri='https://example.test/thesaurus',responsible_person_id=1,responsible_organisation_id=1,responsible_role_code='publisher'))
  source <- datapond:::save_dataset_lineage_source(1,list(title='Original survey',description='Original mapped survey observations.',
    identifier='SRC-01',identifier_code_space='Survey register',source_uri='https://example.test/source',
    citation_date='2016-06-01',citation_date_type_code='publication',temporal_start='2016',temporal_end='2017-06',
    equivalent_scale_denominator=10000,spatial_resolution_value=5,spatial_resolution_unit='m',
    responsible_person_id=1,responsible_organisation_id=1,role_code='originator',supporting_document_id=1),f$path)
  source2 <- datapond:::save_dataset_lineage_source(1,list(title='Satellite mosaic',description='Image source.',
    temporal_start='2016-02-01',spatial_resolution_value=10,spatial_resolution_unit='m',
    responsible_organisation_id=3,role_code='publisher'),f$path)
  # The source link API is exercised by the existing lineage tests; these rows
  # make the export fixture explicit without depending on UI state.
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_process_step_tbl(dataset_id,step_order,description,rationale,step_datetime,
    processor_person_id,processor_organisation_id,role_code,tool_software_reference,script_reference,output_description)
    VALUES (1,1,'Intersected observations with imagery.','Remove duplicate coverage.','2026-01-02T11:12:13+02:00',
    2,3,'processor','Processing toolkit 2','scripts/intersect.R','Clean observations')")
  DBI::dbExecute(conn, "INSERT INTO dataset_lineage_step_source_tbl(dataset_id,process_step_id,source_id) VALUES (1,1,?),(1,1,?)",params=list(source,source2))
  datapond:::replace_dataset_quality_results(1,data.frame(measure_identifier='CHECK-1',measure_type_code='logicalConsistency',
    quality_element_code='DQ_ConceptualConsistency',evaluation_method='Compared with source protocol.',
    result_type_code='descriptive',explanation='Consistent.'),f$path)
  f
}

test_that("populated contacts, citations and lineage validate with vector and raster exports", {
  core_validation_engine()
  for (kind in c('vector','raster')) {
    f <- contact_lineage_fixture(kind)
    a <- datapond:::assess_iso_metadata_export(1,f$path)
    expect_true(a$validation$valid,info=paste(a$validation$issues$message,collapse='\n'))
    expect_true(a$semantics$valid, info = paste(a$semantics$issues$message, collapse = "\n"))
    xml <- a$document; ns <- xml2::xml_ns(xml)
    authors <- xml2::xml_find_all(xml,'//mri:citation/cit:CI_Citation/cit:citedResponsibleParty',ns)
    expect_identical(core_xpath_text(authors,'.//cit:CI_Individual/cit:name/*'),c('Bongi','Ada Surveyor'))
    expect_identical(core_xpath_text(xml,'//mri:citation/cit:CI_Citation/cit:otherCitationDetails/*'),'Citation date (publication 2026-10-01): First public release')
    expect_identical(core_xpath_text(xml,'//mrd:distributorContact//cit:number/*'),c('+27 10 333 3333','+27 10 444 4444'))
    expect_identical(core_xpath_text(xml,'//mco:responsibleParty//cit:onlineResource//cit:linkage/*'),c('https://example.test/office','https://example.test/branch'))
    expect_identical(core_xpath_text(authors,'.//cit:CI_Individual/cit:positionName/*'),c('Data analyst','Field ecologist'))
    expect_identical(core_xpath_text(authors,'.//cit:CI_Individual//cit:electronicMailAddress/*'),c('bongi@example.test','ada@example.test'))
    expect_false(grepl('legacy@example.test',as.character(xml),fixed=TRUE))
    expect_identical(core_xpath_text(authors,'.//cit:CI_Individual//cit:number/*'),c('+27 10 222 2222','+27 10 111 1111'))
    expect_identical(core_xpath_text(xml,'//mdb:contact//cit:CI_Individual/cit:name/*'),'Bongi')
    expect_identical(core_xpath_text(xml,'//mdb:contact//cit:CI_Individual//cit:electronicMailAddress/*'),'bongi@example.test')
    expect_length(xml2::xml_find_all(xml,'//mri:pointOfContact/cit:CI_Responsibility[cit:role/*/@codeListValue="owner"]',ns),1L)
    expect_true(any(grepl('Wetlands unit, Fixture Metadata Office, Parent Conservation Authority',core_xpath_text(xml,'//cit:CI_Organisation/cit:name/*'),fixed=TRUE)))
    thesaurus <- xml2::xml_find_first(xml,'//mri:thesaurusName/cit:CI_Citation',ns)
    expect_identical(core_xpath_text(thesaurus,'cit:identifier//mcc:code/*'),'ECO-2026')
    expect_identical(core_xpath_text(thesaurus,'cit:date//gco:Date'),'2026-01-01')
    expect_identical(core_xpath_text(thesaurus,'cit:citedResponsibleParty//cit:CI_Individual/cit:name/*'),'Ada Surveyor')
    expect_identical(core_xpath_text(xml,'//mrl:sourceCitation//cit:citedResponsibleParty/cit:CI_Responsibility/cit:role/*'),c('originator','publisher'))
    expect_identical(core_xpath_text(xml,'//mrl:processor//cit:CI_Individual/cit:name/*'),'Bongi')
    expect_identical(core_xpath_text(xml,'//mrl:scope//gml:beginPosition'),c('2016','2016-02-01'))
    expect_identical(core_xpath_text(xml,'//mrl:scope//gml:endPosition'),c('2017-06',''))
    expect_identical(xml2::xml_attr(xml2::xml_find_all(xml,'//mrl:scope//gml:endPosition',ns),'indeterminatePosition'),c(NA_character_,'unknown'))
    expect_identical(core_xpath_text(xml,'//mrl:sourceSpatialResolution//mri:denominator/*'),'10000')
    expect_identical(core_xpath_text(xml,'//mrl:sourceSpatialResolution//gco:Distance'),'10')
    expect_identical(xml2::xml_attr(xml2::xml_find_all(xml,'//mrl:sourceSpatialResolution//gco:Distance',ns),'uom'),'m')
    expect_match(core_xpath_text(xml,'//mrl:LI_Source/mrl:description/*')[1],'Spatial resolution: 5 m',fixed=TRUE)
    expect_length(xml2::xml_find_all(xml,'//mrl:additionalDocumentation',ns),1L)
    expect_length(xml2::xml_find_all(xml,'//mri:additionalDocumentation',ns),1L)
    expect_identical(core_xpath_text(xml,'//mrl:additionalDocumentation//cit:otherCitationDetails/*'),'Document type: Methods report\nNotes: Approved protocol')
    expect_false(grepl('PRIVATE NOTE|RETIRED NOTE',as.character(xml)))
    links <- xml2::xml_attr(xml2::xml_find_all(xml,'//mrl:sourceMetadata|//mrl:LI_ProcessStep/mrl:source',ns),'xlink:href',ns=ns)
    ids <- xml2::xml_attr(xml2::xml_find_all(xml,'//*[@id]',ns),'id')
    expect_true(all(sub('^#','',links) %in% ids))
    unlink(f$path,recursive=TRUE)
  }
})

test_that("unknown roles and conflicting organisations are never guessed", {
  f <- contact_lineage_fixture(); on.exit(unlink(f$path,recursive=TRUE),add=TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  DBI::dbExecute(conn,"UPDATE poc_tbl SET organisation_id=3 WHERE person_id=1")
  xml <- datapond:::build_iso_core_metadata_xml(1,f$path); ns <- xml2::xml_ns(xml)
  party <- xml2::xml_find_first(xml,'//mri:pointOfContact/cit:CI_Responsibility[cit:role/*/@codeListValue="custodian"]',ns)
  expect_length(xml2::xml_find_all(party,'cit:party',ns),2L)
  expect_length(xml2::xml_find_all(party,'cit:party/cit:CI_Organisation/cit:individual',ns),0L)
  DBI::dbExecute(conn,"UPDATE authority_tbl SET role='unknown-role' WHERE id=1")
  expect_error(datapond:::build_iso_core_metadata_xml(1,f$path),'Unmapped contact role')
  DBI::dbExecute(conn,"UPDATE authority_tbl SET role='author' WHERE id=1")
  DBI::dbExecute(conn,"UPDATE persons_tbl SET name_first=NULL,name_last=NULL WHERE id=1")
  expect_error(datapond:::build_iso_core_metadata_xml(1,f$path),'no usable name')
})

test_that("lineage precision, units and authoritative document links are checked", {
  f <- contact_lineage_fixture(); on.exit(unlink(f$path,recursive=TRUE),add=TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  DBI::dbExecute(conn,"UPDATE dataset_lineage_source_tbl SET temporal_start='2018',temporal_end='2016' WHERE id=1")
  expect_error(datapond:::build_iso_lineage_xml(1,f$path),'start cannot be later')
  DBI::dbExecute(conn,"UPDATE dataset_lineage_source_tbl SET temporal_start='2016-02-30',temporal_end=NULL WHERE id=1")
  expect_error(datapond:::build_iso_lineage_xml(1,f$path),'valid ISO date')
  DBI::dbExecute(conn,"UPDATE dataset_lineage_source_tbl SET temporal_start='2016',spatial_resolution_unit=NULL WHERE id=1")
  expect_error(datapond:::build_iso_lineage_xml(1,f$path),'recorded unit')
  DBI::dbExecute(conn,"UPDATE dataset_lineage_source_tbl SET spatial_resolution_unit='m' WHERE id=1")
  DBI::dbExecute(conn,"UPDATE supporting_document_tbl SET active=0 WHERE id=1")
  expect_error(datapond:::build_iso_lineage_xml(1,f$path),'inactive or foreign')
})

test_that("linked authority alone does not publish document notes or metadata-only citations", {
  f <- contact_lineage_fixture(); on.exit(unlink(f$path,recursive=TRUE),add=TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  DBI::dbExecute(conn,"UPDATE supporting_document_tbl SET include_in_metadata=0,include_in_lineage=0 WHERE id=1")
  xml <- datapond:::build_iso_core_metadata_xml(1,f$path); ns <- xml2::xml_ns(xml)
  expect_length(xml2::xml_find_all(xml,'//mrl:additionalDocumentation',ns),1L)
  expect_length(xml2::xml_find_all(xml,'//mri:additionalDocumentation',ns),0L)
  expect_false(grepl('Approved protocol|PRIVATE NOTE|RETIRED NOTE',as.character(xml)))
  DBI::dbExecute(conn,"UPDATE supporting_document_tbl SET document_rel_path='file:///home/editor/private.pdf' WHERE id=1")
  expect_error(datapond:::build_iso_lineage_xml(1,f$path),'portable relative reference')
})

test_that("organisation hierarchy cycles and legacy contact fallbacks are explicit", {
  f <- contact_lineage_fixture(); on.exit(unlink(f$path,recursive=TRUE),add=TRUE)
  conn <- datapond:::get_conn(f$path); on.exit(DBI::dbDisconnect(conn),add=TRUE)
  DBI::dbExecute(conn,"UPDATE organisation_tbl SET organisation_address='Original postal address' WHERE id=3")
  DBI::dbExecute(conn,"INSERT INTO party_address_tbl(organisation_id,phone_number) VALUES (3,'+27 99 000 0000')")
  xml <- datapond:::build_iso_core_metadata_xml(1,f$path)
  expect_true('Original postal address' %in% core_xpath_text(xml,'//cit:deliveryPoint/*'))
  DBI::dbExecute(conn,"UPDATE organisation_tbl SET parent_organisation_id=1 WHERE id=2")
  expect_error(datapond:::build_iso_core_metadata_xml(1,f$path),'hierarchy contains a cycle')
})
