test_that("legacy topic keywords migrate into one canonical theme vocabulary", {
  pond <- tempfile("keyword-theme-")
  dir.create(pond)
  on.exit(unlink(pond, recursive = TRUE, force = TRUE), add = TRUE)
  datapond:::init_db(pond)

  conn <- datapond:::get_conn(pond)
  datapond:::ensure_columns(
    conn,
    "entry_tbl",
    c("keyword_object", "keyword_topic")
  )
  DBI::dbExecute(conn, paste(
    "INSERT INTO entry_tbl (uuid, title, keyword_object, keyword_topic)",
    "VALUES ('theme-merge', 'Theme merge', 'vegetation; Fire', 'fire; stream flow')"
  ))
  DBI::dbExecute(conn, paste(
    "INSERT INTO keyword_thesaurus_tbl (field_code, citation_title)",
    "VALUES ('keyword_object', 'Canonical themes')"
  ))
  DBI::dbExecute(conn, paste(
    "INSERT INTO keyword_thesaurus_tbl (field_code, citation_title)",
    "VALUES ('keyword_topic', 'Legacy topics')"
  ))
  DBI::dbExecute(conn, paste(
    "INSERT INTO keyword_vocab_tbl",
    "(field_code, option_value, option_label, is_active, approval_status)",
    "VALUES ('keyword_object', 'Fire', 'Fire', 0, 'approved')"
  ))
  DBI::dbExecute(conn, paste(
    "INSERT INTO keyword_vocab_tbl",
    "(field_code, option_value, option_label, is_active, approval_status)",
    "VALUES ('keyword_topic', 'fire', 'Fire legacy', 1, 'pending')"
  ))
  DBI::dbExecute(conn, paste(
    "INSERT INTO keyword_vocab_tbl (field_code, option_value, option_label)",
    "VALUES ('keyword_topic', 'stream_flow', 'Stream flow')"
  ))
  DBI::dbDisconnect(conn)

  datapond:::init_db(pond)
  datapond:::init_db(pond)
  conn <- datapond:::get_conn(pond)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  entry <- DBI::dbGetQuery(conn, paste(
    "SELECT keyword_object, keyword_topic FROM entry_tbl",
    "WHERE uuid = 'theme-merge'"
  ))
  expect_identical(entry$keyword_object, "vegetation; Fire; stream flow")
  expect_true(is.na(entry$keyword_topic))

  vocabulary <- DBI::dbGetQuery(conn, paste(
    "SELECT field_code, option_value, is_active, approval_status",
    "FROM keyword_vocab_tbl WHERE LOWER(option_value) IN ('fire', 'stream_flow')",
    "ORDER BY LOWER(option_value)"
  ))
  expect_true(all(vocabulary$field_code == "keyword_object"))
  expect_identical(sum(tolower(vocabulary$option_value) == "fire"), 1L)
  fire <- vocabulary[tolower(vocabulary$option_value) == "fire", ]
  expect_identical(fire$is_active, 1L)
  expect_identical(fire$approval_status, "approved")

  thesauri <- DBI::dbGetQuery(conn, paste(
    "SELECT field_code, citation_title FROM keyword_thesaurus_tbl",
    "WHERE field_code IN ('keyword_object', 'keyword_topic')"
  ))
  expect_identical(thesauri$field_code, "keyword_object")
  expect_identical(thesauri$citation_title, "Canonical themes")
})

test_that("the app exposes the merged keyword field under its new name", {
  prepared <- datapond:::dataset_overview_schema()[["Dataset overview"]][["About"]]

  expect_false("keyword_topic" %in% prepared$field)
  expect_identical(
    prepared$label[prepared$field == "keyword_object"],
    "Theme / subject keywords"
  )
  expect_identical(
    unname(datapond:::keyword_admin_field_choices()),
    c("keyword_object", "keyword_discipline", "keyword_geo")
  )
  expect_identical(
    datapond:::get_keyword_vocab_field("keyword_topic"),
    "keyword_object"
  )
  expect_identical(
    datapond:::keyword_vocab_field_code("keyword_topic_extra"),
    "keyword_object"
  )
})

test_that("topic seed terms merge into the object vocabulary", {
  seed <- data.frame(
    field_code = c("keyword_object", "keyword_topic", "keyword_topic"),
    option_value = c("fire", "FIRE", "drought"),
    option_label = c("Fire", "Fire duplicate", "Drought"),
    sort_order = 1:3
  )

  prepared <- datapond:::prepare_keyword_vocab_seed(seed)
  expect_true(all(prepared$field_code == "keyword_object"))
  expect_setequal(tolower(prepared$option_value), c("fire", "drought"))
})
