test_that("the bundled guide opens without requiring a document toolchain", {
  path <- datapond::open_user_guide(browse = FALSE)
  expect_true(file.exists(path))
  expect_identical(basename(path), "offline.html")
  html <- xml2::read_html(path)
  # The standalone copy must retain its content without neighbouring files.
  assets <- xml2::xml_find_all(html, "//script[@src] | //link[@rel='stylesheet'] | //img[@src]")
  refs <- vapply(assets, function(node) {
    src <- xml2::xml_attr(node, "src")
    if (!is.na(src)) src else xml2::xml_attr(node, "href")
  }, character(1))
  expect_true(all(startsWith(refs, "data:")))
  ids <- xml2::xml_attr(xml2::xml_find_all(html, "//section[@id]"), "id")
  expect_true(all(c("install", "first-pond", "lineage", "releases", "maintaining") %in% ids))
  expect_error(datapond::open_user_guide(browse = NA), "TRUE or FALSE")
})

test_that("the app serves the bundled guide from its advertised URL", {
  app <- datapond:::datapond_app()
  root <- unname(shiny::resourcePaths()[["datapond-guide"]])
  expect_true(file.exists(file.path(root, "index.html")))
  index <- jsonlite::fromJSON(file.path(root, "search.json"))
  expect_true(any(grepl("install.html", index$href, fixed = TRUE)))
  expect_match(as.character(datapond:::ui),
    'href="datapond-guide/index.html"', fixed = TRUE)
})

test_that("bundled guide stylesheet references resolve after packaging", {
  root <- system.file("guide", package = "datapond", mustWork = TRUE)
  pages <- list.files(root, pattern = "\\.html$", recursive = TRUE,
                      full.names = TRUE)
  expect_gt(length(pages), 1L)
  for (page in pages) {
    html <- xml2::read_html(page)
    refs <- xml2::xml_attr(xml2::xml_find_all(html, "//link[@rel='stylesheet']"), "href")
    refs <- refs[!grepl("^(data:|https?:|//)", refs)]
    for (ref in refs) {
      target <- file.path(dirname(page), utils::URLdecode(sub("[?#].*$", "", ref)))
      expect_true(file.exists(target), info = paste(basename(page), ref))
    }
  }
  files <- list.files(root, recursive = TRUE)
  expect_true(all(nchar(paste0("datapond/inst/guide/", files), type = "bytes") <= 100L))
})
