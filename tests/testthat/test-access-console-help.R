test_that("console commands preserve quoted paths and only call the public console API", {
  pond <- "/pond/quotes\" and 'apostrophe'/back\\slash\nnew line"
  received <- NULL
  local_mocked_bindings(
    setup_pond_access = function(data_pond, username, person_id = NULL,
        first_name = NULL, last_name = NULL) {
      received <<- list(pond = data_pond, username = username, person_id = person_id)
      "test-activation-code"
    },
    recover_pond_access = function(data_pond, username) {
      received <<- list(pond = data_pond, username = username)
      "test-recovery-code"
    }, .package = "datapond"
  )
  commands <- datapond:::access_console_commands(pond)
  for (name in names(commands)) {
    env <- new.env()
    env$YOUR_PERSON_ID <- 42L
    value <- eval(parse(text = commands[[name]]), envir = env)
    expect_identical(received$pond, pond)
    expect_identical(received$username, "your.username")
    expect_identical(value, if (name == "recover") "test-recovery-code" else "test-activation-code")
    if (name == "setup_existing") expect_identical(received$person_id, 42L)
  }
})

test_that("administrator help offers copyable commands appropriate to pond setup", {
  pond <- "/pond/<script>not executable</script>"
  for (configured in c(FALSE, TRUE)) {
    html <- xml2::read_html(as.character(datapond:::access_console_help_ui(pond, configured)))
    expect_length(xml2::xml_find_all(html, "//details[not(@open)]"), 1L)
    expect_length(xml2::xml_find_all(html, "//script"), 0L)
    keys <- if (configured) "recover" else c("setup_existing", "setup_new")
    buttons <- xml2::xml_find_all(html, "//button[@data-copy-command]")
    expect_identical(xml2::xml_attr(buttons, "data-copy-command"), paste0("access_", keys, "_command"))
    expect_true(all(xml2::xml_attr(buttons, "type") == "button"))
    for (key in keys) {
      command <- xml2::xml_text(xml2::xml_find_first(html, paste0("//pre[@id='access_", key, "_command']")))
      expect_identical(command, datapond:::access_console_commands(pond)[[key]])
    }
  }
})

test_that("sign-in loads theme preferences and copy support before authentication", {
  html <- htmltools::renderTags(datapond:::access_ui())
  markup <- xml2::read_html(paste(html$head, html$html))
  sources <- xml2::xml_attr(xml2::xml_find_all(markup, "//script[@src]"), "src")
  expect_true(any(grepl("display-theme.js", sources, fixed = TRUE)))
  expect_true(any(grepl("access-help.js", sources, fixed = TRUE)))
})
