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

test_that("administrator help asks for a username before offering commands", {
  pond <- "/pond/<script>not executable</script>"
  for (configured in c(FALSE, TRUE)) {
    html <- xml2::read_html(as.character(datapond:::access_console_help_ui(pond, configured)))
    expect_length(xml2::xml_find_all(html, if (configured) "//details[not(@open)]" else "//details[@open]"), 1L)
    expect_length(xml2::xml_find_all(html, "//script"), 0L)
    expect_length(xml2::xml_find_all(html, "//input[@id='access_console_username']"), 1L)
    expect_length(xml2::xml_find_all(html, "//*[@id='access_console_command']"), 1L)
    expect_length(xml2::xml_find_all(html, "//button[@data-copy-command]"), 0L)
    expect_length(xml2::xml_find_all(xml2::read_html(as.character(
      datapond:::access_console_command_ui(pond, configured, ""))), "//button[@data-copy-command]"), 0L)
    command_ui <- xml2::read_html(as.character(datapond:::access_console_command_ui(pond, configured, "paul")))
    key <- if (configured) "recover" else "setup_new"
    button <- xml2::xml_find_first(command_ui, "//button[@data-copy-command]")
    expect_identical(xml2::xml_attr(button, "data-copy-command"), paste0("access_", key, "_command"))
    command <- xml2::xml_text(xml2::xml_find_first(command_ui, paste0("//pre[@id='access_", key, "_command']")))
    expect_identical(command, datapond:::access_console_commands(pond, username = "paul")[[key]])
  }
})

test_that("first administrator selector shows pond people and fills their person ID", {
  pond <- tempfile("setup-persons-")
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  datapond:::seed_pond_parties(pond)
  people <- datapond:::get_persons(pond)
  users <- datapond:::get_users(pond)
  expect_gt(nrow(people), 0L)
  person <- people[people$id %in% users$person_id, , drop = FALSE][1, ]
  user <- users[users$person_id == person$id, , drop = FALSE]

  html <- xml2::read_html(as.character(datapond:::access_console_help_ui(pond, FALSE)))
  options <- xml2::xml_find_all(html, "//select[@id='access_setup_person']/option")
  expect_identical(xml2::xml_attr(options[1], "value"), "")
  selected <- options[xml2::xml_attr(options, "value") == as.character(person$id)]
  expect_length(selected, 1L)
  expect_match(xml2::xml_text(selected), paste("person ID", person$id), fixed = TRUE)
  expect_match(xml2::xml_text(selected), paste("user ID", user$id), fixed = TRUE)
  command_ui <- xml2::read_html(as.character(datapond:::access_console_command_ui(pond, FALSE, "paul", person$id)))
  command <- xml2::xml_text(xml2::xml_find_first(command_ui, "//pre[@id='access_setup_existing_command']"))
  expect_match(command, paste0("person_id = ", person$id, "L"), fixed = TRUE)
  expect_match(command, 'username = "paul"', fixed = TRUE)
  expect_false(grepl("YOUR_PERSON_ID", command, fixed = TRUE))
  expect_length(xml2::xml_find_all(command_ui, "//pre[@id='access_setup_new_command']"), 0L)
})

test_that("selecting a pond person updates the command on the sign-in screen", {
  pond <- tempfile("setup-person-screen-")
  on.exit(unlink(pond, recursive = TRUE), add = TRUE)
  datapond:::init_db(pond)
  datapond:::seed_pond_parties(pond)
  person_id <- datapond:::get_persons(pond)$id[1]
  shiny::testServer(function(input, output, session) {
    state <- shiny::reactiveValues(data_pond = pond, active_user_id = NULL)
    datapond:::access_server(input, output, session, state,
      switch_pond = function(path) NULL)
  }, {
    before <- xml2::read_html(output$access_console_command$html)
    expect_length(xml2::xml_find_all(before, "//button[@data-copy-command]"), 0L)
    session$setInputs(access_console_username = "paul", access_setup_person = as.character(person_id))
    after <- xml2::read_html(output$access_console_command$html)
    command <- xml2::xml_text(xml2::xml_find_first(after, "//pre[@id='access_setup_existing_command']"))
    expect_match(command, paste0("person_id = ", person_id, "L"), fixed = TRUE)
    expect_match(command, 'username = "paul"', fixed = TRUE)
  })
})

test_that("sign-in loads theme preferences and copy support before authentication", {
  html <- htmltools::renderTags(datapond:::access_ui())
  markup <- xml2::read_html(paste(html$head, html$html))
  sources <- xml2::xml_attr(xml2::xml_find_all(markup, "//script[@src]"), "src")
  expect_true(any(grepl("display-theme.js", sources, fixed = TRUE)))
  expect_true(any(grepl("access-help.js", sources, fixed = TRUE)))
})

test_that("compiled sign-in screens survive a Windows locale change", {
  skip_if(.Platform$OS.type != "windows")
  original_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", original_locale), add = TRUE)
  utf8_locale <- suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8"))
  skip_if(!nzchar(utf8_locale), "A Windows UTF-8 locale is required")
  Sys.setlocale("LC_CTYPE", "C")
  env <- new.env(parent = asNamespace("datapond"))
  source(file.path(datapond_test_source_root(), "R", "mod_access.R"),
    local = env, encoding = "UTF-8")
  env$access_setup_people <- function(...) data.frame(
    id = 1L, name_first = "Test", name_last = "Owner", user_id = 2L)
  serialized_screen <- serialize(compiler::cmpfun(env$access_login_ui), NULL)
  Sys.setlocale("LC_CTYPE", utf8_locale)
  screen <- unserialize(serialized_screen)
  for (configured in c(FALSE, TRUE)) {
    html <- htmltools::renderTags(screen("/unused", configured,
      registry = datapond:::empty_connected_pond_registry()))$html
    expect_match(html, "Sign in to DataPond", fixed = TRUE)
    if (!configured) {
      expect_match(html, "- person ID 1", fixed = TRUE)
      expect_match(html, "I am not listed - create a new person", fixed = TRUE)
    }
  }
})
