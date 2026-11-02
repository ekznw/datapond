document_report_fixture <- function(quarto = TRUE, tex = TRUE, python = TRUE,
                                    rendered = TRUE, tinytex = FALSE, existing = FALSE) {
  probe <- function(ok, path) list(ok = ok, path = if (ok) path else "", detail = if (ok) "Available" else "Not found")
  list(ready = all(quarto, tex, python, rendered),
    quarto = probe(quarto, "/test/quarto"), xelatex = probe(tex, "/test/xelatex"),
    python = probe(python, "/test/python"), verified = TRUE,
    tex = list(path = if (tex) "/test/xelatex" else "", tlmgr = "/test/tlmgr", tinytex = tinytex, existing_root = existing),
    checks = data.frame(tool = c("Quarto", "XeLaTeX", "Python 3.9+", "Metadata PDF", "Development PDF, Word and HTML"),
      ready = c(quarto, tex, python, rendered, rendered), detail = "Test result"))
}

test_that("local tool discovery honours explicit paths and Quarto-managed TinyTeX", {
  keys <- c("DATAPOND_QUARTO", "DATAPOND_RELEASE_PYTHON")
  previous <- Sys.getenv(keys, unset = NA_character_)
  on.exit({
    Sys.unsetenv(keys[is.na(previous)])
    if (any(!is.na(previous))) do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
  }, add = TRUE)
  Sys.setenv(DATAPOND_QUARTO = "/custom tools/quarto", DATAPOND_RELEASE_PYTHON = "/custom tools/python")
  expect_identical(datapond:::document_quarto_path(), "/custom tools/quarto")
  expect_identical(datapond:::document_python_path(), "/custom tools/python")
  root <- tempfile(); bin <- file.path(root, "bin", "test-platform")
  dir.create(bin, recursive = TRUE); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.create(file.path(bin, if (.Platform$OS.type == "windows") c("xelatex.exe", "tlmgr.bat") else c("xelatex", "tlmgr")))
  testthat::local_mocked_bindings(document_tinytex_roots = function() root, .package = "datapond")
  tex <- datapond:::document_tex_paths()
  expect_true(tex$tinytex)
  expect_identical(dirname(tex$path), bin)
})

test_that("checking executables does not run a renderer or install tools", {
  calls <- list()
  testthat::local_mocked_bindings(
    document_quarto_path = function() "/test/quarto",
    document_python_path = function() "/test/python",
    document_tex_paths = function() list(path = "/test/xelatex"),
    document_tool_run = function(command, args, ...) {
      calls[[length(calls) + 1L]] <<- list(command, args)
      list(status = 0L, stdout = "Available", stderr = "")
    },
    document_tools_render_check = function(...) stop("Must not render"), .package = "datapond")
  report <- datapond::check_document_tools(verify = FALSE, quiet = TRUE)
  expect_false(report$ready)
  expect_false(report$verified)
  expect_true(all(report$checks$ready[1:3]))
  expect_length(calls, 3)
  expect_false(any(grepl("install", unlist(calls))))
  expect_match(calls[[3]][[2]][2], "sys.version_info >= (3, 9)", fixed = TRUE)
})

test_that("missing Quarto receives guidance and no downloads", {
  testthat::local_mocked_bindings(
    check_document_tools = function(...) document_report_fixture(quarto = FALSE, rendered = FALSE),
    document_tool_run = function(...) stop("Must not install"), .package = "datapond")
  expect_message(report <- datapond::setup_document_tools(), "Install Quarto")
  expect_false(report$ready)
})

test_that("offline setup and ready installations never invoke installers", {
  testthat::local_mocked_bindings(
    check_document_tools = function(...) document_report_fixture(),
    document_tool_run = function(...) stop("Must not install"), .package = "datapond")
  expect_true(datapond::setup_document_tools(quiet = TRUE)$ready)
  expect_true(datapond::setup_document_tools(install = FALSE, quiet = TRUE)$ready)
})

test_that("explicit setup installs missing TinyTeX and verifies without downloads", {
  checks <- 0L; calls <- list(); provisioning <- logical()
  testthat::local_mocked_bindings(
    check_document_tools = function(...) {
      checks <<- checks + 1L
      if (checks == 1L) document_report_fixture(tex = FALSE, rendered = FALSE) else
        document_report_fixture(rendered = checks >= 3L, tinytex = TRUE)
    },
    document_tool_run = function(command, args, ...) {
      calls[[length(calls) + 1L]] <<- list(command = command, args = args)
      list(status = 0L, stdout = "", stderr = "")
    },
    document_tools_report = function(install_packages, ...) {
      provisioning <<- c(provisioning, install_packages)
      document_report_fixture(tinytex = TRUE)
    }, .package = "datapond")
  expect_true(datapond::setup_document_tools(quiet = TRUE)$ready)
  expect_identical(calls[[1]]$args, c("install", "tinytex", "--no-prompt"))
  expect_identical(calls[[2]]$command, "/test/tlmgr")
  expect_true("tex-gyre" %in% calls[[2]]$args)
  expect_identical(provisioning, TRUE)
  expect_identical(checks, 3L)
})

test_that("system TeX, failed installations and incomplete TinyTeX are protected", {
  testthat::local_mocked_bindings(
    check_document_tools = function(...) document_report_fixture(rendered = FALSE),
    document_tool_run = function(...) stop("Must not modify system TeX"), .package = "datapond")
  expect_false(datapond::setup_document_tools(quiet = TRUE)$ready)
  testthat::local_mocked_bindings(
    check_document_tools = function(...) document_report_fixture(tex = FALSE, rendered = FALSE, existing = TRUE), .package = "datapond")
  expect_false(datapond::setup_document_tools(quiet = TRUE)$ready)
  testthat::local_mocked_bindings(
    check_document_tools = function(...) document_report_fixture(tex = FALSE, rendered = FALSE),
    document_tool_run = function(...) list(status = 1L, stdout = "", stderr = "Download failed"), .package = "datapond")
  expect_error(datapond::setup_document_tools(quiet = TRUE), "TinyTeX installation failed: Download failed")
})

test_that("temporary checks exercise both themes without execution or package downloads", {
  calls <- list()
  theme <- datapond:::datapond_resource("ekznw_document_theme", "ekznw.yml")
  before <- tools::md5sum(theme)
  testthat::local_mocked_bindings(document_tool_run = function(command, args, wd, ...) {
    calls[[length(calls) + 1L]] <<- list(args = args, wd = wd)
    expect_true(file.exists(file.path(wd, "ekznw-theme", "ekznw.yml")))
    outputs <- if (args[2] == "check.qmd") "check.pdf" else c("data_dev.pdf", "data_dev.docx", "data_dev.html")
    file.create(file.path(wd, outputs))
    list(status = 0L, stdout = "", stderr = "")
  }, .package = "datapond")
  result <- datapond:::document_tools_render_check("/test/quarto")
  expect_true(result$metadata$ok && result$development$ok)
  expect_true(all(vapply(calls, function(x) "--no-execute" %in% x$args && "latex-auto-install:false" %in% x$args, logical(1))))
  expect_false(any(dir.exists(vapply(calls, `[[`, character(1), "wd"))))
  expect_identical(tools::md5sum(theme), before)
})

test_that("failed rendering and missing outputs cannot report readiness", {
  testthat::local_mocked_bindings(document_tool_run = function(...) list(status = 0L, stdout = "", stderr = "Missing font"), .package = "datapond")
  result <- datapond:::document_tools_render_check("/test/quarto")
  expect_false(result$metadata$ok)
  expect_false(result$development$ok)
  expect_match(result$metadata$detail, "Missing font")
})

test_that("Configuration checks only on request and cannot install tools", {
  local_trusted_access_fixture()

  calls <- 0L
  testthat::local_mocked_bindings(check_document_tools = function(...) {
    calls <<- calls + 1L
    document_report_fixture()
  }, setup_document_tools = function(...) stop("Must not install"), .package = "datapond")
  shiny::testServer(datapond:::mod_document_tools_server, {
    session$flushReact()
    expect_identical(calls, 0L)
    session$setInputs(check = 1)
    expect_identical(calls, 1L)
    expect_match(output$status, "Document tools ready", fixed = TRUE)
  })
  expect_match(as.character(datapond:::config_panel_ui()), "Check document tools", fixed = TRUE)
})
