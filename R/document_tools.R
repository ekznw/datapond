# Document tools are provisioned only by an explicit setup call. No startup,
# package-load or ordinary rendering path invokes an installer.
document_tool_run <- function(command, args, wd = NULL, timeout = 180000) {
  tryCatch(processx::run(command, args, wd = wd, timeout = timeout,
    error_on_status = FALSE), error = function(e)
      list(status = 1L, stdout = "", stderr = conditionMessage(e)))
}

document_quarto_path <- function() {
  configured <- Sys.getenv("DATAPOND_QUARTO", Sys.getenv("QUARTO_PATH", ""))
  if (nzchar(configured)) {
    if (dir.exists(configured)) configured <- file.path(configured,
      if (.Platform$OS.type == "windows") "quarto.cmd" else "quarto")
    return(configured)
  }
  path <- unname(Sys.which("quarto"))
  if (!nzchar(path)) {
    bundled <- Sys.getenv("RSTUDIO_QUARTO", "")
    if (nzchar(bundled)) {
      candidates <- file.path(c(bundled, file.path(bundled, "bin")),
        if (.Platform$OS.type == "windows") "quarto.cmd" else "quarto")
      found <- candidates[file.exists(candidates)]
      if (length(found)) path <- found[1]
    }
  }
  path
}

document_python_path <- function() {
  path <- Sys.getenv("DATAPOND_RELEASE_PYTHON", "")
  if (!nzchar(path)) path <- unname(Sys.which("python3"))
  if (!nzchar(path)) path <- unname(Sys.which("python"))
  path
}

document_tinytex_roots <- function() {
  if (.Platform$OS.type == "windows") {
    app <- Sys.getenv("APPDATA", "")
    if (!nzchar(app) || grepl("[^ -~]", app)) app <- Sys.getenv("ProgramData", "")
    if (!nzchar(app)) return(character())
    file.path(app, "TinyTeX")
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    path.expand("~/Library/TinyTeX")
  } else path.expand("~/.TinyTeX")
}

document_tex_paths <- function() {
  # Match Quarto's preference for its own TinyTeX before the system PATH.
  roots <- document_tinytex_roots()
  bins <- unlist(lapply(roots, function(root)
    list.dirs(file.path(root, "bin"), recursive = FALSE, full.names = TRUE)), use.names = FALSE)
  exe <- if (.Platform$OS.type == "windows") "xelatex.exe" else "xelatex"
  manager <- if (.Platform$OS.type == "windows") "tlmgr.bat" else "tlmgr"
  bins <- bins[file.exists(file.path(bins, exe)) & file.exists(file.path(bins, manager))]
  if (length(bins)) return(list(path = file.path(bins[1], exe),
    tlmgr = file.path(bins[1], manager), tinytex = TRUE, existing_root = TRUE))
  list(path = unname(Sys.which("xelatex")), tlmgr = unname(Sys.which("tlmgr")),
    tinytex = FALSE, existing_root = any(dir.exists(roots)))
}

document_tool_probe <- function(path, args = "--version", pattern = NULL) {
  if (!nzchar(path)) return(list(ok = FALSE, detail = "Not found", path = ""))
  result <- document_tool_run(path, args, timeout = 15000)
  output <- trimws(paste(result$stdout, result$stderr))
  ok <- identical(as.integer(result$status), 0L) &&
    (is.null(pattern) || grepl(pattern, output))
  list(ok = ok, path = path, detail = if (nzchar(output)) strsplit(output, "\n")[[1]][1] else
    if (ok) "Available" else "Could not run")
}

document_tools_render_check <- function(quarto, install_packages = FALSE) {
  directory <- tempfile("datapond-document-check-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  results <- list()
  for (kind in c("metadata", "development")) {
    stage <- file.path(directory, kind); dir.create(stage)
    if (kind == "metadata") {
      if (!file.copy(datapond_resource("ekznw_document_theme"), stage, recursive = TRUE))
        stop("Could not stage the metadata theme.")
      file.rename(file.path(stage, "ekznw_document_theme"), file.path(stage, "ekznw-theme"))
      writeLines(c("---", "title: Document tools check", "metadata-files: [ekznw-theme/ekznw.yml]",
        "execute:", "  enabled: false", "---", "# EKZNW theme check", "Fonts, logo and table rendering.",
        "", "| Field | Value |", "|---|---|", "| Result | Test document |"), file.path(stage, "check.qmd"))
      source <- "check.qmd"; format <- "pdf"; expected <- "check.pdf"
    } else {
      template <- datapond_resource("data_dev_template")
      if (!all(file.copy(file.path(template, c("data_dev.qmd", "ekznw-theme")), stage, recursive = TRUE)))
        stop("Could not stage the development template.")
      source <- "data_dev.qmd"; format <- "all"
      expected <- c("data_dev.pdf", "data_dev.docx", "data_dev.html")
    }
    # Overrides are confined to this temporary setup test. Production documents
    # keep automatic downloads disabled, including after setup succeeds.
    result <- document_tool_run(quarto, c("render", source, "--to", format,
      "--no-execute", "--quiet", "-M",
      paste0("latex-auto-install:", if (install_packages) "true" else "false")),
      wd = stage, timeout = if (install_packages) 900000 else 180000)
    ok <- result$status == 0L && all(file.exists(file.path(stage, expected)))
    results[[kind]] <- list(ok = ok, detail = if (ok) paste("Rendered", paste(expected, collapse = ", ")) else
      paste("Render failed:", substr(trimws(paste(result$stderr, result$stdout)), 1L, 4000L)))
  }
  results
}

document_tools_report <- function(verify = TRUE, install_packages = FALSE) {
  quarto <- document_tool_probe(document_quarto_path())
  tex <- document_tex_paths()
  xelatex <- document_tool_probe(tex$path)
  python <- document_tool_probe(document_python_path(),
    c("-c", "import sys, json, zipfile, hashlib; assert sys.version_info >= (3, 9); print(sys.version.split()[0])"))
  items <- list(Quarto = quarto, XeLaTeX = xelatex, `Python 3.9+` = python)
  render <- list(metadata = list(ok = FALSE, detail = "Not tested"),
    development = list(ok = FALSE, detail = "Not tested"))
  if (verify && quarto$ok && xelatex$ok) {
    render <- document_tools_render_check(quarto$path, install_packages)
  }
  items[["Metadata PDF"]] <- render$metadata
  items[["Development PDF, Word and HTML"]] <- render$development
  checks <- data.frame(tool = names(items),
    ready = vapply(items, function(x) isTRUE(x$ok), logical(1)),
    detail = vapply(items, function(x) x$detail, character(1)), row.names = NULL)
  list(ready = all(checks$ready), checks = checks, quarto = quarto, xelatex = xelatex,
    python = python, tex = tex, verified = verify)
}

document_tools_show <- function(report) {
  print(report$checks, row.names = FALSE)
  if (!report$quarto$ok) message("Install Quarto from https://quarto.org/docs/get-started/ and restart R. ",
    "For a custom installation set DATAPOND_QUARTO to the executable path, then rerun setup_document_tools().")
  if (!report$python$ok) message("Install Python 3.9+ from https://www.python.org/downloads/ and make it available on PATH, ",
    "or set DATAPOND_RELEASE_PYTHON to its executable path. Python is needed for saved-document inventories and releases.")
  if (!report$ready) message("Document setup is not yet verified. See the failed or untested checks above.")
  invisible(report)
}

#' Check or set up Data Pond document tools
#'
#' Checks Quarto, XeLaTeX and Python 3.9+, and renders temporary examples using
#' both bundled EKZNW themes. No dataset is opened or changed. Checking never
#' installs tools or TeX packages. Word-template editing needs none of these tools.
#'
#' Explicit setup can download TinyTeX through Quarto and provision its TeX
#' packages. Existing system TeX installations are tested but never modified.
#' Missing Quarto and Python installations receive official installation links.
#' Setup is not run at package installation, package load or application startup.
#'
#' @param verify Render the metadata PDF and development PDF, Word and HTML
#'   examples. If `FALSE`, only executable checks run and full readiness remains
#'   unverified.
#' @param quiet Suppress printing the returned checks and installation guidance.
#' @param install Allow TinyTeX and its required packages to be downloaded.
#'   Set `FALSE` for an offline verification only.
#' @return Invisibly, a list containing `ready`, a `checks` data frame, executable
#'   probe results and `verified`. Readiness requires all checks, including renders.
#' @details `DATAPOND_QUARTO` (or `QUARTO_PATH`) can select the Quarto executable;
#'   `DATAPOND_RELEASE_PYTHON` can select Python. Configure these in `.Renviron`
#'   for subsequent sessions. TinyTeX installed by Quarto need not be on PATH.
#'   Renders use temporary files and disabled code execution. Installations need
#'   internet access; after successful setup, ordinary rendering does not install
#'   packages. A failed render includes diagnostic output in the result.
#' @export
check_document_tools <- function(verify = TRUE, quiet = FALSE) {
  report <- document_tools_report(verify = verify)
  if (!quiet) document_tools_show(report)
  invisible(report)
}

#' @rdname check_document_tools
#' @export
setup_document_tools <- function(install = TRUE, quiet = FALSE) {
  report <- check_document_tools(quiet = TRUE)
  if (install && report$quarto$ok && !report$ready) {
    if (!report$xelatex$ok && !nzchar(report$tex$path) && !report$tex$existing_root) {
      if (!quiet) message("Installing TinyTeX using Quarto. This needs an internet connection and may take several minutes.")
      result <- document_tool_run(report$quarto$path, c("install", "tinytex", "--no-prompt"), timeout = 900000)
      if (result$status != 0L) stop("TinyTeX installation failed: ", result$stderr, result$stdout, call. = FALSE)
      report <- check_document_tools(quiet = TRUE)
    }
    render_failed <- any(!report$checks$ready[4:5])
    if (report$tex$tinytex && render_failed) {
      if (!quiet) message("Preparing TinyTeX fonts and packages for the EKZNW themes.")
      result <- document_tool_run(report$tex$tlmgr,
        c("install", "tex-gyre", "koma-script", "needspace", "fontawesome5", "tcolorbox"), timeout = 900000)
      if (result$status != 0L) stop("TinyTeX package setup failed: ", result$stderr, result$stdout, call. = FALSE)
      document_tools_report(install_packages = TRUE)
      # Verify again with downloads disabled, as in ordinary app rendering.
      report <- check_document_tools(quiet = TRUE)
    }
    if (!report$ready && !report$tex$tinytex && render_failed && !quiet)
      message("Your existing TeX installation was left unchanged. Use its package manager or ask IT to resolve the render errors, then rerun setup.")
  }
  if (!quiet) {
    document_tools_show(report)
    if (report$ready) message("Document tools are ready. Both EKZNW templates rendered successfully.")
  }
  invisible(report)
}

mod_document_tools_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$details(class = "config-help",
    shiny::tags$summary("Document tools"),
    shiny::p("Check this computer before generating dataset PDFs. This check renders temporary examples and does not install software."),
    shiny::actionButton(ns("check"), "Check document tools"),
    shiny::verbatimTextOutput(ns("status")),
    shiny::p("For first-time setup, run in the R console:"),
    shiny::tags$code("datapond::setup_document_tools()"))
}

mod_document_tools_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactiveVal("Not checked in this session.")
    output$status <- shiny::renderText({ access_require("read"); status() })
    shiny::observeEvent(input$check, {
      access_require("admin", touch = TRUE)
      tryCatch({
        report <- shiny::withProgress(message = "Checking document tools", value = 0.2,
          check_document_tools(quiet = TRUE))
        status(paste(if (report$ready) "Document tools ready." else "Setup needs attention.",
          paste(paste0(report$checks$tool, ": ", ifelse(report$checks$ready, "OK", "Needs attention"),
            " \u2014 ", report$checks$detail), collapse = "\n"), sep = "\n"))
      }, error = function(e) status(conditionMessage(e)))
    })
  })
}
