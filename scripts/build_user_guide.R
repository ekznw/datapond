#!/usr/bin/env Rscript
# Run from the repository root. Rendering never executes the example commands.
if (!file.exists("DESCRIPTION") || !file.exists("guide/_quarto.yml")) {
  stop("Run this script from the DataPond repository root.")
}
quarto <- Sys.getenv("DATAPOND_QUARTO", unset = "")
if (!nzchar(quarto)) quarto <- Sys.getenv("QUARTO_PATH", unset = "")
if (!nzchar(quarto)) quarto <- unname(Sys.which("quarto"))
if (!nzchar(quarto) || !file.exists(quarto)) stop("Install Quarto or set DATAPOND_QUARTO to its executable.")
version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
writeLines(paste0("version: '", version, "'"), "guide/_variables.yml")
dir.create("guide/assets", recursive = TRUE, showWarnings = FALSE)
if (!file.copy("inst/www/datapond-logo.png", "guide/assets/datapond-logo.png", overwrite = TRUE)) {
  stop("Could not copy the guide logo.")
}
status <- system2(quarto, c("render", "guide"))
if (status != 0L) stop("Quarto rendering failed; the bundled guide was not replaced.")
site <- "guide/_site"
required <- c("index.html", "offline.html", "search.json")
if (!all(file.exists(file.path(site, required)))) stop("Rendered guide is incomplete.")
# Stage the copy first so a rendering/copy failure leaves the bundled guide intact.
stage <- tempfile("datapond-guide-", tmpdir = "inst")
dir.create(stage)
files <- list.files(site, full.names = TRUE, all.files = FALSE)
if (!all(file.copy(files, stage, recursive = TRUE))) stop("Could not stage the rendered guide: ", stage)
source("scripts/normalize_user_guide.R")
normalize_user_guide(stage)
dest <- "inst/guide"
previous <- tempfile("datapond-guide-previous-", tmpdir = "inst")
had_previous <- dir.exists(dest)
if (had_previous && !file.rename(dest, previous)) stop("Could not preserve the previous guide.")
if (!file.rename(stage, dest)) {
  if (had_previous) file.rename(previous, dest)
  stop("Could not install the rendered guide.")
}
if (had_previous) unlink(previous, recursive = TRUE)
message("Built DataPond ", version, " guide: ", normalizePath(file.path(dest, "index.html")))
