# Shorten Quarto's generated stylesheet names for portable R source archives.
# Run on the staged site before replacing the bundled guide.
normalize_user_guide <- function(root) {
  styles <- list.files(root, recursive = TRUE, full.names = TRUE,
                       pattern = "^quarto-syntax-highlighting-[[:xdigit:]]+\\.css$")
  for (style in styles) {
    old <- basename(style)
    new <- sub("^quarto-syntax-highlighting-", "syntax-", old)
    destination <- file.path(dirname(style), new)
    if (file.exists(destination) || !file.rename(style, destination)) {
      stop("Could not rename guide stylesheet: ", style)
    }
    pages <- list.files(root, pattern = "\\.html$", recursive = TRUE,
                        full.names = TRUE)
    for (page in pages) {
      text <- readChar(page, file.info(page)$size, useBytes = TRUE)
      if (grepl(old, text, fixed = TRUE)) {
        text <- gsub(old, new, text, fixed = TRUE)
        writeChar(text, page, eos = NULL, useBytes = TRUE)
      }
    }
  }
  invisible(root)
}
