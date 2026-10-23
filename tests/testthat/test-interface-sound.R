test_that("milestone sound messages include their reason", {
  sent <- NULL
  session <- new.env(parent = emptyenv())
  session$sendCustomMessage <- function(type, message) {
    sent <<- list(type = type, message = message)
  }

  expect_true(datapond:::play_finfoot_cue(session, "dataset-saved"))
  expect_identical(sent$type, "datapond-play-finfoot")
  expect_identical(sent$message$reason, "dataset-saved")
})

test_that("finfoot recording and attribution are bundled", {
  audio <- system.file("www", "finfoot-xc1036489.mp3", package = "datapond")
  licence <- system.file("www", "finfoot-xc1036489-LICENSE.txt", package = "datapond")

  expect_true(file.exists(audio))
  expect_gt(file.info(audio)$size, 100000)
  expect_true(file.exists(licence))
  expect_match(paste(readLines(licence), collapse = "\n"), "Dries Van de Loock")
  expect_match(paste(readLines(licence), collapse = "\n"), "CC BY-NC-ND 4.0")
})

test_that("workspace UI exposes the sound control, source and attribution", {
  html <- as.character(htmltools::renderTags(datapond:::ui)$html)

  expect_match(html, "enable_finfoot_sound", fixed = TRUE)
  expect_match(html, "datapond_finfoot_audio", fixed = TRUE)
  expect_match(html, "finfoot-xc1036489.mp3", fixed = TRUE)
  expect_match(html, "Dries Van de Loock, XC1036489", fixed = TRUE)
  expect_match(html, "creativecommons.org/licenses/by-nc-nd/4.0/", fixed = TRUE)
})

test_that("startup sound lasts at least ten seconds", {
  script <- paste(
    readLines(system.file("www", "display-theme.js", package = "datapond")),
    collapse = "\n"
  )

  expect_match(script, "holdDuration = isStartup ? 9000 : 1900", fixed = TRUE)
  expect_match(script, "fadeDuration = isStartup ? 2000 : 1100", fixed = TRUE)
})
