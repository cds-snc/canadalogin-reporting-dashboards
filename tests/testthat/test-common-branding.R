# Logo ------------------------------------------------------------------------

test_that("the logo defaults to the English-first mark", {
  expect_identical(basename(cds_logo_path()), "cds-snc.png")
  expect_identical(
    basename(cds_logo_path(canada_wordmark = TRUE)),
    "EN_Square+CANADA.jpg"
  )
})

test_that("lang fr picks the French-first mark", {
  expect_identical(basename(cds_logo_path(lang = "fr")), "snc-cds.png")
  expect_identical(
    basename(cds_logo_path(canada_wordmark = TRUE, lang = "fr")),
    "FR_Square+CANADA.jpg"
  )
})

test_that("the logo is the same on every call", {
  expect_identical(cds_logo_path(), cds_logo_path())
})

test_that("an unknown lang is an error", {
  expect_error(cds_logo_path(lang = "de"))
})
