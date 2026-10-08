# Fixtures --------------------------------------------------------------------

publishing <- function() load_common("publishing.R")

write_manifest <- function(rows) {
  path <- withr::local_tempfile(fileext = ".tsv", .local_envir = parent.frame())
  utils::write.table(rows, path, sep = "\t", quote = FALSE, row.names = FALSE)
  path
}

# nolint start: line_length_linter.
manifest_rows <- tibble::tribble(
  ~published_at,           ~source,                  ~token, ~url,
  "2026-09-28T13:03:38Z",  "support.html",           "old",  "https://example.org/r/old/support.html",
  "2026-10-05T13:06:12Z",  "support.html",           "new",  "https://example.org/r/new/support.html",
  "2026-10-01T12:00:00Z",  "signal-check.html",      "sc",   ""
)
# nolint end

# Lookup ----------------------------------------------------------------------

test_that("the newest row for a file gives its URL", {
  manifest <- write_manifest(manifest_rows)
  url <- publishing()$published_url("support.html", manifest = manifest)
  expect_identical(url,
                   "https://example.org/r/new/support.html?utm_source=dashboard_link")
})

test_that("utm_source = NULL gives a bare URL", {
  manifest <- write_manifest(manifest_rows)
  url <- publishing()$published_url("support.html", utm_source = NULL,
                                    manifest = manifest)
  expect_identical(url, "https://example.org/r/new/support.html")
})

# Fallback --------------------------------------------------------------------

test_that("a missing manifest gives NULL and says why", {
  missing <- file.path(tempdir(), "no-such-dir", "paths.tsv")
  expect_message(
    url <- publishing()$published_url("support.html", manifest = missing),
    "not found"
  )
  expect_null(url)
})

test_that("a file not in the manifest gives NULL and says why", {
  manifest <- write_manifest(manifest_rows)
  expect_message(
    url <- publishing()$published_url("ibm-verify.html", manifest = manifest),
    "not published"
  )
  expect_null(url)
})

test_that("a row with no URL does not count as published", {
  manifest <- write_manifest(manifest_rows)
  expect_message(
    url <- publishing()$published_url("signal-check.html", manifest = manifest),
    "not published"
  )
  expect_null(url)
})

# Private data ----------------------------------------------------------------

test_that("a present file is read", {
  dir <- withr::local_tempdir()
  writeLines(c("item,up_to,price", "sms,1000,0.5", "sms,Inf,0.25"),
             file.path(dir, "prices.csv"))
  prices <- publishing()$private_data("prices.csv", dir = dir)
  expect_identical(prices$up_to, c(1000, Inf))
  expect_identical(prices$item, c("sms", "sms"))
})

test_that("a missing file gives NULL and says why", {
  dir <- file.path(tempdir(), "no-such-dir")
  expect_message(prices <- publishing()$private_data("prices.csv", dir = dir),
                 "not found")
  expect_null(prices)
})
