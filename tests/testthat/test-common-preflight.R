# Fixtures --------------------------------------------------------------------

passing_result <- list(passed = TRUE, checks = list(), failed = list())

failing_result <- function(details = c("first problem", "second problem")) {
  check <- list(slug = "some-check", title = "A <check> & its title",
                passed = FALSE, details = details)
  list(passed = FALSE, checks = list(check), failed = list(check))
}

# Banner ----------------------------------------------------------------------

test_that("the banner file is written empty when every check passes", {
  path <- withr::local_tempfile(fileext = ".html")
  write_preflight_banner(passing_result, path)
  expect_true(file.exists(path))
  expect_identical(readLines(path), character())
})

test_that("the banner writer returns FALSE when every check passes", {
  path <- withr::local_tempfile(fileext = ".html")
  expect_false(write_preflight_banner(passing_result, path))
})

test_that("the banner writer returns TRUE when a check fails", {
  path <- withr::local_tempfile(fileext = ".html")
  expect_true(write_preflight_banner(failing_result(), path))
})

test_that("the banner is an alert", {
  path <- withr::local_tempfile(fileext = ".html")
  write_preflight_banner(failing_result(), path)
  expect_match(paste(readLines(path), collapse = "\n"), 'role="alert"', fixed = TRUE)
})

test_that("the banner names each failed check by slug", {
  path <- withr::local_tempfile(fileext = ".html")
  write_preflight_banner(failing_result(), path)
  expect_match(paste(readLines(path), collapse = "\n"),
               "<code>some-check</code>", fixed = TRUE)
})

test_that("the banner escapes HTML in a check's title", {
  path <- withr::local_tempfile(fileext = ".html")
  write_preflight_banner(failing_result(), path)
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, "A &lt;check&gt; &amp; its title", fixed = TRUE)
  expect_no_match(html, "<check>", fixed = TRUE)
})

test_that("the banner joins a check's details with semicolons", {
  path <- withr::local_tempfile(fileext = ".html")
  write_preflight_banner(failing_result(), path)
  expect_match(paste(readLines(path), collapse = "\n"),
               "first problem; second problem", fixed = TRUE)
})

# Status file -----------------------------------------------------------------

test_that("the status file records a pass", {
  path <- withr::local_tempfile(fileext = ".json")
  write_preflight_status(passing_result, path)
  status <- jsonlite::read_json(path)
  expect_true(status$passed)
  expect_length(status$failed, 0)
})

test_that("the status file records a failure with its slug and title", {
  path <- withr::local_tempfile(fileext = ".json")
  write_preflight_status(failing_result(), path)
  status <- jsonlite::read_json(path)
  expect_false(status$passed)
  expect_identical(status$failed[[1]]$slug, "some-check")
  expect_identical(status$failed[[1]]$title, "A <check> & its title")
})

test_that("the status file writes several details as an array", {
  path <- withr::local_tempfile(fileext = ".json")
  write_preflight_status(failing_result(), path)
  status <- jsonlite::read_json(path)
  expect_identical(unlist(status$failed[[1]]$details),
                   c("first problem", "second problem"))
})

# Publishing expects one detail to be a scalar.
test_that("the status file writes a single detail as a bare string", {
  path <- withr::local_tempfile(fileext = ".json")
  write_preflight_status(failing_result("only problem"), path)
  raw <- paste(readLines(path), collapse = "\n")
  expect_match(raw, '"details": "only problem"', fixed = TRUE)
})

test_that("the status file stamps a UTC time", {
  path <- withr::local_tempfile(fileext = ".json")
  write_preflight_status(passing_result, path)
  status <- jsonlite::read_json(path)
  expect_match(status$generated_at, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("the status writer returns whether the checks passed", {
  path <- withr::local_tempfile(fileext = ".json")
  expect_true(write_preflight_status(passing_result, path))
  expect_false(write_preflight_status(failing_result(), path))
})
