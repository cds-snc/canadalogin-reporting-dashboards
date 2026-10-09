readership <- load_dashboard("readership")

# Baseline --------------------------------------------------------------------

test_that("clean data passes every check", {
  result <- run_readership_preflight(readership_with_data())
  expect_true(result$passed)
  expect_length(result$failed, 0)
})

test_that("every check keeps its slug", {
  result <- run_readership_preflight(readership_with_data())
  expect_identical(
    purrr::map_chr(result$checks, "slug"),
    c("freshness", "tables-agree", "known-pages", "no-tokens",
      "edition-numbers")
  )
})

# freshness -------------------------------------------------------------------

test_that("freshness fails when the export stops", {
  env <- readership_with_data(exported = readership_today - 5L)
  expect_identical(failed_slugs(run_readership_preflight(env)), "freshness")
})

test_that("a quiet weekend is not a stale export", {
  traffic <- readership_traffic(through = readership_through - 2L)
  env <- readership_with_data(traffic = traffic)
  expect_true(run_readership_preflight(env)$passed)
})

test_that("freshness fails when nobody has read a page in over a week", {
  traffic <- readership_traffic(through = readership_through - 8L)
  env <- readership_with_data(traffic = traffic)
  expect_identical(failed_slugs(run_readership_preflight(env)), "freshness")
})

# tables-agree ----------------------------------------------------------------

test_that("a day the two tables disagree on fails", {
  traffic <- readership_traffic()
  events <- readership_events(traffic) |>
    dplyr::filter(!(day == as.Date("2026-09-15") & eventname == "page_view"))
  env <- readership_with_data(traffic = traffic, events = events)
  result <- run_readership_preflight(env)
  expect_identical(failed_slugs(result), "tables-agree")
  expect_match(check_named(result, "tables-agree")$details, "Sep 15")
})

# known-pages -----------------------------------------------------------------

test_that("an unrecognized page fails", {
  traffic <- dplyr::bind_rows(
    readership_traffic(),
    tibble::tibble(day = readership_through, page_kind = "other",
                   pagepath_redacted =
                     "/canadalogin-signal-check-publishing/(unrecognized)",
                   pagetitle = "?", views = 1, streamname = "Signal Check",
                   sessionsource = "(direct)", sessionmedium = "(none)",
                   sessions = 1)
  )
  env <- readership_with_data(traffic = traffic)
  expect_identical(failed_slugs(run_readership_preflight(env)), "known-pages")
})

# no-tokens -------------------------------------------------------------------

test_that("a path that kept its token fails, without naming the token", {
  traffic <- readership_traffic() |>
    dplyr::mutate(pagepath_redacted = sub(
      "/r/ibm-verify", "/r/AbCd1234/ibm-verify", pagepath_redacted
    ))
  env <- readership_with_data(traffic = traffic)
  result <- run_readership_preflight(env)
  expect_identical(failed_slugs(result), "no-tokens")
  expect_no_match(check_named(result, "no-tokens")$details, "AbCd1234")
})

# edition-numbers -------------------------------------------------------------

test_that("an edition without a number in its title fails", {
  traffic <- readership_traffic() |>
    dplyr::mutate(pagetitle = sub("Signal Check #4", "Signal Check",
                                  pagetitle))
  env <- readership_with_data(traffic = traffic)
  expect_identical(failed_slugs(run_readership_preflight(env)),
                   "edition-numbers")
})

test_that("two editions with one number fails", {
  traffic <- readership_traffic() |>
    dplyr::mutate(pagetitle = sub("#4", "#3", pagetitle))
  env <- readership_with_data(traffic = traffic)
  expect_identical(failed_slugs(run_readership_preflight(env)),
                   "edition-numbers")
})

test_that("a retitled edition with a new number fails", {
  traffic <- readership_traffic() |>
    dplyr::mutate(pagetitle = ifelse(
      day == as.Date("2026-09-01") & grepl("#4", pagetitle),
      "CanadaLogin Signal Check #9", pagetitle
    ))
  env <- readership_with_data(traffic = traffic)
  expect_identical(failed_slugs(run_readership_preflight(env)),
                   "edition-numbers")
})
