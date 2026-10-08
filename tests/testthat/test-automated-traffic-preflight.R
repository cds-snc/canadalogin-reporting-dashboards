traffic <- load_dashboard("automated-traffic")

# Baseline --------------------------------------------------------------------

test_that("clean data passes every check", {
  result <- run_traffic_preflight(traffic_with_data())
  expect_true(result$passed)
  expect_length(result$failed, 0)
})

test_that("every check keeps its slug", {
  result <- run_traffic_preflight(traffic_with_data())
  expect_identical(
    purrr::map_chr(result$checks, "slug"),
    c("freshness", "no-missing-days", "enough-baseline", "known-days-fire")
  )
})

test_that("a failed check raises a warning naming it", {
  sms <- traffic_sms(traffic_days(through = as.Date("2026-10-01")))
  env <- traffic_with_data(sms = sms)
  expect_warning(
    suppressMessages(env$run_preflight_safety_check(
      NULL, traffic_thresholds, traffic_known_days, today = traffic_today
    )),
    "freshness"
  )
})

# freshness -------------------------------------------------------------------

test_that("freshness fails when a source stops early", {
  sms <- traffic_sms(traffic_days(through = as.Date("2026-10-05")))
  result <- run_traffic_preflight(traffic_with_data(sms = sms))
  expect_identical(failed_slugs(result), "freshness")
})

test_that("freshness allows GA its longer lag", {
  codes <- traffic_codes(traffic_days(as.Date("2026-06-01"), traffic_today - 3L))
  result <- run_traffic_preflight(traffic_with_data(codes = codes))
  expect_true(result$passed)
})

# no-missing-days -------------------------------------------------------------

test_that("a missing day fails, rather than reading as a quiet one", {
  sms <- traffic_sms() |> dplyr::filter(day != as.Date("2026-09-02"))
  result <- run_traffic_preflight(traffic_with_data(sms = sms))
  expect_identical(failed_slugs(result), "no-missing-days")
  expect_match(check_named(result, "no-missing-days")$details[1], "Sep 02")
})

test_that("a first sign-in time that did not parse fails", {
  pairs <- dplyr::bind_rows(
    traffic_pairs(),
    tibble::tibble(first_day = as.Date(NA), service_day = as.Date(NA), accounts = 3)
  )
  result <- run_traffic_preflight(traffic_with_data(pairs = pairs))
  expect_identical(failed_slugs(result), "no-missing-days")
})

test_that("a gap before the baseline window does not count", {
  sms <- traffic_sms(traffic_days(from = as.Date("2026-01-01"))) |>
    dplyr::filter(day != as.Date("2026-02-10"))
  result <- run_traffic_preflight(traffic_with_data(sms = sms))
  expect_true(result$passed)
})

# enough-baseline -------------------------------------------------------------

test_that("weeks of flagged days leave too little baseline", {
  bot_days <- seq(as.Date("2026-09-01"), as.Date("2026-09-25"), by = "day")
  result <- run_traffic_preflight(
    traffic_with_data(sms = traffic_sms(bot_days = bot_days))
  )
  expect_true("enough-baseline" %in% failed_slugs(result))
})

test_that("a source too new to compare does not fail", {
  codes <- traffic_codes(traffic_days(as.Date("2026-09-01"), traffic_today - 2L))
  result <- run_traffic_preflight(traffic_with_data(codes = codes))
  expect_false("enough-baseline" %in% failed_slugs(result))
})

# known-days-fire -------------------------------------------------------------

test_that("a confirmed day that stops firing fails", {
  sms <- traffic_sms(bot_days = as.Date(NA))
  result <- run_traffic_preflight(traffic_with_data(sms = sms))
  expect_identical(failed_slugs(result), "known-days-fire")
  expect_match(check_named(result, "known-days-fire")$details, "SMS codes")
})

test_that("a threshold edit that blinds a method fails", {
  loose <- traffic_thresholds
  loose$idle$idle_share_at_least <- 0.99
  result <- run_traffic_preflight(traffic_with_data(), thresholds = loose)
  expect_identical(failed_slugs(result), "known-days-fire")
})

test_that("a method not expected to fire is not checked", {
  known <- dplyr::mutate(traffic_known_days, errors = FALSE)
  codes <- traffic_codes(bot_days = as.Date(NA))
  result <- run_traffic_preflight(traffic_with_data(codes = codes), known = known)
  expect_true(result$passed)
})

test_that("a missing day of email codes fails, rather than going unscored", {
  seconds <- dplyr::filter(traffic_email_seconds(), day != as.Date("2026-09-02"))
  result <- run_traffic_preflight(traffic_with_data(seconds = seconds))
  expect_identical(failed_slugs(result), "no-missing-days")
})
