# Baseline --------------------------------------------------------------------

test_that("clean data passes every check", {
  result <- run_ibm_preflight(ibm_with_data())
  expect_true(result$passed)
  expect_length(result$failed, 0)
})

test_that("every check keeps its slug", {
  result <- run_ibm_preflight(ibm_with_data())
  expect_identical(
    purrr::map_chr(result$checks, "slug"),
    c("verify-freshness", "daily-users-coverage", "service-coverage",
      "rp-resolution", "application-ids", "plausibility")
  )
})

# verify-freshness ------------------------------------------------------------

test_that("freshness fails when one table misses a day", {
  mfa <- dplyr::filter(ibm_mfa(), date != as.Date("2026-09-10"))
  result <- run_ibm_preflight(ibm_with_data(mfa = mfa))
  expect_identical(failed_slugs(result), "verify-freshness")
})

test_that("freshness names the table that missed a day", {
  mfa <- dplyr::filter(ibm_mfa(), date != as.Date("2026-09-10"))
  result <- run_ibm_preflight(ibm_with_data(mfa = mfa))
  details <- check_named(result, "verify-freshness")$details
  expect_length(details, 1)
  expect_match(details, "^events_mfa_activity: no rows for 1 day")
})

test_that("freshness fails when every table stops before yesterday", {
  days <- ibm_days[ibm_days <= as.Date("2026-09-15")]
  env <- ibm_with_data(auth = ibm_auth(days), apps = ibm_apps(days),
                       mfa = ibm_mfa(days))
  result <- run_ibm_preflight(env)
  expect_identical(failed_slugs(result), "verify-freshness")
})

test_that("freshness ignores a gap older than the lookback", {
  auth <- dplyr::filter(ibm_auth(), date != as.Date("2026-07-10"))
  result <- run_ibm_preflight(ibm_with_data(auth = auth))
  expect_true(result$passed)
})

# daily-users-coverage --------------------------------------------------------

test_that("daily users coverage fails on a gap in the current window", {
  auth <- dplyr::filter(ibm_auth(), date != as.Date("2026-08-30"))
  result <- run_ibm_preflight(ibm_with_data(auth = auth))
  expect_identical(failed_slugs(result), "daily-users-coverage")
})

test_that("daily users coverage fails on a gap in the prior window", {
  auth <- dplyr::filter(ibm_auth(), date != as.Date("2026-07-25"))
  result <- run_ibm_preflight(ibm_with_data(auth = auth))
  expect_identical(failed_slugs(result), "daily-users-coverage")
  expect_match(check_named(result, "daily-users-coverage")$details,
               "no rows for 1 day")
})

test_that("daily users coverage ignores a gap before both windows", {
  auth <- dplyr::filter(ibm_auth(), date != as.Date("2026-07-21"))
  result <- run_ibm_preflight(ibm_with_data(auth = auth))
  expect_true(result$passed)
})

test_that("daily users coverage starts no earlier than launch", {
  # Data through May 19: the two windows would reach back to Mar 25.
  days <- seq(as.Date("2026-04-22"), as.Date("2026-05-19"), by = "day")
  env <- ibm_with_data(auth = ibm_auth(days), apps = ibm_apps(days),
                       mfa = ibm_mfa(days))
  result <- run_ibm_preflight(env, today = as.Date("2026-05-20"))
  expect_true(check_named(result, "daily-users-coverage")$passed)
})

# service-coverage ------------------------------------------------------------

test_that("service coverage fails when a service with traffic goes quiet", {
  apps <- dplyr::filter(ibm_apps(), !(application_name == "app-b" &
                                        date > as.Date("2026-08-19")))
  result <- run_ibm_preflight(ibm_with_data(apps = apps))
  expect_true("service-coverage" %in% failed_slugs(result))
  expect_match(check_named(result, "service-coverage")$details, "^Service B:")
})

test_that("service coverage covers internal services too", {
  apps <- dplyr::filter(ibm_apps(), !(application_name == "cl-admin" &
                                        date > as.Date("2026-08-19")))
  result <- run_ibm_preflight(ibm_with_data(apps = apps))
  expect_true("service-coverage" %in% failed_slugs(result))
})

# rp-resolution ---------------------------------------------------------------

test_that("relying-party resolution fails on an unlabelled application", {
  apps <- ibm_apps(applications = c(ibm_lookup$application_name, "new-app"))
  result <- run_ibm_preflight(ibm_with_data(apps = apps))
  expect_identical(failed_slugs(result), "rp-resolution")
  expect_match(check_named(result, "rp-resolution")$details, "new-app", fixed = TRUE)
})

# application-ids -------------------------------------------------------------

test_that("application ids fail when one name spans two ids", {
  apps <- ibm_apps() |>
    dplyr::mutate(application_id = dplyr::if_else(
      application_name == "app-a" & date > as.Date("2026-09-01"), "id-new",
      application_id
    ))
  result <- run_ibm_preflight(ibm_with_data(apps = apps))
  expect_identical(failed_slugs(result), "application-ids")
  expect_match(check_named(result, "application-ids")$details, "app-a", fixed = TRUE)
})

# plausibility ----------------------------------------------------------------

test_that("plausibility fails when the success rate falls below the floor", {
  result <- run_ibm_preflight(ibm_with_data(auth = ibm_auth(failed = 1000)))
  expect_identical(failed_slugs(result), "plausibility")
})

test_that("plausibility fails when a busy service's traffic collapses", {
  apps <- ibm_apps() |>
    dplyr::mutate(total_logins = dplyr::if_else(
      application_name == "app-b" & date > as.Date("2026-08-19"), 2, total_logins
    ))
  result <- run_ibm_preflight(ibm_with_data(apps = apps))
  expect_identical(failed_slugs(result), "plausibility")
  expect_match(check_named(result, "plausibility")$details,
               "Service B: SSO events fell from 1,400 to 56", fixed = TRUE)
})

test_that("plausibility ignores a collapse in a service below the minimum volume", {
  apps <- ibm_apps() |>
    dplyr::mutate(total_logins = dplyr::case_when(
      application_name != "app-b" ~ total_logins,
      date > as.Date("2026-08-19") ~ 0.1,
      .default = 3
    ))
  result <- run_ibm_preflight(ibm_with_data(apps = apps))
  expect_true(result$passed)
})
