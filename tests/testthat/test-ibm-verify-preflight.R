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
    c("verify-freshness", "service-coverage", "rp-resolution", "plausibility")
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
  expect_match(details, "^mfa_activity: no rows for 1 day")
})

test_that("freshness fails when every table stops before yesterday", {
  days <- ibm_days[ibm_days <= as.Date("2026-09-15")]
  env <- ibm_with_data(auth = ibm_auth(days), apps = ibm_apps(days),
                       mfa = ibm_mfa(days))
  result <- run_ibm_preflight(env)
  expect_identical(failed_slugs(result), "verify-freshness")
})

test_that("freshness ignores a gap older than the lookback", {
  auth <- dplyr::filter(ibm_auth(), date != as.Date("2026-08-01"))
  result <- run_ibm_preflight(ibm_with_data(auth = auth))
  expect_true(result$passed)
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
