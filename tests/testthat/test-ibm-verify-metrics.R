ibm <- load_dashboard("ibm-verify")

# Windows ---------------------------------------------------------------------

test_that("in_window keeps the days ending on as_of inclusive", {
  rows <- tibble::tibble(date = as.Date(c("2026-08-20", "2026-08-21",
                                          "2026-09-17", "2026-09-18")))
  kept <- ibm$in_window(rows, as.Date("2026-09-17"), days = 28)
  expect_identical(kept$date, as.Date(c("2026-08-21", "2026-09-17")))
})

test_that("latest_value reads a cumulative column on the newest day", {
  rows <- tibble::tibble(date = as.Date(c("2026-09-02", "2026-09-03", "2026-09-01")),
                         mtd_unique_users = c(20, 30, 10))
  expect_identical(ibm$latest_value(rows, "mtd_unique_users"), 30)
})

test_that("latest_value is NA with no rows", {
  rows <- tibble::tibble(date = as.Date(character()), mtd_unique_users = numeric())
  expect_true(is.na(ibm$latest_value(rows, "mtd_unique_users")))
})

test_that("average_daily_users adds applications within a day, then averages days", {
  rows <- ibm_apps(days = seq(as.Date("2026-08-21"), as.Date("2026-09-17"), by = "day"),
                   applications = c("app-a", "app-b"))
  expect_equal(ibm$average_daily_users(rows, as.Date("2026-09-17"), days = 28), 80)
})

# Calendar months -------------------------------------------------------------

monthly_fixture <- function() {
  days <- as.Date(c("2026-07-30", "2026-07-31", "2026-08-01", "2026-08-02"))
  auth <- tibble::tibble(date = days, successful_logins = 100, failed_logins = 10,
                         mtd_unique_users = c(40, 50, 5, 9))
  mfa <- tidyr::expand_grid(date = days,
                            mfa_type = c("sms_otp", "fido2", "email_otp"),
                            result = c("success", "failure")) |>
    dplyr::mutate(count = 1)
  list(auth = auth, mfa = mfa)
}

test_that("monthly totals add every authentication event in the month", {
  f <- monthly_fixture()
  months <- ibm$monthly_auth_totals(f$auth, f$mfa, as.Date("2026-08-02"))
  expect_identical(months$authentication_events, c(220, 220))
})

test_that("monthly users are the month-to-date count on the month's last day", {
  f <- monthly_fixture()
  months <- ibm$monthly_auth_totals(f$auth, f$mfa, as.Date("2026-08-02"))
  expect_identical(months$users, c(9, 50))
})

test_that("estimated sign-ins subtract successful sign-in factors only", {
  f <- monthly_fixture()
  months <- ibm$monthly_auth_totals(f$auth, f$mfa, as.Date("2026-08-02"))
  # 200 successes minus four successful sign-in factors.
  expect_identical(months$estimated_sign_ins, c(196, 196))
})

test_that("monthly totals put the newest month first", {
  f <- monthly_fixture()
  months <- ibm$monthly_auth_totals(f$auth, f$mfa, as.Date("2026-08-02"))
  expect_identical(months$month, as.Date(c("2026-08-01", "2026-07-01")))
})

test_that("monthly totals stop at as_of", {
  f <- monthly_fixture()
  months <- ibm$monthly_auth_totals(f$auth, f$mfa, as.Date("2026-08-01"))
  expect_identical(months$authentication_events[1], 110)
})

test_that("email OTP is not a sign-in factor", {
  expect_false("email_otp" %in% ibm$sign_in_mfa_types)
})

# Services --------------------------------------------------------------------

unit_rows <- tibble::tibble(
  date = as.Date(c("2026-07-10", "2026-07-20", "2026-09-05", "2026-08-15")),
  unit = c("a", "a", "a", "b"),
  total_logins = c(10, 5, 7, 3),
  mtd_unique_users = c(5, 8, 3, 4)
)

test_that("latest_per_unit takes each unit's newest row in the span", {
  latest <- ibm$latest_per_unit(unit_rows, as.Date("2026-07-01"), as.Date("2026-08-31"))
  expect_identical(latest$date, as.Date(c("2026-07-20", "2026-08-15")))
})

test_that("services_by_volume orders busiest first and drops unlabelled rows", {
  rows <- tibble::tibble(service_name = c("Quiet", "Busy", NA, "Busy"),
                         total_logins = c(5, 10, 100, 10))
  expect_identical(ibm$services_by_volume(rows), c("Busy", "Quiet"))
})

test_that("service_units orders busiest first", {
  expect_identical(ibm$service_units(unit_rows), c("a", "b"))
})

test_that("service_launch is the first day with rows", {
  expect_identical(ibm$service_launch(unit_rows), as.Date("2026-07-10"))
})

test_that("service_launch is NA with no rows", {
  expect_true(is.na(ibm$service_launch(unit_rows[0, ])))
})

test_that("external services leave out internal ones", {
  env <- ibm_with_data()
  expect_setequal(env$external_services(NULL), c("Service A", "Service B"))
  expect_setequal(env$all_services(NULL), c("Service A", "Service B", "Admin"))
})

test_that("service_daily keeps one service's applications as units", {
  env <- ibm_with_data()
  rows <- env$service_daily(NULL, "Service A")
  expect_identical(unique(rows$unit), "app-a")
})

test_that("monthly unit users read the month-to-date count on the last day", {
  months <- ibm$monthly_unit_users(unit_rows, as.Date("2026-09-10"))
  a <- dplyr::filter(months, unit == "a")
  expect_identical(a$users, c(8, 0, 3))
})

test_that("monthly unit users are NA before a unit existed and zero after", {
  months <- ibm$monthly_unit_users(unit_rows, as.Date("2026-09-10"))
  b <- dplyr::filter(months, unit == "b")
  expect_identical(b$users, c(NA, 4, 0))
})

test_that("monthly unit SSO events add up within a month", {
  months <- ibm$monthly_unit_sso_events(unit_rows, as.Date("2026-09-10"))
  a <- dplyr::filter(months, unit == "a")
  expect_identical(a$sso_events, c(15, 0, 7))
})

test_that("monthly unit SSO events are NA before a unit existed and zero after", {
  months <- ibm$monthly_unit_sso_events(unit_rows, as.Date("2026-09-10"))
  b <- dplyr::filter(months, unit == "b")
  expect_identical(b$sso_events, c(NA, 3, 0))
})

# MFA -------------------------------------------------------------------------

test_that("a known factor gets its display name", {
  expect_identical(ibm$mfa_label(c("fido2", "sms_otp")),
                   c("Passkey (FIDO2)", "SMS OTP"))
})

test_that("an unknown factor keeps its raw name", {
  expect_identical(ibm$mfa_label("new_factor"), "new_factor")
})

test_that("mfa_successes keeps successes of the factors asked for", {
  env <- ibm_with_data(mfa = ibm_mfa(days = as.Date("2026-09-16")))
  successes <- env$mfa_successes(NULL, types = "fido2")
  expect_identical(successes$mfa_type, "fido2")
  expect_identical(successes$factor_label, "Passkey (FIDO2)")
})

test_that("mfa_successes keeps every factor when none are asked for", {
  env <- ibm_with_data(mfa = ibm_mfa(days = as.Date("2026-09-16")))
  expect_identical(nrow(env$mfa_successes(NULL)), 3L)
})

# Change ----------------------------------------------------------------------

test_that("a change within half a percent is flat", {
  expect_identical(ibm$delta_direction(c(0.005, -0.005, 0)), rep("flat", 3))
})

test_that("a change beyond half a percent has a direction", {
  expect_identical(ibm$delta_direction(c(0.006, -0.006)), c("up", "down"))
})

test_that("fmt_delta shows direction with an arrow and a sign", {
  expect_identical(ibm$fmt_delta(c(0.25, -0.3)), c("▲ +25%", "▼ -30%"))
})

test_that("fmt_delta tells no change apart from an uncalculable one", {
  expect_identical(ibm$fmt_delta(c(0.001, NA)), c("no change", "-"))
})

test_that("relative_change is undefined against a zero or missing base", {
  expect_equal(ibm$relative_change(15, 10), 0.5)
  expect_true(is.na(ibm$relative_change(5, 0)))
  expect_true(is.na(ibm$relative_change(5, NA)))
})
