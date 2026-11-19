# A Thursday render reporting Nov 9 to 15, with GA through Tuesday Nov 17.
# Steady codes on a steady site, so a test changes one thing and sees it. The
# thresholds mirror the setup chunk's.

errors_today <- as.Date("2026-11-19")
errors_week <- list(through = as.Date("2026-11-17"),
                    start = as.Date("2026-11-09"), end = as.Date("2026-11-15"))

errors_thresholds <- list(
  ratio = 2,
  min_events = 50,
  history_weeks = 4L,
  trend_weeks = 12L,
  top_codes = 10L,
  feature_codes = 3L,
  sessions_floor = 0.2
)

# Every day the page reads: 16 weeks back from the report week, to GA's last day.
errors_days <- function() {
  seq(errors_week$start - 7L * 15L, errors_week$through, by = "day")
}

errors_sessions <- function(days = errors_days()) {
  tibble::tibble(day = days, sessions = 1000)
}

# `per_day` events every day, and `week_per_day` in the report week if given.
errors_code <- function(code, form, per_day, week_per_day = per_day,
                        days = errors_days()) {
  in_week <- days >= errors_week$start & days <= errors_week$end
  tibble::tibble(day = days, error_code = code, form_id = form,
                 events = ifelse(in_week, week_per_day, per_day))
}

errors_rows <- function(days = errors_days()) {
  dplyr::bind_rows(
    errors_code("26", "passwordrecovery", 100, days = days),
    errors_code("CSIAH3315E", "fido2_enrollment_form", 50, days = days),
    errors_code("CSIAQ0287E", "signin", 20, days = days)
  )
}

errors_with_data <- function(errors = errors_rows(), sessions = errors_sessions()) {
  env <- load_dashboard("errors")
  stub(
    env,
    error_rows = function(con, from) errors,
    session_days = function(con, from) sessions
  )
}

run_errors_preflight <- function(env, thresholds = errors_thresholds,
                                 today = errors_today) {
  run_quietly(env$run_preflight_safety_check(NULL, thresholds, today = today))
}

errors_features <- function(env, errors = errors_rows()) {
  env$feature_table(errors, errors_sessions(), errors_week, errors_thresholds)
}

errors_codes <- function(env, errors = errors_rows(),
                         thresholds = errors_thresholds) {
  env$code_table(errors, errors_sessions(), errors_week, thresholds)
}
