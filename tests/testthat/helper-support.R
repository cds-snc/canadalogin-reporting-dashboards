# Model a Thursday render with data due through September 12.

support_today <- as.Date("2026-09-17")

support_weeks <- function(n = 8L, through = as.Date("2026-09-12")) {
  week_end <- seq(through - 7L * (n - 1L), through, by = "7 days")
  tibble::tibble(
    week = week_end - 6L,
    week_end = week_end,
    calls_accepted = 20,
    test_calls = 4,
    calls = 16,
    calls_answered = 15,
    calls_abandoned = 5,
    pct_answered_within_60s = 0.9,
    avg_delay_seconds = 20,
    avg_call_length_seconds = 420
  )
}

support_topics <- function(weeks, topic = "CanadaLogin Password Criteria",
                           env = NULL) {
  days <- seq(min(weeks$week), max(weeks$week_end), by = "day")
  rows <- tibble::tibble(
    call_no = seq_along(days),
    call_date = days,
    topic = topic,
    language = "English",
    clienttype = "Public"
  )
  if (is.null(env)) rows else dplyr::mutate(rows, category = env$topic_category(topic))
}

support_users <- function(weeks, unique_users = 1000) {
  tibble::tibble(
    date = seq(min(weeks$week), max(weeks$week_end), by = "day"),
    unique_users = unique_users
  )
}

support_psom <- function(snapshot = as.Date("2026-09-14")) {
  tibble::tibble(
    snapshot = snapshot,
    key = "PSOM-1",
    summary = "Onboarding request",
    issue_type = "Request",
    status = "In Progress",
    organizations = "Some Department",
    created = snapshot - 10L,
    status_changed = snapshot - 3L
  )
}

support_with_data <- function(weeks = support_weeks(),
                              topics = NULL,
                              users = support_users(weeks),
                              psom = support_psom()) {
  env <- load_dashboard("support")
  if (is.null(topics)) topics <- support_topics(weeks, env = env)
  stub(
    env,
    call_weeks = function(con) weeks,
    call_topics = function(con) topics,
    daily_active_users = function(con) users,
    psom_snapshots = function(con) psom
  )
}

run_support_preflight <- function(env, today = support_today) {
  run_quietly(env$run_preflight_safety_check(NULL, today = today))
}
