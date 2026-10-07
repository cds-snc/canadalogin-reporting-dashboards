# A Friday render with one bot day, Sep 17, that every method catches. The
# thresholds mirror the setup chunk's; a test needing others copies them.

traffic_today <- as.Date("2026-10-09")
traffic_bot_day <- as.Date("2026-09-17")

traffic_thresholds <- list(
  baseline_days = 28L,
  baseline_min_days = c(weekday = 10L, weekend = 4L),
  sms = list(volume = 3, entry_rate_below = 0.5),
  idle = list(volume = 2, idle_share_at_least = 0.5),
  errors = list(group_volume = 3, groups_at_least = 3L, group_floor = 20,
                share_at_least = 0.3)
)

traffic_known_days <- tibble::tibble(
  day = traffic_bot_day, sms = TRUE, idle = TRUE, errors = TRUE
)

traffic_days <- function(from = as.Date("2026-05-01"),
                         through = traffic_today - 1L) {
  seq(from, through, by = "day")
}

is_weekend <- function(day) as.integer(format(day, "%u")) >= 6L

# Codes sent and entered; real users enter 96%, the bot day 8%.
traffic_sms <- function(days = traffic_days(), bot_days = traffic_bot_day) {
  sent <- ifelse(is_weekend(days), 700, 2000)
  bot <- days %in% bot_days
  sent[bot] <- 40000
  tibble::tibble(
    day = days,
    sms_sent = sent,
    sms_success = ifelse(bot, 3000, round(sent * 0.96))
  )
}

# Each day, most accounts reach a service that day and a few never do. The
# bot day makes thousands that never do.
traffic_pairs <- function(days = traffic_days(), bot_days = traffic_bot_day) {
  rows <- purrr::map(days, \(day) {
    bot <- day %in% bot_days
    base <- if (is_weekend(day)) 250 else 600
    tibble::tibble(
      first_day = day,
      service_day = c(day, day + 3L, as.Date(NA)),
      accounts = if (bot) c(500, 0, 9500) else c(base * 0.8, base * 0.05, base * 0.15)
    )
  })
  dplyr::bind_rows(rows) |> dplyr::filter(accounts > 0)
}

# GA error codes; the four signature groups rise together on the bot day.
traffic_codes <- function(days = traffic_days(as.Date("2026-06-01"),
                                              traffic_today - 2L),
                          bot_days = traffic_bot_day) {
  rows <- purrr::map(days, \(day) {
    bot <- day %in% bot_days
    tibble::tibble(
      day = day,
      error_code = c("PHONE_VALIDATION_ERROR", "CSIBN0081E", "CSIAH0004E",
                     "CSIAH2417E", "OTHER_ERROR"),
      events = if (bot) c(1000, 500, 900, 400, 900) else c(25, 0, 50, 40, 900)
    )
  })
  dplyr::bind_rows(rows)
}

traffic_with_data <- function(sms = traffic_sms(), pairs = traffic_pairs(),
                              codes = traffic_codes()) {
  env <- load_dashboard("automated-traffic")
  stub(
    env,
    sms_days = function(con) sms,
    account_pairs = function(con) pairs,
    error_codes = function(con) codes
  )
}

run_traffic_preflight <- function(env, thresholds = traffic_thresholds,
                                  known = traffic_known_days,
                                  today = traffic_today) {
  run_quietly(env$run_preflight_safety_check(NULL, thresholds, known,
                                             today = today))
}
