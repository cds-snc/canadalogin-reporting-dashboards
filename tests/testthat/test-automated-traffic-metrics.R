traffic <- load_dashboard("automated-traffic")

fired_on <- function(rows) rows$day[rows$fired %in% TRUE]

# Scoring ---------------------------------------------------------------------

test_that("SMS fires on a surge of codes nobody enters", {
  scored <- traffic$score_sms(traffic_sms(), traffic_thresholds)
  expect_identical(fired_on(scored), traffic_bot_day)
})

test_that("SMS does not fire on a launch, where real users enter their codes", {
  sms <- traffic_sms(bot_days = as.Date(NA)) |>
    dplyr::mutate(
      sms_sent = ifelse(day == traffic_bot_day, 60000, sms_sent),
      sms_success = ifelse(day == traffic_bot_day, 57000, sms_success)
    )
  expect_length(fired_on(traffic$score_sms(sms, traffic_thresholds)), 0)
})

test_that("a low entry rate alone does not fire", {
  sms <- traffic_sms(bot_days = as.Date(NA)) |>
    dplyr::mutate(sms_success = ifelse(day == traffic_bot_day, 200, sms_success))
  expect_length(fired_on(traffic$score_sms(sms, traffic_thresholds)), 0)
})

test_that("weekends are compared only with weekends", {
  scored <- traffic$score_sms(traffic_sms(), traffic_thresholds)
  saturday <- scored[scored$day == as.Date("2026-09-19"), ]
  expect_equal(saturday$baseline, 700)
})

test_that("a day that fired is left out of later baselines", {
  # Three days a week at 3.5x for four weeks: if fired days joined the
  # baseline, its median would climb and the later ones would stop firing.
  span <- seq(as.Date("2026-08-17"), as.Date("2026-09-11"), by = "day")
  bot_days <- span[format(span, "%u") %in% c("1", "3", "5")]
  sms <- traffic_sms(bot_days = as.Date(NA)) |>
    dplyr::mutate(
      sms_sent = ifelse(day %in% bot_days, 7000, sms_sent),
      sms_success = ifelse(day %in% bot_days, 500, sms_success)
    )
  thresholds <- traffic_thresholds
  thresholds$baseline_min_days[["weekday"]] <- 5L
  expect_identical(fired_on(traffic$score_sms(sms, thresholds)), bot_days)
})

test_that("a day without enough baseline is not scored", {
  scored <- traffic$score_sms(traffic_sms(), traffic_thresholds)
  expect_true(is.na(scored$fired[1]))
})

test_that("SMS location fires on a surge of codes from outside Canada and the US", {
  scored <- traffic$score_location(traffic_origins(), traffic_thresholds)
  expect_identical(fired_on(scored), traffic_bot_day)
})

test_that("SMS location does not fire on a launch at home", {
  origins <- traffic_origins(bot_days = as.Date(NA)) |>
    dplyr::mutate(
      sms_sent = ifelse(day == traffic_bot_day, 60000, sms_sent),
      sms_outside = ifelse(day == traffic_bot_day, 600, sms_outside)
    )
  expect_length(fired_on(traffic$score_location(origins, traffic_thresholds)), 0)
})

test_that("extra codes from outside count those above a typical day", {
  scored <- traffic$score_location(traffic_origins(), traffic_thresholds)
  # The bot day had 34,000 codes from outside against 40 on a typical weekday.
  expect_equal(traffic$extra_on_flagged(scored, as.Date("2026-09-01"),
                                        "sms_outside", baseline = "typical"),
               34000 - 40)
})

test_that("a high share from outside alone does not fire", {
  origins <- traffic_origins(bot_days = as.Date(NA)) |>
    dplyr::mutate(sms_outside = ifelse(day == traffic_bot_day, 1500, sms_outside))
  expect_length(fired_on(traffic$score_location(origins, traffic_thresholds)), 0)
})

test_that("idle accounts counts an account idle unless it reached a service that day", {
  accounts <- traffic$account_days(traffic_pairs(), traffic_today - 1L)
  monday <- accounts[accounts$day == as.Date("2026-09-14"), ]
  expect_equal(monday$new_accounts, 600)
  expect_equal(monday$idle_same_day, 600 * 0.2)
})

test_that("idle accounts fires on a bulk wave of idle accounts", {
  accounts <- traffic$account_days(traffic_pairs(), traffic_today - 1L)
  expect_identical(fired_on(traffic$score_idle(accounts, traffic_thresholds)),
                   traffic_bot_day)
})

test_that("a weighted median counts each value its weight", {
  expect_equal(traffic$weighted_median(c(20, 1), c(1, 3)), 1)
  expect_equal(traffic$weighted_median(c(30, 12, 20), c(1, 1, 1)), 20)
})

test_that("sign-up speed fires on a wave of codes entered by machine", {
  scored <- traffic$score_speed(traffic$speed_days(traffic_email_seconds()),
                                traffic_thresholds)
  expect_identical(fired_on(scored), traffic_bot_day)
  expect_equal(scored$ratio[scored$day == traffic_bot_day], 1)
})

test_that("sign-up speed does not fire on a launch where people type their codes", {
  seconds <- traffic_email_seconds() |>
    dplyr::mutate(seconds = dplyr::if_else(seconds == 1, 20, seconds))
  speed <- traffic$speed_days(seconds)
  expect_length(fired_on(traffic$score_speed(speed, traffic_thresholds)), 0)
})

test_that("fast codes at an ordinary volume do not fire", {
  seconds <- traffic_email_seconds() |>
    dplyr::filter(day != traffic_bot_day | seconds == 1) |>
    dplyr::mutate(codes = dplyr::if_else(day == traffic_bot_day, 700, codes))
  speed <- traffic$speed_days(seconds)
  expect_equal(speed$median_seconds[speed$day == traffic_bot_day], 1)
  expect_length(fired_on(traffic$score_speed(speed, traffic_thresholds)), 0)
})

test_that("a day without email codes is left out, not scored as a quiet day", {
  seconds <- dplyr::filter(traffic_email_seconds(), day != as.Date("2026-09-02"))
  expect_false(as.Date("2026-09-02") %in% traffic$speed_days(seconds)$day)
})

test_that("the typical time to enter a code leaves out flagged days", {
  seconds <- traffic_email_seconds()
  week <- traffic$window_median_seconds(seconds, as.Date("2026-09-14"),
                                        as.Date("2026-09-20"))
  typical <- traffic$window_median_seconds(seconds, as.Date("2026-09-14"),
                                           as.Date("2026-09-20"),
                                           skip = traffic_bot_day)
  expect_equal(week, 1)
  expect_equal(typical, 20)
})

test_that("error patterns fires when the signature errors rise together", {
  errors <- traffic$error_days(traffic_codes())
  expect_identical(fired_on(traffic$score_errors(errors, traffic_thresholds)),
                   traffic_bot_day)
})

test_that("one signature error spiking alone does not fire", {
  codes <- traffic_codes(bot_days = as.Date(NA)) |>
    dplyr::mutate(events = ifelse(day == traffic_bot_day &
                                    error_code == "PHONE_VALIDATION_ERROR",
                                  5000, events))
  errors <- traffic$error_days(codes)
  expect_length(fired_on(traffic$score_errors(errors, traffic_thresholds)), 0)
})

test_that("the floor stops a small error count jumping", {
  # SMS limit is 0 on a typical day; 15 is past 3x of nothing, not of 20.
  codes <- traffic_codes(bot_days = as.Date(NA)) |>
    dplyr::mutate(events = dplyr::case_when(
      day == traffic_bot_day & error_code == "CSIBN0081E" ~ 15,
      day == traffic_bot_day & error_code == "CSIAH0004E" ~ 400,
      day == traffic_bot_day & error_code == "CSIAH2417E" ~ 400,
      .default = events
    ))
  scored <- traffic$score_errors(traffic$error_days(codes), traffic_thresholds)
  expect_equal(scored$groups_up[scored$day == traffic_bot_day], 2)
})

test_that("an error group with no rows at all still gets a column", {
  codes <- dplyr::filter(traffic_codes(), error_code != "CSIBN0081E")
  expect_true(all(traffic$error_days(codes)$sms_limit == 0))
})

# Accounts since launch -------------------------------------------------------

test_that("accounts since launch is point in time and adds up", {
  pairs <- tibble::tibble(
    first_day = as.Date(c("2026-05-01", "2026-05-01", "2026-05-02")),
    service_day = as.Date(c("2026-05-01", "2026-05-03", NA)),
    accounts = c(10, 5, 2)
  )
  launch <- traffic$accounts_since_launch(pairs, as.Date("2026-05-03"))
  expect_equal(launch$all_accounts, c(15, 17, 17))
  # The five who reach a service on May 3 are idle until then.
  expect_equal(launch$idle_accounts, c(5, 7, 2))
  expect_equal(launch$signed_in_to_service, c(10, 10, 15))
  expect_equal(launch$all_accounts,
               launch$idle_accounts + launch$signed_in_to_service)
  expect_identical(
    names(launch),
    c("date", "all_accounts", "idle_accounts", "signed_in_to_service",
      "idle_share", "new_accounts")
  )
})

test_that("an account is never signed in before it exists", {
  pairs <- tibble::tibble(first_day = as.Date("2026-05-02"),
                          service_day = as.Date("2026-05-01"), accounts = 4)
  launch <- traffic$accounts_since_launch(pairs, as.Date("2026-05-02"))
  expect_equal(launch$signed_in_to_service, 4)
  expect_equal(launch$idle_accounts, 0)
})

test_that("weeks run Monday to Sunday and the newest may be in progress", {
  pairs <- tibble::tibble(first_day = as.Date("2026-09-28"),
                          service_day = as.Date(NA), accounts = 2)
  launch <- traffic$accounts_since_launch(pairs, as.Date("2026-10-06"))
  weeks <- traffic$accounts_by_week(launch)
  expect_identical(weeks$week_end, as.Date(c("2026-10-04", "2026-10-11")))
  expect_identical(weeks$date, as.Date(c("2026-10-04", "2026-10-06")))
  expect_equal(weeks$idle_accounts, c(2, 2))
  # Running totals as of the week's last day; new accounts summed over it.
  pairs <- tibble::tibble(first_day = as.Date(c("2026-09-28", "2026-09-30")),
                          service_day = as.Date(NA), accounts = c(2, 3))
  weeks <- traffic$accounts_by_week(
    traffic$accounts_since_launch(pairs, as.Date("2026-10-04"))
  )
  expect_equal(weeks$all_accounts, 5)
  expect_equal(weeks$new_accounts, 5)
})

# Calendar --------------------------------------------------------------------

test_that("the calendar starts on a Monday, six months back", {
  start <- traffic$calendar_start(as.Date("2026-10-08"))
  expect_identical(format(start, "%u"), "1")
  expect_identical(start, as.Date("2026-04-13"))
})

test_that("the method charts start on a Monday, 13 weeks back", {
  start <- traffic$calendar_start(as.Date("2026-10-08"), traffic$chart_weeks)
  expect_identical(start, as.Date("2026-07-13"))
})

test_that("the calendar counts methods, marks waiting days, takes the strongest", {
  scores <- tibble::tribble(
    ~method,    ~day,                  ~multiple, ~ratio, ~fired,
    "sms",      as.Date("2026-09-17"), 20,        0.1,    TRUE,
    "location", as.Date("2026-09-17"), 20,        0.9,    TRUE,
    "idle",     as.Date("2026-09-17"), 4,         0.8,    TRUE,
    "speed",    as.Date("2026-09-17"), 4,         2,      TRUE,
    "errors",   as.Date("2026-09-17"), 5,         0.5,    TRUE,
    "sms",      as.Date("2026-09-18"), 1,         0.95,   FALSE,
    "location", as.Date("2026-09-18"), 1,         0.02,   FALSE,
    "idle",     as.Date("2026-09-18"), 50,        0.1,    FALSE,
    "speed",    as.Date("2026-09-18"), 1,         20,     FALSE,
    "errors",   as.Date("2026-09-18"), NA,        NA,     NA
  )
  cal <- traffic$calendar_days(scores, as.Date("2026-09-17"),
                               as.Date("2026-09-18"))
  expect_equal(cal$n_fired, c(5, 0))
  # Error patterns last reported on the 17th, so the 18th waits on it.
  expect_identical(cal$waiting, c(FALSE, TRUE))
  # Only methods that fired count: 50x idle accounts on the 18th did not.
  expect_equal(cal$strongest, c(20, NA))
  # Peak counts every method that scored, fired or not.
  expect_equal(cal$peak, c(20, 50))
})

test_that("a day before a method could score is not waiting", {
  scores <- tibble::tribble(
    ~method,    ~day,                  ~multiple, ~ratio, ~fired,
    "sms",      as.Date("2026-07-01"), 1,         0.95,   FALSE,
    "location", as.Date("2026-07-01"), 1,         0.02,   FALSE,
    "idle",     as.Date("2026-07-01"), 1,         0.2,    FALSE,
    "speed",    as.Date("2026-07-01"), 1,         20,     FALSE,
    "errors",   as.Date("2026-07-01"), NA,        NA,     NA,
    "sms",      as.Date("2026-07-02"), 1,         0.95,   FALSE,
    "location", as.Date("2026-07-02"), 1,         0.02,   FALSE,
    "idle",     as.Date("2026-07-02"), 1,         0.2,    FALSE,
    "speed",    as.Date("2026-07-02"), 1,         20,     FALSE,
    "errors",   as.Date("2026-07-02"), 1,         0.1,    FALSE
  )
  cal <- traffic$calendar_days(scores, as.Date("2026-07-01"),
                               as.Date("2026-07-02"))
  expect_identical(cal$waiting, c(FALSE, FALSE))
})

test_that("extra SMS counts sends above a typical day on fired days only", {
  scored <- traffic$score_sms(traffic_sms(), traffic_thresholds)
  # The bot day sent 40,000 against a typical weekday of 2,000.
  expect_equal(traffic$extra_on_flagged(scored, as.Date("2026-09-01"), "sms_sent"),
               38000)
  expect_equal(traffic$extra_on_flagged(scored, as.Date("2026-09-18"), "sms_sent"),
               0)
})

test_that("the SMS week compares with a typical week and a typical entry rate", {
  scored <- traffic$score_sms(traffic_sms(), traffic_thresholds)
  # Mon Sep 14 to Sun Sep 20 holds the bot day: 4 weekdays at 2,000, the bot
  # day at 40,000, and a weekend at 700 a day.
  week <- traffic$method_window(scored, as.Date("2026-09-20"), "sms_sent",
                                "sms_success", "sms_sent")
  expect_equal(week$volume, 4 * 2000 + 40000 + 2 * 700)
  expect_equal(week$multiple, week$volume / (5 * 2000 + 2 * 700))
  expect_equal(week$typical_ratio, 0.96, tolerance = 0.001)
})

test_that("an SMS is priced by the tier its month's volume falls in", {
  tiers <- tibble::tibble(up_to = c(50000, 250000, 1000000, Inf),
                          price = c(0.0456, 0.0453, 0.0428, 0.0402))
  expect_equal(traffic$sms_price(c(1, 50000, 50001, 250000, 1e6, 1e6 + 1), tiers),
               c(0.0456, 0.0456, 0.0453, 0.0453, 0.0428, 0.0402))
})

test_that("extra SMS on a flagged day are priced at that day's 30-day volume", {
  tiers <- tibble::tibble(up_to = c(50000, 250000, 1000000, Inf),
                          price = c(0.0456, 0.0453, 0.0428, 0.0402))
  scored <- traffic$score_sms(traffic_sms(), traffic_thresholds)
  # The 30 days to the bot day hold over 50,000 codes: the second tier.
  expect_equal(traffic$extra_sms_cost(scored, as.Date("2026-09-01"), tiers),
               38000 * 0.0453)
})
