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
  # Every Monday, Wednesday and Friday for four weeks at 3.5x: if each joined
  # the baseline, they would soon be most of it, the median would climb, and
  # the later ones would stop firing. The lower minimum keeps the remaining
  # weekdays enough to score against.
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

# Calendar --------------------------------------------------------------------

test_that("the calendar starts on a Monday, 13 weeks back", {
  start <- traffic$calendar_start(as.Date("2026-10-08"))
  expect_identical(format(start, "%u"), "1")
  expect_identical(start, as.Date("2026-07-13"))
})

test_that("the calendar counts methods, marks partial days and spikes", {
  scores <- tibble::tribble(
    ~method,  ~day,                  ~multiple, ~ratio, ~fired,
    "sms",    as.Date("2026-09-17"), 20,        0.1,    TRUE,
    "idle",   as.Date("2026-09-17"), 4,         0.8,    TRUE,
    "errors", as.Date("2026-09-17"), 5,         0.5,    TRUE,
    "sms",    as.Date("2026-09-18"), 1,         0.95,   FALSE,
    "idle",   as.Date("2026-09-18"), 50,        0.1,    FALSE,
    "errors", as.Date("2026-09-18"), NA,        NA,     NA
  )
  cal <- traffic$calendar_days(scores, as.Date("2026-09-17"),
                               as.Date("2026-09-18"), traffic_thresholds)
  expect_equal(cal$n_fired, c(3, 0))
  expect_identical(cal$partial, c(FALSE, TRUE))
  # A spike that did not fire gets no dot.
  expect_identical(cal$spike, c(TRUE, FALSE))
})

test_that("the CSV link carries the whole table", {
  df <- tibble::tibble(date = as.Date("2026-05-01"), all_accounts = 3)
  link <- as.character(traffic$csv_download_link(df, "x.csv", "Download"))
  encoded <- sub('.*base64,([^"]+)".*', "\\1", link)
  csv <- rawToChar(jsonlite::base64_dec(encoded))
  expect_identical(csv, "\"date\",\"all_accounts\"\n2026-05-01,3\n")
})
