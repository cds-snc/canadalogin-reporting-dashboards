support <- load_dashboard("support")

# Windows ---------------------------------------------------------------------

test_that("recent_weeks takes the last weeks ending on or before as_of", {
  weeks <- support_weeks(8)
  recent <- support$recent_weeks(weeks, as.Date("2026-09-12"), weeks = 4)
  expect_identical(recent$week_end, as.Date(c("2026-08-22", "2026-08-29",
                                              "2026-09-05", "2026-09-12")))
})

test_that("recent_weeks leaves out a week that ends after as_of", {
  weeks <- support_weeks(8)
  recent <- support$recent_weeks(weeks, as.Date("2026-09-11"), weeks = 4)
  expect_identical(max(recent$week_end), as.Date("2026-09-05"))
})

test_that("recent_weeks returns what there is when history is short", {
  weeks <- support_weeks(2)
  expect_identical(nrow(support$recent_weeks(weeks, as.Date("2026-09-12"))), 2L)
})

test_that("prior_weeks is the window immediately before the recent one", {
  weeks <- support_weeks(8)
  prior <- support$prior_weeks(weeks, as.Date("2026-09-12"), weeks = 4)
  expect_identical(prior$week_end, as.Date(c("2026-07-25", "2026-08-01",
                                             "2026-08-08", "2026-08-15")))
})

test_that("prior_weeks is empty when there is no recent window", {
  weeks <- support_weeks(8)
  expect_identical(nrow(support$prior_weeks(weeks, as.Date("2020-01-01"))), 0L)
})

test_that("created_in_window includes as_of and excludes the day the window opens", {
  rows <- tibble::tibble(created = as.Date(c("2026-08-20", "2026-08-21",
                                             "2026-09-17", "2026-09-18")))
  kept <- support$created_in_window(rows, as.Date("2026-09-17"), days = 28)
  expect_identical(kept$created, as.Date(c("2026-08-21", "2026-09-17")))
})

test_that("weighted_weekly weights each week by its answered calls", {
  rows <- tibble::tibble(calls_answered = c(10, 30), avg_delay_seconds = c(20, 40))
  expect_equal(support$weighted_weekly(rows, "avg_delay_seconds"), 35)
})

test_that("weighted_weekly is NA when no calls were answered", {
  rows <- tibble::tibble(calls_answered = c(0, 0), avg_delay_seconds = c(20, 40))
  expect_true(is.na(support$weighted_weekly(rows, "avg_delay_seconds")))
})

# Call rate -------------------------------------------------------------------

test_that("call_rate divides a week's calls by the week's user-days", {
  weeks <- support_weeks(1)
  rate <- support$call_rate(weeks, support_users(weeks, unique_users = 1000))
  expect_equal(rate$user_days, 7000)
  expect_equal(rate$calls_per_100, 16 / 7000 * 100)
})

test_that("call_rate gives no rate for a week the sign-in data only part covers", {
  weeks <- support_weeks(1)
  users <- support_users(weeks)[-3, ]
  expect_true(is.na(support$call_rate(weeks, users)$calls_per_100))
})

# Topics ----------------------------------------------------------------------

test_that("a topic in the lookup table takes the table's category", {
  expect_identical(
    as.character(support$topic_category("CanadaLogin Password Criteria")),
    "What is CanadaLogin"
  )
})

test_that("a topic's extra whitespace does not stop it matching the table", {
  expect_identical(
    as.character(support$topic_category("  CanadaLogin   Password Criteria ")),
    "What is CanadaLogin"
  )
})

test_that("a topic missing from the table falls back to the patterns", {
  expect_identical(as.character(support$topic_category("How to Do Something New")),
                   "Account setup and how-to")
})

test_that("the first matching pattern wins", {
  # Matches both the how-to and the 2-step patterns; how-to comes first.
  expect_identical(
    support$topic_category_fallback("Changing a 2-Step Verification Phone Number"),
    "Account setup and how-to"
  )
})

test_that("a topic nothing matches is Other", {
  expect_identical(as.character(support$topic_category("Zzz unheard of")), "Other")
})

test_that("topic categories are a factor with Other as the last level", {
  categories <- support$topic_category(c("Zzz unheard of", "How to Do Something"))
  expect_s3_class(categories, "factor")
  expect_identical(utils::tail(levels(categories), 1), "Other")
})

test_that("every category in the lookup table is a factor level", {
  expect_true(all(support$topic_lookup %in% support$topic_category_levels))
})

test_that("topics_in_weeks keeps calls placed inside the weeks", {
  weeks <- support_weeks(2)
  topics <- support_topics(support_weeks(4))
  kept <- support$topics_in_weeks(topics, weeks)
  expect_identical(min(kept$call_date), min(weeks$week))
  expect_identical(max(kept$call_date), max(weeks$week_end))
})

test_that("topics_in_weeks is empty when there are no weeks", {
  topics <- support_topics(support_weeks(1))
  expect_identical(nrow(support$topics_in_weeks(topics, support_weeks(1)[0, ])), 0L)
})

# PSOM ------------------------------------------------------------------------

test_that("week_of returns the Monday of the week", {
  expect_identical(support$week_of(as.Date("2026-09-17")), as.Date("2026-09-14"))
  expect_identical(support$week_of(as.Date("2026-09-14")), as.Date("2026-09-14"))
  expect_identical(support$week_of(as.Date("2026-09-20")), as.Date("2026-09-14"))
})

test_that("psom_open drops every closed status", {
  rows <- tibble::tibble(status = c("Done", "Canceled", "Ready to archive",
                                    "In Progress", "Waiting"))
  expect_identical(support$psom_open(rows)$status, c("In Progress", "Waiting"))
})

test_that("a ticket's close date is the first day it reached a closed status", {
  rows <- tibble::tibble(
    key = "PSOM-1",
    status = c("In Progress", "Done", "Ready to archive"),
    status_changed = as.Date(c("2026-09-01", "2026-09-05", "2026-09-12"))
  )
  expect_identical(support$psom_closed_on(rows)$closed_on, as.Date("2026-09-05"))
})

test_that("the board is the newest snapshot with close dates from every snapshot", {
  rows <- tibble::tibble(
    snapshot = as.Date(c("2026-09-07", "2026-09-14")),
    key = "PSOM-1",
    status = c("Done", "Ready to archive"),
    status_changed = as.Date(c("2026-09-05", "2026-09-12"))
  )
  board <- support$psom_board(rows)
  expect_identical(nrow(board), 1L)
  expect_identical(board$snapshot, as.Date("2026-09-14"))
  expect_identical(board$closed_on, as.Date("2026-09-05"))
})

test_that("an open ticket has no close date on the board", {
  rows <- tibble::tibble(snapshot = as.Date("2026-09-14"), key = c("open", "done"),
                         status = c("In Progress", "Done"),
                         status_changed = as.Date("2026-09-10"))
  board <- support$psom_board(rows)
  expect_true(is.na(board$closed_on[board$key == "open"]))
})

test_that("closed_in_window counts closes inside the window only", {
  board <- tibble::tibble(closed_on = as.Date(c(NA, "2026-08-20", "2026-08-21",
                                                "2026-09-17")))
  kept <- support$closed_in_window(board, as.Date("2026-09-17"), days = 28)
  expect_identical(kept$closed_on, as.Date(c("2026-08-21", "2026-09-17")))
})

test_that("psom_ages keeps open tickets and tickets closed in the window", {
  board <- tibble::tibble(
    key = c("open", "closed-recently", "closed-long-ago"),
    created = as.Date(c("2026-08-01", "2026-09-01", "2026-06-01")),
    closed_on = as.Date(c(NA, "2026-09-05", "2026-06-10"))
  )
  ages <- support$psom_ages(board, as.Date("2026-09-17"), days = 28)
  expect_identical(ages$key, c("open", "closed-recently"))
})

test_that("psom_ages measures an open ticket to as_of and a closed one to its close", {
  board <- tibble::tibble(
    created = as.Date(c("2026-09-01", "2026-09-01")),
    closed_on = as.Date(c(NA, "2026-09-05"))
  )
  ages <- support$psom_ages(board, as.Date("2026-09-17"))
  expect_identical(ages$age, c(16L, 4L))
})

psom_life <- tibble::tibble(
  key = c("A", "B", "C"),
  created = as.Date(c("2026-09-01", "2026-09-02", "2026-09-10")),
  closed_on = as.Date(c("2026-09-08", NA, "2026-09-11"))
)

test_that("psom_weekly leaves out the week still in progress", {
  weekly <- support$psom_weekly(psom_life, as.Date("2026-08-31"), as.Date("2026-09-16"))
  expect_identical(weekly$week_end, as.Date(c("2026-09-06", "2026-09-13")))
})

test_that("psom_weekly counts tickets opened and closed in each week", {
  weekly <- support$psom_weekly(psom_life, as.Date("2026-08-31"), as.Date("2026-09-16"))
  expect_identical(weekly$opened, c(2L, 1L))
  expect_identical(weekly$closed, c(0L, 2L))
})

test_that("psom_weekly's open count is the running total of opened less closed", {
  weekly <- support$psom_weekly(psom_life, as.Date("2026-08-31"), as.Date("2026-09-16"))
  expect_identical(weekly$open, c(2L, 1L))
  expect_identical(weekly$open, cumsum(weekly$opened - weekly$closed))
})

# Change ----------------------------------------------------------------------

test_that("a change within half a percent is flat", {
  expect_identical(support$delta_direction(c(0.005, -0.005, 0)), rep("flat", 3))
})

test_that("an uncalculable change is flat", {
  expect_identical(support$delta_direction(NA_real_), "flat")
})

test_that("a change beyond half a percent has a direction", {
  expect_identical(support$delta_direction(c(0.006, -0.006)), c("up", "down"))
})

test_that("fmt_delta shows a rise with an up arrow and a sign", {
  expect_identical(support$fmt_delta(0.25), "▲ +25%")
})

test_that("fmt_delta shows a fall with a down arrow and a sign", {
  expect_identical(support$fmt_delta(-0.3), "▼ -30%")
})

test_that("fmt_delta tells no change apart from an uncalculable one", {
  expect_identical(support$fmt_delta(0.001), "no change")
  expect_identical(support$fmt_delta(NA_real_), "-")
})

test_that("every direction has a colour", {
  expect_setequal(names(support$delta_colours), c("up", "down", "flat"))
})

test_that("relative_change is the change over the base", {
  expect_equal(support$relative_change(15, 10), 0.5)
})

test_that("relative_change is undefined against a zero or missing base", {
  expect_true(is.na(support$relative_change(5, 0)))
  expect_true(is.na(support$relative_change(5, NA)))
})

# Formatting ------------------------------------------------------------------

test_that("fmt_count spells out none", {
  expect_identical(support$fmt_count(0, "call"), "No calls")
})

test_that("fmt_count spells out one to ten, singular for one", {
  expect_identical(support$fmt_count(1, "call"), "One call")
  expect_identical(support$fmt_count(10, "call"), "Ten calls")
})

test_that("fmt_count uses digits above ten", {
  expect_identical(support$fmt_count(23, "call"), "23 calls")
  expect_identical(support$fmt_count(1234), "1,234")
})

test_that("fmt_duration keeps short durations in seconds", {
  expect_identical(support$fmt_duration(20), "20&nbsp;seconds")
  expect_identical(support$fmt_duration(1), "1&nbsp;second")
})

test_that("fmt_duration switches to minutes at ninety seconds", {
  expect_identical(support$fmt_duration(90), "1.5&nbsp;minutes")
  expect_identical(support$fmt_duration(420), "7.0&nbsp;minutes")
})

test_that("fmt_duration is a dash when missing", {
  expect_identical(support$fmt_duration(NA_real_), "-")
})

test_that("fmt_days drops the decimal for a whole number of days", {
  expect_identical(support$fmt_days(4), "4&nbsp;days")
  expect_identical(support$fmt_days(1), "1&nbsp;day")
})

test_that("fmt_days keeps one decimal otherwise", {
  expect_identical(support$fmt_days(3.5), "3.5&nbsp;days")
})

test_that("fmt_days is a dash when missing", {
  expect_identical(support$fmt_days(NA_real_), "-")
})

test_that("fmt_minutes formats seconds as m:ss", {
  expect_identical(support$fmt_minutes(c(105, 20, NA)), c("1:45", "0:20", "-"))
})

test_that("fmt_minutes carries a rounded-up minute", {
  expect_identical(support$fmt_minutes(c(59.6, 119.7)), c("1:00", "2:00"))
})
