# Direct dbplyr queries keep the preflight outside this stubbed test suite.

expmon <- load_dashboard("experience-monitoring")

# Stub step counts and record each call.
expmon_with_counts <- function(counts) {
  env <- load_dashboard("experience-monitoring")
  env$calls <- list()
  stub(env, funnel_step_counts = function(con, funnel_id, window_days, from, to) {
    env$calls[[length(env$calls) + 1L]] <- list(
      funnel_id = funnel_id, window_days = window_days, from = from, to = to
    )
    counts
  })
}

counts_fixture <- tibble::tribble(
  ~window_end,              ~funnel_version, ~breakdown_value, ~numerator, ~denominator,
  as.Date("2026-09-01"),    "v1",            "RESERVED_TOTAL", 50,         100,
  as.Date("2026-09-01"),    "v1",            "rp-a",           30,         60,
  as.Date("2026-09-01"),    "v1",            "rp-b",           20,         40,
  as.Date("2026-09-02"),    "v1",            "RESERVED_TOTAL", 90,         120,
  as.Date("2026-09-02"),    "v1",            "rp-a",           90,         120
)

# Configuration ---------------------------------------------------------------

test_that("every required funnel declares its ratio steps", {
  expect_true(all(expmon$required_funnels %in% names(expmon$funnel_ratio_steps)))
})

test_that("every required funnel has a label and a note", {
  for (funnel in expmon$required_funnels) {
    expect_type(expmon$funnel_label(funnel), "character")
    expect_type(expmon$funnel_note(funnel), "character")
  }
})

test_that("every funnel's ratio names a numerator and a denominator", {
  for (steps in expmon$funnel_ratio_steps) {
    expect_setequal(names(steps), c("numerator", "denominator"))
  }
})

test_that("no funnel's numerator is its own denominator", {
  for (steps in expmon$funnel_ratio_steps) {
    expect_false(steps[["numerator"]] == steps[["denominator"]])
  }
})

test_that("an undeclared funnel stops with the table to add it to", {
  expect_error(expmon$funnel_label("not_a_funnel"),
               "'not_a_funnel' has no entry in funnel_presentation")
})

# Rates -----------------------------------------------------------------------

test_that("task success sums the counts before dividing", {
  counts <- tibble::tibble(group = "all", numerator = c(1, 9), denominator = c(10, 10))
  rate <- expmon$as_task_success(counts, group)
  expect_identical(rate$numerator, 10)
  expect_identical(rate$denominator, 20)
  expect_equal(rate$value, 0.5)
})

test_that("task success is NA over a zero denominator", {
  counts <- tibble::tibble(numerator = 0, denominator = 0)
  expect_true(is.na(expmon$as_task_success(counts)$value))
})

test_that("task success ignores a missing count rather than propagating it", {
  counts <- tibble::tibble(numerator = c(5, NA), denominator = c(10, 10))
  expect_equal(expmon$as_task_success(counts)$value, 0.25)
})

test_that("total step counts are GA's overall rows only", {
  env <- expmon_with_counts(counts_fixture)
  totals <- env$total_step_counts(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(unique(totals$breakdown_value), "RESERVED_TOTAL")
})

test_that("party step counts leave out GA's overall rows", {
  env <- expmon_with_counts(counts_fixture)
  parties <- env$party_step_counts(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_setequal(parties$breakdown_value, c("rp-a", "rp-b"))
})

test_that("the task success series has one overall rate per window end", {
  env <- expmon_with_counts(counts_fixture)
  series <- env$task_success_series(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(series$window_end, as.Date(c("2026-09-01", "2026-09-02")))
  expect_equal(series$value, c(0.5, 0.75))
})

test_that("the latest task success is the newest window", {
  env <- expmon_with_counts(counts_fixture)
  latest <- env$task_success_latest(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(latest$window_end, as.Date("2026-09-02"))
  expect_equal(latest$value, 0.75)
})

test_that("the latest task success over an empty span is one all-NA row", {
  env <- expmon_with_counts(counts_fixture[0, ])
  latest <- env$task_success_latest(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(nrow(latest), 1L)
  expect_true(all(is.na(unlist(latest))))
})

test_that("the long window reads the one stored window ending on as_of", {
  env <- expmon_with_counts(counts_fixture)
  env$task_success_long_window(NULL, "sign_in", "2026-09-02")
  call <- env$calls[[1]]
  expect_identical(call$window_days, expmon$long_window_days)
  expect_identical(call$from, as.Date("2026-09-02"))
  expect_identical(call$to, as.Date("2026-09-02"))
})

# Relying party attribution ---------------------------------------------------

starters <- function(...) {
  counts <- c(...)
  tibble::tibble(breakdown_value = names(counts), activeusers = unname(counts))
}

test_that("unattributed share counts empty and (not set) names", {
  window <- tibble::tibble(
    breakdown_value = c("RESERVED_TOTAL", "rp-a", "", "(not set)"),
    activeusers = c(100, 60, 10, 30)
  )
  expect_equal(expmon$unattributed_share(window), 0.4)
})

test_that("unattributed share divides by the total row, not the party sum", {
  window <- starters(RESERVED_TOTAL = 100, "rp-a" = 95, "(not set)" = 10)
  expect_equal(expmon$unattributed_share(window), 0.1)
})

test_that("unattributed share is zero when every party is named", {
  window <- starters(RESERVED_TOTAL = 100, "rp-a" = 100)
  expect_equal(expmon$unattributed_share(window), 0)
})

test_that("unattributed share is NA without a usable total", {
  expect_true(is.na(expmon$unattributed_share(starters("rp-a" = 5))))
  expect_true(is.na(expmon$unattributed_share(starters(RESERVED_TOTAL = 0))))
})

test_that("largest starter drops rank the services that lost most", {
  previous <- starters(RESERVED_TOTAL = 100, "rp-a" = 50, "rp-b" = 30, "rp-c" = 10)
  current <- starters(RESERVED_TOTAL = 100, "rp-a" = 45, "rp-b" = 5, "rp-c" = 12)
  drops <- expmon$largest_starter_drops(current, previous)
  expect_equal(drops$breakdown_value, c("rp-b", "rp-a"))
  expect_equal(drops$drop, c(25, 5))
})

test_that("a service absent from the current window fell by all its starters", {
  previous <- starters(RESERVED_TOTAL = 100, "rp-a" = 40)
  current <- starters(RESERVED_TOTAL = 100, "(not set)" = 90)
  drops <- expmon$largest_starter_drops(current, previous)
  expect_equal(drops$breakdown_value, "rp-a")
  expect_equal(drops$activeusers_current, 0)
})

test_that("largest starter drops ignore the unattributed rows and the total", {
  previous <- starters(RESERVED_TOTAL = 100, "(not set)" = 50, "rp-a" = 5)
  current <- starters(RESERVED_TOTAL = 60, "(not set)" = 0, "rp-a" = 5)
  expect_equal(nrow(expmon$largest_starter_drops(current, previous)), 0)
})

test_that("largest starter drops keep at most n services", {
  previous <- starters(RESERVED_TOTAL = 9, "a" = 3, "b" = 2, "c" = 1)
  current <- starters(RESERVED_TOTAL = 0)
  expect_equal(nrow(expmon$largest_starter_drops(current, previous, n = 2)), 2)
})
