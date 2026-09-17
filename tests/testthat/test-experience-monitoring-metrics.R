# The preflight queries Athena through dbplyr rather than through readers, so
# there is nothing to stub and it is not tested here.

expmon <- load_dashboard("experience-monitoring")

# A dashboard whose step counts come from `counts`, recording each call.
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
