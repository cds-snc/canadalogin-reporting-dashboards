# Direct dbplyr queries keep the preflight outside this stubbed test suite.

expmon <- load_dashboard("experience-monitoring")

# Stub the stored rows and record each call.
expmon_with_rows <- function(rows) {
  env <- load_dashboard("experience-monitoring")
  env$calls <- list()
  stub(env, task_success_rows = function(con, funnel_id, window_days, from, to) {
    env$calls[[length(env$calls) + 1L]] <- list(
      funnel_id = funnel_id, window_days = window_days, from = from, to = to
    )
    rows
  })
}

day_1 <- as.Date("2026-09-01")
day_2 <- as.Date("2026-09-02")

rows_fixture <- tibble::tribble(
  ~window_end, ~funnel_version, ~breakdown_value, ~value, ~starters, ~finished,
  day_1,       "v1",            "RESERVED_TOTAL", 0.5,    100,       50,
  day_1,       "v1",            "rp-a",           0.5,    60,        30,
  day_1,       "v1",            "rp-b",           0.5,    40,        20,
  day_2,       "v1",            "RESERVED_TOTAL", 0.75,   120,       90,
  day_2,       "v1",            "rp-a",           0.75,   120,       90
)

# Configuration ---------------------------------------------------------------

test_that("every required funnel declares its entry step", {
  expect_true(all(expmon$required_funnels %in% names(expmon$funnel_entry_steps)))
})

test_that("every entry step is one step name", {
  for (step in expmon$funnel_entry_steps) {
    expect_type(step, "character")
    expect_length(step, 1)
  }
})

test_that("every required funnel has a label and a note", {
  for (funnel in expmon$required_funnels) {
    expect_type(expmon$funnel_label(funnel), "character")
    expect_type(expmon$funnel_note(funnel), "character")
  }
})

test_that("an undeclared funnel stops with the table to add it to", {
  expect_error(expmon$funnel_label("not_a_funnel"),
               "'not_a_funnel' has no entry in funnel_presentation")
})

# Rates -----------------------------------------------------------------------

test_that("total task success is GA's overall rows only", {
  env <- expmon_with_rows(rows_fixture)
  totals <- env$total_task_success(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(unique(totals$breakdown_value), "RESERVED_TOTAL")
})

test_that("party task success leaves out GA's overall rows", {
  env <- expmon_with_rows(rows_fixture)
  parties <- env$party_task_success(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_setequal(parties$breakdown_value, c("rp-a", "rp-b"))
})

test_that("the task success series is the stored overall value per window end", {
  env <- expmon_with_rows(rows_fixture)
  series <- env$task_success_series(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(series$window_end, as.Date(c("2026-09-01", "2026-09-02")))
  expect_equal(series$value, c(0.5, 0.75))
})

test_that("the latest task success is the newest window", {
  env <- expmon_with_rows(rows_fixture)
  latest <- env$task_success_latest(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(latest$window_end, as.Date("2026-09-02"))
  expect_equal(latest$value, 0.75)
})

test_that("the latest task success over an empty span is one all-NA row", {
  env <- expmon_with_rows(rows_fixture[0, ])
  latest <- env$task_success_latest(NULL, "sign_in", 7, "2026-09-01", "2026-09-02")
  expect_identical(nrow(latest), 1L)
  expect_true(all(is.na(unlist(latest))))
})

test_that("the long window reads the one stored window ending on as_of", {
  env <- expmon_with_rows(rows_fixture)
  env$task_success_long_window(NULL, "sign_in", "2026-09-02")
  call <- env$calls[[1]]
  expect_identical(call$window_days, expmon$long_window_days)
  expect_identical(call$from, as.Date("2026-09-02"))
  expect_identical(call$to, as.Date("2026-09-02"))
})

test_that("a missing long window is one all-NA row, not none", {
  env <- expmon_with_rows(rows_fixture[0, ])
  long <- env$task_success_long_window(NULL, "sign_in", "2026-09-02")
  expect_identical(nrow(long), 1L)
  expect_true(is.na(long$value))
})

test_that("the party long window reads the same stored window", {
  env <- expmon_with_rows(rows_fixture)
  env$party_task_success_long_window(NULL, "sign_in", "2026-09-02")
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

test_that("attribution under the threshold is clear", {
  expect_identical(expmon$attribution_status(0.05, 0.5, 0.1), "clear")
  expect_identical(expmon$attribution_status(0.1, NA, 0.1), "clear")
})

test_that("attribution over the threshold for the first week is an onset", {
  expect_identical(expmon$attribution_status(0.7, 0.01, 0.1), "onset")
  expect_identical(expmon$attribution_status(0.7, 0.1, 0.1), "onset")
})

test_that("attribution with no prior window is an onset", {
  expect_identical(expmon$attribution_status(0.7, NA, 0.1), "onset")
})

test_that("attribution over the threshold a second week is ongoing", {
  expect_identical(expmon$attribution_status(0.8, 0.7, 0.1), "ongoing")
})

test_that("attribution without a current total has no status", {
  expect_true(is.na(expmon$attribution_status(NA, 0.7, 0.1)))
})

# Change against the window before ---------------------------------------------

test_that("change noise is the standard error of the difference", {
  expect_equal(expmon$change_noise(0.5, 100, 0.5, 100), sqrt(0.005))
})

test_that("change noise is pooled, so a window at 0% still has noise", {
  expect_gt(expmon$change_noise(0.8, 30, 0, 2), 0.25)
})

test_that("change noise is NA without starters on both sides", {
  expect_true(all(is.na(expmon$change_noise(c(0.5, 0.5), c(0, 100),
                                            c(0.5, 0.5), c(100, NA)))))
})

test_that("a change is meaningful only past the noise band", {
  expect_identical(
    expmon$is_meaningful_change(c(0.05, -0.05, 0.01), 0.02),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("a change with no delta or no noise is not meaningful", {
  expect_identical(
    expmon$is_meaningful_change(c(NA, 0.05), c(0.02, NA)), c(FALSE, FALSE)
  )
})

test_that("a rate cell shows whole points with no sign", {
  cell <- expmon$rate_change_cell(0.64, -0.042, TRUE)
  expect_match(cell, "^64% ")
  expect_match(cell, "▼ 4<", fixed = TRUE)
  expect_no_match(cell, "-")
})

test_that("a rate cell colours a meaningful change and greys the rest", {
  expect_match(expmon$rate_change_cell(0.64, 0.03, TRUE),
               delta_colours[["up"]], fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, -0.03, TRUE),
               delta_colours[["down"]], fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, 0.03, FALSE),
               expmon$quiet_change_colour, fixed = TRUE)
})

test_that("a rate cell tells no change apart from a dash and n/a", {
  expect_match(expmon$rate_change_cell(0.64, 0.004, TRUE), ">\u25cf\u00a00<",
               fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, NA, FALSE), ">-<", fixed = TRUE)
  expect_identical(expmon$rate_change_cell(NA, NA, FALSE), "n/a")
})
