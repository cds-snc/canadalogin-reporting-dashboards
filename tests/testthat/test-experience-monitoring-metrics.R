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

test_that("every required funnel has a label", {
  for (funnel in expmon$required_funnels) {
    expect_type(expmon$funnel_label(funnel), "character")
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

# GA freshness ----------------------------------------------------------------

freshness_days <- seq(day_1, by = "day", length.out = 3)

# RESERVED_TOTAL rows for `funnel`, one per date in each named window length.
freshness_rows <- function(..., funnel = "recover_2sv") {
  windows <- list(...)
  tibble::tibble(
    funnel_id = funnel,
    window_days = rep(names(windows), lengths(windows)),
    breakdown_value = "RESERVED_TOTAL",
    window_end = do.call(c, unname(windows))
  )
}

test_that("a funnel with rows every day has no missing days", {
  raw <- freshness_rows("1" = freshness_days, "7" = freshness_days)
  expect_length(expmon$funnel_missing_days(raw, "recover_2sv", freshness_days), 0)
})

test_that("a day with no starters is not missing while a longer window ends on it", {
  raw <- freshness_rows("1" = freshness_days[-2], "7" = freshness_days)
  expect_length(expmon$funnel_missing_days(raw, "recover_2sv", freshness_days), 0)
})

test_that("a day with no window at all is missing", {
  raw <- freshness_rows("1" = freshness_days[-2], "7" = freshness_days[-2])
  expect_identical(
    expmon$funnel_missing_days(raw, "recover_2sv", freshness_days),
    freshness_days[2]
  )
})

test_that("missing days read only the total rows of the named funnel", {
  raw <- dplyr::bind_rows(
    freshness_rows("7" = freshness_days[-3]),
    freshness_rows("7" = freshness_days, funnel = "sign_in"),
    dplyr::mutate(freshness_rows("7" = freshness_days), breakdown_value = "rp-a")
  )
  expect_identical(
    expmon$funnel_missing_days(raw, "recover_2sv", freshness_days),
    freshness_days[3]
  )
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
    expmon$is_meaningful_change(c(0.05, -0.05, 0.01), 0.02, 100, 100),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("a change with no delta or no noise is not meaningful", {
  expect_identical(
    expmon$is_meaningful_change(c(NA, 0.05), c(0.02, NA), 100, 100),
    c(FALSE, FALSE)
  )
})

test_that("a change is not meaningful with too few starters in either window", {
  expect_identical(
    expmon$is_meaningful_change(1, 0.02, c(3, 100, 30, NA), c(100, 3, 30, 100)),
    c(FALSE, FALSE, TRUE, FALSE)
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
               verdict_colours[["good"]], fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, -0.03, TRUE),
               verdict_colours[["bad"]], fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, 0.03, FALSE),
               verdict_colours[["flat"]], fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, 0.004, TRUE),
               verdict_colours[["flat"]], fixed = TRUE)
})

test_that("a rate cell tells no change apart from a dash and n/a", {
  expect_match(expmon$rate_change_cell(0.64, 0.004, TRUE), ">\u25cf\u00a00<",
               fixed = TRUE)
  expect_match(expmon$rate_change_cell(0.64, NA, FALSE), ">-<", fixed = TRUE)
  expect_identical(expmon$rate_change_cell(NA, NA, FALSE), "n/a")
})

test_that("a change in points stands alone without a rate", {
  html <- expmon$change_points_html(0.05, FALSE)
  expect_match(html, "^<span")
  expect_match(html, "▲ 5<", fixed = TRUE)
})

long_window_row <- function(value, starters) {
  tibble::tibble(value = value, starters = starters)
}

test_that("the long change line compares with the window before", {
  line <- expmon$long_change_line(long_window_row(0.6, 500),
                                  long_window_row(0.5, 500))
  expect_match(line, "^<span")
  expect_match(line, "</span> points vs. previous 28 days$")
})

test_that("the long change line points up when the rate rose", {
  line <- expmon$long_change_line(long_window_row(0.6, 500),
                                  long_window_row(0.5, 500))
  expect_match(line, "\u25b2\u00a010</span>", fixed = TRUE)
})

test_that("the long change line points down when the rate fell", {
  line <- expmon$long_change_line(long_window_row(0.5, 500),
                                  long_window_row(0.6, 500))
  expect_match(line, "\u25bc\u00a010</span>", fixed = TRUE)
})

test_that("the long change line says point for a change of one", {
  line <- expmon$long_change_line(long_window_row(0.51, 500),
                                  long_window_row(0.5, 500))
  expect_match(line, "</span> point vs. previous 28 days")
})

test_that("the long change line colours a change beyond the noise", {
  line <- expmon$long_change_line(long_window_row(0.6, 500),
                                  long_window_row(0.5, 500))
  expect_match(line, verdict_colours[["good"]], fixed = TRUE)
})

test_that("the long change line greys a change within the noise", {
  line <- expmon$long_change_line(long_window_row(0.6, 40),
                                  long_window_row(0.55, 40))
  expect_match(line, verdict_colours[["flat"]], fixed = TRUE)
})

test_that("the long change line says when there is nothing to compare", {
  expect_identical(
    expmon$long_change_line(long_window_row(0.6, 500),
                            long_window_row(NA_real_, NA_real_)),
    "No earlier window to compare against yet"
  )
})

# Step table -------------------------------------------------------------------

funnel_steps <- function(people = c(100, 80, 60, 40),
                         names = c("a", "b", "c", "d")) {
  tibble::tibble(funnelstepnumber = seq_along(people), funnelstepname = names,
                 activeusers = people)
}

test_that("step rows run in step order", {
  rows <- expmon$step_table_rows(funnel_steps()[4:1, ], "a", "d")
  expect_identical(rows$step, c("a", "b", "c", "d"))
})

test_that("each step's share is of the first step", {
  rows <- expmon$step_table_rows(funnel_steps(), "a", "d")
  expect_equal(rows$share_first, c(1, 0.8, 0.6, 0.4))
})

test_that("each step's continued share is of the step before the next", {
  rows <- expmon$step_table_rows(funnel_steps(), "a", "d")
  expect_equal(rows$continued, c(0.8, 0.75, 40 / 60, NA))
})

test_that("only steps from the entry to the exit step are counted", {
  rows <- expmon$step_table_rows(funnel_steps(), "b", "c")
  expect_identical(rows$counted, c(FALSE, TRUE, TRUE, FALSE))
})

test_that("an undeclared span step leaves no step counted", {
  rows <- expmon$step_table_rows(funnel_steps(), "b", "gone")
  expect_false(any(rows$counted))
})

test_that("a later step with more people keeps its real count", {
  rows <- expmon$step_table_rows(funnel_steps(c(100, 80, 90, 40)), "a", "d")
  expect_identical(rows$users[3], 90)
  expect_equal(rows$continued[2], 90 / 80)
})

test_that("the step table matches the add passkey reference window", {
  rows <- expmon$step_table_rows(funnel_steps(c(120, 111, 102, 64, 60),
                                              letters[1:5]), "b", "e")
  expect_equal(round(100 * rows$share_first, 1), c(100, 92.5, 85, 53.3, 50))
  expect_equal(round(100 * rows$continued, 1), c(92.5, 91.9, 62.7, 93.8, NA))
})

step_window <- function(window_end = as.Date("2026-11-18"), days = "28",
                        breakdown = "RESERVED_TOTAL", funnel = "sign_up") {
  tibble::tibble(funnel_id = funnel, window_days = days,
                 window_end = window_end, breakdown_value = breakdown,
                 funnelstepnumber = 1:2, funnelstepname = c("terms", "complete"),
                 activeusers = c(10, 5))
}

expmon_with_steps <- function(steps) {
  env <- load_dashboard("experience-monitoring")
  stub(env, funnel_tables = function(con) list(steps = steps))
}

test_that("the step table reads the long-window total ending on as_of", {
  steps <- dplyr::bind_rows(
    step_window(),
    step_window(window_end = as.Date("2026-11-11")),
    step_window(days = "7"),
    step_window(breakdown = "rp-a"),
    step_window(funnel = "sign_in")
  )
  env <- expmon_with_steps(steps)
  rows <- env$funnel_step_table(NULL, "sign_up", as.Date("2026-11-18"))
  expect_identical(nrow(rows), 2L)
  expect_true(all(rows$counted))
})

test_that("an empty window gives no rows rather than an earlier window", {
  env <- expmon_with_steps(step_window(window_end = as.Date("2026-11-11")))
  rows <- env$funnel_step_table(NULL, "sign_up", as.Date("2026-11-18"))
  expect_identical(nrow(rows), 0L)
})

test_that("a step without a polite name shows its slug opened up", {
  expect_identical(expmon$step_label(list(), "sign_up", "setup_2sv_start"),
                   "setup 2sv start")
})

test_that("a polite name is looked up by funnel and step", {
  labels <- list(sign_up = list(terms = "Terms"), sign_in = list(terms = "No"))
  expect_identical(expmon$step_label(labels, "sign_up", c("terms", "name")),
                   c("Terms", "name"))
})

test_that("a missing step label file gives no labels", {
  expect_identical(expmon$read_step_labels(tempfile()), list())
})

# step-labels.yml ---------------------------------------------------------------

step_labels_path <- file.path(repo_root, "dashboards", "experience-monitoring",
                              "step-labels.yml")

test_that("step-labels.yml is keyed by required funnels", {
  labels <- expmon$read_step_labels(step_labels_path)
  expect_true(all(names(labels) %in% expmon$required_funnels))
})

test_that("step-labels.yml gives each step one non-empty name", {
  labels <- expmon$read_step_labels(step_labels_path)
  for (funnel in names(labels)) {
    expect_true(is.list(labels[[funnel]]), info = funnel)
    expect_false(is.null(names(labels[[funnel]])), info = funnel)
    for (name in labels[[funnel]]) {
      expect_true(is.character(name) && length(name) == 1 && nzchar(name),
                  info = funnel)
    }
  }
})

test_that("step-labels.yml names no step twice in a funnel", {
  lines <- readLines(step_labels_path)
  funnel <- cumsum(grepl("^[a-z]", lines))
  steps <- sub(":.*", "", trimws(lines[grepl("^  [a-z0-9_]+:", lines)]))
  expect_false(anyDuplicated(paste(funnel[grepl("^  [a-z0-9_]+:", lines)],
                                   steps)) > 0)
})

test_that("a change in points takes an override colour", {
  expect_match(expmon$change_points_html(0.05, TRUE, colour = "inherit"),
               'style="color:inherit"', fixed = TRUE)
})

test_that("the long change line says no change when it rounds to nothing", {
  expect_identical(
    expmon$long_change_line(long_window_row(0.504, 500),
                            long_window_row(0.5, 500)),
    "No change vs. previous 28 days"
  )
})
