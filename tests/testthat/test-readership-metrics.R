readership <- load_dashboard("readership")

# Editions --------------------------------------------------------------------

test_that("edition numbers come from the title", {
  expect_identical(
    readership$edition_number(c("CanadaLogin Signal Check #6", "Signal Check #12",
                                "CanadaLogin Signal Check")),
    c(6L, 12L, NA)
  )
})

test_that("editions are dated by their file name, oldest first", {
  editions <- readership$edition_list(readership_traffic())
  expect_identical(editions$number, 3:5)
  expect_identical(editions$published,
                   as.Date(c("2026-08-10", "2026-08-24", "2026-10-03")))
  expect_identical(editions$label, c("#3", "#4", "#5"))
})

test_that("an edition sent before tracking is not comparable", {
  traffic <- dplyr::bind_rows(
    readership_traffic(),
    tibble::tibble(day = as.Date("2026-07-30"), page_kind = "signal_check",
                   pagepath_redacted = readership_edition("20260713"),
                   pagetitle = "CanadaLogin Signal Check #2", views = 1,
                   streamname = "Signal Check", sessionsource = "slack",
                   sessionmedium = "(not set)", sessions = 1)
  )
  editions <- readership$edition_list(traffic)
  coverage <- readership$edition_coverage(editions, readership_through)
  expect_false(coverage$comparable[coverage$number == 2])
  expect_true(coverage$comparable[coverage$number == 3])
})

test_that("only an edition with all 14 days seen is complete", {
  editions <- readership$edition_list(readership_traffic())
  coverage <- readership$edition_coverage(editions, readership_through)
  expect_identical(coverage$complete, c(TRUE, TRUE, FALSE))
  expect_identical(coverage$days_seen, c(14L, 14L, 4L))
})

test_that("cumulative views stop at day 14 and count quiet days as zero", {
  traffic <- readership_traffic() |>
    dplyr::filter(!(day == as.Date("2026-08-12") & page_kind == "signal_check"))
  editions <- readership$edition_list(traffic)
  coverage <- readership$edition_coverage(editions, readership_through)
  cumulative <- readership$cumulative_views(
    readership$edition_days(traffic, editions), coverage
  )
  third <- cumulative[cumulative$number == 3, ]
  expect_identical(third$age, 0:13)
  # 10 on the day sent, then 2 a day, with day 3 missing.
  expect_equal(third$cumulative[3], 12)
  expect_equal(max(third$cumulative), 10 + 2 * 12)
})

test_that("views before an edition is sent stay out of its first 14 days", {
  traffic <- dplyr::bind_rows(
    readership_traffic(),
    tibble::tibble(day = as.Date("2026-08-08"), page_kind = "signal_check",
                   pagepath_redacted = readership_edition("20260810"),
                   pagetitle = "CanadaLogin Signal Check #3", views = 7,
                   streamname = "Signal Check", sessionsource = "(direct)",
                   sessionmedium = "(none)", sessions = 7)
  )
  editions <- readership$edition_list(traffic)
  days <- readership$edition_days(traffic, editions)
  coverage <- readership$edition_coverage(editions, readership_through)
  cumulative <- readership$cumulative_views(days, coverage)
  expect_equal(sum(days$views[days$age < 0]), 7)
  expect_equal(max(cumulative$cumulative[cumulative$number == 3]), 10 + 2 * 13)
})

test_that("the typical edition is the median of complete ones at that age", {
  editions <- readership$edition_list(readership_traffic())
  coverage <- readership$edition_coverage(editions, readership_through)
  cumulative <- readership$cumulative_views(
    readership$edition_days(readership_traffic(), editions), coverage
  )
  expect_equal(readership$typical_at_age(cumulative, coverage, 1), 12)
})

# Dwell -----------------------------------------------------------------------

curve_of <- function(views, reached) {
  events <- tibble::tibble(
    page = "a",
    eventname = c("page_view", readership$dwell_events),
    events = c(views, reached)
  )
  # A milestone nobody reached has no row, as in the table.
  readership$dwell_curve(dplyr::filter(events, events > 0), "page")
}

test_that("a milestone nobody reached reads as zero", {
  curve <- curve_of(10, c(10, 8, 6, 5, 3, 0, 0, 0))
  expect_identical(nrow(curve), 8L)
  expect_equal(curve$share[curve$seconds == 600], 0)
})

test_that("the median read is the longest milestone half the views reached", {
  curve <- curve_of(10, c(10, 8, 6, 5, 3, 1, 0, 0))
  expect_equal(readership$median_dwell(curve$seconds, curve$share), 30)
})

test_that("the median read is zero when fewer than half reach a second", {
  curve <- curve_of(10, c(4, 2, 0, 0, 0, 0, 0, 0))
  expect_equal(readership$median_dwell(curve$seconds, curve$share), 0)
})

test_that("reading time counts each view down to its last milestone", {
  # Two views stop at 1 s, one at 30 s, one reaches 10 minutes.
  seconds <- readership$dwell_marks
  reached <- c(4, 2, 2, 2, 1, 1, 1, 1)
  expect_equal(readership$reading_seconds(seconds, reached),
               2 * 1 + 1 * 30 + 1 * 600)
})

test_that("dwell is pooled across days by page view, not by user", {
  events <- tibble::tibble(
    page = "a",
    day = rep(as.Date(c("2026-09-01", "2026-09-02")), each = 2),
    eventname = rep(c("page_view", "read_60s"), 2),
    events = c(4, 4, 6, 0)
  )
  curve <- readership$dwell_curve(events, "page")
  expect_equal(curve$share[curve$seconds == 60], 0.4)
})

# Dashboards and sources ----------------------------------------------------------

test_that("a renamed dashboard is counted under its new name", {
  traffic <- dplyr::bind_rows(
    readership_traffic(),
    tibble::tibble(
      day = as.Date(c("2026-08-11", "2026-09-01")), page_kind = "dashboard",
      pagepath_redacted = paste0(readership_prefix,
                                 c("task-success-monitoring.html",
                                   "experience-monitoring.html")),
      pagetitle = c("Task Success Monitoring Dashboard",
                    "Experience Monitoring Dashboard"),
      views = 1, streamname = "Signal Check", sessionsource = "slack",
      sessionmedium = "(not set)", sessions = 1
    )
  )
  dashboards <- readership$dashboard_list(traffic)
  expect_setequal(dashboards$name, c("Sign-In Activity", "Experience Monitoring"))
  expect_identical(
    dashboards$first_seen[dashboards$name == "Experience Monitoring"],
    as.Date("2026-08-11")
  )
})

test_that("sources fall into their groups", {
  expect_identical(
    readership$source_group(c("slack", "teams", "teams.public.onecdn.static.microsoft",
                              "email", "latest-link", "report_link",
                              "dashboard_link", "(direct)", "(not set)",
                              "github.com")),
    c("slack", "teams", "teams", "email", "our_links", "our_links",
      "our_links", "direct", "direct", "other")
  )
})

# Formatting ------------------------------------------------------------------

test_that("a median read reads as the band it falls in", {
  expect_identical(readership$fmt_dwell(c(0, 30, 60, 600, NA)),
                   c("Under 1 s", "30 s to 1 min", "1 to 2 min",
                     "10 min or more", "-"))
})

test_that("labels too close together are pushed apart, in order", {
  expect_equal(readership$spread_labels(c(35, 33, 25), 3), c(36, 33, 25))
  expect_equal(readership$spread_labels(c(10, 30), 3), c(10, 30))
})
