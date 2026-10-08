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
  expect_identical(editions$label,
                   c("Signal Check #3", "Signal Check #4", "Signal Check #5"))
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

test_that("the median read sits between the milestones either side of half", {
  # 60% open at 15 s, 40% at 30 s: halfway on a log scale, about 21 s.
  curve <- curve_of(10, c(10, 8, 6, 4, 3, 1, 0, 0))
  expect_equal(readership$median_read(curve$seconds, curve$share),
               sqrt(15 * 30))
})

test_that("the median read lands on a milestone exactly half reached", {
  curve <- curve_of(10, c(10, 8, 6, 5, 3, 1, 0, 0))
  expect_equal(readership$median_read(curve$seconds, curve$share), 30)
})

test_that("the median read is zero when fewer than half reach a second", {
  curve <- curve_of(10, c(4, 2, 0, 0, 0, 0, 0, 0))
  expect_equal(readership$median_read(curve$seconds, curve$share), 0)
})

test_that("the median read is unbounded when half stay past 10 minutes", {
  curve <- curve_of(10, c(10, 9, 9, 8, 8, 7, 6, 6))
  expect_identical(readership$median_read(curve$seconds, curve$share), Inf)
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

test_that("a dashboard's accent is the primary in its theme", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "support"))
  writeLines(c("// Forest", "/*-- scss:defaults --*/", "$primary: #115740;",
               "$info: #D9E5D3;"),
             file.path(dir, "support", "_theme.scss"))
  expect_identical(readership$dashboard_accent(c("support", "gone"), dir),
                   c("#115740", NA))
})

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

test_that("a median read reads as decimal minutes", {
  expect_identical(
    readership$fmt_read(c(0, 30, 21.2, 85, 120, Inf, NA)),
    c("0 min", "0.5 min", "0.4 min", "1.4 min", "2 min", "Over 10 min", "-")
  )
})
