# The preflight's queries are direct dbplyr, so only its pure helpers are
# tested here.

expmon <- load_dashboard("experience-monitoring")

as_of <- as.Date("2026-11-15")
paused <- c(migration_new_to_cl = "2026-11-06")

starters <- function(funnel = "migration_new_to_cl", activeusers = 30,
                     window_end = as_of, step = "migration_not_skipped",
                     breakdown_value = "RESERVED_TOTAL") {
  tibble::tibble(funnel_id = funnel, funnelstepname = step,
                 window_end = window_end, breakdown_value = breakdown_value,
                 activeusers = activeusers)
}

resumed <- function(counts) {
  expmon$resumed_funnels(counts, as_of, paused, min_starters = 30)
}

# Declaration -----------------------------------------------------------------

test_that("every paused funnel is a required funnel with a valid date", {
  expect_true(all(names(expmon$paused_funnels) %in% expmon$required_funnels))
  expect_false(anyNA(as.Date(expmon$paused_funnels, optional = TRUE)))
})

test_that("live funnels are the required funnels that are not paused", {
  expect_identical(expmon$live_funnels(paused),
                   setdiff(expmon$required_funnels, "migration_new_to_cl"))
  expect_identical(expmon$live_funnels(c()), expmon$required_funnels)
})

# resumed_funnels -------------------------------------------------------------

test_that("a paused funnel at the starter floor has resumed", {
  expect_identical(resumed(starters()), "migration_new_to_cl")
})

test_that("a paused funnel under the floor has not resumed", {
  expect_length(resumed(starters(activeusers = 29)), 0)
})

test_that("only the total at the entry step in the as-of window counts", {
  counts <- rbind(
    starters(window_end = as_of - 7L),
    starters(step = "terms"),
    starters(breakdown_value = "rp-a")
  )
  expect_length(resumed(counts), 0)
})

test_that("a live funnel's starters do not count", {
  expect_length(resumed(starters(funnel = "sign_in", step = "enter_email")), 0)
})

test_that("no rows means nothing has resumed", {
  expect_length(resumed(starters()[0, ]), 0)
})

# starter_volume --------------------------------------------------------------

# sign_in entry-step totals for the window ending as_of and the four before.
weekly <- function(current, earlier = c(100, 100, 100, 100)) {
  starters(funnel = "sign_in", step = "enter_email",
           window_end = as_of - 7L * 0:4, activeusers = c(current, earlier))
}

volume <- function(counts, funnels = "sign_in") {
  expmon$starter_volume(counts, as_of, funnels)
}

test_that("the baseline is the median of the four weeks before", {
  v <- volume(weekly(50, earlier = c(10, 100, 120, 1000)))
  expect_identical(v$current, 50)
  expect_identical(v$baseline, 110)
})

test_that("a window with no row counts as no starters", {
  v <- volume(weekly(50)[-1, ])
  expect_identical(v$current, 0)
  expect_identical(v$baseline, 100)
})

test_that("only the total at the entry step counts", {
  counts <- rbind(
    weekly(50),
    starters(funnel = "sign_in", step = "enter_email", breakdown_value = "rp-a",
             activeusers = 999),
    starters(funnel = "sign_in", step = "terms", activeusers = 999)
  )
  expect_identical(volume(counts)$current, 50)
})

test_that("a funnel with no rows at all has no starters and no baseline", {
  v <- volume(weekly(50), funnels = c("sign_in", "sign_up"))
  expect_identical(v$funnel_id, c("sign_in", "sign_up"))
  expect_identical(v$current, c(50, 0))
  expect_identical(v$baseline, c(100, 0))
})

# step_name_problems -----------------------------------------------------------

seen_steps <- function(funnel = "sign_up", steps = c("terms", "complete")) {
  tibble::tibble(funnel_id = funnel, funnelstepname = steps)
}

test_that("declared entry and exit steps in the data raise nothing", {
  expect_length(expmon$step_name_problems(seen_steps(), "sign_up"), 0)
})

test_that("a missing exit step is a problem", {
  problems <- expmon$step_name_problems(seen_steps(steps = "terms"), "sign_up")
  expect_length(problems, 1)
  expect_match(problems, "exit step 'complete'", fixed = TRUE)
})

test_that("a missing entry step is a problem", {
  problems <- expmon$step_name_problems(seen_steps(steps = "complete"),
                                        "sign_up")
  expect_match(problems, "entry step 'terms'", fixed = TRUE)
})

test_that("an undeclared exit step is a problem", {
  exits <- expmon$funnel_exit_steps
  exits$sign_up <- NULL
  problems <- expmon$step_name_problems(seen_steps(), "sign_up",
                                        exit_steps = exits)
  expect_match(problems, "no exit step declared in funnel_exit_steps",
               fixed = TRUE)
})

test_that("every required funnel declares an exit step", {
  expect_setequal(names(expmon$funnel_exit_steps), expmon$required_funnels)
})
