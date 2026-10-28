# The checks live here, not in the dashboard: people who do not write R edit
# annotations.yml, and CI runs this on every PR so a bad entry fails before it
# merges. The render reads the file without checking it.

expmon <- load_dashboard("experience-monitoring")

annotations_path <- file.path(
  repo_root, "dashboards", "experience-monitoring", "annotations.yml"
)

annotation_keys <- c("funnel", "start", "end", "description", "chart")

is_date <- function(value) {
  is.character(value) && length(value) == 1L &&
    !is.na(as.Date(value, format = "%Y-%m-%d"))
}

# One line per problem, naming the entry so an editor can find it.
annotation_problems <- function(entries, funnel_ids) {
  problems <- character()
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    text <- paste(entry$description, collapse = " ")
    if (nchar(text) > 30L) text <- paste0(substr(text, 1, 30), "...")
    name <- sprintf('entry %d ("%s")', i, text)
    flag <- function(problem) {
      problems <<- c(problems, paste0(name, ": ", problem))
    }

    for (key in setdiff(names(entry), annotation_keys)) {
      flag(paste0("unknown key '", key, "'"))
    }
    if (is.null(entry$funnel)) {
      flag("no funnel")
    }
    for (funnel in setdiff(unlist(entry$funnel), c("all", funnel_ids))) {
      flag(paste0("unknown funnel '", funnel, "'"))
    }
    if (!is.character(entry$description) || !nzchar(entry$description)) {
      flag("no description")
    }
    if (!is.null(entry$start) && !is_date(entry$start)) {
      flag("start is not a YYYY-MM-DD date")
    }
    if (!is.null(entry$end) && !is_date(entry$end)) {
      flag("end is not a YYYY-MM-DD date")
    }
    if (!is.null(entry$end) && is.null(entry$start)) {
      flag("end without a start")
    }
    if (is_date(entry$start) && is_date(entry$end) &&
          as.Date(entry$end) < as.Date(entry$start)) {
      flag("end is before start")
    }
    if (!is.null(entry$chart) && !isTRUE(entry$chart) && !isFALSE(entry$chart)) {
      flag("chart is not true or false")
    }
  }
  problems
}

problems_in <- function(yaml_text) {
  annotation_problems(yaml::yaml.load(yaml_text), expmon$required_funnels)
}

# The file ---------------------------------------------------------------------

test_that("annotations.yml has no problems", {
  entries <- yaml::read_yaml(annotations_path)
  problems <- annotation_problems(entries, expmon$required_funnels)
  # expect() rather than expect_equal(), so the failure lists the problems
  # instead of only counting them.
  expect(length(problems) == 0L,
         paste(c("annotations.yml:", problems), collapse = "\n  "))
})

# The checks -------------------------------------------------------------------

test_that("a complete entry passes", {
  expect_equal(problems_in("
- funnel: [sign_in, sign_up]
  start: 2026-09-17
  end: 2026-10-01
  description: Something happened.
  chart: false
"), character())
})

test_that("a standing note passes", {
  expect_equal(problems_in("
- funnel: all
  description: Always shown.
"), character())
})

test_that("an unknown key fails, naming the entry", {
  expect_equal(
    problems_in("
- funnel: sign_up
  strat: 2026-09-17
  description: Scripted account creation.
"),
    "entry 1 (\"Scripted account creation.\"): unknown key 'strat'"
  )
})

test_that("an unknown funnel fails", {
  expect_match(problems_in("
- funnel: [sign_in, sign_on]
  description: Something.
"), "unknown funnel 'sign_on'")
})

test_that("a missing funnel fails", {
  expect_match(problems_in("
- description: Something.
"), "no funnel")
})

test_that("a missing description fails", {
  expect_match(problems_in("
- funnel: sign_in
  start: 2026-09-17
"), "no description")
})

test_that("a start that is not a date fails", {
  expect_match(problems_in("
- funnel: sign_in
  start: 2026-09-31
  description: Something.
"), "start is not a YYYY-MM-DD date")
})

test_that("an end that is not a date fails", {
  expect_match(problems_in("
- funnel: sign_in
  start: 2026-09-17
  end: next week
  description: Something.
"), "end is not a YYYY-MM-DD date")
})

test_that("an end without a start fails", {
  expect_match(problems_in("
- funnel: sign_in
  end: 2026-09-17
  description: Something.
"), "end without a start")
})

test_that("an end before its start fails", {
  expect_match(problems_in("
- funnel: sign_in
  start: 2026-09-17
  end: 2026-09-01
  description: Something.
"), "end is before start")
})

test_that("a chart that is not true or false fails", {
  expect_match(problems_in("
- funnel: sign_in
  start: 2026-09-17
  description: Something.
  chart: maybe
"), "chart is not true or false")
})

# Rendering --------------------------------------------------------------------

annotations_fixture <- function(yaml_text) {
  path <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml_text, path)
  expmon$read_annotations(path)
}

fixture <- annotations_fixture("
- funnel: all
  description: Points note.
- funnel: sign_up
  description: Sign up note.
- funnel: sign_in
  description: Sign in note.
- funnel: sign_up
  start: 2026-09-30
  description: Late.
- funnel: [sign_in, sign_up]
  start: 2026-09-10
  end: 2026-09-20
  description: Early.
- funnel: sign_up
  start: 2026-09-15
  description: Listed only.
  chart: false
- funnel: sign_up
  start: 2025-01-01
  description: Before the chart.
")

chart_from <- as.Date("2026-09-12")
chart_to <- as.Date("2026-10-25")

sign_up <- expmon$funnel_annotations(fixture, "sign_up", chart_from, chart_to)

test_that("a missing end means a single day", {
  late <- fixture[fixture$description == "Late.", ]
  expect_equal(late$end, late$start)
})

test_that("chart defaults to true", {
  expect_true(fixture$chart[fixture$description == "Late."])
})

test_that("a funnel gets its own entries and the all entries only", {
  expect_false("Sign in note." %in% sign_up$description)
  expect_true(all(c("Points note.", "Sign up note.") %in% sign_up$description))
})

test_that("an entry before the chart is dropped", {
  expect_false("Before the chart." %in% sign_up$description)
})

test_that("a range crossing the chart's edge is kept", {
  expect_true("Early." %in% sign_up$description)
})

test_that("standing notes come first in file order, then dated by start", {
  expect_equal(sign_up$description, c(
    "Points note.", "Sign up note.", "Early.", "Listed only.", "Late."
  ))
})

test_that("only drawn entries are lettered, without skipping", {
  expect_equal(sign_up$letter, c(NA, NA, "A", NA, "B"))
})

test_that("the caption letters drawn entries and dates every dated one", {
  expect_equal(
    expmon$annotation_caption(sign_up),
    paste(
      "Points note.", "Sign up note.",
      "**A.** Sep 10 to Sep 20, 2026: Early.",
      "Sep 15, 2026: Listed only.",
      "**B.** Sep 30, 2026: Late.",
      sep = "<br>"
    )
  )
})

test_that("a range across a year names both years", {
  expect_equal(
    expmon$fmt_event_dates(as.Date("2026-12-20"), as.Date("2027-01-03")),
    "Dec 20, 2026 to Jan 3, 2027"
  )
})

test_that("a band is clipped to the chart", {
  layers <- expmon$annotation_layers(sign_up, chart_from, chart_to)
  expect_equal(layers[[1]]$data$xmin, chart_from)
})

test_that("a single day draws a line, not a band", {
  layers <- expmon$annotation_layers(sign_up, chart_from, chart_to)
  expect_equal(nrow(layers[[1]]$data), 1L)
  expect_true(as.Date("2026-09-30") %in% layers[[2]]$data$xintercept)
})

test_that("a band has a line at each end", {
  layers <- expmon$annotation_layers(sign_up, chart_from, chart_to)
  expect_true(all(c(chart_from, as.Date("2026-09-20")) %in%
                    layers[[2]]$data$xintercept))
})

test_that("an entry with chart false draws nothing", {
  layers <- expmon$annotation_layers(sign_up, chart_from, chart_to)
  expect_equal(layers[[3]]$data$letter, c("A", "B"))
})

test_that("a funnel with no dated entries draws nothing", {
  events <- expmon$funnel_annotations(fixture, "recover_2sv", chart_from,
                                      chart_to)
  layers <- expmon$annotation_layers(events, chart_from, chart_to)
  expect_equal(nrow(layers[[3]]$data), 0L)
})
