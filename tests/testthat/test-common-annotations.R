annotations <- load_common("annotations.R")

# The checks use expmon's key and ids, as a real file would.
funnel_ids <- c("sign_in", "sign_up", "recover_password")

problems_in <- function(yaml_text) {
  annotation_problems(yaml::yaml.load(yaml_text), "funnel", funnel_ids)
}


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
  annotations$read_annotations(path, scope = "funnel")
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

sign_up <- annotations$scoped_annotations(fixture, "sign_up", chart_from,
                                          chart_to)
line <- data.frame(window_end = as.Date(c("2026-09-29", "2026-10-01")),
                   value = c(0.6, 0.8))
line_mapping <- ggplot2::aes(window_end, value)

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
    annotations$annotation_caption(sign_up),
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
    annotations$fmt_event_dates(as.Date("2026-12-20"), as.Date("2027-01-03")),
    "Dec 20, 2026 to Jan 3, 2027"
  )
})

test_that("a band is clipped to the chart", {
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, line,
                                          line_mapping)
  expect_equal(layers[[1]]$data$xmin, chart_from)
})

test_that("a single day draws an arrow, not a band or a line", {
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, line,
                                          line_mapping)
  expect_equal(nrow(layers[[1]]$data), 1L)
  expect_false(as.Date("2026-09-30") %in% layers[[2]]$data$xintercept)
  expect_equal(layers[[3]]$data$xmin, as.Date("2026-09-30"))
})

test_that("a single day's arrow points at the 7-day line", {
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, line,
                                          line_mapping)
  arrow <- layers[[3]]$data
  expect_gt(arrow$tip_y, 0.7)
  expect_lt(arrow$tip_y, 0.72)
  expect_gt(arrow$tail_y, arrow$tip_y)
})

test_that("an arrow stands square to a sloped line", {
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, line,
                                          line_mapping)
  # The line rises, so the arrow leans back from the upper left.
  expect_lt(layers[[3]]$data$tail_x, as.Date("2026-09-30"))
})

test_that("an arrow over a flat line is upright", {
  flat <- data.frame(window_end = as.Date(c("2026-09-01", "2026-10-20")),
                     value = 0.8)
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, flat,
                                          line_mapping)
  expect_equal(layers[[3]]$data$tail_x, as.Date("2026-09-30"))
})

test_that("a band has a line at each end", {
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, line,
                                          line_mapping)
  expect_true(all(c(chart_from, as.Date("2026-09-20")) %in%
                    layers[[2]]$data$xintercept))
})

test_that("an entry with chart false draws nothing", {
  layers <- annotations$annotation_layers(sign_up, chart_from, chart_to, line,
                                          line_mapping)
  expect_equal(c(layers[[5]]$data$letter, layers[[4]]$data$letter),
               c("A", "B"))
})

test_that("a funnel with no dated entries draws nothing", {
  events <- annotations$scoped_annotations(fixture, "recover_2sv", chart_from,
                                           chart_to)
  layers <- annotations$annotation_layers(events, chart_from, chart_to, line,
                                          line_mapping)
  expect_equal(nrow(layers[[4]]$data) + nrow(layers[[5]]$data), 0L)
})

test_that("the scope key is the dashboard's own", {
  path <- withr::local_tempfile(fileext = ".yml")
  writeLines("
- page: [traffic]
  start: 2026-09-30
  description: Late.
", path)
  read <- annotations$read_annotations(path, scope = "page")
  expect_equal(nrow(annotations$scoped_annotations(read, "traffic",
                                                   chart_from, chart_to)), 1L)
})

test_that("the scope check names the dashboard's key", {
  problems <- annotation_problems(
    yaml::yaml.load("- description: Something."), "page", "traffic"
  )
  expect_match(problems, "no page")
})

# The arrow on its own ---------------------------------------------------------

arrow_at <- data.frame(day = as.Date("2026-09-30"), tag = "Z")

test_that("an arrow without a label is one segment layer", {
  layers <- annotations$geom_perpendicular_arrow(
    ggplot2::aes(x = day), arrow_at, line, line_mapping,
    x_range = c(chart_from, chart_to)
  )
  expect_length(layers, 1L)
  expect_s3_class(layers[[1]]$geom, "GeomSegment")
})

test_that("an arrow's label comes from its mapping", {
  layers <- annotations$geom_perpendicular_arrow(
    ggplot2::aes(x = day, label = tag), arrow_at, line, line_mapping,
    x_range = c(chart_from, chart_to)
  )
  expect_equal(layers[[2]]$data$label, "Z")
})

test_that("the line can use any column names", {
  renamed <- data.frame(when = line$window_end, rate = line$value)
  layers <- annotations$geom_perpendicular_arrow(
    ggplot2::aes(x = day), arrow_at, renamed, ggplot2::aes(when, rate),
    x_range = c(chart_from, chart_to)
  )
  expect_gt(layers[[1]]$data$tip_y, 0.7)
  expect_lt(layers[[1]]$data$tip_y, 0.72)
})

# A taller panel shows the same line steeper, so its normal leans further.
test_that("a taller panel leans the arrow more", {
  lean <- function(aspect) {
    layers <- annotations$geom_perpendicular_arrow(
      ggplot2::aes(x = day), arrow_at, line, line_mapping,
      x_range = c(chart_from, chart_to), aspect = aspect
    )
    as.numeric(as.Date("2026-09-30") - layers[[1]]$data$tail_x)
  }
  expect_gt(lean(1), lean(0.36))
})
