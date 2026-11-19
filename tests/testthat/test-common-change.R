# Relative change -------------------------------------------------------------

test_that("a change within half a percent is flat", {
  expect_identical(delta_direction(c(0.005, -0.005, 0)), rep("flat", 3))
})

test_that("an uncalculable change is flat", {
  expect_identical(delta_direction(NA_real_), "flat")
})

test_that("a change beyond half a percent has a direction", {
  expect_identical(delta_direction(c(0.006, -0.006)), c("up", "down"))
})

test_that("fmt_delta shows a rise with an up arrow and a sign", {
  expect_identical(fmt_delta(0.25), "▲ +25%")
})

test_that("fmt_delta shows a fall with a down arrow and a sign", {
  expect_identical(fmt_delta(-0.3), "▼ -30%")
})

test_that("fmt_delta is vectorised", {
  expect_identical(
    fmt_delta(c(0.25, -0.3)), c("▲ +25%", "▼ -30%")
  )
})

test_that("fmt_delta tells no change apart from an uncalculable one", {
  expect_identical(fmt_delta(c(0.001, NA)), c("no change", "-"))
})

# Percentage points -----------------------------------------------------------

test_that("a rate change within a tenth of a point is flat", {
  expect_identical(
    delta_direction(c(0.0005, -0.0005, 0), points = TRUE), rep("flat", 3)
  )
})

test_that("a rate change beyond a tenth of a point has a direction", {
  expect_identical(
    delta_direction(c(0.0006, -0.0006), points = TRUE), c("up", "down")
  )
})

test_that("a rate change of half a percent is not flat in points", {
  expect_identical(delta_direction(0.005, points = TRUE), "up")
})

test_that("an uncalculable rate change is flat", {
  expect_identical(delta_direction(NA_real_, points = TRUE), "flat")
})

test_that("fmt_delta shows a rise in points", {
  expect_identical(fmt_delta(0.012, points = TRUE), "▲ +1.2 pts")
})

test_that("fmt_delta shows a fall in points", {
  expect_identical(fmt_delta(-0.034, points = TRUE), "▼ -3.4 pts")
})

test_that("fmt_delta in points tells no change apart from a dash", {
  expect_identical(
    fmt_delta(c(0.0001, NA), points = TRUE), c("no change", "-")
  )
})

# Colours ---------------------------------------------------------------------

test_that("every direction has a colour", {
  expect_setequal(names(delta_colours), c("up", "down", "flat"))
})

# Verdicts --------------------------------------------------------------------

test_that("a rise is good news by default", {
  expect_identical(change_verdict(c(0.2, -0.2)), c("good", "bad"))
})

test_that("where down is good, a rise is bad news", {
  expect_identical(change_verdict(c(0.2, -0.2), good = "down"), c("bad", "good"))
})

test_that("a flat, uncalculable or unmeaningful change is flat", {
  expect_identical(
    change_verdict(c(0.001, NA, 0.2), meaningful = c(TRUE, TRUE, FALSE)),
    rep("flat", 3)
  )
})

test_that("a metric with no good direction is always flat", {
  expect_identical(change_verdict(c(0.2, -0.2), good = NA), c("flat", "flat"))
})

test_that("change_html colours the change by its verdict", {
  expect_identical(
    change_html(0.25, good = "down"),
    sprintf('<span style="color: %s">▲ +25%%</span>', delta_colours[["down"]])
  )
})

test_that("change_html can word the change itself", {
  expect_match(change_html(Inf, good = "down", text = "▲ from 0"),
               "▲ from 0</span>", fixed = TRUE)
})

test_that("colour_changes colours each row by its verdict, and skips NA", {
  table <- tibble::tibble(change = c("a", "b", "c", "d"),
                          verdict = c("good", "bad", "flat", NA)) |>
    gt::gt() |>
    colour_changes("change", "verdict")
  html <- as.character(gt::as_raw_html(table, inline_css = TRUE))
  for (colour in verdict_colours) {
    expect_match(html, paste0("color: ", colour), ignore.case = TRUE)
  }
  expect_length(table[["_styles"]]$rownum, 3L)
})
