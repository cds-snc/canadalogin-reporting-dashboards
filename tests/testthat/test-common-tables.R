# In-row bars -----------------------------------------------------------------

test_that("share_bar is as wide as the share", {
  expect_match(share_bar(0.42, "#4B2E83"), "width:42%", fixed = TRUE)
})

test_that("share_bar fills with the colour given", {
  expect_match(share_bar(0.5, "#4B2E83"), "background:#4B2E83", fixed = TRUE)
})

test_that("share_bar rounds to a whole percent", {
  expect_match(share_bar(0.126, "red"), "width:13%", fixed = TRUE)
})

test_that("share_bar draws a share over 1 at full width", {
  expect_match(share_bar(1.2, "red"), "width:100%", fixed = TRUE)
})

test_that("share_bar draws a negative share at zero width", {
  expect_match(share_bar(-0.1, "red"), "width:0%", fixed = TRUE)
})

test_that("share_bar keeps a sliver for a share that rounds to zero", {
  expect_match(share_bar(0.004, "red"), "width:0%;min-width:2px", fixed = TRUE)
})

test_that("share_bar draws nothing for a zero share", {
  expect_no_match(share_bar(0, "red"), "min-width", fixed = TRUE)
})

test_that("share_bar has no accessible name unless labelled", {
  expect_no_match(share_bar(0.5, "red"), "aria-label", fixed = TRUE)
})

test_that("share_bar names its share for screen readers when labelled", {
  bar <- share_bar(0.5, "red", label = "50% of errors")
  expect_match(bar, "role='img' aria-label='50% of errors'", fixed = TRUE)
})

test_that("share_bar escapes its label", {
  expect_match(share_bar(0.5, "red", label = "a 'b'"), "a &#39;b&#39;",
               fixed = TRUE)
})

test_that("share_bar is vectorised over share and fill", {
  bars <- share_bar(c(0.1, 0.9), c("red", "blue"))
  expect_length(bars, 2)
  expect_match(bars[2], "background:blue;width:90%", fixed = TRUE)
})
