# Baseline --------------------------------------------------------------------

test_that("clean data passes every check", {
  result <- run_errors_preflight(errors_with_data())
  expect_true(result$passed)
  expect_length(result$failed, 0)
})

test_that("every check keeps its slug", {
  result <- run_errors_preflight(errors_with_data())
  expect_identical(
    purrr::map_chr(result$checks, "slug"),
    c("errors-complete", "sessions-complete", "no-blank-codes")
  )
})

test_that("a failed check raises a warning naming it", {
  errors <- dplyr::filter(errors_rows(), day != as.Date("2026-11-10"))
  env <- errors_with_data(errors = errors)
  expect_warning(
    suppressMessages(env$run_preflight_safety_check(
      NULL, errors_thresholds, today = errors_today
    )),
    "errors-complete"
  )
})

# errors-complete -------------------------------------------------------------

test_that("a missing error day fails, rather than reading as a quiet one", {
  errors <- dplyr::filter(errors_rows(), day != as.Date("2026-11-10"))
  result <- run_errors_preflight(errors_with_data(errors = errors))
  expect_identical(failed_slugs(result), "errors-complete")
  expect_match(check_named(result, "errors-complete")$details, "Nov 10")
})

test_that("errors stopping before GA's last day fail", {
  errors <- dplyr::filter(errors_rows(), day <= as.Date("2026-11-15"))
  result <- run_errors_preflight(errors_with_data(errors = errors))
  expect_identical(failed_slugs(result), "errors-complete")
})

# sessions-complete -----------------------------------------------------------

test_that("a missing session day fails", {
  sessions <- dplyr::filter(errors_sessions(), day != as.Date("2026-11-12"))
  result <- run_errors_preflight(errors_with_data(sessions = sessions))
  expect_identical(failed_slugs(result), "sessions-complete")
  expect_match(check_named(result, "sessions-complete")$details, "Nov 12")
})

test_that("a day with too few sessions fails", {
  sessions <- dplyr::mutate(
    errors_sessions(),
    sessions = ifelse(day == as.Date("2026-11-12"), 50, sessions)
  )
  result <- run_errors_preflight(errors_with_data(sessions = sessions))
  expect_identical(failed_slugs(result), "sessions-complete")
  expect_match(check_named(result, "sessions-complete")$details, "Too few")
})

# no-blank-codes --------------------------------------------------------------

test_that("a blank code fails", {
  errors <- dplyr::bind_rows(
    errors_rows(),
    tibble::tibble(day = as.Date("2026-11-10"), error_code = "(not set)",
                   form_id = "signin", events = 4)
  )
  result <- run_errors_preflight(errors_with_data(errors = errors))
  expect_identical(failed_slugs(result), "no-blank-codes")
})
