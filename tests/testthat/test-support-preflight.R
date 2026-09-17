support <- load_dashboard("support")

# Baseline --------------------------------------------------------------------

test_that("clean data passes every check", {
  result <- run_support_preflight(support_with_data())
  expect_true(result$passed)
  expect_length(result$failed, 0)
})

test_that("every check keeps its slug", {
  result <- run_support_preflight(support_with_data())
  expect_identical(
    purrr::map_chr(result$checks, "slug"),
    c("call-centre-freshness", "call-centre-continuity", "topic-coverage",
      "topic-categories", "topic-lookup", "call-rate-coverage", "psom-freshness",
      "plausibility")
  )
})

test_that("a failed check raises a warning naming it", {
  env <- support_with_data(psom = support_psom(as.Date("2026-08-01")))
  expect_warning(
    suppressMessages(env$run_preflight_safety_check(NULL, today = support_today)),
    "psom-freshness"
  )
})

# call-centre-freshness -------------------------------------------------------

test_that("freshness fails when the due week has not landed", {
  weeks <- support_weeks(through = as.Date("2026-09-05"))
  result <- run_support_preflight(support_with_data(weeks))
  expect_identical(failed_slugs(result), "call-centre-freshness")
})

test_that("freshness says how many weeks behind the data is", {
  weeks <- support_weeks(through = as.Date("2026-08-29"))
  result <- run_support_preflight(support_with_data(weeks))
  expect_match(check_named(result, "call-centre-freshness")$details,
               "2 week(s) behind", fixed = TRUE)
})

test_that("freshness accepts last week's data until the Thursday it is due", {
  weeks <- support_weeks(through = as.Date("2026-09-05"))
  env <- support_with_data(weeks)
  wednesday <- run_support_preflight(env, today = as.Date("2026-09-16"))
  thursday <- run_support_preflight(env, today = as.Date("2026-09-17"))
  expect_true(check_named(wednesday, "call-centre-freshness")$passed)
  expect_false(check_named(thursday, "call-centre-freshness")$passed)
})

# call-centre-continuity ------------------------------------------------------

test_that("continuity fails on a missing week", {
  weeks <- support_weeks()[-4, ]
  result <- run_support_preflight(support_with_data(weeks))
  expect_identical(failed_slugs(result), "call-centre-continuity")
})

test_that("continuity fails on a week reported twice", {
  weeks <- support_weeks()
  weeks <- dplyr::arrange(dplyr::bind_rows(weeks, weeks[4, ]), week)
  result <- run_support_preflight(support_with_data(weeks))
  expect_identical(failed_slugs(result), "call-centre-continuity")
})

# topic-coverage --------------------------------------------------------------

test_that("topic coverage fails when the newest week has no topics", {
  weeks <- support_weeks()
  topics <- support_topics(weeks[-nrow(weeks), ], env = support)
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_identical(failed_slugs(result), "topic-coverage")
})

# topic-categories ------------------------------------------------------------

test_that("topic categories fail on a topic nothing matches", {
  weeks <- support_weeks()
  topics <- support_topics(weeks, topic = "Zzz unheard of", env = support)
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_identical(failed_slugs(result), "topic-categories")
})

test_that("topic categories name the unmatched topic and its count", {
  weeks <- support_weeks(1)
  topics <- support_topics(weeks, topic = "Zzz unheard of", env = support)
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_match(check_named(result, "topic-categories")$details,
               '"Zzz unheard of" (7)', fixed = TRUE)
})

test_that("topic categories pass a topic only the patterns catch", {
  weeks <- support_weeks()
  topics <- support_topics(weeks, topic = "How to Do Something New", env = support)
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_true(check_named(result, "topic-categories")$passed)
})

# topic-lookup ----------------------------------------------------------------

test_that("a repeated lookup row is reported but does not fail the render", {
  env <- support_with_data()
  rows <- env$topic_lookup_rows
  stub(env, topic_lookup_rows = rbind(rows, rows[1, ]))
  result <- run_support_preflight(env)
  expect_false(check_named(result, "topic-lookup")$passed)
  expect_true(result$passed)
})

test_that("the lookup table as committed has no repeated topic", {
  result <- run_support_preflight(support_with_data())
  expect_true(check_named(result, "topic-lookup")$passed)
})

# call-rate-coverage ----------------------------------------------------------

test_that("call rate coverage fails when a recent day has no sign-in data", {
  weeks <- support_weeks()
  users <- dplyr::filter(support_users(weeks), date != as.Date("2026-09-09"))
  result <- run_support_preflight(support_with_data(weeks, users = users))
  expect_identical(failed_slugs(result), "call-rate-coverage")
})

test_that("call rate coverage ignores a gap older than the summary window", {
  weeks <- support_weeks()
  users <- dplyr::filter(support_users(weeks), date != min(weeks$week))
  result <- run_support_preflight(support_with_data(weeks, users = users))
  expect_true(result$passed)
})

# psom-freshness --------------------------------------------------------------

test_that("PSOM freshness fails on a snapshot older than the lag", {
  result <- run_support_preflight(
    support_with_data(psom = support_psom(support_today - 9L))
  )
  expect_identical(failed_slugs(result), "psom-freshness")
})

test_that("PSOM freshness passes a snapshot exactly at the lag", {
  result <- run_support_preflight(
    support_with_data(psom = support_psom(support_today - 8L))
  )
  expect_true(result$passed)
})

# plausibility ----------------------------------------------------------------

test_that("plausibility fails when answered and abandoned exceed accepted", {
  weeks <- support_weeks()
  weeks$calls_answered[3] <- 30
  result <- run_support_preflight(support_with_data(weeks))
  expect_identical(failed_slugs(result), "plausibility")
})

test_that("plausibility fails when test calls exceed accepted", {
  weeks <- support_weeks()
  weeks$test_calls[3] <- 25
  result <- run_support_preflight(support_with_data(weeks))
  expect_true("plausibility" %in% failed_slugs(result))
})
