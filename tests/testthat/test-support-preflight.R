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
      "psom-sla", "plausibility", "app-login-freshness", "escalation-field",
      "daily-share-sample", "ratio-stretch-sample")
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
    support_with_data(psom = support_psom(support_today - 2L),
                      psom_sla = support_psom_sla(support_today - 2L))
  )
  expect_identical(failed_slugs(result), "psom-freshness")
})

test_that("PSOM freshness passes a snapshot exactly at the lag", {
  result <- run_support_preflight(
    support_with_data(psom = support_psom(support_today - 1L),
                      psom_sla = support_psom_sla(support_today - 1L))
  )
  expect_true(result$passed)
})

# psom-sla --------------------------------------------------------------------

test_that("PSOM SLA fails when its newest snapshot is behind the board's", {
  result <- run_support_preflight(
    support_with_data(psom_sla = support_psom_sla(support_today - 2L))
  )
  expect_identical(failed_slugs(result), "psom-sla")
})

test_that("PSOM SLA fails when a started clock has no elapsed time", {
  sla <- support_psom_sla()
  sla$elapsed_hours[2] <- NA
  result <- run_support_preflight(support_with_data(psom_sla = sla))
  expect_identical(failed_slugs(result), "psom-sla")
  expect_match(check_named(result, "psom-sla")$details[2], "PSOM-1")
})

test_that("PSOM SLA ignores a clock the page does not report", {
  sla <- support_psom_sla()
  sla$sla[sla$sla == "escalated_to_pt"] <- "some_other_clock"
  sla$elapsed_hours[sla$sla == "some_other_clock"] <- NA
  result <- run_support_preflight(support_with_data(psom_sla = sla))
  expect_true(result$passed)
})

test_that("PSOM SLA ignores a clock that never started", {
  sla <- support_psom_sla()
  sla$breached[2] <- NA
  sla$elapsed_hours[2] <- NA
  result <- run_support_preflight(support_with_data(psom_sla = sla))
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

# app-login-freshness ---------------------------------------------------------

test_that("app sign-in freshness fails when the last complete month is short", {
  logins <- support_app_logins(through = as.Date("2026-08-30"))
  result <- run_support_preflight(support_with_data(logins = logins))
  expect_identical(failed_slugs(result), "app-login-freshness")
})

test_that("app sign-in freshness passes data through the month's last day", {
  logins <- support_app_logins(through = as.Date("2026-08-31"))
  result <- run_support_preflight(support_with_data(logins = logins))
  expect_true(check_named(result, "app-login-freshness")$passed)
})

test_that("app sign-in freshness waits a day into the month for the last one", {
  logins <- support_app_logins(through = as.Date("2026-08-31"))
  env <- support_with_data(logins = logins)
  first <- run_support_preflight(env, today = as.Date("2026-09-01"))
  expect_true(check_named(first, "app-login-freshness")$passed)
})

# escalation-field ------------------------------------------------------------

test_that("escalation field fails on a blank transaction type in the newest week", {
  weeks <- support_weeks()
  topics <- support_topics(weeks, env = support)
  topics$transaction_type[nrow(topics)] <- NA
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_identical(failed_slugs(result), "escalation-field")
})

test_that("escalation field fails on an empty transaction type", {
  weeks <- support_weeks()
  topics <- support_topics(weeks, env = support)
  topics$transaction_type[nrow(topics)] <- ""
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_identical(failed_slugs(result), "escalation-field")
})

test_that("escalation field ignores a blank older than the newest week", {
  weeks <- support_weeks()
  topics <- support_topics(weeks, env = support)
  topics$transaction_type[1] <- NA
  result <- run_support_preflight(support_with_data(weeks, topics = topics))
  expect_true(result$passed)
})

# daily-share-sample ----------------------------------------------------------

test_that("a thin daily share sample is reported but does not fail the render", {
  logins <- support_app_logins(apps = "App 1")
  result <- run_support_preflight(
    support_with_data(logins = logins, internal = support_internal("App 1"))
  )
  expect_false(check_named(result, "daily-share-sample")$passed)
  expect_true(result$passed)
})

test_that("the daily share sample counts application-months", {
  result <- run_support_preflight(support_with_data())
  expect_match(check_named(result, "daily-share-sample")$details,
               "12 application-month(s)", fixed = TRUE)
})

# ratio-stretch-sample --------------------------------------------------------

test_that("a short history is reported but does not fail the render", {
  # Five weeks hold two four-week stretches, short of the minimum
  weeks <- support_weeks(n = 5L)
  result <- run_support_preflight(support_with_data(weeks))
  expect_false(check_named(result, "ratio-stretch-sample")$passed)
  expect_true(result$passed)
})

test_that("the stretch sample counts the stretches in the span", {
  weeks <- support_weeks(n = 12L, through = as.Date("2026-09-12"))
  result <- run_support_preflight(support_with_data(weeks))
  check <- check_named(result, "ratio-stretch-sample")
  expect_true(check$passed)
  expect_match(check$details, "8 four-week stretch(es)", fixed = TRUE)
  expect_match(check$details, "Jul 01", fixed = TRUE)
})
