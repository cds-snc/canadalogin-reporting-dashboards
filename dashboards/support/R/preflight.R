#' Preflight data-source validation.
#'
#' Runs in the dashboard's setup chunk. Prints a checklist and returns the
#' result. A failed check raises a banner instead of stopping the render. The
#' banner and status writers are shared, in common/preflight.R.
#'
#' Source common/setup.R and R/metrics.R first. Expects an open `con`.

run_preflight_safety_check <- function(con, today = Sys.Date()) {

  # Helpers --------------------------------------------------------------------

  # "Sun Aug 10", or "no data" for the non-finite max() an empty table yields.
  format_date <- function(d) {
    labels <- rep("no data", length(d))
    finite <- is.finite(as.numeric(d))
    labels[finite] <- format(as.Date(d[finite], origin = "1970-01-01"),
                             "%a %b %d")
    labels
  }

  # The Saturday on or before `d`, which is a reported week's last day.
  last_saturday <- function(d) d - (as.integer(format(d, "%u")) + 1L) %% 7L

  # Accumulated so every check runs, rather than stopping at the first problem.
  # An `advisory` check is reported in the checklist and but doesn't fail.
  checks <- list()
  record_check <- function(slug, title, passed, details = character(),
                           advisory = FALSE) {
    checks[[length(checks) + 1L]] <<- list(
      slug = slug, title = as.character(title), passed = passed,
      details = as.character(details), advisory = advisory
    )
  }

  weeks <- call_weeks(con)
  topics <- call_topics(con)
  newest_week_end <- call_centre_data_through(con)

  # Check 1 - call centre freshness --------------------------------------------
  #
  # The expectation steps forward a week on the Thursday, the render day. A
  # render that cannot see the due week is republishing last week's numbers.

  due_week_end <- last_saturday(today - call_centre_report_due_days)
  fresh <- is.finite(newest_week_end) && newest_week_end >= due_week_end

  record_check(
    "call-centre-freshness",
    glue("The newest reported call centre week must be the one ending ",
         "{format_date(due_week_end)}"),
    passed = fresh,
    details = if (!is.finite(newest_week_end)) {
      "weekly_activity_report has no weeks in it"
    } else if (fresh) {
      glue("weekly_activity_report reports through ",
           "{format_date(newest_week_end)}")
    } else {
      behind <- as.integer(due_week_end - newest_week_end) %/% 7L
      glue("weekly_activity_report reports through ",
           "{format_date(newest_week_end)}, {behind} week(s) behind; the ",
           "report for the week ending {format_date(due_week_end)} has not ",
           "landed yet")
    }
  )

  # Check 2 - call centre continuity --------------------------------------------
  #
  # Did we miss uploading the data on a week?

  gaps <- weeks$week[-1] - weeks$week[-nrow(weeks)]
  irregular <- weeks$week[-1][gaps != 7L]
  duplicated_weeks <- weeks$week[duplicated(weeks$week)]

  record_check(
    "call-centre-continuity",
    "Reported weeks must run consecutively, one row each",
    passed = length(irregular) == 0 && length(duplicated_weeks) == 0,
    details = c(
      if (length(irregular) > 0) {
        glue("a gap or overlap before the week starting ",
             "{glue_collapse(format_date(irregular), sep = ', ')}")
      },
      if (length(duplicated_weeks) > 0) {
        glue("more than one row for the week starting ",
             "{glue_collapse(format_date(duplicated_weeks), sep = ', ')}")
      },
      if (length(irregular) == 0 && length(duplicated_weeks) == 0) {
        glue("{nrow(weeks)} consecutive weeks from ",
             "{format_date(min(weeks$week))}")
      }
    )
  )

  # Check 3 - topic coverage ----------------------------------------------------
  #
  # Is there a week without any call topic coverage? That's a mistake.

  newest_week <- weeks |> dplyr::slice_max(week_end, n = 1, with_ties = FALSE)
  newest_week_topics <- topics_in_weeks(topics, newest_week)

  record_check(
    "topic-coverage",
    "The newest reported week must have topic records",
    passed = nrow(newest_week_topics) > 0,
    details = glue("{dplyr::n_distinct(newest_week_topics$call_no)} call(s) with ",
                   "topics in the week ending {format_date(newest_week_end)}")
  )

  # Check 4 - topic categories --------------------------------------------------
  #
  # We classify the call topics both manually and via regular expressions
  # If there's a new topic that didn't match, we flag it.

  uncategorised <- topics |>
    dplyr::filter(category == "Other") |>
    dplyr::count(topic, sort = TRUE)

  record_check(
    "topic-categories",
    "Every call topic must fall into a named category",
    passed = nrow(uncategorised) == 0,
    details = if (nrow(uncategorised) == 0) {
      glue("{dplyr::n_distinct(topics$topic)} distinct topic(s), all categorised")
    } else {
      glue("topic(s) matched by neither the table nor the patterns, add them ",
           "to {topic_categories_file}: ",
           "{glue_collapse(glue('\"{uncategorised$topic}\" ({uncategorised$n})'), sep = '; ')}")
    }
  )

  # Check 5 - topic duplicates ------------------------------------------------------
  #
  # Ensure no duplicates in the topic lookup table, which would cause double-counting,
  # and no category with rows giving both types, which draws as troubleshooting.

  squished <- stringr::str_squish(topic_lookup_rows$topic)
  repeated <- unique(squished[duplicated(squished)])

  typed <- unique(topic_lookup_rows[c("category", "type")])
  mixed <- unique(typed$category[duplicated(typed$category)])

  problems <- c(
    if (length(repeated) > 0) {
      glue("more than one row for: ",
           "{glue_collapse(glue('\"{repeated}\"'), sep = '; ')}")
    },
    if (length(mixed) > 0) {
      glue("more than one type for: ",
           "{glue_collapse(glue('\"{mixed}\"'), sep = '; ')}")
    }
  )

  record_check(
    "topic-lookup",
    glue("No topic may have more than one row, and no category more than one type, ",
         "in {topic_categories_file}"),
    passed = length(problems) == 0,
    advisory = TRUE,
    details = if (length(problems) > 0) {
      glue_collapse(problems, sep = "; ")
    } else {
      glue("{length(topic_lookup)} row(s), no topic repeated, one type per category")
    }
  )

  # Check 6 - call rate coverage ------------------------------------------------
  #
  # Ensure that we're not missing any data from the sign-in dataset, so we don't
  # accidentally calculate too high of a rate

  uncovered <- call_rate(recent_weeks(weeks, newest_week_end),
                         daily_active_users(con)) |>
    dplyr::filter(is.na(calls_per_100))

  record_check(
    "call-rate-coverage",
    glue("Sign-in data must cover every day of the last {summary_window_weeks} ",
         "reported weeks"),
    passed = nrow(uncovered) == 0,
    details = if (nrow(uncovered) == 0) {
      glue("ibm_verify.auth_total_logins covers all {summary_window_weeks * 7} ",
           "days through {format_date(newest_week_end)}")
    } else {
      glue("no calls-per-100-active-users rate for the week(s) starting ",
           "{glue_collapse(format_date(uncovered$week), sep = ', ')}")
    }
  )

  # Check 7 - PSOM freshness ----------------------------------------------------

  snapshot <- psom_snapshot_date(con)
  record_check(
    "psom-freshness",
    glue("The newest PSOM snapshot must be dated on or after ",
         "{format_date(today - psom_lag_days)}"),
    passed = is.finite(snapshot) && snapshot >= today - psom_lag_days,
    details = glue("jira.psom newest snapshot is {format_date(snapshot)}, ",
                   "{nrow(psom_latest(con))} ticket(s)")
  )

  # Check 8 - plausibility ------------------------------------------------------
  #
  # The report is typed into a spreadsheet, so the columns are checked against
  # each other: a week that answered more calls than it accepted was mistyped.

  implausible <- weeks |>
    dplyr::filter(
      calls_answered + calls_abandoned > calls_accepted |
        test_calls > calls_accepted |
        calls < 0
    )

  record_check(
    "plausibility",
    "Answered plus abandoned calls must not exceed accepted, nor test calls",
    passed = nrow(implausible) == 0,
    details = if (nrow(implausible) == 0) {
      "every reported week adds up"
    } else {
      glue("week starting {format_date(implausible$week)}: ",
           "{implausible$calls_accepted} accepted, {implausible$calls_answered} ",
           "answered, {implausible$calls_abandoned} abandoned, ",
           "{implausible$test_calls} test")
    }
  )

  for (i in seq_along(checks)) {
    check <- checks[[i]]
    mark <- if (isTRUE(check$advisory)) "NOTE" else if (check$passed) "PASS" else "FAIL"
    message(glue("[{mark}] {i}. {check$slug}: {check$title}"))
    for (detail in check$details) message(glue("       {detail}"))
  }

  failed_checks <- purrr::keep(checks, \(check) !check$passed && !isTRUE(check$advisory))
  if (length(failed_checks) > 0) {
    failed_slugs <- purrr::map_chr(failed_checks, "slug")
    warning(
      glue("Data quality check failed: ",
           "{glue_collapse(failed_slugs, sep = ', ', last = ' and ')} ",
           "did not hold (see the checklist above). The dashboard will still ",
           "render, with a banner across the top; do not publish it."),
      call. = FALSE
    )
  }

  invisible(list(
    passed = length(failed_checks) == 0,
    checks = checks,
    failed = failed_checks
  ))
}
