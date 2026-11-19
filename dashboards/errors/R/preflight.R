#' Preflight data-source validation: prints a checklist and returns the
#' result. A failed check raises a banner instead of stopping the render.
#'
#' The checks guard against quiet failures: a missing day reads as a calm one,
#' and a broken session count divides every rate by too little. They cover the
#' weeks the page reads. Needs R/metrics.R and an open `con`.

run_preflight_safety_check <- function(con, thresholds, today = toronto_today()) {

  # Helpers --------------------------------------------------------------------

  format_date <- function(d) format(as.Date(d), "%a %b %d")

  list_dates <- function(d) glue_collapse(format_date(d), sep = ", ")

  # Accumulated so every check runs, rather than stopping at the first problem.
  checks <- list()
  record_check <- function(slug, title, passed, details = character()) {
    checks[[length(checks) + 1L]] <<- list(
      slug = slug, title = as.character(title), passed = passed,
      details = as.character(details)
    )
  }

  week <- report_week(today)
  from <- min(weeks_needed(week, thresholds))
  through <- week$through
  expected <- seq(from, through, by = "day")

  errors <- dplyr::filter(error_rows(con, from), day <= through)
  sessions <- dplyr::filter(session_days(con, from), day <= through)

  # Check 1 - errors complete ----------------------------------------------------
  #
  # A missing day is not a zero day: it would hide a spike.

  missing_errors <- expected[!expected %in% errors$day]

  record_check(
    "errors-complete",
    glue("google_analytics.error_events must report every day from ",
         "{format_date(from)} through {format_date(through)}"),
    passed = length(missing_errors) == 0,
    details = if (length(missing_errors) == 0) {
      glue("{length(expected)} days, none missing")
    } else {
      glue("No rows for {list_dates(missing_errors)}")
    }
  )

  # Check 2 - sessions complete and believable ------------------------------------
  #
  # A broken export divides by a tiny number and every rate spikes. Weekends
  # run at about half a weekday, well above the floor.

  missing_sessions <- expected[!expected %in% sessions$day]
  floor <- thresholds$sessions_floor * stats::median(sessions$sessions)
  low <- dplyr::filter(sessions, sessions < floor)

  record_check(
    "sessions-complete",
    glue("google_analytics.property_traffic must report every day, none below ",
         "{scales::percent(thresholds$sessions_floor)} of the median day"),
    passed = length(missing_sessions) == 0 && nrow(low) == 0,
    details = c(
      if (length(missing_sessions) > 0) {
        glue("No sessions for {list_dates(missing_sessions)}")
      },
      if (nrow(low) > 0) {
        glue("Too few sessions on {list_dates(low$day)}")
      },
      if (length(missing_sessions) == 0 && nrow(low) == 0) {
        glue("{nrow(sessions)} days, every one present and believable")
      }
    )
  )

  # Check 3 - no blank codes -----------------------------------------------------
  #
  # The ETL drops these; if it stops, they would become one large fake code.

  blank <- errors |>
    dplyr::filter(is.na(error_code) | trimws(error_code) %in% c("", "(not set)"))

  record_check(
    "no-blank-codes",
    "No error event may have a blank code",
    passed = nrow(blank) == 0,
    details = if (nrow(blank) == 0) {
      "Every event has a code"
    } else {
      glue("{sum(blank$events)} events with a blank code, ",
           "on {dplyr::n_distinct(blank$day)} day(s)")
    }
  )

  for (i in seq_along(checks)) {
    check <- checks[[i]]
    mark <- if (check$passed) "PASS" else "FAIL"
    message(glue("[{mark}] {i}. {check$slug}: {check$title}"))
    for (detail in check$details) message(glue("       {detail}"))
  }

  failed_checks <- purrr::keep(checks, \(check) !check$passed)
  if (length(failed_checks) > 0) {
    failed_slugs <- purrr::map_chr(failed_checks, "slug")
    warning(
      glue("Data quality check failed: ",
           "{glue_collapse(failed_slugs, sep = ', ', last = ' and ')} ",
           "did not hold (see the checklist above). The dashboard will still ",
           "render, with a banner across the top."),
      call. = FALSE
    )
  }

  invisible(list(
    passed = length(failed_checks) == 0,
    checks = checks,
    failed = failed_checks
  ))
}
