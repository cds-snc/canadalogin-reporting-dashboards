#' Preflight data-source validation: prints a checklist and returns the
#' result. A failed check raises a banner instead of stopping the render.
#'
#' The checks guard against a quiet failure: a source goes empty and the
#' calendar shows a calm quarter. Needs R/metrics.R and an open `con`.

# How far behind today each source may be. mfa_activity and the event stream
# land yesterday's day by about 06:00 ET; GA is exported on a two-day lag.
source_lag_days <- c(sms = 2L, location = 2L, idle = 2L, errors = 3L)

run_preflight_safety_check <- function(con, thresholds, known_bot_days,
                                       today = toronto_today()) {

  # Helpers --------------------------------------------------------------------

  # "Sun Aug 10", or "no data" for the non-finite max() an empty table yields.
  format_date <- function(d) {
    labels <- rep("no data", length(d))
    finite <- is.finite(as.numeric(d))
    labels[finite] <- format(as.Date(d[finite], origin = "1970-01-01"),
                             "%a %b %d")
    labels
  }

  # Accumulated so every check runs, rather than stopping at the first problem.
  checks <- list()
  record_check <- function(slug, title, passed, details = character()) {
    checks[[length(checks) + 1L]] <<- list(
      slug = slug, title = as.character(title), passed = passed,
      details = as.character(details)
    )
  }

  through <- today - 1L
  from <- calendar_start(through)

  pairs <- account_pairs(con)
  series <- list(
    sms = dplyr::filter(sms_days(con), day <= through),
    location = dplyr::filter(sms_origin_days(con), day <= through),
    idle = account_days(pairs, through),
    errors = dplyr::filter(error_days(error_codes(con)), day <= through)
  )
  sources <- c(sms = "ibm_verify.mfa_activity",
               location = "ibm_verify_events_raw.mfa_activity",
               idle = "ibm_verify_events_raw",
               errors = "google_analytics.error_events")

  scored <- list(
    sms = score_sms(series$sms, thresholds),
    location = score_location(series$location, thresholds),
    idle = score_idle(series$idle, thresholds),
    errors = score_errors(series$errors, thresholds)
  )
  scores <- method_scores(scored)

  # Check 1 - freshness ---------------------------------------------------------

  newest <- purrr::map(series, \(rows) suppressWarnings(max(rows$day)))
  stale <- purrr::imap_lgl(newest, \(d, m) {
    !is.finite(d) || d < today - source_lag_days[[m]]
  })

  record_check(
    "freshness",
    "Each source must report a day within its usual lag",
    passed = !any(stale),
    details = purrr::imap_chr(newest, \(d, m) {
      glue("{sources[[m]]} through {format_date(d)} ",
           "(due {format_date(today - source_lag_days[[m]])} or later)")
    })
  )

  # Check 2 - no missing days ---------------------------------------------------
  #
  # A missing day is not a zero day. The span starts a baseline window before
  # the calendar, or where the source begins.

  holes <- purrr::imap(series, \(rows, m) {
    if (nrow(rows) == 0) return(as.Date(character()))
    start <- max(min(rows$day), from - thresholds$baseline_days)
    expected <- seq(start, max(rows$day), by = "day")
    expected[!expected %in% rows$day]
  })
  unparsed <- sum(pairs$accounts[is.na(pairs$first_day)])

  record_check(
    "no-missing-days",
    "Every source must report every day of the calendar and its baseline",
    passed = all(lengths(holes) == 0) && unparsed == 0,
    details = c(
      purrr::imap_chr(holes, \(days, m) {
        if (length(days) == 0) {
          glue("{sources[[m]]}: no missing days")
        } else {
          glue("{sources[[m]]}: no rows for ",
               "{glue_collapse(format_date(days), sep = ', ')}")
        }
      }),
      if (unparsed > 0) {
        glue("{unparsed} account(s) with a first sign-in time that did not ",
             "parse to a day")
      }
    )
  )

  # Check 3 - enough baseline ---------------------------------------------------
  #
  # A source too new to score is expected; an unscored day with a full window
  # of history behind it means too many recent days fired.

  thin <- purrr::imap(scored, \(rows, m) {
    rows |>
      dplyr::filter(day >= from, is.na(fired),
                    day - thresholds$baseline_days >= min(rows$day)) |>
      dplyr::pull(day)
  })

  record_check(
    "enough-baseline",
    glue("Every calendar day with {thresholds$baseline_days} days of history ",
         "must have enough unflagged baseline days"),
    passed = all(lengths(thin) == 0),
    details = purrr::imap_chr(thin, \(days, m) {
      label <- methods$label[methods$method == m]
      if (length(days) == 0) {
        glue("{label}: every such day scored")
      } else {
        glue("{label}: too few baseline days for ",
             "{glue_collapse(format_date(days), sep = ', ')}")
      }
    })
  )

  # Check 4 - known days still fire ----------------------------------------------
  #
  # Scored over full history, so a confirmed incident stays a regression test
  # after it leaves the calendar.

  expected <- known_bot_days |>
    tidyr::pivot_longer(-day, names_to = "method", values_to = "expected") |>
    dplyr::filter(expected) |>
    dplyr::left_join(scores, by = c("day", "method"))
  missed <- dplyr::filter(expected, is.na(fired) | !fired)

  record_check(
    "known-days-fire",
    "Every confirmed bot day must still fire on its expected methods",
    passed = nrow(missed) == 0,
    details = if (nrow(missed) == 0) {
      glue("{nrow(expected)} expected firing(s) across ",
           "{dplyr::n_distinct(expected$day)} day(s), all still fire")
    } else {
      glue("{methods$label[match(missed$method, methods$method)]} no longer ",
           "fires on {format_date(missed$day)}")
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
