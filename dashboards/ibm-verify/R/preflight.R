#' Preflight data-source validation.
#'
#' Runs in the dashboard's setup chunk. Prints a numbered checklist and returns
#' the result. A failed check raises a banner instead of stopping the render.
#' The banner and status writers are shared, in common/preflight.R.
#'
#' Source common/setup.R and R/metrics.R first. Expects an open `con`.

run_preflight_safety_check <- function(con,
                                       today = Sys.Date(),
                                       lookback_days = 14L,
                                       min_auth_success_rate = 0.50,
                                       collapse_to = 0.10,
                                       collapse_min_prior = 100) {

  # Helpers --------------------------------------------------------------------

  # "Sun Aug 10", or "no data" for the non-finite max() an empty table yields.
  format_date <- function(d) {
    labels <- rep("no data", length(d))
    finite <- is.finite(as.numeric(d))
    labels[finite] <- format(as.Date(d[finite], origin = "1970-01-01"),
                             "%a %b %d")
    labels
  }

  as_percent <- function(x) sprintf("%.0f%%", x * 100)

  # Accumulated so every check runs, rather than stopping at the first problem.
  checks <- list()
  record_check <- function(slug, title, passed, details = character()) {
    checks[[length(checks) + 1L]] <<- list(
      slug = slug, title = as.character(title), passed = passed,
      details = as.character(details)
    )
  }

  # Date anchors ---------------------------------------------------------------

  # Yesterday's rows land at about 06:00 ET, so this is the newest day to expect
  newest_expected <- today - ibm_verify_lag_days
  expected_days <- seq(newest_expected - (lookback_days - 1L), newest_expected,
                       by = "day")
  as_of <- ibm_verify_data_through(con)

  # Check 1 - freshness (daily rows through yesterday, no gaps) -----------------

  tables <- list(
    auth_total_logins = auth_totals(con),
    app_login_counts  = app_logins(con),
    mfa_activity      = mfa_activity(con)
  )

  freshness_problems <- character()
  for (name in names(tables)) {
    days_present <- unique(tables[[name]]$date)
    missing <- expected_days[!expected_days %in% days_present]
    if (length(missing) > 0) {
      # Capped: the full list would overflow the banner.
      shown <- format_date(utils::head(missing, 5))
      freshness_problems <- c(
        freshness_problems,
        glue("{name}: no rows for {length(missing)} day(s), including ",
             "{glue_collapse(shown, sep = ', ')}; newest day is ",
             "{format_date(max(days_present))}")
      )
    }
  }

  record_check(
    "verify-freshness",
    glue("All three tables must report through ",
         "{format_date(newest_expected)} with no gaps over the last ",
         "{lookback_days} days"),
    passed = length(freshness_problems) == 0,
    details = if (length(freshness_problems) == 0) {
      glue("{glue_collapse(names(tables), sep = ', ')} are all current ",
           "through {format_date(newest_expected)}")
    } else {
      freshness_problems
    }
  )

  # Check 2 - service coverage ---------------------------------------------------
  #
  # An application that quietly stops reporting while the tables stay current
  # renders as a service nobody used, not as a gap.
  service_window_sso_events <- function(as_of) {
    labelled_app_logins(con) |>
      in_window(as_of) |>
      group_by(service_name) |>
      summarise(sso_events = sum(total_logins), .groups = "drop")
  }

  current <- service_window_sso_events(as_of)
  prior <- service_window_sso_events(as_of - summary_window_days)

  went_quiet <- prior |>
    filter(sso_events > 0) |>
    anti_join(filter(current, sso_events > 0), by = "service_name")

  record_check(
    "service-coverage",
    glue("Every service with traffic in the preceding ",
         "{summary_window_days} days must still have traffic"),
    passed = nrow(went_quiet) == 0,
    details = if (nrow(went_quiet) == 0) {
      glue("{nrow(current)} service(s) reporting SSO events in the ",
           "{summary_window_days} days ending {format_date(as_of)}")
    } else {
      glue(
        "{went_quiet$service_name}: {format(went_quiet$sso_events, big.mark = ',')} ",
        "SSO event(s) in the preceding {summary_window_days} days and none since; ",
        "check whether the application stopped reporting or stopped being used"
      )
    }
  )

  # Check 3 - relying-party resolution -------------------------------------------
  #
  # Aliases are added to the source sheet by hand, so a name can appear in IBM
  # Verify before anyone records it
  unmapped <- labelled_app_logins(con) |>
    filter(is.na(service_name)) |>
    distinct(application_name) |>
    pull(application_name)

  applications <- unique(app_logins(con)$application_name)

  record_check(
    "rp-resolution",
    "Every application name must be labelled",
    passed = length(unmapped) == 0,
    details = if (length(unmapped) == 0) {
      glue("{length(applications)} application name(s) in app_login_counts, ",
           "all resolved through rp.alias to ",
           "{length(all_services(con))} service(s)")
    } else {
      glue("application name(s) with no row in rp.alias: ",
           "{glue_collapse(unmapped, sep = ', ')}; ",
           "add them to the CanadaLogin Relying Parties sheet")
    }
  )

  # Check 4 - plausibility --------------------------------------------------------
  #
  # We don't display success rate, because folks tend to confuse it with task success
  # rate. But we do confirm that it looks about right, because it can indicate a data
  # quality issue.

  window_totals <- auth_totals(con) |> in_window(as_of)
  attempts <- sum(window_totals$successful_logins + window_totals$failed_logins)
  success_rate <- if (attempts == 0) {
    NA_real_
  } else {
    sum(window_totals$successful_logins) / attempts
  }

  plausibility_problems <- character()
  if (is.na(success_rate)) {
    plausibility_problems <- c(
      plausibility_problems,
      glue("no authentication events at all in the {summary_window_days} days ",
           "ending {format_date(as_of)}")
    )
  } else if (success_rate < min_auth_success_rate) {
    plausibility_problems <- c(
      plausibility_problems,
      glue("CanadaLogin-wide authentication success rate is ",
           "{as_percent(success_rate)} over the {summary_window_days} days ",
           "ending {format_date(as_of)}, below the ",
           "{as_percent(min_auth_success_rate)} floor; the observed range is ",
           "roughly 75% to 87%, so a rate this low means something stopped ",
           "working rather than that users stopped succeeding")
    )
  }

  collapsed <- full_join(
    rename(current, now = sso_events), rename(prior, before = sso_events),
    by = "service_name"
  ) |>
    mutate(now = coalesce(now, 0), before = coalesce(before, 0)) |>
    filter(before >= collapse_min_prior, now <= collapse_to * before)

  if (nrow(collapsed) > 0) {
    plausibility_problems <- c(
      plausibility_problems,
      glue(
        "{collapsed$service_name}: SSO events fell from ",
        "{format(collapsed$before, big.mark = ',')} to ",
        "{format(collapsed$now, big.mark = ',')} against the preceding ",
        "{summary_window_days} days"
      )
    )
  }

  record_check(
    "plausibility",
    glue("Authentication success rate must be at or above ",
         "{as_percent(min_auth_success_rate)}, with no service's SSO events ",
         "collapsing"),
    passed = length(plausibility_problems) == 0,
    details = if (length(plausibility_problems) == 0) {
      glue("authentication success rate is {as_percent(success_rate)} over the ",
           "{summary_window_days} days ending {format_date(as_of)}, and no ",
           "service with {collapse_min_prior} or more SSO events in the ",
           "preceding window lost {as_percent(1 - collapse_to)} of them")
    } else {
      plausibility_problems
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
      glue("Preflight safety check failed: ",
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
