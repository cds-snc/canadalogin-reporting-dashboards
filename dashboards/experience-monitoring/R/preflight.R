#' Preflight data-source validation.
#'
#' Runs in the dashboard's setup chunk, before any figure or table query. Prints
#' a numbered checklist and returns the result. A failed check raises a banner
#' across the top of the dashboard rather than stopping the render.
#'
#' Source common/setup.R and this dashboard's R/metrics.R first, and define the
#' help_stream_* constants. Expects an open `con`, dplyr/dbplyr/tidyr and glue.

run_preflight_safety_check <- function(con,
                                       today = Sys.Date(),
                                       ga_export_lag_days = 2L,
                                       lookback_days = 14L,
                                       min_task_success_rate = 0.02) {

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

  # Date anchors ---------------------------------------------------------------

  # GA runs on a deliberate ~2-day lag, so as_of is the newest day to expect.
  as_of <- today - ga_export_lag_days
  freshness_start <- as_of - (lookback_days - 1L)

  # The span the per-party table reports on; rp-lookup sweeps the same span.
  lookup_start <- as_of - (long_window_days - 1L)
  long_window <- as.character(long_window_days)

  # One query over the widest span any check needs; the checks then run in R.
  raw <- tbl(
    con, in_schema("google_analytics", "funnel_metrics_current")
  ) |>
    filter(
      metric_id == "task_success",
      funnel_id %in% !!required_funnels,
      !window_incomplete,
      as.Date(window_end) >= as.Date(!!as.character(lookup_start)),
      as.Date(window_end) <= as.Date(!!as.character(as_of))
    ) |>
    select(funnel_id, window_days, breakdown_value, window_end, value) |>
    collect() |>
    mutate(window_end = as.Date(window_end))

  # Check 1 - GA freshness (1-day rows, no gaps over the lookback) --------------

  freshness_problems <- character()
  expected_days <- seq(freshness_start, as_of, by = "day")
  for (funnel in required_funnels) {
    days_present <- raw |>
      filter(funnel_id == funnel,
             window_days == "1",
             breakdown_value == "RESERVED_TOTAL",
             window_end >= freshness_start) |>
      pull(window_end) |>
      unique()
    missing <- expected_days[!expected_days %in% days_present]
    if (length(missing) > 0) {
      # Capped: the full list would overflow the banner.
      shown <- format_date(utils::head(missing, 5))
      freshness_problems <- c(
        freshness_problems,
        glue("{funnel}: missing 1-day rows for {length(missing)} day(s), ",
             "including {glue_collapse(shown, sep = ', ')}")
      )
    }
  }

  record_check(
    "ga-freshness",
    glue("GA freshness - 1-day rows through {format_date(as_of)} ",
         "with no gaps over the last {lookback_days} days"),
    passed = length(freshness_problems) == 0,
    details = if (length(freshness_problems) == 0) {
      glue("funnel_metrics_current is current through {format_date(as_of)} ",
           "for {glue_collapse(required_funnels, sep = ', ')}")
    } else {
      freshness_problems
    }
  )

  # Check 2 - required funnels present ------------------------------------------

  funnels_present <- unique(raw$funnel_id)
  missing_funnels <- setdiff(required_funnels, funnels_present)

  record_check(
    "funnels-present",
    "Required funnels present",
    passed = length(missing_funnels) == 0,
    details = if (length(missing_funnels) == 0) {
      glue("all required funnels found: ",
           "{glue_collapse(required_funnels, sep = ', ')}")
    } else {
      glue("missing required funnel(s): ",
           "{glue_collapse(missing_funnels, sep = ', ')}")
    }
  )

  # Check 3 - relying-party lookup complete -------------------------------------
  #
  # Aliases are added to the source sheet by hand, so a GA rp_name can appear
  # before anyone records it. Left join and check for nulls: an inner join
  # would silently drop the party's rows.
  data_parties <- raw |>
    filter(breakdown_value != "RESERVED_TOTAL",
           !breakdown_value %in% ga_excluded_rp_names) |>
    pull(breakdown_value) |>
    unique()

  labelled <- label_relying_parties(con, data_parties)
  unmapped <- data_parties[is.na(labelled$service_name)]

  record_check(
    "rp-lookup",
    "Relying-party lookup complete - every party in the data is labelled",
    passed = length(unmapped) == 0,
    details = if (length(unmapped) == 0) {
      glue("{length(data_parties)} relying part",
           "{if (length(data_parties) == 1) 'y' else 'ies'} in the data, ",
           "all resolved through rp.alias to ",
           "{length(unique(labelled$service_name))} service",
           "{if (length(unique(labelled$service_name)) == 1) '' else 's'}")
    } else {
      glue("GA rp_name(s) with no row in rp.alias: ",
           "{glue_collapse(unmapped, sep = ', ')}; ",
           "add them to the CanadaLogin Relying Parties sheet")
    }
  )

  # Check 4 - long window present -----------------------------------------------
  #
  # Without it the table's rate, counts and comparison all read n/a at once,
  # which looks like a broken page rather than a late pipeline run.
  window_problems <- character()
  for (funnel in required_funnels) {
    ends_present <- raw |>
      filter(funnel_id == funnel,
             window_days == long_window,
             breakdown_value == "RESERVED_TOTAL") |>
      pull(window_end) |>
      unique()

    if (!as_of %in% ends_present) {
      newest <- if (length(ends_present) == 0) {
        glue("none in the last {long_window} days")
      } else {
        glue("newest is {format_date(max(ends_present))}")
      }
      window_problems <- c(
        window_problems,
        glue("{funnel}: no complete {long_window}-day window ending ",
             "{format_date(as_of)} ({newest})")
      )
    }
  }

  record_check(
    "long-window",
    glue("Long window present - complete {long_window}-day window ending ",
         "{format_date(as_of)}"),
    passed = length(window_problems) == 0,
    details = if (length(window_problems) == 0) {
      glue("{long_window}-day window current through {format_date(as_of)} ",
           "for {glue_collapse(required_funnels, sep = ', ')}")
    } else {
      window_problems
    }
  )

  # Check 5 - ratio steps present in funnels_current -----------------------------
  #
  # Counts are read by step name, so a step renamed upstream returns no rows
  # rather than an error, and reads as a true zero. Checked over all history:
  # an exit step nobody reached in the current span is legitimately absent.
  steps_seen <- tbl(
    con, in_schema("google_analytics", "funnels_current")
  ) |>
    filter(funnel_id %in% !!required_funnels) |>
    distinct(funnel_id, funnelstepname) |>
    collect()

  step_problems <- character()
  for (funnel in required_funnels) {
    declared <- funnel_ratio_steps[[funnel]]
    if (is.null(declared)) {
      step_problems <- c(
        step_problems,
        glue("{funnel}: no ratio steps declared in funnel_ratio_steps")
      )
      next
    }
    present <- steps_seen$funnelstepname[steps_seen$funnel_id == funnel]
    missing_steps <- setdiff(unname(declared), present)
    if (length(missing_steps) > 0) {
      step_problems <- c(
        step_problems,
        glue("{funnel}: step(s) not found in funnels_current: ",
             "{glue_collapse(missing_steps, sep = ', ')}; ",
             "check the funnel YAML against funnel_ratio_steps in R/metrics.R")
      )
    }
  }

  record_check(
    "ratio-steps",
    "Ratio steps present - task success numerator and denominator readable",
    passed = length(step_problems) == 0,
    details = if (length(step_problems) == 0) {
      glue("entry and exit steps found for ",
           "{glue_collapse(required_funnels, sep = ', ')}")
    } else {
      step_problems
    }
  )

  # Check 6 - help site stream reporting -----------------------------------------
  #
  # Nothing else on the dashboard reads page_traffic, so a stream that stopped
  # reporting would render an empty tab rather than raise anything.
  help_days <- tbl(con, in_schema("google_analytics", "page_traffic")) |>
    filter(
      streamid == !!help_stream_id,
      as.Date(date) >= as.Date(!!as.character(freshness_start)),
      as.Date(date) <= as.Date(!!as.character(as_of))
    ) |>
    distinct(date) |>
    collect() |>
    pull(date) |>
    as.Date()

  help_missing <- expected_days[!expected_days %in% help_days]

  record_check(
    "help-freshness",
    glue("Help site stream reporting through {format_date(as_of)}"),
    passed = length(help_missing) == 0,
    details = if (length(help_missing) == 0) {
      glue("help site has page views on all {lookback_days} days ",
           "through {format_date(as_of)}")
    } else {
      glue("no help site page views for {length(help_missing)} day(s), ",
           "including ",
           "{glue_collapse(format_date(utils::head(help_missing, 5)), sep = ', ')}")
    }
  )

  # Check 7 - help site stream identity ------------------------------------------
  #
  # The tab filters on the stream id alone, so an id that came to mean a
  # different stream would report another site's pages under this heading.
  help_names <- tbl(con, in_schema("google_analytics", "page_traffic")) |>
    filter(streamid == !!help_stream_id, !is.na(streamname)) |>
    distinct(streamname) |>
    collect() |>
    pull(streamname)

  identity_ok <- identical(help_names, help_stream_name)

  record_check(
    "help-identity",
    "Help site stream identity - the stream id still names the help site",
    passed = identity_ok,
    details = if (identity_ok) {
      glue("stream {help_stream_id} is '{help_stream_name}'")
    } else if (length(help_names) == 0) {
      glue("stream {help_stream_id} has no named rows in page_traffic; ",
           "the stream fields may need backfilling again")
    } else {
      glue("stream {help_stream_id} reports as ",
           "{glue_collapse(sprintf(\"'%s'\", help_names), sep = ', ')}, ",
           "expected '{help_stream_name}'; check the stream constants in ",
           "the dashboard's setup chunk against the GA4 admin")
    }
  )

  # Check 8 - task success rate implausibly low ---------------------------------
  #
  # An exit step that stops reporting reads as 0%, not as missing, since
  # funnel_step_counts() coalesces an absent numerator to zero. A floor rather
  # than a zero test, because one stray event lifts a broken funnel off zero.
  # The 28-day window is tested in its own right: activeUsers de-duplicate
  # within a window, so it is not a blend of the 7-day ones.
  low_rates <- raw |>
    filter(
      breakdown_value == "RESERVED_TOTAL",
      !is.na(value),                       # NULL is undefined, not low
      value < min_task_success_rate,
      window_days == "7" | (window_days == long_window & window_end == as_of)
    )

  # Run only for a funnel that failed, so a passing render adds no query.
  step_coverage <- function(funnel) {
    steps <- funnel_ratio_steps[[funnel]]
    days <- tbl(
      con, in_schema("google_analytics", "funnels_current")
    ) |>
      filter(
        funnel_id == !!funnel,
        window_days == "1",
        breakdown_value == "RESERVED_TOTAL",
        !window_incomplete,
        funnelstepname %in% !!unname(steps),
        as.Date(window_end) >= as.Date(!!as.character(lookup_start)),
        as.Date(window_end) <= as.Date(!!as.character(as_of))
      ) |>
      distinct(funnelstepname, window_end) |>
      collect()

    vapply(
      steps,
      \(step) length(unique(days$window_end[days$funnelstepname == step])),
      integer(1)
    )
  }

  as_percent <- function(x) sprintf("%.2f%%", x * 100)

  rate_problems <- character()
  for (funnel in unique(low_rates$funnel_id)) {
    # A flat zero ties across windows; ordering keeps the wording stable
    # between renders and reports the day the funnel broke.
    hits <- low_rates[low_rates$funnel_id == funnel, ] |>
      arrange(value, window_end)
    worst <- hits[1, ]
    steps <- funnel_ratio_steps[[funnel]]
    days_seen <- step_coverage(funnel)

    rate_problems <- c(
      rate_problems,
      glue(
        "{funnel}: {nrow(hits)} window(s) below ",
        "{as_percent(min_task_success_rate)}, worst {as_percent(worst$value)} ",
        "on the {worst$window_days}-day window ending ",
        "{format_date(worst$window_end)}; over the last {long_window_days} days ",
        "'{steps[['numerator']]}' reports on {days_seen[['numerator']]} day(s) ",
        "against {days_seen[['denominator']]} for '{steps[['denominator']]}'; ",
        "a rate this low means the funnel stopped reporting, not that users ",
        "stopped succeeding"
      )
    )
  }

  record_check(
    "plausibility",
    glue("Task success plausibility - no funnel below ",
         "{as_percent(min_task_success_rate)} on a displayed window"),
    passed = length(rate_problems) == 0,
    details = if (length(rate_problems) == 0) {
      glue("every 7-day window in the last {long_window_days} days, and the ",
           "{long_window_days}-day window ending {format_date(as_of)}, is at ",
           "or above {as_percent(min_task_success_rate)} for ",
           "{glue_collapse(required_funnels, sep = ', ')}")
    } else {
      rate_problems
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
