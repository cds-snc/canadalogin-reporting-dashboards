#' Preflight data-source validation.
#'
#' Runs in the dashboard's setup chunk, before any figure or table query. Prints
#' a numbered checklist and returns the result. A failed check raises a banner
#' across the top of the dashboard rather than stopping the render.
#'
#' Source common/setup.R and this dashboard's R/metrics.R first, and define the
#' help_stream_* constants. Expects an open `con`, dplyr/dbplyr/tidyr and glue.

# Unattributed share of one window's entry-step starters. `starters` has
# breakdown_value and activeusers. The denominator is GA's own RESERVED_TOTAL
# row, not the sum of the parties: activeUsers de-duplicate within a breakdown,
# so a user seen under two rp_names counts in both and the parties can sum a
# little over the total (about 1% for sign_in). NA when there is no total.
unattributed_share <- function(starters) {
  total <- starters$activeusers[starters$breakdown_value == "RESERVED_TOTAL"]
  if (length(total) != 1 || is.na(total) || total == 0) return(NA_real_)
  lost <- starters$activeusers[starters$breakdown_value %in% ga_excluded_rp_names]
  sum(lost) / total
}

# One funnel's attribution against `threshold`: "clear" at or under it, "onset"
# when over it and the window a week earlier was not (or is missing), "ongoing"
# when both are over it. NA when the current window has no total.
attribution_status <- function(share, prior_share, threshold) {
  if (is.na(share)) return(NA_character_)
  if (share <= threshold) return("clear")
  if (!is.na(prior_share) && prior_share > threshold) "ongoing" else "onset"
}

# The attributed parties whose starters fell most from `previous` to `current`
# (same columns as above), largest fall first. A party absent from `current`
# fell by all of its previous starters.
largest_starter_drops <- function(current, previous, n = 3L) {
  not_party <- c("RESERVED_TOTAL", ga_excluded_rp_names)
  is_party <- \(d) d[!d$breakdown_value %in% not_party, ]
  dplyr::full_join(
    is_party(previous), is_party(current),
    by = "breakdown_value", suffix = c("_previous", "_current")
  ) |>
    dplyr::mutate(
      activeusers_previous = dplyr::coalesce(activeusers_previous, 0),
      activeusers_current = dplyr::coalesce(activeusers_current, 0),
      drop = activeusers_previous - activeusers_current
    ) |>
    dplyr::filter(drop > 0) |>
    dplyr::arrange(dplyr::desc(drop), breakdown_value) |>
    utils::head(n)
}

# Total rows of `funnels` under `floor` among the rates the page shows: every
# 7-day window and the long window ending on as_of. A funnel charting the long
# window is judged on that alone, since a 7-day window of three people can read
# 0% in a normal week.
implausible_rates <- function(raw, funnels, as_of, floor) {
  long_window <- as.character(long_window_days)
  long_line <- funnels[vapply(funnels, funnel_line_days, integer(1)) ==
                         long_window_days]
  raw |>
    dplyr::filter(
      funnel_id %in% funnels,
      breakdown_value == "RESERVED_TOTAL",
      !is.na(value),                       # NULL is undefined, not low
      value < floor,
      (window_days == "7" & !funnel_id %in% long_line) |
        (window_days == long_window & window_end == as_of)
    )
}

# One problem line per funnel whose declared entry or exit step is missing from
# the step names seen in funnels_current, or not declared at all.
step_name_problems <- function(steps_seen, funnels,
                               entry_steps = funnel_entry_steps,
                               exit_steps = funnel_exit_steps) {
  declared <- list(entry = entry_steps, exit = exit_steps)
  lists <- c(entry = "funnel_entry_steps", exit = "funnel_exit_steps")
  problems <- character()
  for (funnel in funnels) {
    present <- steps_seen$funnelstepname[steps_seen$funnel_id == funnel]
    for (kind in names(declared)) {
      step <- declared[[kind]][[funnel]]
      if (is.null(step)) {
        problems <- c(problems,
                      glue("{funnel}: no {kind} step declared in {lists[[kind]]}"))
      } else if (!step %in% present) {
        problems <- c(problems, glue(
          "{funnel}: {kind} step '{step}' not found in funnels_current; ",
          "check the funnel YAML against {lists[[kind]]} in R/metrics.R"
        ))
      }
    }
  }
  problems
}

# The `paused` funnels with at least `min_starters` 7-day starters at their
# entry step in the window ending `as_of`. `entry_counts` has funnel_id,
# funnelstepname, window_end, breakdown_value and activeusers.
resumed_funnels <- function(entry_counts, as_of, paused, min_starters) {
  entry <- unlist(funnel_entry_steps[entry_counts$funnel_id], use.names = FALSE)
  totals <- entry_counts[entry_counts$funnel_id %in% names(paused) &
                           entry_counts$funnelstepname == entry &
                           entry_counts$window_end == as_of &
                           entry_counts$breakdown_value == "RESERVED_TOTAL", ]
  sort(unique(totals$funnel_id[totals$activeusers >= min_starters]))
}

# Each of `funnels`' 7-day entry-step starters in the window ending `as_of`
# (current), and the median of the four windows ending one to four weeks
# earlier (baseline). A window with no row had no starters. `entry_counts` as
# for resumed_funnels().
starter_volume <- function(entry_counts, as_of, funnels) {
  entry <- unlist(funnel_entry_steps[entry_counts$funnel_id], use.names = FALSE)
  totals <- entry_counts[entry_counts$funnelstepname == entry &
                           entry_counts$breakdown_value == "RESERVED_TOTAL", ]
  starters_on <- function(funnel, end) {
    sum(totals$activeusers[totals$funnel_id == funnel & totals$window_end == end])
  }
  baseline_of <- function(funnel) {
    stats::median(vapply(as_of - 7L * 1:4, \(end) starters_on(funnel, end),
                         numeric(1)))
  }
  tibble::tibble(
    funnel_id = funnels,
    current = vapply(funnels, starters_on, numeric(1), end = as_of,
                     USE.NAMES = FALSE),
    baseline = vapply(funnels, baseline_of, numeric(1), USE.NAMES = FALSE)
  )
}

# The `expected_days` with no RESERVED_TOTAL row for `funnel` in any window.
# A day with no starters has no 1-day row but still ends the longer windows,
# so only a day with no rows at all is a gap.
funnel_missing_days <- function(raw, funnel, expected_days) {
  days_present <- raw$window_end[raw$funnel_id == funnel &
                                   raw$breakdown_value == "RESERVED_TOTAL"]
  expected_days[!expected_days %in% days_present]
}

run_preflight_safety_check <- function(con,
                                       today = Sys.Date(),
                                       ga_export_lag_days = 2L,
                                       lookback_days = 14L,
                                       min_task_success_rate = 0.02,
                                       max_unattributed_share = 0.10,
                                       min_starter_share = 0.25,
                                       min_baseline_starters = 30,
                                       min_resumed_starters = 30) {

  # Helpers --------------------------------------------------------------------

  # "Sun Aug 10", or "no data" for the non-finite max() an empty table yields.
  format_date <- function(d) {
    labels <- rep("no data", length(d))
    finite <- is.finite(as.numeric(d))
    labels[finite] <- format(as.Date(d[finite], origin = "1970-01-01"),
                             "%a %b %d")
    labels
  }

  # "10%" for a round share, "68.4%" otherwise.
  as_percent <- function(x) sub("\\.0%$", "%", sprintf("%.1f%%", x * 100))
  n_days <- function(n) paste(n, if (n == 1) "day" else "days")
  list_of <- function(x) glue_collapse(x, sep = ", ")

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

  # The span the per-party table reports on; rp-resolution sweeps the same span.
  lookup_start <- as_of - (long_window_days - 1L)
  long_window <- as.character(long_window_days)

  # Paused funnels have no starters to check; the paused-funnels check watches
  # for their return instead.
  live <- live_funnels()

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

  # Check 1 - GA freshness (no gaps over the lookback) --------------------------
  #
  # A funnel with no rows at all is reported as gone rather than as a run of
  # missing days, since the likely cause is a renamed or dropped funnel id.
  freshness_problems <- character()
  expected_days <- seq(freshness_start, as_of, by = "day")
  for (funnel in live) {
    missing <- funnel_missing_days(raw, funnel, expected_days)
    if (!funnel %in% raw$funnel_id) {
      freshness_problems <- c(
        freshness_problems,
        glue("{funnel}: no data at all in the last {long_window_days} days; ",
             "check whether its funnel id was renamed or dropped in the ",
             "pipeline")
      )
    } else if (length(missing) > 0) {
      # Capped: the full list would overflow the banner.
      shown <- format_date(utils::head(missing, 5))
      freshness_problems <- c(
        freshness_problems,
        glue("{funnel}: {n_days(length(missing))} missing, ",
             "including {list_of(shown)}")
      )
    }
  }

  record_check(
    "ga-freshness",
    glue("Task success data must run through {format_date(as_of)} ",
         "with no missing days"),
    passed = length(freshness_problems) == 0,
    details = if (length(freshness_problems) == 0) {
      glue("no missing days in the {lookback_days} days through ",
           "{format_date(as_of)} for {list_of(live)}")
    } else {
      freshness_problems
    }
  )

  # Check 2 - relying-party lookup complete -------------------------------------
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
    "rp-resolution",
    "Every relying party in the data must be labelled",
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

  # Check 3 - long window present -----------------------------------------------
  #
  # Without it the table's rate, counts and comparison all read n/a at once,
  # which looks like a broken page rather than a late pipeline run.
  window_problems <- character()
  for (funnel in live) {
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
        glue("{funnel}: no {long_window}-day total ending ",
             "{format_date(as_of)}; {newest}")
      )
    }
  }

  record_check(
    "long-window",
    glue("The {long_window}-day totals must run through {format_date(as_of)}"),
    passed = length(window_problems) == 0,
    details = if (length(window_problems) == 0) {
      glue("{long_window}-day totals run through {format_date(as_of)} ",
           "for {list_of(live)}")
    } else {
      window_problems
    }
  )

  # Check 4 - counted steps present in funnels_current ---------------------------
  #
  # Starters and the step table's span are read by step name, so a step renamed
  # upstream returns no rows rather than an error.
  steps_seen <- tbl(
    con, in_schema("google_analytics", "funnels_current")
  ) |>
    filter(funnel_id %in% !!required_funnels) |>
    distinct(funnel_id, funnelstepname) |>
    collect()

  step_problems <- step_name_problems(steps_seen, required_funnels)

  record_check(
    "entry-steps",
    "Every funnel's first and last counted steps must still be in the data",
    passed = length(step_problems) == 0,
    details = if (length(step_problems) == 0) {
      glue("first and last counted steps found for {list_of(required_funnels)}")
    } else {
      step_problems
    }
  )

  # Check 5 - help site stream reporting -----------------------------------------
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
    glue("The help site stream must report through {format_date(as_of)}"),
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

  # Check 6 - help site stream identity ------------------------------------------
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
    "The stream id must still name the help site",
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

  # Check 7 - task success rate implausibly low ---------------------------------
  #
  # An exit step that stops reporting reads as 0%, not as missing. A floor
  # rather than a zero test, because one stray event lifts a broken funnel off
  # zero. The 28-day window is tested in its own right: activeUsers
  # de-duplicate within a window, so it is not a blend of the 7-day ones.
  low_rates <- implausible_rates(raw, live, as_of, min_task_success_rate)

  rate_problems <- character()
  for (funnel in unique(low_rates$funnel_id)) {
    # A flat zero ties across windows; ordering keeps the wording stable
    # between renders and reports the day the funnel broke.
    hits <- low_rates[low_rates$funnel_id == funnel, ] |>
      arrange(value, window_end)
    worst <- hits[1, ]

    rate_problems <- c(
      rate_problems,
      glue(
        "{funnel}: under {as_percent(min_task_success_rate)} in ",
        "{nrow(hits)} window(s), lowest {as_percent(worst$value)} in the ",
        "{worst$window_days} days ending {format_date(worst$window_end)}. ",
        "A rate this low usually means the last step stopped reporting"
      )
    )
  }

  record_check(
    "plausibility",
    glue("No task success rate shown may fall below ",
         "{as_percent(min_task_success_rate)}"),
    passed = length(rate_problems) == 0,
    details = if (length(rate_problems) == 0) {
      glue("every rate shown is {as_percent(min_task_success_rate)} or more ",
           "for {list_of(live)}")
    } else {
      rate_problems
    }
  )

  # Check 8 - starters with no relying party ------------------------------------
  #
  # From 2026-09-22 GA stopped recording rp_name for two services and their
  # traffic moved to "(not set)". Every check passed and the table showed them
  # at 0%. Before that the share never passed 2.1% on any 7-day window of any
  # required funnel (July to Sep 21, max sign_up on Jul 23), and it has been
  # 68% or more since Sep 28, so 10% sits well clear of both. The entry step,
  # because the per-party rates divide by it.
  #
  # Fails only in the week the share crosses the threshold. The lost rp_names
  # cannot be restored, so an outage that is still over it a week later is
  # reported in the details without failing again. A second loss that starts
  # during an ongoing one is not raised. Only funnels with a relying-party
  # breakdown are judged.
  prior_end <- as_of - 7L
  entry_steps <- unname(vapply(
    required_funnels, \(f) funnel_entry_steps[[f]], character(1)
  ))
  entry_counts <- tbl(con, in_schema("google_analytics", "funnels_current")) |>
    filter(
      funnel_id %in% !!required_funnels,
      funnelstepname %in% !!unique(entry_steps),
      window_days == "7",
      !window_incomplete,
      # Back four weeks for the starter-volume baseline.
      as.Date(window_end) >= as.Date(!!as.character(as_of - 28L)),
      as.Date(window_end) <= as.Date(!!as.character(as_of))
    ) |>
    select(funnel_id, funnelstepname, window_end, breakdown_value, activeusers) |>
    collect() |>
    mutate(window_end = as.Date(window_end),
           activeusers = as.numeric(activeusers))

  windows <- list()
  attribution_shares <- numeric()
  statuses <- character()
  crossed_on <- as.Date(character())
  for (funnel in party_funnels(live)) {
    entry <- funnel_entry_steps[[funnel]]
    funnel_counts <- entry_counts[entry_counts$funnel_id == funnel &
                                    entry_counts$funnelstepname == entry, ]
    window_shares <- vapply(
      split(funnel_counts, funnel_counts$window_end), unattributed_share,
      numeric(1)
    )
    share_on <- \(end) unname(window_shares[as.character(end)])
    windows[[funnel]] <- list(
      current = funnel_counts[funnel_counts$window_end == as_of, ],
      previous = funnel_counts[funnel_counts$window_end == prior_end, ]
    )
    attribution_shares[[funnel]] <- share_on(as_of)
    statuses[[funnel]] <- attribution_status(
      share_on(as_of), share_on(prior_end), max_unattributed_share
    )
    over <- as.Date(names(window_shares))[window_shares > max_unattributed_share]
    crossed_on[funnel] <- min(over[over > prior_end], as_of)
  }

  shares_of <- \(funnels) {
    list_of(glue("{funnels} {as_percent(attribution_shares[funnels])}"))
  }
  no_total <- names(statuses)[is.na(statuses)]
  onset <- names(statuses)[statuses %in% "onset"]
  ongoing <- names(statuses)[statuses %in% "ongoing"]

  attribution_problems <- as.character(glue(
    "{no_total}: no starters in the 7 days ending {format_date(as_of)}, so ",
    "the share with no service cannot be worked out"
  ))
  if (length(onset) > 0) {
    # The services that fell are named from the largest crossing funnel only;
    # the others lose the same services.
    totals <- vapply(onset, \(f) {
      current <- windows[[f]]$current
      sum(current$activeusers[current$breakdown_value == "RESERVED_TOTAL"])
    }, numeric(1))
    lead <- onset[which.max(totals)]
    drops <- largest_starter_drops(windows[[lead]]$current,
                                   windows[[lead]]$previous)
    fell <- if (nrow(drops) == 0) {
      "no service lost starters"
    } else {
      list_of(glue("{drops$breakdown_value} ",
                   "{drops$activeusers_previous} to {drops$activeusers_current}"))
    }
    attribution_problems <- c(
      attribution_problems,
      glue("starters with no service recorded (GA rp_name) passed ",
           "{as_percent(max_unattributed_share)} in the 7 days ending ",
           "{format_date(min(crossed_on[onset]))}"),
      glue("7 days ending {format_date(as_of)}: {shares_of(onset)}"),
      glue("{lead} starters fell most for: {fell} ",
           "(7 days ending {format_date(prior_end)} to {format_date(as_of)})"),
      "per-service rates are unreliable until GA records the service again"
    )
  }

  attribution_details <- if (length(ongoing) > 0) {
    c(glue("still over {as_percent(max_unattributed_share)} with no service ",
           "recorded, not failed again because it already was last week: ",
           "{shares_of(ongoing)} in the 7 days ending {format_date(as_of)}"),
      "per-service rates stay unreliable until GA records the service again")
  }
  if (length(attribution_problems) == 0 && length(ongoing) == 0) {
    attribution_details <- glue(
      "starters with no service recorded, 7 days ending ",
      "{format_date(as_of)}: {shares_of(names(attribution_shares))}"
    )
  }

  record_check(
    "rp-attribution",
    glue("Starters with no service recorded must stay under ",
         "{as_percent(max_unattributed_share)}"),
    passed = length(attribution_problems) == 0,
    details = c(attribution_problems, attribution_details)
  )

  # Check 9 - starter volume -----------------------------------------------------
  #
  # A funnel whose entry step stops firing keeps a plausible rate on falling
  # counts, and a partial drop passes every other check. From Jul 13 to Nov 15,
  # 2026, no live funnel's 7-day starters fell below 42% of the median of the
  # four weeks before (recover_password on Oct 27, after a surge), so 25% sits
  # clear of normal weeks. A zero outage crosses it about five days in. Only
  # funnels with rows ending on as_of are judged: a missing day is ga-freshness's
  # to report, not a fall to zero.
  reported <- live[vapply(live, \(f) {
    length(funnel_missing_days(raw, f, as_of)) == 0
  }, logical(1))]
  volume <- starter_volume(entry_counts, as_of, reported)
  judged <- volume[volume$baseline >= min_baseline_starters, ]
  judged$share <- judged$current / judged$baseline
  fallen <- judged[judged$share < min_starter_share, ]
  too_few <- volume$funnel_id[volume$baseline < min_baseline_starters]
  count <- \(x) format(round(x), big.mark = ",", trim = TRUE)

  record_check(
    "starter-volume",
    glue("No funnel's weekly starters may fall below ",
         "{as_percent(min_starter_share)} of usual"),
    passed = nrow(fallen) == 0,
    details = c(
      if (nrow(fallen) > 0) {
        glue("{fallen$funnel_id}: {count(fallen$current)} starters in the 7 ",
             "days ending {format_date(as_of)}, {as_percent(fallen$share)} of ",
             "the usual {count(fallen$baseline)}; check that its first step ",
             "still fires")
      } else if (nrow(judged) > 0) {
        glue("7 days ending {format_date(as_of)} as a share of the usual ",
             "(median of the 4 weeks before): ",
             "{list_of(glue('{judged$funnel_id} {as_percent(judged$share)}'))}")
      },
      if (length(too_few) > 0) {
        glue("not judged, under {min_baseline_starters} starters a week: ",
             "{list_of(too_few)}")
      }
    )
  )

  # Check 10 - paused funnels still paused --------------------------------------
  #
  # The checks above skip paused funnels, so one that restarts is unchecked
  # until its paused_funnels entry goes. A floor rather than any starter, so a
  # stray event does not raise a banner.
  resumed <- resumed_funnels(entry_counts, as_of, paused_funnels,
                             min_resumed_starters)
  paused <- names(paused_funnels)

  record_check(
    "paused-funnels",
    glue("Paused funnels must stay paused (under {min_resumed_starters} ",
         "starters a week)"),
    passed = length(resumed) == 0,
    details = if (length(paused) == 0) {
      "no funnels are paused"
    } else if (length(resumed) == 0) {
      glue("{paused}: paused since {format_date(as.Date(paused_funnels))}, ",
           "so the starter checks skip it")
    } else {
      glue("{resumed}: {min_resumed_starters} or more starters in the 7 days ",
           "ending {format_date(as_of)}, so it looks restarted. Remove it from ",
           "paused_funnels in R/metrics.R and annotate the restart")
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
      glue("Data quality checks failed: ",
           "{glue_collapse(failed_slugs, sep = ', ', last = ' and ')} ",
           "(see the checklist above). The page still renders, with a banner ",
           "across the top."),
      call. = FALSE
    )
  }

  invisible(list(
    passed = length(failed_checks) == 0,
    checks = checks,
    failed = failed_checks
  ))
}
