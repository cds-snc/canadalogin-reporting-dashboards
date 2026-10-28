#' Task success metric layer, over the google_analytics funnel tables.
#'
#' The rate is the pipeline's stored value, never recomputed here: the funnel
#' YAML in the pipeline repo defines it, so a change there reaches the page
#' without a change here. Starters come from the entry step's activeUsers, and
#' finishers are derived from the two.
#'
#' Every date and window column here is a string partition key: compare
#' window_days == "7", not == 7. Expects dplyr / dbplyr on the caller.

# Every funnel must be present in the data (preflight) and have its own tab.
required_funnels <- c(
  "sign_in", "sign_up", "recover_password", "recover_2sv",
  "migration_existing_to_cl", "migration_new_to_cl"
)

# The denominator step of each funnel's `metrics:` block in the pipeline YAML,
# which is canonical. Only starters are read by step name; the `entry-steps`
# preflight check fails the render if one drifts from the data.
funnel_entry_steps <- list(
  sign_in = "enter_email",
  sign_up = "terms",
  recover_password = "reset_password",
  recover_2sv = "initiate_recovery",
  migration_existing_to_cl = "migration_not_skipped",
  migration_new_to_cl = "migration_not_skipped"
)

# Labels and the note shown under each funnel's table, local until the
# pipeline emits a label column. Keyed by funnel id so a mismatch fails rather
# than heading a page with another funnel's numbers.
funnel_presentation <- list(
  sign_in = list(
    label = "Sign in",
    note = paste(
      "**Note.** This rate stops when the user submits their second factor,",
      "the last step Google Analytics can see before the off-site redirect to",
      "IBM Verify. It counts submits, so a wrong code still counts as finished."
    )
  ),
  sign_up = list(
    label = "Sign up",
    note = paste(
      "**Note.** Sign up's exit is the account-creation confirmation page, a",
      "first-party event."
    )
  ),
  recover_password = list(
    label = "Recover password",
    note = paste(
      "**Note.** The exit here is the confirmation shown once a new password is",
      "set, a first-party event."
    )
  ),
  recover_2sv = list(
    label = "Recover 2-step verification",
    note = paste(
      "**Note.** This funnel measures users who lost their 2-step verification",
      "method and set up a new one. The exit is the screen shown once the new",
      "method is set up, which asks the user to delete methods they no longer",
      "use. It is a low volume funnel, about ten starts a day as of October 2026."
    )
  ),
  migration_existing_to_cl = list(
    label = "Migration, existing CanadaLogin account",
    note = paste(
      "**Note.** This funnel measures users who are migrating a legacy credential to",
      "CanadaLogin and already have a CanadaLogin account. As of August 2026, this",
      "is a low volume funnel, since most users are creating a new account."
    )
  ),
  migration_new_to_cl = list(
    label = "Migration, new CanadaLogin account",
    note = paste(
      "**Note.** This funnel measures users who are migrating a legacy credential to",
      "CanadaLogin and want to create a new CanadaLogin account."
    )
  )
)

# A missing entry fails here rather than heading a page with a warehouse id.
declared_or_stop <- function(table, funnel_id, table_name) {
  entry <- table[[funnel_id]]
  if (is.null(entry)) {
    stop("Funnel '", funnel_id, "' has no entry in ", table_name,
         " in R/metrics.R; add one.", call. = FALSE)
  }
  entry
}

funnel_label <- function(funnel_id) {
  declared_or_stop(funnel_presentation, funnel_id, "funnel_presentation")$label
}

funnel_note <- function(funnel_id) {
  declared_or_stop(funnel_presentation, funnel_id, "funnel_presentation")$note
}

# Both funnel tables, loaded whole once per render and sliced in R. Each
# Athena query costs about 3.5 s however few rows it returns, and a funnel
# page asks for about seven slices, so querying per slice took most of the
# render. Only entry steps are kept from funnels_current: starters are the
# only step count read here.
funnel_tables <- local({
  cache <- NULL
  function(con) {
    if (!is.null(cache)) return(cache)

    rates <- dplyr::tbl(
      con, dbplyr::in_schema("google_analytics", "funnel_metrics_current")
    ) |>
      dplyr::filter(metric_id == "task_success", !window_incomplete) |>
      dplyr::select(funnel_id, window_days, window_end, funnel_version,
                    breakdown_value, value) |>
      dplyr::collect()

    entry_steps <- unique(unlist(funnel_entry_steps))
    steps <- dplyr::tbl(
      con, dbplyr::in_schema("google_analytics", "funnels_current")
    ) |>
      dplyr::filter(funnelstepname %in% !!entry_steps, !window_incomplete) |>
      dplyr::select(funnel_id, window_days, window_end, funnelstepname,
                    funnelstepnumber, breakdown_value, activeusers) |>
      dplyr::collect()

    cache <<- list(
      rates = dplyr::mutate(rates, window_end = as.Date(window_end)),
      steps = dplyr::mutate(steps, window_end = as.Date(window_end))
    )
    cache
  }
})

# Stored rates with their starters and finishers, one row per (window_end,
# breakdown_value). `from` and `to` bound window_end inclusively;
# funnel_version is kept for the chart. `finished` is derived, rate times
# starters, since the stored table holds the rate alone.
task_success_rows <- function(con, funnel_id, window_days, from, to) {
  entry_step <- declared_or_stop(funnel_entry_steps, funnel_id,
                                 "funnel_entry_steps")
  tables <- funnel_tables(con)
  window_days <- as.character(window_days)
  from <- as.Date(from)
  to <- as.Date(to)

  in_span <- function(table) {
    table[table$funnel_id == funnel_id &
            table$window_days == window_days &
            table$window_end >= from &
            table$window_end <= to, ]
  }

  rates <- in_span(tables$rates) |>
    dplyr::select(window_end, funnel_version, breakdown_value, value)

  starters <- in_span(tables$steps) |>
    dplyr::filter(funnelstepname == entry_step) |>
    dplyr::select(window_end, breakdown_value, starters = activeusers)

  rates |>
    dplyr::left_join(starters, by = c("window_end", "breakdown_value")) |>
    dplyr::mutate(
      # integer64 does not survive arithmetic with doubles cleanly.
      starters = as.numeric(starters),
      finished = round(value * starters)
    ) |>
    dplyr::arrange(window_end)
}

# GA's own total across every relying party, internal included. The headline
# figures and the chart use this.
total_task_success <- function(con, funnel_id, window_days, from, to) {
  task_success_rows(con, funnel_id, window_days, from, to) |>
    dplyr::filter(breakdown_value == "RESERVED_TOTAL")
}

# One row per GA rp_name, internal and unattributed included; dropping happens
# at display time. Stored rates cannot be pooled, so two names stay two rows.
party_task_success <- function(con, funnel_id, window_days, from, to) {
  task_success_rows(con, funnel_id, window_days, from, to) |>
    dplyr::filter(breakdown_value != "RESERVED_TOTAL")
}

# GA's overall rate as a series over window_end.
task_success_series <- function(con, funnel_id, window_days, from, to) {
  total_task_success(con, funnel_id, window_days, from, to) |>
    dplyr::select(window_end, funnel_version, value, starters, finished)
}

# The latest overall rate, as one row. An empty span returns an all-NA row so
# callers can format it without a special case.
task_success_latest <- function(con, funnel_id, window_days, from, to) {
  series <- task_success_series(con, funnel_id, window_days, from, to)
  if (nrow(series) == 0) {
    return(dplyr::tibble(
      window_end = as.Date(NA), funnel_version = NA_character_,
      value = NA_real_, starters = NA_real_, finished = NA_real_
    ))
  }
  series[which.max(series$window_end), ]
}

# The entry step as the data spells it, and its position, so labels follow the
# funnel definition.
funnel_entry_step <- function(con, funnel_id) {
  entry_step <- declared_or_stop(funnel_entry_steps, funnel_id,
                                 "funnel_entry_steps")

  steps <- funnel_tables(con)$steps
  numbers <- unique(steps$funnelstepnumber[
    steps$funnel_id == funnel_id & steps$funnelstepname == entry_step
  ])

  dplyr::tibble(
    entry_step = entry_step,
    entry_number = if (length(numbers) == 0) NA_integer_ else
      as.integer(min(numbers))
  )
}

# Length of the long window. Always read as a stored window, never built from
# shorter ones: activeUsers de-duplicate within a window, not across them.
long_window_days <- 28L

# The overall rate over the one stored window ending on as_of, as one row (all
# NA if that window is missing). Move as_of back long_window_days for the
# preceding period.
task_success_long_window <- function(con, funnel_id, as_of) {
  as_of <- as.Date(as_of)
  task_success_latest(con, funnel_id, long_window_days, as_of, as_of)
}

# The same window, one row per GA rp_name.
party_task_success_long_window <- function(con, funnel_id, as_of) {
  as_of <- as.Date(as_of)
  party_task_success(con, funnel_id, long_window_days, as_of, as_of)
}

# Change against the window before -------------------------------------------

# A change is meaningful past this many standard errors, about 95% two-sided.
noise_multiple <- 1.96

# Standard error of the difference between two rates from their starters, as
# in the two-proportion test: the sampling noise alone, so a busy row gets a
# tight band and a thin one a wide band. Pooled, so a window at 0% or 100%
# still has noise. NA where either side has no starters.
change_noise <- function(rate, starters, prior_rate, prior_starters) {
  pooled <- (rate * starters + prior_rate * prior_starters) /
    (starters + prior_starters)
  variance <- pooled * (1 - pooled) * (1 / starters + 1 / prior_starters)
  ifelse(starters > 0 & prior_starters > 0, sqrt(variance), NA_real_)
}

# Lighter than delta_colours' grey, so a change within the noise stands well
# apart from a coloured one. It sits at 3.2:1 on white, under the 4.5:1 AA
# minimum for text; the rate beside it carries the figure.
quiet_change_colour <- "#8A9299"

# FALSE where either side is missing, so an unmeasured change reads grey.
is_meaningful_change <- function(delta, noise) {
  dplyr::coalesce(abs(delta) > noise_multiple * noise, FALSE)
}

# One table cell as HTML: the rate, then its change in whole points, such as
# "64% ▼ 4". The arrow carries the direction, so there is no sign. The change
# is coloured only when meaningful and grey otherwise; one that rounds to
# nothing is "● 0", and one with nothing to compare against is a dash.
rate_change_cell <- function(rate, delta, meaningful) {
  points <- floor(abs(delta) * 100 + 0.5)
  direction <- dplyr::case_when(
    is.na(delta) | points == 0 ~ "flat",
    delta > 0 ~ "up",
    .default = "down"
  )
  change <- dplyr::case_when(
    is.na(delta) ~ "-",
    direction == "up" ~ paste0("\u25b2\u00a0", points),
    direction == "down" ~ paste0("\u25bc\u00a0", points),
    .default = "\u25cf\u00a00"
  )
  colour <- ifelse(meaningful & direction != "flat",
                   delta_colours[direction], quiet_change_colour)
  ifelse(
    is.na(rate), "n/a",
    sprintf('%s%% <span style="color:%s">%s</span>',
            round(rate * 100), colour, change)
  )
}
