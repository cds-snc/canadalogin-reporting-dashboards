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

# The numerator step of each funnel's `metrics:` block, copied by hand like
# funnel_entry_steps: a render reads Athena only, never the YAML. With the entry
# step it bounds the steps the step table marks as counted.
funnel_exit_steps <- list(
  sign_in = "mfa_submit",
  sign_up = "complete",
  recover_password = "complete",
  recover_2sv = "confirmation",
  migration_existing_to_cl = "migration_complete",
  migration_new_to_cl = "migration_complete"
)

# Funnels switched off in production, with the date they went off. Preflight
# skips the checks that need starters for these, and fails once one has
# starters again: delete its entry then, and annotate the restart.
paused_funnels <- c(
  migration_existing_to_cl = "2026-11-06",   # gc-signin-terraform #643
  migration_new_to_cl = "2026-11-06"
)

# Required funnels that are not paused.
live_funnels <- function(paused = paused_funnels) {
  setdiff(required_funnels, names(paused))
}

# Labels, local until the pipeline emits a label column. Keyed by funnel id so
# a mismatch fails rather than heading a page with another funnel's numbers.
# Each funnel's notes are in annotations.yml.
funnel_presentation <- list(
  sign_in = list(label = "Sign in"),
  sign_up = list(label = "Sign up"),
  recover_password = list(label = "Recover password"),
  recover_2sv = list(label = "Recover 2-step verification"),
  migration_existing_to_cl = list(
    label = "Migration, existing CanadaLogin account"
  ),
  migration_new_to_cl = list(label = "Migration, new CanadaLogin account")
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

# Both funnel tables, loaded whole once per render and sliced in R. Each
# Athena query costs about 3.5 s however few rows it returns, and a funnel
# page asks for about seven slices, so querying per slice took most of the
# render. From funnels_current, entry steps are kept for starters, and every
# step of the long-window total for the step table.
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
    long_days <- as.character(long_window_days)
    steps <- dplyr::tbl(
      con, dbplyr::in_schema("google_analytics", "funnels_current")
    ) |>
      dplyr::filter(
        funnelstepname %in% !!entry_steps |
          (window_days == !!long_days & breakdown_value == "RESERVED_TOTAL"),
        !window_incomplete
      ) |>
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
    dplyr::mutate(finished = round(value * starters)) |>
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

# Fewest starters either window needs for a meaningful change. Below this the
# normal approximation behind change_noise() breaks down.
min_change_starters <- 30

# FALSE where either side is missing, so an unmeasured change reads grey.
is_meaningful_change <- function(delta, noise, starters, prior_starters) {
  enough <- pmin(starters, prior_starters) >= min_change_starters
  dplyr::coalesce(enough & abs(delta) > noise_multiple * noise, FALSE)
}

# A change in whole points as HTML, such as "▼ 4". The arrow carries the
# direction, so there is no sign. Coloured only when meaningful and grey
# otherwise; one that rounds to nothing is "● 0", and one with nothing to
# compare against is a dash.
change_points_html <- function(delta, meaningful) {
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
  # Whole points, not change_verdict()'s tenth of a point, decide flat here.
  change_html(delta, meaningful = meaningful & direction != "flat",
              points = TRUE, text = change)
}

# One table cell as HTML: the rate, then its change, such as "64% ▼ 4".
rate_change_cell <- function(rate, delta, meaningful) {
  ifelse(
    is.na(rate), "n/a",
    paste0(round(rate * 100), "% ", change_points_html(delta, meaningful))
  )
}

# The value box line comparing the latest long window with the one before,
# led by words: a lone change would be a single Str and lose its paragraph.
long_change_line <- function(now, prior) {
  if (is.na(now$value) || is.na(prior$value)) {
    return("No earlier window to compare against yet")
  }
  delta <- now$value - prior$value
  noise <- change_noise(now$value, now$starters, prior$value, prior$starters)
  meaningful <- is_meaningful_change(delta, noise, now$starters,
                                     prior$starters)
  unit <- if (floor(abs(delta) * 100 + 0.5) == 1) "point" else "points"
  paste("Latest", long_window_days, "days",
        change_points_html(delta, meaningful), unit)
}

# Step table -----------------------------------------------------------------

# Polite step names, keyed by funnel id then step slug. People who do not write
# R edit the file, so it is YAML; tests check its shape and nothing checks it
# at render time.
read_step_labels <- function(path = "step-labels.yml") {
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path) %||% list()
}

# A step's polite name, else its slug with the underscores opened up.
step_label <- function(labels, funnel_id, step) {
  polite <- vapply(step, \(s) {
    name <- labels[[funnel_id]][[s]]
    if (is.null(name)) NA_character_ else as.character(name)
  }, character(1), USE.NAMES = FALSE)
  dplyr::coalesce(polite, gsub("_", " ", step))
}

# One row per step from one window's step counts: people, their share of the
# first step and of the step before, and whether the step is in the span the
# task success rate counts. A later step can hold more people than an earlier
# one (GA de-duplicates each step separately); its real count shows, and the
# share is left over 1 for the bar to clamp.
step_table_rows <- function(steps, entry_step, exit_step) {
  steps <- dplyr::arrange(steps, funnelstepnumber)
  first <- steps$funnelstepnumber[match(entry_step, steps$funnelstepname)]
  last <- steps$funnelstepnumber[match(exit_step, steps$funnelstepname)]
  steps |>
    dplyr::transmute(
      number = as.integer(funnelstepnumber),
      step = funnelstepname,
      people = activeusers,
      share_first = activeusers / dplyr::first(activeusers),
      continued = dplyr::lead(activeusers) / activeusers,
      counted = !is.na(first) & !is.na(last) &
        funnelstepnumber >= first & funnelstepnumber <= last
    )
}

# The step table's rows for a funnel's long-window total ending on as_of. No
# rows means nobody entered in that window; there is no fallback to an earlier
# one, which would show old numbers on a dated page.
funnel_step_table <- function(con, funnel_id, as_of) {
  entry <- declared_or_stop(funnel_entry_steps, funnel_id, "funnel_entry_steps")
  exit <- declared_or_stop(funnel_exit_steps, funnel_id, "funnel_exit_steps")
  steps <- funnel_tables(con)$steps
  window <- steps[steps$funnel_id == funnel_id &
                    steps$window_days == as.character(long_window_days) &
                    steps$breakdown_value == "RESERVED_TOTAL" &
                    steps$window_end == as.Date(as_of), ]
  step_table_rows(window, entry, exit)
}
