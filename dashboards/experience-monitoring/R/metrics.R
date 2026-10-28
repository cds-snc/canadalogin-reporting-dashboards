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

# Chart annotations -----------------------------------------------------------

# Nothing is checked here: the annotations test checks the file on every PR.
read_annotations <- function(path = "annotations.yml") {
  entries <- yaml::read_yaml(path)
  field <- function(name, default) {
    purrr::map(entries, \(entry) entry[[name]] %||% default)
  }
  start <- as.Date(unlist(field("start", NA_character_)))
  tibble::tibble(
    funnel = field("funnel", NULL),
    start = start,
    # A single day ends where it starts.
    end = dplyr::coalesce(as.Date(unlist(field("end", NA_character_))), start),
    description = unlist(field("description", NA_character_)),
    chart = unlist(field("chart", TRUE))
  )
}

# One funnel's entries in caption order: standing notes as written, then dated
# ones by start. Dated ones wholly outside `from` to `to` are dropped, and only
# drawn ones are lettered so the letters never skip.
funnel_annotations <- function(annotations, funnel_id, from, to) {
  mine <- purrr::map_lgl(annotations$funnel,
                         \(funnel) any(c("all", funnel_id) %in% funnel))
  standing <- annotations[mine & is.na(annotations$start), ]
  dated <- annotations[mine & !is.na(annotations$start), ]
  dated <- dated[dated$end >= from & dated$start <= to, ]
  dated <- dated[order(dated$start), ]
  dated$letter <- NA_character_
  dated$letter[dated$chart] <- LETTERS[seq_len(sum(dated$chart))]
  dplyr::bind_rows(standing, dated)
}

# "Sep 30, 2026", "Sep 17 to Oct 1, 2026", or a year on each end when a range
# crosses one. month.abb rather than %b, which follows the locale.
fmt_event_dates <- function(start, end) {
  day <- function(date) {
    paste(month.abb[as.integer(format(date, "%m"))],
          as.integer(format(date, "%d")))
  }
  start_year <- format(start, "%Y")
  end_year <- format(end, "%Y")
  dplyr::case_when(
    start == end ~ paste0(day(end), ", ", end_year),
    start_year == end_year ~ paste0(day(start), " to ", day(end), ", ", end_year),
    .default = paste0(day(start), ", ", start_year, " to ", day(end), ", ",
                      end_year)
  )
}

# Joined with <br>: a blank line in a fig-cap breaks the figure.
annotation_caption <- function(events) {
  lines <- events$description
  dated <- !is.na(events$start)
  lines[dated] <- paste0(fmt_event_dates(events$start[dated], events$end[dated]),
                         ": ", lines[dated])
  lettered <- !is.na(events$letter)
  lines[lettered] <- paste0("**", events$letter[lettered], ".** ",
                            lines[lettered])
  paste(lines, collapse = "<br>")
}

# Grey and added before the data, so events sit behind it. Bands are clipped
# to `from` and `to`; the x scale has no fixed limits, so an unclipped band
# would stretch the axis. Bands are translucent with dashed edges so two that
# overlap stay apart; a single day is an edge on its own.
annotation_layers <- function(events, from, to) {
  drawn <- events[!is.na(events$letter), ]
  drawn$xmin <- pmax(drawn$start, from)
  drawn$xmax <- pmin(drawn$end, to)
  drawn$middle <- drawn$xmin + as.numeric(drawn$xmax - drawn$xmin) / 2
  list(
    ggplot2::geom_rect(
      data = drawn[drawn$xmin < drawn$xmax, ],
      ggplot2::aes(xmin = xmin, xmax = xmax),
      ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.12,
      inherit.aes = FALSE
    ),
    ggplot2::geom_vline(
      xintercept = unique(c(drawn$xmin, drawn$xmax)),
      colour = "grey55", linetype = "dashed", linewidth = 0.4
    ),
    # Inf survives the y limits and lands in the headroom above 100%.
    ggplot2::geom_text(
      data = drawn, ggplot2::aes(x = middle, label = letter), y = Inf,
      vjust = 1.4, colour = "grey45", fontface = "bold", size = 3.3,
      inherit.aes = FALSE
    )
  )
}
