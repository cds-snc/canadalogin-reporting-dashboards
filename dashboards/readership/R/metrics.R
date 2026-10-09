# Readership metric layer: who reads Signal Check, the dashboards and the
# Metrics Cookbook, from the meta-analytics GA4 property.
#
# A page is always `pagepath_redacted`, never the raw `pagepath`, which holds
# each edition's private token. Neither read below selects `pagepath`.

# The first day the meta-analytics tables cover. Editions 1 and 2 were
# published before it.
tracking_from <- as.Date("2026-07-23")

# A Signal Check is compared over its first two weeks, publication day
# included.
edition_window_days <- 14L

# The dwell milestones every edition and dashboard fires, in seconds.
dwell_marks <- c(1, 5, 15, 30, 60, 120, 300, 600)
dwell_events <- paste0("read_", dwell_marks, "s")

edition_pattern <-
  "^/canadalogin-signal-check-publishing/r/([0-9]{8})_signal-check\\.html$"
dashboard_pattern <- "^/canadalogin-signal-check-publishing/r/([^/]+)\\.html$"

# A published file renamed since, read as the file it became.
dashboard_renames <- c(
  "task-success-monitoring" = "experience-monitoring"
)

# How a session started, in legend order: the announcement posts and our own
# pages tag their links; anything else is the referrer GA saw, if any.
source_groups <- tibble::tribble(
  ~group,        ~label,
  "slack",       "Slack",
  "teams",       "Teams",
  "email",       "Email",
  "our_links",   "Our own links",
  "direct",      "No link recorded",
  "other",       "Other sites"
)

source_group <- function(source) {
  dplyr::case_when(
    source == "slack" ~ "slack",
    source == "teams" | startsWith(source, "teams.") ~ "teams",
    source == "email" ~ "email",
    source %in% c("latest-link", "report_link", "dashboard_link") ~ "our_links",
    source %in% c("(direct)", "(not set)") ~ "direct",
    .default = "other"
  )
}

# Today in Toronto. The runner's clock is UTC, which is a day ahead late in
# the Toronto evening.
toronto_today <- function(now = Sys.time()) {
  as.Date(format(now, "%Y-%m-%d", tz = "America/Toronto"))
}

# Reads ----------------------------------------------------------------------

# Cache a read for the session. Every page reads the same rows, and the data
# cannot change mid-render.
read_once <- function(read) {
  cache <- new.env(parent = emptyenv())
  function(con) {
    if (is.null(cache$value)) cache$value <- read(con)
    cache$value
  }
}

# Drops integer64, which does not survive arithmetic with doubles cleanly.
as_plain_numbers <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::where(bit64::is.integer64), as.numeric))
}

# Views and sessions per day, page, title and session source. The campaign
# is dropped: tagged links carry a source only.
page_traffic <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("google_analytics", "meta_page_traffic")) |>
    dplyr::group_by(date, streamname, page_kind, pagepath_redacted, pagetitle,
                    sessionsource, sessionmedium) |>
    dplyr::summarise(sessions = sum(sessions, na.rm = TRUE),
                     views = sum(screenpageviews, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::mutate(day = as.Date(date), .keep = "unused") |>
    dplyr::arrange(day)
})

# Page views and dwell milestones per day and page.
page_events <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("google_analytics", "meta_page_events")) |>
    dplyr::filter(eventname %in% !!c("page_view", dwell_events)) |>
    dplyr::group_by(date, page_kind, pagepath_redacted, eventname) |>
    dplyr::summarise(events = sum(eventcount, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::mutate(day = as.Date(date), .keep = "unused") |>
    dplyr::arrange(day)
})

# The newest day exported for the CanadaLogin property, in the same run as the
# meta tables but never empty: a day nobody reads our pages leaves no meta rows.
export_through <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("google_analytics", "property_traffic")) |>
    dplyr::summarise(day = max(date, na.rm = TRUE)) |>
    dplyr::collect() |>
    dplyr::pull(day) |>
    as.Date()
})

# Editions --------------------------------------------------------------------

# "CanadaLogin Signal Check #6" to 6; NA for a title with no number.
edition_number <- function(title) {
  as.integer(stringr::str_match(title, "#([0-9]+)")[, 2])
}

# Every number an edition's titles carry, one row per page and number. More
# than one, or none, is a preflight failure.
edition_titles <- function(traffic) {
  traffic |>
    dplyr::filter(grepl(edition_pattern, pagepath_redacted)) |>
    dplyr::distinct(pagepath_redacted, number = edition_number(pagetitle))
}

# One row per edition: page, number and publication date, oldest first. The
# date is the one in the file name, which is the day it was sent.
edition_list <- function(traffic) {
  edition_titles(traffic) |>
    dplyr::filter(!is.na(number)) |>
    dplyr::group_by(pagepath_redacted) |>
    dplyr::summarise(number = if (dplyr::n() == 1L) number else NA_integer_,
                     .groups = "drop") |>
    dplyr::filter(!is.na(number)) |>
    dplyr::mutate(
      published = as.Date(sub(edition_pattern, "\\1", pagepath_redacted),
                          "%Y%m%d"),
      # Never a bare "#6": readers know them as Signal Checks.
      label = paste0("Signal Check #", number)
    ) |>
    dplyr::arrange(published)
}

# Each edition's views per day since tracking began. `age` is days since
# publication; a view before it is the team reviewing the draft.
edition_days <- function(traffic, editions) {
  traffic |>
    dplyr::inner_join(editions, by = "pagepath_redacted") |>
    dplyr::group_by(number, label, published, day) |>
    dplyr::summarise(views = sum(views), .groups = "drop") |>
    dplyr::mutate(age = as.integer(day - published))
}

# How many of an edition's first 14 days have data, and whether tracking saw
# all of them.
edition_coverage <- function(editions, through) {
  editions |>
    dplyr::mutate(
      days_seen = pmax(0L, pmin(edition_window_days,
                                as.integer(through - published) + 1L)),
      window_end = published + edition_window_days - 1L,
      comparable = published >= tracking_from,
      complete = comparable & window_end <= through
    )
}

# Cumulative views by day since publication, over the first 14 days or as
# many as have passed. A day with no rows is a day with no views.
cumulative_views <- function(days, coverage) {
  coverage |>
    dplyr::filter(comparable, days_seen > 0) |>
    dplyr::select(number, label, days_seen) |>
    dplyr::mutate(age = purrr::map(days_seen, \(n) seq_len(n) - 1L)) |>
    tidyr::unnest(age) |>
    dplyr::left_join(dplyr::select(days, number, age, views),
                     by = c("number", "age")) |>
    dplyr::mutate(views = dplyr::coalesce(views, 0)) |>
    dplyr::group_by(number, label) |>
    dplyr::arrange(age, .by_group = TRUE) |>
    dplyr::mutate(cumulative = cumsum(views)) |>
    dplyr::ungroup()
}

# Median cumulative views at `age` over the Signal Checks with two full weeks.
typical_at_age <- function(cumulative, coverage, age) {
  complete <- coverage$number[coverage$complete]
  at <- cumulative[cumulative$number %in% complete & cumulative$age == age, ]
  if (nrow(at) == 0L) NA_real_ else stats::median(at$cumulative)
}

# Each edition's events within its first `days` days (or as many as have
# passed).
edition_window_events <- function(events, editions,
                                  days = edition_window_days) {
  events |>
    dplyr::inner_join(editions, by = "pagepath_redacted") |>
    dplyr::filter(day >= published, day < published + days)
}

# Dwell per edition over its first `days` days.
edition_dwell <- function(events, editions, days = edition_window_days) {
  edition_window_events(events, editions, days) |>
    dwell_curve(c("number", "label")) |>
    dwell_summary(c("number", "label"))
}

# Dwell ------------------------------------------------------------------------

# The share of page views that reached each milestone, per group of `by`
# columns. Page views, not `totalusers`, since users cannot be summed.
dwell_curve <- function(events, by) {
  totals <- events |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "eventname")))) |>
    dplyr::summarise(events = sum(events), .groups = "drop")
  views <- totals |>
    dplyr::filter(eventname == "page_view") |>
    dplyr::select(dplyr::all_of(by), page_views = events)

  totals |>
    dplyr::filter(eventname %in% dwell_events) |>
    dplyr::mutate(seconds = dwell_marks[match(eventname, dwell_events)]) |>
    dplyr::select(dplyr::all_of(by), seconds, reached = events) |>
    # Every milestone, so one nobody reached reads as zero, not missing.
    dplyr::right_join(tidyr::crossing(views[by], seconds = dwell_marks),
                      by = c(by, "seconds")) |>
    dplyr::mutate(reached = dplyr::coalesce(reached, 0)) |>
    dplyr::inner_join(views, by = by) |>
    dplyr::filter(page_views > 0) |>
    dplyr::mutate(share = reached / page_views) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "seconds"))))
}

# The time half the page views were still open, interpolated on a log scale
# between the milestones either side of 50%. Zero when fewer than half
# reached a second; Inf when half were still open at the last milestone.
median_read <- function(seconds, share) {
  ordered <- order(seconds)
  seconds <- seconds[ordered]
  share <- share[ordered]
  above <- which(share >= 0.5)
  if (length(above) == 0L) return(0)
  i <- max(above)
  if (i == length(seconds)) return(Inf)
  step <- (share[i] - 0.5) / (share[i] - share[i + 1L])
  exp(log(seconds[i]) + step * (log(seconds[i + 1L]) - log(seconds[i])))
}

# The least time a curve's page views add up to, in seconds: each view that
# reached a milestone but not the next counts as the milestone alone, and the
# last milestone caps it.
reading_seconds <- function(seconds, reached) {
  ordered <- order(seconds)
  seconds <- seconds[ordered]
  reached <- reached[ordered]
  stopped_here <- reached - c(reached[-1], 0)
  sum(pmax(stopped_here, 0) * seconds)
}

# One row per group: page views, median dwell, the share past one and five
# minutes, and reading time.
dwell_summary <- function(curve, by) {
  curve |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      page_views = dplyr::first(page_views),
      median_seconds = median_read(seconds, share),
      past_minute = share[seconds == 60],
      past_five = share[seconds == 300],
      reading_seconds = reading_seconds(seconds, reached),
      .groups = "drop"
    )
}

# Dashboards ------------------------------------------------------------------

# The dashboard file a path belongs to; a renamed file reads as its new name.
dashboard_file <- function(path) {
  file <- sub(dashboard_pattern, "\\1", path)
  renamed <- dashboard_renames[file]
  ifelse(is.na(renamed), file, renamed)
}

# One row per dashboard: file, name (its newest title less "Dashboard") and
# first view.
dashboard_list <- function(traffic) {
  traffic |>
    dplyr::filter(page_kind == "dashboard") |>
    dplyr::mutate(original = sub(dashboard_pattern, "\\1", pagepath_redacted),
                  file = dashboard_file(pagepath_redacted)) |>
    dplyr::group_by(file) |>
    # The newest title the file carried under its current name.
    dplyr::arrange(original != file, dplyr::desc(day), .by_group = TRUE) |>
    dplyr::summarise(title = dplyr::first(pagetitle), first_seen = min(day),
                     .groups = "drop") |>
    dplyr::mutate(name = sub("\\s*Dashboard$", "", title)) |>
    dplyr::select(file, name, first_seen)
}

# The `$primary` in a dashboard folder's _theme.scss, named like its published
# file, so its bars match its navbar. NA when there is no such file or line.
dashboard_accent <- function(file, dashboards_dir = "..") {
  vapply(file, \(f) {
    path <- file.path(dashboards_dir, f, "_theme.scss")
    if (!file.exists(path)) return(NA_character_)
    found <- grep("^\\$primary:\\s*#[0-9A-Fa-f]{6}", readLines(path, warn = FALSE),
                  value = TRUE)
    if (length(found) == 0L) return(NA_character_)
    sub("^.*(#[0-9A-Fa-f]{6}).*$", "\\1", found[1])
  }, character(1), USE.NAMES = FALSE)
}

# Weeks ------------------------------------------------------------------------

# The Monday a day's week starts on. Weeks run Monday to Sunday, as on
# Sign-In Activity.
monday_of <- function(day) day - (as.integer(format(day, "%u")) - 1L)

# Window totals: the `days` days through `through`, and the same span before.
window_totals <- function(rows, value, through, days = 28L) {
  current <- rows$day > through - days & rows$day <= through
  prior <- rows$day > through - 2L * days & rows$day <= through - days
  c(current = sum(rows[[value]][current]), prior = sum(rows[[value]][prior]))
}

# Change ----------------------------------------------------------------------

# "up" / "down" / "flat"; a change under half a percent of the base reads as
# none, and an uncalculable one is flat so it formats as a dash.
delta_direction <- function(delta) {
  dplyr::case_when(
    is.na(delta) | abs(delta) <= 0.005 ~ "flat",
    delta > 0 ~ "up",
    .default = "down"
  )
}

# The arrow and sign carry direction, not colour alone. Nothing to compare
# against is a dash, distinct from a measured "no change".
fmt_delta <- function(delta) {
  direction <- delta_direction(delta)
  dplyr::case_when(
    is.na(delta) ~ "-",
    direction == "up" ~ sprintf("▲ %+.0f%%", delta * 100),
    direction == "down" ~ sprintf("▼ %+.0f%%", delta * 100),
    .default = "no change"
  )
}

delta_colours <- c(up = "#115740", down = "#AB2328", flat = "#5C6670")

# Relative change from `before` to `after`. Undefined against a zero base,
# which is a new arrival rather than a percentage rise.
relative_change <- function(after, before) {
  dplyr::if_else(before == 0 | is.na(before), NA_real_,
                 (after - before) / before)
}

# Formatting ------------------------------------------------------------------

# "Aug 8"
format_short_date <- function(d) {
  d <- as.Date(d)
  paste0(format(d, "%b"), " ", as.integer(format(d, "%d")))
}

# "August 8, 2026", not "August 08".
format_long_date <- function(d) {
  d <- as.Date(d)
  paste0(format(d, "%B"), " ", as.integer(format(d, "%d")), ", ",
         format(d, "%Y"))
}

# Spaces to non-breaking, so a value box value stays one Str and keeps its class.
nbsp <- function(x) gsub(" ", " ", x, fixed = TRUE)

# Short axis label for a milestone: "30s", "2m".
fmt_mark_short <- function(seconds) {
  ifelse(seconds < 60, paste0(seconds, "s"), paste0(seconds / 60, "m"))
}

# A median read in minutes to one decimal: "0.4 min", "1.4 min", "2 min".
# Zero when fewer than half the views stayed a second.
fmt_read <- function(seconds) {
  minutes <- as.character(round(seconds / 60, 1))
  dplyr::case_when(
    is.na(seconds) ~ "-",
    is.infinite(seconds) ~ "Over 10 min",
    .default = paste(minutes, "min")
  )
}

# Seconds as hours or minutes: "3.4 hours", "45 minutes".
fmt_reading <- function(seconds) {
  dplyr::case_when(
    is.na(seconds) ~ "-",
    seconds >= 3600 ~ paste(formatC(seconds / 3600, format = "f", digits = 1),
                            "hours"),
    .default = paste(round(seconds / 60), "minutes")
  )
}
