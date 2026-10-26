# Support metric layer, over the two call centre tables and the PSOM board.

# Summary figure is over this many reported weeks, compared against the
# same number before it.
summary_window_weeks <- 4L

# The same span in days for the Jira side
summary_window_days <- summary_window_weeks * 7L

# A week ends Saturday and its report is emailed/ETL'd the Wednesday or Thursday
# after, so that week is due once this many days have passed since it ended.
call_centre_report_due_days <- 5L

# The snapshot runs nightly (weekly on Mondays until 2026-09-28), so the newest
# partition is today's or, before tonight's run, yesterday's.
psom_lag_days <- 1L

# Reads ----------------------------------------------------------------------

# Read a table once and cache it for the session
read_once <- function(schema, name, prepare) {
  cache <- new.env(parent = emptyenv())
  function(con) {
    if (!is.null(cache$value)) {
      return(cache$value)
    }
    raw <- dplyr::tbl(con, dbplyr::in_schema(schema, name)) |>
      dplyr::collect()
    cache$value <- raw |>
      dplyr::mutate(dplyr::across(dplyr::where(bit64::is.integer64), as.numeric)) |>
      prepare()
    cache$value
  }
}

# One row per reported week, Sunday to Saturday. The date columns are text.
call_weeks <- read_once("call_centre", "weekly_activity_report", \(x) {
  x |>
    dplyr::transmute(
      week = as.Date(date_range_start),
      week_end = as.Date(date_range_end),
      calls_accepted,
      test_calls = dplyr::coalesce(test_calls, 0),
      calls = calls_accepted - test_calls,
      calls_answered,
      calls_abandoned,
      callbacks = callback,
      pct_answered_within_60s,
      avg_delay_seconds,
      avg_call_length_seconds
    ) |>
    dplyr::arrange(week)
})

# One row per topic per call. One call can have many topics
call_topics <- read_once("call_centre", "weekly_topic_dump", \(x) {
  x |>
    dplyr::transmute(
      call_no,
      call_date = as.Date(call_date),
      topic = stringr::str_squish(statobject),
      language,
      clienttype
    ) |>
    dplyr::mutate(category = topic_category(topic))
})

# Daily CanadaLogin-wide active users, the denominator of the call rate
daily_active_users <- read_once(
  "ibm_verify_events_transformed", "events_auth_total_logins",
  \(x) {
    x |>
      dplyr::transmute(date = as.Date(date), unique_users) |>
      dplyr::arrange(date)
  }
)

# Every snapshot of the board. `key` recurs once per snapshot.
psom_snapshots <- read_once("jira", "psom", \(x) {
  x |>
    dplyr::transmute(
      snapshot = as.Date(date),
      key, summary, issue_type, status, organizations, form_topic,
      created = as.Date(substr(created, 1, 10)),
      status_changed = as.Date(substr(status_changed, 1, 10))
    )
})

# Every snapshot of each ticket's SLA clocks, one row per ticket per SLA.
# `due` is text with its own offset, so its first ten characters are the local day.
psom_sla_snapshots <- read_once("jira", "psom_sla", \(x) {
  x |>
    dplyr::transmute(
      snapshot = as.Date(date),
      key, sla, breached, paused,
      due_on = as.Date(substr(due, 1, 10)),
      goal_hours = goal_minutes / 60,
      elapsed_hours = elapsed_minutes / 60
    )
})

# The newest snapshot, which is the board as it stands.
psom_latest <- function(con) {
  rows <- psom_snapshots(con)
  dplyr::filter(rows, snapshot == max(snapshot))
}

# The last day the call centre has reported.
call_centre_data_through <- function(con) max(call_weeks(con)$week_end)

# The day the newest snapshot was taken
psom_snapshot_date <- function(con) max(psom_snapshots(con)$snapshot)

# Windows ---------------------------------------------------------------------

# The `weeks` reported weeks ending on or before `as_of`, newest last.
recent_weeks <- function(rows, as_of, weeks = summary_window_weeks) {
  rows |>
    dplyr::filter(week_end <= as.Date(as_of)) |>
    dplyr::slice_tail(n = weeks)
}

# The window before that one, for comparison.
prior_weeks <- function(rows, as_of, weeks = summary_window_weeks) {
  current <- recent_weeks(rows, as_of, weeks)
  if (nrow(current) == 0) {
    return(current)
  }
  recent_weeks(rows, min(current$week) - 1L, weeks)
}

# Tickets created in the `days` days ending on `as_of` inclusive.
created_in_window <- function(rows, as_of, days = summary_window_days) {
  as_of <- as.Date(as_of)
  dplyr::filter(rows, created > as_of - days, created <= as_of)
}

# A weekly rate pooled across weeks, weighted by answered calls
weighted_weekly <- function(rows, column) {
  weights <- rows$calls_answered
  if (sum(weights, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  sum(rows[[column]] * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
}

# Call rate -------------------------------------------------------------------

# Calls per 100 active users, see the Cookbook details
call_rate <- function(weeks, users) {
  week_user_days <- function(from, to) {
    sum(users$unique_users[users$date >= from & users$date <= to], na.rm = TRUE)
  }
  week_covered_days <- function(from, to) sum(users$date >= from & users$date <= to)

  weeks |>
    dplyr::mutate(
      user_days = purrr::map2_dbl(week, week_end, week_user_days),
      calls_per_100 = dplyr::if_else(
        purrr::map2_int(week, week_end, week_covered_days) ==
          as.integer(week_end - week) + 1L,
        calls / user_days * 100,
        NA_real_
      )
    )
}

# Topics -----------------------------------------------------------------------

# A map of call topics to categories, maintained by hand
topic_categories_file <- "topic-categories.csv"

topic_lookup_rows <- utils::read.csv(topic_categories_file, colClasses = "character")
topic_lookup <- stats::setNames(topic_lookup_rows$category,
                                stringr::str_squish(topic_lookup_rows$topic))

# Fallback regular expression for topics that haven't made it into topic-categories.csv
# nolint start: line_length_linter. Keep one regex alternation per line.
topic_patterns <- c(
  "What is CanadaLogin\\?|What is 2-Step Verification|What Is a Passkey|Password Criteria" =
    "What is CanadaLogin",
  "^How to |^Changing |^Deleting " =
    "Account setup and how-to",
  "2-Step|Passkey" =
    "2-step verification problems",
  "Verifying the Email Address" =
    "Email verification problems",
  "Difficulties When Signing [Ii]n|Technical Difficulties|Creating a Password" =
    "Sign-in problems",
  "Outage" =
    "Service outages",
  "Follow-Up Procedure" =
    "Follow-up requests",
  "PrairiesCan|GC Digital Talent|VAC Healthshare|CED Client Space|MyCGC|O-Canada|ATIP|Access to Information" =
    "Partner and other services"
)
# nolint end

# If the topic isn't in the file and doesn't match any regex, it gets "Other"
# If "Other" becomes too large, update topic-categories.csv
topic_category_levels <- c(
  unique(c(unname(topic_patterns), unname(topic_lookup))),
  "Other"
)

# The first pattern that matches, or "Other".
topic_category_fallback <- function(topic) {
  out <- rep("Other", length(topic))
  for (pattern in rev(names(topic_patterns))) {
    out[grepl(pattern, topic)] <- topic_patterns[[pattern]]
  }
  out
}

topic_category <- function(topic) {
  topic <- stringr::str_squish(topic)
  out <- unname(topic_lookup[topic])
  uncovered <- is.na(out)
  out[uncovered] <- topic_category_fallback(topic[uncovered])
  factor(out, levels = topic_category_levels)
}

# Informational or troubleshooting, from the type column of topic-categories.csv.
# A category with no type, or with both, is troubleshooting.
topic_category_type <- function(category) {
  category <- as.character(category)
  informational <- topic_lookup_rows |>
    dplyr::distinct(category, type) |>
    dplyr::group_by(category) |>
    dplyr::filter(dplyr::n() == 1, type == "Informational") |>
    dplyr::pull(category)
  ifelse(category %in% informational, "Informational", "Troubleshooting")
}

# Topic rows for the calls placed inside a set of reported weeks.
topics_in_weeks <- function(topics, weeks) {
  if (nrow(weeks) == 0) {
    return(topics[0, ])
  }
  dplyr::filter(topics, call_date >= min(weeks$week), call_date <= max(weeks$week_end))
}

# PSOM -----------------------------------------------------------------------

# A ticket in one of these is complete
psom_closed_statuses <- c("Done", "Canceled", "Ready to archive")

psom_open <- function(rows) dplyr::filter(rows, !status %in% psom_closed_statuses)

# The Monday of the week a date falls in
week_of <- function(d) as.Date(d) - (as.integer(format(as.Date(d), "%u")) - 1L)

# The day a ticket first reached a closed status, NA while it is still open.
# Read as the earliest status change on a closed row, so a ticket that went
# Done and then Ready to archive keeps the day it was done.
psom_closed_on <- function(rows) {
  rows |>
    dplyr::filter(status %in% psom_closed_statuses) |>
    dplyr::group_by(key) |>
    dplyr::summarise(closed_on = min(status_changed), .groups = "drop")
}

# Every ticket ever on the board, each from its own last snapshot, with the day
# it closed attached. Not the newest snapshot alone: a ticket archived out of
# Jira leaves it, and would drop out of the opened and closed history.
psom_board <- function(rows) {
  rows |>
    dplyr::group_by(key) |>
    dplyr::slice_max(snapshot, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(psom_closed_on(rows), by = "key")
}

# Tickets closed in the `days` days ending on `as_of` inclusive.
closed_in_window <- function(rows, as_of, days = summary_window_days) {
  as_of <- as.Date(as_of)
  dplyr::filter(rows, !is.na(closed_on), closed_on > as_of - days, closed_on <= as_of)
}

# Days open, to the close or to `as_of`. Everything closed in the window plus
# everything still open, because a cohort by creation date is right-censored.
psom_ages <- function(board, as_of, days = summary_window_days) {
  as_of <- as.Date(as_of)
  board |>
    # A ticket opened after as_of did not exist yet, and would read a negative age
    dplyr::filter(created <= as_of, is.na(closed_on) | closed_on > as_of - days) |>
    dplyr::mutate(age = as.integer(dplyr::coalesce(closed_on, as_of) - created))
}

# The first day of the month a date falls in
month_of <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))

# The queue chart never starts before this month
psom_chart_start <- as.Date("2026-08-01")

# The first of the earliest month on the queue chart: `months` calendar months
# ending with the one `through` falls in, but not before psom_chart_start.
psom_months_from <- function(through, months = 13L) {
  back <- seq(month_of(through), by = "-1 month", length.out = months)
  max(min(back), psom_chart_start)
}

# One row per calendar month from `from` to the month `through` falls in:
# opened, closed, and open at the end of it. The last month runs only to
# `through` and is flagged `partial` when that is before its end. All read off
# the board's own dates, so open is the running total of the rest.
psom_monthly <- function(life, from, through) {
  through <- as.Date(through)
  starts <- seq(month_of(from), month_of(through), by = "1 month")
  month_ends <- seq(starts[1], by = "1 month",
                    length.out = length(starts) + 1L)[-1] - 1L
  ends <- pmin(month_ends, through)

  count_in <- function(d) {
    vapply(seq_along(starts), \(i) sum(!is.na(d) & d >= starts[i] & d <= ends[i]),
           integer(1))
  }
  tibble::tibble(
    month = starts,
    month_end = ends,
    partial = ends < month_ends,
    opened = count_in(life$created),
    closed = count_in(life$closed_on),
    open = vapply(ends, \(e) {
      sum(life$created <= e & (is.na(life$closed_on) | life$closed_on > e))
    }, integer(1))
  )
}

# PSOM SLAs ------------------------------------------------------------------

# The first daily snapshot. Earlier ones are weekly backfills with no elapsed
# time and no PSO clock, so nothing that stopped before its week is reported.
psom_sla_from <- as.Date("2026-09-29")

# The clocks on the scorecard, in the order a ticket meets them.
# `escalated_to_pt` runs while a ticket is with the Auth product team.
sla_clocks <- c(
  first_response   = "Time to first response",
  pso_time_to_done = "PSO handling",
  escalated_to_pt  = "Auth product team handling",
  time_to_done     = "Time to done, whole ticket"
)

# Each ticket's clocks as last seen. An archived ticket leaves the snapshots,
# so its last one stands rather than the newest.
psom_sla_last <- function(rows) {
  rows |>
    dplyr::filter(snapshot >= psom_sla_from) |>
    dplyr::group_by(key, sla) |>
    dplyr::slice_max(snapshot, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

# One row per stopped clock on the scorecard, met or missed, on the day it
# stopped. As in Jira's SLA success rate report, a running clock counts nowhere
# until it stops, even past target. `due` is the stop time on a stopped clock.
sla_outcomes <- function(last, through) {
  last |>
    dplyr::filter(sla %in% names(sla_clocks), !is.na(breached), is.na(paused)) |>
    dplyr::mutate(
      outcome = dplyr::if_else(breached, "missed", "met"),
      stopped_on = due_on,
      stopped_week = week_of(stopped_on)
    ) |>
    dplyr::filter(stopped_on >= week_of(psom_sla_from),
                  stopped_on <= as.Date(through))
}

# The clocks on the scorecard for the tickets in `keys`, as of the newest
# snapshot, one row per ticket per clock. Covers what sla_outcomes() leaves
# out: clocks still running or paused, past target or not.
sla_open_clocks <- function(rows, keys) {
  rows |>
    dplyr::filter(snapshot == max(snapshot), key %in% keys,
                  sla %in% names(sla_clocks))
}

# Where a clock stands: "met" or "missed" once stopped, else "running" or
# "paused", "past target" added when it has run over.
sla_state <- function(breached, paused) {
  over <- dplyr::if_else(breached %in% TRUE, ", past target", "")
  dplyr::case_when(
    is.na(breached) ~ "not started",
    is.na(paused) ~ dplyr::if_else(breached, "missed", "met"),
    paused ~ paste0("paused", over),
    .default = paste0("running", over)
  )
}

# Jira's page for a ticket
psom_ticket_url <- function(key) paste0("https://jtickets.atlassian.net/browse/", key)

# 157 for "PSOM-157", so tickets sort in the order they were opened
psom_key_number <- function(key) as.integer(sub("^.*-", "", key))

# One row per ticket open on the newest snapshot or closed on or after `from`,
# oldest first, with where it stands on every SLA. SLA columns are blank for a
# ticket last seen before `psom_sla_from`.
psom_ticket_export <- function(snapshots, sla_snapshots, from) {
  newest <- max(snapshots$snapshot)
  # A reopened ticket keeps its old close date on the board, so it is cleared here
  tickets <- psom_board(snapshots) |>
    dplyr::mutate(closed_on = dplyr::if_else(status %in% psom_closed_statuses,
                                             closed_on, as.Date(NA))) |>
    dplyr::filter((snapshot == newest & is.na(closed_on)) |
                    closed_on >= as.Date(from))

  clocks <- psom_sla_last(sla_snapshots) |>
    dplyr::filter(key %in% tickets$key, sla %in% names(sla_clocks)) |>
    dplyr::transmute(
      key,
      sla = factor(sla, levels = names(sla_clocks)),
      state = sla_state(breached, paused),
      hours = round(elapsed_hours, 1),
      target_hours = goal_hours
    ) |>
    tidyr::pivot_wider(names_from = sla,
                       values_from = c(state, hours, target_hours),
                       names_glue = "{sla}_{.value}", names_vary = "slowest",
                       names_expand = TRUE)

  tickets |>
    dplyr::arrange(psom_key_number(key)) |>
    dplyr::select(ticket = key, summary, status, opened = created,
                  closed = closed_on) |>
    dplyr::left_join(clocks, by = c(ticket = "key"))
}

# Monday of each of the `weeks` complete weeks ending on or before `through`,
# oldest first. The current week is left off, and so is
# any week before the first daily snapshot's.
sla_weeks <- function(through, weeks = summary_window_weeks) {
  last <- week_of(as.Date(through) + 1L) - 7L
  starts <- seq(last - 7L * (weeks - 1L), last, by = "7 days")
  starts[starts >= week_of(psom_sla_from)]
}

# Met and stopped clocks, the share met and the median working hours, per
# group. Pass grouped rows.
sla_tally <- function(outcomes) {
  outcomes |>
    dplyr::summarise(
      met = sum(outcome == "met"),
      stopped = dplyr::n(),
      share = met / stopped,
      median_hours = stats::median(elapsed_hours),
      .groups = "drop"
    )
}

# sla_tally() per clock per week, every week present even when empty. `share`
# and the hours are NA where nothing stopped.
sla_weekly <- function(outcomes, weeks) {
  tidyr::expand_grid(sla = names(sla_clocks), week = weeks) |>
    dplyr::left_join(
      outcomes |>
        dplyr::group_by(sla, week = stopped_week) |>
        sla_tally(),
      by = c("sla", "week")
    ) |>
    dplyr::mutate(dplyr::across(c(met, stopped), \(x) dplyr::coalesce(x, 0L)))
}

# A clock's target today, in working hours: the goal on the newest ticket that
# has one. Jira does not re-goal old tickets when a target changes.
sla_current_target <- function(last, clock) {
  goals <- dplyr::filter(last, sla == clock, !is.na(goal_hours))
  if (nrow(goals) == 0L) {
    NA_real_
  } else {
    goals$goal_hours[which.max(psom_key_number(goals$key))]
  }
}

# "Target: 16 working hours"
sla_target_label <- function(last, clock) {
  target <- sla_current_target(last, clock)
  if (is.na(target)) {
    "No target set"
  } else {
    glue::glue("Target: {format(target)} working hours")
  }
}

# Handling ---------------------------------------------------------------------

# The statuses that hand a ticket to another team, and what the page calls it
psom_escalation_statuses <- c(
  "Escalated to PTT"     = "Escalated to the Partner Transition team",
  "Escalated to Auth PT" = "Escalated to the Auth product team"
)

psom_handled_by_levels <- c("PSO alone", unname(psom_escalation_statuses))

# One row per ticket opened in the `days` days ending on `as_of`, cancelled ones
# left out, with who handled it: the first team it was escalated to, else PSO
# alone. A ticket still open and not yet escalated counts as PSO alone.
psom_handled_by <- function(snapshots, as_of, days = summary_window_days) {
  first_escalation <- snapshots |>
    dplyr::filter(status %in% names(psom_escalation_statuses)) |>
    dplyr::group_by(key) |>
    dplyr::slice_min(snapshot, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(key, escalated_to = psom_escalation_statuses[status])

  psom_board(snapshots) |>
    created_in_window(as_of, days) |>
    dplyr::filter(status != "Canceled") |>
    dplyr::left_join(first_escalation, by = "key") |>
    dplyr::transmute(
      key,
      handled_by = factor(dplyr::coalesce(escalated_to, "PSO alone"),
                          levels = psom_handled_by_levels)
    )
}

# Tickets per form topic, for the tickets in `board`. A ticket with two topics
# counts in both; one with none is "No topic given". The form groups its topics
# into sections, named before the " - ".
psom_topic_counts <- function(board) {
  board |>
    dplyr::transmute(key, topic = dplyr::coalesce(form_topic, "")) |>
    tidyr::separate_longer_delim(topic, delim = ";") |>
    dplyr::mutate(topic = trimws(topic)) |>
    dplyr::distinct(key, topic) |>
    dplyr::mutate(
      topic = dplyr::if_else(topic == "", "No topic given", topic),
      section = dplyr::if_else(grepl(" - ", topic, fixed = TRUE),
                               sub(" - .*$", "", topic), "No topic given"),
      topic = sub("^.*? - ", "", topic)
    ) |>
    dplyr::count(section, topic, name = "tickets") |>
    dplyr::arrange(dplyr::desc(tickets))
}

# Change ----------------------------------------------------------------------

# Relative change from `before` to `after`. Undefined against a zero base,
# which is a new arrival rather than a percentage rise.
relative_change <- function(after, before) {
  dplyr::if_else(before == 0 | is.na(before), NA_real_,
                 (after - before) / before)
}

# "No calls", "One call", "Five calls", "23 calls"
fmt_count <- function(n, noun = NULL) {
  words <- c("One", "Two", "Three", "Four", "Five",
             "Six", "Seven", "Eight", "Nine", "Ten")
  count <- if (n == 0) "No" else if (n <= 10) words[n] else format(n, big.mark = ",")
  if (is.null(noun)) {
    count
  } else {
    paste(count, if (n == 1) noun else paste0(noun, "s"))
  }
}

# "20 seconds" or "7.0 minutes", for a value box. Prevents 0:20 being read as 20 mins
fmt_duration <- function(seconds) {
  if (is.na(seconds)) {
    "-"
  } else if (seconds < 90) {
    whole <- round(seconds)
    paste0(whole, "&nbsp;", if (whole == 1) "second" else "seconds")
  } else {
    paste0(sprintf("%.1f", seconds / 60), "&nbsp;minutes")
  }
}

# "4 days" or "3.5 days", for a value box. The nbsp keeps the value box class.
fmt_days <- function(days) {
  if (is.na(days)) {
    "-"
  } else {
    whole <- if (days == round(days)) format(round(days)) else sprintf("%.1f", days)
    paste0(whole, "&nbsp;", if (days == 1) "day" else "days")
  }
}

# "88%", or NA for a share that cannot be calculated
fmt_pct <- function(share) {
  dplyr::if_else(is.na(share), NA_character_, sprintf("%.0f%%", share * 100))
}

# "<1 min", "54 min", "5.1 hrs" or "38 hrs", for a table cell
fmt_work_time <- function(hours) {
  minutes <- round(hours * 60)
  dplyr::case_when(
    is.na(hours) ~ "-",
    minutes < 1 ~ "<1 min",
    minutes < 60 ~ paste(minutes, "min"),
    hours < 10 ~ paste(sprintf("%.1f", hours), "hrs"),
    .default = paste(sprintf("%.0f", hours), "hrs")
  )
}

# "1:45" for 105 seconds, for a table column, where the compact form fits and
# the header carries the unit.
fmt_minutes <- function(seconds) {
  # Round before splitting to avoid values such as 0:60.
  whole <- round(seconds)
  ifelse(
    is.na(seconds), "-",
    sprintf("%d:%02d", as.integer(whole %/% 60), as.integer(whole %% 60))
  )
}
