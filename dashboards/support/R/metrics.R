# Support metric layer, over the two call centre tables and the PSOM board.

# Summary figure is over this many reported weeks, compared against the
# same number before it.
summary_window_weeks <- 4L

# The same span in days for the Jira side
summary_window_days <- summary_window_weeks * 7L

# A week's report is emailed/ETL'd on the Wednesday or Thursday after the week ends,
# so the newest week to expect ended within this many days of today
call_centre_lag_days <- 13L

# The snapshot lands early Monday, so the newest partition to expect is within a
# week of today.
psom_lag_days <- 8L

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
daily_active_users <- read_once("ibm_verify", "auth_total_logins", \(x) {
  x |>
    dplyr::transmute(date = as.Date(date), unique_users) |>
    dplyr::arrange(date)
})

# Every weekly snapshot of the board. `key` recurs once per snapshot.
psom_snapshots <- read_once("jira", "psom", \(x) {
  x |>
    dplyr::transmute(
      snapshot = as.Date(date),
      key, summary, issue_type, status, organizations,
      created = as.Date(substr(created, 1, 10)),
      status_changed = as.Date(substr(status_changed, 1, 10))
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
topic_patterns <- c(
  "What is CanadaLogin\\?|What is 2-Step Verification|What Is a Passkey|Password Criteria" =
    "What is CanadaLogin",
  "^How to |^Changing |^Deleting |Creating a Password" =
    "Account setup and how-to",
  "2-Step|Passkey" =
    "2-step verification problems",
  "Verifying the Email Address" =
    "Email verification problems",
  "Difficulties When Signing [Ii]n|Technical Difficulties" =
    "Sign-in problems",
  "Follow-Up Procedure" =
    "Follow-up requests",
  "PrairiesCan|GC Digital Talent|VAC Healthshare|CED Client Space|MyCGC|O-Canada|ATIP" =
    "Partner and other services"
)

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

# Open tickets at every snapshot
psom_backlog <- function(rows) {
  rows |>
    dplyr::group_by(snapshot) |>
    dplyr::summarise(
      open = sum(!status %in% psom_closed_statuses),
      escalated = sum(grepl("^Escalated", status)),
      .groups = "drop"
    ) |>
    dplyr::arrange(snapshot)
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

# "1:45" for 105 seconds, for a table column, where the compact form fits and
# the header carries the unit.
fmt_minutes <- function(seconds) {
  ifelse(
    is.na(seconds), "-",
    sprintf("%d:%02d", as.integer(seconds %/% 60), as.integer(round(seconds %% 60)))
  )
}
