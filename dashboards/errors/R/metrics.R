# Errors metric layer: GA error events per 1,000 site-wide sessions, by week,
# for each feature and each error code. Google Analytics only. The
# thresholds are set at the top of the qmd setup chunk and passed in, so this
# file holds no tunable numbers.

# GA exports each day two days late: a Wednesday render sees Monday.
ga_lag_days <- 2L

local_tz <- "America/Toronto"

# Features as readers know them, by GA form_id. A form missing here still gets
# its own row, labelled with the form_id, so an out-of-date map only costs a
# friendlier name.
feature_of_form <- c(
  signin = "Sign in",
  signup = "Sign up",
  passwordrecovery = "Password recovery",
  fido2_enrollment_form = "Passkey setup",
  add_passkey = "Passkey setup",
  sms_enrollment_form = "2-step phone setup",
  phone_enrollment_form = "2-step phone setup",
  mfa_phone_number = "2-step phone setup",
  contact_phone_number_update = "Manage account settings",
  email_address_update = "Manage account settings",
  password_change = "Manage account settings",
  delete_passkey = "Manage account settings",
  delete_mfa_phone_number = "Manage account settings"
)

feature_label <- function(form_id) {
  form_id <- dplyr::coalesce(form_id, "(no form)")
  unname(ifelse(form_id %in% names(feature_of_form),
                feature_of_form[form_id], form_id))
}

# Forms with errors that the map does not name.
unlabelled_forms <- function(errors) {
  sort(unique(setdiff(dplyr::coalesce(errors$form_id, "(no form)"),
                      names(feature_of_form))))
}

# Today in Toronto. The runner's clock is UTC, which is a day ahead late in
# the Toronto evening.
toronto_today <- function(now = Sys.time()) {
  as.Date(format(now, "%Y-%m-%d", tz = local_tz))
}

monday_of <- function(day) day - (as.integer(format(day, "%u")) - 1L)

# The Monday-to-Sunday week the page reports: the last one GA has finished.
report_week <- function(today) {
  through <- today - ga_lag_days
  week_end <- through - as.integer(format(through, "%u")) %% 7L
  list(through = through, start = week_end - 6L, end = week_end)
}

# The Mondays of every week the page needs: the trend, which ends with the
# report week, and the history the oldest trend week is compared against.
weeks_needed <- function(week, thresholds) {
  n <- thresholds$trend_weeks + thresholds$history_weeks
  week$start - 7L * rev(seq_len(n) - 1L)
}

# Reads ----------------------------------------------------------------------

# Cache a read for the session. Every table reads the same rows, and the data
# cannot change mid-render.
read_once <- function(read) {
  cache <- new.env(parent = emptyenv())
  function(con, from) {
    key <- as.character(from)
    if (is.null(cache[[key]])) cache[[key]] <- read(con, key)
    cache[[key]]
  }
}

# `date` is varchar. noctua turns a date-shaped string into a date literal,
# which Athena will not compare with varchar, so the bound goes in as SQL.
since <- function(from) dbplyr::sql(glue::glue("\"date\" >= '{from}'"))

# GA error events per day, code and form, from `from` on.
error_rows <- read_once(\(con, from) {
  dplyr::tbl(con, dbplyr::in_schema("google_analytics", "error_events")) |>
    dplyr::filter(!!since(from)) |>
    dplyr::group_by(date, error_code, form_id) |>
    dplyr::summarise(events = sum(eventcount, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::transmute(day = as.Date(date), error_code, form_id,
                     events = as.numeric(events))
})

# Site-wide sessions per day.
session_days <- read_once(\(con, from) {
  dplyr::tbl(con, dbplyr::in_schema("google_analytics", "property_traffic")) |>
    dplyr::filter(!!since(from)) |>
    dplyr::select(date, sessions) |>
    dplyr::collect() |>
    dplyr::transmute(day = as.Date(date), sessions = as.numeric(sessions)) |>
    dplyr::arrange(day)
})

# Weeks ----------------------------------------------------------------------

# Events per key and week, with a zero for every week a key had none, so a
# quiet week reads as zero rather than missing. `errors` has day, key, events.
weekly_events <- function(errors, weeks) {
  errors |>
    dplyr::mutate(week = monday_of(day)) |>
    dplyr::filter(week %in% weeks) |>
    dplyr::group_by(key, week) |>
    dplyr::summarise(events = sum(events), .groups = "drop") |>
    tidyr::complete(key, week = weeks, fill = list(events = 0))
}

weekly_sessions <- function(sessions, weeks) {
  sessions |>
    dplyr::mutate(week = monday_of(day)) |>
    dplyr::filter(week %in% weeks) |>
    dplyr::group_by(week) |>
    dplyr::summarise(sessions = sum(sessions), .groups = "drop")
}

# Errors per 1,000 sessions per key and week.
weekly_rates <- function(errors, sessions, weeks) {
  weekly_events(errors, weeks) |>
    dplyr::left_join(weekly_sessions(sessions, weeks), by = "week") |>
    dplyr::mutate(rate = ifelse(sessions > 0, events / sessions * 1000,
                                NA_real_)) |>
    dplyr::arrange(key, week)
}

# Each key's report week against the median of the weeks before it.
#
# - new: no events in any of the history weeks, and some this week.
# - returning: new, but with events in an earlier week the page read.
# - flagged: not new, at least `min_events` events, and a rate at least
#   `ratio` times the typical week. A typical week of zero with events in the
#   history still counts, as an infinite ratio.
# - change: the rate against the typical week, NA when that is zero.
# - trend: the rate for each of the last `trend_weeks` weeks, oldest first.
score_weeks <- function(rates, week_start, thresholds) {
  history_from <- week_start - 7L * thresholds$history_weeks
  trend_from <- week_start - 7L * (thresholds$trend_weeks - 1L)

  rates |>
    dplyr::group_by(key) |>
    # The history first: summarise() sees each new column, so `rate` and
    # `events` must still be the weekly ones when it is read.
    dplyr::summarise(
      typical = stats::median(rate[week >= history_from & week < week_start]),
      history_events = sum(events[week >= history_from & week < week_start]),
      earlier_events = sum(events[week < history_from]),
      trend = list(rate[week >= trend_from & week <= week_start]),
      events = sum(events[week == week_start]),
      rate = sum(rate[week == week_start]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      new = history_events == 0 & events > 0,
      returning = new & earlier_events > 0,
      ratio = rate / typical,
      flagged = !new & events >= thresholds$min_events &
        !is.na(ratio) & ratio >= thresholds$ratio,
      change = ifelse(typical > 0, rate / typical - 1, NA_real_)
    )
}

# Messages ---------------------------------------------------------------------

no_messages <- tibble::tibble(error_code = character(), message = character())

# In brackets, so it cannot be read as the message itself: it is the dashboard
# not knowing the code.
no_message <- "(No error message on record)"

# One message per code, from the error code catalogue in the publishing repo.
# The text users see, newest first: gadrift's latest dated capture, else the
# first place the software defines it. Failing that, IBM Verify's reference
# text, marked as IBM's. No messages without the catalogue. gadrift's captures
# lead with the id ("006: Invalid ..."), which is dropped.
code_messages <- function(catalogue) {
  entries <- catalogue$codes
  if (length(entries) == 0) return(no_messages)
  message <- purrr::map_chr(entries, \(entry) {
    shown <- purrr::keep(entry$shown %||% list(), \(s) !is.null(s$en))
    if (length(shown) > 0) {
      captured <- purrr::map_chr(shown, \(s) s$captured %||% NA_character_)
      latest <- if (all(is.na(captured))) 1L else which.max(as.Date(captured))
      return(sub("^\\d{3}: ", "", shown[[latest]]$en))
    }
    ibm <- trimws(entry$ibm_reference %||% "")
    if (nzchar(ibm)) return(paste("IBM Verify:", ibm))
    no_message
  })
  tibble::tibble(error_code = names(entries), message = unname(message))
}

# HTML with a hover tooltip, or the HTML alone where `tip` is NA.
tip_html <- function(html, tip) {
  ifelse(
    is.na(tip), html,
    paste0('<span class="hover-tip" title="',
           htmltools::htmlEscape(tip, attribute = TRUE), '">', html, "</span>")
  )
}

# A code as HTML, with its message as a hover tooltip. A code missing from the
# catalogue says so; with no catalogue at all, codes have no tooltip.
code_html <- function(code, messages = no_messages) {
  message <- messages$message[match(code, messages$error_code)]
  if (nrow(messages) > 0) message[is.na(message)] <- no_message
  tip_html(paste0("<code>", htmltools::htmlEscape(code), "</code>"), message)
}

# A feature name as HTML, with what it counts as a hover tooltip: every form
# the map assigns it, whether or not they had errors this week.
feature_html <- function(feature) {
  tip <- purrr::map_chr(feature, \(name) {
    forms <- names(feature_of_form)[feature_of_form == name]
    if (length(forms) == 0) return("(No feature name yet: shown by its form ID)")
    paste(forms, collapse = "\n")
  })
  tip_html(htmltools::htmlEscape(feature), tip)
}

# Tables ---------------------------------------------------------------------

# The top codes in each feature this week as HTML, one per line: a bar for the
# code's share of the feature's errors, then the code. The bar has no number,
# so codes like 26 are never read as one; a screen reader hears the share.
top_codes <- function(errors, week_start, n, messages = no_messages) {
  errors |>
    dplyr::filter(day >= week_start, day <= week_start + 6L) |>
    dplyr::mutate(feature = feature_label(form_id)) |>
    dplyr::group_by(feature, error_code) |>
    dplyr::summarise(events = sum(events), .groups = "drop_last") |>
    dplyr::mutate(share = events / sum(events)) |>
    dplyr::arrange(dplyr::desc(events), error_code, .by_group = TRUE) |>
    dplyr::slice_head(n = n) |>
    dplyr::summarise(
      codes = paste0(
        '<span class="code-share"><span class="share-bar" role="img" ',
        'aria-label="', fmt_share(share), ' of errors" style="--share: ',
        round(share * 100, 1), '%"></span>', code_html(error_code, messages),
        "</span>", collapse = ""
      ),
      .groups = "drop"
    )
}

# One row per feature: the mapped forms and any unlabelled form. Flagged
# first, then by events.
feature_table <- function(errors, sessions, week, thresholds,
                          messages = no_messages) {
  weeks <- weeks_needed(week, thresholds)

  ga <- errors |>
    dplyr::transmute(day, key = feature_label(form_id), events)

  rates <- weekly_rates(ga, sessions, weeks)
  codes <- top_codes(errors, week$start, thresholds$feature_codes, messages)

  score_weeks(rates, week$start, thresholds) |>
    dplyr::rename(feature = key) |>
    dplyr::left_join(codes, by = "feature") |>
    dplyr::mutate(codes = dplyr::coalesce(codes, "-")) |>
    dplyr::arrange(dplyr::desc(flagged), dplyr::desc(events), feature)
}

# Where a code happened most this week, by feature.
main_feature <- function(errors, week_start) {
  errors |>
    dplyr::filter(day >= week_start, day <= week_start + 6L) |>
    dplyr::mutate(feature = feature_label(form_id)) |>
    dplyr::group_by(error_code, feature) |>
    dplyr::summarise(events = sum(events), .groups = "drop_last") |>
    dplyr::arrange(dplyr::desc(events), feature, .by_group = TRUE) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(error_code, feature)
}

# New and returning codes with at least `new_min_events` events (a single
# stray error is not worth a row), then every other code with at least
# `list_min_events`, each part by volume. A minimum, not a rank: below it the
# changes swing on a handful of errors. Flagged codes need more events than
# either, so they are always listed.
code_table <- function(errors, sessions, week, thresholds) {
  weeks <- weeks_needed(week, thresholds)
  rates <- weekly_rates(dplyr::transmute(errors, day, key = error_code, events),
                        sessions, weeks)

  scored <- score_weeks(rates, week$start, thresholds) |>
    dplyr::rename(error_code = key) |>
    dplyr::filter(events > 0)

  arrivals <- scored |>
    dplyr::filter(new, events >= thresholds$new_min_events) |>
    dplyr::arrange(dplyr::desc(events), error_code)
  regulars <- scored |>
    dplyr::filter(!new, events >= thresholds$list_min_events) |>
    dplyr::arrange(dplyr::desc(events), error_code)

  dplyr::bind_rows(arrivals, regulars) |>
    dplyr::left_join(main_feature(errors, week$start), by = "error_code")
}

# Distinct codes in the report week, and the median over the history weeks.
active_codes <- function(errors, week, thresholds) {
  weeks <- weeks_needed(week, thresholds)
  history <- utils::tail(weeks, thresholds$history_weeks + 1L)
  counts <- errors |>
    dplyr::filter(events > 0) |>
    dplyr::mutate(week = monday_of(day)) |>
    dplyr::filter(week %in% history) |>
    dplyr::distinct(week, error_code) |>
    dplyr::count(week) |>
    tidyr::complete(week = history, fill = list(n = 0L))
  list(now = counts$n[counts$week == week$start],
       typical = stats::median(counts$n[counts$week < week$start]))
}

# The feature with the most errors this week, and its share of them.
busiest_feature <- function(features) {
  top <- dplyr::slice_max(features, events, n = 1, with_ties = FALSE)
  list(feature = top$feature, share = top$events / sum(features$events))
}

# All GA errors per 1,000 sessions: the report week and its typical week.
overall_rate <- function(errors, sessions, week, thresholds) {
  weeks <- weeks_needed(week, thresholds)
  rates <- weekly_rates(dplyr::transmute(errors, day, key = "all", events),
                        sessions, weeks)
  score_weeks(rates, week$start, thresholds)
}

# Errors per 1,000 sessions for every week the page reads, for the charts:
# all errors as one series, or one series per feature.
chart_weeks <- function(errors, sessions, week, thresholds, by_feature = FALSE) {
  key <- if (by_feature) feature_label(errors$form_id) else rep("all", nrow(errors))
  weekly_rates(tibble::tibble(day = errors$day, key = key, events = errors$events),
               sessions, weeks_needed(week, thresholds))
}

# Formatting -----------------------------------------------------------------

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

# Errors per 1,000 sessions: one decimal, which small rates need.
fmt_rate <- function(x) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = 1, big.mark = ","))
}

# "▲ +150%" against the typical week; "New" or "Returning" with no errors in
# the history; "▲ from 0" with some errors in the history but a typical week
# of none.
fmt_against <- function(change, new, returning, typical, events) {
  dplyr::case_when(
    returning ~ "Returning",
    new ~ "New",
    typical == 0 & events > 0 ~ "\u25b2 from 0",
    .default = fmt_delta(change)
  )
}

# "88%", or "<1%" rather than a misleading "0%".
fmt_share <- function(x) {
  ifelse(x < 0.005, "<1%", scales::percent(x, accuracy = 1))
}

# The shared change_verdict() for errors, where down is good. Rising from a
# typical week of zero counts as a rise; a new or returning code has no arrow,
# so no verdict.
error_verdict <- function(change, new, typical, events) {
  delta <- ifelse(!new & typical == 0 & events > 0, Inf, change)
  verdict <- change_verdict(delta, good = "down")
  verdict[new] <- NA_character_
  verdict
}

# Spaces to non-breaking, so a value box value stays one Str and keeps its class.
nbsp <- function(x) gsub(" ", "\u00a0", x, fixed = TRUE)
