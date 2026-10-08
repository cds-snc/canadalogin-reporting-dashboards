# Automated traffic metric layer: three detection methods over three sources.
# The thresholds every method reads are set at the top of the qmd setup chunk
# and passed in, so this file holds no tunable numbers.

# The day CanadaLogin launched, and the first day the IBM Verify sources cover.
canadalogin_launch <- as.Date("2026-04-22")

# Every day is a Toronto day. mfa_activity and GA already are; the event
# stream's time is a UTC string, converted on read.
local_tz <- "America/Toronto"

# The methods, in the order they appear in the navbar and every table. `page`
# is the page heading's id, for links from the Overview.
methods <- tibble::tribble(
  ~method,    ~label,           ~page,
  "sms",      "SMS codes",      "sms-codes",
  "location", "SMS location",   "sms-location",
  "idle",     "Idle accounts",  "idle-accounts",
  "speed",    "Sign-up speed",  "sign-up-speed",
  "errors",   "Error patterns", "error-patterns"
)

# GA error codes in the bot signature. The two SMS limit codes are one limit.
signature_groups <- c(
  PHONE_VALIDATION_ERROR = "phone_invalid",
  CSIBN0081E = "sms_limit",
  CSIAP3512E = "sms_limit",
  CSIAH0004E = "interrupted",
  CSIAH2417E = "wrong_code"
)

signature_labels <- c(
  phone_invalid = "Invalid phone number",
  sms_limit = "SMS limit reached",
  interrupted = "Sign-in interrupted",
  wrong_code = "Wrong one-time code"
)

# Today in Toronto. The runner's clock is UTC, which is a day ahead late in
# the Toronto evening.
toronto_today <- function(now = Sys.time()) {
  as.Date(format(now, "%Y-%m-%d", tz = local_tz))
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

# Adds any of `columns` missing from `df` as zeros, so a result or error group
# with no rows on any day still gets its column.
with_columns <- function(df, columns) {
  for (column in setdiff(columns, names(df))) df[[column]] <- 0
  df
}

# SMS codes sent and entered correctly, one row per day.
sms_days <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("ibm_verify", "mfa_activity")) |>
    dplyr::filter(mfa_type == "sms_otp", result %in% c("sent", "success")) |>
    dplyr::group_by(date, result) |>
    dplyr::summarise(n = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::transmute(day = as.Date(date), column = paste0("sms_", result), n) |>
    tidyr::pivot_wider(names_from = column, values_from = n, values_fill = 0) |>
    with_columns(c("sms_sent", "sms_success")) |>
    dplyr::arrange(day)
})

# SMS code requests from these GeoIP countries are not counted as outside.
home_countries <- c("CAN", "USA")

# SMS codes sent per day, and how many were requested from outside
# `home_countries`. A request with no country is not counted as outside.
sms_origin_days <- read_once(\(con) {
  inside <- c(home_countries, "", "UNKNOWN", "Unknown")
  dplyr::tbl(con, dbplyr::in_schema("ibm_verify_events_raw", "mfa_activity")) |>
    dplyr::filter(mfamethod == "SMS OTP", result == "sent") |>
    dplyr::mutate(
      day = !!toronto_day_sql("time"),
      outside = dplyr::if_else(
        is.na(geoip__country_iso_code) |
          geoip__country_iso_code %in% !!inside,
        0L, 1L
      )
    ) |>
    dplyr::group_by(day) |>
    dplyr::summarise(sms_sent = dplyr::n(),
                     sms_outside = sum(outside, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::mutate(day = as.Date(day)) |>
    dplyr::arrange(day)
})

# SMS codes sent per day by hashed phone number: numbers reached, codes per
# number, and the most to one number.
sms_phone_days <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("ibm_verify_events_raw", "mfa_activity")) |>
    dplyr::filter(mfamethod == "SMS OTP", result == "sent", !is.na(mfadevice),
                  !mfadevice %in% c("", "UNKNOWN", "Unknown")) |>
    dplyr::mutate(day = !!toronto_day_sql("time")) |>
    dplyr::count(day, mfadevice, name = "codes") |>
    dplyr::group_by(day) |>
    dplyr::summarise(
      phones = dplyr::n(),
      sms_sent = sum(codes, na.rm = TRUE),
      top_phone = max(codes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::mutate(day = as.Date(day), per_phone = sms_sent / phones) |>
    dplyr::arrange(day)
})

# Longest wait recorded between an email code sent and entered, in seconds.
email_seconds_cap <- 600L

# Seconds from an email code sent by the sign-up API to its entry, linked by
# hashed address: one row per day and whole second, capped.
email_code_seconds <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("ibm_verify_events_raw", "mfa_activity")) |>
    dplyr::filter(mfamethod == "Email OTP", performedby_type == "api",
                  !is.na(mfadevice),
                  !mfadevice %in% c("", "UNKNOWN", "Unknown")) |>
    dplyr::mutate(at = !!utc_seconds_sql("time")) |>
    dplyr::group_by(mfadevice) |>
    dbplyr::window_order(at) |>
    dplyr::mutate(prev_result = dplyr::lag(result), prev_at = dplyr::lag(at)) |>
    dplyr::ungroup() |>
    dplyr::filter(result == "success", prev_result == "sent") |>
    dplyr::mutate(
      day = !!toronto_day_sql("time"),
      seconds = least(floor(at - prev_at), !!email_seconds_cap)
    ) |>
    dplyr::count(day, seconds, name = "codes") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::mutate(day = as.Date(day)) |>
    dplyr::arrange(day, seconds)
})

# The event stream's "2026-10-05 17:10:37 UTC" as a SQL timestamp.
utc_time_sql <- function(column) {
  glue::glue(
    "from_iso8601_timestamp(replace(substr({column}, 1, 19), ' ', 'T') || 'Z')"
  )
}

toronto_day_sql <- function(column) {
  dplyr::sql(glue::glue("date(at_timezone({utc_time_sql(column)}, '{local_tz}'))"))
}

utc_seconds_sql <- function(column) {
  dplyr::sql(glue::glue("to_unixtime({utc_time_sql(column)})"))
}

# Application names that belong to a public partner service. Unknown names
# count as public, as in is_internal_rp().
public_applications <- function(con) {
  apps <- dplyr::tbl(
    con, dbplyr::in_schema("ibm_verify_events_raw", "application_usage")
  ) |>
    dplyr::distinct(applicationname) |>
    dplyr::collect() |>
    dplyr::pull(applicationname)
  apps[!is.na(apps) & !is_internal_rp(con, apps)]
}

# Accounts counted per pair of days: first seen, and first signed in to a
# public partner service (NA if never). Everything about accounts reads this.
account_pairs <- read_once(\(con) {
  public_apps <- public_applications(con)

  first_seen <- dplyr::tbl(
    con, dbplyr::in_schema("ibm_verify_events_raw", "authentication_activity")
  ) |>
    dplyr::filter(!is.na(subject), subject != "") |>
    dplyr::group_by(user = subject) |>
    dplyr::summarise(first_time = min(time), .groups = "drop") |>
    dplyr::mutate(first_day = !!toronto_day_sql("first_time"))

  first_service <- dplyr::tbl(
    con, dbplyr::in_schema("ibm_verify_events_raw", "application_usage")
  ) |>
    dplyr::filter(result == "success", !is.na(userid), userid != "",
                  applicationname %in% !!public_apps) |>
    dplyr::group_by(user = userid) |>
    dplyr::summarise(service_time = min(time), .groups = "drop") |>
    dplyr::mutate(service_day = !!toronto_day_sql("service_time"))

  first_seen |>
    dplyr::left_join(first_service, by = "user") |>
    dplyr::count(first_day, service_day, name = "accounts") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::mutate(first_day = as.Date(first_day),
                  service_day = as.Date(service_day))
})

# GA error events per day per error code.
error_codes <- read_once(\(con) {
  dplyr::tbl(con, dbplyr::in_schema("google_analytics", "error_events")) |>
    dplyr::group_by(date, error_code) |>
    dplyr::summarise(events = sum(eventcount, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    as_plain_numbers() |>
    dplyr::transmute(day = as.Date(date), error_code, events)
})

# Daily series ---------------------------------------------------------------

# New accounts per day, and how many had not signed in to a partner service by
# the end of that day.
account_days <- function(pairs, through) {
  pairs |>
    dplyr::filter(!is.na(first_day), first_day <= through) |>
    dplyr::group_by(day = first_day) |>
    dplyr::summarise(
      new_accounts = sum(accounts),
      idle_same_day = sum(accounts[is.na(service_day) | service_day > day]),
      .groups = "drop"
    ) |>
    dplyr::arrange(day)
}

# The middle of `x`, each value counted `w` times.
weighted_median <- function(x, w) {
  o <- order(x)
  x[o][which(cumsum(w[o]) >= sum(w) / 2)[1]]
}

# Email codes entered per day, and the median seconds to enter one. A day
# with no codes has no row, so preflight sees it as missing.
speed_days <- function(seconds) {
  seconds |>
    dplyr::group_by(day) |>
    dplyr::summarise(codes_entered = sum(codes),
                     median_seconds = weighted_median(seconds, codes),
                     .groups = "drop")
}

# Accounts since launch as of each day. Point in time: an account leaves the
# idle count the day it first signs in to a service; no earlier day changes.
accounts_since_launch <- function(pairs, through) {
  pairs <- dplyr::filter(pairs, !is.na(first_day), first_day <= through)
  days <- tibble::tibble(date = seq(min(pairs$first_day), through, by = "day"))

  new <- pairs |>
    dplyr::count(date = first_day, wt = accounts, name = "new_accounts")

  # An account counts as signed in from the later of its two days, so it is
  # never signed in before it exists.
  signed_in <- pairs |>
    dplyr::filter(!is.na(service_day)) |>
    dplyr::mutate(date = pmax(first_day, service_day)) |>
    dplyr::filter(date <= through) |>
    dplyr::count(date, wt = accounts, name = "newly_signed_in")

  days |>
    dplyr::left_join(new, by = "date") |>
    dplyr::left_join(signed_in, by = "date") |>
    dplyr::mutate(
      new_accounts = dplyr::coalesce(new_accounts, 0),
      all_accounts = cumsum(new_accounts),
      signed_in_to_service = cumsum(dplyr::coalesce(newly_signed_in, 0)),
      idle_accounts = all_accounts - signed_in_to_service,
      idle_share = round(idle_accounts / all_accounts, 3)
    ) |>
    dplyr::select(date, all_accounts, idle_accounts, signed_in_to_service,
                  idle_share, new_accounts)
}

# Accounts since launch per Monday-to-Sunday week, as Sign-In Activity. Totals
# are as of `date`, the week's last day so far; `new_accounts` is the week's sum.
accounts_by_week <- function(since_launch) {
  since_launch |>
    dplyr::mutate(week_end = date - (as.integer(format(date, "%u")) - 1L) + 6L) |>
    dplyr::group_by(week_end) |>
    dplyr::mutate(new_accounts = sum(new_accounts)) |>
    dplyr::slice_max(date, n = 1) |>
    dplyr::ungroup()
}

# Signature error groups and all errors, one row per day GA reported.
error_days <- function(codes) {
  all <- codes |>
    dplyr::group_by(day) |>
    dplyr::summarise(all_errors = sum(events), .groups = "drop")

  groups <- codes |>
    dplyr::mutate(group = unname(signature_groups[error_code])) |>
    dplyr::filter(!is.na(group)) |>
    dplyr::group_by(day, group) |>
    dplyr::summarise(events = sum(events), .groups = "drop") |>
    tidyr::pivot_wider(names_from = group, values_from = events)

  all |>
    dplyr::left_join(groups, by = "day") |>
    with_columns(names(signature_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(names(signature_labels)),
                                \(x) dplyr::coalesce(x, 0))) |>
    dplyr::arrange(day)
}

# Scoring --------------------------------------------------------------------

day_type <- function(day) {
  ifelse(as.integer(format(day, "%u")) >= 6L, "weekend", "weekday")
}

# Scores a method day by day, oldest first, since a day's baseline leaves out
# earlier days that fired. `score_one(i, history)` scores day i against the
# row indices of its baseline days. A day with too few baseline days is not
# scored: `fired` is NA.
score_days <- function(day, score_one, thresholds) {
  type <- day_type(day)
  need <- thresholds$baseline_min_days[type]
  fired <- rep(FALSE, length(day))

  rows <- purrr::map(seq_along(day), \(i) {
    history <- which(
      day >= day[i] - thresholds$baseline_days & day < day[i] &
        type == type[i] & !fired
    )
    if (length(history) < need[[i]]) {
      return(tibble::tibble(baseline = NA_real_, multiple = NA_real_,
                            ratio = NA_real_, fired = NA))
    }
    scored <- score_one(i, history)
    fired[i] <<- scored$fired
    scored
  })

  dplyr::bind_cols(tibble::tibble(day = day), dplyr::bind_rows(rows))
}

# SMS codes: sends well above normal, and few of them entered.
score_sms <- function(sms, thresholds) {
  t <- thresholds$sms
  scored <- score_days(sms$day, \(i, history) {
    baseline <- stats::median(sms$sms_sent[history])
    multiple <- sms$sms_sent[i] / baseline
    ratio <- sms$sms_success[i] / sms$sms_sent[i]
    tibble::tibble(
      baseline = baseline, multiple = multiple, ratio = ratio,
      fired = multiple >= t$volume & ratio < t$entry_rate_below
    )
  }, thresholds)
  dplyr::bind_cols(sms, dplyr::select(scored, -day))
}

# SMS location: sends well above normal, and many requested from outside
# Canada and the US.
score_location <- function(sms, thresholds) {
  t <- thresholds$location
  scored <- score_days(sms$day, \(i, history) {
    baseline <- stats::median(sms$sms_sent[history])
    multiple <- sms$sms_sent[i] / baseline
    ratio <- sms$sms_outside[i] / sms$sms_sent[i]
    tibble::tibble(
      baseline = baseline, multiple = multiple, ratio = ratio,
      fired = multiple >= t$volume & ratio >= t$share_at_least
    )
  }, thresholds)
  dplyr::bind_cols(sms, dplyr::select(scored, -day))
}

# Idle accounts: sign-ups well above normal, and most never reached a partner
# service on the day they were made.
score_idle <- function(accounts, thresholds) {
  t <- thresholds$idle
  scored <- score_days(accounts$day, \(i, history) {
    baseline <- stats::median(accounts$new_accounts[history])
    multiple <- accounts$new_accounts[i] / baseline
    ratio <- accounts$idle_same_day[i] / accounts$new_accounts[i]
    tibble::tibble(
      baseline = baseline, multiple = multiple, ratio = ratio,
      fired = multiple >= t$volume & ratio >= t$idle_share_at_least
    )
  }, thresholds)
  dplyr::bind_cols(accounts, dplyr::select(scored, -day))
}

# Sign-up speed: email codes entered well above normal, and entered faster
# than people enter them.
score_speed <- function(speed, thresholds) {
  t <- thresholds$speed
  scored <- score_days(speed$day, \(i, history) {
    baseline <- stats::median(speed$codes_entered[history])
    multiple <- speed$codes_entered[i] / baseline
    ratio <- speed$median_seconds[i]
    tibble::tibble(
      baseline = baseline, multiple = multiple, ratio = ratio,
      fired = multiple >= t$volume & ratio < t$median_seconds_below
    )
  }, thresholds)
  dplyr::bind_cols(speed, dplyr::select(scored, -day))
}

# Error patterns: several signature errors up together, each against its own
# baseline, floored so a jump from 2 to 9 is not 4.5x.
score_errors <- function(errors, thresholds) {
  t <- thresholds$errors
  groups <- names(signature_labels)
  scored <- score_days(errors$day, \(i, history) {
    medians <- purrr::map_dbl(groups, \(g) stats::median(errors[[g]][history]))
    baselines <- pmax(medians, t$group_floor)
    today <- purrr::map_dbl(groups, \(g) errors[[g]][i])
    groups_up <- sum(today / baselines >= t$group_volume)
    ratio <- sum(today) / errors$all_errors[i]
    tibble::tibble(
      baseline = sum(baselines), multiple = sum(today) / sum(baselines),
      ratio = ratio, groups_up = groups_up,
      fired = groups_up >= t$groups_at_least & ratio >= t$share_at_least
    )
  }, thresholds)
  dplyr::bind_cols(
    dplyr::mutate(errors, signature = rowSums(dplyr::pick(dplyr::all_of(groups)))),
    dplyr::select(scored, -day)
  )
}

# Every method's score on every day it reported, one row per method per day.
method_scores <- function(scored) {
  purrr::imap(scored, \(rows, method) {
    dplyr::transmute(rows, method = method, day, multiple, fired)
  }) |>
    dplyr::bind_rows()
}

# Calendar --------------------------------------------------------------------

# The Overview calendar covers six months; each method page three months.
calendar_weeks <- 26L
chart_weeks <- 13L

# The Monday that starts a span of `weeks` whole weeks, the last of them
# holding `through`.
calendar_start <- function(through, weeks = calendar_weeks) {
  through - (as.integer(format(through, "%u")) - 1L) - 7L * (weeks - 1L)
}

# One row per calendar day: methods fired, `strongest` (methods that fired)
# and `peak` (every method that scored), and `waiting` when a method has not
# caught up to the day yet.
calendar_days <- function(scores, from, through) {
  caught_up <- min(purrr::map_dbl(methods$method, \(m) {
    max(scores$day[scores$method == m & !is.na(scores$fired)], -Inf)
  }))
  tibble::tibble(day = seq(from, through, by = "day")) |>
    dplyr::left_join(scores, by = "day") |>
    dplyr::group_by(day) |>
    # Named n_fired, so `fired` stays per method for the lines below it.
    dplyr::summarise(
      strongest = if (any(fired %in% TRUE)) {
        max(multiple[fired %in% TRUE])
      } else {
        NA_real_
      },
      peak = if (any(!is.na(fired))) {
        max(multiple[!is.na(fired)])
      } else {
        NA_real_
      },
      methods_fired = list(method[!is.na(fired) & fired]),
      n_fired = sum(fired, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(waiting = day > caught_up)
}

# A method's newest flagged day from `from` on, for its value boxes: that
# day's `volume`, multiple and ratio, and the median ratio over the
# `baseline_days` before it, flagged days left out, and `n` flagged days in
# all. All NA if none fired.
last_flag <- function(scored, from, volume, baseline_days = 28L) {
  flagged <- dplyr::filter(scored, day >= from, fired %in% TRUE)
  if (nrow(flagged) == 0) {
    return(list(day = as.Date(NA), volume = NA, multiple = NA, ratio = NA,
                typical_ratio = NA, n = 0L))
  }
  row <- dplyr::slice_max(flagged, day, n = 1)
  before <- dplyr::filter(scored, day >= row$day - baseline_days,
                          day < row$day, !(fired %in% TRUE))
  list(day = row$day, volume = row[[volume]], multiple = row$multiple,
       ratio = row$ratio,
       typical_ratio = stats::median(before$ratio, na.rm = TRUE),
       n = nrow(flagged))
}

# The price of one SMS at a month's volume, from tiers of `up_to` and `price`.
sms_price <- function(volume, tiers) {
  tiers$price[findInterval(volume, tiers$up_to, left.open = TRUE) + 1L]
}

# What the extra SMS on flagged days cost. Each day's extra codes are priced
# at the tier set by the codes sent in the 30 days ending that day.
extra_sms_cost <- function(sms_scored, from, tiers) {
  sms_scored |>
    dplyr::mutate(month_volume = purrr::map_dbl(day, \(d) {
      sum(sms_sent[day > d - 30L & day <= d])
    })) |>
    dplyr::filter(day >= from, fired %in% TRUE) |>
    dplyr::summarise(cost = sum(pmax(sms_sent - baseline, 0) *
                                  sms_price(month_volume, tiers))) |>
    dplyr::pull(cost)
}

# Days flagged in the last seven, for the publishing workflow's Slack post.
write_traffic_summary <- function(calendar, through,
                                  path = "traffic-summary.json") {
  recent <- dplyr::filter(calendar, day > through - 7L, n_fired > 0)
  jsonlite::write_json(
    list(
      through = format(through),
      flagged_days = purrr::pmap(recent, \(day, n_fired, methods_fired, ...) {
        list(
          day = format(day),
          methods = n_fired,
          of = nrow(methods),
          fired = methods$label[methods$method %in% methods_fired]
        )
      })
    ),
    path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  invisible(nrow(recent))
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

# "4.4x", or a dash where there is no baseline.
fmt_multiple <- function(x) {
  ifelse(is.na(x) | !is.finite(x), "-",
         paste0(formatC(x, format = "f", digits = 1, big.mark = ","), "x"))
}

# Spaces to non-breaking, so a value box value stays one Str and keeps its class.
nbsp <- function(x) gsub(" ", "\u00a0", x, fixed = TRUE)

# "21 seconds", one Str for a value box; a dash where there is none.
fmt_seconds <- function(x) {
  if (is.na(x)) return("-")
  x <- round(x)
  nbsp(paste(x, if (x == 1) "second" else "seconds"))
}

# "12,286" or "34%" for a value box; a dash where there is none.
fmt_count <- function(x) if (is.na(x)) "-" else scales::comma(x)
fmt_share <- function(x) if (is.na(x)) "-" else scales::percent(x, accuracy = 1)

# "Wed, September 30", one Str for a value box.
fmt_flag_day <- function(d) {
  if (is.na(d)) return(nbsp("None in three months"))
  nbsp(paste0(format(d, "%a, %B "), as.integer(format(d, "%d"))))
}

# Captions under the last-flag value boxes; the multiple and typical lines
# are blank when no day was flagged.
flag_count_line <- function(flag) {
  paste(flag$n, if (flag$n == 1) "flagged day" else "flagged days",
        "in three months")
}
multiple_line <- function(flag) {
  if (is.na(flag$day)) "" else paste(fmt_multiple(flag$multiple), "a typical day")
}
typical_line <- function(flag, fmt) {
  if (is.na(flag$day)) "" else paste(fmt(flag$typical_ratio), "on a typical day")
}
