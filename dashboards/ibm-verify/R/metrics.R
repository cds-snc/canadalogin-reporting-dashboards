# Sign-in activity metric layer, over the three ibm_verify tables.

# The day CanadaLogin launched, and the first day any of these tables covers.
ibm_verify_launch <- as.Date("2026-04-22")

# Every summary figure is over this window, compared against the one before it.
summary_window_days <- 28L

# Yesterday's rows land at about 06:00 ET, so the newest day to expect is
# today minus one. Preflight check 1 is what reports a table that fell behind.
ibm_verify_lag_days <- 1L

# Reads ----------------------------------------------------------------------

# Read a whole ibm_verify table once and cache it for the session. Every page
# reads the same rows, and the data cannot change mid-render.
read_once <- function(name, prepare) {
  cache <- new.env(parent = emptyenv())
  function(con) {
    if (!is.null(cache$value)) {
      return(cache$value)
    }
    raw <- dplyr::tbl(con, dbplyr::in_schema("ibm_verify", name)) |>
      dplyr::collect()
    # BIGINT arrives as integer64, which does not survive arithmetic with
    # doubles cleanly.
    cache$value <- raw |>
      dplyr::mutate(
        date = as.Date(date),
        dplyr::across(dplyr::where(bit64::is.integer64), as.numeric)
      ) |>
      prepare() |>
      dplyr::arrange(date)
    cache$value
  }
}

# CanadaLogin-wide daily totals
auth_totals <- read_once("auth_total_logins", \(x) {
  dplyr::select(
    x, date, unique_users, successful_logins, failed_logins,
    mtd_unique_users, ytd_unique_users, rolling_365d_unique_users
  )
})

# Daily counts per application
app_logins <- read_once("app_login_counts", \(x) {
  dplyr::select(
    x, date, application_name, total_logins, unique_users,
    successful_logins, failed_logins,
    mtd_unique_users, ytd_unique_users, rolling_365d_unique_users
  )
})

# Daily MFA attempts, one row per factor per outcome. 
mfa_activity <- read_once("mfa_activity", \(x) {
  dplyr::select(x, date, mfa_type, result, count)
})

# The newest day every table covers
ibm_verify_data_through <- function(con) {
  min(
    max(auth_totals(con)$date),
    max(app_logins(con)$date),
    max(mfa_activity(con)$date)
  )
}

# Windows ---------------------------------------------------------------------

# The `days` days ending on `as_of` inclusive
in_window <- function(rows, as_of, days = summary_window_days) {
  as_of <- as.Date(as_of)
  dplyr::filter(rows, date > as_of - days, date <= as_of)
}

# The value of a cumulative column on the newest day of a set of rows
latest_value <- function(rows, column) {
  if (nrow(rows) == 0) {
    return(NA_real_)
  }
  rows[[column]][which.max(rows$date)]
}

# Mean of the daily active-user counts
average_daily_users <- function(rows, as_of, days = summary_window_days) {
  daily <- in_window(rows, as_of, days) |>
    dplyr::group_by(date) |>
    dplyr::summarise(users = sum(unique_users), .groups = "drop")
  sum(daily$users) / days
}

# CanadaLogin-wide calendar months -  useful for billing and monthly reporting
monthly_auth_totals <- function(rows, as_of) {
  as_of <- as.Date(as_of)
  rows |>
    dplyr::filter(date <= as_of) |>
    dplyr::mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      authentications = sum(successful_logins + failed_logins),
      users = mtd_unique_users[which.max(date)],
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(month))
}

# Each unit's newest row - for info boxes
latest_per_unit <- function(rows, from, to) {
  rows |>
    dplyr::filter(date >= as.Date(from), date <= as.Date(to)) |>
    dplyr::group_by(unit) |>
    dplyr::slice_max(date, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

# Services --------------------------------------------------------------------

# Daily application rows with the service each belongs to. An application missing from 
# rp.alias is NA rather than dropped. Preflight check 3 fails on an NA here.
labelled_app_logins <- local({
  cached <- NULL
  function(con) {
    if (!is.null(cached)) {
      return(cached)
    }
    lookup <- relying_party_lookup(con) |>
      dplyr::select(application_name = alias, service_name, is_internal)
    cached <<- app_logins(con) |>
      dplyr::left_join(lookup, by = "application_name")
    cached
  }
})

# Busiest first, which is the relying-party table's row order.
services_by_volume <- function(rows) {
  rows |>
    dplyr::filter(!is.na(service_name)) |>
    dplyr::group_by(service_name) |>
    dplyr::summarise(sign_ins = sum(total_logins), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(sign_ins)) |>
    dplyr::pull(service_name)
}

# Every service that has ever had traffic, internal included. Used for preflight
all_services <- function(con) {
  services_by_volume(labelled_app_logins(con))
}

# What the Services page lists. Excludes internal services.
external_services <- function(con) {
  services_by_volume(dplyr::filter(labelled_app_logins(con), !is_internal))
}

# Daily rows for one service, where `unit` is the application name its chart
# stacks and its table lists
service_daily <- function(con, service) {
  labelled_app_logins(con) |>
    dplyr::filter(service_name == service) |>
    dplyr::mutate(unit = application_name) |>
    dplyr::select(date, unit, total_logins, unique_users, successful_logins,
                  failed_logins, mtd_unique_users, ytd_unique_users,
                  rolling_365d_unique_users)
}

# The first day a service's rows carry any traffic, which is when it came onto
# CanadaLogin.
service_launch <- function(rows) {
  if (nrow(rows) == 0) as.Date(NA) else min(rows$date)
}

# Each unit's own first day
unit_first_seen <- function(rows) {
  rows |>
    dplyr::group_by(unit) |>
    dplyr::summarise(first_seen = min(date), .groups = "drop")
}

# Active users per unit per calendar month. Current month is partial.
#
# NA, not zero, before a unit existed, instead of zero.
monthly_unit_users <- function(rows, as_of) {
  as_of <- as.Date(as_of)
  current_month <- as.Date(format(as_of, "%Y-%m-01"))
  months <- seq(as.Date(format(min(rows$date), "%Y-%m-01")), current_month,
                by = "1 month")

  measured <- purrr::map(months, function(m) {
    month_end <- min(seq(m, by = "1 month", length.out = 2)[2] - 1L, as_of)
    latest_per_unit(rows, m, month_end) |>
      dplyr::transmute(month = m, unit, users = mtd_unique_users)
  }) |>
    dplyr::bind_rows()

  tidyr::expand_grid(month = months, unit = service_units(rows)) |>
    dplyr::left_join(measured, by = c("month", "unit")) |>
    dplyr::left_join(unit_first_seen(rows), by = "unit") |>
    dplyr::mutate(
      users = dplyr::if_else(
        month < as.Date(format(first_seen, "%Y-%m-01")),
        NA_real_, dplyr::coalesce(users, 0)
      )
    ) |>
    dplyr::select(month, unit, users)
}

# Sign-ins per unit per calendar month

monthly_unit_sign_ins <- function(rows, as_of) {
  as_of <- as.Date(as_of)
  current_month <- as.Date(format(as_of, "%Y-%m-01"))
  months <- seq(as.Date(format(min(rows$date), "%Y-%m-01")), current_month,
                by = "1 month")

  measured <- rows |>
    dplyr::filter(date <= as_of) |>
    dplyr::mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
    dplyr::group_by(month, unit) |>
    dplyr::summarise(sign_ins = sum(total_logins), .groups = "drop")

  tidyr::expand_grid(month = months, unit = service_units(rows)) |>
    dplyr::left_join(measured, by = c("month", "unit")) |>
    dplyr::left_join(unit_first_seen(rows), by = "unit") |>
    dplyr::mutate(
      sign_ins = dplyr::if_else(
        month < as.Date(format(first_seen, "%Y-%m-01")),
        NA_real_, dplyr::coalesce(sign_ins, 0)
      )
    ) |>
    dplyr::select(month, unit, sign_ins)
}

# Units, busiest first, so the stack and the table read in the same order.
service_units <- function(rows) {
  rows |>
    dplyr::group_by(unit) |>
    dplyr::summarise(sign_ins = sum(total_logins), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(sign_ins)) |>
    dplyr::pull(unit)
}

# MFA -------------------------------------------------------------------------

# The factors a member of the public can present when signing in. email_otp is
# left out because its only used for sign-up, not sign-in.
sign_in_mfa_types <- c("sms_otp", "voice_otp", "fido2")

# Display names, so the dashboard speaks the report's language.
mfa_labels <- c(
  sms_otp         = "SMS OTP",
  email_otp       = "Email OTP",
  voice_otp       = "Voice OTP",
  fido2           = "Passkey (FIDO2)",
  totp            = "TOTP",
  kq              = "Knowledge question",
  qr              = "QR code",
  ibm_verify_push = "IBM Verify push"
)

# An unknown factor keeps its raw name
mfa_label <- function(x) dplyr::coalesce(unname(mfa_labels[x]), x)

# Successful MFA attempts per day per factor
mfa_successes <- function(con, types = NULL) {
  rows <- mfa_activity(con) |>
    dplyr::filter(result == "success")
  if (!is.null(types)) rows <- dplyr::filter(rows, mfa_type %in% types)
  rows |>
    dplyr::select(date, mfa_type, count) |>
    dplyr::mutate(factor_label = mfa_label(mfa_type))
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

# Direction is carried by the arrow and the sign, not by colour alone; a change
# with nothing to compare against is a dash, deliberately distinct from a
# measured "no change". gt colours it, see delta_colours.
fmt_delta <- function(delta) {
  direction <- delta_direction(delta)
  dplyr::case_when(
    is.na(delta) ~ "-",
    direction == "up" ~ sprintf("\u25b2 %+.0f%%", delta * 100),
    direction == "down" ~ sprintf("\u25bc %+.0f%%", delta * 100),
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
