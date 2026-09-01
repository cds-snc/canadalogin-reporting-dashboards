#' Task success metric layer, over the google_analytics funnel tables.
#'
#' Reads the two activeUsers counts behind the stored rate rather than the rate
#' itself, so the dashboard can show a numerator and denominator and pool
#' relying parties its own way. Dividing the counts reproduces the stored rate
#' to floating-point precision.
#'
#' Every date and window column here is a string partition key: compare
#' window_days == "7", not == 7. Expects dplyr / dbplyr / tidyr on the caller.

# Every funnel must be present in the data (preflight) and have its own page.
required_funnels <- c(
  "sign_in", "sign_up", "recover_password",
  "migration_existing_to_cl", "migration_new_to_cl"
)

# Mirrors the `metrics:` block of each funnel's YAML in the pipeline repo, which
# is canonical. The `ratio-steps` preflight check fails the render if these
# drift from the data.
funnel_ratio_steps <- list(
  sign_in = c(numerator = "mfa", denominator = "enter_email"),
  sign_up = c(numerator = "complete", denominator = "terms"),
  recover_password = c(numerator = "complete", denominator = "reset_password"),
  migration_existing_to_cl = c(numerator = "migration_complete",
                               denominator = "migration_not_skipped"),
  migration_new_to_cl = c(numerator = "migration_complete",
                          denominator = "migration_not_skipped")
)

# Labels and the note shown under each funnel's table, local until the
# pipeline emits a label column. Keyed by funnel id so a mismatch fails rather
# than heading a page with another funnel's numbers.
funnel_presentation <- list(
  sign_in = list(
    label = "Sign in",
    note = paste(
      "**Note.** This rate stops at MFA selection, the last step Google",
      "Analytics can see before the off-site redirect to IBM Verify."
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
  migration_existing_to_cl = list(
    label = "Migration (existing)",
    note = paste(
      "**Note.** This funnel measures users who are migrating a legacy credential to",
      "CanadaLogin and already have a CanadaLogin account. As of August 2026, this",
      "is a low volume funnel, since most users are creating a new account."
    )
  ),
  migration_new_to_cl = list(
    label = "Migration (new)",
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

# Counts behind the rate, one row per (window_end, breakdown_value). `from` and
# `to` bound window_end inclusively; funnel_version is kept for the chart.
#
# Memoised: one funnel page calls this with the same (fid, window_days, from,
# to) from several places (value boxes, table columns, the overall row), and
# the data cannot change mid-render, so caching dedupes the Athena queries
# without changing any call site.
funnel_step_counts <- local({
  cache <- new.env(parent = emptyenv())
  function(con, funnel_id, window_days, from, to) {
    key <- paste(funnel_id, window_days, from, to, sep = "|")
    cached <- cache[[key]]
    if (!is.null(cached)) return(cached)

    steps <- declared_or_stop(funnel_ratio_steps, funnel_id, "funnel_ratio_steps")

    raw <- dplyr::tbl(
      con, dbplyr::in_schema("google_analytics", "funnels_current")
    ) |>
      dplyr::filter(
        funnel_id == !!funnel_id,
        funnelstepname %in% !!unname(steps),
        window_days == !!as.character(window_days),
        !window_incomplete,
        as.Date(window_end) >= as.Date(!!as.character(from)),
        as.Date(window_end) <= as.Date(!!as.character(to))
      ) |>
      dplyr::select(
        window_end, funnel_version, breakdown_value, funnelstepname, activeusers
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        window_end = as.Date(window_end),
        # integer64 does not survive arithmetic with doubles cleanly.
        activeusers = as.numeric(activeusers),
        role = ifelse(funnelstepname == steps[["numerator"]],
                      "numerator", "denominator")
      )

    out <- raw |>
      dplyr::select(-funnelstepname) |>
      tidyr::pivot_wider(names_from = role, values_from = activeusers)

    # pivot_wider only creates the columns it saw.
    for (column in c("numerator", "denominator")) {
      if (!column %in% names(out)) out[[column]] <- NA_real_
    }

    # No exit row at all means nobody reached the step: a true zero, not missing.
    out <- out |>
      dplyr::mutate(numerator = dplyr::coalesce(numerator, 0)) |>
      dplyr::arrange(window_end)

    cache[[key]] <- out
    out
  }
})

# GA's own total across every relying party, internal included. The headline
# figures and the chart use this.
total_step_counts <- function(con, funnel_id, window_days, from, to) {
  funnel_step_counts(con, funnel_id, window_days, from, to) |>
    dplyr::filter(breakdown_value == "RESERVED_TOTAL")
}

# One row per relying party. Every party is kept, including internal ones and
# GA's unattributed "" and "(not set)", so these rows partition
# total_step_counts exactly; pooling and dropping happen at display time.
party_step_counts <- function(con, funnel_id, window_days, from, to) {
  funnel_step_counts(con, funnel_id, window_days, from, to) |>
    dplyr::filter(breakdown_value != "RESERVED_TOTAL")
}

# Sum counts into rates, one row per group. Summing before dividing keeps the
# rate volume-weighted; a zero denominator is undefined (NA), not zero.
as_task_success <- function(counts, ...) {
  counts |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      numerator = sum(numerator, na.rm = TRUE),
      denominator = sum(denominator, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(denominator == 0, NA_real_, numerator / denominator)
    )
}

# GA's overall rate as a series over window_end. Grouping by funnel_version does
# not split rows: funnels_current resolves each day to one version.
task_success_series <- function(con, funnel_id, window_days, from, to) {
  total_step_counts(con, funnel_id, window_days, from, to) |>
    as_task_success(window_end, funnel_version)
}

# The latest overall rate, as one row. An empty span returns an all-NA row so
# callers can format it without a special case.
task_success_latest <- function(con, funnel_id, window_days, from, to) {
  series <- task_success_series(con, funnel_id, window_days, from, to)
  if (nrow(series) == 0) {
    return(dplyr::tibble(
      window_end = as.Date(NA), funnel_version = NA_character_,
      numerator = NA_real_, denominator = NA_real_, value = NA_real_
    ))
  }
  series[which.max(series$window_end), ]
}

# The ratio's step names as the data spells them, so labels follow the funnel
# definition. One row: entry/exit step, their numbers, and step_count.
funnel_step_names <- function(con, funnel_id) {
  steps <- declared_or_stop(funnel_ratio_steps, funnel_id, "funnel_ratio_steps")

  in_data <- dplyr::tbl(
    con, dbplyr::in_schema("google_analytics", "funnels_current")
  ) |>
    dplyr::filter(funnel_id == !!funnel_id) |>
    dplyr::distinct(funnelstepname, funnelstepnumber) |>
    dplyr::collect()

  number_of <- function(step) {
    match <- in_data$funnelstepnumber[in_data$funnelstepname == step]
    if (length(match) == 0) NA_integer_ else as.integer(min(match))
  }

  dplyr::tibble(
    entry_step = unname(steps[["denominator"]]),
    entry_number = number_of(steps[["denominator"]]),
    exit_step = unname(steps[["numerator"]]),
    exit_number = number_of(steps[["numerator"]]),
    step_count = nrow(in_data)
  )
}

# Length of the long window. Always read as a stored window, never built from
# shorter ones: activeUsers de-duplicate within a window, not across them.
long_window_days <- 28L

# The rate over the one stored window ending on as_of. `counts` selects the
# population; move as_of back long_window_days for the preceding period.
task_success_long_window <- function(con, funnel_id, as_of, ...,
                                     counts = total_step_counts) {
  as_of <- as.Date(as_of)
  counts(con, funnel_id, window_days = long_window_days,
         from = as_of, to = as_of) |>
    as_task_success(...)
}
