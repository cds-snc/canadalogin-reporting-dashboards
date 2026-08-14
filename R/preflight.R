#' Preflight data-source validation.
#'
#' Runs in the dashboard's setup chunk, before any figure or table query. Prints
#' a numbered checklist and returns the result. A failed check raises a banner
#' across the top of the dashboard rather than stopping the render.
#'
#' Source R/connection.R, R/registry.R and R/metrics.R first: this reuses an
#' open connection passed in as `con` and expects dplyr/dbplyr/tidyr and glue.

run_preflight_safety_check <- function(con,
                                       today = Sys.Date(),
                                       ga_export_lag_days = 2L,
                                       lookback_days = 14L) {

  # Helpers --------------------------------------------------------------------

  # "Sun Aug 10", or "no data" for the non-finite max() an empty table yields.
  format_date <- function(d) {
    labels <- rep("no data", length(d))
    finite <- is.finite(as.numeric(d))
    labels[finite] <- format(as.Date(d[finite], origin = "1970-01-01"),
                             "%a %b %d")
    labels
  }

  # Accumulated so every check runs, rather than stopping at the first problem.
  checks <- list()
  record_check <- function(number, title, passed, details = character()) {
    checks[[length(checks) + 1L]] <<- list(
      number = number, title = as.character(title), passed = passed,
      details = as.character(details)
    )
  }

  # Date anchors ---------------------------------------------------------------

  # GA runs on a deliberate ~2-day lag, so as_of is the newest day to expect.
  as_of <- today - ga_export_lag_days
  freshness_start <- as_of - (lookback_days - 1L)
  proxy_start <- as_of - 29L

  # One query over the widest span any check needs; the checks then run in R.
  raw <- tbl(
    con, in_schema("google_analytics", "funnel_metrics_current")
  ) |>
    filter(
      metric_id == "task_success",
      funnel_id %in% !!required_funnels,
      !window_incomplete,
      as.Date(window_end) >= as.Date(!!as.character(proxy_start)),
      as.Date(window_end) <= as.Date(!!as.character(as_of))
    ) |>
    select(funnel_id, window_days, breakdown_value, window_end) |>
    collect() |>
    mutate(window_end = as.Date(window_end))

  # Check 1 - GA freshness (1-day rows, no gaps over the lookback) --------------

  freshness_problems <- character()
  expected_days <- seq(freshness_start, as_of, by = "day")
  for (funnel in required_funnels) {
    days_present <- raw |>
      filter(funnel_id == funnel,
             window_days == "1",
             breakdown_value == "RESERVED_TOTAL",
             window_end >= freshness_start) |>
      pull(window_end) |>
      unique()
    missing <- expected_days[!expected_days %in% days_present]
    if (length(missing) > 0) {
      # Capped: this detail goes on the banner, where a full list runs off it.
      shown <- format_date(utils::head(missing, 5))
      freshness_problems <- c(
        freshness_problems,
        glue("{funnel}: missing 1-day rows for {length(missing)} day(s), ",
             "including {glue_collapse(shown, sep = ', ')}")
      )
    }
  }

  record_check(
    1,
    glue("GA freshness - 1-day rows through {format_date(as_of)} ",
         "with no gaps over the last {lookback_days} days"),
    passed = length(freshness_problems) == 0,
    details = if (length(freshness_problems) == 0) {
      glue("funnel_metrics_current is current through {format_date(as_of)} ",
           "for {glue_collapse(required_funnels, sep = ', ')}")
    } else {
      freshness_problems
    }
  )

  # Check 2 - required funnels present ------------------------------------------

  funnels_present <- unique(raw$funnel_id)
  missing_funnels <- setdiff(required_funnels, funnels_present)

  record_check(
    2,
    "Required funnels present",
    passed = length(missing_funnels) == 0,
    details = if (length(missing_funnels) == 0) {
      glue("all required funnels found: ",
           "{glue_collapse(required_funnels, sep = ', ')}")
    } else {
      glue("missing required funnel(s): ",
           "{glue_collapse(missing_funnels, sep = ', ')}")
    }
  )

  # Check 3 - relying-party lookup complete -------------------------------------
  #
  # Aliases are added to the source sheet by hand, so a GA rp_name can turn up
  # before anyone records it. Left join and check for nulls: an inner join would
  # drop the party's rows without complaint and hole the table.
  data_parties <- raw |>
    filter(breakdown_value != "RESERVED_TOTAL",
           !breakdown_value %in% ga_excluded_rp_names) |>
    pull(breakdown_value) |>
    unique()

  labelled <- label_relying_parties(con, data_parties)
  unmapped <- data_parties[is.na(labelled$service_name)]

  record_check(
    3,
    "Relying-party lookup complete - every party in the data is labelled",
    passed = length(unmapped) == 0,
    details = if (length(unmapped) == 0) {
      glue("{length(data_parties)} relying part",
           "{if (length(data_parties) == 1) 'y' else 'ies'} in the data, ",
           "all resolved through rp.alias to ",
           "{length(unique(labelled$service_name))} service",
           "{if (length(unique(labelled$service_name)) == 1) '' else 's'}")
    } else {
      glue("GA rp_name(s) with no row in rp.alias: ",
           "{glue_collapse(unmapped, sep = ', ')}; ",
           "add them to the CanadaLogin Relying Parties sheet")
    }
  )

  # Check 4 - 30-day proxy window integrity -------------------------------------
  #
  # A missing day inside the span quietly shrinks both counts. A young funnel has
  # fewer than 30 days, which is not a hole, so this asks for contiguity within
  # the funnel's own history rather than data from before it existed.
  min_proxy_inputs <- 14L
  proxy_problems <- character()
  for (funnel in required_funnels) {
    proxy_days_present <- raw |>
      filter(funnel_id == funnel,
             window_days == "1",
             breakdown_value == "RESERVED_TOTAL",
             window_end >= proxy_start) |>
      pull(window_end) |>
      unique()

    if (length(proxy_days_present) == 0) {
      proxy_problems <- c(
        proxy_problems,
        glue("{funnel}: no 1-day inputs in the trailing 30 days")
      )
      next
    }

    span_start <- max(proxy_start, min(proxy_days_present))
    expected <- seq(span_start, as_of, by = "day")
    missing <- expected[!expected %in% proxy_days_present]

    if (length(missing) > 0) {
      proxy_problems <- c(
        proxy_problems,
        glue("{funnel}: 30-day proxy has interior gaps ",
             "({length(missing)} day(s), including ",
             "{glue_collapse(format_date(utils::head(missing, 3)), sep = ', ')})")
      )
    } else if (length(proxy_days_present) < min_proxy_inputs) {
      proxy_problems <- c(
        proxy_problems,
        glue("{funnel}: only {length(proxy_days_present)} 1-day inputs ",
             "(fewer than {min_proxy_inputs}); 30-day proxy is too thin")
      )
    }
  }

  record_check(
    4,
    glue("Window integrity - 30-day proxy inputs contiguous through ",
         "{format_date(as_of)}"),
    passed = length(proxy_problems) == 0,
    details = if (length(proxy_problems) == 0) {
      glue("1-day inputs contiguous through {format_date(as_of)} ",
           "for all required funnels")
    } else {
      proxy_problems
    }
  )

  # Check 5 - ratio steps present in funnels_current -----------------------------
  #
  # Counts are read by step NAME, so a step renamed upstream would return no rows
  # rather than an error, and read as a true zero. Checked over all history,
  # since an exit step nobody reached in the current span is legitimately absent.
  steps_seen <- tbl(
    con, in_schema("google_analytics", "funnels_current")
  ) |>
    filter(funnel_id %in% !!required_funnels) |>
    distinct(funnel_id, funnelstepname) |>
    collect()

  step_problems <- character()
  for (funnel in required_funnels) {
    declared <- funnel_ratio_steps[[funnel]]
    if (is.null(declared)) {
      step_problems <- c(
        step_problems,
        glue("{funnel}: no ratio steps declared in funnel_ratio_steps")
      )
      next
    }
    present <- steps_seen$funnelstepname[steps_seen$funnel_id == funnel]
    missing_steps <- setdiff(unname(declared), present)
    if (length(missing_steps) > 0) {
      step_problems <- c(
        step_problems,
        glue("{funnel}: step(s) not found in funnels_current: ",
             "{glue_collapse(missing_steps, sep = ', ')}; ",
             "check the funnel YAML against funnel_ratio_steps in R/metrics.R")
      )
    }
  }

  record_check(
    5,
    "Ratio steps present - task success numerator and denominator readable",
    passed = length(step_problems) == 0,
    details = if (length(step_problems) == 0) {
      glue("entry and exit steps found for ",
           "{glue_collapse(required_funnels, sep = ', ')}")
    } else {
      step_problems
    }
  )

  for (check in checks) {
    mark <- if (check$passed) "PASS" else "FAIL"
    message(glue("[{mark}] Check {check$number}: {check$title}"))
    for (detail in check$details) message(glue("       {detail}"))
  }

  failed_checks <- purrr::keep(checks, \(check) !check$passed)
  if (length(failed_checks) > 0) {
    failed_numbers <- purrr::map_int(failed_checks, "number")
    warning(
      glue("Preflight safety check failed: check ",
           "{glue_collapse(failed_numbers, sep = ', ', last = ' and ')} ",
           "did not hold (see the checklist above). The dashboard will still ",
           "render, with a banner across the top; do not publish it."),
      call. = FALSE
    )
  }

  invisible(list(
    passed = length(failed_checks) == 0,
    checks = checks,
    failed = failed_checks
  ))
}

# Banner --------------------------------------------------------------------

preflight_contact_url <- "https://gcdigital.slack.com/archives/C0A6S9F7KV4"

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Writes the banner a failed check raises, from run_preflight_safety_check()'s
# result. A file, not chunk output, which a dashboard would turn into a card.
write_preflight_banner <- function(result, path = "preflight-banner.html") {
  if (result$passed) {
    writeLines(character(), path)
    return(invisible(FALSE))
  }

  items <- purrr::map_chr(result$failed, \(check) {
    glue("<li><strong>Check {check$number}: {escape_html(check$title)}.</strong> ",
         "{escape_html(glue_collapse(check$details, sep = '; '))}</li>")
  })

  writeLines(c(
    '<div class="preflight-banner" role="alert">',
    '  <p class="preflight-banner-title">Preflight safety checks failed</p>',
    '  <p>The numbers below may be wrong or incomplete. Check the data before',
    "     quoting them, notify someone in",
    paste0('     <a href="', preflight_contact_url, '">#edcp-data-and-research</a>'),
    "     and do not publish this dashboard as it stands.</p>",
    paste0("  <ul>", paste(items, collapse = ""), "</ul>"),
    "</div>"
  ), path)

  invisible(TRUE)
}

# Status file ----------------------------------------------------------------

# The banner's machine-readable twin. Unattended there is nobody to read the
# banner, so CI reads this instead and fails the run on a failed check.
write_preflight_status <- function(result, path = "preflight-status.json") {
  jsonlite::write_json(
    list(
      passed = result$passed,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      failed = purrr::map(result$failed, \(check) list(
        number = check$number,
        title = check$title,
        details = as.character(check$details)
      ))
    ),
    path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(result$passed)
}
