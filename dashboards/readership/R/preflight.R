#' Preflight data-source validation: prints a checklist and returns the
#' result. A failed check raises a banner instead of stopping the render.
#'
#' The checks guard against a quiet failure: an export stops and readership
#' looks like a slow week, or a page drops out of the counts. Needs
#' R/metrics.R and an open `con`.

# GA is exported on a two-day lag, by 07:00 ET.
export_lag_days <- 3L

# A week with no reader on any page means the meta reports have stopped,
# since a day nobody reads leaves no rows to tell the two apart.
quiet_days_allowed <- 7L

# A publishing path that still holds a token segment: /r/<token>/<file>.
token_pattern <- "^/canadalogin-signal-check-publishing/r/[^/]+/"

run_preflight_safety_check <- function(con, today = toronto_today(),
                                       snapshot = NULL) {

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
  record_check <- function(slug, title, passed, details = character()) {
    checks[[length(checks) + 1L]] <<- list(
      slug = slug, title = as.character(title), passed = passed,
      details = as.character(details)
    )
  }

  traffic <- page_traffic(con)
  events <- page_events(con)

  # Check 1 - live data ---------------------------------------------------------

  record_check(
    "live-data",
    "The dashboard must read the warehouse, not a saved snapshot",
    passed = is.null(snapshot),
    details = if (is.null(snapshot)) {
      "Read from Athena"
    } else {
      glue("Read from {snapshot}")
    }
  )

  # Check 2 - freshness ---------------------------------------------------------

  exported <- export_through(con)
  newest <- suppressWarnings(max(traffic$day))

  record_check(
    "freshness",
    "The GA export must have run, and someone must have read a page this week",
    passed = isTRUE(exported >= today - export_lag_days) &&
      is.finite(newest) && newest >= exported - quiet_days_allowed,
    details = c(
      glue("google_analytics.property_traffic through {format_date(exported)} ",
           "(due {format_date(today - export_lag_days)} or later)"),
      glue("google_analytics.meta_page_traffic through {format_date(newest)}")
    )
  )

  # Check 3 - the tables agree --------------------------------------------------
  #
  # Both come from one run; a day where they differ was half loaded.

  by_day <- function(rows, value) {
    rows |>
      dplyr::group_by(day) |>
      dplyr::summarise(n = sum(.data[[value]]), .groups = "drop")
  }
  disagree <- dplyr::full_join(
    by_day(traffic, "views"),
    by_day(dplyr::filter(events, eventname == "page_view"), "events"),
    by = "day", suffix = c("_traffic", "_events")
  ) |>
    dplyr::filter(is.na(n_traffic) | is.na(n_events) | n_traffic != n_events)

  record_check(
    "tables-agree",
    "Page views must match between meta_page_traffic and meta_page_events",
    passed = nrow(disagree) == 0,
    details = if (nrow(disagree) == 0) {
      glue("{dplyr::n_distinct(traffic$day)} days match")
    } else {
      glue("{format_date(disagree$day)}: {dplyr::coalesce(disagree$n_traffic, 0)} ",
           "views against {dplyr::coalesce(disagree$n_events, 0)} page_view events")
    }
  )

  # Check 4 - every page known ---------------------------------------------------
  #
  # An unrecognized path is a page the dashboard cannot place, so its views
  # drop out of every count.

  unknown <- traffic |>
    dplyr::filter(page_kind == "other" | grepl("(unrecognized)",
                                               pagepath_redacted, fixed = TRUE)) |>
    dplyr::group_by(page_kind) |>
    dplyr::summarise(views = sum(views), .groups = "drop")

  record_check(
    "known-pages",
    "Every page must be a known kind",
    passed = nrow(unknown) == 0,
    details = if (nrow(unknown) == 0) {
      "No page of kind other or (unrecognized)"
    } else {
      glue("{unknown$views} views on pages of kind {unknown$page_kind}")
    }
  )

  # Check 5 - no tokens ----------------------------------------------------------
  #
  # The page never shows a path, but a token in the redacted column means the
  # ETL's redaction broke, and a published link could follow.

  tokens <- unique(c(traffic$pagepath_redacted[grepl(token_pattern,
                                                     traffic$pagepath_redacted)],
                     events$pagepath_redacted[grepl(token_pattern,
                                                    events$pagepath_redacted)]))

  record_check(
    "no-tokens",
    "No redacted path may still hold a publish token",
    passed = length(tokens) == 0,
    details = if (length(tokens) == 0) {
      "Every publishing path is /r/<file>"
    } else {
      # The paths themselves stay out of the banner.
      glue("{length(tokens)} path(s) with a segment between /r/ and the file")
    }
  )

  # Check 6 - edition numbers ----------------------------------------------------

  titles <- edition_titles(traffic)
  numbers <- titles |>
    dplyr::filter(!is.na(number)) |>
    dplyr::count(pagepath_redacted, name = "numbers")
  editions <- edition_list(traffic)
  unnumbered <- setdiff(unique(titles$pagepath_redacted),
                        numbers$pagepath_redacted)
  conflicting <- numbers$pagepath_redacted[numbers$numbers > 1]
  shared <- editions$number[duplicated(editions$number)]

  edition_date <- function(path) {
    format_short_date(as.Date(sub(edition_pattern, "\\1", path), "%Y%m%d"))
  }

  record_check(
    "edition-numbers",
    "Every Signal Check must carry one number in its title, and no two the same",
    passed = length(unnumbered) == 0 && length(conflicting) == 0 &&
      length(shared) == 0,
    details = c(
      glue("{nrow(editions)} Signal Checks numbered"),
      if (length(unnumbered) > 0) {
        glue("No number in the title of the Signal Check sent ",
             "{edition_date(unnumbered)}")
      },
      if (length(conflicting) > 0) {
        glue("More than one number in the titles of the Signal Check sent ",
             "{edition_date(conflicting)}")
      },
      if (length(shared) > 0) glue("Signal Check #{shared} is in more than one title")
    )
  )

  for (i in seq_along(checks)) {
    check <- checks[[i]]
    mark <- if (check$passed) "PASS" else "FAIL"
    message(glue("[{mark}] {i}. {check$slug}: {check$title}"))
    for (detail in check$details) message(glue("       {detail}"))
  }

  failed_checks <- purrr::keep(checks, \(check) !check$passed)
  if (length(failed_checks) > 0) {
    failed_slugs <- purrr::map_chr(failed_checks, "slug")
    warning(
      glue("Data quality check failed: ",
           "{glue_collapse(failed_slugs, sep = ', ', last = ' and ')} ",
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
