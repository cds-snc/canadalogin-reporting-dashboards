# A Thursday render over three editions and one dashboard, read every day,
# that passes every preflight check. Tests break one thing each.

readership_today <- as.Date("2026-10-08")
readership_through <- readership_today - 2L

readership_prefix <- "/canadalogin-signal-check-publishing/r/"

readership_edition <- function(stamp) {
  paste0(readership_prefix, stamp, "_signal-check.html")
}

# Editions #3 and #4 have a full fortnight; #5 was sent three days ago.
readership_editions <- tibble::tribble(
  ~stamp,     ~title,
  "20260810", "CanadaLogin Signal Check #3",
  "20260824", "CanadaLogin Signal Check #4",
  "20261003", "CanadaLogin Signal Check #5"
)

# Ten views on the day an edition is sent, two a day after, and a dashboard
# with three a day. Each view is its own session from Slack.
readership_traffic <- function(through = readership_through) {
  editions <- purrr::pmap(readership_editions, \(stamp, title) {
    sent <- as.Date(stamp, "%Y%m%d")
    if (sent > through) return(NULL)
    days <- seq(sent, through, by = "day")
    tibble::tibble(day = days, page_kind = "signal_check",
                   pagepath_redacted = readership_edition(stamp),
                   pagetitle = title,
                   views = ifelse(days == sent, 10, 2))
  })
  dashboard <- tibble::tibble(
    day = seq(as.Date("2026-08-10"), through, by = "day"),
    page_kind = "dashboard",
    pagepath_redacted = paste0(readership_prefix, "ibm-verify.html"),
    pagetitle = "Sign-In Activity Dashboard",
    views = 3
  )
  dplyr::bind_rows(editions, dashboard) |>
    dplyr::mutate(streamname = "Signal Check", sessionsource = "slack",
                  sessionmedium = "(not set)", sessions = views)
}

# Page views as in the traffic, and half of them reaching each milestone up to
# a minute.
readership_events <- function(traffic = readership_traffic()) {
  views <- dplyr::transmute(traffic, day, page_kind, pagepath_redacted,
                            eventname = "page_view", events = views)
  reads <- purrr::map(c(1, 5, 15, 30, 60), \(mark) {
    dplyr::mutate(views, eventname = paste0("read_", mark, "s"),
                  events = events / 2)
  })
  dplyr::bind_rows(views, reads)
}

readership_with_data <- function(traffic = readership_traffic(),
                                 events = readership_events(traffic),
                                 exported = readership_through) {
  env <- load_dashboard("readership")
  stub(
    env,
    page_traffic = function(con) traffic,
    page_events = function(con) events,
    export_through = function(con) exported
  )
}

run_readership_preflight <- function(env, today = readership_today,
                                     snapshot = NULL) {
  run_quietly(env$run_preflight_safety_check(NULL, today = today,
                                             snapshot = snapshot))
}
