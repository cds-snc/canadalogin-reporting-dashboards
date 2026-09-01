#' Preflight reporting, shared by every dashboard. The checks themselves are
#' dashboard-specific and live in dashboards/<name>/R/preflight.R; what a
#' dashboard does with the result is not.
#'
#' A check function accumulates a list of `list(slug, title, passed, details)`
#' and returns `list(passed, checks, failed)`. `slug` is the stable id used to
#' reference a check outside this file, so it must not change when a check is
#' added, removed or reordered. Both writers below take that.

preflight_contact_url <- "https://gcdigital.slack.com/archives/C0A6S9F7KV4"

# Banner ----------------------------------------------------------------------

# Writes the failure banner from a preflight result. A file, not chunk output,
# which a dashboard would turn into a card. Written on every render, empty when
# the checks pass, and pulled in with include-before-body. Each dashboard
# appends its context strip to the same file.
write_preflight_banner <- function(result, path = "page-header.html") {
  if (result$passed) {
    writeLines(character(), path)
    return(invisible(FALSE))
  }

  items <- purrr::map_chr(result$failed, \(check) {
    slug <- htmltools::htmlEscape(check$slug)
    title <- htmltools::htmlEscape(check$title)
    details <- htmltools::htmlEscape(glue::glue_collapse(check$details, sep = "; "))
    glue::glue("<li><strong><code>{slug}</code> - {title}.</strong> {details}</li>")
  })

  writeLines(c(
    '<div class="preflight-banner" role="alert">',
    '  <p class="preflight-banner-title">Preflight safety checks failed</p>',
    "  <p>The numbers below may be wrong or incomplete. Check the data before",
    "     quoting them, notify someone in",
    paste0('     <a href="', preflight_contact_url, '">#edcp-data-and-research</a>'),
    "     and do not publish this dashboard as it stands.</p>",
    paste0("  <ul>", paste(items, collapse = ""), "</ul>"),
    "</div>"
  ), path)

  invisible(TRUE)
}

# Status file ----------------------------------------------------------------

# Machine-readable version of the banner, for unattended renders: CI reads
# this and fails the run on a failed check.
write_preflight_status <- function(result, path = "preflight-status.json") {
  jsonlite::write_json(
    list(
      passed = result$passed,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      failed = purrr::map(result$failed, \(check) {
        list(
          slug = check$slug,
          title = check$title,
          details = as.character(check$details)
        )
      })
    ),
    path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(result$passed)
}
