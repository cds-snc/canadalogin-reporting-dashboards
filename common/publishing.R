#' Links between published dashboards. A dashboard's URL carries its publish
#' token, the only access control, so it is read at render time from the
#' publishing repo's paths.tsv and never written into this public repo.
#' paths.tsv is the manifest publish.sh reuses tokens from, so it holds the
#' current URL. Renders locally read the full sibling checkout; the render
#' workflow checks out paths.tsv alone into the same place.

publishing_repo_root <- function() {
  repo_path("..", "canadalogin-signal-check-publishing")
}

# URL of a published file by name, e.g. published_url("support.html"), from
# its newest row in paths.tsv. NULL with a console message when there is none,
# so a missing link never fails a render. Pass utm_source = NULL for a bare URL.
published_url <- function(file, utm_source = "dashboard_link",
                          manifest = file.path(publishing_repo_root(),
                                               "paths.tsv")) {
  if (!file.exists(manifest)) {
    message("No link to ", file, ": ", manifest, " not found")
    return(NULL)
  }
  rows <- utils::read.delim(manifest, colClasses = "character")
  rows <- rows[rows$source == file & nzchar(rows$url), ]
  if (nrow(rows) == 0L) {
    message("No link to ", file, ": not published in ", manifest)
    return(NULL)
  }
  # ISO 8601 timestamps sort as text.
  url <- rows$url[order(rows$published_at, decreasing = TRUE)][1]
  if (is.null(utm_source)) url else paste0(url, "?utm_source=", utm_source)
}
