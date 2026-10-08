#' Links between published dashboards. A URL carries its publish token, the
#' only access control, so it is read at render time from the publishing
#' repo's paths.tsv and never written into this public repo.

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

# A file in the publishing repo's private data/ folder, which Pages does not
# serve, e.g. private_data("prices.csv"). NULL with a console message when it
# is missing, so a missing file never fails a render.
private_data <- function(file, read = utils::read.csv,
                         dir = file.path(publishing_repo_root(), "data")) {
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    message("No data from ", file, ": ", path, " not found")
    return(NULL)
  }
  read(path)
}
