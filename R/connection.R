#' Athena connection and non-analytical setup. Queries live in R/metrics.R and
#' R/preflight.R; relying-party labelling in R/registry.R.
#'
#' Config comes from a gitignored .env at the project root. Authenticate first:
#' aws sso login --profile cl-data-admin.

# Packages -------------------------------------------------------------------

required_packages <- c(
  "DBI", "RAthena", "dplyr", "dbplyr", "tidyr",
  "ggplot2", "scales", "gt", "cowplot", "magick", "dotenv",
  "glue", "purrr", "jsonlite"
)

check_packages <- function() {
  missing <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      "\nInstall with: install.packages(c(",
      paste0("'", missing, "'", collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Configuration --------------------------------------------------------------

load_config <- function() {
  for (p in c(".env", "../.env", "../../.env")) {
    if (file.exists(p)) {
      dotenv::load_dot_env(p)
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

# Connection -----------------------------------------------------------------

connect_athena <- function() {
  check_packages()
  load_config()
  RAthena::RAthena_options(verbose = FALSE)

  args <- list(
    drv            = RAthena::athena(),
    region_name    = Sys.getenv("AWS_REGION", "ca-central-1"),
    s3_staging_dir = Sys.getenv("ATHENA_S3_STAGING_DIR")
  )

  # Name a profile only when one is set. In CI there is none: credentials
  # arrive as env vars, and an empty profile_name fails the lookup outright.
  profile <- Sys.getenv("AWS_PROFILE")
  if (nzchar(profile)) args$profile_name <- profile

  do.call(DBI::dbConnect, args)
}

# Date helpers ---------------------------------------------------------------

# The Monday on or before a date.
monday_of <- function(d) {
  d <- as.Date(d)
  d - ((as.integer(format(d, "%u")) - 1L))
}
