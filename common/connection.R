#' Athena connection, shared by every dashboard. Relying-party labelling is in
#' common/registry.R; a dashboard's own queries and preflight checks live in
#' its dashboards/<name>/R/ folder.
#'
#' Config comes from a gitignored .env at the repo root. Authenticate first:
#' aws sso login --profile cl-data-admin.

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
  load_config()
  RAthena::RAthena_options(verbose = FALSE)

  args <- list(
    drv            = RAthena::athena(),
    region_name    = Sys.getenv("AWS_REGION", "ca-central-1"),
    s3_staging_dir = Sys.getenv("ATHENA_S3_STAGING_DIR")
  )

  # Pass profile_name only when set. CI has none: credentials come from env
  # vars, and an empty profile_name fails the lookup.
  profile <- Sys.getenv("AWS_PROFILE")
  if (nzchar(profile)) args$profile_name <- profile

  do.call(DBI::dbConnect, args)
}
