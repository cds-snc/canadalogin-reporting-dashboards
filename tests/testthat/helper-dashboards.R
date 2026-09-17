# Loads the shared layer and a dashboard's R/ files the way its setup chunk
# does, without opening a connection.

repo_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)

# setup.R finds the repo root two levels up from a dashboard folder, and
# sources the common modules into the global environment, as a render does.
withr::with_dir(
  file.path(repo_root, "dashboards", "support"),
  source(file.path(repo_root, "common", "setup.R"))
)

# A fresh environment per call. Support and Sign-In Activity define the same
# names with different values, and a test stubs readers by assigning into it.
load_dashboard <- function(name) {
  env <- new.env(parent = globalenv())
  withr::with_dir(file.path(repo_root, "dashboards", name), {
    sys.source("R/metrics.R", envir = env)
    sys.source("R/preflight.R", envir = env)
  })
  env
}

# One shared module in its own environment, so its cached lookups can be stubbed
# without touching the copy setup.R loaded.
load_common <- function(file) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "common", file), envir = env)
  env
}

# Replace functions in a loaded environment, e.g. the table readers.
stub <- function(env, ...) {
  replacements <- list(...)
  for (name in names(replacements)) {
    stopifnot(exists(name, envir = env, inherits = FALSE))
    assign(name, replacements[[name]], envir = env)
  }
  invisible(env)
}

# The checklist and the failure warning are for a human watching a render.
run_quietly <- function(expr) suppressWarnings(suppressMessages(expr))

check_named <- function(result, slug) {
  purrr::detect(result$checks, \(check) check$slug == slug)
}

failed_slugs <- function(result) purrr::map_chr(result$failed, "slug")
