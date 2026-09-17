# Load dashboard code without connecting to Athena.

repo_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)

# Mirror a setup chunk, which sources shared code globally.
withr::with_dir(
  file.path(repo_root, "dashboards", "support"),
  source(file.path(repo_root, "common", "setup.R"))
)

# Isolate dashboards because some function names overlap.
load_dashboard <- function(name) {
  env <- new.env(parent = globalenv())
  withr::with_dir(file.path(repo_root, "dashboards", name), {
    sys.source("R/metrics.R", envir = env)
    sys.source("R/preflight.R", envir = env)
  })
  env
}

# Isolate shared modules so tests can stub their caches.
load_common <- function(file) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "common", file), envir = env)
  env
}

stub <- function(env, ...) {
  replacements <- list(...)
  for (name in names(replacements)) {
    stopifnot(exists(name, envir = env, inherits = FALSE))
    assign(name, replacements[[name]], envir = env)
  }
  invisible(env)
}

# Preflight output belongs in dashboard renders, not tests.
run_quietly <- function(expr) suppressWarnings(suppressMessages(expr))

check_named <- function(result, slug) {
  purrr::detect(result$checks, \(check) check$slug == slug)
}

failed_slugs <- function(result) purrr::map_chr(result$failed, "slug")
