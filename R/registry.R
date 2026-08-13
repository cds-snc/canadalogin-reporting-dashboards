#' Relying-party labelling: shared registry plus a local GA-name crosswalk.
#'
#' The registry is Signal Check's hand-maintained file, read cross-repo so
#' operator and is_internal have one source of truth. It is keyed by
#' application_name; GA breaks down by customEvent:rp_name instead, so
#' data/ga_rp_names.csv bridges the two through service_name.
#'
#' Preflight check 3 fails the render on any rp_name this cannot label.

# Named constant so the cross-repo dependency is visible in one place.
relying_parties_path <- "../canadalogin-signal-check/data/relying_parties.csv"

# Local GA rp_name -> registry service_name crosswalk.
ga_rp_crosswalk_path <- "data/ga_rp_names.csv"

# What GA reports when the rp_name custom event was absent. Not relying parties,
# so exempt from labelling and from the registry gate.
ga_excluded_rp_names <- c("", "(not set)")

# The shared registry: application_name, service_name, operator, is_internal.
load_relying_parties <- function(path = relying_parties_path) {
  if (!file.exists(path)) {
    stop(
      "Relying-party registry not found at '", path, "'.\n",
      "It is read cross-repo from the sibling canadalogin-signal-check repo; ",
      "check that repo out beside this one.",
      call. = FALSE
    )
  }
  readr::read_csv(
    path,
    col_types = readr::cols(
      .default = readr::col_character(),
      is_internal = readr::col_logical()
    )
  )
}

# The local crosswalk: rp_name, service_name.
load_ga_rp_crosswalk <- function(path = ga_rp_crosswalk_path) {
  if (!file.exists(path)) {
    stop("GA rp_name crosswalk not found at '", path, "'.", call. = FALSE)
  }
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
}

# One row per service_name, the grain GA identifies parties at. is_internal is an
# application property, so a service is internal only when all of its apps are.
registry_by_service <- function() {
  load_relying_parties() |>
    dplyr::group_by(service_name) |>
    dplyr::summarise(
      operator = dplyr::first(operator),
      is_internal = all(is_internal),
      .groups = "drop"
    )
}

# service_name, operator and is_internal for each rp_name. Exactly one row per
# input, in input order, with rp_name preserved as GA's own label.
label_relying_parties <- function(rp_names) {
  dplyr::tibble(rp_name = rp_names) |>
    dplyr::left_join(load_ga_rp_crosswalk(), by = "rp_name") |>
    dplyr::left_join(registry_by_service(), by = "service_name")
}

# TRUE for each rp_name belonging to an internal service. An rp_name the
# crosswalk does not know is external: those are real, unattributed users.
is_internal_rp <- function(rp_names) {
  labels <- label_relying_parties(rp_names)
  !is.na(labels$is_internal) & labels$is_internal
}
