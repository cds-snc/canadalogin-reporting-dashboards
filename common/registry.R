#' Relying-party labelling, from the shared lookup tables in the data lake.
#'
#' `rp.alias` maps every string a source system uses for a service back to an
#' rp_id; `rp.service` holds that service's names, operator and is_internal.
#' Both are refreshed each weekday from the hand-maintained source sheet by the
#' rp-sync Lambda in the pipeline repo, so this repo holds no copy of its own.
#' The operator is abbreviated on the way through; see common/gcorg.R.
#'
#' GA breaks its funnels down by customEvent:rp_name, which is one of those
#' alias strings. A service with more than one GA name pools into a single row
#' here, which the old per-name labelling could not do.
#'
#' Preflight check 3 fails the render on any rp_name this cannot label.

# What GA reports when the rp_name custom event was absent. Not relying parties,
# so exempt from labelling and from the lookup gate.
ga_excluded_rp_names <- c("", "(not set)")

# alias -> service_name, operator, is_internal. Tens of rows, read many times per
# render, so it is fetched once and memoised for the life of the session.
relying_party_lookup <- local({
  cached <- NULL
  function(con) {
    if (!is.null(cached)) {
      return(cached)
    }
    aliases <- dplyr::tbl(con, dbplyr::in_schema("rp", "alias")) |>
      dplyr::select(alias, rp_id)
    services <- dplyr::tbl(con, dbplyr::in_schema("rp", "service")) |>
      dplyr::select(rp_id, service_name = service_name_en, operator,
                    gc_orgid, is_internal)

    # Join on alias alone, never filtering on `source`: it records where a name
    # was first seen, not which system it belongs to.
    cached <<- aliases |>
      dplyr::inner_join(services, by = "rp_id") |>
      dplyr::select(-rp_id) |>
      dplyr::collect() |>
      # Inside the memoised lookup, so a render costs one call to the resolver.
      dplyr::mutate(
        operator_abbr = dplyr::coalesce(gcorg_abbreviations(gc_orgid), operator)
      )
    cached
  }
})

# One row per service, for looking attributes up by the name shown in a table.
# is_internal is a property of the service in this schema, so there is nothing
# to collapse across a service's applications.
relying_party_services <- function(con) {
  relying_party_lookup(con) |>
    dplyr::distinct(service_name, .keep_all = TRUE) |>
    dplyr::select(service_name, operator, operator_abbr, is_internal)
}

# service_name, operator and is_internal for each rp_name. Exactly one row per
# input, in input order, with an unknown name left NA rather than dropped.
label_relying_parties <- function(con, rp_names) {
  dplyr::tibble(alias = rp_names) |>
    dplyr::left_join(relying_party_lookup(con), by = "alias")
}

# The abbreviated operator of each service, by the service name a table shows.
service_operators <- function(con, service_names) {
  dplyr::tibble(service_name = service_names) |>
    dplyr::left_join(relying_party_services(con), by = "service_name") |>
    dplyr::pull(operator_abbr)
}

# TRUE for each rp_name belonging to an internal service. An rp_name the lookup
# does not know is external: those are real, unattributed users.
is_internal_rp <- function(con, rp_names) {
  labels <- label_relying_parties(con, rp_names)
  !is.na(labels$is_internal) & labels$is_internal
}
