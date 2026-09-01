#' Relying-party labelling, from the rp.alias and rp.service lookup tables in
#' the data lake. rp.alias maps each source-system string (GA's rp_name
#' included) to an rp_id; rp.service holds the service's name, operator and
#' is_internal. Both are synced each weekday from a hand-maintained sheet by
#' the rp-sync Lambda in the pipeline repo. Operators are abbreviated through
#' common/gcorg.R. Each dashboard's own preflight (rp-lookup in Experience
#' Monitoring, rp-resolution in Sign-In Activity) fails the render on any
#' unlabelled rp_name.

# What GA reports when the rp_name custom event did not fire. Not relying
# parties, so exempt from labelling and from the lookup gate.
ga_excluded_rp_names <- c("", "(not set)")

# alias -> service_name, operator, is_internal. Small and read many times per
# render, so fetched once and cached for the session.
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

    # Join on alias alone; `source` records where a name was first seen, not
    # which system it belongs to, so never filter on it.
    cached <<- aliases |>
      dplyr::inner_join(services, by = "rp_id") |>
      dplyr::select(-rp_id) |>
      dplyr::collect() |>
      # Inside the cached lookup, so a render makes one resolver call.
      dplyr::mutate(
        operator_abbr = dplyr::coalesce(gcorg_abbreviations(gc_orgid), operator)
      )
    cached
  }
})

# One row per service, keyed by the service name shown in tables. is_internal
# is a service-level column in this schema, so distinct() loses nothing.
relying_party_services <- function(con) {
  relying_party_lookup(con) |>
    dplyr::distinct(service_name, .keep_all = TRUE) |>
    dplyr::select(service_name, operator, operator_abbr, is_internal)
}

# service_name, operator and is_internal for each rp_name. One row per input,
# in input order; an unknown name is NA rather than dropped.
label_relying_parties <- function(con, rp_names) {
  dplyr::tibble(alias = rp_names) |>
    dplyr::left_join(relying_party_lookup(con), by = "alias")
}

# Abbreviated operator for each service name.
service_operators <- function(con, service_names) {
  dplyr::tibble(service_name = service_names) |>
    dplyr::left_join(relying_party_services(con), by = "service_name") |>
    dplyr::pull(operator_abbr)
}

# TRUE for each rp_name belonging to an internal service. Unknown names count
# as external: they are real, unattributed users.
is_internal_rp <- function(con, rp_names) {
  labels <- label_relying_parties(con, rp_names)
  !is.na(labels$is_internal) & labels$is_internal
}
