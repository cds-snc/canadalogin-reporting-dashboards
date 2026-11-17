#' Relying-party labels from rp.alias and rp.service, synced each weekday from
#' a hand-kept sheet. An alias is a GA rp_name, or an IBM Verify application
#' name or ID.

# GA's value when the rp_name event did not fire. Not a relying party.
ga_excluded_rp_names <- c("", "(not set)")

# IBM Verify's value for an event with no application ID. Not a relying party.
ibm_excluded_application_ids <- "UNKNOWN"

# alias -> service_name, operator, is_internal. Cached for the session.
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

    # Never filter on `source`: it records where an alias was first seen.
    cached <<- aliases |>
      dplyr::inner_join(services, by = "rp_id") |>
      dplyr::select(-rp_id) |>
      dplyr::collect() |>
      dplyr::mutate(
        operator_abbr = dplyr::coalesce(gcorg_abbreviations(gc_orgid), operator)
      )
    cached
  }
})

# One row per service, keyed by service name.
relying_party_services <- function(con) {
  relying_party_lookup(con) |>
    dplyr::distinct(service_name, .keep_all = TRUE) |>
    dplyr::select(service_name, operator, operator_abbr, is_internal)
}

# Labels for each alias, in input order; an unknown alias is NA.
label_relying_parties <- function(con, aliases) {
  dplyr::tibble(alias = aliases) |>
    dplyr::left_join(relying_party_lookup(con), by = "alias")
}

# Abbreviated operator for each service name.
service_operators <- function(con, service_names) {
  dplyr::tibble(service_name = service_names) |>
    dplyr::left_join(relying_party_services(con), by = "service_name") |>
    dplyr::pull(operator_abbr)
}

# "Full operator name (ABBR)", or the bare name with no abbreviation.
service_operator_labels <- function(con, service_names) {
  rows <- dplyr::tibble(service_name = service_names) |>
    dplyr::left_join(relying_party_services(con), by = "service_name")
  ifelse(is.na(rows$operator) | rows$operator == rows$operator_abbr,
         rows$operator,
         paste0(rows$operator, " (", rows$operator_abbr, ")"))
}

# TRUE for each alias of an internal service. Unknown aliases count as
# external.
is_internal_rp <- function(con, aliases) {
  labels <- label_relying_parties(con, aliases)
  !is.na(labels$is_internal) & labels$is_internal
}
