#' Operator abbreviations from the GC organization dataset, resolved through
#' https://github.com/cds-snc/gcorg-resolver.
#'
#' `rp.service` carries the operating organization's `gc_orgid`, which the
#' resolver takes as an alias of itself, so tables can show "VAC" rather than
#' "Veterans Affairs Canada".

# One abbreviation per id, in input order. NA where the dataset holds no such
# organization, gc_orgid 0 included, and NA throughout if the resolver is down.
gcorg_abbreviations <- function(gc_orgid) {
  ids <- as.character(gc_orgid)
  body <- jsonlite::toJSON(
    list(names = unique(ids[!is.na(ids)])), auto_unbox = FALSE
  )
  handle <- curl::new_handle(timeout = 15L)
  curl::handle_setheaders(handle, "Content-Type" = "application/json")
  curl::handle_setopt(handle, post = TRUE, postfields = charToRaw(body))

  response <- tryCatch(
    curl::curl_fetch_memory("https://gcorgs.cdssandbox.xyz/resolve", handle),
    error = function(e) NULL
  )
  if (is.null(response) || response$status_code != 200L) {
    return(NA_character_)
  }

  # as.character() because a response that matched nothing at all parses the
  # column as logical, which would clash on type where the result is coalesced.
  resolved <- jsonlite::fromJSON(rawToChar(response$content))$results
  as.character(resolved$abbreviation)[match(ids, resolved$input)]
}
