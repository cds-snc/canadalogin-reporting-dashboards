#' Operator abbreviations from the GC organization dataset, resolved through
#' https://github.com/cds-snc/gcorg-resolver, so tables can show "VAC" rather
#' than "Veterans Affairs Canada". Resolves the gc_orgid from rp.service, which
#' the resolver accepts as an alias of itself.

# One abbreviation per id, in input order. NA where the dataset has no such
# organization (gc_orgid 0 included); all NA if the resolver is unreachable.
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

  # as.character(): a response with no matches parses the column as logical,
  # which would break the coalesce downstream on type.
  resolved <- jsonlite::fromJSON(rawToChar(response$content))$results
  as.character(resolved$abbreviation)[match(ids, resolved$input)]
}
