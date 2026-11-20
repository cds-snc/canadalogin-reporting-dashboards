# In-row bars -----------------------------------------------------------------

# A bar as wide as the share, for a gt cell passed through unescaped
# (fmt_passthrough(escape = FALSE) or fmt_markdown()). Clamped to the cell, so a
# share over 1 draws full width rather than spilling into the next column, and
# any share above zero keeps a sliver, so it does not read as none.
# `label` names the share for screen readers where no number sits beside it.
# `track = TRUE` draws a thin line at 100%, so a bar's distance from full reads
# at a glance.
share_bar <- function(share, fill, label = NULL, track = FALSE) {
  width <- round(100 * pmin(pmax(share, 0), 1))
  sliver <- ifelse(!is.na(share) & share > 0, "min-width:2px;", "")
  a11y <- if (is.null(label)) "" else
    sprintf(" role='img' aria-label='%s'", htmltools::htmlEscape(label, TRUE))
  bar <- sprintf("<div%s style='background:%s;width:%s%%;%sheight:14px'></div>",
                 a11y, fill, width, sliver)
  if (!track) return(bar)
  paste0("<div style='border-right:2px solid ", share_bar_track_colour, "'>",
         bar, "</div>")
}

# The 100% line: quiet beside the bar, still visible on a striped row.
share_bar_track_colour <- "#8A9299"
