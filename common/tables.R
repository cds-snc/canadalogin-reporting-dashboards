# In-row bars -----------------------------------------------------------------

# A bar as wide as the share, clamped to its cell, for a gt column passed
# through unescaped. A share above zero keeps a 2px sliver.
# `label` is read by screen readers; `track` draws a line at 100%.
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

share_bar_track_colour <- "#8A9299"

# Table layout ----------------------------------------------------------------

# Full width, no top border, at the dashboards' usual text size and row height.
table_options <- function(table, font_size = 14, padding = 4) {
  gt::tab_options(
    table,
    table.width = gt::pct(100),
    table.font.size = gt::px(font_size),
    data_row.padding = gt::px(padding),
    table.border.top.style = "none"
  )
}
