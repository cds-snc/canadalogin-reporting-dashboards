# In-row bars -----------------------------------------------------------------

# A bar as wide as the share, for a gt cell passed through unescaped
# (fmt_passthrough(escape = FALSE) or fmt_markdown()). Clamped to the cell, so a
# share over 1 draws full width rather than spilling into the next column.
share_bar <- function(share, fill) {
  width <- round(100 * pmin(pmax(share, 0), 1))
  sprintf("<div style='background:%s;width:%s%%;height:14px'></div>",
          fill, width)
}
