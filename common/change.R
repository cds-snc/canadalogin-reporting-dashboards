# Change arrows ---------------------------------------------------------------

# Direction is carried by the arrow and the sign, not by colour alone. A change
# with nothing to compare against is a dash, deliberately distinct from a
# measured "no change". A relative change is flat within half a percent of the
# base; a change in a rate (points = TRUE) is a fraction and flat within half
# a tenth of a point. gt colours the cells, see delta_colours.

flat_threshold <- function(points) if (points) 0.0005 else 0.005

# "up" / "down" / "flat"; an uncalculable change is flat so it formats as a dash.
delta_direction <- function(delta, points = FALSE) {
  dplyr::case_when(
    is.na(delta) | abs(delta) <= flat_threshold(points) ~ "flat",
    delta > 0 ~ "up",
    .default = "down"
  )
}

# "▲ +12%", or "▲ +1.2 pts" when points = TRUE.
fmt_delta <- function(delta, points = FALSE) {
  direction <- delta_direction(delta, points)
  text <- sprintf(if (points) "%+.1f pts" else "%+.0f%%", delta * 100)
  dplyr::case_when(
    is.na(delta) ~ "-",
    direction == "up" ~ paste("▲", text),
    direction == "down" ~ paste("▼", text),
    .default = "no change"
  )
}

# Greener than brand forest, darker than brand green, which is too light to
# pass contrast. 5.4:1 on white.
delta_colours <- c(up = "#0F7B32", down = "#AB2328", flat = "#5C6670")
