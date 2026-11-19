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

# Colouring a change -----------------------------------------------------------

# Whether a change is good news, bad news, or neither: "good" / "bad" / "flat".
# `good` is the direction that is good for this metric, "up" or "down", or NA
# where neither is. `meaningful` is each dashboard's own test of whether a move
# is big enough to colour, such as a noise band; FALSE reads as "flat". A flat
# or uncalculable change is always "flat".
change_verdict <- function(delta, good = "up", meaningful = TRUE, points = FALSE) {
  direction <- delta_direction(delta, points)
  dplyr::case_when(
    direction == "flat" | !dplyr::coalesce(meaningful, FALSE) | is.na(good) ~ "flat",
    direction == good ~ "good",
    .default = "bad"
  )
}

# The colour for each verdict, from delta_colours: good news green, bad news
# red, neither grey.
verdict_colours <- c(
  good = delta_colours[["up"]], bad = delta_colours[["down"]],
  flat = delta_colours[["flat"]]
)

# A change as HTML, "▲ +12%" in its verdict's colour, for prose or a value box.
# `text` replaces the formatted change where a dashboard words it differently.
change_html <- function(delta, good = "up", meaningful = TRUE, points = FALSE,
                        text = fmt_delta(delta, points)) {
  verdict <- change_verdict(delta, good, meaningful, points)
  sprintf('<span style="color: %s">%s</span>', verdict_colours[verdict],
          htmltools::htmlEscape(text))
}

# Colours a gt column by a verdict column of the same table, which may be
# hidden. A row whose verdict is NA keeps the default colour.
colour_changes <- function(gt_table, column, verdict) {
  for (v in names(verdict_colours)) {
    gt_table <- gt::tab_style(
      gt_table, gt::cell_text(color = verdict_colours[[v]]),
      gt::cells_body(columns = dplyr::all_of(column),
                     rows = !!rlang::sym(verdict) %in% !!v)
    )
  }
  gt_table
}
