#' Chart annotations from a dashboard's own annotations.yml: standing notes
#' under a chart, and dated events drawn on it as bands and arrows.
#'
#' Each entry names the charts it belongs to under a dashboard-chosen key, the
#' `scope` (expmon's is "funnel"), or "all". Nothing is checked at render: the
#' checks are in the tests (helper-annotations.R), run on every PR.

# Room above the top of the y range for letters and arrows, as a fraction of
# that range. The y scale also needs oob_keep, or an arrow above it is dropped.
annotation_headroom <- 0.2

# Reading ---------------------------------------------------------------------

read_annotations <- function(path = "annotations.yml", scope) {
  entries <- yaml::read_yaml(path)
  field <- function(name, default) {
    purrr::map(entries, \(entry) entry[[name]] %||% default)
  }
  start <- as.Date(unlist(field("start", NA_character_)))
  tibble::tibble(
    scope = field(scope, NULL),
    start = start,
    # A single day ends where it starts.
    end = dplyr::coalesce(as.Date(unlist(field("end", NA_character_))), start),
    description = unlist(field("description", NA_character_)),
    chart = unlist(field("chart", TRUE))
  )
}

# One chart's entries in caption order: standing notes as written, then dated
# ones by start. Dated ones wholly outside `from` to `to` are dropped, and only
# drawn ones are lettered so the letters never skip.
scoped_annotations <- function(annotations, id, from, to) {
  mine <- purrr::map_lgl(annotations$scope, \(scope) any(c("all", id) %in% scope))
  standing <- annotations[mine & is.na(annotations$start), ]
  dated <- annotations[mine & !is.na(annotations$start), ]
  dated <- dated[dated$end >= from & dated$start <= to, ]
  dated <- dated[order(dated$start), ]
  dated$letter <- NA_character_
  dated$letter[dated$chart] <- LETTERS[seq_len(sum(dated$chart))]
  dplyr::bind_rows(standing, dated)
}

# Caption ---------------------------------------------------------------------

# "Sep 30, 2026", "Sep 17 to Oct 1, 2026", or a year on each end when a range
# crosses one. month.abb rather than %b, which follows the locale.
fmt_event_dates <- function(start, end) {
  day <- function(date) {
    paste(month.abb[as.integer(format(date, "%m"))],
          as.integer(format(date, "%d")))
  }
  start_year <- format(start, "%Y")
  end_year <- format(end, "%Y")
  dplyr::case_when(
    start == end ~ paste0(day(end), ", ", end_year),
    start_year == end_year ~ paste0(day(start), " to ", day(end), ", ", end_year),
    .default = paste0(day(start), ", ", start_year, " to ", day(end), ", ",
                      end_year)
  )
}

# Joined with <br>: a blank line in a fig-cap breaks the figure.
annotation_caption <- function(events) {
  lines <- events$description
  dated <- !is.na(events$start)
  lines[dated] <- paste0(fmt_event_dates(events$start[dated], events$end[dated]),
                         ": ", lines[dated])
  lettered <- !is.na(events$letter)
  lines[lettered] <- paste0("**", events$letter[lettered], ".** ",
                            lines[lettered])
  paste(lines, collapse = "<br>")
}

# Drawing ---------------------------------------------------------------------

# Bands for ranges and arrows for single days, square to `line`. Grey and added
# first, so events sit behind the data. Bands are clipped to `from` and `to`,
# since the x scale has no fixed limits, and translucent with dashed edges so
# overlaps stay apart.
annotation_layers <- function(events, from, to, line, line_mapping,
                              y_range = c(0, 1), aspect = 0.36) {
  drawn <- events[!is.na(events$letter), ]
  drawn$xmin <- pmax(drawn$start, from)
  drawn$xmax <- pmin(drawn$end, to)
  drawn$middle <- drawn$xmin + as.numeric(drawn$xmax - drawn$xmin) / 2
  band <- drawn[drawn$xmin < drawn$xmax, ]
  day <- drawn[drawn$xmin == drawn$xmax, ]
  c(
    list(
      ggplot2::geom_rect(
        data = band,
        ggplot2::aes(xmin = xmin, xmax = xmax),
        ymin = -Inf, ymax = Inf, fill = "grey50", alpha = 0.12,
        inherit.aes = FALSE
      ),
      ggplot2::geom_vline(
        xintercept = unique(c(band$xmin, band$xmax)),
        colour = "grey55", linetype = "dashed", linewidth = 0.4
      )
    ),
    geom_perpendicular_arrow(
      ggplot2::aes(x = xmin, label = letter), day,
      line = line, line_mapping = line_mapping, x_range = c(from, to),
      y_range = y_range, aspect = aspect
    ),
    list(
      # Inf lands at the top of the headroom.
      ggplot2::geom_text(
        data = band, ggplot2::aes(x = middle, label = letter),
        y = Inf,
        vjust = 1.4, colour = "grey45", fontface = "bold", size = 3.3,
        inherit.aes = FALSE
      )
    )
  )
}

# An arrow at each `x`, pointing at `line` and square to it on screen, with an
# optional `label` at its tail. Returns a segment layer, plus a text layer given
# a label. Square only when `aspect` (panel height over width, 0.36 at 7.7 by
# 3.6 inches) is right, the x scale pads 5% each side and the y scale adds
# `headroom` on top. Distances are in panel widths.
geom_perpendicular_arrow <- function(mapping, data, line, line_mapping,
                                     x_range, y_range = c(0, 1),
                                     aspect = 0.36,
                                     headroom = annotation_headroom,
                                     colour = "grey45", linewidth = 0.5,
                                     size = 3.3) {
  x <- rlang::eval_tidy(mapping$x, data)
  line_x <- rlang::eval_tidy(line_mapping$x, line)
  line_y <- rlang::eval_tidy(line_mapping$y, line)
  # Interpolated, so an x missing from the line still lands on it.
  at <- function(at_x) {
    stats::approx(line_x, line_y, at_x, rule = 2, ties = mean)$y
  }
  per_x <- 1 / (as.numeric(diff(x_range)) * 1.1)
  per_y <- aspect / (diff(y_range) * (1 + headroom))
  slope <- (at(x + 3) - at(x - 3)) / 6 * per_y / per_x
  # The unit normal pointing up.
  nx <- -slope / sqrt(1 + slope^2)
  ny <- 1 / sqrt(1 + slope^2)
  y <- at(x)
  along_x <- function(dist) x + nx * dist / per_x
  along_y <- function(dist) y + ny * dist / per_y

  data$tip_x <- along_x(0.006)
  data$tip_y <- along_y(0.006)
  data$tail_x <- along_x(0.036)
  data$tail_y <- along_y(0.036)
  layers <- list(
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(x = tail_x, xend = tip_x, y = tail_y, yend = tip_y),
      colour = colour, linewidth = linewidth,
      arrow = grid::arrow(length = grid::unit(0.06, "inches"), type = "closed"),
      inherit.aes = FALSE
    )
  )
  if (is.null(mapping$label)) return(layers)

  data$label_x <- along_x(0.046)
  data$label_y <- along_y(0.046)
  data$label <- rlang::eval_tidy(mapping$label, data)
  c(layers, list(
    ggplot2::geom_text(
      data = data, ggplot2::aes(x = label_x, y = label_y, label = label),
      colour = colour, fontface = "bold", size = size,
      inherit.aes = FALSE
    )
  ))
}
