#' CDS/SNC logo branding for ggplot graphs.
#'
#' add_cds_logo() overlays the logo with cowplot::draw_image, which preserves its
#' aspect ratio. The result is a cowplot drawing knitr renders like any other
#' figure, so embed-resources inlines it and no external file is referenced.
#'
#' The mark is bilingual and the variant is picked at random per call, by design:
#' graphs in one report may differ in language, and a re-render may flip them.

suppressPackageStartupMessages({
  library(cowplot)
  library(ggplot2)
})

# Assets live at the project root, which is also the working directory every
# dashboard renders in; see execute-dir in _quarto.yml.
cds_logo_path <- function(canada_wordmark = FALSE) {
  variants <- if (canada_wordmark) {
    c("EN_Square+CANADA.jpg", "FR_Square+CANADA.jpg")
  } else {
    c("cds-snc.png", "snc-cds.png")
  }
  present <- file.path("img", variants)
  present <- present[file.exists(present)]
  if (length(present) > 0) return(sample(present, 1))
  stop(
    "CDS logo not found in img/ (", paste(variants, collapse = " / "), ")",
    call. = FALSE
  )
}

# Overlay the logo flush in a corner. `height` is a fraction of the figure;
# width is generous so height is what constrains it, keeping it undistorted.
add_cds_logo <- function(
    plot,
    position = c(
      "top-right", "bottom-left",
      "top-left", "bottom-right",
      "all"
    ),
    height = 0.13,
    canada_wordmark = FALSE) {
  position <- match.arg(position)

  corners <- list(
    "top-right" = list(
      x = 0.995, y = 0.995,
      hjust = 1, vjust = 1, halign = 1, valign = 1
    ),
    "bottom-left" = list(
      x = 0.005, y = 0.005,
      hjust = 0, vjust = 0, halign = 0, valign = 0
    ),
    "top-left" = list(
      x = 0.005, y = 0.995,
      hjust = 0, vjust = 1, halign = 0, valign = 1
    ),
    "bottom-right" = list(
      x = 0.995, y = 0.005,
      hjust = 1, vjust = 0, halign = 1, valign = 0
    )
  )

  if (position == "all") {
    placements <- corners
  } else {
    placements <- corners[position]
  }

  # Margin on the logo's side, so it lands in whitespace rather than the panel.
  margin_pt <- grid::unit(height * 55, "pt")
  sides <- unique(sub("-.*", "", names(placements)))
  current_margin <- ggplot2::calc_element("plot.margin", plot$theme)
  if (is.null(current_margin)) current_margin <- ggplot2::margin(5.5, 5.5, 5.5, 5.5)
  if ("top" %in% sides) current_margin[1] <- current_margin[1] + margin_pt
  if ("bottom" %in% sides) current_margin[3] <- current_margin[3] + margin_pt
  plot <- plot + ggplot2::theme(plot.margin = current_margin)

  result <- cowplot::ggdraw(plot)
  for (corner in placements) {
    result <- result +
      cowplot::draw_image(
        cds_logo_path(canada_wordmark),
        x = corner$x, y = corner$y,
        hjust = corner$hjust, vjust = corner$vjust,
        halign = corner$halign,
        valign = corner$valign,
        width = 0.4, height = height
      )
  }
  result
}


# Watermark -------------------------------------------------------------------

# Light bottom-right watermark: dashboard name and data-through date. There is
# no edition number, since a dashboard is re-rendered in place.
#
# report_name is an argument rather than a constant because this file is shared
# by every dashboard in the repo. The default is the dashboard that was here
# first; a new dashboard must pass its own name. The cds package's copy of this
# function takes the same argument.
add_watermark <- function(plot,
                          report_name = "CanadaLogin Experience Monitoring",
                          date = Sys.Date()) {
  if (inherits(date, "Date")) date <- format(date, "%B %e, %Y")
  label <- paste0(report_name, " // Data through ", trimws(date))
  font_family <- if (register_cds_fonts()) cds_font else ""
  current_margin <- ggplot2::calc_element("plot.margin", plot$theme)
  if (is.null(current_margin)) current_margin <- ggplot2::margin(5.5, 5.5, 5.5, 5.5)
  current_margin[3] <- current_margin[3] + grid::unit(5, "pt")
  plot <- plot + ggplot2::theme(plot.margin = current_margin)
  cowplot::ggdraw(plot) +
    cowplot::draw_text(
      label,
      x = 0.99, y = 0.01,
      hjust = 1, vjust = 0,
      size = 7,
      colour = "grey65",
      family = font_family
    )
}

# Brand typography ------------------------------------------------------------

#' Brand typeface for ggplot graphs, matching the document typography in
#' _brand.yml. Loaded from the TTFs vendored in fonts/, so a render needs no
#' network; offline it falls back to the default sans rather than failing.

# The current release of the brand guide's "Source Sans Pro", which _brand.yml
# and _fonts.scss name unsuffixed for the document text. Suffixed here because
# systemfonts will not register a family that shadows an installed system font.
cds_font <- "Source Sans 3 (CDS)"

# At the project root, like img/. TTF, not the WOFF2 the dashboard embeds:
# systemfonts cannot read WOFF2, so fonts/ holds both formats.
cds_font_dir <- function() {
  if (file.exists(file.path("fonts", "source-sans-3-400-normal.ttf"))) "fonts" else NULL
}

# Semibold (600) is mapped to the "bold" face, so theme titles render as
# Semibold rather than a heavier 700. Safe to call per plot.
register_cds_fonts <- function() {
  dir <- cds_font_dir()
  if (is.null(dir)) {
    warning(
      "Could not find the brand font files in fonts/; ",
      "graphs will use the default sans font.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  systemfonts::register_font(
    cds_font,
    plain = file.path(dir, "source-sans-3-400-normal.ttf"),
    bold  = file.path(dir, "source-sans-3-600-normal.ttf")
  )
  invisible(TRUE)
}

# theme_bw() in the brand typeface, with Semibold titles.
theme_cds <- function(base_size = 11, base_family = cds_font) {
  if (!register_cds_fonts()) base_family <- ""
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey30"),
      plot.caption = element_text(colour = "grey40")
    )
}
