#' CDS/SNC logo branding for ggplot graphs.
#'
#' add_cds_logo() overlays the logo with cowplot::draw_image, preserving its
#' aspect ratio. The result renders like any other figure, so embed-resources
#' inlines it. The mark is bilingual and the variant is picked at random per
#' call, deliberately: graphs in one report may differ in language.

suppressPackageStartupMessages({
  library(cowplot)
  library(ggplot2)
})

# Shared assets sit at the repo root; repo_path() comes from common/setup.R.
cds_logo_path <- function(canada_wordmark = FALSE) {
  variants <- if (canada_wordmark) {
    c("EN_Square+CANADA.jpg", "FR_Square+CANADA.jpg")
  } else {
    c("cds-snc.png", "snc-cds.png")
  }
  present <- repo_path("img", variants)
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

# Light bottom-right watermark: report name and data-through date. A dashboard
# other than the default passes its own report_name.
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
#' _brand.yml. Loaded from the TTFs vendored in fonts/; if they are missing it
#' falls back to the default sans rather than failing.

# Suffixed because systemfonts will not register a family that shadows an
# installed system font; the document text uses the unsuffixed name.
cds_font <- "Source Sans 3 (CDS)"

# TTF, not the WOFF2 the document embeds: systemfonts cannot read WOFF2, so
# fonts/ holds both formats.
cds_font_dir <- function() {
  dir <- repo_path("fonts")
  if (file.exists(file.path(dir, "source-sans-3-400-normal.ttf"))) dir else NULL
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
