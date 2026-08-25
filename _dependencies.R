#' Packages this project needs at render time that renv's scanner cannot see.
#'
#' renv builds renv.lock by parsing the code for library() and pkg:: calls, so a
#' package that is only ever loaded by another package goes missing from the lock
#' and the render then fails on a clean machine. Naming it here puts it back in.
#'
#' This file is never sourced. It exists to be read by renv::dependencies().

# cowplot::draw_image() reads the PNG logo through magick, but never names it in
# code we control. See add_cds_logo() in common/branding.R.
library(magick)

# Athena BIGINT columns arrive as integer64, which vctrs/dplyr need bit64
# loaded to handle - never named directly in code we control either.
library(bit64)

# The figures are drawn on ragg's device, selected by the `dev: ragg_png` chunk
# option in experience-monitoring.qmd rather than by any call renv can see.
library(ragg)
