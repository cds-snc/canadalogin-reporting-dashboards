#' Dependencies renv cannot infer from direct calls.
#'
#' This file is scanned by renv but never sourced.

# cowplot loads magick indirectly for logo images.
library(magick)

# dplyr needs bit64 methods for Athena BIGINT columns.
library(bit64)

# Quarto selects ragg through the document's dev option.
library(ragg)

# CI invokes lintr outside R source files.
library(lintr)
