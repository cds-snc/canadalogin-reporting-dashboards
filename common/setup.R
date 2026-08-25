#' Shared setup for every dashboard in this repo.
#'
#' Sourced as the first line of a dashboard's setup chunk. Quarto's
#' execute-dir is the project root (see _quarto.yml), so the path is
#' "common/setup.R" whatever depth the dashboard sits at.
#'
#' Loads the packages every dashboard uses and sources the shared modules.
#' It deliberately has no side effects beyond that: it does not open a
#' connection, and it reads no data. A dashboard calls connect_athena()
#' itself, so a dashboard that does not need Athena does not open one.

suppressPackageStartupMessages({
  library(DBI)
  library(RAthena)
  library(dplyr)
  library(dbplyr)
  library(tidyr)
  library(glue)
  library(purrr)
  library(ggplot2)
  library(scales)
  library(gt)
})

source("common/connection.R")
source("common/gcorg.R")
source("common/registry.R")
source("common/branding.R")
source("common/colours.R")

# A dashboard's own directory, for anything that has to sit beside its qmd.
#
# The working directory is the project root, so a bare filename does not mean
# "next to this dashboard" any more. Two things need this. Files a dashboard
# writes for its own front matter to pull in (include-before-body,
# include-after-body), since those paths resolve against the qmd's directory -
# which is also what keeps N dashboards from overwriting each other's banner.
# And files a dashboard reads from beside itself, such as a knit_child()
# template.
render_dir <- function() {
  input <- knitr::current_input(dir = TRUE)
  if (is.null(input)) "." else dirname(input)
}
