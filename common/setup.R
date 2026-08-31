#' Shared setup for every dashboard: attaches the common packages and sources
#' the shared modules. Opens no connection and reads no data.

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

# A dashboard renders with its own folder as the working directory, so two
# levels up is the repo root.
repo_root <- normalizePath("../..", winslash = "/", mustWork = TRUE)

# A path at the repo root, for assets shared by every dashboard.
repo_path <- function(...) file.path(repo_root, ...)

# Point reticulate at the project venv so RAthena gets the boto3 pinned in
# requirements.txt.
venv_python <- repo_path(".venv", "bin", "python")
if (file.exists(venv_python)) Sys.setenv(RETICULATE_PYTHON = venv_python)

source(repo_path("common", "connection.R"))
source(repo_path("common", "gcorg.R"))
source(repo_path("common", "registry.R"))
source(repo_path("common", "branding.R"))
source(repo_path("common", "colours.R"))
source(repo_path("common", "preflight.R"))
