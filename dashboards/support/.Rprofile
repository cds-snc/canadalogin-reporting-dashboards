# Activates the repo-root renv when the render is invoked from inside this folder
local({
  root <- normalizePath("../..", winslash = "/", mustWork = TRUE)
  Sys.setenv(RENV_PROJECT = root)
  source(file.path(root, "renv", "activate.R"))
})
