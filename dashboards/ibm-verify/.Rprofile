# Activates the repo-root renv when the render is invoked from inside this
# folder, where R reads no other .Rprofile. renv/activate.R takes RENV_PROJECT
# over getwd(), which would otherwise be this directory.
local({
  root <- normalizePath("../..", winslash = "/", mustWork = TRUE)
  Sys.setenv(RENV_PROJECT = root)
  source(file.path(root, "renv", "activate.R"))
})
