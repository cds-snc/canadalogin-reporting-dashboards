#!/usr/bin/env Rscript
#
# Rewrites _brand.yml's logo paths to absolute ones, as _brand.generated.yml.
# Run as the project's pre-render step; _quarto.yml points `brand:` at the
# generated file. _brand.yml remains the source of brand truth and is the only
# one anybody edits.
#
# Why this exists. On Quarto 1.9.37 through 1.11.1, a document in ANY
# subdirectory of the project fails to render when the project declares a brand
# with a logo block and the format sets embed-resources: true. Quarto hands
# pandoc a root-absolute "/img/cds-snc.png" whatever the brand file says, and
# pandoc cannot read it:
#
#   pandoc: /img/cds-snc.png: withBinaryFile: does not exist
#
# Every portable path form fails - "img/", "./img/", "../../img/", the logo
# moved to the dashboard's own front matter, a copy of img/ beside the qmd, and
# data-URI logos (those blow pandoc's argument limit). Only an absolute
# filesystem path works, and an absolute path cannot be committed. Hence
# generating one at render time.
#
# Since dashboards live one level down by design, this is load-bearing: without
# it nothing in dashboards/ renders at all. On 1.10.18 and 1.11.1 the failure is
# silent - exit 1 with no message - so a regression here will not announce
# itself.
#
# Delete this script and point `brand:` back at _brand.yml once Quarto resolves
# brand logo paths correctly for nested documents.

# Quarto runs a pre-render script with the project root as the working
# directory, which is also where _brand.yml and img/ live.
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

lines <- readLines("_brand.yml", warn = FALSE)

# Only the image paths, which are the values ending a "key: img/..." pair. The
# pattern stops at whitespace so a trailing comment on the line is left alone.
lines <- sub("(: )(img/[^[:space:]#]+)", paste0("\\1", root, "/\\2"), lines)

writeLines(lines, "_brand.generated.yml")
