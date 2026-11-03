# Checks for a dashboard's annotations.yml. They live here, not in the
# dashboard: people who do not write R edit the file, and CI runs each
# dashboard's file test on every PR so a bad entry fails before it merges. The
# render reads the file without checking it.

is_annotation_date <- function(value) {
  is.character(value) && length(value) == 1L &&
    !is.na(as.Date(value, format = "%Y-%m-%d"))
}

# One line per problem, naming the entry so an editor can find it. `scope` is
# the dashboard's key for which charts an entry belongs to, and `ids` its
# valid values besides "all".
annotation_problems <- function(entries, scope, ids) {
  keys <- c(scope, "start", "end", "description", "chart")
  problems <- character()
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    text <- paste(entry$description, collapse = " ")
    if (nchar(text) > 30L) text <- paste0(substr(text, 1, 30), "...")
    name <- sprintf('entry %d ("%s")', i, text)
    flag <- function(problem) {
      problems <<- c(problems, paste0(name, ": ", problem))
    }

    for (key in setdiff(names(entry), keys)) {
      flag(paste0("unknown key '", key, "'"))
    }
    if (is.null(entry[[scope]])) {
      flag(paste0("no ", scope))
    }
    for (id in setdiff(unlist(entry[[scope]]), c("all", ids))) {
      flag(paste0("unknown ", scope, " '", id, "'"))
    }
    if (!is.character(entry$description) || !nzchar(entry$description)) {
      flag("no description")
    }
    if (!is.null(entry$start) && !is_annotation_date(entry$start)) {
      flag("start is not a YYYY-MM-DD date")
    }
    if (!is.null(entry$end) && !is_annotation_date(entry$end)) {
      flag("end is not a YYYY-MM-DD date")
    }
    if (!is.null(entry$end) && is.null(entry$start)) {
      flag("end without a start")
    }
    if (is_annotation_date(entry$start) && is_annotation_date(entry$end) &&
          as.Date(entry$end) < as.Date(entry$start)) {
      flag("end is before start")
    }
    if (!is.null(entry$chart) && !isTRUE(entry$chart) && !isFALSE(entry$chart)) {
      flag("chart is not true or false")
    }
  }
  problems
}

# expect() rather than expect_equal(), so the failure lists the problems
# instead of only counting them.
expect_no_annotation_problems <- function(path, scope, ids) {
  problems <- annotation_problems(yaml::read_yaml(path), scope, ids)
  expect(length(problems) == 0L,
         paste(c(paste0(basename(path), ":"), problems), collapse = "\n  "))
}
