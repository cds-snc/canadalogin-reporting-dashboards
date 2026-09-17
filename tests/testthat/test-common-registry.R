# Fixtures --------------------------------------------------------------------

# The lookup as relying_party_lookup() returns it once collected and resolved.
registry_with_lookup <- function() {
  env <- load_common("registry.R")
  lookup <- tibble::tribble(
    ~alias,         ~service_name,       ~operator,                 ~is_internal,
    "vac-portal",   "VAC Portal",        "Veterans Affairs Canada", FALSE,
    "vac-portal-2", "VAC Portal",        "Veterans Affairs Canada", FALSE,
    "cl-admin",     "CanadaLogin Admin", "CDS",                     TRUE,
    "no-abbr",      "Unabbreviated",     "Some Office",             FALSE
  ) |>
    dplyr::mutate(
      gc_orgid = c("1", "1", "2", "0"),
      operator_abbr = c("VAC", "VAC", "CDS", "Some Office")
    )
  stub(env, relying_party_lookup = function(con) lookup)
}

# Labelling -------------------------------------------------------------------

test_that("labels come back one row per name, in input order", {
  env <- registry_with_lookup()
  labelled <- env$label_relying_parties(NULL, c("cl-admin", "vac-portal"))
  expect_identical(labelled$alias, c("cl-admin", "vac-portal"))
  expect_identical(labelled$service_name, c("CanadaLogin Admin", "VAC Portal"))
})

test_that("an unknown name is labelled NA rather than dropped", {
  env <- registry_with_lookup()
  labelled <- env$label_relying_parties(NULL, c("vac-portal", "never-seen"))
  expect_identical(nrow(labelled), 2L)
  expect_true(is.na(labelled$service_name[2]))
})

test_that("an internal service's name is internal", {
  env <- registry_with_lookup()
  expect_identical(env$is_internal_rp(NULL, c("cl-admin", "vac-portal")),
                   c(TRUE, FALSE))
})

test_that("an unknown name counts as external", {
  env <- registry_with_lookup()
  expect_false(env$is_internal_rp(NULL, "never-seen"))
})

test_that("GA's unattributed names are excluded from labelling", {
  expect_setequal(ga_excluded_rp_names, c("", "(not set)"))
})

# Services --------------------------------------------------------------------

test_that("a service with several aliases is one service row", {
  env <- registry_with_lookup()
  services <- env$relying_party_services(NULL)
  expect_identical(sum(services$service_name == "VAC Portal"), 1L)
})

test_that("an operator label carries its abbreviation in brackets", {
  env <- registry_with_lookup()
  expect_identical(env$service_operator_labels(NULL, "VAC Portal"),
                   "Veterans Affairs Canada (VAC)")
})

test_that("an operator label is bare when the abbreviation is the name", {
  env <- registry_with_lookup()
  expect_identical(env$service_operator_labels(NULL, "Unabbreviated"), "Some Office")
})

test_that("an operator label is NA for an unknown service", {
  env <- registry_with_lookup()
  expect_true(is.na(env$service_operator_labels(NULL, "Nothing")))
})

test_that("operators come back abbreviated, in input order", {
  env <- registry_with_lookup()
  expect_identical(env$service_operators(NULL, c("CanadaLogin Admin", "VAC Portal")),
                   c("CDS", "VAC"))
})
