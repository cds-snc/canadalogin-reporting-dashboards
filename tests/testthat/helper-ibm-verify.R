# Daily fixtures cover the current and previous reporting windows.

ibm_today <- as.Date("2026-09-17")
ibm_days <- seq(as.Date("2026-07-01"), ibm_today - 1L, by = "day")

ibm_auth <- function(days = ibm_days, successful = 800, failed = 200) {
  tibble::tibble(
    date = days,
    unique_users = 1000,
    successful_logins = successful,
    failed_logins = failed,
    mtd_unique_users = 5000,
    ytd_unique_users = 20000
  )
}

ibm_names <- tibble::tribble(
  ~alias,     ~service_name, ~is_internal,
  "app-a",    "Service A",   FALSE,
  "app-b",    "Service B",   FALSE,
  "cl-admin", "Admin",       TRUE
)
ibm_applications <- ibm_names$alias

# Each application under its name and its ID, as in rp.alias.
ibm_lookup <- dplyr::bind_rows(
  ibm_names, dplyr::mutate(ibm_names, alias = paste0("id-", alias))
)

ibm_apps <- function(days = ibm_days, applications = ibm_applications) {
  tidyr::expand_grid(date = days, application_name = applications) |>
    dplyr::mutate(
      application_id = paste0("id-", application_name),
      total_logins = 50,
      unique_users = 40,
      successful_logins = 45,
      failed_logins = 5,
      mtd_unique_users = 400,
      ytd_unique_users = 1000
    )
}

# app-a renamed to app-a-renamed from September 2, under the same ID
ibm_renamed_apps <- function() {
  ibm_apps() |>
    dplyr::mutate(application_name = dplyr::if_else(
      application_name == "app-a" & date > as.Date("2026-09-01"),
      "app-a-renamed", application_name
    ))
}

# ibm_lookup plus one alias of Service A
ibm_lookup_with <- function(alias) {
  dplyr::add_row(ibm_lookup, alias = alias, service_name = "Service A",
                 is_internal = FALSE)
}

ibm_mfa <- function(days = ibm_days) {
  tidyr::expand_grid(date = days, mfa_type = c("sms_otp", "fido2", "email_otp"),
                     result = c("success", "failure")) |>
    dplyr::mutate(count = 10)
}

# Labels through the real label_app_logins(), so the join is tested too.
ibm_with_data <- function(auth = ibm_auth(), apps = ibm_apps(), mfa = ibm_mfa(),
                          lookup = ibm_lookup) {
  env <- load_dashboard("ibm-verify")
  labelled <- env$label_app_logins(apps, lookup)
  stub(
    env,
    auth_totals = function(con) auth,
    app_logins = function(con) apps,
    mfa_activity = function(con) mfa,
    labelled_app_logins = function(con) labelled
  )
}

run_ibm_preflight <- function(env, today = ibm_today) {
  run_quietly(env$run_preflight_safety_check(NULL, today = today))
}
