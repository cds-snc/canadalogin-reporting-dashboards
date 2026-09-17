# Sign-In Activity fixtures, in the shape the readers return. Rows run daily
# through yesterday, long enough to fill the current and the preceding window.

ibm_today <- as.Date("2026-09-17")
ibm_days <- seq(as.Date("2026-07-01"), ibm_today - 1L, by = "day")

ibm_auth <- function(days = ibm_days, successful = 800, failed = 200) {
  tibble::tibble(
    date = days,
    unique_users = 1000,
    successful_logins = successful,
    failed_logins = failed,
    mtd_unique_users = 5000,
    ytd_unique_users = 20000,
    rolling_365d_unique_users = 20000
  )
}

# Two external services and one internal, one application each.
ibm_lookup <- tibble::tribble(
  ~application_name, ~service_name, ~is_internal,
  "app-a",           "Service A",   FALSE,
  "app-b",           "Service B",   FALSE,
  "cl-admin",        "Admin",       TRUE
)

ibm_apps <- function(days = ibm_days, applications = ibm_lookup$application_name) {
  tidyr::expand_grid(date = days, application_name = applications) |>
    dplyr::mutate(
      total_logins = 50,
      unique_users = 40,
      successful_logins = 45,
      failed_logins = 5,
      mtd_unique_users = 400,
      ytd_unique_users = 1000,
      rolling_365d_unique_users = 1000
    )
}

ibm_mfa <- function(days = ibm_days) {
  tidyr::expand_grid(date = days, mfa_type = c("sms_otp", "fido2", "email_otp"),
                     result = c("success", "failure")) |>
    dplyr::mutate(count = 10)
}

# A freshly loaded Sign-In Activity dashboard whose readers return the fixtures.
# The labelled rows are joined here, as labelled_app_logins() joins rp.alias.
ibm_with_data <- function(auth = ibm_auth(), apps = ibm_apps(), mfa = ibm_mfa(),
                          lookup = ibm_lookup) {
  labelled <- dplyr::left_join(apps, lookup, by = "application_name")
  stub(
    load_dashboard("ibm-verify"),
    auth_totals = function(con) auth,
    app_logins = function(con) apps,
    mfa_activity = function(con) mfa,
    labelled_app_logins = function(con) labelled
  )
}

run_ibm_preflight <- function(env, today = ibm_today) {
  run_quietly(env$run_preflight_safety_check(NULL, today = today))
}
