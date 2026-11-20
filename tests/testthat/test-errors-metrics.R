env <- load_dashboard("errors")

# Weeks -----------------------------------------------------------------------

test_that("the report week is the last Monday-to-Sunday week GA has finished", {
  expect_identical(env$report_week(errors_today), errors_week)
  # A Monday render sees only Saturday, so the week before.
  expect_identical(env$report_week(as.Date("2026-11-16"))$start,
                   as.Date("2026-11-02"))
})

test_that("the weeks needed end with the report week", {
  weeks <- env$weeks_needed(errors_week, errors_thresholds)
  expect_length(weeks, 16L)
  expect_identical(max(weeks), errors_week$start)
  expect_true(all(diff(weeks) == 7))
})

test_that("a week with no events reads as zero, not missing", {
  errors <- dplyr::filter(errors_rows(),
                          !(error_code == "CSIAQ0287E" &
                              day >= as.Date("2026-10-19") &
                              day <= as.Date("2026-10-25")))
  codes <- errors_codes(env, errors)
  trend <- codes$trend[[which(codes$error_code == "CSIAQ0287E")]]
  expect_length(trend, 12L)
  expect_identical(sum(trend == 0), 1L)
})

# Flags -----------------------------------------------------------------------

test_that("a steady code is not flagged and shows no change", {
  codes <- errors_codes(env)
  expect_false(any(codes$flagged))
  expect_false(any(codes$new))
  expect_equal(codes$change, rep(0, 3))
})

test_that("a code at twice its usual rate on enough events is flagged", {
  errors <- dplyr::bind_rows(errors_rows(),
                             errors_code("CSIBN0021E", "signup", 5, 10))
  codes <- errors_codes(env, errors)
  row <- dplyr::filter(codes, error_code == "CSIBN0021E")
  expect_true(row$flagged)
  expect_identical(row$events, 70)
  expect_equal(row$change, 1)
  # Listed by volume, not ahead of busier codes.
  expect_identical(match("CSIBN0021E", codes$error_code), 4L)
})

test_that("a doubling on too few events is not flagged", {
  errors <- dplyr::bind_rows(errors_rows(),
                             errors_code("CSIBN0021E", "signup", 1, 7))
  row <- dplyr::filter(errors_codes(env, errors), error_code == "CSIBN0021E")
  expect_identical(row$events, 49)
  expect_false(row$flagged)
})

test_that("less than twice the usual rate is not flagged", {
  errors <- dplyr::bind_rows(errors_rows(),
                             errors_code("CSIBN0021E", "signup", 10, 19))
  row <- dplyr::filter(errors_codes(env, errors), error_code == "CSIBN0021E")
  expect_false(row$flagged)
})

test_that("a code with no errors in the history weeks is new, not flagged", {
  days <- errors_days()
  errors <- dplyr::bind_rows(
    errors_rows(),
    errors_code("CSIAH0001E", "signin", 0, 100, days = days) |>
      dplyr::filter(events > 0)
  )
  codes <- errors_codes(env, errors)
  row <- dplyr::filter(codes, error_code == "CSIAH0001E")
  expect_true(row$new)
  expect_false(row$returning)
  expect_false(row$flagged)
  expect_identical(env$fmt_against(row$change, row$new, row$returning, row$typical,
                                   row$events), "New")
})

test_that("a code quiet for the history weeks but seen before is returning", {
  errors <- dplyr::bind_rows(
    errors_rows(),
    tibble::tibble(day = as.Date("2026-09-01"), error_code = "FID024E",
                   form_id = "add_passkey", events = 2),
    tibble::tibble(day = errors_week$start, error_code = "FID024E",
                   form_id = "add_passkey", events = 5)
  )
  row <- dplyr::filter(errors_codes(env, errors), error_code == "FID024E")
  expect_true(row$new)
  expect_true(row$returning)
  expect_identical(env$fmt_against(row$change, row$new, row$returning,
                                   row$typical, row$events), "Returning")
})

test_that("a typical week of zero, with some history, flags from zero", {
  # One week of errors in the history, then a busy report week.
  errors <- dplyr::bind_rows(
    errors_rows(),
    tibble::tibble(day = as.Date("2026-10-13"), error_code = "CSIAH0001E",
                   form_id = "signin", events = 3),
    tibble::tibble(day = errors_week$start, error_code = "CSIAH0001E",
                   form_id = "signin", events = 60)
  )
  row <- dplyr::filter(errors_codes(env, errors), error_code == "CSIAH0001E")
  expect_false(row$new)
  expect_true(row$flagged)
  expect_identical(env$fmt_against(row$change, row$new, row$returning, row$typical,
                                   row$events), "▲ from 0")
})

test_that("the codes table lists new codes, then the rest from a minimum, by volume", {
  errors <- dplyr::bind_rows(
    errors_rows(),
    errors_code("QUIET", "signin", 2),
    tibble::tibble(day = errors_week$start, error_code = "BRAND_NEW",
                   form_id = "signin", events = 6)
  )
  codes <- errors_codes(env, errors)
  # QUIET has 14 a week, under the minimum of 20.
  expect_identical(codes$error_code,
                   c("BRAND_NEW", "26", "CSIAH3315E", "CSIAQ0287E"))
})

# Features --------------------------------------------------------------------

test_that("forms map to features, and passkey setup spans its forms", {
  errors <- dplyr::bind_rows(errors_rows(),
                             errors_code("CSIAH3315E", "add_passkey", 10))
  features <- errors_features(env, errors)
  passkey <- dplyr::filter(features, feature == "Passkey setup")
  expect_identical(passkey$events, 7 * 60)
  expect_setequal(features$feature,
                  c("Password recovery", "Passkey setup", "Sign in"))
})

test_that("an unlabelled form keeps its own row, named by its form_id", {
  errors <- dplyr::bind_rows(errors_rows(),
                             errors_code("NEW_CODE", "brand_new_form", 4))
  features <- errors_features(env, errors)
  expect_true("brand_new_form" %in% features$feature)
  expect_identical(env$unlabelled_forms(errors), "brand_new_form")
})

test_that("a feature's top codes carry their share of its errors", {
  errors <- dplyr::bind_rows(errors_rows(),
                             errors_code("PHONE_VALIDATION_ERROR",
                                         "passwordrecovery", 0.1))
  row <- dplyr::filter(errors_features(env, errors),
                       feature == "Password recovery")
  expect_match(row$codes,
               paste0('<div class="code-share-bar"><div role=\'img\' ',
                      "aria-label='100% of errors'"),
               fixed = TRUE)
  expect_match(row$codes, "aria-label='&lt;1% of errors'", fixed = TRUE)
  # Busiest first.
  expect_lt(regexpr("<code>26</code>", row$codes),
            regexpr("<code>PHONE_VALIDATION_ERROR</code>", row$codes))
})

test_that("the overall rate is every GA error over sessions", {
  overall <- env$overall_rate(errors_rows(), errors_sessions(), errors_week,
                              errors_thresholds)
  expect_equal(overall$rate, 170)
  expect_equal(overall$change, 0)
})

# Formatting ------------------------------------------------------------------

test_that("rates show three significant figures", {
  expect_identical(env$fmt_rate(c(170.886, 59.757, 1.5674, 0.0392, 2, NA)),
                   c("171", "59.8", "1.57", "0.0392", "2.00", "-"))
})

test_that("small shares read as under one per cent", {
  expect_identical(env$fmt_share(c(0.004, 0.88)), c("<1%", "88%"))
})

# Messages --------------------------------------------------------------------

errors_catalogue <- list(
  "CLS-0001" = list(
    en = "Invalid password.", fr = "Mot de passe invalide.",
    aliases = list(api = list("006"), gcs = list("GCSP-0002"))
  ),
  "CLS-0002" = list(
    en = "Server error.", fr = "Erreur de serveur.",
    aliases = list(api = list("000", "010"))
  ),
  "CLM-0001" = list(
    en = "Invalid code.", fr = "Code invalide.",
    aliases = list(ibm = list("CSIBN0021E"), webapp = list("otp_invalid"))
  ),
  # The same code under a second entry.
  "CLM-0002" = list(
    en = "Code is invalid.", fr = "Le code est invalide.",
    aliases = list(webapp = list("otp_invalid"))
  ),
  "CLP-0001" = list(en = "No aliases yet.", fr = "Aucun alias.")
)

message_of <- function(code) {
  messages <- env$code_messages(errors_catalogue)
  messages$message[messages$error_code == code]
}

test_that("a code's message is its entry's English text, without the id", {
  expect_identical(message_of("GCSP-0002"), "Invalid password.")
})

test_that("every alias of an entry gets its message", {
  expect_identical(message_of("10"), "Server error.")
  expect_identical(message_of("CSIBN0021E"), "Invalid code.")
})

test_that("API ids match GA's form, without leading zeros", {
  expect_identical(message_of("6"), "Invalid password.")
  expect_identical(message_of("0"), "Server error.")
  expect_identical(message_of("006"), character())
})

test_that("only API ids lose leading zeros", {
  catalogue <- list("CLS-0001" = list(en = "A.", aliases = list(gcs = list("007"))))
  expect_identical(env$code_messages(catalogue)$error_code, "007")
})

test_that("a code under two entries takes the first", {
  expect_identical(message_of("otp_invalid"), "Invalid code.")
})

test_that("an entry with no aliases adds no codes", {
  messages <- env$code_messages(errors_catalogue)
  expect_false("No aliases yet." %in% messages$message)
})

test_that("an unquoted API id read as a number still matches", {
  catalogue <- list("CLS-0001" = list(en = "A.", aliases = list(api = list(6L))))
  expect_identical(env$code_messages(catalogue)$error_code, "6")
})

test_that("an empty catalogue gives no messages, but is still a catalogue", {
  messages <- env$code_messages(stats::setNames(list(), character()))
  expect_identical(nrow(messages), 0L)
  expect_identical(names(messages), c("error_code", "message"))
})

test_that("an IBM code the catalogue lacks shows IBM Verify's text, marked", {
  ibm <- list(CSIAQ0287E = "Idle for too long.", CSIBN0021E = "IBM's own words.")
  messages <- env$code_messages(errors_catalogue, ibm = ibm)
  expect_identical(messages$message[messages$error_code == "CSIAQ0287E"],
                   "(IBM Verify) Idle for too long.")
  # The catalogue's text, what users see, wins.
  expect_identical(messages$message[messages$error_code == "CSIBN0021E"],
                   "Invalid code.")
})

test_that("no IBM text leaves the catalogue's messages as they are", {
  expect_identical(env$code_messages(errors_catalogue, ibm = NULL),
                   env$code_messages(errors_catalogue))
})

test_that("no catalogue means no messages, not an error", {
  expect_null(env$code_messages(NULL))
})

test_that("a code with a message gets an escaped tooltip", {
  messages <- tibble::tibble(error_code = "X1", message = "Use <b> & \"this\"\nnext")
  html <- env$code_html(c("X1", "X2"), messages)
  expect_match(html[1], 'class="hover-tip"', fixed = TRUE)
  expect_match(html[1], "&lt;b&gt; &amp; &quot;this&quot;&#10;next", fixed = TRUE)
  # A code the catalogue does not know says so.
  expect_match(html[2], 'title="(No error message on record)"', fixed = TRUE)
})

test_that("with an empty catalogue, every code says it has no message", {
  html <- env$code_html("X2", env$code_messages(stats::setNames(list(), character())))
  expect_match(html, 'title="(No error message on record)"', fixed = TRUE)
})

test_that("without a catalogue, codes have no tooltip", {
  expect_identical(env$code_html("X2"), "<code>X2</code>")
  expect_identical(env$code_html(c("X1", "X2"), NULL),
                   c("<code>X1</code>", "<code>X2</code>"))
})

test_that("feature top codes carry the tooltips", {
  messages <- tibble::tibble(error_code = "26", message = "No account.")
  features <- env$feature_table(errors_rows(), errors_sessions(), errors_week,
                                errors_thresholds, "red", messages)
  row <- dplyr::filter(features, feature == "Password recovery")
  expect_match(row$codes, 'title="No account."', fixed = TRUE)
})

test_that("a feature's tooltip lists every form the map gives it", {
  html <- env$feature_html(c("Passkey setup", "brand_new_form"))
  expect_match(html[1], 'title="fido2_enrollment_form&#10;add_passkey"',
               fixed = TRUE)
  expect_match(html[2], "(No feature name yet: shown by its form ID)", fixed = TRUE)
  expect_match(html[2], ">brand_new_form</span>", fixed = TRUE)
})

test_that("for errors a rise is bad, a fall good, and a new code has no verdict", {
  expect_identical(
    env$error_verdict(change = c(0.5, -0.5, 0.001, NA, NA),
                      new = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                      typical = c(10, 10, 10, 0, 0), events = c(15, 5, 10, 60, 3)),
    c("bad", "good", "flat", "bad", NA)
  )
})

test_that("a new code below the event minimum is left out of the table", {
  errors <- dplyr::bind_rows(
    errors_rows(),
    tibble::tibble(day = errors_week$start, error_code = c("RARE", "ENOUGH"),
                   form_id = "signin", events = c(4, 5))
  )
  codes <- errors_codes(env, errors)
  expect_false("RARE" %in% codes$error_code)
  expect_true("ENOUGH" %in% codes$error_code)
})

test_that("the charts start at the first whole week of data, up to a limit", {
  # Data from a Friday: the chart starts the Monday after.
  span <- env$chart_span(errors_week, errors_thresholds, as.Date("2026-07-10"))
  expect_identical(span[1], as.Date("2026-07-13"))
  expect_identical(max(span), errors_week$start)
  # Data from a Monday starts that Monday.
  expect_identical(
    env$chart_span(errors_week, errors_thresholds, as.Date("2026-07-13"))[1],
    as.Date("2026-07-13")
  )
  # Plenty of data: capped at chart_weeks.
  long <- env$chart_span(errors_week, errors_thresholds, as.Date("2025-01-01"))
  expect_length(long, errors_thresholds$chart_weeks)
})
