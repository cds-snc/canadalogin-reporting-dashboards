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
  # Flagged codes lead the table.
  expect_identical(codes$error_code[1], "CSIBN0021E")
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
                   form_id = "add_passkey", events = 1)
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

test_that("the codes table lists flagged and new codes, then the busiest", {
  quiet <- purrr::map_dfr(sprintf("Q%02d", 1:12), \(code) {
    errors_code(code, "signin", 1)
  })
  codes <- errors_codes(env, dplyr::bind_rows(errors_rows(), quiet))
  expect_identical(nrow(codes), errors_thresholds$top_codes)
  expect_identical(codes$error_code[1:3], c("26", "CSIAH3315E", "CSIAQ0287E"))
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
  expect_match(row$codes, "<code>26</code> 100%", fixed = TRUE)
  expect_match(row$codes, "<code>PHONE_VALIDATION_ERROR</code> <1%",
               fixed = TRUE)
})

test_that("the overall rate is every GA error over sessions", {
  overall <- env$overall_rate(errors_rows(), errors_sessions(), errors_week,
                              errors_thresholds)
  expect_equal(overall$rate, 170)
  expect_equal(overall$change, 0)
})

# Formatting ------------------------------------------------------------------

test_that("small shares read as under one per cent", {
  expect_identical(env$fmt_share(c(0.004, 0.88)), c("<1%", "88%"))
})

# Messages --------------------------------------------------------------------

errors_catalogue <- list(codes = list(
  "6" = list(meaning = "Password is the example password", shown = list(
    list(source = "api", en = "Invalid password.", fr = "Mot de passe invalide."),
    list(source = "gadrift", en = "006: Invalid password, captured.",
         captured = "2026-09-28")
  )),
  CSIBN0021E = list(shown = list(
    list(en = "Invalid code."),
    list(en = "Code is invalid.", captured = "2026-09-20"),
    list(en = "Invalid code. Try again.", captured = "2026-10-23")
  )),
  "205" = list(shown = list(list(en = "Request invalid."),
                            list(en = "Something else."))),
  CSIAQ0287E = list(meaning = "Sign-in idle for too long",
                    ibm_reference = "The transaction has been idle for too long."),
  CSIAH0001E = list(notes = "Nothing users see"),
  FID024E = list(ibm_reference = "")
))

message_of <- function(code) {
  messages <- env$code_messages(errors_catalogue)
  messages$message[messages$error_code == code]
}

test_that("a code's message is its newest capture, without the id", {
  expect_identical(message_of("CSIBN0021E"), "Invalid code. Try again.")
  expect_identical(message_of("6"), "Invalid password, captured.")
})

test_that("with no capture, the first place the software defines it wins", {
  expect_identical(message_of("205"), "Request invalid.")
})

test_that("with no text users see, IBM's text is used and marked", {
  expect_identical(message_of("CSIAQ0287E"),
                   "IBM Verify: The transaction has been idle for too long.")
})

test_that("with nothing at all, the code says it has no message", {
  expect_identical(message_of("CSIAH0001E"), "(No error message on record)")
  # A blank IBM text is no text.
  expect_identical(message_of("FID024E"), "(No error message on record)")
})

test_that("no catalogue means no messages, not an error", {
  expect_identical(nrow(env$code_messages(NULL)), 0L)
})

test_that("a code with a message gets an escaped tooltip", {
  messages <- tibble::tibble(error_code = "X1", message = "Use <b> & \"this\"\nnext")
  html <- env$code_html(c("X1", "X2"), messages)
  expect_match(html[1], 'class="hover-tip"', fixed = TRUE)
  expect_match(html[1], "&lt;b&gt; &amp; &quot;this&quot;&#10;next", fixed = TRUE)
  # A code the catalogue does not know says so.
  expect_match(html[2], 'title="(No error message on record)"', fixed = TRUE)
})

test_that("without a catalogue, codes have no tooltip", {
  expect_identical(env$code_html("X2"), "<code>X2</code>")
})

test_that("feature top codes carry the tooltips", {
  messages <- tibble::tibble(error_code = "26", message = "No account.")
  features <- env$feature_table(errors_rows(), errors_sessions(), errors_week,
                                errors_thresholds, messages)
  row <- dplyr::filter(features, feature == "Password recovery")
  expect_match(row$codes, 'title="No account."', fixed = TRUE)
})

# Value boxes -----------------------------------------------------------------

test_that("codes seen this week compare with the median of the weeks before", {
  errors <- dplyr::bind_rows(
    errors_rows(),
    tibble::tibble(day = as.Date("2026-10-20"), error_code = "ONCE",
                   form_id = "signin", events = 1)
  )
  active <- env$active_codes(errors, errors_week, errors_thresholds)
  expect_identical(active$now, 3L)
  expect_equal(active$typical, 3)
})

test_that("the busiest feature has the most errors, with its share", {
  busiest <- env$busiest_feature(errors_features(env))
  expect_identical(busiest$feature, "Password recovery")
  expect_equal(busiest$share, 100 / 170)
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
