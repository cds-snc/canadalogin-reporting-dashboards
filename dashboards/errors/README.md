# CanadaLogin Errors

An internal-only dashboard that shows how often CanadaLogin users run into
errors, and whether any error is happening more than usual. It answers
questions like "are we seeing more passkey errors than normal?" without
needing the data team.

It reads Google Analytics only: errors from `google_analytics.error_events`,
and sessions from `google_analytics.property_traffic`. Every rate is errors
per 1,000 site-wide sessions.

After an About page, the dashboard has two pages, each a table under three
value boxes:

- **By Feature**: one row per part of CanadaLogin where errors happen, such as
  passkey setup or sign in, whichever journey it was part of. GA's `form_id`
  decides the feature, through `feature_of_form` in `R/metrics.R`. A form not in
  that map still gets its own row, named by its `form_id`.
- **Error codes**: flagged, new and returning codes, then the busiest codes of
  the week.

Codes are shown raw. Hovering over one shows its message, read at render time
from a private catalogue in the publishing repo: the newest text users see,
else IBM Verify's own text, marked as IBM's. Without that file, codes have no
tooltip.

A feature or code is flagged when its rate this week is at least twice the
median of the four weeks before, on at least 50 errors. A code with no errors
in those four weeks is new, or returning if it had some before them. Each row
has a 12-week trend line. The thresholds sit at the top of the qmd's setup
chunk.

The dashboard is rendered on Wednesday mornings and reports the Monday-to-Sunday
week before, via the
[Signal Check Publishing](https://github.com/cds-snc/canadalogin-signal-check-publishing)
repository to a single URL that does not change week to week.

Render with `quarto render dashboards/errors` from the repo root, or
`quarto render errors.qmd` from inside this folder. See the top-level README
for the shared layout and conventions this dashboard follows, `R/metrics.R`
for the reads and the flag, and `R/preflight.R` for the data-quality checks
that run at render time.
