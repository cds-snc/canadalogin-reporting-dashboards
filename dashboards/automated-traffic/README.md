# CanadaLogin Automated Traffic

An internal-only dashboard that looks for automated traffic (scripts and bots)
on CanadaLogin. Each page is one detection method:

- **SMS codes**, from `ibm_verify.mfa_activity`: far more SMS codes sent than
  usual, and few of them entered.
- **Idle accounts**, from the IBM Verify event stream: far more new accounts
  than usual, and most never signing in to a partner service that day. A second
  tab has accounts since launch, with idle accounts by week.
- **Error patterns**, from `google_analytics.error_events`: four rare errors
  rising together.

The Overview has a calendar of the last six months, each day shaded by its
strongest method against a typical day. Each method page covers the last three
months. A method fires only when volume is well above a typical day and
a ratio is unlike real use. The thresholds sit at the top of the qmd's setup
chunk.

The dashboard is rendered on Monday mornings, alongside Sign-In Activity, via the
[Signal Check Publishing](https://github.com/cds-snc/canadalogin-signal-check-publishing)
repository to a single URL that does not change week to week. Each render also
writes `traffic-summary.json`, the past week's flagged days, for the Slack post.

Render with `quarto render dashboards/automated-traffic` from the repo root, or
`quarto render automated-traffic.qmd` from inside this folder. See the top-level
README for the shared layout and conventions this dashboard follows,
`R/metrics.R` for the reads and scoring, and `R/preflight.R` for the
data-quality checks that run at render time.
