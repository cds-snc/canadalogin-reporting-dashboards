# CanadaLogin Reporting Readership

An internal-only dashboard of how much attention our own analytics products
get (not CanadaLogin itself), from the meta-analytics GA4 property. It has
three pages:

- **Signal Check**: views each day, how long readers keep each Signal Check
  open over its first 14 days (a drop-off curve per Signal Check), where its
  sessions started (the tagged Slack, Teams and email links, and others), and
  a table of every Signal Check.
- **Dashboards**: weekly views in each dashboard's own colour, drop-off
  curves, where sessions started, and a table.
- **Metrics Cookbook**: weekly views and the most viewed pages.

It reads `google_analytics.meta_page_traffic` and `meta_page_events`, and
`property_traffic` to tell whether the export ran. Pages are always grouped on
`pagepath_redacted`; the raw `pagepath` holds each Signal Check's private
token and is never read.

Render with `quarto render dashboards/readership` from the repo root, or
`quarto render readership.qmd` from inside this folder, after
`aws sso login --profile cl-data-admin`. See `R/metrics.R` for the reads and
measures, and `R/preflight.R` for the data-quality checks.
