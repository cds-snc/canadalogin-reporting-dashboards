# CanadaLogin Task Success Monitoring

An internal-only dashboard for the EDCP Data and Research Team that monitors
CanadaLogin task success rates: the share of users who start a flow (sign in,
sign up) and reach its defined finishing point, measured from Google Analytics 4
funnel data. It is a working feeder, not the source of truth. The canonical task
success numbers live in the monthly task success report, a written Google doc;
this dashboard saves analysts the Google Analytics dig and hands them
copy-ready numbers for writing that report.

Unlike Signal Check, this is a single dashboard at one stable URL, re-rendered
and republished in place on a weekly cadence rather than a series of timestamped
editions.

## Layout

- `task-success-monitoring.qmd` - the single dashboard document.
- `R/` - shared code: Athena connection, branding, colours, the metric layer,
  the 30-day proxy seam, the relying-party registry reader, and the preflight.
- `_brand.yml`, `_analytics.qmd`, `img/` - branding and the GA4 tag, carried
  over from Signal Check.
- The relying-party registry is read cross-repo from the sibling
  `../canadalogin-signal-check/data/relying_parties.csv`; do not fork a copy.

## Weekly render and publish (manual)

1. `aws sso login --profile cl-data-admin`
2. `quarto render task-success-monitoring.qmd`
   (this runs the preflight first and stops the render on any data problem;
   `_quarto.yml` writes the HTML into the sibling publishing repo's `reports/`)
3. In `../canadalogin-signal-check-publishing`,
   `./publish.sh reports/task-success-monitoring.html`

Because the source filename is fixed, `publish.sh` reuses the same token every
week, so the URL stays stable across re-renders.

## Review gate

- A routine re-render (same code, new data) may publish directly; the preflight
  guards the data.
- Any code change (queries, metrics, layout, the funnel allowlist, the 30-day
  seam) goes through a pull request in this repo first.
