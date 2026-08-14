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
  the 30-day proxy seam, the relying-party lookup reader, and the preflight.
- `_brand.yml`, `_analytics.qmd`, `img/` - branding and the GA4 tag, carried
  over from Signal Check.
- Relying-party names, operators and the internal flag come from the shared
  `rp.service` and `rp.alias` tables in the data lake, over the same Athena
  connection. This repo keeps no copy of that list.


Because the source filename is fixed, `publish.sh` reuses the same token every
week, so the URL stays stable across re-renders.

