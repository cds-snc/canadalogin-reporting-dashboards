# CanadaLogin Experience Monitoring

An internal-only dashboard for the EDCP Data and Research Team that watches how
people get through CanadaLogin, from Google Analytics 4. It covers two things:

- **Task success rates** for the sign-in and sign-up funnels, the share of users
  who start a flow and reach its defined finishing point, overall and per
  relying party.
- **Help site page views**, the most-read pages in English and French, which is
  a read on what people look up when a flow does not go smoothly.

It is a working feeder, not the source of truth. The canonical task success
numbers live in the monthly task success report, a written Google doc; this
dashboard saves analysts the Google Analytics dig and hands them copy-ready
numbers for writing that report.

Unlike Signal Check, this is a single dashboard at one stable URL, re-rendered
and republished in place on a weekly cadence rather than a series of timestamped
editions.

## Layout

- `experience-monitoring.qmd` - the single dashboard document.
- `R/` - shared code: Athena connection, branding, colours, the funnel metric
  layer and its 28-day window seam, the relying-party lookup reader, and the
  preflight. The help site tab reads `page_traffic` inline in the qmd.
- `_brand.yml`, `_analytics.qmd`, `img/` - branding and the GA4 tag, carried
  over from Signal Check.
- Relying-party names, operators and the internal flag come from the shared
  `rp.service` and `rp.alias` tables in the data lake, over the same Athena
  connection. This repo keeps no copy of that list.

## Checks

The dashboard runs data-quality checks at render time (`R/preflight.R`). A failed
check does not stop the render. It puts a unmissable large red banner at the top of the
dashboard instead, so read the rendered page before publishing it.
