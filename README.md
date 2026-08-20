# CanadaLogin Experience Monitoring

An internal-only dashboard for the EDCP Data and Research Team that helps us understand
the experience of a CanadaLogin user. It covers:

- **Task success rates** for the sign-in and sign-up funnels, the share of users
  who start a flow and reach its defined finishing point, overall and per
  relying party.
- **Help site page views**, the most-read pages in English and French, which is
  a read on what people look up when a flow does not go smoothly.

It is a working feeder, not the source of truth. The canonical task success
numbers live in the monthly task success report, a written Google doc; this
dashboard saves analysts the Google Analytics dig and hands them copy-ready
numbers for writing that report.

The dashboard is rendered on Monday mornings via the
[Signal Check Publishing](https://github.com/cds-snc/canadalogin-signal-check-publishing)
repository to a single URL that does not change week-to-week.

## Layout

- `experience-monitoring.qmd` - the single dashboard document.
- `R/` - shared code: Athena connection, branding, colours, the funnel metric
  layer.
- `_brand.yml`, `_analytics.qmd`, `img/` - branding and the GA4 tag.

## Checks

The dashboard runs data-quality checks at render time (`R/preflight.R`). A failed
check does not stop the render, and instead flags the dashboard with a big red banner.
