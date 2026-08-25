# CanadaLogin Experience Monitoring

An internal-only dashboard for the EDCP Data and Research Team that helps us understand
the experience of a CanadaLogin user. It covers:

- **Task success rates** for the sign-in, sign-up, recover-password and migration
  funnels, the share of users who start a flow and reach its defined finishing
  point, overall and per relying party.
- **Help site page views**, the most-read pages in English and French, which is
  a read on what people look up when a flow does not go smoothly.

It is a working feeder, not the source of truth. The canonical task success
numbers live in the monthly task success report, a written Google doc; this
dashboard saves analysts the Google Analytics dig and hands them copy-ready
numbers for writing that report.

The dashboard is rendered on Monday mornings via the
[Signal Check Publishing](https://github.com/cds-snc/canadalogin-signal-check-publishing)
repository to a single URL that does not change week-to-week.

Render with `quarto render dashboards/experience-monitoring` from the repo root,
or `quarto render experience-monitoring.qmd` from inside this folder. See the
top-level README for the shared layout and conventions this dashboard follows,
and `R/preflight.R` for the data-quality checks that run at render time.
