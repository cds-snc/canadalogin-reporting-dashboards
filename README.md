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

- `dashboards/<name>/` - one folder per dashboard: its `.qmd`, its own `R/` for
  the code only it uses, and its notes.
- `common/` - code shared by every dashboard: the Athena connection, the
  relying-party lookup, branding and colours. `common/setup.R` is the single
  entry point a dashboard sources.
- `_quarto.yml` - the format, theme and execution defaults every dashboard
  inherits. Paths declared there resolve against the repo root; paths declared
  in a dashboard's own front matter resolve against that dashboard's folder.
- `_brand.yml`, `_fonts.scss`, `styles.css`, `fonts/`, `img/`, `_analytics.qmd` -
  shared branding, typography and the GA4 tag.

Render one dashboard with `quarto render dashboards/<name>`.

## Checks

Each dashboard runs its own data-quality checks at render time (see its
`R/preflight.R`). A failed check does not stop the render, and instead flags the
dashboard with a big red banner.
