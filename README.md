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

## Layout

- `dashboards/<name>/` - one folder per dashboard, and **each is its own Quarto
  project**: its `.qmd`, its `_quarto.yml`, its `.Rprofile`, its own `R/` for the
  code only it uses, and its notes.
- `common/` - code shared by every dashboard: the Athena connection, the
  relying-party lookup, branding and colours. `common/setup.R` is the single
  entry point a dashboard sources.
- `_common.yml` - the format, theme and execution defaults every dashboard
  pulls in through `metadata-files`. Quarto does not inherit config across
  project boundaries, so this file is how dashboards share one instead.
- `_brand.yml`, `_fonts.scss`, `styles.css`, `fonts/`, `img/`, `_analytics.qmd` -
  shared branding, typography and the GA4 tag.

A dashboard renders with its own folder as the working directory, so paths
inside it are plain filenames and the shared layer is `../../`.

Render one dashboard with `quarto render dashboards/<name>`, from either the
repo root or the dashboard folder.

## Adding a dashboard

1. Create `dashboards/<name>/` with the qmd, and give the qmd a filename no
   other dashboard uses - `publish.sh` keys its stable URL on the basename
   alone, so a collision would overwrite another dashboard's live page.
2. Copy `_quarto.yml` and `.Rprofile` from an existing dashboard, changing the
   `render:` entry to the new qmd. The `.Rprofile` is what activates the shared
   renv when the render is invoked from inside the folder.
3. Add a `notes/` folder with the whitelist `.gitignore`, if it needs one.
4. Mint the publishing token by running `publish.sh` on the first rendered
   file, then copy the render workflow in the publishing repo and change
   `QMD_PATH`, `OUTPUT_HTML`, `PUBLISH_TOKEN` and the cron.

## Checks

Each dashboard runs its own data-quality checks at render time (see its
`R/preflight.R`). A failed check does not stop the render, and instead flags the
dashboard with a big red banner.
