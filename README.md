# CanadaLogin Reporting

A collection of internal-only dashboards for the EDCP Data and Research Team,
built on shared Quarto branding, connection and publishing conventions.

## Dashboards

- <img src="img/accent-expmon.svg" width="16" height="16" alt=""> [Experience Monitoring](dashboards/experience-monitoring/README.md) (expmon) - task success rates and help site page views for CanadaLogin.
- 📶 Are you looking for Signal Check? That's a special case, and you can find them
[here](https://www.github.com/cds-snc/canadalogin-reporting-signal-check).

## Layout

- `dashboards/<name>/` - one Quarto project per dashboard.
  - A `<name>.qmd` for the dashbboard 
  - A `_quarto.yml` for the Quarto config
  - An `.Rprofile` for the R project config
  - An `R/` for the code only it uses
- `common/` - code shared by every dashboard. Use `common/setup.R` to load it.
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
3. Add a `_theme.scss` naming the dashboard's accent colour, mirror the same hex
   into the qmd's setup chunk for the figures, and drop a matching
   `img/accent-<name>.svg` swatch beside the entry in the list above.
4. Publish via the `publish.sh` script in `cds-snc/canadalogin-signal-check-publishing`

## Checks

Each dashboard runs its own data-quality checks at render time (see its
`R/preflight.R`). A failed check does not stop the render, and instead flags the
dashboard with a big red banner.
