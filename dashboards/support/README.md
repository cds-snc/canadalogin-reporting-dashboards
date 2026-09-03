# CanadaLogin Support

An internal-only dashboard reporting the support CanadaLogin provides, from two
sources:

- **Call centre**, calls from the public to the CanadaLogin support line: weekly
  volume, calls per 100 active users, wait times, and why people called.
- **Partner support** (in development), tickets on the Partner Success and Operations (PSOM) Jira
  board, where departments integrating with CanadaLogin ask for help. 
Definitions come from the
[CanadaLogin Metrics Cookbook](https://cds-snc.github.io/canadalogin-metrics-cookbook/cookbook/call_centre.html)
and its [Jira chapter](https://cds-snc.github.io/canadalogin-metrics-cookbook/cookbook/jira.html).

Render with `quarto render dashboards/support` from the repo root, or
`quarto render support.qmd` from inside this folder. See the top-level README
for the shared layout and conventions this dashboard follows, `R/metrics.R` for
the metric layer, `topic-categories.csv` for the topic-to-category lookup (edit
that rather than the fallback patterns), and `R/preflight.R` for the
data-quality checks that run at render time.
