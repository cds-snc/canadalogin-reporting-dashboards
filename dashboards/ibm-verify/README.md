# CanadaLogin Sign-In Activity

An internal-only dashboard reporting sign-in activity on CanadaLogin. It replaces the 
Power BI report "CanadaLogin Report - IBM Verify data". It covers:

- **Overall activity**, users authenticating with CanadaLogin
- **Services**, users *using* CanadaLogin to authenticate against a relying party
- **Second factors**, weekly successful attempts per factor, passkey uptake as a
  share of them, and the factor mix over the summary window.

Counting in this data set is sometimes complicated. Look up a definition in the
[CanadaLogin Metrics Cookbook](https://cds-snc.github.io/canadalogin-metrics-cookbook/cookbook/ibm_verify.html)
instead of assuming the definition is obvious.

The dashboard is rendered on Monday mornings via the
[Signal Check Publishing](https://github.com/cds-snc/canadalogin-signal-check-publishing)
repository to a single URL that does not change week-to-week.

Render with `quarto render dashboards/ibm-verify` from the repo root, or
`quarto render ibm-verify.qmd` from inside this folder. See the top-level README
for the shared layout and conventions this dashboard follows, `R/metrics.R` for
the metric layer, and `R/preflight.R` for the data-quality checks that run at
render time.
