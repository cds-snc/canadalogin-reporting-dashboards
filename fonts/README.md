# Brand typeface

Source Sans 3, the current release of the Source Sans Pro named in the CDS Brand
Guide. Vendored so that neither rendering the dashboard nor reading it needs
network access. The same files ship in the Signal Check repo; keep them in step.

Two formats, because nothing reads both:

| Files | Used by | Purpose |
| --- | --- | --- |
| `*.woff2` (6) | `_fonts.scss` | Document text. Quarto inlines them at render time. |
| `*.ttf` (2) | `R/branding.R` | ggplot figures via ragg. `systemfonts` cannot read WOFF2. |

Keep the two in step. If the typeface is ever changed, both sets have to move
together or the prose and the graphs will disagree.

## Provenance

- **WOFF2**, latin subset, ~16 KB each: `@fontsource/source-sans-3` 5.3.0, via
  `cdn.jsdelivr.net/npm/@fontsource/source-sans-3@5.3.0/files/`.
- **TTF**, weights 400 and 600, ~235 KB each: `fonts.gstatic.com`. `R/branding.R`
  registers this pair with systemfonts under a private family name, mapping 600
  to the bold face, so a figure looks the same wherever it is rendered.

## Licence

SIL Open Font License 1.1. Full text in `LICENSE.txt`.

> Copyright 2010-2024 Adobe (http://www.adobe.com/), with Reserved Font Name
> 'Source'.

The OFL permits redistribution, bundling, and embedding in documents. It
requires that the copyright notice and licence travel with the font, which is
what `LICENSE.txt` is for, and it forbids selling the font on its own.
