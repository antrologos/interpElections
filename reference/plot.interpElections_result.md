# Plot an interpolated variable as a choropleth map

Produces a ggplot2 choropleth map of an interpolated variable. Variables
can be referenced by column name, ballot number, candidate name, or
party abbreviation.

## Usage

``` r
# S3 method for class 'interpElections_result'
plot(
  x,
  variable = NULL,
  type = "pct_tract",
  palette = "RdYlBu",
  breaks = "quantile",
  n_breaks = 5L,
  title = NULL,
  subtitle = NULL,
  legend_title = NULL,
  caption = NULL,
  show_sources = FALSE,
  border_color = "white",
  border_width = 0.05,
  limits = NULL,
  scale_bar = TRUE,
  ...
)

# S3 method for class 'interpElections_result'
autoplot(object, ...)
```

## Arguments

- x:

  An `interpElections_result` object.

- variable:

  Variable to plot. Accepts a column name (e.g., `"CAND_13"`), ballot
  number (numeric, e.g. `13`), candidate name substring (e.g.,
  `"Lula"`), or party abbreviation (e.g., `"PT"`). Multiple values
  produce a faceted comparison. If NULL, auto-selects the first
  candidate variable.

- type:

  Quantity to map: `"pct_tract"` (default), `"absolute"`, `"pct_muni"`,
  `"pct_valid"`, `"pct_eligible"`, `"density"`.

- palette:

  Color palette name. RColorBrewer palettes (e.g., `"RdYlBu"`,
  `"YlGnBu"`, `"Spectral"`) or viridis palettes (e.g., `"viridis"`,
  `"magma"`, `"plasma"`). Default: `"RdYlBu"` (diverging,
  colorblind-friendly).

- breaks:

  Scale breaks: `"quantile"` (default), `"continuous"`, `"jenks"`
  (requires classInt), or a numeric vector of custom break points.

- n_breaks:

  Number of breaks for `"quantile"` or `"jenks"`. Default: 5.

- title:

  Plot title. NULL = auto-generated from dictionary (title-cased
  candidate name with party).

- subtitle:

  Plot subtitle. NULL = auto-generated from municipality/year/quantity
  metadata.

- legend_title:

  Legend title. NULL = auto-generated from quantity type.

- caption:

  Plot caption. NULL = auto-generated source note. Use `""` to suppress.

- show_sources:

  Overlay source points (polling stations) on the map. Requires
  `electoral_sf` in the result (use `keep = "electoral_sf"` when
  interpolating). Default: FALSE.

- border_color:

  Tract border color. Default: `"white"`.

- border_width:

  Tract border width. Default: 0.05.

- limits:

  Bounding box for the map extent as `c(xmin, xmax, ymin, ymax)` in the
  CRS of the data (typically longitude/latitude). NULL (default) shows
  the full extent. Use this to zoom into a region of interest.

- scale_bar:

  Add a scale bar (requires ggspatial). Default: TRUE.

- ...:

  Ignored.

- object:

  An `interpElections_result` object.

## Value

A `ggplot` object (invisibly). Can be further customized with ggplot2
`+` syntax.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- interpolate_election_br(3304557, 2020, 2022)

# By candidate name (default: % of tract votes, quantile breaks)
plot(result, variable = "Lula")

# By ballot number
plot(result, variable = 13)

# Absolute counts with continuous scale
plot(result, variable = "PT", type = "absolute",
     breaks = "continuous")

# Faceted comparison
plot(result, variable = c("Lula", "Bolsonaro"))

# Zoom into a region
plot(result, variable = "Lula",
     limits = c(-43.2, -43.1, -22.95, -22.85))

# Composable with ggplot2
library(ggplot2)
plot(result, variable = "PT") + theme_dark()
} # }
```
