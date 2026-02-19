# Interactive map of interpolated results

Creates an interactive leaflet/mapview map of an interpolated variable.
Variables can be referenced by column name, ballot number, candidate
name, or party abbreviation. When multiple variables are provided,
creates synchronized side-by-side panels via
[`leafsync::sync()`](https://rdrr.io/pkg/leafsync/man/latticeView.html).

## Usage

``` r
plot_interactive(
  result,
  variable = NULL,
  type = "pct_tract",
  palette = "RdYlBu",
  breaks = "quantile",
  n_breaks = 5L,
  popup_vars = NULL,
  alpha = 0.7,
  legend = TRUE,
  layer_name = NULL,
  basemap = "OpenStreetMap",
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- variable:

  Variable to map. Accepts a column name, ballot number (numeric),
  candidate name substring, or party abbreviation. Multiple values
  create synchronized comparison panels. If NULL, auto-selects the first
  candidate variable.

- type:

  Quantity to map: `"pct_tract"` (default), `"absolute"`, `"pct_muni"`,
  `"pct_valid"`, `"pct_eligible"`, `"density"`.

- palette:

  Color palette name. Default: `"RdYlBu"` (diverging,
  colorblind-friendly).

- breaks:

  Scale breaks: `"quantile"` (default), `"continuous"`, `"jenks"`
  (requires classInt), or a numeric vector of custom break points.

- n_breaks:

  Number of breaks for `"quantile"` or `"jenks"`. Default: 5.

- popup_vars:

  Character vector of column names to show in click popups. If NULL,
  auto-selects census tract ID, the plotted variable, turnout, and
  calibration columns (capped at 8).

- alpha:

  Fill opacity (0 to 1). Default: 0.7.

- legend:

  Show legend. Default: TRUE.

- layer_name:

  Layer name in the map. NULL = auto-generated from dictionary metadata.

- basemap:

  Base map tile provider. Default: `"OpenStreetMap"`.

- ...:

  Additional arguments passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).

## Value

A `mapview` object (single variable) or a `leafsync` object (multiple
variables with synchronized panels).

## Examples

``` r
if (FALSE) { # \dontrun{
result <- interpolate_election_br(3304557, 2020, 2022,
                                   keep = "sources_sf")

# Interactive map by candidate name
plot_interactive(result, variable = "Lula", type = "pct_tract")

# Side-by-side comparison (synced panels)
plot_interactive(result, variable = c("Lula", "Bolsonaro"),
                 type = "pct_tract")

# Custom popup
plot_interactive(result, variable = "PT",
                 popup_vars = c("code_tract", "PARTY_PT",
                                "QT_COMPARECIMENTO"))
} # }
```
