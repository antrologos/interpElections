# Moran's I and LISA cluster map for an interpolated variable

Computes global Moran's I and optionally maps Local Indicators of
Spatial Association (LISA) clusters for an interpolated variable.

## Usage

``` r
plot_moran(
  result,
  variable = NULL,
  type = c("lisa", "moran"),
  significance = 0.05,
  nsim = 999L,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- variable:

  Variable to analyze. Accepts column name, ballot number, candidate
  name, or party abbreviation.

- type:

  Character. `"moran"` for the Moran scatterplot, or `"lisa"` for the
  LISA cluster map. Default: `"lisa"`.

- significance:

  Significance level for LISA. Default: 0.05.

- nsim:

  Number of permutations. Default: 999.

- ...:

  Ignored.

## Value

A `ggplot` object (invisibly).
