# Plot weight matrix diagnostics

Produces choropleth maps of weight matrix properties: entropy (effective
number of sources), dominant station assignment, or a catchment view
showing weight distribution for a specific tract.

## Usage

``` r
plot_weights(
  result,
  tract = NULL,
  type = c("catchment", "dominant", "entropy"),
  top_k = 5L,
  threshold = 0.01,
  palette = "YlOrRd",
  breaks = "quantile",
  n_breaks = 5L,
  interactive = FALSE,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- tract:

  Tract ID or index for catchment mode. NULL for overview.

- type:

  Character. `"catchment"`, `"dominant"`, or `"entropy"`.

- top_k:

  Integer. Max stations to show in catchment.

- threshold:

  Numeric. Min weight to show a connection.

- palette:

  Color palette.

- breaks:

  Break method.

- n_breaks:

  Number of breaks.

- interactive:

  Logical. Use leaflet instead of ggplot2.

- ...:

  Ignored.

## Value

A ggplot object (invisibly).
