# Plot tract-station connections

Draws lines between tract centroids and their assigned stations, with
width and alpha proportional to weight.

## Usage

``` r
plot_connections(
  result,
  tract = NULL,
  top_k = NULL,
  threshold = 0.01,
  show_all_tracts = TRUE,
  palette = "viridis",
  interactive = FALSE,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- tract:

  Tract ID(s) or index(es). NULL for overview.

- top_k:

  Max connections per tract.

- threshold:

  Min weight for a connection.

- show_all_tracts:

  Logical. Show all tract polygons as background.

- palette:

  Color palette.

- interactive:

  Logical.

- ...:

  Ignored.

## Value

A ggplot object (invisibly).
