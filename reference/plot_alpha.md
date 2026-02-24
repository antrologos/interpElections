# Plot alpha decay parameter diagnostics

Visualize the spatial distribution and bracket variation of optimized
alpha decay parameters.

## Usage

``` r
plot_alpha(
  result,
  type = c("map", "histogram", "bracket"),
  summary_fn = "median",
  brackets = NULL,
  palette = "YlOrRd",
  breaks = "quantile",
  n_breaks = 5L,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- type:

  Character. Plot type: `"map"`, `"histogram"`, or `"bracket"`.

- summary_fn:

  Summary function for collapsing alpha across brackets when
  `type = "map"`. One of `"median"` (default), `"mean"`,
  `"pop_weighted"`, `"min"`, `"max"`, `"range"`, or an integer bracket
  index.

- brackets:

  Integer vector. Bracket indices to include. NULL = all.

- palette:

  Character. Color palette. Default: `"YlOrRd"`.

- breaks:

  Break method: `"quantile"`, `"continuous"`, `"jenks"`, or numeric
  vector.

- n_breaks:

  Integer. Number of breaks. Default: 5.

- ...:

  Ignored.

## Value

A ggplot object (invisibly). Prints the plot.
