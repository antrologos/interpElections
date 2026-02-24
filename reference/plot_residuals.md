# Plot calibration residual diagnostics

Plot calibration residual diagnostics

## Usage

``` r
plot_residuals(
  result,
  type = c("map", "histogram", "bracket", "scatter"),
  residual_type = c("raw", "pearson", "deviance"),
  summary_fn = "rmse",
  brackets = NULL,
  palette = "RdBu",
  breaks = "quantile",
  n_breaks = 5L,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- type:

  Character. Plot type: `"map"`, `"histogram"`, `"bracket"`, or
  `"scatter"`.

- residual_type:

  Character. `"raw"`, `"pearson"`, or `"deviance"`.

- summary_fn:

  Summary for collapsing across brackets in map mode. One of `"rmse"`
  (default), `"mean_abs"`, `"max_abs"`, `"mean"`, or an integer bracket
  index.

- brackets:

  Integer vector of bracket indices. NULL = all.

- palette:

  Color palette. Default: `"RdBu"`.

- breaks:

  Break method.

- n_breaks:

  Integer.

- ...:

  Ignored.

## Value

A ggplot object (invisibly).
