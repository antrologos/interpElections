# Test residuals for spatial autocorrelation

Computes Moran's I test for spatial autocorrelation in calibration
residuals. Significant spatial autocorrelation suggests model
misspecification — the IDW kernel may be missing spatial structure.

## Usage

``` r
plot_residual_autocorrelation(
  result,
  summary_fn = "rmse",
  residual_type = c("raw", "pearson", "deviance"),
  nsim = 999L,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- summary_fn:

  Summary function for collapsing residuals across brackets: `"rmse"`
  (default), `"mean"`, `"mean_abs"`, `"max_abs"`.

- residual_type:

  Residual type: `"raw"`, `"pearson"`, or `"deviance"`. Default:
  `"raw"`.

- nsim:

  Integer. Number of permutations for the test. Default: 999.

- ...:

  Ignored.

## Value

A `ggplot` object (invisibly). Prints a Moran scatterplot and the test
result. Returns NULL with a message if `spdep` is not installed.
