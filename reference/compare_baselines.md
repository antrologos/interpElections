# Compare interpolation against baseline methods

Recomputes calibration residuals under naive weight matrices and reports
a comparison table showing the value added by optimization.

## Usage

``` r
compare_baselines(result, methods = c("nearest", "uniform", "areal"), ...)
```

## Arguments

- result:

  An `interpElections_result` object.

- methods:

  Character vector. Baseline methods to compare: `"nearest"` (assign to
  nearest station), `"uniform"` (equal weights), `"areal"` (area-based
  interpolation). Default: all three.

- ...:

  Ignored.

## Value

A data frame with columns: method, rmse, poisson_deviance,
relative_improvement. Prints the table. Returns invisibly.
