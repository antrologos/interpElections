# Compute calibration residuals

Returns the matrix of calibration residuals (fitted minus observed) for
each zone and calibration bracket. Requires the weight matrix or travel
time matrix to be present in the result (use `keep = "weights"` or
`keep = "time_matrix"` when running the interpolation).

## Usage

``` r
# S3 method for class 'interpElections_result'
residuals(object, ...)
```

## Arguments

- object:

  An `interpElections_result` object.

- ...:

  Ignored.

## Value

Numeric matrix \[n x k\] of residuals (fitted - observed), where k is
the number of calibration brackets.
