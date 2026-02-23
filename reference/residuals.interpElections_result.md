# Compute calibration residuals

Returns the matrix of calibration residuals (fitted minus observed) for
each census tract and calibration bracket. Both the weight matrix and
travel time matrix are kept by default.

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
the number of calibration brackets. Returns `NULL` (with a message) if
the required data is not available.
