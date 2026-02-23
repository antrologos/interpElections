# Leave-one-out pseudo cross-validation

For each polling station (or a random sample), removes it from the
source data, re-optimizes alpha on the remaining stations, and predicts
the removed station's demographics. This is the only form of
out-of-sample validation available.

## Usage

``` r
leave_one_out(result, max_stations = 30L, verbose = TRUE, ...)
```

## Arguments

- result:

  An `interpElections_result` object.

- max_stations:

  Integer. Maximum stations to evaluate. If the municipality has more
  stations, a random sample is used. Default: 30.

- verbose:

  Logical. Print progress? Default: TRUE.

- ...:

  Ignored.

## Value

A data frame with one row per evaluated station, containing prediction
errors. Returns NULL if required data is missing.
