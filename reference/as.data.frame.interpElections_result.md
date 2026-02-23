# Convert result to data frame

Drops geometry from `tracts_sf` and returns a plain data frame with
census tract IDs and interpolated values.

## Usage

``` r
# S3 method for class 'interpElections_result'
as.data.frame(x, ...)
```

## Arguments

- x:

  An `interpElections_result` object.

- ...:

  Ignored.

## Value

A data frame.
