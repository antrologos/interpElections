# Plot an interpolated variable on a map

Produces a quick choropleth map of an interpolated variable using the
`tracts_sf` stored in the result object.

## Usage

``` r
# S3 method for class 'interpElections_result'
plot(x, var = NULL, ...)
```

## Arguments

- x:

  An `interpElections_result` object.

- var:

  Character. Name of the variable to plot. Must be one of
  `x$interp_cols`. If NULL, plots the first interpolated variable.

- ...:

  Additional arguments passed to
  [plot.sf()](https://r-spatial.github.io/sf/reference/plot.html).

## Value

Invisibly returns `x`.
