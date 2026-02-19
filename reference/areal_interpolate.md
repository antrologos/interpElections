# Aggregate data from source zones to target polygons using areal weights

Applies a precomputed areal weight matrix (from
[`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md))
to transfer data from source zones to target polygons.

## Usage

``` r
areal_interpolate(data, weights)
```

## Arguments

- data:

  Numeric matrix or data.frame \[n_source x p\]. Data from source zones.
  Rows must correspond to the columns of `weights`.

- weights:

  Numeric matrix \[n_target x n_source\]. Output of
  [`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md).

## Value

Numeric matrix \[n_target x p\]. Aggregated values in target polygons.

## See also

Other spatial:
[`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md),
[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md),
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)

## Examples

``` r
W <- matrix(c(0.7, 0.3, 0.2, 0.8), nrow = 2)  # 2 targets x 2 sources
src_data <- matrix(c(100, 200), nrow = 2)        # 2 sources x 1 variable
areal_interpolate(src_data, W)
#>      [,1]
#> [1,]  110
#> [2,]  190
```
