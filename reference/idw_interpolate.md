# Interpolate source data into target zones using IDW weights

Applies inverse-distance-weighted interpolation to transfer data from
geolocated source points (e.g., polling locations) into target zones
(e.g., census tracts), using optimized decay parameters.

## Usage

``` r
idw_interpolate(time_matrix, alpha, source_matrix, offset = 1)
```

## Arguments

- time_matrix:

  Numeric matrix \[n x m\]. Raw travel times. Rows = target zones,
  columns = source points.

- alpha:

  Numeric vector of length n. Optimal decay parameters, typically from
  [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md).

- source_matrix:

  Numeric matrix or data.frame \[m x p\]. Data at source points to be
  interpolated. Each column is a variable (e.g., candidate votes,
  attendance counts).

- offset:

  Numeric. Travel time offset. Default: 1.

## Value

Numeric matrix \[n x p\]. Interpolated values at target zones. Column
names are preserved from `source_matrix`.

## Details

For each target zone i and variable v: \$\$\hat{v}\_i = \sum_j
W\_{ij}^{std} \cdot v_j\$\$

where \\W^{std}\\ is the column-standardized weight matrix from
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md).

This function is the main workhorse for applying the interpolation after
optimal alpha values have been found via
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md).

## See also

[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
to find optimal alpha values,
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md)
to inspect the weight matrix.

Other IDW core:
[`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md),
[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md),
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md),
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)

## Examples

``` r
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2) # 2 zones x 3 sources
alpha <- c(1, 1.5)
src <- matrix(c(80, 120, 100), nrow = 3)      # 3 sources x 1 variable
idw_interpolate(tt, alpha, src)
#>          [,1]
#> [1,] 197.4233
#> [2,] 102.5767
```
