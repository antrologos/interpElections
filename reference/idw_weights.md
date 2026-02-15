# Compute the column-standardized IDW weight matrix

Builds the inverse distance weight matrix from a travel-time matrix and
per-zone decay parameters, then column-standardizes it so each column
sums to 1.

## Usage

``` r
idw_weights(time_matrix, alpha, offset = 1)
```

## Arguments

- time_matrix:

  Numeric matrix \[n x m\]. Raw travel times. Rows = target zones,
  columns = source points.

- alpha:

  Numeric vector of length n. Decay parameters per target zone.

- offset:

  Numeric. Value added to travel times before applying the power
  function, to avoid `0^(-alpha)`. Default: 1.

## Value

Numeric matrix \[n x m\]. Column-standardized inverse distance weights.
Each column sums to 1. Row/column names are preserved from
`time_matrix`.

## Details

The weight for zone i and source point j is: \$\$W\_{ij} = (t\_{ij} +
\text{offset})^{-\alpha_i}\$\$ After computing raw weights, each column
is divided by its sum so that the weights for each source point form a
probability distribution over target zones.

## See also

[`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md)
to apply these weights.

Other IDW core:
[`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md),
[`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md),
[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md),
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)

## Examples

``` r
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2) # 2 zones x 3 sources
alpha <- c(1, 1.5)
W <- idw_weights(tt, alpha, offset = 1)
colSums(W) # each column sums to 1
#> [1] 1 1 1
```
