# Compute the IDW interpolation objective value

Calculates the sum of squared errors between the IDW-interpolated values
and the known population matrix. This is the loss function that
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
minimizes.

## Usage

``` r
idw_objective(alpha, time_matrix, pop_matrix, source_matrix)
```

## Arguments

- alpha:

  Numeric vector of length n. Decay parameters per target zone. Must be
  non-negative.

- time_matrix:

  Numeric matrix \[n x m\]. Adjusted travel times (with offset already
  applied). Rows = target zones, columns = source points.

- pop_matrix:

  Numeric matrix \[n x k\]. Known population counts per target zone,
  with k demographic groups as columns.

- source_matrix:

  Numeric matrix \[m x k\]. Known counts at source points (e.g.,
  registered voters by age group).

## Value

Single numeric value: `sum((W_std %*% source_matrix - pop_matrix)^2)`

## Details

The weight matrix is computed as: \$\$W\_{ij} = t\_{ij}^{-\alpha_i}\$\$
then column-standardized so each column sums to 1. The interpolated
values are \\\hat{V} = W\_{std} \times V\\, and the objective is \\\sum
(\hat{V} - P)^2\\.

## See also

[`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md)
for the analytical gradient,
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md)
for the weight matrix,
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
for the optimization wrapper.

Other IDW core:
[`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md),
[`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md),
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md),
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)

## Examples

``` r
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2) # 2 zones x 3 sources
pop <- matrix(c(100, 200), nrow = 2)          # 2 zones x 1 group
src <- matrix(c(80, 120, 100), nrow = 3)      # 3 sources x 1 group
alpha <- c(1, 1.5)
idw_objective(alpha, tt, pop, src)
#> [1] 15197.62
```
