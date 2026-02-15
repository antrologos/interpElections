# Compute the analytical gradient of the IDW objective

Computes the gradient of
[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md)
with respect to each element of the alpha vector. Used by the CPU
optimizer for faster convergence.

## Usage

``` r
idw_gradient(alpha, time_matrix, pop_matrix, source_matrix)
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

Numeric vector of length n: the gradient of the objective with respect
to each element of alpha.

## Details

The gradient is computed analytically using the chain rule through the
column-standardized weight matrix. This avoids numerical differentiation
and is significantly faster for large problems.

For k demographic groups, the total gradient is the sum of per-group
gradients.

## See also

[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md)
for the objective function,
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
for the optimization wrapper.

Other IDW core:
[`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md),
[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md),
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md),
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)

## Examples

``` r
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
pop <- matrix(c(100, 200), nrow = 2)
src <- matrix(c(80, 120, 100), nrow = 3)
alpha <- c(1, 1.5)
idw_gradient(alpha, tt, pop, src)
#> [1] -25209.66  22546.36
```
