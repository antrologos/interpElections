# Compute Sinkhorn-balanced IDW weights

Builds inverse distance weights from a travel-time matrix and per-zone
decay parameters, then applies Sinkhorn balancing to enforce both row
and column marginal constraints.

## Usage

``` r
sinkhorn_weights(
  time_matrix,
  alpha,
  offset = 1,
  row_targets = NULL,
  col_targets = NULL,
  max_iter = 1000L,
  tol = 1e-10
)
```

## Arguments

- time_matrix:

  Numeric matrix \[n x m\]. Raw travel times. Rows = target zones,
  columns = source points.

- alpha:

  Numeric vector of length n. Decay parameters per target zone.

- offset:

  Numeric. Added to travel times before applying decay. Default: 1.

- row_targets:

  Numeric vector of length n. Desired row sums. For electoral
  interpolation, use `pop / sum(pop) * m` where `pop` is the population
  vector and `m = ncol(time_matrix)`. Default: `NULL` (uniform
  allocation).

- col_targets:

  Numeric vector of length m. Desired column sums. Default: `NULL`
  (`rep(1, m)` for source conservation).

- max_iter:

  Integer. Max Sinkhorn iterations. Default: 1000.

- tol:

  Numeric. Convergence tolerance. Default: 1e-10.

## Value

Numeric matrix \[n x m\]. Sinkhorn-balanced weights with `rowSums(W)`
approximately equal to `row_targets` and `colSums(W)` approximately
equal to `col_targets`. Preserves dimnames from `time_matrix`. Carries
attributes from
[`sinkhorn_balance()`](https://antrologos.github.io/interpElections/reference/sinkhorn_balance.md).

## See also

[`sinkhorn_balance()`](https://antrologos.github.io/interpElections/reference/sinkhorn_balance.md)
for the core algorithm,
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
to find optimal alpha values.

Other Sinkhorn:
[`sinkhorn_balance()`](https://antrologos.github.io/interpElections/reference/sinkhorn_balance.md),
[`sinkhorn_objective()`](https://antrologos.github.io/interpElections/reference/sinkhorn_objective.md)

## Examples

``` r
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
alpha <- c(1, 1.5)
pop <- c(100, 200)
m <- ncol(tt)
W <- sinkhorn_weights(tt, alpha, row_targets = pop / sum(pop) * m)
colSums(W)  # approx 1 (source conservation)
#> [1] 1 1 1
rowSums(W)  # proportional to population
#> [1] 1 2
```
