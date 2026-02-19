# Compute the per-bracket Sinkhorn-balanced interpolation objective

Calculates the sum of squared errors between per-bracket
Sinkhorn-balanced IDW-interpolated values and known population. This is
the R-level version of the loss function used by
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md).

## Usage

``` r
sinkhorn_objective(
  alpha,
  time_matrix,
  pop_matrix,
  source_matrix,
  row_targets,
  sk_iter = 50L
)
```

## Arguments

- alpha:

  Numeric vector of length n. Decay parameters.

- time_matrix:

  Numeric matrix \[n x m\]. Adjusted travel times (offset already
  applied). Must be strictly positive.

- pop_matrix:

  Numeric matrix \[n x k\]. Known population per zone.

- source_matrix:

  Numeric matrix \[m x k\]. Known counts at source points.

- row_targets:

  Numeric vector of length n. Desired row sums for the final aggregate
  Sinkhorn balancing.

- sk_iter:

  Integer. Maximum Sinkhorn iterations per bracket and for the final
  aggregate step. Default: 50.

## Value

Single numeric value:
`sum((W_balanced %*% source_matrix - pop_matrix)^2)`.

## Details

Builds a per-bracket weight matrix: for each demographic column, runs
convergence-based Sinkhorn with bracket-specific row/column targets,
then sums contributions and applies a final aggregate Sinkhorn to
enforce overall row and column constraints. Computes the calibration SSE
against the known population.

Note: `time_matrix` is expected with offset already applied (i.e.,
`time_matrix + offset`). Do not apply offset again.

## See also

[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
for the optimization wrapper,
[`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md)
for the final weight matrix.

Other Sinkhorn:
[`sinkhorn_balance()`](https://antrologos.github.io/interpElections/reference/sinkhorn_balance.md),
[`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md)
