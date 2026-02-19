# Balance a weight matrix using Sinkhorn/IPF iterations

Applies alternating row and column scaling to enforce both row and
column marginal constraints simultaneously. Also known as Iterative
Proportional Fitting (IPF) or the RAS algorithm.

## Usage

``` r
sinkhorn_balance(
  W,
  row_targets = NULL,
  col_targets = NULL,
  max_iter = 1000L,
  tol = 1e-10
)
```

## Arguments

- W:

  Numeric matrix \[n x m\]. Raw non-negative weight matrix.

- row_targets:

  Numeric vector of length n. Desired row sums. Default: `NULL`
  (uniform: `rep(sum(col_targets) / n, n)`).

- col_targets:

  Numeric vector of length m. Desired column sums. Default: `NULL`
  (`rep(1, m)`, preserving source totals).

- max_iter:

  Integer. Maximum Sinkhorn iterations. Default: 1000.

- tol:

  Numeric. Convergence tolerance on maximum marginal error. Default:
  1e-10.

## Value

Numeric matrix \[n x m\] with row sums approximately equal to
`row_targets` and column sums approximately equal to `col_targets`.
Attributes:

- iterations:

  Integer. Number of iterations performed.

- converged:

  Logical. Whether convergence tolerance was met.

- unreachable:

  Integer vector. Indices of all-zero rows (skipped).

## Details

At each iteration:

1.  Row scaling: multiply each row by `row_targets[i] / rowSums(W)[i]`

2.  Column scaling: multiply each column by
    `col_targets[j] / colSums(W)[j]`

Rows that are all-zero (unreachable targets) are skipped during scaling
and remain zero. A warning is issued listing these rows.

The algorithm requires `sum(row_targets)` approximately equal to
`sum(col_targets)` for feasibility. A warning is issued if they differ
by more than 1\\

## See also

Other Sinkhorn:
[`sinkhorn_objective()`](https://antrologos.github.io/interpElections/reference/sinkhorn_objective.md),
[`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md)

## Examples

``` r
W <- matrix(c(3, 1, 0.5, 2, 4, 1.5), nrow = 2)
balanced <- sinkhorn_balance(W, row_targets = c(1, 2), col_targets = c(1, 1, 1))
rowSums(balanced)  # approx c(1, 2)
#> [1] 1 2
colSums(balanced)  # approx c(1, 1, 1)
#> [1] 1 1 1
```
