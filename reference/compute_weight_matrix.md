# Compute the weight matrix

Builds a weight matrix \\W\\ from per-tract-per-bracket decay parameters
using column normalization. Uses torch for computation (CPU or GPU).

## Usage

``` r
compute_weight_matrix(
  time_matrix,
  alpha,
  pop_matrix,
  source_matrix,
  offset = 1,
  use_gpu = NULL,
  verbose = FALSE
)
```

## Arguments

- time_matrix:

  Numeric matrix \[n x m\]. Travel times or distances from each zone
  (row) to each source point (column).

- alpha:

  Numeric scalar, vector of length n, or matrix \[n x k\].
  Per-tract-per-bracket decay parameters. A scalar or n-vector is
  recycled across all k brackets. Typically from
  `optimize_alpha()$alpha`.

- pop_matrix:

  Numeric matrix \[n x k\]. Known population counts per zone and
  demographic bracket.

- source_matrix:

  Numeric matrix \[m x k\]. Known counts at source points per
  demographic bracket.

- offset:

  Numeric scalar. Added to `time_matrix` before exponentiation:
  \\K^b\_{ij} = (t\_{ij} + \text{offset})^{-\alpha\_{ib}}\\. Default: 1.

- use_gpu:

  Logical or NULL. If TRUE, use GPU; if FALSE, use CPU; if NULL
  (default), respect the global setting from
  [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md).

- verbose:

  Logical. Print progress? Default: FALSE.

## Value

Numeric matrix \[n x m\]. Column-normalized weight matrix where each
column sums to approximately 1. Use as `W \%*\% data` to interpolate any
source-level variable into zones.

## Details

This function performs the same column normalization used internally by
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
during epoch evaluation. It is useful when you have pre-computed alpha
values and need to build the weight matrix without running optimization.

Column normalization ensures voter conservation: each source-bracket
pair distributes its full count across zones.

## See also

[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
which returns both alpha and W.

## Examples

``` r
if (FALSE) { # \dontrun{
# After optimization
result <- optimize_alpha(tt, pop, src)
W <- result$W   # weight matrix already computed

# Or build W from pre-existing alpha
W <- compute_weight_matrix(tt, alpha, pop, src)
interpolated <- W %*% electoral_data
} # }
```
