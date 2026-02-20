# Find optimal decay parameters (alpha) for spatial interpolation

Optimizes the per-zone decay parameters that minimize the squared error
between per-bracket Sinkhorn-balanced interpolated values and known
population counts. Uses Per-Bracket SGD (PB-SGD) with mini-batch
sampling and log-domain Sinkhorn inside torch autograd. Each demographic
bracket gets its own Sinkhorn transport; gradients flow through all
unrolled iterations. Works on both CPU and GPU (CUDA/MPS).

## Usage

``` r
optimize_alpha(
  time_matrix,
  pop_matrix,
  source_matrix,
  row_targets = NULL,
  alpha_init = NULL,
  batch_size = 500L,
  sk_iter = 15L,
  max_steps = 800L,
  lr_init = 0.05,
  use_gpu = NULL,
  device = NULL,
  dtype = "float32",
  lower_bound = 0.01,
  upper_bound = 20,
  convergence_tol = 1e-04,
  patience = 3L,
  offset = 1,
  verbose = TRUE
)
```

## Arguments

- time_matrix:

  Numeric matrix \[n x m\]. Raw travel times. Rows = target zones,
  columns = source points.

- pop_matrix:

  Numeric matrix \[n x k\]. Known population per zone, with k
  demographic groups as columns.

- source_matrix:

  Numeric matrix \[m x k\]. Known counts at source points (e.g.,
  registered voters by age group).

- row_targets:

  Numeric vector of length n, or NULL. Target row sums for Sinkhorn
  balancing. Each element specifies how much weight a zone should
  attract, proportional to its share of total population. If NULL
  (default), auto-computed as
  `rowSums(pop_matrix) / sum(pop_matrix) * m`.

- alpha_init:

  Numeric vector of length n, or a single value to be recycled. Initial
  guess for alpha. Default: `rep(1, n)`.

- batch_size:

  Integer. Number of zones (rows) sampled per SGD step. For cities with
  `n <= batch_size`, the full batch is used. Default: 500.

- sk_iter:

  Integer. Number of log-domain Sinkhorn iterations per SGD step. Higher
  values give more accurate per-bracket transport but increase memory
  usage. Default: 15.

- max_steps:

  Integer. Total number of SGD steps. Default: 800.

- lr_init:

  Numeric. Initial ADAM learning rate. Halved at steps 200, 400,
  and 600. Default: 0.05.

- use_gpu:

  Logical or NULL. If `TRUE`, use GPU (CUDA or MPS). If `FALSE`, use
  CPU. If `NULL` (default), reads the package option
  `interpElections.use_gpu` (set via
  [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)).

- device:

  Character or NULL. Torch device: `"cuda"`, `"mps"`, or `"cpu"`. Only
  used when GPU is enabled. Default: NULL (auto-detect).

- dtype:

  Character. Torch dtype: `"float32"` or `"float64"`. Default:
  `"float32"`. Float32 halves memory usage with negligible precision
  loss.

- lower_bound:

  Numeric. Lower bound for alpha values. Default: 0.01. Alpha is
  parameterized as `exp(theta)` internally (GLM log-link), so this bound
  is always satisfied smoothly without clamping.

- upper_bound:

  Numeric. Upper bound for alpha values. Default: 20. Kept for backward
  compatibility but rarely binding; the loss function naturally prevents
  excessively large alpha.

- convergence_tol:

  Numeric. Relative change in EMA loss below which the optimizer
  considers the solution converged. Default: 1e-4.

- patience:

  Integer. Number of consecutive convergence checks (every 50 steps)
  that must pass before early stopping. Default: 3.

- offset:

  Numeric. Value added to travel times before exponentiation. Default:
  1.

- verbose:

  Logical. Print progress messages? Default: TRUE.

## Value

A list of class `"interpElections_optim"` with components:

- alpha:

  Numeric vector. Optimal alpha values.

- value:

  Numeric. Objective function value at optimum.

- method:

  Character. Method used (e.g., `"pb_sgd_sinkhorn_cpu"`,
  `"pb_sgd_sinkhorn_cuda"`).

- convergence:

  Integer. 0 = success.

- iterations:

  Number of SGD steps taken.

- elapsed:

  `difftime` object. Wall-clock time.

- message:

  Character. Additional information.

- history:

  Numeric vector. Loss values at each step.

- grad_norm_final:

  Numeric. Final gradient norm.

- row_targets:

  Numeric vector. Row targets used for Sinkhorn.

- sk_iter:

  Integer. Sinkhorn iterations per step.

- batch_size:

  Integer. Mini-batch size used.

## Details

The optimization requires the `torch` R package. Install it with
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
if not already available.

Two execution paths:

- **CPU** (default): `use_gpu = FALSE` or `NULL`. Uses torch on CPU
  device. Fast for small/medium problems (\< 2000 tracts).

- **GPU**: `use_gpu = TRUE`. Uses CUDA or MPS. Faster for large problems
  (\> 2000 tracts).

Both paths use PB-SGD: mini-batch ADAM with per-bracket log-domain
Sinkhorn. Gradients are computed via torch autograd through the unrolled
Sinkhorn iterations. GPU memory usage is bounded by
`ka * min(batch_size, n) * m * bytes_per_elem * (2 + 2 * sk_iter)`.

## See also

[`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
to toggle GPU globally,
[`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md)
to build the final weight matrix,
[`sinkhorn_objective()`](https://antrologos.github.io/interpElections/reference/sinkhorn_objective.md)
for the objective function,
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
to install torch.

## Examples

``` r
if (FALSE) { # \dontrun{
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
pop <- matrix(c(100, 200), nrow = 2)
src <- matrix(c(80, 120, 100), nrow = 3)
result <- optimize_alpha(tt, pop, src, verbose = FALSE)
result$alpha
} # }
```
