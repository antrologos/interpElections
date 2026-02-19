# Find optimal decay parameters (alpha) for spatial interpolation

Optimizes the per-zone decay parameters that minimize the squared error
between Sinkhorn-balanced interpolated values and known population
counts. Uses torch autograd for gradient computation with ADAM optimizer
on both CPU and GPU.

## Usage

``` r
optimize_alpha(
  time_matrix,
  pop_matrix,
  source_matrix,
  row_targets = NULL,
  alpha_init = NULL,
  sinkhorn_iter = 5L,
  use_gpu = NULL,
  device = NULL,
  dtype = "float32",
  gpu_iterations = 20L,
  gpu_lr_init = 0.1,
  gpu_lr_decay = 0.6,
  gpu_grad_tol = 1e-04,
  gpu_grad_clip = 1,
  gpu_warmup_steps = 10L,
  lower_bound = 0,
  upper_bound = 20,
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

- sinkhorn_iter:

  Integer. Number of Sinkhorn iterations per objective evaluation during
  optimization. Higher values give more accurate balancing but are
  slower. Default: 5 (sufficient for optimization; final weights use
  full convergence via
  [`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md)).

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

- gpu_iterations:

  Integer. Number of outer ADAM phases with learning rate decay.
  Default: 20.

- gpu_lr_init:

  Numeric. Initial ADAM learning rate. Default: 0.1.

- gpu_lr_decay:

  Numeric. Learning rate decay factor per phase. Default: 0.6.

- gpu_grad_tol:

  Numeric. Gradient norm threshold for convergence. Default: 1e-4.

- gpu_grad_clip:

  Numeric or NULL. Maximum gradient norm for clipping. `NULL` disables
  clipping. Default: 1.0.

- gpu_warmup_steps:

  Integer. Linear learning rate warmup steps at the start of phase 1.
  Default: 10.

- lower_bound:

  Numeric. Lower bound for alpha values. Default: 0.

- upper_bound:

  Numeric. Upper bound for alpha values. Default: 20.

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

  Character. Method used (e.g., `"torch_adam_sinkhorn_cpu"`,
  `"torch_adam_sinkhorn_cuda"`).

- convergence:

  Integer. 0 = success.

- iterations:

  Number of ADAM steps taken.

- elapsed:

  `difftime` object. Wall-clock time.

- message:

  Character. Additional information.

- history:

  Numeric vector. Objective values at each step.

- grad_norm_final:

  Numeric. Final gradient norm.

- row_targets:

  Numeric vector. Row targets used for Sinkhorn.

- sinkhorn_iter:

  Integer. Sinkhorn iterations used.

## Details

The weight matrix is balanced via log-domain Sinkhorn iterations (row
sums proportional to population, column sums = 1) before computing the
calibration loss. Gradients are obtained by differentiating through the
unrolled Sinkhorn iterations via torch autograd.

The optimization requires the `torch` R package. Install it with
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
if not already available.

Two execution paths:

- **CPU** (default): `use_gpu = FALSE` or `NULL`. Uses torch on CPU
  device. Fast for small/medium problems (\< 2000 tracts).

- **GPU**: `use_gpu = TRUE`. Uses CUDA or MPS. Faster for large problems
  (\> 2000 tracts).

Both paths use the same ADAM optimizer with log-domain Sinkhorn.
Gradients are computed via torch autograd through the unrolled Sinkhorn
iterations.

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
