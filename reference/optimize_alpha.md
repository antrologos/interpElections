# Find optimal decay parameters (alpha) for IDW interpolation

Optimizes the per-zone decay parameters that minimize the squared error
between IDW-interpolated values and known population counts. Supports
GPU-accelerated optimization via torch (ADAM) and CPU optimization via
L-BFGS-B.

## Usage

``` r
optimize_alpha(
  time_matrix,
  pop_matrix,
  source_matrix,
  alpha_init = NULL,
  use_gpu = NULL,
  device = NULL,
  dtype = "float32",
  gpu_iterations = 20L,
  gpu_lr_init = 0.1,
  gpu_lr_decay = 0.6,
  cpu_method = "auto",
  cpu_parallel = NULL,
  cpu_ncores = NULL,
  lower_bound = 0,
  upper_bound = 20,
  maxit = 10000L,
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

- alpha_init:

  Numeric vector of length n, or a single value to be recycled. Initial
  guess for alpha. Default: `rep(1, n)`.

- use_gpu:

  Logical or NULL. If `TRUE`, use torch ADAM optimizer. If `FALSE`, use
  CPU optimization. If `NULL` (default), reads the package option
  `interpElections.use_gpu` (set via
  [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)).

- device:

  Character or NULL. Torch device: `"cuda"`, `"mps"`, or `"cpu"`. Only
  used when GPU is enabled. Default: NULL (auto-detect).

- dtype:

  Character. Torch dtype: `"float32"` or `"float64"`. Default:
  `"float32"`. Float32 halves GPU memory usage with negligible precision
  loss for this optimization problem.

- gpu_iterations:

  Integer. Number of outer ADAM iterations. Default: 20.

- gpu_lr_init:

  Numeric. Initial ADAM learning rate. Default: 0.1.

- gpu_lr_decay:

  Numeric. Learning rate decay factor per outer iteration. Default: 0.6.

- cpu_method:

  Character. CPU optimization method: `"L-BFGS-B"`, `"BFGS"`, or
  `"auto"`. `"auto"` tries parallel L-BFGS-B first, then serial
  L-BFGS-B, then BFGS. Default: `"auto"`.

- cpu_parallel:

  Logical or NULL. Use `optimParallel` for CPU? Default: NULL
  (auto-detect based on package availability).

- cpu_ncores:

  Integer or NULL. Number of cores for parallel optimization. Default:
  NULL (auto = `max(1, detectCores() - 2)`).

- lower_bound:

  Numeric. Lower bound for alpha values. Default: 0.

- upper_bound:

  Numeric. Upper bound for alpha values. Values above ~10 produce nearly
  identical weights (the nearest source dominates), so capping prevents
  meaningless divergence between methods. Default: 20.

- maxit:

  Integer. Maximum iterations for CPU optimizer. Default: 10000.

- offset:

  Numeric. Value added to travel times. Default: 1.

- verbose:

  Logical. Print progress messages? Default: TRUE.

## Value

A list of class `"interpElections_optim"` with components:

- alpha:

  Numeric vector. Optimal alpha values.

- value:

  Numeric. Objective function value at optimum.

- method:

  Character. Optimization method used (e.g., `"gpu_adam"`,
  `"cpu_lbfgsb_parallel"`, `"cpu_lbfgsb"`, `"cpu_bfgs"`).

- convergence:

  Integer. 0 = success.

- iterations:

  Number of iterations/steps taken.

- elapsed:

  `difftime` object. Wall-clock time.

- message:

  Character. Additional information.

- history:

  Numeric vector. Objective values at each step (GPU only).

## See also

[`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
to toggle GPU globally,
[`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md)
to apply the optimal alphas,
[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md)
and
[`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md)
for the underlying math.

Other IDW core:
[`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md),
[`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md),
[`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md),
[`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md)

## Examples

``` r
tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
pop <- matrix(c(100, 200), nrow = 2)
src <- matrix(c(80, 120, 100), nrow = 3)
result <- optimize_alpha(tt, pop, src, verbose = FALSE)
result$alpha
#> [1] 1.5069488 0.9746938
```
