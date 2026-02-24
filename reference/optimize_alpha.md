# Find optimal decay parameters (alpha) for spatial interpolation

Optimizes the per-tract-per-bracket decay parameters that minimize the
error between interpolated values and known population counts. Uses
per-bracket SGD with column normalization and log-barrier penalty inside
torch autograd. Each demographic bracket gets its own per-tract kernel;
gradients flow through all computations. Works on both CPU and GPU
(CUDA/MPS).

## Usage

``` r
optimize_alpha(
  time_matrix,
  pop_matrix,
  source_matrix,
  row_targets = NULL,
  optim = optim_control(),
  offset = 1,
  verbose = TRUE,
  ...
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

  Numeric vector of length n, or NULL. Target row sums for balancing.
  Each element specifies how much weight a zone should attract,
  proportional to its share of total population. If NULL (default),
  auto-computed as `rowSums(pop_matrix) / sum(pop_matrix) * m`.

- optim:

  An
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)
  object with optimization parameters (max_epochs, lr_init,
  convergence_tol, patience, barrier_mu, alpha_init, alpha_min, use_gpu,
  device, dtype). Default:
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md).

- offset:

  Numeric. Value added to travel times before exponentiation. Default:
  1.

- verbose:

  Logical. Print progress messages? Default: TRUE.

- ...:

  **Deprecated**. Old-style individual parameters (`alpha_init`,
  `max_epochs`, `lr_init`, `use_gpu`, `device`, `dtype`,
  `convergence_tol`, `patience`, `barrier_mu`, `alpha_min`) are still
  accepted via `...` for backward compatibility, but will be removed in
  a future release. Use `optim = optim_control(...)` instead.

## Value

A list of class `"interpElections_optim"` with components:

- alpha:

  Numeric matrix \[n x k\]. Optimal per-tract-per-bracket decay
  parameters. Each row is a census tract, each column is a demographic
  bracket. Inactive brackets (zero population or voters) are filled with
  1.

- value:

  Numeric. Objective function value at optimum (Poisson deviance).

- W:

  Numeric matrix \[n x m\]. Column-normalized weight matrix from the
  best-epoch. Use directly for interpolation via `W \%*\% data`. Column
  sums are approximately 1.

- method:

  Character. Method used (e.g., `"pb_sgd_colnorm_cpu"`,
  `"pb_sgd_colnorm_cuda"`).

- convergence:

  Integer. 0 = early-stopped (improvement plateau detected); 1 = stopped
  at max_epochs.

- epochs:

  Integer. Number of epochs completed.

- steps:

  Integer. Total number of SGD gradient steps.

- elapsed:

  `difftime` object. Wall-clock time.

- message:

  Character. Additional information.

- history:

  Numeric vector. Full-dataset loss at each epoch.

- grad_norm_final:

  Numeric. Final gradient norm (theta-space).

- grad_history:

  Numeric vector. Gradient norm (theta-space) at each epoch.

- lr_history:

  Numeric vector. Learning rate at each epoch.

## Details

The optimization requires the `torch` R package. Install it with
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
if not already available.

**Parameterization**: alpha\[i,b\] is reparameterized as
`alpha = alpha_min + softplus(theta)` with `theta` unconstrained, where
`softplus(x) = log(1 + exp(x))`. With the default `alpha_min = 1`, alpha
is always at least 1 (inverse-distance decay or steeper). Set
`alpha_min = 0` for unconstrained optimization (similar to legacy
`exp(theta)`).

**Epoch structure**: Each epoch is one full-data gradient step with
exact gradients (column sums require all tracts). The loss reported at
each epoch is the true objective evaluated on the full dataset.

Two execution paths:

- **CPU** (default): `use_gpu = FALSE` or `NULL`. Uses torch on CPU
  device. Fast for small/medium problems (\< 2000 tracts).

- **GPU**: `use_gpu = TRUE`. Uses CUDA or MPS. Faster for large problems
  (\> 2000 tracts).

GPU memory usage is bounded by `2 * ka * n * m * bytes_per_elem`.

## See also

[`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)
for tuning parameters,
[`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
to toggle GPU globally,
[`compute_weight_matrix()`](https://antrologos.github.io/interpElections/reference/compute_weight_matrix.md)
to rebuild the weight matrix from pre-computed alpha,
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

# With custom control
result <- optimize_alpha(tt, pop, src,
  optim = optim_control(use_gpu = TRUE, max_epochs = 5000),
  verbose = FALSE)
} # }
```
