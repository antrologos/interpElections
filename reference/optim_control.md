# Optimization control parameters

Creates a control object for the SGD optimizer in
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md).
All parameters have sensible defaults; override only what you need.

## Usage

``` r
optim_control(
  max_epochs = 2000L,
  lr_init = 0.05,
  convergence_tol = 1e-04,
  patience = 50L,
  barrier_mu = 10,
  alpha_init = 2,
  alpha_min = 1,
  use_gpu = NULL,
  device = NULL,
  dtype = "float32"
)
```

## Arguments

- max_epochs:

  Integer. Maximum number of epochs (full passes through all tracts).
  The optimizer may stop earlier if convergence is detected. Default:
  2000.

- lr_init:

  Numeric. Initial ADAM learning rate. Reduced automatically via
  ReduceLROnPlateau when the epoch loss plateaus. Default: 0.05.

- convergence_tol:

  Numeric. Relative change in epoch loss below which the optimizer
  considers the solution converged. Default: 1e-4.

- patience:

  Integer. Number of consecutive epochs with no improvement (at minimum
  learning rate) before early stopping. The LR scheduler uses
  `2 * patience` as its own patience. Default: 50.

- barrier_mu:

  Numeric. Strength of the log-barrier penalty that prevents any census
  tract from receiving zero predicted voters. Set to 0 to disable.
  Default: 10.

- alpha_init:

  Numeric scalar, vector of length n, or matrix \[n x k\]. Initial guess
  for alpha. A scalar is recycled to all tracts and brackets. Default:
  2.

- alpha_min:

  Numeric. Lower bound for alpha values. The reparameterization becomes
  `alpha = alpha_min + softplus(theta)`. Default: 1, which restricts
  decay to linear or steeper.

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

## Value

A list of class `"interpElections_optim_control"` with one element per
parameter.

## See also

[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md),
[`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md)

## Examples

``` r
# Default settings
optim_control()
#> interpElections optimization control:
#>   max_epochs: 2000 
#>   lr_init: 0.05 
#>   convergence_tol: 1e-04 
#>   patience: 50 
#>   barrier_mu: 10 
#>   alpha_init: 2 
#>   alpha_min: 1 
#>   use_gpu: NULL (auto) 
#>   device: auto 
#>   dtype: float32 

# Use GPU with more epochs
optim_control(use_gpu = TRUE, max_epochs = 5000)
#> interpElections optimization control:
#>   max_epochs: 5000 
#>   lr_init: 0.05 
#>   convergence_tol: 1e-04 
#>   patience: 50 
#>   barrier_mu: 10 
#>   alpha_init: 2 
#>   alpha_min: 1 
#>   use_gpu: TRUE 
#>   device: auto 
#>   dtype: float32 

# Stricter convergence
optim_control(convergence_tol = 1e-6, patience = 100)
#> interpElections optimization control:
#>   max_epochs: 2000 
#>   lr_init: 0.05 
#>   convergence_tol: 1e-06 
#>   patience: 100 
#>   barrier_mu: 10 
#>   alpha_init: 2 
#>   alpha_min: 1 
#>   use_gpu: NULL (auto) 
#>   device: auto 
#>   dtype: float32 
```
