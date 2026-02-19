# Enable or disable GPU acceleration

Sets package-wide options controlling whether
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
uses GPU-accelerated optimization (PB-SGD) or CPU-only methods.

## Usage

``` r
use_gpu(enable = TRUE, device = NULL, dtype = "float32")
```

## Arguments

- enable:

  Logical. `TRUE` to enable GPU, `FALSE` for CPU only.

- device:

  Character or NULL. Torch device: `"cuda"`, `"mps"`, or `"cpu"`. `NULL`
  = auto-detect.

- dtype:

  Character. `"float32"` or `"float64"`. Default: `"float32"`.

## Value

Invisibly returns the previous settings as a list.

## Details

When GPU is enabled,
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
uses torch-based PB-SGD (per-bracket stochastic gradient descent). This
requires the `torch` package to be installed with a working GPU backend
(CUDA for NVIDIA, MPS for Apple Silicon).

The per-call `use_gpu` parameter in
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
always overrides the global setting.

## See also

[`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md)
to diagnose GPU setup,
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
to install torch with GPU support.

## Examples

``` r
use_gpu(FALSE) # ensure CPU mode
#> GPU disabled. Using CPU optimization.
```
