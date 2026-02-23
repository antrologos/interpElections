# Install and configure torch with GPU support

Installs the torch R package (if missing) and downloads the
platform-appropriate libtorch + lantern binaries with GPU support. After
installation, verifies that GPU acceleration works via
[`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md).

## Usage

``` r
setup_torch(reinstall = FALSE, type = NULL, verbose = TRUE)
```

## Arguments

- reinstall:

  Logical. Force re-download of libtorch/lantern binaries even if they
  already exist. Use this to fix broken CUDA installations (e.g.,
  CPU-only lantern accidentally installed). Default: FALSE.

- type:

  Character or NULL. Installation type, passed to
  [`torch::install_torch()`](https://torch.mlverse.org/docs/reference/install_torch.html).
  Common values: `NULL` (auto-detect), `"cuda"` (force CUDA on
  Windows/Linux), `"cpu"` (force CPU-only). Default: NULL (auto-detect
  based on available GPU).

- verbose:

  Logical. Default: TRUE.

## Value

Invisibly, the result of
[`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md)
after installation, or a partial list with `needs_restart = TRUE` if R
must be restarted.

## Details

The installation proceeds in stages:

1.  If the `torch` R package is not installed, it is installed from
    CRAN.

2.  [`torch::install_torch()`](https://torch.mlverse.org/docs/reference/install_torch.html)
    downloads libtorch and lantern binaries. On Windows/Linux with an
    NVIDIA GPU and a CUDA toolkit, this automatically selects
    CUDA-enabled binaries.

3.  A verification step runs
    [`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md)
    to confirm GPU works.

**Important**: If the torch package was already loaded in the current R
session before calling `setup_torch()`, you must restart R for the new
binaries to take effect. The function detects this and warns
accordingly.

## CUDA auto-detection

[`torch::install_torch()`](https://torch.mlverse.org/docs/reference/install_torch.html)
detects the CUDA version from:

- The `CUDA` environment variable (if set, forces a version)

- `CUDA_PATH` (Windows) or `CUDA_HOME` (Linux)

- `nvcc --version` on PATH

## See also

[`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md)
to diagnose the current setup,
[`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
to enable GPU acceleration after setup.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_torch()                    # auto-detect GPU
setup_torch(type = "cuda")       # force CUDA
setup_torch(reinstall = TRUE)    # fix broken install
} # }
```
