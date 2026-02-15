# Check torch and GPU setup

Runs a comprehensive diagnostic of the torch dependency chain: whether
the torch R package is installed, whether libtorch/lantern binaries are
present, what GPU hardware is available, and whether CUDA or MPS
acceleration is actually working.

## Usage

``` r
check_torch(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. Print diagnostic messages. Default: TRUE.

## Value

Invisibly, a list with components:

- torch_installed:

  Logical. Is the torch R package installed?

- torch_version:

  Character or NA. torch package version.

- binaries_installed:

  Logical. Are libtorch/lantern binaries present?

- gpu_hardware:

  Character. `"nvidia"`, `"apple_silicon"`, or `"none"`.

- gpu_name:

  Character or NA. GPU name from hardware detection.

- gpu_vram:

  Character or NA. VRAM in MB (NVIDIA only).

- cuda_available:

  Logical. Does torch report CUDA as available?

- cuda_runtime:

  Character or NA. CUDA runtime version.

- cuda_compute:

  Character or NA. GPU compute capability.

- cudnn_available:

  Logical.

- mps_available:

  Logical. Is MPS available (Apple Silicon)?

- tensor_test:

  Logical. Can a tensor be created on the GPU?

- device:

  Character. Best available device: `"cuda"`, `"mps"`, or `"cpu"`.

- ready:

  Logical. TRUE if GPU acceleration is fully working.

## See also

[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
to install torch with GPU support,
[`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
to enable GPU acceleration,
[`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)
for the analogous Java/r5r diagnostic.

## Examples

``` r
if (FALSE) { # \dontrun{
check_torch()
} # }
```
