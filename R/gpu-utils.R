#' Enable or disable GPU acceleration
#'
#' Sets package-wide options controlling whether [optimize_alpha()] uses
#' GPU-accelerated optimization (torch ADAM) or CPU-only methods.
#'
#' @param enable Logical. `TRUE` to enable GPU, `FALSE` for CPU only.
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. `NULL` = auto-detect.
#' @param dtype Character. `"float32"` or `"float64"`. Default: `"float32"`.
#'
#' @details
#' When GPU is enabled, [optimize_alpha()] uses the torch ADAM optimizer
#' by default. This requires the `torch` package to be installed with a
#' working GPU backend (CUDA for NVIDIA, MPS for Apple Silicon).
#'
#' The per-call `use_gpu` parameter in [optimize_alpha()] always overrides
#' the global setting.
#'
#' @return Invisibly returns the previous settings as a list.
#'
#' @seealso [check_torch()] to diagnose GPU setup,
#'   [setup_torch()] to install torch with GPU support.
#'
#' @examples
#' use_gpu(FALSE) # ensure CPU mode
#'
#' @export
use_gpu <- function(enable = TRUE, device = NULL, dtype = "float32") {
  if (!is.null(device)) {
    valid_devices <- c("cuda", "mps", "cpu")
    if (!device %in% valid_devices) {
      stop(sprintf("device must be one of %s, got '%s'",
                   paste(valid_devices, collapse = ", "), device),
           call. = FALSE)
    }
  }
  if (!dtype %in% c("float32", "float64")) {
    stop("dtype must be 'float32' or 'float64'", call. = FALSE)
  }

  prev <- list(
    use_gpu = getOption("interpElections.use_gpu"),
    device = getOption("interpElections.device"),
    dtype = getOption("interpElections.dtype")
  )

  options(
    interpElections.use_gpu = enable,
    interpElections.device = device,
    interpElections.dtype = dtype
  )

  if (enable) {
    if (!requireNamespace("torch", quietly = TRUE)) {
      warning(
        "torch package is not installed. GPU optimization will not be available.\n",
        "Run interpElections::setup_torch() to install and configure torch.",
        call. = FALSE
      )
      options(interpElections.use_gpu = FALSE)
    } else if (!.gpu_available(device)) {
      warning(
        "No GPU device detected. Falling back to CPU.\n",
        "Run interpElections::check_torch() for detailed diagnostics.",
        call. = FALSE
      )
      options(interpElections.use_gpu = FALSE)
    } else {
      msg_device <- device %||% .detect_device()
      message("GPU enabled. Device: ", msg_device, ", dtype: ", dtype)
    }
  } else {
    message("GPU disabled. Using CPU optimization.")
  }

  invisible(prev)
}


# Internal: check if torch + GPU is accessible
.gpu_available <- function(device = NULL) {
  if (!requireNamespace("torch", quietly = TRUE)) return(FALSE)

  if (!is.null(device)) {
    switch(device,
      "cuda" = torch::cuda_is_available(),
      "mps"  = tryCatch(torch::backends_mps_is_available(), error = function(e) FALSE),
      "cpu"  = TRUE,
      FALSE
    )
  } else {
    torch::cuda_is_available() ||
      tryCatch(torch::backends_mps_is_available(), error = function(e) FALSE)
  }
}

# Internal: auto-detect best available device
.detect_device <- function() {
  if (!requireNamespace("torch", quietly = TRUE)) return("cpu")
  if (torch::cuda_is_available()) return("cuda")
  if (tryCatch(torch::backends_mps_is_available(), error = function(e) FALSE)) {
    return("mps")
  }
  "cpu"
}

# Internal: map dtype string to torch dtype
.resolve_dtype <- function(dtype_string) {
  switch(dtype_string,
    "float32" = torch::torch_float(),
    "float64" = torch::torch_double(),
    stop("dtype must be 'float32' or 'float64', got '", dtype_string, "'",
         call. = FALSE)
  )
}

# Internal: detect if running inside RStudio
.is_rstudio <- function() {
  nzchar(Sys.getenv("RSTUDIO"))
}

# Internal: find the interpElections source package root (for pkgload in subprocess).
# Returns "" if the package was installed normally (not loaded via devtools).
.find_package_root <- function() {
  pkg_ns <- asNamespace("interpElections")
  tryCatch({
    path <- attr(pkg_ns, "path") %||% ""
    # Source packages have R/*.R files; installed packages have R/*.rdb
    if (nzchar(path) && file.exists(file.path(path, "R", "optimize-gpu.R"))) {
      path
    } else {
      ""
    }
  }, error = function(e) "")
}
