#' Enable or disable GPU acceleration
#'
#' Sets package-wide options controlling whether [optimize_alpha()] uses
#' GPU-accelerated optimization (PB-SGD) or CPU-only methods.
#'
#' @param enable Logical. `TRUE` to enable GPU, `FALSE` for CPU only.
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. `NULL` = auto-detect.
#' @param dtype Character. `"float32"` or `"float64"`. Default: `"float32"`.
#'
#' @details
#' When GPU is enabled, [optimize_alpha()] uses torch-based PB-SGD
#' (per-bracket stochastic gradient descent). This requires the `torch`
#' package to be installed with a working GPU backend (CUDA for NVIDIA,
#' MPS for Apple Silicon).
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
    if (nzchar(path) && file.exists(file.path(path, "R", "control.R"))) {
      path
    } else {
      ""
    }
  }, error = function(e) "")
}

# ---- Internal helpers --------------------------------------------------------

#' Detect NVIDIA GPU hardware via nvidia-smi (no torch needed)
#'
#' @return A list with: found, name, vram_mb, driver_version, cuda_driver_version.
#' @noRd
.detect_gpu_nvidia <- function() {
  default <- list(
    found = FALSE,
    name = NA_character_,
    vram_mb = NA_real_,
    driver_version = NA_character_,
    cuda_driver_version = NA_character_
  )

  # Find nvidia-smi (may not be on PATH on Windows)
  nvsmi <- "nvidia-smi"
  if (.detect_platform()$os == "windows" && !nzchar(Sys.which(nvsmi))) {
    win_paths <- c(
      "C:/Program Files/NVIDIA Corporation/NVSMI/nvidia-smi.exe",
      "C:/Program Files (x86)/NVIDIA Corporation/NVSMI/nvidia-smi.exe"
    )
    found <- win_paths[file.exists(win_paths)]
    if (length(found) > 0) nvsmi <- found[1]
  }

  # Query GPU name and VRAM
  csv <- tryCatch(
    system2(nvsmi,
            c("--query-gpu=name,memory.total,driver_version",
              "--format=csv,noheader,nounits"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(csv) || length(csv) == 0 || !is.null(attr(csv, "status"))) {
    return(default)
  }

  parts <- trimws(strsplit(csv[1], ",")[[1]])
  if (length(parts) < 3) return(default)

  name <- parts[1]
  vram_mb <- suppressWarnings(as.numeric(parts[2]))
  driver_version <- parts[3]

  # Get max CUDA version from the header output
  cuda_driver_version <- NA_character_
  header <- tryCatch(
    system2(nvsmi, stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(header)) {
    cuda_line <- grep("CUDA Version:", header, value = TRUE)
    if (length(cuda_line) > 0) {
      # Extract version after "CUDA Version:" specifically
      after <- sub(".*CUDA Version:\\s*", "", cuda_line[1])
      m <- regmatches(after, regexpr("[0-9]+\\.[0-9]+", after))
      if (length(m) > 0) cuda_driver_version <- m[1]
    }
  }

  list(
    found = TRUE,
    name = name,
    vram_mb = vram_mb,
    driver_version = driver_version,
    cuda_driver_version = cuda_driver_version
  )
}


#' Detect Apple Silicon GPU (no torch needed)
#'
#' @return A list with: found, chip.
#' @noRd
.detect_gpu_apple <- function() {
  default <- list(found = FALSE, chip = NA_character_)

  if (.detect_platform()$os != "mac") return(default)

  # arm64 means Apple Silicon (M1/M2/M3/M4)
  if (Sys.info()[["machine"]] != "arm64") return(default)

  # Try to get chip name from system_profiler
  chip <- NA_character_
  sp <- tryCatch(
    system2("system_profiler", "SPDisplaysDataType",
            stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )
  if (!is.null(sp)) {
    chip_line <- grep("Chipset Model:", sp, value = TRUE)
    if (length(chip_line) > 0) {
      chip <- trimws(sub(".*Chipset Model:\\s*", "", chip_line[1]))
    }
  }

  list(found = TRUE, chip = chip)
}


#' Detect GPU hardware without requiring torch
#'
#' @return A list with: type ("nvidia", "apple_silicon", or "none"),
#'   nvidia (from .detect_gpu_nvidia), apple (from .detect_gpu_apple).
#' @noRd
.detect_gpu_hardware <- function() {
  nvidia <- .detect_gpu_nvidia()
  apple <- .detect_gpu_apple()

  type <- if (nvidia$found) {
    "nvidia"
  } else if (apple$found) {
    "apple_silicon"
  } else {
    "none"
  }

  list(type = type, nvidia = nvidia, apple = apple)
}


#' Check torch R package and binary installation
#'
#' @return A list with: pkg_installed, pkg_version, binaries_installed, install_path.
#' @noRd
.check_torch_install <- function() {
  pkg_installed <- requireNamespace("torch", quietly = TRUE)
  pkg_version <- NA_character_
  binaries_installed <- FALSE
  install_path <- NA_character_

  if (pkg_installed) {
    pkg_version <- as.character(utils::packageVersion("torch"))
    binaries_installed <- tryCatch(
      torch::torch_is_installed(),
      error = function(e) FALSE
    )
    install_path <- tryCatch(
      torch::torch_install_path(),
      error = function(e) NA_character_
    )
  }

  list(
    pkg_installed = pkg_installed,
    pkg_version = pkg_version,
    binaries_installed = binaries_installed,
    install_path = install_path
  )
}


#' Check CUDA runtime via torch (requires torch loaded with binaries)
#'
#' @return A list with: cuda_available, cuda_device_count, cuda_runtime_version,
#'   cudnn_available, cudnn_version, compute_capability, tensor_test_passed.
#' @noRd
.check_cuda_runtime <- function() {
  default <- list(
    cuda_available = FALSE,
    cuda_device_count = 0L,
    cuda_runtime_version = NA_character_,
    cudnn_available = FALSE,
    cudnn_version = NA_character_,
    compute_capability = NA_character_,
    tensor_test_passed = FALSE
  )

  if (!requireNamespace("torch", quietly = TRUE)) return(default)

  cuda_available <- tryCatch(torch::cuda_is_available(), error = function(e) FALSE)
  if (!cuda_available) return(default)

  device_count <- tryCatch(torch::cuda_device_count(), error = function(e) 0L)

  runtime_version <- tryCatch({
    v <- torch::cuda_runtime_version()
    as.character(v)
  }, error = function(e) NA_character_)

  cudnn_available <- tryCatch(
    torch::backends_cudnn_is_available(),
    error = function(e) FALSE
  )
  cudnn_version <- tryCatch({
    # backends_cudnn_version() returns a numeric_version that may be
    # mis-parsed (e.g., 90701 → "90.7.1" instead of "9.7.1").
    # Re-parse from the raw integer if available.
    raw <- tryCatch(torch:::cpp_cudnn_runtime_version(), error = function(e) NULL)
    if (!is.null(raw) && is.numeric(raw) && raw > 0) {
      paste0(raw %/% 10000, ".", (raw %% 10000) %/% 100, ".", raw %% 100)
    } else {
      as.character(torch::backends_cudnn_version())
    }
  }, error = function(e) NA_character_)

  compute_cap <- tryCatch({
    cap <- torch::cuda_get_device_capability(0L)
    paste0(cap$Major, ".", cap$Minor)
  }, error = function(e) NA_character_)

  # Tensor smoke test — catches CPU-only lantern with CUDA DLLs present
  tensor_ok <- tryCatch({
    x <- torch::torch_tensor(1, device = "cuda")
    TRUE
  }, error = function(e) FALSE)

  list(
    cuda_available = cuda_available,
    cuda_device_count = device_count,
    cuda_runtime_version = runtime_version,
    cudnn_available = cudnn_available,
    cudnn_version = cudnn_version,
    compute_capability = compute_cap,
    tensor_test_passed = tensor_ok
  )
}


#' Check MPS runtime via torch
#'
#' @return A list with: mps_available, tensor_test_passed.
#' @noRd
.check_mps_runtime <- function() {
  default <- list(mps_available = FALSE, tensor_test_passed = FALSE)

  if (!requireNamespace("torch", quietly = TRUE)) return(default)

  mps_available <- tryCatch(
    torch::backends_mps_is_available(),
    error = function(e) FALSE
  )
  if (!mps_available) return(default)

  tensor_ok <- tryCatch({
    x <- torch::torch_tensor(1, device = "mps")
    TRUE
  }, error = function(e) FALSE)

  list(mps_available = mps_available, tensor_test_passed = tensor_ok)
}


# ---- Exported functions ------------------------------------------------------

#' Check torch and GPU setup
#'
#' Runs a comprehensive diagnostic of the torch dependency chain: whether the
#' torch R package is installed, whether libtorch/lantern binaries are present,
#' what GPU hardware is available, and whether CUDA or MPS acceleration is
#' actually working.
#'
#' @param verbose Logical. Print diagnostic messages. Default: TRUE.
#'
#' @return Invisibly, a list with components:
#' \describe{
#'   \item{torch_installed}{Logical. Is the torch R package installed?}
#'   \item{torch_version}{Character or NA. torch package version.}
#'   \item{binaries_installed}{Logical. Are libtorch/lantern binaries present?}
#'   \item{gpu_hardware}{Character. `"nvidia"`, `"apple_silicon"`, or `"none"`.}
#'   \item{gpu_name}{Character or NA. GPU name from hardware detection.}
#'   \item{gpu_vram}{Character or NA. VRAM in MB (NVIDIA only).}
#'   \item{cuda_available}{Logical. Does torch report CUDA as available?}
#'   \item{cuda_runtime}{Character or NA. CUDA runtime version.}
#'   \item{cuda_compute}{Character or NA. GPU compute capability.}
#'   \item{cudnn_available}{Logical.}
#'   \item{mps_available}{Logical. Is MPS available (Apple Silicon)?}
#'   \item{tensor_test}{Logical. Can a tensor be created on the GPU?}
#'   \item{device}{Character. Best available device: `"cuda"`, `"mps"`, or `"cpu"`.}
#'   \item{ready}{Logical. TRUE if GPU acceleration is fully working.}
#' }
#'
#' @seealso [setup_torch()] to install torch with GPU support,
#'   [use_gpu()] to enable GPU acceleration, [check_r5r()] for
#'   the analogous Java/r5r diagnostic.
#'
#' @examples
#' \dontrun{
#' check_torch()
#' }
#'
#' @keywords internal
check_torch <- function(verbose = TRUE) {
  msg <- if (verbose) message else function(...) invisible(NULL)

  # -- 1. torch R package --
  ti <- .check_torch_install()
  if (ti$pkg_installed) {
    msg("[ok] torch package installed (v", ti$pkg_version, ")")
  } else {
    msg("[!!] torch package not installed")
    msg("     Install with: install.packages('torch')")
    msg("     Then run: interpElections::setup_torch()")
  }

  # -- 2. libtorch/lantern binaries --
  if (ti$pkg_installed) {
    if (ti$binaries_installed) {
      msg("[ok] libtorch + lantern binaries installed")
      if (!is.na(ti$install_path)) {
        msg("     Path: ", ti$install_path)
      }
    } else {
      msg("[!!] libtorch/lantern binaries not installed")
      msg("     Run: interpElections::setup_torch()")
    }
  }

  # -- 3. GPU hardware (works without torch) --
  hw <- .detect_gpu_hardware()

  if (hw$nvidia$found) {
    vram_str <- if (!is.na(hw$nvidia$vram_mb)) {
      paste0(hw$nvidia$vram_mb, " MB VRAM")
    } else {
      ""
    }
    msg("[ok] NVIDIA GPU: ", hw$nvidia$name,
        if (nzchar(vram_str)) paste0(" (", vram_str, ")") else "")
    driver_info <- character(0)
    if (!is.na(hw$nvidia$driver_version)) {
      driver_info <- c(driver_info,
                       paste0("Driver: ", hw$nvidia$driver_version))
    }
    if (!is.na(hw$nvidia$cuda_driver_version)) {
      driver_info <- c(driver_info,
                       paste0("Max CUDA: ", hw$nvidia$cuda_driver_version))
    }
    if (length(driver_info) > 0) {
      msg("     ", paste(driver_info, collapse = " | "))
    }
  } else if (hw$apple$found) {
    chip_str <- if (!is.na(hw$apple$chip)) hw$apple$chip else "Apple Silicon"
    msg("[ok] Apple Silicon GPU: ", chip_str)
  } else {
    msg("[--] No GPU hardware detected")
  }

  # -- 4. CUDA runtime (only if NVIDIA + binaries) --
  cuda <- list(
    cuda_available = FALSE, cuda_device_count = 0L,
    cuda_runtime_version = NA_character_,
    cudnn_available = FALSE, cudnn_version = NA_character_,
    compute_capability = NA_character_,
    tensor_test_passed = FALSE
  )
  if (ti$binaries_installed && hw$nvidia$found) {
    cuda <- .check_cuda_runtime()
    if (cuda$cuda_available) {
      rt_str <- if (!is.na(cuda$cuda_runtime_version)) {
        paste0(" ", cuda$cuda_runtime_version)
      } else {
        ""
      }
      msg("[ok] CUDA runtime", rt_str,
          " (", cuda$cuda_device_count, " device",
          if (cuda$cuda_device_count != 1L) "s" else "", ")")
      detail <- character(0)
      if (!is.na(cuda$compute_capability)) {
        detail <- c(detail,
                    paste0("Compute capability: ", cuda$compute_capability))
      }
      if (cuda$cudnn_available && !is.na(cuda$cudnn_version)) {
        detail <- c(detail, paste0("cuDNN: ", cuda$cudnn_version))
      }
      if (length(detail) > 0) {
        msg("     ", paste(detail, collapse = " | "))
      }
    } else {
      msg("[!!] CUDA not available (torch reports no CUDA support)")
      msg("     Run: interpElections::setup_torch(reinstall = TRUE)")
    }
  } else if (ti$binaries_installed && !hw$nvidia$found) {
    msg("[--] CUDA: not applicable (no NVIDIA GPU)")
  }

  # -- 5. MPS runtime (only if Apple Silicon + binaries) --
  mps <- list(mps_available = FALSE, tensor_test_passed = FALSE)
  if (ti$binaries_installed && hw$apple$found) {
    mps <- .check_mps_runtime()
    if (mps$mps_available) {
      msg("[ok] MPS available")
    } else {
      msg("[!!] MPS not available (torch reports no MPS support)")
    }
  } else if (.detect_platform()$os != "mac") {
    msg("[--] MPS: not applicable (not macOS)")
  }

  # -- 6. Tensor smoke test --
  tensor_test <- cuda$tensor_test_passed || mps$tensor_test_passed
  if (ti$binaries_installed && (hw$nvidia$found || hw$apple$found)) {
    if (tensor_test) {
      device_name <- if (cuda$tensor_test_passed) "cuda" else "mps"
      msg("[ok] GPU tensor test: passed (", device_name, ")")
    } else if (cuda$cuda_available || mps$mps_available) {
      msg("[!!] GPU tensor test: FAILED")
      msg("     torch reports GPU available but cannot create tensors on it.")
      msg("     This usually means libtorch was built without GPU support.")
      msg("     Run: interpElections::setup_torch(reinstall = TRUE)")
    }
  }

  # -- 7. Summary --
  device <- if (cuda$tensor_test_passed) "cuda"
            else if (mps$tensor_test_passed) "mps"
            else "cpu"
  ready <- ti$pkg_installed && ti$binaries_installed && tensor_test

  if (ready) {
    msg("\nAll checks passed. GPU acceleration is ready.")
    msg("  Device: ", device, " | Enable with: use_gpu(TRUE)")
  } else if (!ti$pkg_installed) {
    msg("\nGPU not available. Install torch first:")
    msg("  interpElections::setup_torch()")
  } else if (!ti$binaries_installed) {
    msg("\nGPU not available. Install torch binaries:")
    msg("  interpElections::setup_torch()")
  } else if (hw$type == "none") {
    msg("\nNo GPU acceleration available. interpElections will use CPU optimization.")
    msg("This is fine for small municipalities (< 1000 census tracts).")
  } else {
    msg("\nGPU detected but not working. Try reinstalling torch binaries:")
    msg("  interpElections::setup_torch(reinstall = TRUE)")
  }

  # GPU name for return value
  gpu_name <- if (hw$nvidia$found) hw$nvidia$name
              else if (hw$apple$found) (hw$apple$chip %||% "Apple Silicon")
              else NA_character_
  gpu_vram <- if (hw$nvidia$found && !is.na(hw$nvidia$vram_mb)) {
    paste0(hw$nvidia$vram_mb, " MB")
  } else {
    NA_character_
  }

  invisible(list(
    torch_installed    = ti$pkg_installed,
    torch_version      = ti$pkg_version,
    binaries_installed = ti$binaries_installed,
    gpu_hardware       = hw$type,
    gpu_name           = gpu_name,
    gpu_vram           = gpu_vram,
    cuda_available     = cuda$cuda_available,
    cuda_runtime       = cuda$cuda_runtime_version,
    cuda_compute       = cuda$compute_capability,
    cudnn_available    = cuda$cudnn_available,
    mps_available      = mps$mps_available,
    tensor_test        = tensor_test,
    device             = device,
    ready              = ready
  ))
}

#' Install and configure torch with GPU support
#'
#' Installs the torch R package (if missing) and downloads the
#' platform-appropriate libtorch + lantern binaries with GPU support.
#' After installation, verifies that GPU acceleration works via
#' [check_torch()].
#'
#' @param reinstall Logical. Force re-download of libtorch/lantern binaries
#'   even if they already exist. Use this to fix broken CUDA installations
#'   (e.g., CPU-only lantern accidentally installed). Default: FALSE.
#' @param type Character or NULL. Installation type, passed to
#'   `torch::install_torch()`. Common values: `NULL` (auto-detect),
#'   `"cuda"` (force CUDA on Windows/Linux), `"cpu"` (force CPU-only).
#'   Default: NULL (auto-detect based on available GPU).
#' @param verbose Logical. Default: TRUE.
#'
#' @details
#' The installation proceeds in stages:
#' \enumerate{
#'   \item If the `torch` R package is not installed, it is installed from CRAN.
#'   \item `torch::install_torch()` downloads libtorch and lantern binaries.
#'        On Windows/Linux with an NVIDIA GPU and a CUDA toolkit, this
#'        automatically selects CUDA-enabled binaries.
#'   \item A verification step runs [check_torch()] to confirm GPU works.
#' }
#'
#' **Important**: If the torch package was already loaded in the current R
#' session before calling `setup_torch()`, you must restart R for the new
#' binaries to take effect. The function detects this and warns accordingly.
#'
#' @section CUDA auto-detection:
#' `torch::install_torch()` detects the CUDA version from:
#' \itemize{
#'   \item The `CUDA` environment variable (if set, forces a version)
#'   \item `CUDA_PATH` (Windows) or `CUDA_HOME` (Linux)
#'   \item `nvcc --version` on PATH
#' }
#'
#' @return Invisibly, the result of [check_torch()] after installation,
#'   or a partial list with `needs_restart = TRUE` if R must be restarted.
#'
#' @seealso [check_torch()] to diagnose the current setup,
#'   [use_gpu()] to enable GPU acceleration after setup.
#'
#' @examples
#' \dontrun{
#' setup_torch()                    # auto-detect GPU
#' setup_torch(type = "cuda")       # force CUDA
#' setup_torch(reinstall = TRUE)    # fix broken install
#' }
#'
#' @export
setup_torch <- function(reinstall = FALSE, type = NULL, verbose = TRUE) {
  msg <- if (verbose) message else function(...) invisible(NULL)

  # -- 1. Install torch R package if missing --
  if (!requireNamespace("torch", quietly = TRUE)) {
    msg("Installing torch R package from CRAN...")
    utils::install.packages("torch", quiet = !verbose)
    if (!requireNamespace("torch", quietly = TRUE)) {
      stop("Failed to install the 'torch' package from CRAN.\n",
           "Try manually: install.packages('torch')", call. = FALSE)
    }
    msg("[ok] torch package installed")
  } else {
    msg("[ok] torch package already installed (v",
        as.character(utils::packageVersion("torch")), ")")
  }

  # -- 2. Check if torch namespace was already loaded --
  torch_was_loaded <- isNamespaceLoaded("torch")

  # -- 3. Install binaries --
  needs_install <- reinstall || !torch::torch_is_installed()
  if (needs_install) {
    # Show what we detected about GPU hardware
    hw <- .detect_gpu_hardware()
    if (is.null(type)) {
      if (hw$type == "nvidia") {
        msg("NVIDIA GPU detected -- downloading CUDA-enabled binaries...")
      } else if (hw$type == "apple_silicon") {
        msg("Apple Silicon detected -- downloading MPS-enabled binaries...")
      } else {
        msg("No GPU detected -- downloading CPU-only binaries...")
      }
    } else {
      msg("Downloading binaries (type = '", type, "')...")
    }

    if (torch_was_loaded && needs_install) {
      warning(
        "The torch package is already loaded in this R session.\n",
        "New binaries will be downloaded but will NOT take effect until you restart R.\n",
        "After this function completes, restart R and run check_torch().",
        call. = FALSE
      )
    }

    install_args <- list(reinstall = reinstall)
    if (!is.null(type)) install_args$type <- type

    tryCatch(
      do.call(torch::install_torch, install_args),
      error = function(e) {
        stop("torch binary installation failed: ", conditionMessage(e), "\n",
             "You can try manually: torch::install_torch()",
             call. = FALSE)
      }
    )
    msg("[ok] Binaries installed")
  } else {
    msg("[ok] Binaries already present (use reinstall = TRUE to force)")
  }

  # -- 4. Verify --
  if (torch_was_loaded && needs_install) {
    msg("\n[!!] Restart R for new binaries to take effect.")
    msg("     Then run: interpElections::check_torch()")
    result <- list(
      torch_installed = TRUE,
      binaries_installed = TRUE,
      needs_restart = TRUE,
      ready = NA
    )
    return(invisible(result))
  }

  msg("\nVerifying installation...")
  result <- check_torch(verbose = verbose)

  if (isTRUE(result$ready)) {
    msg("\nSetup complete! Enable GPU with: use_gpu(TRUE)")
  } else if (result$torch_installed && result$binaries_installed &&
             !result$tensor_test) {
    msg("\n[!!] Binaries installed but GPU tensor test failed.")
    msg("     Try: interpElections::setup_torch(reinstall = TRUE)")
  }

  invisible(result)
}
