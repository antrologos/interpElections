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
