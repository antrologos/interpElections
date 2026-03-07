#' Find optimal decay parameters (alpha) for spatial interpolation
#'
#' Optimizes the per-tract-per-bracket decay parameters that minimize the
#' error between interpolated values and known population counts. Uses
#' per-bracket SGD with column normalization and log-barrier penalty inside
#' torch autograd. Each demographic bracket gets its own per-tract kernel;
#' gradients flow through all computations. Works on both CPU and GPU
#' (CUDA/MPS).
#'
#' @param time_matrix Numeric matrix \[n x m\]. Raw travel times.
#'   Rows = target zones, columns = source points.
#' @param pop_matrix Numeric matrix \[n x k\]. Known population per zone,
#'   with k demographic groups as columns.
#' @param source_matrix Numeric matrix \[m x k\]. Known counts at source
#'   points (e.g., registered voters by age group).
#' @param row_targets Numeric vector of length n, or NULL. Target row sums
#'   for balancing. Each element specifies how much weight a zone should
#'   attract, proportional to its share of total population. If NULL
#'   (default), auto-computed as `rowSums(pop_matrix) / sum(pop_matrix) * m`.
#' @param optim An [optim_control()] object with optimization parameters
#'   (max_epochs, lr_init, convergence_tol, patience, barrier_mu,
#'   alpha_init, alpha_min, use_gpu, device, dtype). Default:
#'   `optim_control()`.
#' @param offset Numeric. Value added to travel times before exponentiation
#'   (power kernel only; ignored when `kernel = "exponential"`).
#'   Default: 1.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#'
#' @return A list of class `"interpElections_optim"` with components:
#' \describe{
#'   \item{alpha}{Numeric matrix \[n x k\]. Optimal per-tract-per-bracket
#'     decay parameters. Each row is a census tract, each column is a
#'     demographic bracket. Inactive brackets (zero population or voters)
#'     are filled with 1.}
#'   \item{value}{Numeric. Poisson deviance at optimum.}
#'   \item{loss}{Numeric. Full loss at optimum (deviance + barrier + entropy).}
#'   \item{W}{Numeric matrix \[n x m\]. Column-normalized weight matrix
#'     from the best-epoch. Use directly for interpolation
#'     via `W \%*\% data`. Column sums are approximately 1.}
#'   \item{method}{Character. Method used (e.g.,
#'     `"pb_sgd_colnorm_cpu"`, `"pb_sgd_colnorm_cuda"`).}
#'   \item{convergence}{Integer. 0 = converged (gradient-based or
#'     window-based early stopping); 1 = stopped at max_epochs.}
#'   \item{epochs}{Integer. Number of epochs completed.}
#'   \item{steps}{Integer. Total number of SGD gradient steps.}
#'   \item{elapsed}{`difftime` object. Wall-clock time.}
#'   \item{message}{Character. Additional information.}
#'   \item{history}{Numeric vector. Full-dataset loss at each epoch.}
#'   \item{grad_norm_final}{Numeric. Final gradient norm (theta-space).}
#'   \item{grad_history}{Numeric vector. Gradient norm (theta-space) at
#'     each epoch.}
#'   \item{lr_history}{Numeric vector. Learning rate at each epoch.}
#'   \item{kernel}{Character. Kernel used (`"power"` or `"exponential"`).}
#' }
#'
#' @details
#' The optimization requires the `torch` R package. Install it with
#' [setup_torch()] if not already available.
#'
#' **Kernel**: Two kernel functions are available, controlled via the
#' `kernel` field in [optim_control()]:
#' \itemize{
#'   \item **Power** (default): \eqn{K(t) = (t + \text{offset})^{-\alpha}}.
#'     Classic inverse distance weighting.
#'   \item **Exponential**: \eqn{K(t) = \exp(-\alpha \cdot t)}.
#'     Lighter tail; relative decay increases with distance. Does not use
#'     `offset`.
#' }
#'
#' **Parameterization**: alpha\[i,b\] is reparameterized as
#' `alpha = alpha_min + softplus(theta)` with `theta` unconstrained,
#' where `softplus(x) = log(1 + exp(x))`. With the default
#' `alpha_min = 1` (power kernel), alpha is always at least 1
#' (inverse-distance decay or steeper). The exponential kernel defaults
#' to `alpha_min = 0`. Set `alpha_min = 0` for unconstrained
#' optimization (similar to legacy `exp(theta)`).
#'
#' **Epoch structure**: Each epoch is one full-data gradient step with
#' exact gradients (column sums require all tracts). The loss reported at
#' each epoch is the true loss evaluated on the full dataset.
#'
#' Two execution paths:
#' \itemize{
#'   \item **CPU** (default): `use_gpu = FALSE` or `NULL`. Uses torch on CPU
#'     device. Fast for small/medium problems (< 2000 tracts).
#'   \item **GPU**: `use_gpu = TRUE`. Uses CUDA or MPS. Faster for large
#'     problems (> 2000 tracts).
#' }
#'
#' GPU memory usage is bounded by
#' `2 * ka * n * m * bytes_per_elem`.
#'
#' @examples
#' \dontrun{
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
#' pop <- matrix(c(100, 200), nrow = 2)
#' src <- matrix(c(80, 120, 100), nrow = 3)
#' result <- optimize_alpha(tt, pop, src, verbose = FALSE)
#' result$alpha
#'
#' # With custom control
#' result <- optimize_alpha(tt, pop, src,
#'   optim = optim_control(use_gpu = TRUE, max_epochs = 5000),
#'   verbose = FALSE)
#' }
#'
#' @seealso [optim_control()] for tuning parameters,
#'   [use_gpu()] to toggle GPU globally, [compute_weight_matrix()]
#'   to rebuild the weight matrix from pre-computed alpha,
#'   [setup_torch()] to install torch.
#'
#' @export
optimize_alpha <- function(
    time_matrix,
    pop_matrix,
    source_matrix,
    row_targets = NULL,
    optim = optim_control(),
    offset = 1,
    verbose = TRUE
) {
  # Extract from control object
  alpha_init      <- optim$alpha_init
  max_epochs      <- optim$max_epochs
  lr_init         <- optim$lr_init
  use_gpu         <- optim$use_gpu
  device          <- optim$device
  dtype           <- optim$dtype
  convergence_tol <- optim$convergence_tol
  patience        <- optim$patience
  barrier_mu      <- optim$barrier_mu
  alpha_min       <- optim$alpha_min
  kernel          <- optim$kernel %||% "power"
  entropy_mu      <- optim$entropy_mu %||% 0
  target_eff_src  <- optim$target_eff_src
  dual_eta        <- optim$dual_eta %||% 1.0
  force_chunked   <- optim$force_chunked
  # Enable per-op MPS fallback BEFORE loading torch.  The env var is read
  # at libtorch initialization; setting it after requireNamespace() is too late.
  # Harmless on non-MPS platforms (only activated when an MPS op is missing).
  if (isTRUE(use_gpu) || isTRUE(getOption("interpElections.use_gpu"))) {
    Sys.setenv(PYTORCH_ENABLE_MPS_FALLBACK = "1")
  }

  # --- Check torch is available ---
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop(
      "The 'torch' package is required for optimization.\n",
      "Install with: interpElections::setup_torch()",
      call. = FALSE
    )
  }

  # Coerce to matrix if data.frame
  if (is.data.frame(time_matrix))   time_matrix   <- as.matrix(time_matrix)
  if (is.data.frame(pop_matrix))    pop_matrix    <- as.matrix(pop_matrix)
  if (is.data.frame(source_matrix)) source_matrix <- as.matrix(source_matrix)

  # Apply offset (only needed for power kernel to avoid singularity)
  if (kernel == "exponential") {
    t_adj <- time_matrix  # no offset for exponential
  } else {
    t_adj <- .apply_offset(time_matrix, offset)
  }

  # Validate offset-adjusted matrices
  .validate_matrices(t_adj, pop_matrix, source_matrix,
                     allow_zero_time = (kernel == "exponential"))

  n <- nrow(t_adj)
  m <- ncol(t_adj)
  k <- ncol(pop_matrix)

  # Auto-compute row_targets if not provided
  if (is.null(row_targets)) {
    pop_total <- rowSums(pop_matrix)
    row_targets <- pop_total / sum(pop_total) * m
  }
  if (length(row_targets) != n) {
    stop(sprintf(
      "row_targets must have length %d (one per row of time_matrix), got %d",
      n, length(row_targets)), call. = FALSE)
  }

  # alpha_init: scalar, n-vector, or n×k matrix.
  # When alpha_min > 0, ensure initialization is above alpha_min with
  # enough margin for the softplus gradient to be non-negligible.
  if (is.null(alpha_init)) {
    alpha_init <- alpha_min + 1  # scalar -> recycled inside .optimize_torch
  } else {
    if (!is.numeric(alpha_init))
      stop("alpha_init must be numeric", call. = FALSE)
    if (is.matrix(alpha_init)) {
      if (nrow(alpha_init) != n || ncol(alpha_init) != k)
        stop(sprintf(
          "alpha_init matrix must be %d x %d (n x k)", n, k),
          call. = FALSE)
    } else if (length(alpha_init) != 1L && length(alpha_init) != n) {
      stop(sprintf(
        "alpha_init must be a scalar, length-%d vector, or %d x %d matrix",
        n, n, k), call. = FALSE)
    }
    if (any(!is.finite(alpha_init)) || any(alpha_init <= 0))
      stop("alpha_init must contain only finite positive values",
           call. = FALSE)
  }

  # Coerce and validate integer params
  max_epochs <- as.integer(max_epochs)
  if (is.na(max_epochs) || max_epochs < 1L) {
    stop("max_epochs must be a positive integer (>= 1)", call. = FALSE)
  }

  # Validate lr_init
  if (!is.numeric(lr_init) || length(lr_init) != 1 ||
      !is.finite(lr_init) || lr_init <= 0) {
    stop("lr_init must be a single positive number", call. = FALSE)
  }

  # Validate barrier_mu
  if (!is.numeric(barrier_mu) || length(barrier_mu) != 1 ||
      !is.finite(barrier_mu) || barrier_mu < 0) {
    stop("barrier_mu must be a single non-negative number", call. = FALSE)
  }

  # Validate alpha_min
  if (!is.numeric(alpha_min) || length(alpha_min) != 1 ||
      !is.finite(alpha_min) || alpha_min < 0) {
    stop("alpha_min must be a single non-negative finite number", call. = FALSE)
  }

  # Resolve GPU setting
  resolved_use_gpu <- if (!is.null(use_gpu)) {
    use_gpu
  } else {
    getOption("interpElections.use_gpu", default = FALSE)
  }

  if (resolved_use_gpu) {
    resolved_device <- device %||%
      getOption("interpElections.device") %||%
      .detect_device()
    if (resolved_device == "cpu") {
      warning("GPU requested but no GPU found; using CPU torch",
              call. = FALSE)
    }
  } else {
    resolved_device <- "cpu"
  }
  resolved_dtype <- dtype %||%
    getOption("interpElections.dtype", default = "float32")

  start_time <- Sys.time()

  .run_torch <- function(dev) {
    .optimize_torch(
      time_matrix    = t_adj,
      pop_matrix     = pop_matrix,
      source_matrix  = source_matrix,
      alpha_init     = alpha_init,
      max_epochs     = max_epochs,
      lr_init        = lr_init,
      device         = dev,
      dtype          = resolved_dtype,
      convergence_tol = convergence_tol,
      patience       = as.integer(patience),
      barrier_mu     = barrier_mu,
      alpha_min      = alpha_min,
      kernel         = kernel,
      entropy_mu     = entropy_mu,
      target_eff_src = target_eff_src,
      dual_eta       = dual_eta,
      force_chunked  = force_chunked,
      verbose        = verbose
    )
  }

  # Run optimization with automatic MPS-to-CPU fallback.
  # MPS (Apple Silicon) has known issues with some torch ops that can
  # produce NaN or unsupported-operation errors.
  raw <- if (resolved_device == "mps") {
    tryCatch(.run_torch("mps"), error = function(e) {
      warning(sprintf(
        "MPS optimization failed (%s); retrying on CPU",
        conditionMessage(e)
      ), call. = FALSE)
      if (verbose) message("  Falling back to CPU...")
      .run_torch("cpu")
    })
  } else {
    .run_torch(resolved_device)
  }

  elapsed <- Sys.time() - start_time

  # Expand active-bracket alpha (n x ka) to full alpha (n x k).
  # Inactive brackets (zero pop or zero voters) get default alpha = 1.
  alpha_full <- matrix(1, n, k)
  alpha_full[, raw$active_brackets] <- raw$alpha

  # Post-optimization validation: replace non-finite alpha cells with 1
  if (any(!is.finite(alpha_full))) {
    n_bad <- sum(!is.finite(alpha_full))
    warning(sprintf(
      "%d alpha values are non-finite after optimization; replacing with 1",
      n_bad
    ), call. = FALSE)
    alpha_full[!is.finite(alpha_full)] <- 1
  }

  # Use torch-computed loss directly (consistent with epoch reporting).
  final_value <- raw$value

  result <- list(
    alpha               = alpha_full,
    value               = final_value,
    loss                = raw$loss %||% final_value,
    W                   = raw$W,
    method              = raw$method,
    convergence         = raw$convergence,
    epochs              = raw$epochs,
    steps               = raw$steps,
    elapsed             = elapsed,
    message             = raw$message %||% "",
    history             = raw$history %||% NULL,
    deviance_history    = raw$deviance_history %||% NULL,
    barrier_history     = raw$barrier_history %||% NULL,
    entropy_history     = raw$entropy_history %||% NULL,
    best_epoch          = raw$best_epoch %||% NULL,
    grad_norm_final     = raw$grad_norm_final %||% NULL,
    grad_history        = raw$grad_history %||% NULL,
    lr_history          = raw$lr_history %||% NULL,
    mean_eff_sources    = if (is.null(raw$mean_eff_sources) ||
                               is.na(raw$mean_eff_sources)) NULL
                          else raw$mean_eff_sources,
    entropy_mu_final    = raw$entropy_mu_final,
    entropy_mu_history  = raw$entropy_mu_history,
    target_eff_src      = raw$target_eff_src,
    row_targets         = row_targets,
    barrier_mu          = barrier_mu,
    alpha_min           = alpha_min,
    kernel              = kernel
  )
  class(result) <- "interpElections_optim"

  if (verbose) {
    conv_msg <- if (raw$convergence == 0L) " (converged)" else ""
    eff_msg <- if (!is.null(result$mean_eff_sources)) {
      sprintf(", eff_src=%.1f", result$mean_eff_sources)
    } else ""
    dual_msg <- if (!is.null(target_eff_src)) {
      sprintf(", entropy_mu_final=%.4f", result$entropy_mu_final)
    } else ""
    loss_display <- if (!is.null(raw$loss)) {
      format(round(raw$loss), big.mark = ",")
    } else {
      format(round(result$value), big.mark = ",")
    }
    message(sprintf(
      "  Completed %d epochs (%.1fs), loss=%s, deviance=%s%s%s%s",
      result$epochs,
      as.numeric(elapsed, units = "secs"),
      loss_display,
      format(round(result$value), big.mark = ","),
      eff_msg,
      dual_msg,
      conv_msg
    ))
  }

  result
}

#' @export
print.interpElections_optim <- function(x, ...) {
  cat("interpElections optimization result\n")
  cat(sprintf("  Method:      %s\n", x$method))
  cat("  Loss fn:     poisson\n")
  cat(sprintf("  Deviance:    %.4f\n", x$value))
  cat(sprintf("  Convergence: %d\n", x$convergence))
  if (is.matrix(x$alpha)) {
    cat(sprintf("  Alpha:       %d x %d matrix (tracts x brackets)\n",
                nrow(x$alpha), ncol(x$alpha)))
  } else {
    cat(sprintf("  Alpha:       %d values\n", length(x$alpha)))
  }
  cat(sprintf("  Alpha range: [%.3f, %.3f]\n",
              min(x$alpha), max(x$alpha)))
  cat(sprintf("  Epochs:      %d\n", x$epochs %||% NA))
  cat(sprintf("  Elapsed:     %.1f secs\n",
              as.numeric(x$elapsed, units = "secs")))
  if (!is.null(x$target_eff_src)) {
    cat(sprintf("  Dual ascent: target_eff_src=%.1f, final_eff_src=%.1f, entropy_mu=%.4f\n",
                x$target_eff_src, x$mean_eff_sources, x$entropy_mu_final))
  }
  invisible(x)
}

# ============================================================
# Internal: Per-Tract-Per-Bracket SGD via torch (GPU or CPU)
# ============================================================
#
# Column normalization uses full-data gradient steps (1 per epoch).
# Per-tract-per-bracket decay kernels:
#   K^b[i,j] = t[i,j]^(-alpha[i,b]).
# Softplus reparameterization: alpha = alpha_min + softplus(theta),
# theta unconstrained — no clamping, no gradient death at boundaries.
# Epoch structure: the loss reported at each epoch is the true
# loss evaluated on the full dataset.

.optimize_torch <- function(time_matrix, pop_matrix, source_matrix,
                             alpha_init,
                             max_epochs, lr_init, device, dtype,
                             convergence_tol, patience,
                             barrier_mu, alpha_min,
                             kernel,
                             entropy_mu = 0,
                             target_eff_src = NULL,
                             dual_eta = 1.0,
                             force_chunked = NULL,
                             verbose) {

  # --- RStudio subprocess delegation ---
  if (.is_rstudio() && !identical(Sys.getenv("INTERPELECTIONS_SUBPROCESS"), "1")) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "Torch optimization inside RStudio requires the 'callr' package.\n",
        "Install with: install.packages('callr')",
        call. = FALSE
      )
    }
    if (verbose) {
      message("  Running torch optimization in subprocess...")
    }
    result <- callr::r(
      function(time_matrix, pop_matrix, source_matrix, alpha_init,
               max_epochs, lr_init,
               device, dtype,
               convergence_tol, patience,
               barrier_mu, alpha_min,
               kernel, entropy_mu,
               target_eff_src, dual_eta,
               force_chunked,
               verbose, pkg_path) {
        Sys.setenv(INTERPELECTIONS_SUBPROCESS = "1")
        # Enable per-op MPS fallback BEFORE torch loads (env var is read
        # at libtorch initialization).  This lets unsupported Metal ops
        # silently fall back to CPU instead of crashing the whole run.
        if (device == "mps") {
          Sys.setenv(PYTORCH_ENABLE_MPS_FALLBACK = "1")
        }
        if (nzchar(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))) {
          pkgload::load_all(pkg_path, quiet = TRUE)
        } else {
          library(interpElections)
        }
        interpElections:::.optimize_torch(
          time_matrix = time_matrix,
          pop_matrix = pop_matrix,
          source_matrix = source_matrix,
          alpha_init = alpha_init,
          max_epochs = max_epochs, lr_init = lr_init,
          device = device, dtype = dtype,
          convergence_tol = convergence_tol, patience = patience,
          barrier_mu = barrier_mu,
          alpha_min = alpha_min,
          kernel = kernel,
          entropy_mu = entropy_mu,
          target_eff_src = target_eff_src,
          dual_eta = dual_eta,
          force_chunked = force_chunked,
          verbose = verbose
        )
      },
      args = list(
        time_matrix = time_matrix,
        pop_matrix = pop_matrix,
        source_matrix = source_matrix,
        alpha_init = alpha_init,
        max_epochs = max_epochs, lr_init = lr_init,
        device = device, dtype = dtype,
        convergence_tol = convergence_tol, patience = patience,
        barrier_mu = barrier_mu,
        alpha_min = alpha_min,
        kernel = kernel,
        entropy_mu = entropy_mu,
        target_eff_src = target_eff_src,
        dual_eta = dual_eta,
        force_chunked = force_chunked,
        verbose = verbose,
        pkg_path = .find_package_root()
      ),
      show = verbose
    )
    return(result)
  }

  torch_dtype <- .resolve_dtype(dtype)
  n <- nrow(time_matrix)
  m <- ncol(time_matrix)
  k <- ncol(pop_matrix)

  # --- Per-bracket preprocessing (R-side) ---
  P_cpu <- pop_matrix
  V_cpu <- source_matrix
  c_mat <- matrix(0, k, m)
  skip  <- logical(k)

  for (bi in seq_len(k)) {
    rb <- P_cpu[, bi]
    cb <- V_cpu[, bi]
    if (sum(cb) < 0.5 || sum(rb) < 0.5) {
      skip[bi] <- TRUE
      next
    }
    c_mat[bi, ] <- cb
  }

  active <- which(!skip)
  ka     <- length(active)

  if (ka == 0L) {
    stop("All demographic brackets are empty (sum < 0.5). Cannot optimize.",
         call. = FALSE)
  }

  c_mat <- c_mat[active, , drop = FALSE]

  # LR schedule parameters.
  # Non-adaptive: SGDR cosine annealing with warm restarts (deterministic).
  # Adaptive (dual ascent): constant LR. The dual update changes the loss
  # surface every epoch; monotone LR decay starves the primal optimizer.
  lr_T_0         <- 500L           # first cycle length
  lr_T_mult      <- 2L             # cycle length multiplier
  lr_eta_min_rat <- 0.01           # floor at 1% of lr_init
  min_lr         <- lr_init * lr_eta_min_rat

  # Verbose: print ~20 lines regardless of epoch count
  report_every <- max(1L, max_epochs %/% 20L)

  # --- Station-chunked forward pass gate ---
  # When ka*n*m > 50M, batched (ka,n,m) tensors exceed VRAM.
  # Station-chunked path loops over station slices of size m_chunk,
  # keeping (ka,n,m_chunk) tensors — full CUDA parallelism across all brackets.
  use_chunked <- if (!is.null(force_chunked)) isTRUE(force_chunked) else
    as.double(ka) * n * m > 50e6

  # --- Memory safety check (GPU only) ---
  bytes_per_elem <- if (dtype == "float32") 4 else 8
  # Batched dominant cost: 6 × (ka,n,m) tensors (log_K, W, autograd)
  estimated_mb   <- (6.0 * as.double(ka) * n * m) * bytes_per_elem / 1e6

  if (use_chunked) {
    # Auto-size station chunk from available VRAM (or fallback for CPU/unknown)
    vram_mb_for_chunk <- tryCatch({
      if (grepl("cuda", device, fixed = TRUE)) {
        hw <- .detect_gpu_nvidia()
        if (hw$found && !is.na(hw$vram_mb)) hw$vram_mb else NA_real_
      } else NA_real_
    }, error = function(e) NA_real_)

    if (!is.na(vram_mb_for_chunk)) {
      # 4 dominant tensors per chunk: log_K, W, grad_W, grad_log_K during backward.
      m_chunk <- max(1L, min(m, as.integer(floor(
        0.60 * vram_mb_for_chunk * 1e6 / (4.0 * ka * n * bytes_per_elem)))))
    } else {
      m_chunk <- min(m, 300L)  # CPU fallback
    }
    chunk_starts <- seq(1L, m, by = m_chunk)
    n_chunks     <- length(chunk_starts)

    if (verbose) message(sprintf(
      "  Problem too large for batched path (%.0f MB); using station-chunked path (%d chunks, m_chunk=%d)",
      estimated_mb, n_chunks, m_chunk))

  } else if (grepl("cuda", device, fixed = TRUE)) {
    vram_mb <- tryCatch({
      hw <- .detect_gpu_nvidia()
      if (hw$found && !is.na(hw$vram_mb)) hw$vram_mb else NA_real_
    }, error = function(e) NA_real_)

    if (!is.na(vram_mb) && estimated_mb > vram_mb * 0.8) {
      stop(sprintf(paste0(
        "Estimated GPU memory: %.0f MB ",
        "(%.0f MB VRAM available).\n",
        "Use use_gpu = FALSE for CPU optimization."
      ), estimated_mb, vram_mb), call. = FALSE)
    }

    if (verbose && !is.na(vram_mb)) {
      message(sprintf("  Estimated memory: %.0f MB / %.0f MB VRAM",
                      estimated_mb, vram_mb))
    }
  }

  # --- Dual ascent setup ---
  adaptive <- !is.null(target_eff_src)
  if (adaptive && entropy_mu <= 0) entropy_mu <- m / target_eff_src

  # Augmented Lagrangian: quadratic penalty (rho/2)*n*(mean_H - h_target)^2.
  # rho = m: the crossover (where quadratic restoring force balances the linear
  # entropy penalty) occurs at delta = entropy_mu_init / m = 1/target nats,
  # i.e., eff_src ≈ target * exp(-1/target). For target=5, crossover at eff_src≈4.1.
  # This prevents overshoot while allowing the dual update to fine-tune.
  h_target <- if (adaptive) log(target_eff_src) else NULL
  entropy_mu_init <- entropy_mu
  rho_quad <- if (adaptive) m else 0

  if (adaptive && target_eff_src > m * 0.8) {
    warning(sprintf(paste0(
      "target_eff_src (%.1f) is close to or above the number of stations (%d). ",
      "Maximum achievable eff_src with uniform weights is %d. ",
      "Consider a smaller target."),
      target_eff_src, m, m), call. = FALSE)
  }

  if (verbose) {
    if (adaptive) {
      entropy_msg <- sprintf(", target_eff_src=%.1f (dual ascent, rho=%.1f)", target_eff_src, rho_quad)
    } else if (entropy_mu > 0) {
      entropy_msg <- sprintf(", entropy_mu=%.2f", entropy_mu)
    } else {
      entropy_msg <- ""
    }
    message(sprintf(paste0(
      "  PB-SGD colnorm (%s, %s, %s kernel): full-data gradient, barrier_mu=%.1f, ",
      "loss=poisson, alpha_min=%.1f%s, max %d epochs"),
      device, dtype, kernel, barrier_mu, alpha_min, entropy_msg, max_epochs))
  }

  # Track tensors for cleanup
  gpu_tensors <- new.env(parent = emptyenv())
  on.exit({
    rm(list = ls(gpu_tensors), envir = gpu_tensors)
    gc(verbose = FALSE)
    if (requireNamespace("torch", quietly = TRUE)) {
      tryCatch(torch::cuda_empty_cache(), error = function(e) NULL)
    }
  }, add = TRUE)

  result <- tryCatch({
    # Detect unreachable pairs (NA in time_matrix) and create mask
    na_mask <- is.na(time_matrix)
    has_na <- any(na_mask)

    # t_basis: the quantity multiplied by -alpha to form log_K.
    # Power:       t_basis = log(t_adj)  =>  log_K = -alpha * log(t) = log(t^(-alpha))
    # Exponential: t_basis = t_adj       =>  log_K = -alpha * t      = log(exp(-alpha*t))
    if (kernel == "power") {
      if (has_na) time_matrix[na_mask] <- 1  # safe placeholder for log
      t_basis <- torch::torch_log(torch::torch_tensor(
        time_matrix, device = device, dtype = torch_dtype))
    } else {
      if (has_na) time_matrix[na_mask] <- 0  # safe placeholder (exp(-alpha*0)=1, masked later)
      t_basis <- torch::torch_tensor(
        time_matrix, device = device, dtype = torch_dtype)
    }

    # Additive log-mask for unreachable pairs.
    # Unreachable (NA) pairs get -1e20 in log space, so
    # exp(log_K - 1e20) = 0 exactly. MPS-safe (not -Inf).
    # Also keep multiplicative mask_float for diagnostics/extraction.
    # All $unsqueeze() views are made $contiguous() because MPS
    # (Apple Silicon) silently produces NaN on non-contiguous views.
    if (has_na) {
      mask_float <- torch::torch_tensor(
        ifelse(na_mask, 0, 1),
        device = device, dtype = torch_dtype)           # (n, m)
      log_mask_3d <- torch::torch_where(
        mask_float$unsqueeze(1L)$contiguous() > 0.5,
        torch::torch_zeros(1L, n, m, device = device, dtype = torch_dtype),
        torch::torch_full(c(1L, n, m), -1e20,
                          device = device, dtype = torch_dtype)
      )$contiguous()                                    # (1, n, m)
      gpu_tensors$mask <- mask_float
    }

    # --- Pre-computations ---
    p_mat_active <- P_cpu[, active, drop = FALSE]   # n × ka

    # Voter counts per bracket per station — linear (not log)
    c_mat_torch <- torch::torch_tensor(
      c_mat, device = device, dtype = torch_dtype)    # (ka, m)
    c_total <- c_mat_torch$sum(dim = 1L)              # (m,)
    c_total_2d <- c_total$unsqueeze(1L)$contiguous()  # (1, m)

    # Pre-compute contiguous 3D view of t_basis for epoch loop
    t_basis_3d <- t_basis$unsqueeze(1L)$contiguous()  # (1, n, m)

    # Active-bracket population tensor for loss computation
    p_active_torch <- torch::torch_tensor(
      p_mat_active, device = device, dtype = torch_dtype)  # (n, ka)

    # Precompute constant part of Poisson deviance:
    # D = 2*(sum(V) - sum(P*log(V)) + sum(P*log(P)) - sum(P))
    # The last two terms depend only on P (constant), so precompute them.
    P_safe_pre <- torch::torch_where(
      p_active_torch > 0, p_active_torch,
      torch::torch_ones_like(p_active_torch))
    P_log_P_sum <- as.numeric(
      (p_active_torch * torch::torch_log(P_safe_pre))$sum()$item())
    sum_P <- as.numeric(p_active_torch$sum()$item())
    deviance_const <- 2 * (P_log_P_sum - sum_P)
    rm(P_safe_pre)

    # Pre-allocate ones tensor for torch_where safe-log (entropy path).
    # Shape (n, m) matches W_agg; avoids allocation inside the epoch loop.
    W_ones_nm <- torch::torch_ones(n, m, device = device, dtype = torch_dtype)

    # --- Softplus reparameterization — per-tract-per-bracket ---
    # alpha[i,b] = alpha_min + softplus(theta[i,b]), theta unconstrained.
    # One alpha per census tract per active bracket.
    alpha_init_mat <- matrix(
      pmax(alpha_init, alpha_min + 1e-6), nrow = n, ncol = ka)
    # Invert: theta = softplus_inverse(alpha - alpha_min)
    # softplus_inverse(x) = log(exp(x) - 1); for x > 20, ~ x
    sp_arg <- alpha_init_mat - alpha_min
    theta_init <- ifelse(
      sp_arg > 20,
      sp_arg,
      log(pmax(exp(sp_arg) - 1, 1e-30))
    )

    # Break initialization symmetry with small deterministic perturbation
    # so different tracts/brackets get different initial gradients.
    old_seed <- if (exists(".Random.seed", envir = globalenv()))
      get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = globalenv()))
          rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(n * 7919L + ka * 104729L)
    theta_init <- theta_init + matrix(
      stats::rnorm(n * ka, mean = 0, sd = 0.1),
      nrow = n, ncol = ka
    )

    theta_torch <- torch::torch_tensor(
      theta_init,
      device        = device,
      requires_grad = TRUE,
      dtype         = torch_dtype
    )

    # alpha_fn: theta -> alpha (always >= alpha_min)
    alpha_min_t <- torch::torch_tensor(
      alpha_min, device = device, dtype = torch_dtype)
    gpu_tensors$alpha_min_t <- alpha_min_t
    alpha_fn <- function(th) alpha_min_t + torch::nnf_softplus(th)

    # Loss computation: Poisson deviance (constant terms precomputed)
    compute_data_loss <- function(V_hat) {
      V_clamped <- torch::torch_clamp(V_hat, min = 1e-30)
      # torch_where safe-log: avoid ::: and guard log(0) for zero-population brackets.
      V_for_log <- torch::torch_where(
        p_active_torch > 0, V_clamped,
        torch::torch_ones_like(V_clamped))
      2 * (V_clamped - p_active_torch * torch::torch_log(V_for_log))$sum() +
        deviance_const
    }

    rm(t_basis)  # Free (n,m) tensor; only t_basis_3d is used in epoch loop.
    gpu_tensors$t_basis_3d <- t_basis_3d
    gpu_tensors$p_act      <- p_active_torch
    gpu_tensors$theta      <- theta_torch
    gpu_tensors$c_mat      <- c_mat_torch
    gpu_tensors$c_total    <- c_total
    gpu_tensors$c_total_2d <- c_total_2d
    gpu_tensors$W_ones_nm  <- W_ones_nm

    if (use_chunked) {
      # Transpose t_basis to (m, n) so narrow along dim=1 gives contiguous (m_s, n) slices.
      # Softmax over n (last dim) is then fully coalesced on GPU.
      t_basis_mn  <- t_basis_3d$squeeze(1L)$t()$contiguous()              # (m, n)
      log_mask_mn <- if (has_na) log_mask_3d$squeeze(1L)$t()$contiguous() else NULL
      gpu_tensors$t_basis_mn  <- t_basis_mn
      gpu_tensors$log_mask_mn <- log_mask_mn
    }

    optim_params <- list(theta_torch)
    optimizer <- torch::optim_adam(
      optim_params, lr = lr_init, betas = c(0.9, 0.99))

    # SGDR cosine annealing with warm restarts (Loshchilov & Hutter, 2017).
    # lr_lambda receives epoch index (0-based from init, then 1, 2, ...),
    # returns a multiplier in [eta_min_ratio, 1.0].
    .sgdr_T_0    <- lr_T_0
    .sgdr_T_mult <- lr_T_mult
    .sgdr_eta    <- lr_eta_min_rat
    .sgdr_decay <- 0.7  # max LR decays by this factor each restart
    sgdr_lambda <- function(epoch) {
      T_cur <- epoch
      T_i   <- .sgdr_T_0
      n_restart <- 0L
      while (T_cur >= T_i) {
        T_cur <- T_cur - T_i
        T_i   <- T_i * .sgdr_T_mult
        n_restart <- n_restart + 1L
      }
      max_mult  <- .sgdr_decay ^ n_restart
      eta_floor <- .sgdr_eta * max_mult
      eta_floor + (max_mult - eta_floor) * 0.5 * (1 + cos(pi * T_cur / T_i))
    }
    if (adaptive) {
      # Constant LR in adaptive mode. The dual update changes the loss
      # surface every epoch, so any monotone LR decay starves the primal
      # optimizer. Adam's per-parameter v_hat already handles oscillation.
      scheduler <- NULL
    } else {
      scheduler <- torch::lr_lambda(optimizer, lr_lambda = sgdr_lambda)
    }

    best_loss        <- Inf
    best_data_loss   <- Inf
    best_eff_src     <- NA_real_
    best_alpha <- as.matrix(
      alpha_fn(theta_torch)$detach()$cpu())            # (n, ka)
    best_W     <- NULL                                   # n × m R matrix
    epoch_losses     <- numeric(max_epochs)
    deviance_history <- numeric(max_epochs)
    barrier_history  <- numeric(max_epochs)
    entropy_history  <- numeric(max_epochs)
    lr_history       <- numeric(max_epochs)
    last_grad_norm   <- NA_real_
    step_counter     <- 0L
    epoch            <- 0L
    best_epoch       <- 1L

    median_eff_src     <- NA_real_
    mean_eff_src       <- NA_real_
    eff_src_ema      <- NA_real_    # EMA-smoothed eff_src for dual ascent
    entropy_mu_history <- numeric(max_epochs)

    # Convergence tracking
    grad_conv_counter  <- 0L        # consecutive epochs with small rel_grad
    converged          <- FALSE
    nan_epoch_count    <- 0L        # consecutive NaN-gradient epochs (MPS guard)
    prev_lr            <- lr_init   # for warm restart detection
    dual_cycle_T       <- lr_T_0    # current cosine cycle length for dual normalization

    grad_history <- numeric(max_epochs)

    # Loop invariant: whether W_agg (voter-weighted aggregation) is needed.
    # TRUE when entropy or adaptive mode is active (entropy penalty uses W_agg).
    # W_3d is always computed via softmax; this flag only gates W_agg.
    needs_W_3d <- entropy_mu > 0 || adaptive

    prev_dH_dW_agg <- NULL  # delayed entropy gradient: updated at end of each epoch

    for (epoch in seq_len(max_epochs)) {

      # --- Full-data gradient step ---
      # Column normalization requires all n tracts for column sums,
      # so the forward pass is O(ka*n*m) regardless of batch size.
      # Using full-data loss gives exact gradients at the same cost.
      optimizer$zero_grad()

      # For chunked path, Pass 2 builds fresh alpha graphs per chunk; detach here.
      # For monolithic path, keep graph for backward().
      alpha_all <- if (use_chunked) {
        torch::with_no_grad(alpha_fn(theta_torch))
      } else {
        alpha_fn(theta_torch)
      }                                               # (n, ka)

      if (use_chunked) {
        # ================================================================
        # STATION-CHUNKED FORWARD PASS
        # Loop over station slices of size m_chunk, (ka,n,m') tensors.
        # Full CUDA parallelism across all ka brackets per slice.
        # Peak VRAM: O(ka*n*m_chunk) instead of O(ka*n*m).
        # ================================================================

        # Pass 1 (no_grad): accumulate V_hat_all (n,ka) and W_agg (n,m).
        V_hat_all   <- torch::torch_zeros(n, ka, device = device, dtype = torch_dtype)
        W_agg_parts <- vector("list", n_chunks)

        torch::with_no_grad({
          alpha_kn   <- alpha_all$t()$contiguous()  # (ka, n), contiguous — computed once
          W_tc_total <- torch::torch_zeros(n, ka, device = device, dtype = torch_dtype)

          for (s in seq_along(chunk_starts)) {
            j1  <- chunk_starts[s]
            m_s <- min(m_chunk, m - j1 + 1L)

            t_s     <- torch::torch_narrow(t_basis_mn, 1L, j1, m_s)   # (m_s, n), contiguous
            c_s     <- torch::torch_narrow(c_mat_torch, 2L, j1, m_s)  # (ka, m_s)
            # (ka,1,n) × (1,m_s,n) → (ka,m_s,n); n is last dim → coalesced softmax
            log_K_s <- -alpha_kn$unsqueeze(2L) * t_s$unsqueeze(1L)    # (ka, m_s, n)
            if (has_na) {
              log_K_s <- log_K_s +
                torch::torch_narrow(log_mask_mn, 1L, j1, m_s)$unsqueeze(1L)
            }
            W_s  <- torch::nnf_softmax(log_K_s, dim = 3L)             # (ka, m_s, n)
            Wt_s <- W_s * t_s$unsqueeze(1L)                            # (ka, m_s, n): W*t

            V_hat_all   <- V_hat_all + torch::torch_einsum(
              "bjn,bj->nb", list(W_s, c_s))                            # (n, ka)
            W_agg_parts[[s]] <- torch::torch_einsum(
              "bjn,bj->nj", list(W_s, c_s))                            # (n, m_s)

            # W_tc[i,b] = Σ_j W[b,j,i]*t[j,i]*c[b,j] (needed for analytical gradient)
            W_tc_total <- W_tc_total + torch::torch_einsum(
              "bjn,bj->nb", list(Wt_s, c_s))                           # (n, ka)
          }
        })

        W_agg <- torch::torch_cat(W_agg_parts, dim = 2L) / c_total_2d  # (n, m)
        rm(W_agg_parts)

        # NaN check (one GPU sync)
        if (!isTRUE(torch::torch_isfinite(V_hat_all$sum())$item())) {
          nan_epoch_count <- nan_epoch_count + 1L
          if (nan_epoch_count >= 5L) {
            stop("Forward pass produced NaN for 5 consecutive epochs",
                 call. = FALSE)
          }
          epoch_losses[epoch]       <- NA_real_
          deviance_history[epoch]   <- NA_real_
          barrier_history[epoch]    <- NA_real_
          entropy_history[epoch]    <- NA_real_
          grad_history[epoch]       <- NA_real_
          entropy_mu_history[epoch] <- entropy_mu
          if (!is.null(scheduler)) scheduler$step()
          nan_lr <- optimizer$param_groups[[1]]$lr
          lr_history[epoch] <- nan_lr
          if (!adaptive && nan_lr > prev_lr * 1.5) {
            grad_conv_counter <- 0L
            dual_cycle_T <- dual_cycle_T * lr_T_mult
            for (group in optimizer$param_groups) {
              for (p in group$params) {
                s_st <- optimizer$state$get(p)
                if (!is.null(s_st) && !is.null(s_st[["exp_avg"]])) {
                  s_st[["exp_avg"]]$zero_()
                }
              }
            }
          }
          prev_lr <- nan_lr
          next
        }

        # --- Scalar values (ONE GPU sync for data_loss; no syncs in grad loop) ---
        data_loss_val <- as.numeric(compute_data_loss(V_hat_all)$item())

        # Barrier signal: dBarrier/dV_hat[i,b] = -barrier_mu / V_total[i]  (same for all b)
        dBarrier_dVhat <- NULL
        barrier_val    <- 0
        if (barrier_mu > 0) {
          V_hat_total    <- V_hat_all$sum(dim = 2L)                # (n,)
          V_total_cl     <- torch::torch_clamp(V_hat_total, min = 1e-30)
          barrier_val    <- as.numeric(
            (-barrier_mu * V_total_cl$log()$sum())$item())
          dBarrier_dVhat <- (-barrier_mu / V_total_cl)$unsqueeze(2L)$expand(c(-1L, ka))
        }

        # Analytical surrogate: dL/dV_hat + dBarrier/dV_hat  (detached constants)
        # dL/dV_hat[i,b] = 2*(1 - P[i,b]/V_hat[i,b])
        V_hat_cl  <- torch::torch_clamp(V_hat_all, min = 1e-30)
        V_for_log <- torch::torch_where(
          p_active_torch > 0, V_hat_cl, torch::torch_ones_like(V_hat_cl))
        dL_dVhat <- (2 * (1 - p_active_torch / V_for_log))$detach()  # (n, ka)
        if (!is.null(dBarrier_dVhat)) dL_dVhat <- dL_dVhat + dBarrier_dVhat

        # Entropy + quadratic penalty signal from W_agg (same mini-autograd as monolithic path)
        dH_dW_agg    <- NULL
        H_agg_torch  <- NULL
        mean_H_torch <- NULL
        entropy_val  <- 0
        quad_val     <- 0
        if (needs_W_3d) {
          W_agg_g  <- W_agg$detach()$requires_grad_(TRUE)    # (n, m), mini-autograd leaf
          row_sum  <- torch::torch_clamp(
            W_agg_g$sum(dim = 2L, keepdim = TRUE), min = 1e-30)
          p_safe_g <- W_agg_g / row_sum
          p_log_g  <- torch::torch_where(p_safe_g > 0, p_safe_g, W_ones_nm)
          H_g      <- -(p_safe_g * torch::torch_log(p_log_g))$sum(dim = 2L)  # (n,)
          mean_H_torch      <- H_g$mean()
          entropy_penalty_g <- entropy_mu * H_g$sum()
          quad_penalty_g    <- if (adaptive) {
            (rho_quad / 2) * n * (mean_H_torch - h_target)^2
          } else {
            0
          }
          # Single backward on combined penalty → W_agg_g$grad = d(E+Q)/dW_agg
          (entropy_penalty_g + quad_penalty_g)$backward()
          dH_dW_agg    <- W_agg_g$grad                       # (n, m), detached
          H_agg_torch  <- H_g$detach()
          entropy_val  <- as.numeric(entropy_penalty_g$item())
          quad_val     <- if (adaptive) as.numeric(quad_penalty_g$item()) else 0
          median_eff_src <- exp(as.numeric(H_agg_torch$median()$item()))
          mean_eff_src   <- exp(as.numeric(mean_H_torch$item()))
        }

        total_loss_val <- data_loss_val + barrier_val + entropy_val + quad_val

        # Analytical Pass 2 (no backward): correct softmax Jacobian gradient.
        # dL/d_alpha[i,b] = Σ_j c[b,j]*W[b,j,i]*t[j,i]*(dL_bar[b,j] - dL[i,b])
        # where dL_bar[b,j] = Σ_i W[b,j,i]*dL[i,b]  (W-weighted mean of dL over tracts)
        grad_analytic <- torch::torch_zeros(n, ka, device = device, dtype = torch_dtype)
        torch::with_no_grad({
          for (s in seq_along(chunk_starts)) {
            j1  <- chunk_starts[s]
            m_s <- min(m_chunk, m - j1 + 1L)
            t_s     <- torch::torch_narrow(t_basis_mn, 1L, j1, m_s)
            c_s     <- torch::torch_narrow(c_mat_torch, 2L, j1, m_s)
            log_K_s <- -alpha_kn$unsqueeze(2L) * t_s$unsqueeze(1L)
            if (has_na) {
              log_K_s <- log_K_s +
                torch::torch_narrow(log_mask_mn, 1L, j1, m_s)$unsqueeze(1L)
            }
            W_s      <- torch::nnf_softmax(log_K_s, dim = 3L)          # (ka, m_s, n)
            Wt_s     <- W_s * t_s$unsqueeze(1L)                         # (ka, m_s, n)
            # dL_bar[b,j] = W-weighted mean of dL[·,b] over tracts
            dL_bar_s <- torch::torch_einsum(
              "bjn,nb->bj", list(W_s, dL_dVhat))                        # (ka, m_s)
            grad_analytic <- grad_analytic + torch::torch_einsum(
              "bjn,bj->nb", list(Wt_s, c_s * dL_bar_s))                 # (n, ka)
            if (needs_W_3d && !is.null(prev_dH_dW_agg)) {
              dH_s         <- torch::torch_narrow(prev_dH_dW_agg, 2L, j1, m_s)  # (n, m_s)
              c_tot_s      <- torch::torch_narrow(c_total_2d, 2L, j1, m_s)      # (1, m_s)
              eff_dH_s     <- dH_s / c_tot_s                                      # (n, m_s)
              # eff_dH_bar[b,j] = W-weighted mean of eff_dH[·,j] over tracts
              eff_dH_bar_s <- torch::torch_einsum(
                "bjn,nj->bj", list(W_s, eff_dH_s))                      # (ka, m_s)
              grad_analytic <- grad_analytic +
                torch::torch_einsum("bjn,bj->nb",
                                    list(Wt_s, c_s * eff_dH_bar_s)) -
                torch::torch_einsum("bjn,bj,nj->nb",
                                    list(Wt_s, c_s, eff_dH_s))
            }
          }
        })
        # d_alpha = Σ_j c*W*t*(dL_bar - dL) = grad_analytic - dL * W_tc_total
        d_alpha <- grad_analytic - dL_dVhat * W_tc_total                # (n, ka)

        # Chain rule through alpha = alpha_min + softplus(theta):
        # d_theta = d_alpha * sigmoid(theta)   [softplus'(theta) = sigmoid(theta)]
        sigmoid_theta <- torch::with_no_grad(torch::torch_sigmoid(theta_torch))
        theta_torch$grad <- (d_alpha * sigmoid_theta)$detach()

        # Store dH_dW_agg for next epoch's delayed entropy gradient.
        prev_dH_dW_agg <- dH_dW_agg

        # ================================================================

      } else {
        # ================================================================
        # MONOLITHIC BATCHED FORWARD PASS
        # ================================================================

        # 3D log-kernel: log K^b[i,j] = -alpha[i,b] * t_basis[i,j].
        # All views made $contiguous() to prevent MPS (Apple Silicon)
        # from silently producing NaN on non-contiguous tensors.
        alpha_3d <- alpha_all$t()$unsqueeze(3L)$contiguous()  # (ka, n, 1)
        log_K_3d <- -alpha_3d * t_basis_3d                    # (ka, n, m)

        # Additive mask: unreachable pairs get -1e20 → exp(-1e20) = 0.
        if (has_na) log_K_3d <- log_K_3d + log_mask_3d

        # Unified forward pass: softmax + bmm (fused CUDA kernels).
        # softmax = exp(x - logsumexp(x, dim)) — single fused kernel,
        # numerically stable via internal max-subtraction.
        W_3d <- torch::nnf_softmax(log_K_3d, dim = 2L)        # (ka, n, m)

        # V_hat via batched matrix-vector: cuBLAS GEMM dispatch.
        V_hat_all <- torch::torch_bmm(
          W_3d, c_mat_torch$unsqueeze(3L)
        )$squeeze(3L)$t()$contiguous()                           # (n, ka)

        # W_agg: voter-weighted aggregation across brackets (entropy path only).
        if (needs_W_3d) {
          W_agg <- torch::torch_einsum(
            "bij,bj->ij", list(W_3d, c_mat_torch)
          ) / c_total_2d                                         # (n, m)
        }

        # --- Entropy penalty on AGGREGATED weights ---
        # Penalize the entropy of the aggregated weight matrix (voter-weighted
        # mixture across brackets).  This is what weight_summary() reports and
        # what the user sees.  Computing on the aggregated W (instead of per-
        # bracket) ensures the gradient directly targets the metric that dual
        # ascent steers, eliminating the Jensen's-inequality gap that caused
        # entropy_mu instability with per-bracket entropy.
        if (entropy_mu > 0 || adaptive) {
          # W_agg already computed above via einsum contraction.

          # Row-normalize for entropy.  Clamp to avoid log(0).
          row_sum <- torch::torch_clamp(
            W_agg$sum(dim = 2L, keepdim = TRUE), min = 1e-30)
          p_safe <- W_agg / row_sum                                   # (n, m)

          # H[i] = -sum_j p[i,j] * log(p[i,j])
          # torch_where safe-log: redirect log(0) → log(1)=0 for zero entries.
          # Gradient at p_safe=0: p_safe * (1/p_for_log) = 0*1 = 0. NaN-free.
          p_for_log   <- torch::torch_where(p_safe > 0, p_safe, W_ones_nm)
          H_agg_torch <- -(p_safe * torch::torch_log(p_for_log))$sum(dim = 2L)  # (n,)

          if (entropy_mu > 0) {
            entropy_penalty <- entropy_mu * H_agg_torch$sum()
          } else {
            entropy_penalty <- 0
          }

          # Augmented Lagrangian quadratic penalty: (rho/2)*n*(mean_H - h_target)^2
          # Compute mean_H once — reused for both quad_penalty and mean_eff_src.
          mean_H_torch <- H_agg_torch$mean()
          quad_penalty <- 0
          if (adaptive) {
            quad_penalty <- (rho_quad / 2) * n * (mean_H_torch - h_target)^2
          }

        } else {
          entropy_penalty <- 0
          quad_penalty <- 0
        }

        # Full-data loss (exact, no scaling needed)
        data_loss <- compute_data_loss(V_hat_all)

        # Barrier penalty: log-barrier prevents zero-voter tracts
        barrier <- 0
        if (barrier_mu > 0) {
          V_hat_total <- V_hat_all$sum(dim = 2L)        # (n,)
          barrier <- -barrier_mu *
            torch::torch_log(
              torch::torch_clamp(V_hat_total, min = 1e-30))$sum()
        }

        # Total loss = deviance + barrier + entropy + quadratic penalty.
        loss <- data_loss
        if (barrier_mu > 0) loss <- loss + barrier
        if (entropy_mu > 0) loss <- loss + entropy_penalty
        if (adaptive) loss <- loss + quad_penalty

        # NaN check: single GPU sync on the scalar loss tensor.
        # All component $item() calls deferred to AFTER backward() to
        # avoid stalling the GPU pipeline with mid-forward sync barriers.
        loss_finite <- isTRUE(torch::torch_isfinite(loss)$item())
        if (!loss_finite) {
          nan_epoch_count <- nan_epoch_count + 1L
          if (nan_epoch_count >= 5L) {
            stop("Forward pass produced NaN for 5 consecutive epochs",
                 call. = FALSE)
          }
          epoch_losses[epoch]     <- NA_real_
          deviance_history[epoch] <- NA_real_
          barrier_history[epoch]  <- NA_real_
          entropy_history[epoch]  <- NA_real_
          grad_history[epoch]     <- NA_real_
          entropy_mu_history[epoch] <- entropy_mu
          if (!is.null(scheduler)) scheduler$step()
          nan_lr <- optimizer$param_groups[[1]]$lr
          lr_history[epoch] <- nan_lr
          # Keep dual_cycle_T, prev_lr, and Adam state in sync on NaN epochs.
          # Must mirror the normal-path warm restart handler (lines below).
          if (!adaptive && nan_lr > prev_lr * 1.5) {
            grad_conv_counter <- 0L
            dual_cycle_T <- dual_cycle_T * lr_T_mult
            for (group in optimizer$param_groups) {
              for (p in group$params) {
                s <- optimizer$state$get(p)
                if (!is.null(s) && !is.null(s[["exp_avg"]])) {
                  s[["exp_avg"]]$zero_()
                }
              }
            }
          }
          prev_lr <- nan_lr
          next
        }

        loss$backward()

        # --- Extract scalar values AFTER backward (GPU already synced) ---
        # Backward is the natural sync point; $item() calls here are free.
        data_loss_val <- as.numeric(data_loss$item())
        barrier_val <- if (barrier_mu > 0) as.numeric(barrier$item()) else 0
        entropy_val <- if (entropy_mu > 0) as.numeric(entropy_penalty$item()) else 0
        quad_val <- if (adaptive) as.numeric(quad_penalty$item()) else 0
        total_loss_val <- data_loss_val + barrier_val + entropy_val + quad_val
        if (entropy_mu > 0 || adaptive) {
          # CPU-side exp() instead of GPU kernel on scalar tensors.
          median_eff_src <- exp(as.numeric(H_agg_torch$median()$item()))
          mean_eff_src <- exp(as.numeric(mean_H_torch$item()))
        }

      }  # end if (use_chunked) else

      # Capture pre-clip gradient norm, then clip.
      # Guard: nn_utils_clip_grad_norm_ has an unguarded
      # `if (clip_coef$item() < 1)` that crashes with "missing value
      # where TRUE/FALSE needed" when gradients are NaN — common on
      # MPS (Apple Silicon) where some ops silently produce NaN.
      # When gradients are unusable, skip clip + step to avoid
      # poisoning the optimizer state.
      # Clip gradients and capture pre-clip norm in one pass.
      # nn_utils_clip_grad_norm_ returns the total gradient norm.
      clip_result <- tryCatch(
        torch::nn_utils_clip_grad_norm_(optim_params, max_norm = 1.0),
        error = function(e) NULL)
      grad_ok <- !is.null(clip_result)
      last_grad_norm <- if (grad_ok) as.numeric(clip_result$cpu()) else NA_real_

      if (grad_ok) {
        optimizer$step()
        step_counter <- step_counter + 1L
        nan_epoch_count <- 0L        # reset on success
      } else {
        # NaN gradients — zero them to prevent accumulation, skip step
        optimizer$zero_grad()
        nan_epoch_count <- nan_epoch_count + 1L
        if (nan_epoch_count >= 5L) {
          stop("Gradient computation produced NaN for 5 consecutive epochs",
               call. = FALSE)
        }
      }

      # W and alpha extraction deferred to best-model tracking to avoid
      # per-epoch GPU->CPU transfers.

      # --- Dual ascent: per-epoch additive update (augmented Lagrangian) ---
      # Standard ALM dual step: lambda += rho * g(x) per "inner solve."
      # Dampened by T_cycle so per-cycle cumulative is dual_eta * rho * mean_error
      # (one full ALM step per cosine cycle).
      if (adaptive && isTRUE(is.finite(mean_eff_src)) && mean_eff_src > 0) {
        mean_H_val <- log(mean_eff_src)  # = mean(H), recovered from exp(mean(H))
        if (is.na(eff_src_ema)) {
          eff_src_ema <- mean_eff_src
        } else {
          eff_src_ema <- 0.95 * eff_src_ema + 0.05 * mean_eff_src
        }
        entropy_mu <- entropy_mu +
          (dual_eta * rho_quad / dual_cycle_T) * (mean_H_val - h_target)
        entropy_mu <- max(entropy_mu, 0.01)
        entropy_mu <- min(entropy_mu, 1e3)
      }
      entropy_mu_history[epoch] <- entropy_mu

      epoch_losses[epoch]     <- total_loss_val
      deviance_history[epoch] <- data_loss_val
      barrier_history[epoch]  <- barrier_val
      entropy_history[epoch]  <- entropy_val

      # Record LR BEFORE the scheduler step (captures the LR actually used
      # for this epoch's gradient step, not the next epoch's LR).
      lr_history[epoch]   <- optimizer$param_groups[[1]]$lr
      grad_history[epoch] <- last_grad_norm

      # --- LR schedule ---
      if (!is.null(scheduler)) {
        # Non-adaptive: SGDR cosine annealing (epoch-based, no metric).
        scheduler$step()
      }
      # Adaptive mode: constant LR (no schedule). Adam handles oscillation.

      # --- Convergence check (two-tier) ---
      current_lr <- optimizer$param_groups[[1]]$lr

      # Warm restart detection (non-adaptive only): reset gradient convergence
      # counter and clear Adam momentum (keep curvature estimates).
      # In adaptive mode, LR only decreases so this never triggers.
      if (!adaptive && current_lr > prev_lr * 1.5) {
        grad_conv_counter <- 0L
        dual_cycle_T <- dual_cycle_T * lr_T_mult
        for (group in optimizer$param_groups) {
          for (p in group$params) {
            s <- optimizer$state$get(p)
            if (!is.null(s) && !is.null(s[["exp_avg"]])) {
              s[["exp_avg"]]$zero_()
            }
          }
        }
      }
      prev_lr <- current_lr

      # eff_near_target: 20% tolerance (patience tier), 5% (gradient tier)
      # Use EMA (smoother) to avoid epoch-to-epoch noise blocking convergence.
      eff_near_target <- if (adaptive && isTRUE(is.finite(eff_src_ema))) {
        abs(eff_src_ema - target_eff_src) / target_eff_src < 0.2
      } else {
        TRUE
      }
      eff_near_target_tight <- if (adaptive && isTRUE(is.finite(eff_src_ema))) {
        abs(eff_src_ema - target_eff_src) / target_eff_src < 0.05
      } else {
        TRUE
      }

      # Dual stability gate (adaptive mode only).
      # Augmented Lagrangian convergence requires the dual variable
      # (entropy_mu) to have stabilized, not just the primal (deviance).
      # Use sqrt(convergence_tol) as the dual tolerance — the dual
      # operates on a different scale and converges more slowly.
      dual_stable <- TRUE
      mu_rel_change <- NA_real_
      if (adaptive && epoch > patience) {
        mu_lookback <- entropy_mu_history[epoch - patience]
        if (isTRUE(is.finite(mu_lookback)) && abs(mu_lookback) > 1e-10) {
          mu_rel_change <- abs(entropy_mu - mu_lookback) / abs(mu_lookback)
          dual_stable <- mu_rel_change < sqrt(convergence_tol)
        }
      }

      # Total loss stability gate: allows convergence when the total loss
      # (including all penalties) has stagnated, even if entropy_mu is still
      # slowly drifting.  If total_loss is flat, the primal solution is at
      # a practical optimum — residual dual drift is compensated by equal
      # and opposite primal adjustments.
      total_loss_stable <- FALSE
      tl_rel_change <- NA_real_
      if (epoch > patience) {
        tl_lookback <- epoch_losses[epoch - patience]
        if (isTRUE(is.finite(tl_lookback)) && abs(tl_lookback) > 1e-10) {
          tl_rel_change <- abs(total_loss_val - tl_lookback) / abs(tl_lookback)
          total_loss_stable <- tl_rel_change < sqrt(convergence_tol)
        }
      }

      # Tier 1: Gradient-based convergence (true stationarity).
      # rel_grad = |grad|_2 / (1 + |theta|_2) is scale-invariant.
      # Require 5 consecutive epochs to filter transient cancellations
      # in the non-stationary objective (entropy_mu changes each epoch).
      if (isTRUE(is.finite(last_grad_norm)) && epoch >= 20L && grad_ok) {
        theta_norm <- tryCatch(
          as.numeric(theta_torch$detach()$norm()$cpu()),
          error = function(e) NA_real_
        )
        rel_grad <- if (isTRUE(is.finite(theta_norm))) {
          last_grad_norm / (1 + theta_norm)
        } else {
          NA_real_
        }

        if (isTRUE(rel_grad < 1e-4) && isTRUE(eff_near_target_tight) &&
            (dual_stable || total_loss_stable)) {
          grad_conv_counter <- grad_conv_counter + 1L
        } else {
          grad_conv_counter <- 0L
        }

        if (grad_conv_counter >= 5L) {
          converged <- TRUE
          if (verbose) {
            quad_conv_log <- if (adaptive && quad_val > 0) {
              sprintf(", quad=%s", format(round(quad_val), big.mark = ","))
            } else ""
            stability_log <- if (adaptive && !is.na(mu_rel_change)) {
              gate <- if (dual_stable) "mu" else if (total_loss_stable) "total_loss" else "?"
              sprintf(", mu_change=%.4f%%, tl_change=%.4f%% [gate=%s]",
                      mu_rel_change * 100,
                      if (!is.na(tl_rel_change)) tl_rel_change * 100 else NA_real_,
                      gate)
            } else ""
            message(sprintf(paste0(
              "  Converged at epoch %d (gradient criterion: ",
              "rel_grad=%.2e for 5 epochs, loss=%s",
              " [deviance=%s, barrier=%s, entropy=%s%s]%s)"),
              epoch, rel_grad,
              format(round(total_loss_val), big.mark = ","),
              format(round(data_loss_val), big.mark = ","),
              format(round(barrier_val), big.mark = ","),
              format(round(entropy_val), big.mark = ","),
              quad_conv_log, stability_log))
          }
          break
        }
      } else {
        grad_conv_counter <- 0L
      }

      # Tier 2: Window-based convergence (stagnation detection).
      # Compare the current deviance (or total loss in non-adaptive mode)
      # against its value `patience` epochs ago. Converge when the relative
      # improvement over the window is less than convergence_tol.
      # This measures actual trend rather than comparing against a noisy
      # absolute minimum — robust to epoch-to-epoch oscillations caused
      # by dual ascent adjustments.
      # Only active in the refinement phase (LR < 10% of lr_init) to avoid
      # triggering during high-LR exploration after warm restarts.
      conv_metric <- if (adaptive) data_loss_val else total_loss_val
      lr_in_refinement <- if (adaptive) TRUE else (current_lr < lr_init * 0.1)

      if (epoch > patience && lr_in_refinement &&
          isTRUE(eff_near_target) && (dual_stable || total_loss_stable) &&
          isTRUE(is.finite(conv_metric))) {
        lookback <- if (adaptive) deviance_history[epoch - patience]
                    else epoch_losses[epoch - patience]
        if (isTRUE(is.finite(lookback)) && abs(lookback) > 1e-10) {
          window_rel_change <- (lookback - conv_metric) / abs(lookback)
          if (isTRUE(abs(window_rel_change) < convergence_tol)) {
            converged <- TRUE
            if (verbose) {
              eff_conv <- if ((entropy_mu > 0 || adaptive) &&
                              !is.na(median_eff_src)) {
                sprintf(", eff_src=%.1f", median_eff_src)
              } else ""
              dual_conv <- if (adaptive) {
                sprintf(", entropy_mu=%.4f", entropy_mu)
              } else ""
              stability_log <- if (adaptive && !is.na(mu_rel_change)) {
                gate <- if (dual_stable) "mu" else if (total_loss_stable) "total_loss" else "?"
                sprintf(", mu_change=%.4f%%, tl_change=%.4f%% [gate=%s]",
                        mu_rel_change * 100,
                        if (!is.na(tl_rel_change)) tl_rel_change * 100 else NA_real_,
                        gate)
              } else ""
              quad_win_log <- if (adaptive && quad_val > 0) {
                sprintf(", quad=%s", format(round(quad_val), big.mark = ","))
              } else ""
              message(sprintf(paste0(
                "  Converged at epoch %d (loss=%s",
                " [deviance=%s, barrier=%s, entropy=%s%s]%s%s%s, ",
                "lr=%.1e, %.6f%% change over last %d epochs)"),
                epoch,
                format(round(total_loss_val), big.mark = ","),
                format(round(data_loss_val), big.mark = ","),
                format(round(barrier_val), big.mark = ","),
                format(round(entropy_val), big.mark = ","),
                quad_win_log,
                eff_conv,
                dual_conv,
                stability_log,
                current_lr, abs(window_rel_change) * 100, patience))
            }
            break
          }
        }
      }

      # Best model tracking.
      # In dual ascent: the converged state IS the saddle-point solution.
      # Models from earlier epochs were optimized for wrong entropy_mu values.
      # Track best near-target model only as fallback for non-convergence.
      # At convergence, we override with the final epoch (see after loop).
      # W extraction is deferred to here (only when a new best is found)
      # to avoid the expensive GPU->CPU transfer of the (n,m) matrix
      # on every epoch.
      if (adaptive) {
        eff_ok <- isTRUE(!is.na(median_eff_src) &&
          abs(median_eff_src - target_eff_src) / target_eff_src < 0.2)
        if (eff_ok && isTRUE(is.finite(data_loss_val)) &&
            isTRUE(data_loss_val < best_data_loss)) {
          best_loss       <- total_loss_val
          best_data_loss  <- data_loss_val
          torch::with_no_grad({
            best_alpha <- as.matrix(alpha_all$detach()$cpu())
            best_W <- as.matrix(W_agg$detach()$cpu())
          })
          best_eff_src    <- median_eff_src
          best_epoch      <- epoch
        }
      } else {
        # Non-adaptive: fixed objective, standard best-model tracking
        if (isTRUE(is.finite(total_loss_val)) &&
            isTRUE(total_loss_val < best_loss)) {
          best_loss       <- total_loss_val
          best_data_loss  <- data_loss_val
          torch::with_no_grad({
            best_alpha <- as.matrix(alpha_all$detach()$cpu())
            if (needs_W_3d || use_chunked) {
              # W_agg always computed in Pass 1 for chunked path; use directly.
              best_W <- as.matrix(W_agg$detach()$cpu())
            } else {
              best_W <- as.matrix(
                (torch::torch_einsum("bij,bj->ij",
                  list(W_3d$detach(), c_mat_torch)) / c_total_2d)$cpu())
            }
          })
          best_epoch      <- epoch
        }
      }

      if (verbose &&
          (epoch == 1L || epoch %% report_every == 0L)) {
        eff_src_log <- if ((entropy_mu > 0 || adaptive) &&
                            !is.na(median_eff_src)) {
          sprintf(", eff_src=%.1f", median_eff_src)
        } else ""
        dual_log <- if (adaptive) {
          sprintf(", entropy_mu=%.4f", entropy_mu)
        } else ""
        quad_log <- if (adaptive && quad_val > 0) {
          sprintf(", quad=%s", format(round(quad_val), big.mark = ","))
        } else ""
        components <- sprintf(" [deviance=%s, barrier=%s, entropy=%s%s]",
          format(round(data_loss_val), big.mark = ","),
          format(round(barrier_val), big.mark = ","),
          format(round(entropy_val), big.mark = ","),
          quad_log)
        # Compute alpha stats on GPU (3 scalars) instead of transferring
        # the full alpha matrix to CPU for min/median/max.
        alpha_min_val <- as.numeric(alpha_all$min()$item())
        alpha_med_val <- as.numeric(alpha_all$median()$item())
        alpha_max_val <- as.numeric(alpha_all$max()$item())
        message(sprintf(paste0(
          "  Epoch %3d/%d: loss=%s%s, grad=%.2e, lr=%.1e, ",
          "alpha=[%.2f, %.2f, %.2f]%s%s"),
          epoch, max_epochs,
          format(round(total_loss_val), big.mark = ","),
          components,
          last_grad_norm,
          optimizer$param_groups[[1]]$lr,
          alpha_min_val, alpha_med_val, alpha_max_val,
          eff_src_log,
          dual_log
        ))
      }
    }  # end epoch loop

    # Extract final-epoch W for post-loop model selection (convergence
    # override, fallback paths).  Single transfer, amortized over all epochs.
    # Guard: if the last epoch was NaN (via `next`), W_agg may hold NaN
    # data — fall back to the best model's W (which may be NULL if no
    # good epoch).
    if (isTRUE(is.finite(total_loss_val))) {
      torch::with_no_grad({
        if (needs_W_3d || use_chunked) {
          # W_agg always computed in Pass 1 for chunked path; use directly.
          final_W <- as.matrix(W_agg$detach()$cpu())
        } else {
          final_W <- as.matrix(
            (torch::torch_einsum("bij,bj->ij",
              list(W_3d$detach(), c_mat_torch)) / c_total_2d)$cpu())
        }
      })
    } else {
      final_W <- best_W
    }
    final_alpha <- as.matrix(alpha_all$detach()$cpu())

    # Dual ascent model selection:
    # - Converged: the final epoch IS the saddle-point solution (theta and
    #   entropy_mu jointly stabilized).  Earlier models were optimized for
    #   wrong entropy_mu values and are not valid candidates.
    # - Non-converged but had near-target models: use best tracked model.
    # - Never reached near target: use final epoch as last resort.
    if (adaptive) {
      if (converged) {
        best_loss      <- total_loss_val
        best_data_loss <- data_loss_val
        best_alpha     <- final_alpha
        best_W         <- final_W
        best_eff_src   <- median_eff_src
        best_epoch     <- epoch
      } else if (isTRUE(!is.finite(best_data_loss))) {
        best_loss      <- total_loss_val
        best_data_loss <- data_loss_val
        best_alpha     <- final_alpha
        best_W         <- final_W
        best_eff_src   <- median_eff_src
        best_epoch     <- epoch
      }
    }

    # Non-adaptive fallback: if no epoch produced a finite loss (e.g.,
    # all NaN on MPS), use the last epoch's W to avoid returning NULL
    # and crashing downstream.
    if (!adaptive && !isTRUE(is.finite(best_loss))) {
      best_loss      <- total_loss_val
      best_data_loss <- data_loss_val
      best_alpha     <- final_alpha
      best_W         <- final_W
      best_epoch     <- epoch
    }

    if (adaptive && !converged && verbose) {
      eff_str <- if (!is.na(median_eff_src)) {
        sprintf("%.1f", median_eff_src)
      } else "NA"
      message(sprintf(
        paste0("  Note: dual ascent did not converge in %d epochs. ",
               "Final eff_src=%s vs target=%.1f. ",
               "Consider increasing max_epochs or adjusting target_eff_src."),
        epoch, eff_str, target_eff_src))
    }

    reported_eff_src <- if (adaptive && !is.na(best_eff_src)) {
      best_eff_src
    } else {
      median_eff_src
    }

    list(
      alpha           = best_alpha,   # n × ka matrix
      active_brackets = active,       # which brackets are active
      W               = best_W,       # n × m weight matrix
      value           = best_data_loss, # Poisson deviance
      loss            = best_loss,      # Full loss: deviance + barrier + entropy
      method          = paste0("pb_sgd_colnorm_", device),
      convergence     = if (converged) 0L else 1L,
      epochs          = epoch,
      steps           = step_counter,
      message         = sprintf(paste0(
        "PB-SGD colnorm on %s (%s), %d epochs (%d steps), ",
        "%d brackets, full-data gradient"),
        device, dtype, epoch, step_counter, ka),
      history             = epoch_losses[seq_len(epoch)],
      deviance_history    = deviance_history[seq_len(epoch)],
      barrier_history     = barrier_history[seq_len(epoch)],
      entropy_history     = entropy_history[seq_len(epoch)],
      best_epoch          = best_epoch,
      grad_norm_final     = last_grad_norm,
      grad_history        = grad_history[seq_len(epoch)],
      lr_history          = lr_history[seq_len(epoch)],
      mean_eff_sources    = reported_eff_src,
      entropy_mu_final    = entropy_mu,
      entropy_mu_history  = entropy_mu_history[seq_len(epoch)],
      target_eff_src      = target_eff_src
    )
  }, error = function(e) {
    stop(sprintf(
      "Torch optimization failed on device '%s': %s",
      device, e$message
    ), call. = FALSE)
  })

  result
}
