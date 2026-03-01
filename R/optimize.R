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
#'   \item{convergence}{Integer. 0 = early-stopped (improvement plateau
#'     detected); 1 = stopped at max_epochs.}
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
  dual_eta        <- optim$dual_eta %||% 0.05
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

  raw <- .optimize_torch(
    time_matrix    = t_adj,
    pop_matrix     = pop_matrix,
    source_matrix  = source_matrix,
    alpha_init     = alpha_init,
    max_epochs     = max_epochs,
    lr_init        = lr_init,
    device         = resolved_device,
    dtype          = resolved_dtype,
    convergence_tol = convergence_tol,
    patience       = as.integer(patience),
    barrier_mu     = barrier_mu,
    alpha_min      = alpha_min,
    kernel         = kernel,
    entropy_mu     = entropy_mu,
    target_eff_src = target_eff_src,
    dual_eta       = dual_eta,
    verbose        = verbose
  )

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
                             dual_eta = 0.05,
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
               verbose, pkg_path) {
        Sys.setenv(INTERPELECTIONS_SUBPROCESS = "1")
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

  # LR schedule: ReduceLROnPlateau — halve LR when epoch loss plateaus.
  min_lr <- lr_init * 1e-3

  # Verbose: print ~20 lines regardless of epoch count
  report_every <- max(1L, max_epochs %/% 20L)

  # --- Memory safety check (GPU only) ---
  bytes_per_elem <- if (dtype == "float32") 4 else 8
  # Dominant cost: log_K_3d (ka×n×m)
  estimated_mb   <- (2.0 * as.double(ka) * n * m) * bytes_per_elem / 1e6

  if (grepl("cuda", device, fixed = TRUE)) {
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
  if (adaptive && entropy_mu <= 0) entropy_mu <- 0.01

  if (adaptive && target_eff_src > m * 0.8) {
    warning(sprintf(paste0(
      "target_eff_src (%.1f) is close to or above the number of stations (%d). ",
      "Maximum achievable eff_src with uniform weights is %d. ",
      "Consider a smaller target."),
      target_eff_src, m, m), call. = FALSE)
  }

  if (verbose) {
    if (adaptive) {
      entropy_msg <- sprintf(", target_eff_src=%.1f (dual ascent)", target_eff_src)
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

    # Mask tensor for unreachable pairs (TRUE = reachable)
    if (has_na) {
      mask_torch <- torch::torch_tensor(
        !na_mask, device = device, dtype = torch::torch_bool())
      gpu_tensors$mask <- mask_torch
    }

    # --- Pre-computations ---
    p_mat_active <- P_cpu[, active, drop = FALSE]   # n × ka

    log_V_b_torch <- torch::torch_tensor(
      log(pmax(c_mat, 1e-30)),
      device = device, dtype = torch_dtype)          # (ka, m)

    # Active-bracket population tensor for loss computation
    p_active_torch <- torch::torch_tensor(
      p_mat_active, device = device, dtype = torch_dtype)  # (n, ka)

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
    alpha_fn <- function(th) alpha_min_t + torch::nnf_softplus(th)

    # Loss computation: Poisson deviance
    compute_data_loss <- function(V_hat, P, scale_factor) {
      V_clamped <- torch::torch_clamp(V_hat, min = 1e-30)
      P_safe <- torch::torch_where(
        P > 0, P, torch::torch_ones_like(P))
      log_ratio <- torch::torch_where(
        P > 0,
        torch::torch_log(P_safe) - torch::torch_log(V_clamped),
        torch::torch_zeros_like(P))
      dev <- 2 * (V_clamped - P + P * log_ratio)
      dev$sum() * scale_factor
    }

    gpu_tensors$t_basis <- t_basis
    gpu_tensors$p_act   <- p_active_torch
    gpu_tensors$theta   <- theta_torch
    gpu_tensors$log_V_b <- log_V_b_torch

    optim_params <- list(theta_torch)
    optimizer <- torch::optim_adam(
      optim_params, lr = lr_init, betas = c(0.9, 0.99))

    scheduler <- torch::lr_reduce_on_plateau(
      optimizer,
      mode       = "min",
      factor     = 0.5,
      patience   = as.integer(patience * 2L),
      threshold  = convergence_tol,
      threshold_mode = "rel",
      min_lr     = min_lr,
      verbose    = FALSE
    )

    best_loss        <- Inf
    best_data_loss   <- Inf
    best_eff_src     <- NA_real_
    best_alpha <- as.matrix(
      alpha_fn(theta_torch)$detach()$cpu())            # (n, ka)
    best_W     <- NULL                                   # n × m R matrix
    epoch_losses     <- numeric(max_epochs)
    lr_history       <- numeric(max_epochs)
    last_grad_norm   <- NA_real_
    step_counter     <- 0L
    epoch            <- 0L

    mean_eff_src     <- NA_real_
    entropy_mu_history <- numeric(max_epochs)

    # Convergence tracking
    patience_counter <- 0L
    converged        <- FALSE

    grad_history <- numeric(max_epochs)

    for (epoch in seq_len(max_epochs)) {

      # --- Full-data gradient step ---
      # Column normalization requires all n tracts for column sums,
      # so the forward pass is O(ka*n*m) regardless of batch size.
      # Using full-data loss gives exact gradients at the same cost.
      optimizer$zero_grad()

      alpha_all <- alpha_fn(theta_torch)              # (n, ka)
      log_K_3d_full <- -alpha_all$t()$unsqueeze(3L) *
        t_basis$unsqueeze(1L)                          # (ka, n, m)

      # Mask unreachable pairs: set log_K = -Inf so exp(log_K) = 0
      if (has_na) {
        log_K_3d_full <- torch::torch_where(
          mask_torch$unsqueeze(1L), log_K_3d_full,
          torch::torch_tensor(-Inf, device = device,
                              dtype = torch_dtype))
      }

      # Column normalization in log-space
      log_col_sum_full <- torch::torch_logsumexp(
        log_K_3d_full, dim = 2L)                      # (ka, m)
      log_W_3d <- log_K_3d_full -
        log_col_sum_full$unsqueeze(2L)                # (ka, n, m)

      # Handle all-unreachable columns: -Inf - (-Inf) = NaN -> -Inf
      if (has_na) {
        log_W_3d <- torch::torch_where(
          torch::torch_isnan(log_W_3d),
          torch::torch_tensor(-Inf, device = device,
                              dtype = torch_dtype),
          log_W_3d)
      }

      # Voter counts per bracket per station (needed for V_hat and entropy)
      log_c_3d <- log_V_b_torch$unsqueeze(2L)        # (ka, 1, m)

      # --- Entropy penalty on AGGREGATED weights ---
      # Penalize the entropy of the aggregated weight matrix (voter-weighted
      # mixture across brackets).  This is what weight_summary() reports and
      # what the user sees.  Computing on the aggregated W (instead of per-
      # bracket) ensures the gradient directly targets the metric that dual
      # ascent steers, eliminating the Jensen's-inequality gap that caused
      # entropy_mu instability with per-bracket entropy.
      if (entropy_mu > 0 || adaptive) {
        # Aggregate per-bracket column-normalized weights, weighted by
        # station voter counts c[b,j]
        #
        # Mask unreachable pairs: log_W_3d has -Inf for unreachable (i,j)
        # entries (from NaN-to-Inf cleanup).  logsumexp(-Inf, dim=1)
        # backward computes softmax = 0/0 = NaN, poisoning gradients.
        # torch_where routes unreachable entries to a detached finite fill
        # so logsumexp backward gets well-defined softmax; gradients for
        # unreachable entries flow to the dead-end fill (discarded),
        # leaving reachable gradients untouched.
        if (has_na) {
          reachable_3d <- mask_torch$unsqueeze(1L)                         # (1, n, m)
          fill_val <- torch::torch_tensor(
            -1e20, device = device, dtype = torch_dtype)
          log_W_3d_ent <- torch::torch_where(reachable_3d, log_W_3d, fill_val)
        } else {
          log_W_3d_ent <- log_W_3d
        }
        log_T_ent <- log_W_3d_ent + log_c_3d                              # (ka, n, m)
        log_T_agg <- torch::torch_logsumexp(log_T_ent, dim = 1L)          # (n, m)
        log_c_agg <- torch::torch_logsumexp(log_V_b_torch, dim = 1L)      # (m,)
        log_W_agg <- log_T_agg - log_c_agg$unsqueeze(1L)                  # (n, m)

        # Row-normalize for entropy.  Clamp for NaN-free gradient.
        log_W_agg_c <- torch::torch_clamp(log_W_agg, min = -1e20)
        log_p_agg <- log_W_agg_c -
          torch::torch_logsumexp(log_W_agg_c, dim = 2L, keepdim = TRUE)   # (n, m)

        # H[i] = -sum_j p[i,j] * log(p[i,j])
        H_agg_torch <- -(
          torch::torch_exp(log_p_agg) * log_p_agg)$sum(dim = 2L)          # (n,)

        if (entropy_mu > 0) {
          entropy_penalty <- entropy_mu * H_agg_torch$mean()
        } else {
          entropy_penalty <- 0
        }

        # eff_src = exp(median entropy) — matches weight_summary()
        mean_eff_src <- as.numeric(
          torch::torch_exp(H_agg_torch$median())$item())

      } else {
        entropy_penalty <- 0
      }

      # V_hat[i,b] = sum_j W[b,i,j] * c[b,j]
      V_hat_all <- torch::torch_exp(
        torch::torch_logsumexp(
          log_W_3d + log_c_3d, dim = 3L))$t()        # (n, ka)

      # Full-data loss (exact, no scaling needed)
      data_loss <- compute_data_loss(V_hat_all, p_active_torch, 1.0)
      data_loss_val <- as.numeric(data_loss$item())


      # Barrier penalty: log-barrier prevents zero-voter tracts
      barrier_val <- 0
      if (barrier_mu > 0) {
        V_hat_total <- V_hat_all$sum(dim = 2L)        # (n,)
        barrier <- -barrier_mu *
          torch::torch_log(
            torch::torch_clamp(V_hat_total, min = 1e-30))$sum()
        barrier_val <- as.numeric(barrier$item())

      }

      # Entropy penalty value for logging

      entropy_val <- if (entropy_mu > 0) as.numeric(entropy_penalty$item()) else 0

      # Total loss = deviance + barrier + entropy. Always.
      loss <- data_loss
      if (barrier_mu > 0) loss <- loss + barrier
      if (entropy_mu > 0) loss <- loss + entropy_penalty
      total_loss_val <- data_loss_val + barrier_val + entropy_val


      loss$backward()

      # Capture pre-clip gradient norm, then clip
      last_grad_norm <- tryCatch(
        as.numeric(theta_torch$grad$norm()$cpu()),
        error = function(e) NA_real_
      )
      torch::nn_utils_clip_grad_norm_(
        optim_params, max_norm = 1.0)

      optimizer$step()
      step_counter <- step_counter + 1L

      # Extract W, alpha candidates (no extra forward pass needed).
      # All come from pre-step tensors (alpha_all, log_W_3d
      # were computed before optimizer$step()).
      # Note: mean_eff_src is computed above in the differentiable
      # entropy block (aggregated entropy on W_agg).
      torch::with_no_grad({
        log_T_cn <- log_W_3d$detach() + log_c_3d     # (ka, n, m)
        log_c_agg_cn <- torch::torch_logsumexp(
          log_V_b_torch, dim = 1L)                    # (m,)
        log_T_agg_cn <- torch::torch_logsumexp(
          log_T_cn, dim = 1L)                         # (n, m)
        W_candidate <- as.matrix(torch::torch_exp(
          log_T_agg_cn - log_c_agg_cn$unsqueeze(1L))$cpu())
        alpha_candidate <- as.matrix(alpha_all$detach()$cpu())
      })

      # --- Dual ascent: adapt entropy_mu to reach target eff_src ---
      if (adaptive && !is.na(mean_eff_src) && mean_eff_src > 0) {
        ratio <- mean_eff_src / target_eff_src
        entropy_mu <- entropy_mu * ratio ^ dual_eta
        entropy_mu <- max(entropy_mu, 1e-8)
        entropy_mu <- min(entropy_mu, 1e6)

      }
      entropy_mu_history[epoch] <- entropy_mu

      epoch_losses[epoch] <- total_loss_val

      # LR scheduler: when dual ascent is active, track deviance only
      # (entropy_mu changes make total_loss unstable for plateau detection)
      if (adaptive) {
        scheduler$step(data_loss_val)
      } else {
        scheduler$step(total_loss_val)
      }

      grad_history[epoch] <- last_grad_norm
      lr_history[epoch]   <- optimizer$param_groups[[1]]$lr

      # --- Convergence check ---
      # Converge when LR has been reduced to min_lr AND no improvement
      # for `patience` consecutive epochs at that LR.
      # In dual ascent mode, also require eff_src near target.
      current_lr <- optimizer$param_groups[[1]]$lr
      lr_at_min  <- (current_lr <= min_lr * 1.01)

      # For dual ascent: track deviance for patience (stable component)
      conv_metric <- if (adaptive) data_loss_val else total_loss_val

      eff_near_target <- if (adaptive && !is.na(mean_eff_src)) {
        abs(mean_eff_src - target_eff_src) / target_eff_src < 0.2
      } else {
        TRUE
      }

      if (is.finite(conv_metric) && epoch >= 5L) {
        if (!eff_near_target) {
          # Far from target: can't converge, reset patience
          patience_counter <- 0L
        } else if (is.finite(best_data_loss) &&
                   conv_metric < best_data_loss -
                     convergence_tol * abs(best_data_loss)) {
          # Near target and deviance improving: reset patience
          patience_counter <- 0L
        } else if (eff_near_target) {
          # Near target, no improvement: count patience
          patience_counter <- patience_counter + 1L
        }

        if (lr_at_min && patience_counter >= patience && eff_near_target) {
          converged <- TRUE
          if (verbose) {
            eff_conv <- if ((entropy_mu > 0 || adaptive) &&
                            !is.na(mean_eff_src)) {
              sprintf(", eff_src=%.1f", mean_eff_src)
            } else ""
            dual_conv <- if (adaptive) {
              sprintf(", entropy_mu=%.4f", entropy_mu)
            } else ""
            message(sprintf(paste0(
              "  Converged at epoch %d (loss=%s",
              " [deviance=%s, barrier=%s, entropy=%s]%s%s, ",
              "lr=%.1e, no improvement for %d epochs)"),
              epoch,
              format(round(total_loss_val), big.mark = ","),
              format(round(data_loss_val), big.mark = ","),
              format(round(barrier_val), big.mark = ","),
              format(round(entropy_val), big.mark = ","),
              eff_conv,
              dual_conv,
              current_lr, patience))
          }
          break
        }
      }

      # Best model tracking.
      # In dual ascent: the converged state IS the saddle-point solution.
      # Models from earlier epochs were optimized for wrong entropy_mu values.
      # Track best near-target model only as fallback for non-convergence.
      # At convergence, we override with the final epoch (see after loop).
      if (adaptive) {
        eff_ok <- !is.na(mean_eff_src) &&
          abs(mean_eff_src - target_eff_src) / target_eff_src < 0.2
        if (eff_ok && is.finite(data_loss_val) &&
            data_loss_val < best_data_loss) {
          best_loss       <- total_loss_val
          best_data_loss  <- data_loss_val
          best_alpha      <- alpha_candidate
          best_W          <- W_candidate
          best_eff_src    <- mean_eff_src
        }
      } else {
        # Non-adaptive: fixed objective, standard best-model tracking
        if (is.finite(total_loss_val) &&
            total_loss_val < best_loss) {
          best_loss       <- total_loss_val
          best_data_loss  <- data_loss_val
          best_alpha      <- alpha_candidate
          best_W          <- W_candidate
        }
      }

      if (verbose &&
          (epoch == 1L || epoch %% report_every == 0L)) {
        eff_src_log <- if ((entropy_mu > 0 || adaptive) &&
                            !is.na(mean_eff_src)) {
          sprintf(", eff_src=%.1f", mean_eff_src)
        } else ""
        dual_log <- if (adaptive) {
          sprintf(", entropy_mu=%.4f", entropy_mu)
        } else ""
        components <- sprintf(" [deviance=%s, barrier=%s, entropy=%s]",
          format(round(data_loss_val), big.mark = ","),
          format(round(barrier_val), big.mark = ","),
          format(round(entropy_val), big.mark = ","))
        message(sprintf(paste0(
          "  Epoch %3d/%d: loss=%s%s, grad=%.2e, lr=%.1e, ",
          "alpha=[%.2f, %.2f, %.2f]%s%s"),
          epoch, max_epochs,
          format(round(total_loss_val), big.mark = ","),
          components,
          last_grad_norm,
          optimizer$param_groups[[1]]$lr,
          min(alpha_candidate), stats::median(alpha_candidate),
          max(alpha_candidate),
          eff_src_log,
          dual_log
        ))
      }
    }  # end epoch loop

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
        best_alpha     <- alpha_candidate
        best_W         <- W_candidate
        best_eff_src   <- mean_eff_src
      } else if (!is.finite(best_data_loss)) {
        best_loss      <- total_loss_val
        best_data_loss <- data_loss_val
        best_alpha     <- alpha_candidate
        best_W         <- W_candidate
        best_eff_src   <- mean_eff_src
      }
    }

    if (adaptive && !converged && verbose) {
      eff_str <- if (!is.na(mean_eff_src)) {
        sprintf("%.1f", mean_eff_src)
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
      mean_eff_src
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
