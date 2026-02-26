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
#' @param ... **Deprecated**. Old-style individual parameters
#'   (`alpha_init`, `max_epochs`, `lr_init`, `use_gpu`, `device`, `dtype`,
#'   `convergence_tol`, `patience`, `barrier_mu`, `alpha_min`) are still
#'   accepted via `...` for backward compatibility, but will be removed
#'   in a future release. Use `optim = optim_control(...)` instead.
#'
#' @return A list of class `"interpElections_optim"` with components:
#' \describe{
#'   \item{alpha}{Numeric matrix \[n x k\]. Optimal per-tract-per-bracket
#'     decay parameters. Each row is a census tract, each column is a
#'     demographic bracket. Inactive brackets (zero population or voters)
#'     are filled with 1.}
#'   \item{value}{Numeric. Objective function value at optimum (Poisson
#'     deviance).}
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
#' each epoch is the true objective evaluated on the full dataset.
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
    verbose = TRUE,
    ...
) {
  # --- Backward compatibility: detect old-style params in ... ---
  dots <- list(...)
  optim_param_names <- c("alpha_init", "max_epochs", "lr_init", "use_gpu",
                         "device", "dtype", "convergence_tol", "patience",
                         "barrier_mu", "alpha_min")
  old_params <- intersect(names(dots), optim_param_names)
  if (length(old_params) > 0) {
    warning(
      "Passing optimization parameters directly is deprecated.\n",
      "Use optim = optim_control(...) instead.\n",
      "Deprecated parameters: ", paste(old_params, collapse = ", "),
      call. = FALSE
    )
    # Override control object fields with old-style params
    ctrl_list <- unclass(optim)
    ctrl_list[old_params] <- dots[old_params]
    optim <- do.call(optim_control, ctrl_list)
  }

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
    row_targets         = row_targets,
    barrier_mu          = barrier_mu,
    alpha_min           = alpha_min,
    kernel              = kernel
  )
  class(result) <- "interpElections_optim"

  if (verbose) {
    conv_msg <- if (raw$convergence == 0L) " (converged)" else ""
    message(sprintf(
      "  Completed %d epochs (%.1fs), objective=%s%s",
      result$epochs,
      as.numeric(elapsed, units = "secs"),
      format(round(result$value), big.mark = ","),
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
  cat(sprintf("  Objective:   %.4f\n", x$value))
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
# objective evaluated on the full dataset.

.optimize_torch <- function(time_matrix, pop_matrix, source_matrix,
                             alpha_init,
                             max_epochs, lr_init, device, dtype,
                             convergence_tol, patience,
                             barrier_mu, alpha_min,
                             kernel,
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
               kernel,
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

  if (verbose) {
    message(sprintf(paste0(
      "  PB-SGD colnorm (%s, %s, %s kernel): full-data gradient, barrier_mu=%.1f, ",
      "loss=poisson, alpha_min=%.1f, max %d epochs"),
      device, dtype, kernel, barrier_mu, alpha_min,
      max_epochs))
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

    optimizer <- torch::optim_adam(
      list(theta_torch), lr = lr_init, betas = c(0.9, 0.99))

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

    best_epoch_loss  <- Inf
    best_alpha <- as.matrix(
      alpha_fn(theta_torch)$detach()$cpu())            # (n, ka)
    best_W     <- NULL                                   # n × m R matrix
    epoch_losses     <- numeric(max_epochs)
    lr_history       <- numeric(max_epochs)
    last_grad_norm   <- NA_real_
    step_counter     <- 0L
    epoch            <- 0L

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

      # V_hat[i,b] = sum_j W[b,i,j] * c[b,j]
      log_c_3d <- log_V_b_torch$unsqueeze(2L)        # (ka, 1, m)
      V_hat_all <- torch::torch_exp(
        torch::torch_logsumexp(
          log_W_3d + log_c_3d, dim = 3L))$t()        # (n, ka)

      # Full-data loss (exact, no scaling needed)
      data_loss <- compute_data_loss(V_hat_all, p_active_torch, 1.0)

      # Capture epoch loss before adding barrier
      epoch_loss_val <- as.numeric(data_loss$item())

      # Log-barrier penalty on ALL tracts
      if (barrier_mu > 0) {
        V_hat_total <- V_hat_all$sum(dim = 2L)        # (n,)
        barrier <- -barrier_mu *
          torch::torch_log(
            torch::torch_clamp(V_hat_total, min = 1e-30))$sum()
        loss <- data_loss + barrier
      } else {
        loss <- data_loss
      }

      loss$backward()

      # Capture pre-clip gradient norm, then clip
      last_grad_norm <- tryCatch(
        as.numeric(theta_torch$grad$norm()$cpu()),
        error = function(e) NA_real_
      )
      torch::nn_utils_clip_grad_norm_(
        list(theta_torch), max_norm = 1.0)

      optimizer$step()
      step_counter <- step_counter + 1L

      # Extract W and alpha candidates (no extra forward pass needed)
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

      epoch_losses[epoch] <- epoch_loss_val

      # Step the LR scheduler with the true epoch loss
      scheduler$step(epoch_loss_val)

      grad_history[epoch] <- last_grad_norm
      lr_history[epoch]   <- optimizer$param_groups[[1]]$lr

      # --- Convergence check ---
      # Converge when LR has been reduced to min_lr AND no improvement
      # for `patience` consecutive epochs at that LR.
      # NOTE: must check BEFORE updating best_epoch_loss, otherwise the
      # comparison (epoch_loss_val < best_epoch_loss - tol*|best|) is
      # always false after a best-loss update.
      current_lr <- optimizer$param_groups[[1]]$lr
      lr_at_min  <- (current_lr <= min_lr * 1.01)

      if (is.finite(epoch_loss_val) && epoch >= 5L) {
        if (epoch_loss_val < best_epoch_loss -
              convergence_tol * abs(best_epoch_loss)) {
          patience_counter <- 0L
        } else {
          patience_counter <- patience_counter + 1L
        }

        if (lr_at_min && patience_counter >= patience) {
          converged <- TRUE
          if (verbose) {
            message(sprintf(paste0(
              "  Converged at epoch %d (loss=%s, lr=%.1e, ",
              "no improvement for %d epochs)"),
              epoch,
              format(round(epoch_loss_val), big.mark = ","),
              current_lr, patience))
          }
          break
        }
      }

      # Track best alpha by true epoch loss (after convergence check)
      if (is.finite(epoch_loss_val) &&
          epoch_loss_val < best_epoch_loss) {
        best_epoch_loss <- epoch_loss_val
        best_alpha <- alpha_candidate
        best_W <- W_candidate
      }

      if (verbose &&
          (epoch == 1L || epoch %% report_every == 0L)) {
        message(sprintf(paste0(
          "  Epoch %3d/%d: loss=%s, grad=%.2e, lr=%.1e, ",
          "alpha=[%.2f, %.2f, %.2f]"),
          epoch, max_epochs,
          format(round(epoch_loss_val), big.mark = ","),
          last_grad_norm,
          optimizer$param_groups[[1]]$lr,
          min(alpha_candidate), stats::median(alpha_candidate),
          max(alpha_candidate)
        ))
      }
    }  # end epoch loop

    list(
      alpha           = best_alpha,   # n × ka matrix
      active_brackets = active,       # which brackets are active
      W               = best_W,       # n × m weight matrix
      value           = best_epoch_loss,
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
      lr_history          = lr_history[seq_len(epoch)]
    )
  }, error = function(e) {
    stop(sprintf(
      "Torch optimization failed on device '%s': %s",
      device, e$message
    ), call. = FALSE)
  })

  result
}
