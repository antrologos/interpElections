#' Find optimal decay parameters (alpha) for spatial interpolation
#'
#' Optimizes the per-tract-per-bracket decay parameters that minimize the
#' squared error between Sinkhorn-balanced interpolated values and known
#' population counts. Uses Per-Bracket SGD (PB-SGD) with mini-batch sampling
#' and log-domain 3D Sinkhorn inside torch autograd. Each demographic bracket
#' gets its own per-tract kernel; gradients flow through all unrolled
#' iterations. Works on both CPU and GPU (CUDA/MPS).
#'
#' @param time_matrix Numeric matrix \[n x m\]. Raw travel times.
#'   Rows = target zones, columns = source points.
#' @param pop_matrix Numeric matrix \[n x k\]. Known population per zone,
#'   with k demographic groups as columns.
#' @param source_matrix Numeric matrix \[m x k\]. Known counts at source
#'   points (e.g., registered voters by age group).
#' @param row_targets Numeric vector of length n, or NULL. Target row sums
#'   for Sinkhorn balancing. Each element specifies how much weight a zone
#'   should attract, proportional to its share of total population. If NULL
#'   (default), auto-computed as `rowSums(pop_matrix) / sum(pop_matrix) * m`.
#' @param alpha_init Numeric scalar, vector of length n, or matrix \[n x k\].
#'   Initial guess for alpha. A scalar is recycled to all n tracts and k
#'   brackets. A vector of length n is recycled across brackets. Default: 1.
#' @param batch_size Integer. Number of zones (rows) sampled per SGD step.
#'   For cities with `n <= batch_size`, the full batch is used. Default: 500.
#' @param sk_iter Integer. Maximum number of log-domain Sinkhorn iterations
#'   per SGD step. The Sinkhorn loop stops early if the dual variables
#'   converge within `sk_tol`. Default: 100.
#' @param sk_tol Numeric. Convergence tolerance for Sinkhorn iterations.
#'   Iteration stops when the maximum absolute change in the log-domain
#'   row dual variable falls below `sk_tol`. A warning is issued if
#'   Sinkhorn does not converge within `sk_iter` iterations. Default: 1e-6.
#' @param max_epochs Integer. Maximum number of epochs (full passes through
#'   all tracts). The optimizer may stop earlier if convergence is detected.
#'   Default: 200.
#' @param lr_init Numeric. Initial ADAM learning rate. Reduced automatically
#'   via ReduceLROnPlateau when the epoch loss plateaus. Default: 0.05.
#' @param use_gpu Logical or NULL. If `TRUE`, use GPU (CUDA or MPS). If
#'   `FALSE`, use CPU. If `NULL` (default), reads the package option
#'   `interpElections.use_gpu` (set via [use_gpu()]).
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. Only used when GPU is enabled. Default: NULL (auto-detect).
#' @param dtype Character. Torch dtype: `"float32"` or `"float64"`. Default:
#'   `"float32"`. Float32 halves memory usage with negligible precision loss.
#' @param convergence_tol Numeric. Relative change in epoch loss below which
#'   the optimizer considers the solution converged. Default: 1e-4.
#' @param patience Integer. Number of consecutive epochs with no improvement
#'   (at minimum learning rate) before early stopping. The LR scheduler
#'   uses `2 * patience` as its own patience. Default: 5.
#' @param method Character. Normalization method: `"colnorm"` (column-only
#'   normalization with log-barrier penalty) or `"sinkhorn"` (3D Sinkhorn
#'   IPF with shared row constraint). Column normalization preserves source
#'   conservation (column sums = 1) without constraining row sums, giving
#'   alpha more freedom to control spatial weight patterns. Default:
#'   `"colnorm"`.
#' @param barrier_mu Numeric. Strength of the log-barrier penalty that
#'   prevents any census tract from receiving zero predicted voters. Only
#'   used when `method = "colnorm"`. The penalty term is
#'   `-barrier_mu * sum(log(V_hat_total))`. Set to 0 to disable.
#'   Default: 10.
#' @param offset Numeric. Value added to travel times before exponentiation.
#'   Default: 1.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#'
#' @return A list of class `"interpElections_optim"` with components:
#' \describe{
#'   \item{alpha}{Numeric matrix \[n x k\]. Optimal per-tract-per-bracket
#'     decay parameters. Each row is a census tract, each column is a
#'     demographic bracket. Inactive brackets (zero population or voters)
#'     are filled with 1.}
#'   \item{value}{Numeric. Objective function value at optimum (sum of
#'     squared errors across all tracts and active brackets).}
#'   \item{W}{Numeric matrix \[n x m\]. Sinkhorn-balanced weight matrix
#'     from the best-epoch transport plan. Use directly for interpolation
#'     via `W \%*\% data`. Column sums are approximately 1.}
#'   \item{method}{Character. Method used (e.g.,
#'     `"pb_sgd_sinkhorn_cpu"`, `"pb_sgd_sinkhorn_cuda"`).}
#'   \item{convergence}{Integer. 0 = early-stopped (improvement plateau
#'     detected); 1 = stopped at max_epochs.}
#'   \item{epochs}{Integer. Number of epochs completed.}
#'   \item{steps}{Integer. Total number of SGD gradient steps.}
#'   \item{elapsed}{`difftime` object. Wall-clock time.}
#'   \item{message}{Character. Additional information.}
#'   \item{history}{Numeric vector. Full-dataset loss at each epoch.}
#'   \item{grad_norm_final}{Numeric. Final gradient norm (theta-space).}
#'   \item{grad_history}{Numeric vector. Gradient norm (theta-space) after
#'     the last mini-batch of each epoch.}
#'   \item{lr_history}{Numeric vector. Learning rate at each epoch.}
#'   \item{n_batches_per_epoch}{Integer. Number of mini-batches per epoch
#'     (`ceiling(n / batch_size)`).}
#'   \item{row_targets}{Numeric vector. Row targets used for Sinkhorn.}
#'   \item{sk_iter}{Integer. Maximum Sinkhorn iterations per step.}
#'   \item{sk_tol}{Numeric. Sinkhorn convergence tolerance.}
#'   \item{batch_size}{Integer. Mini-batch size used.}
#' }
#'
#' @details
#' The optimization requires the `torch` R package. Install it with
#' [setup_torch()] if not already available.
#'
#' **Parameterization**: alpha\[i,b\] is reparameterized as
#' `alpha = exp(theta)` with `theta` unconstrained. This avoids gradient
#' death at any boundary and removes the need for projected gradient
#' descent or clamping. Alpha is always strictly positive.
#'
#' **Epoch structure**: Each epoch is one full shuffled pass through all
#' n tracts, divided into mini-batches. The loss reported at each epoch
#' is the true objective evaluated on the full dataset — not a noisy
#' mini-batch estimate.
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
#' `2 * ka * min(batch_size, n) * m * bytes_per_elem`.
#'
#' @examples
#' \dontrun{
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
#' pop <- matrix(c(100, 200), nrow = 2)
#' src <- matrix(c(80, 120, 100), nrow = 3)
#' result <- optimize_alpha(tt, pop, src, verbose = FALSE)
#' result$alpha
#' }
#'
#' @seealso [use_gpu()] to toggle GPU globally, [compute_weight_matrix()]
#'   to rebuild the weight matrix from pre-computed alpha,
#'   [setup_torch()] to install torch.
#'
#' @export
optimize_alpha <- function(
    time_matrix,
    pop_matrix,
    source_matrix,
    row_targets = NULL,
    alpha_init = NULL,
    batch_size = 500L,
    sk_iter = 100L,
    sk_tol = 1e-6,
    max_epochs = 200L,
    lr_init = 0.05,
    use_gpu = NULL,
    device = NULL,
    dtype = "float32",
    convergence_tol = 1e-4,
    patience = 5L,
    method = c("colnorm", "sinkhorn"),
    barrier_mu = 10,
    offset = 1,
    verbose = TRUE
) {
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

  # Apply offset first (needed for positivity in validate)
  t_adj <- .apply_offset(time_matrix, offset)

  # Validate offset-adjusted matrices
  .validate_matrices(t_adj, pop_matrix, source_matrix)

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
  if (is.null(alpha_init)) {
    alpha_init <- 1  # scalar → recycled inside .optimize_torch
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
  batch_size <- as.integer(batch_size)
  sk_iter    <- as.integer(sk_iter)
  max_epochs <- as.integer(max_epochs)
  if (is.na(sk_iter) || sk_iter < 1L) {
    stop("sk_iter must be a positive integer (>= 1)", call. = FALSE)
  }
  if (!is.numeric(sk_tol) || length(sk_tol) != 1 ||
      !is.finite(sk_tol) || sk_tol <= 0) {
    stop("sk_tol must be a single positive number", call. = FALSE)
  }
  if (is.na(max_epochs) || max_epochs < 1L) {
    stop("max_epochs must be a positive integer (>= 1)", call. = FALSE)
  }
  if (is.na(batch_size) || batch_size < 1L) {
    stop("batch_size must be a positive integer (>= 1)", call. = FALSE)
  }

  # Validate lr_init
  if (!is.numeric(lr_init) || length(lr_init) != 1 ||
      !is.finite(lr_init) || lr_init <= 0) {
    stop("lr_init must be a single positive number", call. = FALSE)
  }

  # Validate method and barrier_mu
  method <- match.arg(method)
  if (!is.numeric(barrier_mu) || length(barrier_mu) != 1 ||
      !is.finite(barrier_mu) || barrier_mu < 0) {
    stop("barrier_mu must be a single non-negative number", call. = FALSE)
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
    batch_size     = batch_size,
    sk_iter        = sk_iter,
    sk_tol         = sk_tol,
    max_epochs     = max_epochs,
    lr_init        = lr_init,
    device         = resolved_device,
    dtype          = resolved_dtype,
    convergence_tol = convergence_tol,
    patience       = as.integer(patience),
    method         = method,
    barrier_mu     = barrier_mu,
    verbose        = verbose
  )

  elapsed <- Sys.time() - start_time

  # Expand active-bracket alpha (n × ka) to full alpha (n × k).
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
    n_batches_per_epoch = raw$n_batches_per_epoch %||% NULL,
    row_targets         = row_targets,
    sk_iter             = sk_iter,
    sk_tol              = sk_tol,
    batch_size          = min(batch_size, nrow(t_adj)),
    method_type         = method,
    barrier_mu          = barrier_mu
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
  cat(sprintf("  Epochs:      %d (batch=%d, sk_iter=%d, sk_tol=%s)\n",
              x$epochs %||% NA,
              x$batch_size %||% NA, x$sk_iter %||% NA,
              format(x$sk_tol %||% NA, scientific = TRUE)))
  cat(sprintf("  Elapsed:     %.1f secs\n",
              as.numeric(x$elapsed, units = "secs")))
  invisible(x)
}
