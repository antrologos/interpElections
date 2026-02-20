#' Find optimal decay parameters (alpha) for spatial interpolation
#'
#' Optimizes the per-zone decay parameters that minimize the squared error
#' between per-bracket Sinkhorn-balanced interpolated values and known
#' population counts. Uses Per-Bracket SGD (PB-SGD) with mini-batch sampling
#' and log-domain Sinkhorn inside torch autograd. Each demographic bracket
#' gets its own Sinkhorn transport; gradients flow through all unrolled
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
#' @param alpha_init Numeric vector of length n, or a single value to be
#'   recycled. Initial guess for alpha. Default: `rep(1, n)`.
#' @param batch_size Integer. Number of zones (rows) sampled per SGD step.
#'   For cities with `n <= batch_size`, the full batch is used. Default: 500.
#' @param sk_iter Integer. Number of log-domain Sinkhorn iterations per
#'   SGD step. Higher values give more accurate per-bracket transport but
#'   increase memory usage. Default: 15.
#' @param max_steps Integer. Total number of SGD steps. Default: 800.
#' @param lr_init Numeric. Initial ADAM learning rate. Halved at steps 200,
#'   400, and 600. Default: 0.05.
#' @param use_gpu Logical or NULL. If `TRUE`, use GPU (CUDA or MPS). If
#'   `FALSE`, use CPU. If `NULL` (default), reads the package option
#'   `interpElections.use_gpu` (set via [use_gpu()]).
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. Only used when GPU is enabled. Default: NULL (auto-detect).
#' @param dtype Character. Torch dtype: `"float32"` or `"float64"`. Default:
#'   `"float32"`. Float32 halves memory usage with negligible precision loss.
#' @param lower_bound Numeric. Lower bound for alpha values. Default: 0.01.
#'   Alpha is parameterized via scaled sigmoid internally, so this bound
#'   is always satisfied smoothly without clamping.
#' @param upper_bound Numeric. Upper bound for alpha values. Default: 20.
#'   Alpha is computed as `upper_bound * sigmoid(theta)`, so alpha is
#'   always in `(0, upper_bound)`. This prevents both corner solutions at
#'   the lower boundary and alpha explosion at the upper end.
#' @param convergence_tol Numeric. Relative change in EMA loss below which
#'   the optimizer considers the solution converged. Default: 1e-4.
#' @param patience Integer. Number of consecutive convergence checks (every
#'   50 steps) that must pass before early stopping. Default: 3.
#' @param offset Numeric. Value added to travel times before exponentiation.
#'   Default: 1.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#'
#' @return A list of class `"interpElections_optim"` with components:
#' \describe{
#'   \item{alpha}{Numeric vector. Optimal alpha values.}
#'   \item{value}{Numeric. Objective function value at optimum.}
#'   \item{method}{Character. Method used (e.g.,
#'     `"pb_sgd_sinkhorn_cpu"`, `"pb_sgd_sinkhorn_cuda"`).}
#'   \item{convergence}{Integer. 0 = success.}
#'   \item{iterations}{Number of SGD steps taken.}
#'   \item{elapsed}{`difftime` object. Wall-clock time.}
#'   \item{message}{Character. Additional information.}
#'   \item{history}{Numeric vector. Loss values at each step.}
#'   \item{grad_norm_final}{Numeric. Final gradient norm.}
#'   \item{row_targets}{Numeric vector. Row targets used for Sinkhorn.}
#'   \item{sk_iter}{Integer. Sinkhorn iterations per step.}
#'   \item{batch_size}{Integer. Mini-batch size used.}
#' }
#'
#' @details
#' The optimization requires the `torch` R package. Install it with
#' [setup_torch()] if not already available.
#'
#' Two execution paths:
#' \itemize{
#'   \item **CPU** (default): `use_gpu = FALSE` or `NULL`. Uses torch on CPU
#'     device. Fast for small/medium problems (< 2000 tracts).
#'   \item **GPU**: `use_gpu = TRUE`. Uses CUDA or MPS. Faster for large
#'     problems (> 2000 tracts).
#' }
#'
#' Both paths use PB-SGD: mini-batch ADAM with per-bracket log-domain
#' Sinkhorn. Gradients are computed via torch autograd through the unrolled
#' Sinkhorn iterations. GPU memory usage is bounded by
#' `ka * min(batch_size, n) * m * bytes_per_elem * (2 + 2 * sk_iter)`.
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
#' @seealso [use_gpu()] to toggle GPU globally, [sinkhorn_weights()] to
#'   build the final weight matrix, [sinkhorn_objective()] for the
#'   objective function, [setup_torch()] to install torch.
#'
#' @export
optimize_alpha <- function(
    time_matrix,
    pop_matrix,
    source_matrix,
    row_targets = NULL,
    alpha_init = NULL,
    batch_size = 500L,
    sk_iter = 15L,
    max_steps = 800L,
    lr_init = 0.05,
    use_gpu = NULL,
    device = NULL,
    dtype = "float32",
    lower_bound = 0.01,
    upper_bound = 20,
    convergence_tol = 1e-4,
    patience = 3L,
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

  # Auto-compute row_targets if not provided
  if (is.null(row_targets)) {
    pop_total <- rowSums(pop_matrix)
    row_targets <- pop_total / sum(pop_total) * m
  }
  if (length(row_targets) != n) {
    stop(sprintf("row_targets must have length %d (one per row of time_matrix), got %d",
                 n, length(row_targets)), call. = FALSE)
  }

  # Default alpha_init
  if (is.null(alpha_init)) {
    alpha_init <- rep(1, n)
  } else if (length(alpha_init) == 1) {
    alpha_init <- rep(alpha_init, n)
  }
  .validate_alpha(alpha_init, n)

  # Coerce and validate integer params
  batch_size <- as.integer(batch_size)
  sk_iter <- as.integer(sk_iter)
  max_steps <- as.integer(max_steps)
  if (is.na(sk_iter) || sk_iter < 1L) {
    stop("sk_iter must be a positive integer (>= 1)", call. = FALSE)
  }
  if (is.na(max_steps) || max_steps < 1L) {
    stop("max_steps must be a positive integer (>= 1)", call. = FALSE)
  }
  if (is.na(batch_size) || batch_size < 1L) {
    stop("batch_size must be a positive integer (>= 1)", call. = FALSE)
  }

  # Validate lr_init
  if (!is.numeric(lr_init) || length(lr_init) != 1 || !is.finite(lr_init) || lr_init <= 0) {
    stop("lr_init must be a single positive number", call. = FALSE)
  }

  # Validate bounds
  if (!is.numeric(lower_bound) || length(lower_bound) != 1 || !is.finite(lower_bound) ||
      lower_bound < 0) {
    stop("lower_bound must be a single non-negative number", call. = FALSE)
  }
  if (!is.numeric(upper_bound) || length(upper_bound) != 1 || !is.finite(upper_bound) ||
      upper_bound <= 0) {
    stop("upper_bound must be a single positive number", call. = FALSE)
  }
  if (lower_bound >= upper_bound) {
    stop(sprintf("lower_bound (%.4f) must be less than upper_bound (%.4f)",
                 lower_bound, upper_bound), call. = FALSE)
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
      warning("GPU requested but no GPU found; using CPU torch", call. = FALSE)
    }
  } else {
    resolved_device <- "cpu"
  }
  resolved_dtype <- dtype %||%
    getOption("interpElections.dtype", default = "float32")

  start_time <- Sys.time()

  raw <- .optimize_torch(
    time_matrix = t_adj,
    pop_matrix = pop_matrix,
    source_matrix = source_matrix,
    alpha_init = alpha_init,
    batch_size = batch_size,
    sk_iter = sk_iter,
    max_steps = max_steps,
    lr_init = lr_init,
    device = resolved_device,
    dtype = resolved_dtype,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    convergence_tol = convergence_tol,
    patience = as.integer(patience),
    verbose = verbose
  )

  elapsed <- Sys.time() - start_time

  # Post-optimization validation: replace non-finite alpha with initial values
  if (any(!is.finite(raw$alpha))) {
    n_bad <- sum(!is.finite(raw$alpha))
    warning(sprintf(
      "%d alpha values are non-finite after optimization; replacing with initial values",
      n_bad
    ), call. = FALSE)
    bad <- !is.finite(raw$alpha)
    raw$alpha[bad] <- alpha_init[bad]
  }

  # Recompute objective with convergence-based Sinkhorn for consistency
  # with the W that sinkhorn_weights() will produce
  final_value <- sinkhorn_objective(
    raw$alpha, t_adj, pop_matrix, source_matrix,
    row_targets, sk_iter = 1000L
  )

  result <- list(
    alpha = raw$alpha,
    value = final_value,
    method = raw$method,
    convergence = raw$convergence,
    iterations = raw$iterations,
    elapsed = elapsed,
    message = raw$message %||% "",
    history = raw$history %||% NULL,
    grad_norm_final = raw$grad_norm_final %||% NULL,
    row_targets = row_targets,
    sk_iter = sk_iter,
    batch_size = min(batch_size, nrow(t_adj))
  )
  class(result) <- "interpElections_optim"

  if (verbose) {
    conv_msg <- if (raw$convergence == 0L) " (converged)" else ""
    message(sprintf(
      "  Completed %d steps (%.1fs), objective=%s%s",
      result$iterations,
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
  cat(sprintf("  Alpha range: [%.3f, %.3f]\n", min(x$alpha), max(x$alpha)))
  cat(sprintf("  N tracts:    %d\n", length(x$alpha)))
  cat(sprintf("  Steps:       %d (batch=%d, sk_iter=%d)\n",
              x$iterations, x$batch_size %||% NA, x$sk_iter %||% NA))
  cat(sprintf("  Elapsed:     %.1f secs\n", as.numeric(x$elapsed, units = "secs")))
  invisible(x)
}
