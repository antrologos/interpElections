#' Find optimal decay parameters (alpha) for spatial interpolation
#'
#' Optimizes the per-zone decay parameters that minimize the squared error
#' between Sinkhorn-balanced interpolated values and known population counts.
#' Uses torch autograd for gradient computation with ADAM optimizer on both
#' CPU and GPU.
#'
#' The weight matrix is balanced via log-domain Sinkhorn iterations
#' (row sums proportional to population, column sums = 1) before computing
#' the calibration loss. Gradients are obtained by differentiating through
#' the unrolled Sinkhorn iterations via torch autograd.
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
#' @param sinkhorn_iter Integer. Number of Sinkhorn iterations per objective
#'   evaluation during optimization. Higher values give more accurate balancing
#'   but are slower. Default: 5 (sufficient for optimization; final weights
#'   use full convergence via [sinkhorn_weights()]).
#' @param use_gpu Logical or NULL. If `TRUE`, use GPU (CUDA or MPS). If
#'   `FALSE`, use CPU. If `NULL` (default), reads the package option
#'   `interpElections.use_gpu` (set via [use_gpu()]).
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. Only used when GPU is enabled. Default: NULL (auto-detect).
#' @param dtype Character. Torch dtype: `"float32"` or `"float64"`. Default:
#'   `"float32"`. Float32 halves memory usage with negligible precision loss.
#' @param gpu_iterations Integer. Number of outer ADAM phases with learning
#'   rate decay. Default: 20.
#' @param gpu_lr_init Numeric. Initial ADAM learning rate. Default: 0.1.
#' @param gpu_lr_decay Numeric. Learning rate decay factor per phase.
#'   Default: 0.6.
#' @param gpu_grad_tol Numeric. Gradient norm threshold for convergence.
#'   Default: 1e-4.
#' @param gpu_grad_clip Numeric or NULL. Maximum gradient norm for clipping.
#'   `NULL` disables clipping. Default: 1.0.
#' @param gpu_warmup_steps Integer. Linear learning rate warmup steps at
#'   the start of phase 1. Default: 10.
#' @param lower_bound Numeric. Lower bound for alpha values. Default: 0.
#' @param upper_bound Numeric. Upper bound for alpha values. Default: 20.
#' @param offset Numeric. Value added to travel times before exponentiation.
#'   Default: 1.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#'
#' @return A list of class `"interpElections_optim"` with components:
#' \describe{
#'   \item{alpha}{Numeric vector. Optimal alpha values.}
#'   \item{value}{Numeric. Objective function value at optimum.}
#'   \item{method}{Character. Method used (e.g.,
#'     `"torch_adam_sinkhorn_cpu"`, `"torch_adam_sinkhorn_cuda"`).}
#'   \item{convergence}{Integer. 0 = success.}
#'   \item{iterations}{Number of ADAM steps taken.}
#'   \item{elapsed}{`difftime` object. Wall-clock time.}
#'   \item{message}{Character. Additional information.}
#'   \item{history}{Numeric vector. Objective values at each step.}
#'   \item{grad_norm_final}{Numeric. Final gradient norm.}
#'   \item{row_targets}{Numeric vector. Row targets used for Sinkhorn.}
#'   \item{sinkhorn_iter}{Integer. Sinkhorn iterations used.}
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
#' Both paths use the same ADAM optimizer with log-domain Sinkhorn.
#' Gradients are computed via torch autograd through the unrolled
#' Sinkhorn iterations.
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
    sinkhorn_iter = 5L,
    use_gpu = NULL,
    device = NULL,
    dtype = "float32",
    gpu_iterations = 20L,
    gpu_lr_init = 0.1,
    gpu_lr_decay = 0.6,
    gpu_grad_tol = 1e-4,
    gpu_grad_clip = 1.0,
    gpu_warmup_steps = 10L,
    lower_bound = 0,
    upper_bound = 20,
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

  # Coerce sinkhorn_iter
  sinkhorn_iter <- as.integer(sinkhorn_iter)
  if (sinkhorn_iter < 1L) {
    stop("sinkhorn_iter must be >= 1", call. = FALSE)
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
    row_targets = row_targets,
    sinkhorn_iter = sinkhorn_iter,
    device = resolved_device,
    dtype = resolved_dtype,
    iterations = gpu_iterations,
    lr_init = gpu_lr_init,
    lr_decay = gpu_lr_decay,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    grad_tol = gpu_grad_tol,
    grad_clip = gpu_grad_clip,
    warmup_steps = gpu_warmup_steps,
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
    raw$value <- sinkhorn_objective(
      raw$alpha, t_adj, pop_matrix, source_matrix,
      row_targets, sinkhorn_iter
    )
  }

  result <- list(
    alpha = raw$alpha,
    value = raw$value,
    method = raw$method,
    convergence = raw$convergence,
    iterations = raw$iterations,
    elapsed = elapsed,
    message = raw$message %||% "",
    history = raw$history %||% NULL,
    grad_norm_final = raw$grad_norm_final %||% NULL,
    row_targets = row_targets,
    sinkhorn_iter = sinkhorn_iter
  )
  class(result) <- "interpElections_optim"

  if (verbose) {
    message(sprintf(
      "  Converged in %d steps (%.1fs), objective=%s",
      result$iterations,
      as.numeric(elapsed, units = "secs"),
      format(round(result$value), big.mark = ",")
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
  cat(sprintf("  Elapsed:     %.1f secs\n", as.numeric(x$elapsed, units = "secs")))
  invisible(x)
}
