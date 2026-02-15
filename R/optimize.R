#' Find optimal decay parameters (alpha) for IDW interpolation
#'
#' Optimizes the per-zone decay parameters that minimize the squared error
#' between IDW-interpolated values and known population counts. Supports
#' GPU-accelerated optimization via torch (ADAM) and CPU optimization via
#' L-BFGS-B.
#'
#' @param time_matrix Numeric matrix \[n x m\]. Raw travel times.
#'   Rows = target zones, columns = source points.
#' @param pop_matrix Numeric matrix \[n x k\]. Known population per zone,
#'   with k demographic groups as columns.
#' @param source_matrix Numeric matrix \[m x k\]. Known counts at source
#'   points (e.g., registered voters by age group).
#' @param alpha_init Numeric vector of length n, or a single value to be
#'   recycled. Initial guess for alpha. Default: `rep(1, n)`.
#' @param use_gpu Logical or NULL. If `TRUE`, use torch ADAM optimizer. If
#'   `FALSE`, use CPU optimization. If `NULL` (default), reads the package
#'   option `interpElections.use_gpu` (set via [use_gpu()]).
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. Only used when GPU is enabled. Default: NULL (auto-detect).
#' @param dtype Character. Torch dtype: `"float32"` or `"float64"`. Default:
#'   `"float32"`. Float32 halves GPU memory usage with negligible precision
#'   loss for this optimization problem.
#' @param gpu_iterations Integer. Number of outer ADAM iterations. Default: 20.
#' @param gpu_lr_init Numeric. Initial ADAM learning rate. Default: 0.1.
#' @param gpu_lr_decay Numeric. Learning rate decay factor per outer iteration.
#'   Default: 0.6.
#' @param cpu_method Character. CPU optimization method: `"L-BFGS-B"`,
#'   `"BFGS"`, or `"auto"`. `"auto"` tries parallel L-BFGS-B first, then
#'   serial L-BFGS-B, then BFGS. Default: `"auto"`.
#' @param cpu_parallel Logical or NULL. Use `optimParallel` for CPU? Default:
#'   NULL (auto-detect based on package availability).
#' @param cpu_ncores Integer or NULL. Number of cores for parallel optimization.
#'   Default: NULL (auto = `max(1, detectCores() - 2)`).
#' @param lower_bound Numeric. Lower bound for alpha values. Default: 0.
#' @param upper_bound Numeric. Upper bound for alpha values. Values above
#'   ~10 produce nearly identical weights (the nearest source dominates),
#'   so capping prevents meaningless divergence between methods. Default: 20.
#' @param maxit Integer. Maximum iterations for CPU optimizer. Default: 10000.
#' @param offset Numeric. Value added to travel times. Default: 1.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#'
#' @return A list of class `"interpElections_optim"` with components:
#' \describe{
#'   \item{alpha}{Numeric vector. Optimal alpha values.}
#'   \item{value}{Numeric. Objective function value at optimum.}
#'   \item{method}{Character. Optimization method used (e.g.,
#'     `"gpu_adam"`, `"cpu_lbfgsb_parallel"`, `"cpu_lbfgsb"`, `"cpu_bfgs"`).}
#'   \item{convergence}{Integer. 0 = success.}
#'   \item{iterations}{Number of iterations/steps taken.}
#'   \item{elapsed}{`difftime` object. Wall-clock time.}
#'   \item{message}{Character. Additional information.}
#'   \item{history}{Numeric vector. Objective values at each step (GPU only).}
#' }
#'
#' @examples
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
#' pop <- matrix(c(100, 200), nrow = 2)
#' src <- matrix(c(80, 120, 100), nrow = 3)
#' result <- optimize_alpha(tt, pop, src, verbose = FALSE)
#' result$alpha
#'
#' @family IDW core
#'
#' @seealso [use_gpu()] to toggle GPU globally, [idw_interpolate()] to apply
#'   the optimal alphas, [idw_objective()] and [idw_gradient()] for the
#'   underlying math.
#'
#' @export
optimize_alpha <- function(
    time_matrix,
    pop_matrix,
    source_matrix,
    alpha_init = NULL,
    use_gpu = NULL,
    device = NULL,
    dtype = "float32",
    gpu_iterations = 20L,
    gpu_lr_init = 0.1,
    gpu_lr_decay = 0.6,
    cpu_method = "auto",
    cpu_parallel = NULL,
    cpu_ncores = NULL,
    lower_bound = 0,
    upper_bound = 20,
    maxit = 10000L,
    offset = 1,
    verbose = TRUE
) {
  # Validate cpu_method
  cpu_method <- match.arg(cpu_method, c("auto", "L-BFGS-B", "BFGS"))

  # Coerce to matrix if data.frame
  if (is.data.frame(time_matrix))   time_matrix   <- as.matrix(time_matrix)
  if (is.data.frame(pop_matrix))    pop_matrix    <- as.matrix(pop_matrix)
  if (is.data.frame(source_matrix)) source_matrix <- as.matrix(source_matrix)

  # Apply offset first (needed for positivity in validate)
  t_adj <- .apply_offset(time_matrix, offset)

  # Validate offset-adjusted matrices
  .validate_matrices(t_adj, pop_matrix, source_matrix)

  n <- nrow(t_adj)

  # Default alpha_init
  if (is.null(alpha_init)) {
    alpha_init <- rep(1, n)
  } else if (length(alpha_init) == 1) {
    alpha_init <- rep(alpha_init, n)
  }
  .validate_alpha(alpha_init, n)

  # Resolve GPU setting
  resolved_use_gpu <- if (!is.null(use_gpu)) {
    use_gpu
  } else {
    getOption("interpElections.use_gpu", default = FALSE)
  }

  start_time <- Sys.time()

  if (resolved_use_gpu) {
    # GPU path
    if (!requireNamespace("torch", quietly = TRUE)) {
      stop("use_gpu = TRUE but the 'torch' package is not installed.\n",
           "Install with: install.packages('torch')", call. = FALSE)
    }

    resolved_device <- device %||%
      getOption("interpElections.device") %||%
      .detect_device()
    resolved_dtype <- dtype %||%
      getOption("interpElections.dtype", default = "float32")

    if (verbose) {
      message(sprintf("  GPU (ADAM, %s, %s, %d phases)",
                       resolved_device, resolved_dtype, gpu_iterations))
    }

    raw <- .optimize_gpu(
      time_matrix = t_adj,
      pop_matrix = pop_matrix,
      source_matrix = source_matrix,
      alpha_init = alpha_init,
      device = resolved_device,
      dtype = resolved_dtype,
      iterations = gpu_iterations,
      lr_init = gpu_lr_init,
      lr_decay = gpu_lr_decay,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      verbose = verbose
    )
  } else {
    # CPU path
    # Resolve parallel settings
    if (is.null(cpu_parallel)) {
      cpu_parallel <- requireNamespace("optimParallel", quietly = TRUE) &&
        requireNamespace("parallel", quietly = TRUE)
    }
    if (is.null(cpu_ncores) && cpu_parallel) {
      cpu_ncores <- max(1L, parallel::detectCores() - 2L)
    }

    if (verbose) {
      message(sprintf("  CPU optimization: method=%s, parallel=%s",
                       cpu_method, cpu_parallel))
    }

    raw <- .optimize_cpu(
      time_matrix = t_adj,
      pop_matrix = pop_matrix,
      source_matrix = source_matrix,
      alpha_init = alpha_init,
      method = cpu_method,
      use_parallel = cpu_parallel,
      ncores = cpu_ncores,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      maxit = maxit,
      verbose = verbose
    )
  }

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
    # Recompute objective with corrected alpha
    raw$value <- idw_objective(raw$alpha, t_adj, pop_matrix, source_matrix)
  }

  result <- list(
    alpha = raw$alpha,
    value = raw$value,
    method = raw$method,
    convergence = raw$convergence,
    iterations = raw$iterations,
    elapsed = elapsed,
    message = raw$message %||% "",
    history = raw$history %||% NULL
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
  cat(sprintf("  N zones:     %d\n", length(x$alpha)))
  cat(sprintf("  Elapsed:     %.1f secs\n", as.numeric(x$elapsed, units = "secs")))
  invisible(x)
}
