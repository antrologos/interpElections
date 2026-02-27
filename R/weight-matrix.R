# ============================================================
# compute_weight_matrix(): Torch-based weight matrix
# ============================================================

#' Compute the weight matrix
#'
#' Builds a weight matrix \eqn{W} from per-tract-per-bracket decay parameters
#' using column normalization. Uses torch for computation (CPU or GPU).
#'
#' @param time_matrix Numeric matrix \[n x m\]. Travel times or distances
#'   from each zone (row) to each source point (column).
#' @param alpha Numeric scalar, vector of length n, or matrix \[n x k\].
#'   Per-tract-per-bracket decay parameters. A scalar or n-vector is
#'   recycled across all k brackets. Typically from `optimize_alpha()$alpha`.
#' @param pop_matrix Numeric matrix \[n x k\]. Known population counts
#'   per zone and demographic bracket.
#' @param source_matrix Numeric matrix \[m x k\]. Known counts at source
#'   points per demographic bracket.
#' @param offset Numeric scalar. Added to `time_matrix` before
#'   exponentiation for the power kernel:
#'   \eqn{K^b_{ij} = (t_{ij} + \text{offset})^{-\alpha_{ib}}}.
#'   Ignored when `kernel = "exponential"`. Default: 1.
#' @param kernel Character. `"power"` (default) or `"exponential"`.
#'   See [optim_control()] for details.
#' @param use_gpu Logical or NULL. If TRUE, use GPU; if FALSE, use CPU;
#'   if NULL (default), respect the global setting from [use_gpu()].
#' @param verbose Logical. Print progress? Default: FALSE.
#'
#' @return Numeric matrix \[n x m\]. Column-normalized weight matrix where
#'   each column sums to approximately 1. Use as `W \%*\% data` to
#'   interpolate any source-level variable into zones.
#'
#' @details
#' This function performs the same column normalization used internally
#' by [optimize_alpha()] during epoch evaluation. It is useful when you
#' have pre-computed alpha values and need to build the weight matrix
#' without running optimization.
#'
#' Column normalization ensures voter conservation: each source-bracket
#' pair distributes its full count across zones.
#'
#' @seealso [optimize_alpha()] which returns both alpha and W.
#'
#' @examples
#' \dontrun{
#' # After optimization
#' result <- optimize_alpha(tt, pop, src)
#' W <- result$W   # weight matrix already computed
#'
#' # Or build W from pre-existing alpha
#' W <- compute_weight_matrix(tt, alpha, pop, src)
#' interpolated <- W %*% electoral_data
#' }
#'
#' @export
compute_weight_matrix <- function(time_matrix, alpha, pop_matrix,
                                   source_matrix, offset = 1,
                                   kernel = "power",
                                   use_gpu = NULL, verbose = FALSE) {
  # --- Input validation ---
  if (!is.matrix(time_matrix) || !is.numeric(time_matrix)) {
    stop("time_matrix must be a numeric matrix", call. = FALSE)
  }
  if (!is.matrix(pop_matrix) || !is.numeric(pop_matrix)) {
    stop("pop_matrix must be a numeric matrix", call. = FALSE)
  }
  if (!is.matrix(source_matrix) || !is.numeric(source_matrix)) {
    stop("source_matrix must be a numeric matrix", call. = FALSE)
  }

  n <- nrow(time_matrix)
  m <- ncol(time_matrix)
  k <- ncol(pop_matrix)

  # Coerce alpha: scalar or vector → matrix [n x k]
  if (!is.numeric(alpha)) {
    stop("alpha must be numeric", call. = FALSE)
  }
  if (!is.matrix(alpha)) {
    if (length(alpha) == 1L) {
      alpha <- matrix(alpha, n, k)
    } else if (length(alpha) == n) {
      alpha <- matrix(alpha, n, k)
    } else {
      stop(sprintf(
        "alpha must be a scalar, length-%d vector, or %d x %d matrix",
        n, n, k), call. = FALSE)
    }
  }

  if (nrow(alpha) != n) {
    stop(sprintf("alpha has %d rows but time_matrix has %d rows",
                 nrow(alpha), n), call. = FALSE)
  }
  if (ncol(alpha) != k) {
    stop(sprintf("alpha has %d columns but pop_matrix has %d columns",
                 ncol(alpha), k), call. = FALSE)
  }
  if (nrow(pop_matrix) != n) {
    stop(sprintf("pop_matrix has %d rows but time_matrix has %d rows",
                 nrow(pop_matrix), n), call. = FALSE)
  }
  if (nrow(source_matrix) != m) {
    stop(sprintf("source_matrix has %d rows but time_matrix has %d columns",
                 nrow(source_matrix), m), call. = FALSE)
  }
  if (ncol(source_matrix) != k) {
    stop(sprintf("source_matrix has %d cols but pop_matrix has %d cols",
                 ncol(source_matrix), k), call. = FALSE)
  }

  kernel <- match.arg(kernel, c("power", "exponential"))

  # Apply offset (only for power kernel)
  if (kernel == "power") {
    t_adj <- time_matrix + offset
    if (any(t_adj <= 0, na.rm = TRUE)) {
      stop("time_matrix + offset must be strictly positive (where not NA)",
           call. = FALSE)
    }
  } else {
    t_adj <- time_matrix  # no offset for exponential
    if (any(t_adj < 0, na.rm = TRUE)) {
      stop("time_matrix must contain only non-negative values for exponential kernel",
           call. = FALSE)
    }
  }

  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("The 'torch' package is required. Install with: setup_torch()",
         call. = FALSE)
  }

  # Resolve GPU setting
  resolved_use_gpu <- if (!is.null(use_gpu)) {
    use_gpu
  } else {
    getOption("interpElections.use_gpu", default = FALSE)
  }

  if (resolved_use_gpu) {
    device <- .detect_device()
    if (device == "cpu") {
      warning("GPU requested but no GPU found; using CPU", call. = FALSE)
    }
  } else {
    device <- "cpu"
  }
  dtype <- getOption("interpElections.dtype", default = "float32")

  # --- Run via callr subprocess (same pattern as optimize_alpha) ---
  .compute_W_subprocess(
    t_adj = t_adj, alpha = alpha,
    pop_matrix = pop_matrix, source_matrix = source_matrix,
    kernel = kernel,
    device = device, dtype = dtype, verbose = verbose
  )
}


# Internal: subprocess wrapper for compute_weight_matrix
.compute_W_subprocess <- function(t_adj, alpha, pop_matrix, source_matrix,
                                   kernel,
                                   device, dtype, verbose) {

  # RStudio subprocess delegation
  if (.is_rstudio() &&
      !identical(Sys.getenv("INTERPELECTIONS_SUBPROCESS"), "1")) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop("Computing weight matrix inside RStudio requires 'callr'.\n",
           "Install with: install.packages('callr')", call. = FALSE)
    }
    if (verbose) message("  Running weight matrix computation in subprocess...")
    return(callr::r(
      function(t_adj, alpha, pop_matrix, source_matrix,
               kernel, device, dtype, verbose, pkg_path) {
        Sys.setenv(INTERPELECTIONS_SUBPROCESS = "1")
        if (nzchar(pkg_path) &&
            file.exists(file.path(pkg_path, "DESCRIPTION"))) {
          pkgload::load_all(pkg_path, quiet = TRUE)
        } else {
          library(interpElections)
        }
        interpElections:::.compute_W_subprocess(
          t_adj = t_adj, alpha = alpha,
          pop_matrix = pop_matrix, source_matrix = source_matrix,
          kernel = kernel,
          device = device, dtype = dtype, verbose = verbose
        )
      },
      args = list(
        t_adj = t_adj, alpha = alpha,
        pop_matrix = pop_matrix, source_matrix = source_matrix,
        kernel = kernel,
        device = device, dtype = dtype, verbose = verbose,
        pkg_path = .find_package_root()
      ),
      show = verbose
    ))
  }

  # --- Torch computation ---
  torch_dtype <- .resolve_dtype(dtype)
  n <- nrow(t_adj)
  m <- ncol(t_adj)
  k <- ncol(pop_matrix)

  # Identify active brackets
  active <- which(
    colSums(pop_matrix) >= 0.5 & colSums(source_matrix) >= 0.5
  )
  if (length(active) == 0L) {
    stop("All demographic brackets are empty", call. = FALSE)
  }
  ka <- length(active)

  # Source counts per active bracket: c_mat[b, j]
  c_mat <- t(source_matrix[, active, drop = FALSE])  # (ka, m)

  # Alpha for active brackets only
  alpha_active <- alpha[, active, drop = FALSE]  # (n, ka)

  # Torch tensors
  on.exit(torch::cuda_empty_cache(), add = TRUE)

  # Detect unreachable pairs (NA in t_adj) and create mask
  na_mask <- is.na(t_adj)
  has_na <- any(na_mask)

  # t_basis: quantity multiplied by -alpha to form log_K
  if (kernel == "power") {
    if (has_na) t_adj[na_mask] <- 1  # safe placeholder for log
    t_basis <- torch::torch_log(torch::torch_tensor(
      t_adj, device = device, dtype = torch_dtype))
  } else {
    if (has_na) t_adj[na_mask] <- 0  # safe placeholder
    t_basis <- torch::torch_tensor(
      t_adj, device = device, dtype = torch_dtype)
  }

  # Mask tensor for unreachable pairs (TRUE = reachable)
  if (has_na) {
    mask_torch <- torch::torch_tensor(
      !na_mask, device = device, dtype = torch::torch_bool())
  }

  alpha_torch <- torch::torch_tensor(
    alpha_active, device = device, dtype = torch_dtype)

  log_V_b_torch <- torch::torch_tensor(
    log(pmax(c_mat, 1e-30)),
    device = device, dtype = torch_dtype)

  # Build 3D kernel: log_K_3d[b, i, j] = -alpha[i, b] * t_basis[i, j]
  log_K_3d <- -alpha_torch$t()$unsqueeze(3L) *
    t_basis$unsqueeze(1L)                             # (ka, n, m)

  # Mask unreachable pairs: set log_K = -Inf so exp(log_K) = 0
  if (has_na) {
    log_K_3d <- torch::torch_where(
      mask_torch$unsqueeze(1L), log_K_3d,
      torch::torch_tensor(-Inf, device = device,
                          dtype = torch_dtype))
  }

  # Column normalization
  torch::with_no_grad({
    log_col_sum <- torch::torch_logsumexp(
      log_K_3d, dim = 2L)                         # (ka, m)
    log_W_3d <- log_K_3d -
      log_col_sum$unsqueeze(2L)                   # (ka, n, m)

    # Handle all-unreachable columns: -Inf - (-Inf) = NaN -> -Inf
    if (has_na) {
      log_W_3d <- torch::torch_where(
        torch::torch_isnan(log_W_3d),
        torch::torch_tensor(-Inf, device = device,
                            dtype = torch_dtype),
        log_W_3d)
    }

    # Aggregate: W[i,j] = sum_b (W^b[i,j] * c[b,j]) / sum_b c[b,j]
    log_T <- log_W_3d + log_V_b_torch$unsqueeze(2L)
    log_c_agg <- torch::torch_logsumexp(log_V_b_torch, dim = 1L)
    log_T_agg <- torch::torch_logsumexp(log_T, dim = 1L)
    W <- as.matrix(torch::torch_exp(
      log_T_agg - log_c_agg$unsqueeze(1L))$cpu())
  })

  if (!is.null(dimnames(t_adj))) dimnames(W) <- dimnames(t_adj)
  W
}
