# ============================================================
# compute_weight_matrix(): Torch-based 3D Sinkhorn weight matrix
# ============================================================

#' Compute the Sinkhorn-balanced weight matrix
#'
#' Builds a weight matrix \eqn{W} from per-tract-per-bracket decay parameters
#' using the same 3D Sinkhorn IPF as [optimize_alpha()]. Uses torch for
#' computation (CPU or GPU).
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
#'   exponentiation: \eqn{K^b_{ij} = (t_{ij} + \text{offset})^{-\alpha_{ib}}}.
#'   Default: 1.
#' @param method Character. Normalization method: `"colnorm"` (column-only
#'   normalization, default) or `"sinkhorn"` (3D Sinkhorn IPF). Should match
#'   the method used during optimization.
#' @param sk_iter Integer. Maximum Sinkhorn iterations. Only used when
#'   `method = "sinkhorn"`. Default: 100L.
#' @param sk_tol Numeric. Sinkhorn convergence tolerance. Default: 1e-6.
#' @param use_gpu Logical or NULL. If TRUE, use GPU; if FALSE, use CPU;
#'   if NULL (default), respect the global setting from [use_gpu()].
#' @param verbose Logical. Print progress? Default: FALSE.
#'
#' @return Numeric matrix \[n x m\]. Sinkhorn-balanced weight matrix where
#'   each column sums to approximately 1. Use as `W \%*\% data` to
#'   interpolate any source-level variable into zones.
#'
#' @details
#' This function performs the same 3D Sinkhorn computation used internally
#' by [optimize_alpha()] during epoch evaluation. It is useful when you
#' have pre-computed alpha values and need to build the weight matrix
#' without running optimization.
#'
#' The 3D Sinkhorn enforces two properties:
#' \itemize{
#'   \item \strong{Property 1 (voter conservation)}: Each source-bracket
#'     pair distributes its full count across zones.
#'   \item \strong{Property 2 (population proportions)}: Each zone's total
#'     inflow is proportional to its population.
#' }
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
                                   method = c("colnorm", "sinkhorn"),
                                   sk_iter = 100L, sk_tol = 1e-6,
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

  # Apply offset
  t_adj <- time_matrix + offset
  if (any(t_adj <= 0)) {
    stop("time_matrix + offset must be strictly positive", call. = FALSE)
  }

  # Coerce parameters
  sk_iter <- as.integer(sk_iter)
  if (is.na(sk_iter) || sk_iter < 1L) {
    stop("sk_iter must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(sk_tol) || length(sk_tol) != 1 ||
      !is.finite(sk_tol) || sk_tol <= 0) {
    stop("sk_tol must be a single positive number", call. = FALSE)
  }

  method <- match.arg(method)

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
    method = method,
    sk_iter = sk_iter, sk_tol = sk_tol,
    device = device, dtype = dtype, verbose = verbose
  )
}


# Internal: subprocess wrapper for compute_weight_matrix
.compute_W_subprocess <- function(t_adj, alpha, pop_matrix, source_matrix,
                                   method, sk_iter, sk_tol,
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
               method, sk_iter, sk_tol,
               device, dtype, verbose, pkg_path) {
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
          method = method,
          sk_iter = sk_iter, sk_tol = sk_tol,
          device = device, dtype = dtype, verbose = verbose
        )
      },
      args = list(
        t_adj = t_adj, alpha = alpha,
        pop_matrix = pop_matrix, source_matrix = source_matrix,
        method = method,
        sk_iter = sk_iter, sk_tol = sk_tol,
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

  # Population for row targets
  p_mat_active <- pop_matrix[, active, drop = FALSE]
  r_pop_total  <- rowSums(p_mat_active)
  P_total_val  <- sum(r_pop_total)
  r_total_norm <- r_pop_total / pmax(P_total_val, 1e-30)

  V_total_val <- sum(c_mat)

  # Alpha for active brackets only
  alpha_active <- alpha[, active, drop = FALSE]  # (n, ka)

  # Torch tensors
  on.exit(torch::cuda_empty_cache(), add = TRUE)

  log_t_torch <- torch::torch_log(torch::torch_tensor(
    t_adj, device = device, dtype = torch_dtype))

  alpha_torch <- torch::torch_tensor(
    alpha_active, device = device, dtype = torch_dtype)

  log_V_b_torch <- torch::torch_tensor(
    log(pmax(c_mat, 1e-30)),
    device = device, dtype = torch_dtype)

  # Build 3D kernel: log_K_3d[b, i, j] = -alpha[i, b] * log_t[i, j]
  log_K_3d <- -alpha_torch$t()$unsqueeze(3L) *
    log_t_torch$unsqueeze(1L)                      # (ka, n, m)

  if (method == "colnorm") {
    # Column-only normalization (no Sinkhorn IPF)
    torch::with_no_grad({
      log_col_sum <- torch::torch_logsumexp(
        log_K_3d, dim = 2L)                         # (ka, m)
      log_W_3d <- log_K_3d -
        log_col_sum$unsqueeze(2L)                   # (ka, n, m)

      # Aggregate: W[i,j] = sum_b (W^b[i,j] * c[b,j]) / sum_b c[b,j]
      log_T <- log_W_3d + log_V_b_torch$unsqueeze(2L)
      log_c_agg <- torch::torch_logsumexp(log_V_b_torch, dim = 1L)
      log_T_agg <- torch::torch_logsumexp(log_T, dim = 1L)
      W <- as.matrix(torch::torch_exp(
        log_T_agg - log_c_agg$unsqueeze(1L))$cpu())
    })
  } else {
    # 3D Sinkhorn IPF — compute row targets (only needed for Sinkhorn)
    log_row_target <- torch::torch_tensor(
      log(pmax(r_total_norm, 1e-30)) + log(pmax(V_total_val, 1e-30)),
      device = device, dtype = torch_dtype)

    log_u <- torch::torch_zeros(n, device = device, dtype = torch_dtype)
    log_v <- torch::torch_zeros(
      c(ka, m), device = device, dtype = torch_dtype)

    torch::with_no_grad({
      converged <- FALSE
      for (iter in seq_len(sk_iter)) {
        log_u_prev <- log_u

        log_KV <- log_K_3d + log_v$unsqueeze(2L)
        log_row_sum <- torch::torch_logsumexp(
          torch::torch_logsumexp(log_KV, dim = 3L), dim = 1L)
        log_u <- log_row_target - log_row_sum

        log_KU <- log_K_3d + log_u$unsqueeze(1L)$unsqueeze(3L)
        log_col_sum <- torch::torch_logsumexp(log_KU, dim = 2L)
        log_v <- log_V_b_torch - log_col_sum

        if (iter >= 2L) {
          sk_delta <- (log_u - log_u_prev)$abs()$max()$item()
          if (is.finite(sk_delta) && sk_delta < sk_tol) {
            converged <- TRUE
            break
          }
        }
      }

      if (!converged && verbose) {
        message(sprintf(
          "  Sinkhorn did not converge after %d iterations (delta=%.2e)",
          sk_iter, if (exists("sk_delta")) sk_delta else Inf))
      }

      log_T <- log_K_3d +
        log_u$unsqueeze(1L)$unsqueeze(3L) +
        log_v$unsqueeze(2L)

      log_c_agg <- torch::torch_logsumexp(log_V_b_torch, dim = 1L)
      log_T_agg <- torch::torch_logsumexp(log_T, dim = 1L)
      W <- as.matrix(torch::torch_exp(
        log_T_agg - log_c_agg$unsqueeze(1L))$cpu())
    })
  }

  if (!is.null(dimnames(t_adj))) dimnames(W) <- dimnames(t_adj)
  W
}
