# Sinkhorn/IPF weight balancing for spatial interpolation
#
# Core functions:
#   sinkhorn_balance()   — low-level IPF algorithm
#   sinkhorn_weights()   — IDW kernel + Sinkhorn balancing
#   sinkhorn_objective() — SSE objective with Sinkhorn-balanced weights

# ============================================================
# sinkhorn_balance: core Sinkhorn/IPF algorithm
# ============================================================

#' Balance a weight matrix using Sinkhorn/IPF iterations
#'
#' Applies alternating row and column scaling to enforce both row and column
#' marginal constraints simultaneously. Also known as Iterative Proportional
#' Fitting (IPF) or the RAS algorithm.
#'
#' @param W Numeric matrix \[n x m\]. Raw non-negative weight matrix.
#' @param row_targets Numeric vector of length n. Desired row sums.
#'   Default: `NULL` (uniform: `rep(sum(col_targets) / n, n)`).
#' @param col_targets Numeric vector of length m. Desired column sums.
#'   Default: `NULL` (`rep(1, m)`, preserving source totals).
#' @param max_iter Integer. Maximum Sinkhorn iterations. Default: 1000.
#' @param tol Numeric. Convergence tolerance on maximum marginal error.
#'   Default: 1e-10.
#'
#' @return Numeric matrix \[n x m\] with row sums approximately equal to
#'   `row_targets` and column sums approximately equal to `col_targets`.
#'   Attributes:
#'   \describe{
#'     \item{iterations}{Integer. Number of iterations performed.}
#'     \item{converged}{Logical. Whether convergence tolerance was met.}
#'     \item{unreachable}{Integer vector. Indices of all-zero rows (skipped).}
#'   }
#'
#' @details
#' At each iteration:
#' \enumerate{
#'   \item Row scaling: multiply each row by `row_targets[i] / rowSums(W)[i]`
#'   \item Column scaling: multiply each column by `col_targets[j] / colSums(W)[j]`
#' }
#'
#' Rows that are all-zero (unreachable targets) are skipped during scaling
#' and remain zero. A warning is issued listing these rows.
#'
#' The algorithm requires `sum(row_targets)` approximately equal to
#' `sum(col_targets)` for feasibility. A warning is issued if they
#' differ by more than 1\%.
#'
#' @examples
#' W <- matrix(c(3, 1, 0.5, 2, 4, 1.5), nrow = 2)
#' balanced <- sinkhorn_balance(W, row_targets = c(1, 2), col_targets = c(1, 1, 1))
#' rowSums(balanced)  # approx c(1, 2)
#' colSums(balanced)  # approx c(1, 1, 1)
#'
#' @family Sinkhorn
#' @export
sinkhorn_balance <- function(W, row_targets = NULL, col_targets = NULL,
                              max_iter = 1000L, tol = 1e-10) {
  if (!is.matrix(W) || !is.numeric(W)) {
    stop("W must be a numeric matrix", call. = FALSE)
  }
  if (nrow(W) == 0 || ncol(W) == 0) {
    stop("W must not be empty", call. = FALSE)
  }
  if (anyNA(W) || any(!is.finite(W))) {
    stop("W must not contain NA, NaN, or Inf values", call. = FALSE)
  }
  if (any(W < 0)) {
    stop("W must be non-negative", call. = FALSE)
  }

  n <- nrow(W)
  m <- ncol(W)

  # Default targets
  if (is.null(col_targets)) col_targets <- rep(1, m)
  if (is.null(row_targets)) row_targets <- rep(sum(col_targets) / n, n)

  # Validate targets
  if (!is.numeric(row_targets) || length(row_targets) != n) {
    stop(sprintf("row_targets must be a numeric vector of length %d", n),
         call. = FALSE)
  }
  if (!is.numeric(col_targets) || length(col_targets) != m) {
    stop(sprintf("col_targets must be a numeric vector of length %d", m),
         call. = FALSE)
  }
  if (any(row_targets < 0) || any(col_targets < 0)) {
    stop("row_targets and col_targets must be non-negative", call. = FALSE)
  }

  # Feasibility check
  sum_r <- sum(row_targets)
  sum_c <- sum(col_targets)
  if (max(sum_r, sum_c) > 0 &&
      abs(sum_r - sum_c) / max(sum_r, sum_c) > 0.01) {
    warning(sprintf(
      "sum(row_targets)=%.4f differs from sum(col_targets)=%.4f by %.1f%%. Sinkhorn may not converge.",
      sum_r, sum_c, abs(sum_r - sum_c) / max(sum_r, sum_c) * 100
    ), call. = FALSE)
  }

  # Detect unreachable rows (all zeros)
  unreachable <- which(rowSums(W) == 0)
  if (length(unreachable) > 0) {
    warning(sprintf(
      "%d rows are all-zero (unreachable) and will remain zero: %s",
      length(unreachable),
      paste(head(unreachable, 10), collapse = ", ")
    ), call. = FALSE)
  }

  reachable <- setdiff(seq_len(n), unreachable)

  # Early exit: nothing to balance
  if (length(reachable) == 0) {
    attr(W, "iterations") <- 0L
    attr(W, "converged") <- TRUE
    attr(W, "unreachable") <- unreachable
    return(W)
  }

  converged <- FALSE
  max_err <- Inf
  iter <- 0L

  for (iter in seq_len(max_iter)) {
    # Row scaling
    rs <- rowSums(W)
    rs[rs == 0] <- 1
    W <- W * (row_targets / rs)

    # Column scaling
    cs <- colSums(W)
    cs[cs == 0] <- 1
    W <- t(t(W) * (col_targets / cs))

    # Convergence check
    rs_err <- max(abs(rowSums(W)[reachable] - row_targets[reachable]))
    cs_err <- max(abs(colSums(W) - col_targets))
    max_err <- max(rs_err, cs_err)

    if (max_err < tol) {
      converged <- TRUE
      break
    }
  }

  if (!converged) {
    warning(sprintf(
      "Sinkhorn did not converge after %d iterations (max marginal error: %.2e)",
      max_iter, max_err
    ), call. = FALSE)
  }

  W[!is.finite(W)] <- 0

  attr(W, "iterations") <- iter
  attr(W, "converged") <- converged
  attr(W, "unreachable") <- unreachable
  W
}


# ============================================================
# sinkhorn_weights: IDW kernel + Sinkhorn balancing
# ============================================================

#' Compute Sinkhorn-balanced IDW weights
#'
#' Builds inverse distance weights from a travel-time matrix and per-zone
#' decay parameters, then applies Sinkhorn balancing to enforce both row
#' and column marginal constraints.
#'
#' When `pop_matrix` and `source_matrix` are provided, per-bracket Sinkhorn
#' balancing is used: each demographic column gets its own Sinkhorn transport
#' (with row targets rescaled to match source totals), then all bracket
#' contributions are summed and a final aggregate Sinkhorn enforces the
#' overall `row_targets` / `col_targets` constraints.
#'
#' @param time_matrix Numeric matrix \[n x m\]. Raw travel times.
#'   Rows = target zones, columns = source points.
#' @param alpha Numeric vector of length n. Decay parameters per target zone.
#' @param offset Numeric. Added to travel times before applying decay.
#'   Default: 1.
#' @param row_targets Numeric vector of length n. Desired row sums.
#'   For electoral interpolation, use `pop / sum(pop) * m` where `pop`
#'   is the population vector and `m = ncol(time_matrix)`. Default: `NULL`
#'   (uniform allocation).
#' @param col_targets Numeric vector of length m. Desired column sums.
#'   Default: `NULL` (`rep(1, m)` for source conservation).
#' @param pop_matrix Numeric matrix \[n x k\] or NULL. Population per zone
#'   per demographic bracket. When provided together with `source_matrix`,
#'   enables per-bracket Sinkhorn mode. Default: NULL.
#' @param source_matrix Numeric matrix \[m x k\] or NULL. Source counts per
#'   demographic bracket. Must be provided together with `pop_matrix`.
#'   Default: NULL.
#' @param max_iter Integer. Max Sinkhorn iterations. Default: 1000.
#' @param tol Numeric. Convergence tolerance. Default: 1e-10.
#'
#' @return Numeric matrix \[n x m\]. Sinkhorn-balanced weights with
#'   `rowSums(W)` approximately equal to `row_targets` and `colSums(W)`
#'   approximately equal to `col_targets`. Preserves dimnames from
#'   `time_matrix`. Carries attributes from [sinkhorn_balance()].
#'
#' @examples
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
#' alpha <- c(1, 1.5)
#' pop <- c(100, 200)
#' m <- ncol(tt)
#' W <- sinkhorn_weights(tt, alpha, row_targets = pop / sum(pop) * m)
#' colSums(W)  # approx 1 (source conservation)
#' rowSums(W)  # proportional to population
#'
#' @seealso [sinkhorn_balance()] for the core algorithm,
#'   [optimize_alpha()] to find optimal alpha values.
#'
#' @family Sinkhorn
#' @export
sinkhorn_weights <- function(time_matrix, alpha, offset = 1,
                              row_targets = NULL, col_targets = NULL,
                              pop_matrix = NULL, source_matrix = NULL,
                              max_iter = 1000L, tol = 1e-10) {
  if (!is.matrix(time_matrix) || !is.numeric(time_matrix)) {
    stop("time_matrix must be a numeric matrix", call. = FALSE)
  }
  if (nrow(time_matrix) == 0 || ncol(time_matrix) == 0) {
    stop("time_matrix must not be empty", call. = FALSE)
  }
  if (anyNA(time_matrix) || any(!is.finite(time_matrix))) {
    stop("time_matrix must not contain NA, NaN, or Inf values",
         call. = FALSE)
  }
  .validate_alpha(alpha, nrow(time_matrix))

  # Validate per-bracket args: both or neither
  if (is.null(pop_matrix) != is.null(source_matrix)) {
    stop("pop_matrix and source_matrix must both be provided or both be NULL",
         call. = FALSE)
  }

  t_adj <- .apply_offset(time_matrix, offset)
  if (any(t_adj <= 0)) {
    stop("time_matrix + offset must be strictly positive", call. = FALSE)
  }

  n <- nrow(t_adj)
  m <- ncol(t_adj)
  K <- t_adj ^ (-alpha)
  K[!is.finite(K)] <- 0

  if (!is.null(pop_matrix) && !is.null(source_matrix)) {
    # --- Per-bracket Sinkhorn mode ---
    if (!is.matrix(pop_matrix) || !is.numeric(pop_matrix)) {
      stop("pop_matrix must be a numeric matrix", call. = FALSE)
    }
    if (!is.matrix(source_matrix) || !is.numeric(source_matrix)) {
      stop("source_matrix must be a numeric matrix", call. = FALSE)
    }
    if (nrow(pop_matrix) != n) {
      stop(sprintf(
        "pop_matrix has %d rows but time_matrix has %d rows (must match)",
        nrow(pop_matrix), n), call. = FALSE)
    }
    if (nrow(source_matrix) != m) {
      stop(sprintf(
        "source_matrix has %d rows but time_matrix has %d columns (must match)",
        nrow(source_matrix), m), call. = FALSE)
    }
    if (ncol(pop_matrix) != ncol(source_matrix)) {
      stop(sprintf(
        "pop_matrix has %d columns but source_matrix has %d columns (must match)",
        ncol(pop_matrix), ncol(source_matrix)), call. = FALSE)
    }
    k <- ncol(pop_matrix)
    W_total <- matrix(0, n, m)

    suppressWarnings({
      for (bi in seq_len(k)) {
        rb <- pop_matrix[, bi]
        cb <- source_matrix[, bi]
        if (sum(cb) < 0.5 || sum(rb) < 0.5) next
        # Rescale row targets so sum(rb) == sum(cb)
        rb <- rb / sum(rb) * sum(cb)
        W_b <- sinkhorn_balance(K, row_targets = rb, col_targets = cb,
                                 max_iter = max_iter, tol = tol)
        W_total <- W_total + W_b
      }
    })

    # Final aggregate Sinkhorn
    final_col <- col_targets %||% rep(1, m)
    sinkhorn_balance(W_total, row_targets = row_targets,
                      col_targets = final_col,
                      max_iter = max_iter, tol = tol)
  } else {
    # --- Single Sinkhorn mode (backward compatible) ---
    sinkhorn_balance(K, row_targets = row_targets, col_targets = col_targets,
                      max_iter = max_iter, tol = tol)
  }
}


# ============================================================
# sinkhorn_objective: SSE with Sinkhorn-balanced weights
# ============================================================

#' Compute the per-bracket Sinkhorn-balanced interpolation objective
#'
#' Calculates the sum of squared errors between per-bracket Sinkhorn-balanced
#' IDW-interpolated values and known population. This is the R-level
#' version of the loss function used by [optimize_alpha()].
#'
#' @param alpha Numeric vector of length n. Decay parameters.
#' @param time_matrix Numeric matrix \[n x m\]. Adjusted travel times
#'   (offset already applied). Must be strictly positive.
#' @param pop_matrix Numeric matrix \[n x k\]. Known population per zone.
#' @param source_matrix Numeric matrix \[m x k\]. Known counts at source points.
#' @param row_targets Numeric vector of length n. Desired row sums for
#'   the final aggregate Sinkhorn balancing.
#' @param sk_iter Integer. Maximum Sinkhorn iterations per bracket and for
#'   the final aggregate step. Default: 50.
#'
#' @return Single numeric value:
#'   `sum((W_balanced %*% source_matrix - pop_matrix)^2)`.
#'
#' @details
#' Builds a per-bracket weight matrix: for each demographic column, runs
#' convergence-based Sinkhorn with bracket-specific row/column targets,
#' then sums contributions and applies a final aggregate Sinkhorn to
#' enforce overall row and column constraints. Computes the calibration
#' SSE against the known population.
#'
#' Note: `time_matrix` is expected with offset already applied (i.e.,
#' `time_matrix + offset`). Do not apply offset again.
#'
#' @seealso [optimize_alpha()] for the optimization wrapper,
#'   [sinkhorn_weights()] for the final weight matrix.
#'
#' @family Sinkhorn
#' @export
sinkhorn_objective <- function(alpha, time_matrix, pop_matrix, source_matrix,
                                row_targets, sk_iter = 50L) {
  .validate_matrices(time_matrix, pop_matrix, source_matrix, alpha)

  if (!is.numeric(row_targets) || length(row_targets) != nrow(time_matrix)) {
    stop(sprintf("row_targets must be a numeric vector of length %d",
                 nrow(time_matrix)), call. = FALSE)
  }

  n <- nrow(time_matrix)
  m <- ncol(time_matrix)
  k <- ncol(pop_matrix)

  K <- time_matrix ^ (-alpha)
  K[!is.finite(K)] <- 0

  # Per-bracket Sinkhorn
  W_total <- matrix(0, n, m)
  suppressWarnings({
    for (bi in seq_len(k)) {
      rb <- pop_matrix[, bi]
      cb <- source_matrix[, bi]
      if (sum(cb) < 0.5 || sum(rb) < 0.5) next
      rb <- rb / sum(rb) * sum(cb)
      W_b <- sinkhorn_balance(K, row_targets = rb, col_targets = cb,
                               max_iter = sk_iter, tol = 1e-10)
      W_total <- W_total + W_b
    }
  })

  # Final aggregate Sinkhorn
  col_targets <- rep(1, m)
  W <- sinkhorn_balance(W_total, row_targets = row_targets,
                         col_targets = col_targets,
                         max_iter = sk_iter, tol = 1e-10)

  v_hat <- W %*% source_matrix
  sum((v_hat - pop_matrix)^2)
}
