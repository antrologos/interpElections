#' Compute the analytical gradient of the IDW objective
#'
#' Computes the gradient of [idw_objective()] with respect to each element
#' of the alpha vector. Used by the CPU optimizer for faster convergence.
#'
#' @inheritParams idw_objective
#'
#' @return Numeric vector of length n: the gradient of the objective with
#'   respect to each element of alpha.
#'
#' @details
#' The gradient is computed analytically using the chain rule through the
#' column-standardized weight matrix. The computation is structured to avoid
#' forming any n x n intermediate matrices, keeping the per-evaluation cost
#' at O(n * m) rather than O(n^2 * m). This makes CPU optimization feasible
#' even for large municipalities with thousands of census tracts.
#'
#' For k demographic groups, the total gradient is the sum of per-group
#' gradients.
#'
#' @examples
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2)
#' pop <- matrix(c(100, 200), nrow = 2)
#' src <- matrix(c(80, 120, 100), nrow = 3)
#' alpha <- c(1, 1.5)
#' idw_gradient(alpha, tt, pop, src)
#'
#' @family IDW core
#'
#' @seealso [idw_objective()] for the objective function,
#'   [optimize_alpha()] for the optimization wrapper.
#'
#' @export
idw_gradient <- function(alpha, time_matrix, pop_matrix, source_matrix) {
  .validate_matrices(time_matrix, pop_matrix, source_matrix, alpha)

  n <- nrow(pop_matrix)
  k <- ncol(pop_matrix)

  # W = t^(-alpha)
  tmA <- time_matrix ^ (-alpha)
  log_t <- log(time_matrix)

  # Column sums of W (guard against zero, consistent with
  # .col_standardize in utils.R)
  g <- colSums(tmA)
  g[g == 0] <- 1

  # Column-standardized weights (correct column-wise division)
  W_std <- t(t(tmA) / g)

  # Scaled source matrices
  V_g <- source_matrix / g
  V_g2 <- source_matrix / (g^2)

  # Residual: interpolated - actual
  tmA_v_g_mp <- (tmA %*% V_g) - pop_matrix

  # W * log(t)
  tmA_logt <- tmA * log_t

  # Precompute element-wise product used in diagonal term for every group
  tmA_logt_tmA <- tmA_logt * tmA  # n x m

  # Sum gradient across demographic groups — O(nm) per group.
  #
  # The gradient for group j involves differentiating the column-standardized
  # weight matrix via the quotient rule. The key identity that avoids forming
  # an n x n intermediate matrix:
  #
  #   Let AB = tmA_logt %*% diag(v2) %*% t(tmA)  (would be n x n)
  #   We only need colSums(t(AB) * r) and diag(t(AB) * r), which decompose:
  #     colSums(t(AB) * r) = AB %*% r = tmA_logt %*% (v2 * crossprod(tmA, r))
  #     diag(AB) * r       = ((tmA_logt * tmA) %*% v2) * r
  #
  # Both are O(nm) matrix-vector products instead of O(n^2 m) matrix-matrix.
  list_result <- lapply(seq_len(k), function(j) {
    r  <- tmA_v_g_mp[, j]   # n-vector (residual)
    v2 <- V_g2[, j]          # m-vector
    v1 <- V_g[, j]           # m-vector

    # colSums(tmp1) via two matrix-vector products (avoids n x n)
    tmA_r <- crossprod(tmA, r)            # m-vector: t(tmA) %*% r
    cs <- as.numeric(tmA_logt %*% (v2 * tmA_r))  # n-vector

    # diag(tmp1) via element-wise product
    d <- as.numeric(tmA_logt_tmA %*% v2) * r  # n-vector

    tmp2 <- cs - d

    # Direct term (already O(nm))
    term2 <- r * as.numeric((tmA_logt * (W_std - 1)) %*% v1)

    2 * (tmp2 + term2)
  })

  Reduce("+", list_result)
}
