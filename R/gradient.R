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
#' column-standardized weight matrix. This avoids numerical differentiation
#' and is significantly faster for large problems.
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

  # Sum gradient across demographic groups.
  # For each group j, the gradient df/dalpha_i has two terms:
  #   (1) Cross-term: how alpha_i changes other zones' standardized weights
  #   (2) Direct term: how alpha_i changes zone i's own weight
  # Both arise from differentiating the column-standardized W through the

  # quotient rule: d(W_ij/g_j)/dalpha_i = -W_ij*log(t_ij)/g_j + W_ij*(sum_k W_kj*log(t_kj))/g_j^2
  list_result <- lapply(seq_len(k), function(j) {
    tmp1 <- t(tmA_logt %*% (t(tmA) * V_g2[, j])) * tmA_v_g_mp[, j]
    tmp2 <- colSums(tmp1) - diag(tmp1)
    2 * as.numeric(
      tmp2 + (tmA_v_g_mp[, j] * (tmA_logt * (W_std - 1)) %*% V_g[, j])
    )
  })

  Reduce("+", list_result)
}
