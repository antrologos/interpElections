#' Compute the IDW interpolation objective value
#'
#' Calculates the sum of squared errors between the IDW-interpolated values
#' and the known population matrix. This is the loss function that
#' [optimize_alpha()] minimizes.
#'
#' @param alpha Numeric vector of length n. Decay parameters per target zone.
#'   Must be non-negative.
#' @param time_matrix Numeric matrix \[n x m\]. Adjusted travel times (with
#'   offset already applied). Rows = target zones, columns = source points.
#' @param pop_matrix Numeric matrix \[n x k\]. Known population counts per
#'   target zone, with k demographic groups as columns.
#' @param source_matrix Numeric matrix \[m x k\]. Known counts at source
#'   points (e.g., registered voters by age group).
#'
#' @return Single numeric value: `sum((W_std %*% source_matrix - pop_matrix)^2)`
#'
#' @details
#' The weight matrix is computed as:
#' \deqn{W_{ij} = t_{ij}^{-\alpha_i}}
#' then column-standardized so each column sums to 1.
#' The interpolated values are \eqn{\hat{V} = W_{std} \times V}{V_hat = W_std %*% V},
#' and the objective is \eqn{\sum (\hat{V} - P)^2}{sum((V_hat - P)^2)}.
#'
#' @examples
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2) # 2 zones x 3 sources
#' pop <- matrix(c(100, 200), nrow = 2)          # 2 zones x 1 group
#' src <- matrix(c(80, 120, 100), nrow = 3)      # 3 sources x 1 group
#' alpha <- c(1, 1.5)
#' idw_objective(alpha, tt, pop, src)
#'
#' @seealso [idw_gradient()] for the analytical gradient, [idw_weights()]
#'   for the weight matrix, [optimize_alpha()] for the optimization wrapper.
#'
#' @family IDW core
#' @export
idw_objective <- function(alpha, time_matrix, pop_matrix, source_matrix) {
  .validate_matrices(time_matrix, pop_matrix, source_matrix, alpha)

  # W = t^(-alpha), element-wise with per-row alpha
  W <- time_matrix ^ (-alpha)

  # Column-standardize
  W_std <- .col_standardize(W)

  # Interpolate and compute squared error
  v_hat <- W_std %*% source_matrix

  sum((v_hat - pop_matrix)^2)
}
