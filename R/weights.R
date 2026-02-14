#' Compute the column-standardized IDW weight matrix
#'
#' Builds the inverse distance weight matrix from a travel-time matrix and
#' per-zone decay parameters, then column-standardizes it so each column
#' sums to 1.
#'
#' @param time_matrix Numeric matrix \[n x m\]. Raw travel times.
#'   Rows = target zones, columns = source points.
#' @param alpha Numeric vector of length n. Decay parameters per target zone.
#' @param offset Numeric. Value added to travel times before applying the
#'   power function, to avoid `0^(-alpha)`. Default: 1.
#'
#' @return Numeric matrix \[n x m\]. Column-standardized inverse distance
#'   weights. Each column sums to 1. Row/column names are preserved from
#'   `time_matrix`.
#'
#' @details
#' The weight for zone i and source point j is:
#' \deqn{W_{ij} = (t_{ij} + \text{offset})^{-\alpha_i}}
#' After computing raw weights, each column is divided by its sum so that
#' the weights for each source point form a probability distribution over
#' target zones.
#'
#' @examples
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2) # 2 zones x 3 sources
#' alpha <- c(1, 1.5)
#' W <- idw_weights(tt, alpha, offset = 1)
#' colSums(W) # each column sums to 1
#'
#' @family IDW core
#'
#' @seealso [idw_interpolate()] to apply these weights.
#'
#' @export
idw_weights <- function(time_matrix, alpha, offset = 1) {
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

  t_adj <- .apply_offset(time_matrix, offset)
  if (any(t_adj <= 0)) {
    stop("time_matrix + offset must be strictly positive", call. = FALSE)
  }
  W <- t_adj ^ (-alpha)
  .col_standardize(W)
}
