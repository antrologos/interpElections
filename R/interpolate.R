#' Interpolate source data into target zones using IDW weights
#'
#' Applies inverse-distance-weighted interpolation to transfer data from
#' geolocated source points (e.g., polling locations) into target zones
#' (e.g., census tracts), using optimized decay parameters.
#'
#' @param time_matrix Numeric matrix \[n x m\]. Raw travel times.
#'   Rows = target zones, columns = source points.
#' @param alpha Numeric vector of length n. Optimal decay parameters,
#'   typically from [optimize_alpha()].
#' @param source_matrix Numeric matrix or data.frame \[m x p\]. Data at source
#'   points to be interpolated. Each column is a variable (e.g., candidate
#'   votes, attendance counts).
#' @param offset Numeric. Travel time offset. Default: 1.
#'
#' @return Numeric matrix \[n x p\]. Interpolated values at target zones.
#'   Column names are preserved from `source_matrix`.
#'
#' @details
#' For each target zone i and variable v:
#' \deqn{\hat{v}_i = \sum_j W_{ij}^{std} \cdot v_j}
#'
#' where \eqn{W^{std}} is the column-standardized weight matrix from
#' [idw_weights()].
#'
#' This function is the main workhorse for applying the interpolation after
#' optimal alpha values have been found via [optimize_alpha()].
#'
#' @examples
#' tt <- matrix(c(2, 5, 3, 4, 6, 2), nrow = 2) # 2 zones x 3 sources
#' alpha <- c(1, 1.5)
#' src <- matrix(c(80, 120, 100), nrow = 3)      # 3 sources x 1 variable
#' idw_interpolate(tt, alpha, src)
#'
#' @family IDW core
#'
#' @seealso [optimize_alpha()] to find optimal alpha values,
#'   [idw_weights()] to inspect the weight matrix.
#'
#' @export
idw_interpolate <- function(time_matrix, alpha, source_matrix, offset = 1) {
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

  # Convert data.frame to matrix if needed
  col_names <- NULL
  if (is.data.frame(source_matrix)) {
    col_names <- names(source_matrix)
    source_matrix <- as.matrix(source_matrix)
  } else if (is.matrix(source_matrix)) {
    col_names <- colnames(source_matrix)
  }

  if (!is.numeric(source_matrix)) {
    stop("source_matrix must be numeric", call. = FALSE)
  }
  if (anyNA(source_matrix) || any(!is.finite(source_matrix))) {
    stop("source_matrix must not contain NA, NaN, or Inf values",
         call. = FALSE)
  }
  if (nrow(source_matrix) != ncol(time_matrix)) {
    stop(sprintf(
      "source_matrix has %d rows but time_matrix has %d columns (must match)",
      nrow(source_matrix), ncol(time_matrix)
    ), call. = FALSE)
  }

  W_std <- idw_weights(time_matrix, alpha, offset)

  result <- W_std %*% source_matrix
  if (!is.null(col_names)) {
    colnames(result) <- col_names
  }
  result
}
