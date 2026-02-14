# Input validation helpers

# Check a single matrix is numeric, non-empty, and contains only finite values
.check_matrix <- function(mat, name) {
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop(name, " must be a numeric matrix", call. = FALSE)
  }
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    stop(name, " must not be empty", call. = FALSE)
  }
  if (anyNA(mat) || any(!is.finite(mat))) {
    stop(name, " must not contain NA, NaN, or Inf values", call. = FALSE)
  }
}

.validate_matrices <- function(time_matrix, pop_matrix, source_matrix,
                               alpha = NULL) {
  .check_matrix(time_matrix, "time_matrix")
  .check_matrix(pop_matrix, "pop_matrix")
  .check_matrix(source_matrix, "source_matrix")

  n <- nrow(time_matrix)
  m <- ncol(time_matrix)

  if (nrow(pop_matrix) != n) {
    stop(sprintf(
      "pop_matrix has %d rows but time_matrix has %d rows (must match)",
      nrow(pop_matrix), n
    ), call. = FALSE)
  }
  if (nrow(source_matrix) != m) {
    stop(sprintf(
      "source_matrix has %d rows but time_matrix has %d columns (must match)",
      nrow(source_matrix), m
    ), call. = FALSE)
  }
  if (ncol(pop_matrix) != ncol(source_matrix)) {
    stop(sprintf(
      "pop_matrix has %d columns but source_matrix has %d columns (must match)",
      ncol(pop_matrix), ncol(source_matrix)
    ), call. = FALSE)
  }

  # time_matrix must be strictly positive (required for t^(-alpha))
  if (any(time_matrix <= 0)) {
    stop(
      "time_matrix must contain only positive values ",
      "(apply offset before validation if needed)",
      call. = FALSE
    )
  }

  if (!is.null(alpha)) {
    .validate_alpha(alpha, n)
  }

  invisible(TRUE)
}

.validate_alpha <- function(alpha, n) {
  if (is.matrix(alpha)) {
    stop("alpha must be a numeric vector, not a matrix", call. = FALSE)
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be a numeric vector", call. = FALSE)
  }
  if (length(alpha) != n) {
    stop(sprintf(
      "alpha has length %d but expected length %d",
      length(alpha), n
    ), call. = FALSE)
  }
  if (anyNA(alpha) || any(!is.finite(alpha))) {
    stop("alpha must not contain NA, NaN, or Inf values", call. = FALSE)
  }
  if (any(alpha < 0)) {
    stop("alpha values must be non-negative", call. = FALSE)
  }
  invisible(TRUE)
}
