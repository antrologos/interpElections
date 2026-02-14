test_that("idw_interpolate returns correct dimensions", {
  t_mat <- matrix(runif(30, 1, 10), nrow = 10, ncol = 3)
  alpha <- rep(1, 10)
  source_data <- matrix(runif(15), nrow = 3, ncol = 5)
  result <- idw_interpolate(t_mat, alpha, source_data)
  expect_equal(dim(result), c(10, 5))
})

test_that("idw_interpolate preserves column names from matrix", {
  t_mat <- matrix(c(2, 3, 4, 5), nrow = 2)
  alpha <- c(1, 1)
  source_data <- matrix(c(10, 20, 30, 40), nrow = 2)
  colnames(source_data) <- c("votes_A", "votes_B")
  result <- idw_interpolate(t_mat, alpha, source_data)
  expect_equal(colnames(result), c("votes_A", "votes_B"))
})

test_that("idw_interpolate accepts data.frame source_data", {
  t_mat <- matrix(c(2, 3, 4, 5), nrow = 2)
  alpha <- c(1, 1)
  source_df <- data.frame(x = c(10, 20), y = c(30, 40))
  result <- idw_interpolate(t_mat, alpha, source_df)
  expect_equal(dim(result), c(2, 2))
  expect_equal(colnames(result), c("x", "y"))
})

test_that("idw_interpolate column sums equal source sums", {
  # For column-standardized weights (each col sums to 1),
  # the sum of interpolated values across all zones should equal
  # the sum of source values.
  t_mat <- matrix(runif(12, 1, 10), nrow = 4, ncol = 3)
  alpha <- rep(1, 4)
  source_data <- matrix(c(100, 200, 300), nrow = 3, ncol = 1)
  result <- idw_interpolate(t_mat, alpha, source_data)
  expect_equal(sum(result), sum(source_data), tolerance = 1e-8)
})

test_that("idw_interpolate rejects dimension mismatch", {
  t_mat <- matrix(1:6, nrow = 3, ncol = 2)
  alpha <- c(1, 1, 1)
  # Source has 3 rows but time_matrix has 2 cols
  source_data <- matrix(1:6, nrow = 3)
  expect_error(idw_interpolate(t_mat, alpha, source_data), "must match")
})
