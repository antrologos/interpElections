test_that("areal_interpolate produces correct dimensions", {
  # 3 target zones, 4 source zones, 2 variables
  weights <- matrix(c(0.5, 0.3, 0.2, 0, 0.4, 0.4, 0.2, 0, 0, 0.3, 0.3, 0.4),
                    nrow = 3, ncol = 4)
  data <- matrix(c(10, 20, 30, 40, 100, 200, 300, 400), nrow = 4, ncol = 2)

  result <- areal_interpolate(data, weights)
  expect_equal(dim(result), c(3, 2))
})

test_that("areal_interpolate preserves column names from data.frame", {
  weights <- matrix(c(0.6, 0.4, 0.3, 0.7), nrow = 2, ncol = 2)
  data <- data.frame(votes = c(100, 200), pop = c(50, 60))

  result <- areal_interpolate(data, weights)
  expect_equal(colnames(result), c("votes", "pop"))
})

test_that("areal_interpolate rejects dimension mismatch", {
  weights <- matrix(1:4, nrow = 2, ncol = 2)
  data <- matrix(1:6, nrow = 3, ncol = 2)  # 3 rows but weights has 2 cols
  expect_error(areal_interpolate(data, weights), "must match")
})
