test_that(".validate_matrices rejects non-matrix input", {
  m <- matrix(1:4, 2, 2)
  p <- matrix(1:4, 2, 2)
  s <- matrix(1:4, 2, 2)

  expect_error(
    interpElections:::.validate_matrices("not", p, s),
    "time_matrix must be a numeric matrix"
  )
  expect_error(
    interpElections:::.validate_matrices(m, data.frame(a = 1), s),
    "pop_matrix must be a numeric matrix"
  )
})

test_that(".validate_matrices rejects empty matrices", {
  m <- matrix(numeric(0), 0, 2)
  p <- matrix(numeric(0), 0, 2)
  s <- matrix(1:4, 2, 2)

  expect_error(
    interpElections:::.validate_matrices(m, p, s),
    "must not be empty"
  )
})

test_that(".validate_matrices rejects NA values", {
  m <- matrix(c(1, NA, 3, 4), 2, 2)
  p <- matrix(1:4, 2, 2)
  s <- matrix(1:4, 2, 2)

  expect_error(
    interpElections:::.validate_matrices(m, p, s),
    "must not contain NA"
  )
})

test_that(".validate_matrices rejects Inf values", {
  m <- matrix(c(1, Inf, 3, 4), 2, 2)
  p <- matrix(1:4, 2, 2)
  s <- matrix(1:4, 2, 2)

  expect_error(
    interpElections:::.validate_matrices(m, p, s),
    "NA, NaN, or Inf"
  )
})

test_that(".validate_matrices rejects zero/negative time_matrix values", {
  m <- matrix(c(0, 1, 2, 3), 2, 2)
  p <- matrix(1:4, 2, 2)
  s <- matrix(1:4, 2, 2)

  expect_error(
    interpElections:::.validate_matrices(m, p, s),
    "positive values"
  )
})

test_that(".validate_matrices rejects dimension mismatches", {
  m <- matrix(1:6, 3, 2)
  p <- matrix(1:4, 2, 2)
  s <- matrix(1:4, 2, 2)

  expect_error(
    interpElections:::.validate_matrices(m, p, s),
    "must match"
  )
})

test_that(".validate_matrices accepts valid input", {
  m <- matrix(c(1, 2, 3, 4), 2, 2)
  p <- matrix(c(10, 20, 30, 40), 2, 2)
  s <- matrix(c(5, 15, 25, 35), 2, 2)

  expect_true(interpElections:::.validate_matrices(m, p, s))
})

test_that(".validate_alpha rejects matrix alpha", {
  expect_error(
    interpElections:::.validate_alpha(matrix(1:4, 2, 2), 2),
    "not a matrix"
  )
})

test_that(".validate_alpha rejects NA/NaN/Inf alpha", {
  expect_error(
    interpElections:::.validate_alpha(c(1, NA), 2),
    "must not contain NA"
  )
  expect_error(
    interpElections:::.validate_alpha(c(1, NaN), 2),
    "must not contain NA"
  )
  expect_error(
    interpElections:::.validate_alpha(c(1, Inf), 2),
    "must not contain NA"
  )
})

test_that(".validate_alpha rejects negative values", {
  expect_error(
    interpElections:::.validate_alpha(c(1, -0.1), 2),
    "non-negative"
  )
})

test_that(".validate_alpha accepts valid input", {
  expect_true(interpElections:::.validate_alpha(c(0, 1.5), 2))
})
