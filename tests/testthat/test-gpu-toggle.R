test_that("use_gpu sets and retrieves options", {
  # Save current state
  old <- getOption("interpElections.use_gpu")

  # Disable GPU (should always work)
  prev <- use_gpu(enable = FALSE)
  expect_false(getOption("interpElections.use_gpu"))

  # Restore
  options(interpElections.use_gpu = old)
})

test_that("use_gpu returns previous settings invisibly", {
  old <- getOption("interpElections.use_gpu")

  prev <- use_gpu(enable = FALSE)
  expect_type(prev, "list")
  expect_true("use_gpu" %in% names(prev))
  expect_true("device" %in% names(prev))
  expect_true("dtype" %in% names(prev))

  options(interpElections.use_gpu = old)
})

test_that("optimize_alpha use_gpu=FALSE uses torch CPU", {
  skip_if_not_installed("torch")

  set.seed(1)
  t_mat <- matrix(runif(8, 1, 10), nrow = 4)
  p_mat <- matrix(runif(4), nrow = 4)
  s_mat <- matrix(runif(2), nrow = 2)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_steps = 50L,
    verbose = FALSE
  )
  expect_s3_class(result, "interpElections_optim")
  expect_true(grepl("cpu", result$method))
})

test_that("GPU detection functions handle missing torch gracefully", {
  # .gpu_available should return FALSE if torch is not loaded
  # (we can't easily test this without unloading torch)
  expect_type(interpElections:::.gpu_available(), "logical")
  expect_type(interpElections:::.detect_device(), "character")
})
