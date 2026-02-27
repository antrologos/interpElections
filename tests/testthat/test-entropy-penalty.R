# Tests for entropy penalty (entropy_mu parameter)

test_that("optim_control accepts entropy_mu = 0 (default)", {
  ctrl <- optim_control()
  expect_equal(ctrl$entropy_mu, 0)
})

test_that("optim_control accepts entropy_mu > 0", {
  ctrl <- optim_control(entropy_mu = 1)
  expect_equal(ctrl$entropy_mu, 1)
  ctrl2 <- optim_control(entropy_mu = 0.5)
  expect_equal(ctrl2$entropy_mu, 0.5)
})

test_that("optim_control rejects negative entropy_mu", {
  expect_error(optim_control(entropy_mu = -1), "entropy_mu.*non-negative")
})

test_that("optim_control rejects non-numeric entropy_mu", {
  expect_error(optim_control(entropy_mu = "high"), "entropy_mu.*non-negative")
})

test_that("print method shows entropy_mu only when > 0", {
  ctrl0 <- optim_control(entropy_mu = 0)
  out0 <- capture.output(print(ctrl0))
  expect_false(any(grepl("entropy_mu", out0)))

  ctrl1 <- optim_control(entropy_mu = 1.5)
  out1 <- capture.output(print(ctrl1))
  expect_true(any(grepl("entropy_mu", out1)))
})

test_that("optimize_alpha with entropy_mu = 0 matches default behavior", {
  skip_if_not_installed("torch")
  set.seed(42)
  n <- 5; m <- 4; k <- 2
  tt <- matrix(abs(rnorm(n * m, 10, 5)), n, m)
  pop <- matrix(rpois(n * k, 100), n, k)
  src <- matrix(rpois(m * k, 80), m, k)

  r0 <- optimize_alpha(tt, pop, src,
    optim = optim_control(max_epochs = 20L, entropy_mu = 0),
    verbose = FALSE)
  expect_true(is.finite(r0$value))
  expect_null(r0$mean_eff_sources)
})

test_that("optimize_alpha with entropy_mu > 0 returns mean_eff_sources", {
  skip_if_not_installed("torch")
  set.seed(42)
  n <- 5; m <- 4; k <- 2
  tt <- matrix(abs(rnorm(n * m, 10, 5)), n, m)
  pop <- matrix(rpois(n * k, 100), n, k)
  src <- matrix(rpois(m * k, 80), m, k)

  r1 <- optimize_alpha(tt, pop, src,
    optim = optim_control(max_epochs = 20L, entropy_mu = 1),
    verbose = FALSE)
  expect_true(is.finite(r1$value))
  expect_true(is.numeric(r1$mean_eff_sources))
  expect_true(is.finite(r1$mean_eff_sources))
  expect_true(r1$mean_eff_sources > 0)
})

# --- Tests for target_eff_src / dual ascent ---

test_that("optim_control accepts target_eff_src = NULL (default)", {
  ctrl <- optim_control()
  expect_null(ctrl$target_eff_src)
})

test_that("optim_control accepts target_eff_src > 1", {
  ctrl <- optim_control(target_eff_src = 10)
  expect_equal(ctrl$target_eff_src, 10)
  ctrl2 <- optim_control(target_eff_src = 1.5)
  expect_equal(ctrl2$target_eff_src, 1.5)
})

test_that("optim_control rejects target_eff_src <= 1", {
  expect_error(optim_control(target_eff_src = 1), "target_eff_src.*> 1")
  expect_error(optim_control(target_eff_src = 0.5), "target_eff_src.*> 1")
  expect_error(optim_control(target_eff_src = 0), "target_eff_src.*> 1")
  expect_error(optim_control(target_eff_src = -1), "target_eff_src.*> 1")
})

test_that("optim_control rejects non-numeric target_eff_src", {
  expect_error(optim_control(target_eff_src = "ten"), "target_eff_src.*> 1")
})

test_that("optim_control accepts dual_eta in (0, 1]", {
  ctrl <- optim_control(dual_eta = 0.1)
  expect_equal(ctrl$dual_eta, 0.1)
  ctrl2 <- optim_control(dual_eta = 1)
  expect_equal(ctrl2$dual_eta, 1)
})

test_that("optim_control rejects invalid dual_eta", {
  expect_error(optim_control(dual_eta = 0), "dual_eta.*\\(0, 1\\]")
  expect_error(optim_control(dual_eta = -0.1), "dual_eta.*\\(0, 1\\]")
  expect_error(optim_control(dual_eta = 1.5), "dual_eta.*\\(0, 1\\]")
})

test_that("print method shows target_eff_src when dual ascent active", {
  ctrl <- optim_control(target_eff_src = 10)
  out <- capture.output(print(ctrl))
  expect_true(any(grepl("target_eff_src", out)))
  expect_true(any(grepl("dual ascent", out, ignore.case = TRUE)))

  ctrl0 <- optim_control()
  out0 <- capture.output(print(ctrl0))
  expect_false(any(grepl("target_eff_src", out0)))
})

test_that("optimize_alpha with target_eff_src runs and returns dual ascent fields", {
  skip_if_not_installed("torch")
  set.seed(42)
  n <- 5; m <- 4; k <- 2
  tt <- matrix(abs(rnorm(n * m, 10, 5)), n, m)
  pop <- matrix(rpois(n * k, 100), n, k)
  src <- matrix(rpois(m * k, 80), m, k)

  r <- optimize_alpha(tt, pop, src,
    optim = optim_control(max_epochs = 50L, target_eff_src = 2),
    verbose = FALSE)
  expect_true(is.finite(r$value))
  expect_true(is.numeric(r$entropy_mu_final))
  expect_true(r$entropy_mu_final > 0)
  expect_true(is.numeric(r$entropy_mu_history))
  expect_true(length(r$entropy_mu_history) == r$epochs)
  expect_equal(r$target_eff_src, 2)
  expect_true(is.numeric(r$mean_eff_sources))
  expect_true(r$mean_eff_sources > 0)
})

test_that("optimize_alpha with entropy_mu = 0 (no entropy) returns NULL target_eff_src", {
  skip_if_not_installed("torch")
  set.seed(42)
  n <- 5; m <- 4; k <- 2
  tt <- matrix(abs(rnorm(n * m, 10, 5)), n, m)
  pop <- matrix(rpois(n * k, 100), n, k)
  src <- matrix(rpois(m * k, 80), m, k)

  r <- optimize_alpha(tt, pop, src,
    optim = optim_control(max_epochs = 20L, entropy_mu = 0),
    verbose = FALSE)
  expect_null(r$target_eff_src)
})
