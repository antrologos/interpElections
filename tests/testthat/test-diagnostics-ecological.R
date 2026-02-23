# Tests for plot_turnout_rates() and plot_ecological()
# Note: plot_income() requires censobr download, tested separately

.mock_eco_result <- function(n = 10, m = 5, k = 2) {
  set.seed(42)
  pop_cols <- paste0("pop_hom_", c("18_20", "21_24")[seq_len(k)])
  src_cols <- paste0("src_", seq_len(k))
  interp_names <- paste0("VAR_", 1:3)

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) tracts_df[[col]] <- rpois(n, 80)
  tracts_sf <- sf::st_sf(tracts_df,
                           geometry = sf::st_sfc(polys, crs = 4326))

  interp_mat <- matrix(runif(n * 3, 10, 200), n, 3)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 1, 4)
  K <- (tt + 1)^(-alpha); cs <- colSums(K); cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  result <- list(
    interpolated = interp_mat, alpha = alpha,
    tracts_sf = tracts_sf, sources = sources,
    optimization = list(method = "mock", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = W, time_matrix = tt,
    electoral_sf = NULL,
    code_muni = NULL, muni_boundary = NULL,
    year = 2020L, census_year = 2022L,
    dictionary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- plot_turnout_rates ---

test_that("plot_turnout_rates bracket returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_eco_result()

  p <- plot_turnout_rates(obj, type = "bracket")
  expect_s3_class(p, "ggplot")
})

test_that("plot_turnout_rates map returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_eco_result()

  p <- plot_turnout_rates(obj, type = "map")
  expect_s3_class(p, "ggplot")
})

test_that("plot_turnout_rates histogram returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_eco_result()

  p <- plot_turnout_rates(obj, type = "histogram")
  expect_s3_class(p, "ggplot")
})

test_that("plot_turnout_rates returns NULL without calib_cols", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_eco_result()
  obj$calib_cols <- NULL

  expect_message(p <- plot_turnout_rates(obj), "Calibration columns")
  expect_null(p)
})

test_that("plot_turnout_rates returns NULL without weights", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_eco_result()
  obj$weights <- NULL

  expect_message(p <- plot_turnout_rates(obj), "Weight matrix")
  expect_null(p)
})


# --- plot_ecological ---

test_that("plot_ecological scatter returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj1 <- .mock_eco_result()
  obj2 <- .mock_eco_result()
  obj2$year <- 2024L
  set.seed(99)
  obj2$interpolated <- matrix(runif(10 * 3, 10, 200), 10, 3)
  colnames(obj2$interpolated) <- paste0("VAR_", 1:3)

  p <- plot_ecological(obj1, obj2, variable = "VAR_1", type = "scatter")
  expect_s3_class(p, "ggplot")
})

test_that("plot_ecological auto-selects matching variable", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj1 <- .mock_eco_result()
  obj2 <- .mock_eco_result()

  expect_message(
    p <- plot_ecological(obj1, obj2, type = "scatter"),
    "Auto-selected"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_ecological rejects non-result object", {
  skip_if_not_installed("sf")
  obj <- .mock_eco_result()

  expect_error(
    plot_ecological(obj, list(a = 1)),
    "interpElections_result"
  )
})


# --- plot_income ---

test_that("plot_income requires Brazilian result", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_eco_result()
  # code_muni is NULL by default in mock

  expect_message(
    p <- plot_income(obj, variable = "VAR_1"),
    "requires a Brazilian result"
  )
  expect_null(p)
})
