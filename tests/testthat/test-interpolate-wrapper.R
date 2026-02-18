# Tests for interpolate_election() wrapper

# Helper: create mock sf polygon layer (census tracts) with population columns
.mock_tracts_sf <- function(n, pop_cols, seed = 42) {
  set.seed(seed)
  # Create simple square polygons
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5
    y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) {
    df[[col]] <- rpois(n, 80)
  }
  sf::st_sf(df, geometry = sfc)
}

# Helper: create mock sf point layer (sources) with voter columns
.mock_electoral_sf <- function(m, src_cols, extra_cols = NULL, seed = 42) {
  set.seed(seed + 1)
  pts <- sf::st_sfc(
    lapply(seq_len(m), function(i) {
      sf::st_point(c(runif(1, 0, 5), runif(1, 0, 3)))
    }),
    crs = 4326
  )
  df <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) {
    df[[col]] <- rpois(m, 160)
  }
  if (!is.null(extra_cols)) {
    for (col in extra_cols) {
      df[[col]] <- rpois(m, 50)
    }
  }
  sf::st_sf(df, geometry = pts)
}


test_that("interpolate_election() with pre-computed time_matrix matches manual pipeline", {
  skip_if_not_installed("sf")

  set.seed(42)
  n <- 10; m <- 5; k <- 3
  pop_cols <- paste0("pop_", letters[1:k])
  src_cols <- paste0("src_", letters[1:k])

  tracts <- .mock_tracts_sf(n, pop_cols)
  sources <- .mock_electoral_sf(m, src_cols)

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)

  # Manual pipeline
  pop_mat <- as.matrix(sf::st_drop_geometry(tracts)[, pop_cols])
  src_mat <- as.matrix(sf::st_drop_geometry(sources)[, src_cols])
  storage.mode(pop_mat) <- "double"
  storage.mode(src_mat) <- "double"

  opt <- optimize_alpha(tt, pop_mat, src_mat, verbose = FALSE)
  W_manual <- sinkhorn_weights(tt, opt$alpha, offset = 1,
                                row_targets = opt$row_targets)
  manual <- W_manual %*% src_mat

  # Wrapper
  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt,
    verbose = FALSE
  )

  expect_s3_class(result, "interpElections_result")
  expect_equal(result$interpolated, manual, tolerance = 1e-6)
  expect_equal(result$alpha, opt$alpha, tolerance = 1e-6)
  expect_equal(result$optimization$value, opt$value, tolerance = 1e-6)

  # Lightweight by default: heavy objects are NULL
  expect_null(result$weights)
  expect_null(result$time_matrix)
  expect_null(result$sources_sf)

  # New fields present
  expect_s3_class(result$tracts_sf, "sf")
  expect_true(is.data.frame(result$sources))
  expect_equal(result$tract_id, "zone_id")
  expect_equal(result$point_id, "point_id")
  expect_equal(result$interp_cols, src_cols)
  expect_equal(result$calib_cols$tracts, pop_cols)
  expect_equal(result$calib_cols$sources, src_cols)

  # Brazilian metadata NULL in generic call
  expect_null(result$code_muni)
  expect_null(result$year)
  expect_null(result$census_year)
  expect_null(result$what)
  expect_null(result$pop_data)
})

test_that("interpolate_election() with keep includes heavy objects", {
  skip_if_not_installed("sf")

  set.seed(42)
  n <- 10; m <- 5; k <- 3
  pop_cols <- paste0("pop_", letters[1:k])
  src_cols <- paste0("src_", letters[1:k])

  tracts <- .mock_tracts_sf(n, pop_cols)
  sources <- .mock_electoral_sf(m, src_cols)
  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt,
    keep = c("weights", "time_matrix"),
    verbose = FALSE
  )

  expect_true(is.matrix(result$weights))
  expect_true(is.matrix(result$time_matrix))
  expect_equal(dim(result$weights), c(n, m))
  expect_equal(dim(result$time_matrix), c(n, m))
})

test_that("interpolate_election() keep sources_sf returns sf object", {
  skip_if_not_installed("sf")

  set.seed(42)
  n <- 10; m <- 5; k <- 3
  pop_cols <- paste0("pop_", letters[1:k])
  src_cols <- paste0("src_", letters[1:k])

  tracts <- .mock_tracts_sf(n, pop_cols)
  sources <- .mock_electoral_sf(m, src_cols)
  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt,
    keep = "sources_sf",
    verbose = FALSE
  )

  expect_s3_class(result$sources_sf, "sf")
  expect_equal(nrow(result$sources_sf), m)
  # Without keep, should be NULL
  expect_null(result$weights)
  expect_null(result$time_matrix)
})

test_that("interpolate_election() tracts_sf contains interpolated columns", {
  skip_if_not_installed("sf")

  set.seed(42)
  n <- 10; m <- 5
  pop_cols <- c("pop_a", "pop_b")
  src_cols <- c("src_a", "src_b")
  extra <- c("CAND_X", "CAND_Y")

  tracts <- .mock_tracts_sf(n, pop_cols)
  sources <- .mock_electoral_sf(m, src_cols, extra_cols = extra)
  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    interp_sources = extra,
    time_matrix = tt,
    verbose = FALSE
  )

  # tracts_sf should have the interpolated columns
  expect_true("CAND_X" %in% names(result$tracts_sf))
  expect_true("CAND_Y" %in% names(result$tracts_sf))
  expect_equal(result$interp_cols, c("CAND_X", "CAND_Y"))

  # values should match the interpolated matrix
  expect_equal(
    result$tracts_sf$CAND_X,
    as.numeric(result$interpolated[, "CAND_X"])
  )
})

test_that("interpolate_election() with pre-supplied alpha skips optimization", {
  skip_if_not_installed("sf")

  set.seed(1)
  n <- 8; m <- 4; k <- 2
  pop_cols <- c("pop_young", "pop_old")
  src_cols <- c("src_young", "src_old")

  tracts <- .mock_tracts_sf(n, pop_cols, seed = 1)
  sources <- .mock_electoral_sf(m, src_cols, seed = 1)
  tt <- matrix(abs(rnorm(n * m, 30, 10)), n, m)
  alpha_fixed <- rep(1.5, n)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt,
    alpha = alpha_fixed,
    verbose = FALSE
  )

  expect_null(result$optimization)
  expect_equal(result$alpha, alpha_fixed)

  # Should match direct call with sinkhorn weights
  src_mat <- as.matrix(sf::st_drop_geometry(sources)[, src_cols])
  storage.mode(src_mat) <- "double"
  W_direct <- sinkhorn_weights(tt, alpha_fixed, offset = 1,
                                row_targets = result$row_targets)
  direct <- W_direct %*% src_mat
  expect_equal(result$interpolated, direct)
})

test_that("interpolate_election() with explicit interp_sources works", {
  skip_if_not_installed("sf")

  set.seed(7)
  n <- 8; m <- 5
  pop_cols <- c("pop_young", "pop_old")
  src_cols <- c("vot_young", "vot_old")
  extra <- c("CAND_A", "CAND_B")

  tracts <- .mock_tracts_sf(n, pop_cols, seed = 7)
  sources <- .mock_electoral_sf(m, src_cols, extra_cols = extra, seed = 7)
  tt <- matrix(abs(rnorm(n * m, 40, 15)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    interp_sources = extra,
    time_matrix = tt,
    verbose = FALSE
  )

  expect_equal(ncol(result$interpolated), 2)
  expect_equal(colnames(result$interpolated), c("CAND_A", "CAND_B"))
  expect_equal(nrow(result$interpolated), n)
})

test_that("interpolate_election() auto-detects interp columns as non-calib numeric", {
  skip_if_not_installed("sf")

  set.seed(3)
  n <- 6; m <- 4
  pop_cols <- c("pop_a", "pop_b")
  src_cols <- c("src_a", "src_b")
  extra <- c("votes", "turnout")

  tracts <- .mock_tracts_sf(n, pop_cols, seed = 3)
  sources <- .mock_electoral_sf(m, src_cols, extra_cols = extra, seed = 3)
  tt <- matrix(abs(rnorm(n * m, 30, 10)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt,
    verbose = FALSE
  )

  # Should auto-detect votes and turnout as interp columns
  expect_equal(ncol(result$interpolated), 2)
  expect_true(all(c("votes", "turnout") %in% colnames(result$interpolated)))
})

test_that("interpolate_election() filters census tracts with min_pop", {
  skip_if_not_installed("sf")

  set.seed(99)
  n <- 10; m <- 4
  pop_cols <- c("pop_a", "pop_b")
  src_cols <- c("src_a", "src_b")

  tracts <- .mock_tracts_sf(n, pop_cols, seed = 99)
  sources <- .mock_electoral_sf(m, src_cols, seed = 99)

  # Zero out population for first 3 tracts
  tracts$pop_a[1:3] <- 0
  tracts$pop_b[1:3] <- 0

  tt <- matrix(abs(rnorm(n * m, 40, 10)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt[4:10, ],  # match filtered tracts
    min_pop = 1,
    verbose = FALSE
  )

  expect_equal(nrow(result$interpolated), 7)
})

test_that("interpolate_election() errors on invalid inputs", {
  skip_if_not_installed("sf")

  tracts <- .mock_tracts_sf(3, "pop_a")
  sources <- .mock_electoral_sf(2, "src_a")

  # Missing calib_tracts column
  expect_error(
    interpolate_election(
      tracts, sources, "zone_id", "point_id",
      calib_tracts = "nonexistent", calib_sources = "src_a",
      time_matrix = matrix(1, 3, 2), verbose = FALSE
    ),
    "not found"
  )

  # Mismatched calib lengths
  expect_error(
    interpolate_election(
      tracts, sources, "zone_id", "point_id",
      calib_tracts = c("pop_a", "pop_a"), calib_sources = "src_a",
      time_matrix = matrix(1, 3, 2), verbose = FALSE
    ),
    "same length"
  )

  # Not sf objects
  expect_error(
    interpolate_election(
      data.frame(x = 1), sources, "zone_id", "point_id",
      calib_tracts = "pop_a", calib_sources = "src_a",
      time_matrix = matrix(1, 1, 2), verbose = FALSE
    ),
    "sf object"
  )

  # Dimension mismatch (sources must overlap tracts spatially)
  near_pts <- sf::st_sfc(
    sf::st_point(c(0.5, 0.5)),
    sf::st_point(c(1.5, 0.5)),
    crs = 4326
  )
  near_sources <- sf::st_sf(
    data.frame(point_id = c("P1", "P2"), src_a = c(10, 20)),
    geometry = near_pts
  )
  expect_error(
    interpolate_election(
      tracts, near_sources, "zone_id", "point_id",
      calib_tracts = "pop_a", calib_sources = "src_a",
      time_matrix = matrix(1, 5, 2),  # 5 rows but 3 tracts
      verbose = FALSE
    ),
    "rows"
  )

  # Spatial mismatch: tracts and sources in completely different locations
  far_pts <- sf::st_sfc(
    sf::st_point(c(100, 50)),
    sf::st_point(c(101, 51)),
    crs = 4326
  )
  far_sources <- sf::st_sf(
    data.frame(point_id = c("P1", "P2"), src_a = c(10, 20)),
    geometry = far_pts
  )
  expect_error(
    interpolate_election(
      tracts, far_sources, "zone_id", "point_id",
      calib_tracts = "pop_a", calib_sources = "src_a",
      time_matrix = matrix(1, 3, 2),
      verbose = FALSE
    ),
    "Spatial mismatch"
  )
})

test_that("print.interpElections_result works", {
  skip_if_not_installed("sf")

  set.seed(1)
  n <- 5; m <- 3; k <- 2
  pop_cols <- c("pop_a", "pop_b")
  src_cols <- c("src_a", "src_b")

  tracts <- .mock_tracts_sf(n, pop_cols, seed = 1)
  sources <- .mock_electoral_sf(m, src_cols, seed = 1)
  tt <- matrix(abs(rnorm(n * m, 30, 10)), n, m)

  result <- interpolate_election(
    tracts_sf = tracts,
    electoral_sf = sources,
    tract_id = "zone_id",
    point_id = "point_id",
    calib_tracts = pop_cols,
    calib_sources = src_cols,
    time_matrix = tt,
    verbose = FALSE
  )

  expect_output(print(result), "interpElections result")
  expect_output(print(result), "Census tracts:")
  expect_output(print(result), "result\\$tracts_sf")
  expect_output(print(result), "Methods:")
})
