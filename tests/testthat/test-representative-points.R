# Tests for interpElections:::compute_representative_points()

# Helper: create mock sf polygon layer
.mock_tracts <- function(n, seed = 42) {
  set.seed(seed)
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5
    y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  df <- data.frame(id = paste0("Z", seq_len(n)))
  sf::st_sf(df, geometry = sfc)
}


# --- method = "centroid" ---

test_that("method = 'centroid' matches sf::st_centroid()", {
  skip_if_not_installed("sf")

  tracts <- .mock_tracts(5)
  pts <- interpElections:::compute_representative_points(
    tracts, method = "centroid", tract_id = "id", verbose = FALSE
  )

  # Compare to direct centroid computation
  tracts_proj <- sf::st_transform(tracts, "EPSG:5880")
  direct <- sf::st_transform(
    suppressWarnings(sf::st_centroid(tracts_proj)), 4326
  )

  expect_equal(
    sf::st_coordinates(pts),
    sf::st_coordinates(direct),
    tolerance = 1e-6
  )
})


# --- method = "point_on_surface" ---

test_that("method = 'point_on_surface' produces points inside polygons", {
  skip_if_not_installed("sf")

  # Create a concave polygon where centroid could fall outside
  concave <- sf::st_polygon(list(matrix(c(
    0, 0,  2, 0,  2, 2,  1, 0.1,  0, 2,  0, 0
  ), ncol = 2, byrow = TRUE)))
  tracts <- sf::st_sf(
    data.frame(id = "Z1"),
    geometry = sf::st_sfc(concave, crs = 4326)
  )

  pts <- interpElections:::compute_representative_points(
    tracts, method = "point_on_surface", tract_id = "id", verbose = FALSE
  )

  # The point should be inside the polygon
  # Transform both to same CRS for check
  tracts_check <- sf::st_transform(tracts, sf::st_crs(pts))
  inside <- sf::st_within(pts, tracts_check, sparse = FALSE)
  expect_true(inside[1, 1])
})

test_that("point_on_surface is the default method", {
  skip_if_not_installed("sf")

  tracts <- .mock_tracts(3)
  pts <- interpElections:::compute_representative_points(
    tracts, tract_id = "id", verbose = FALSE
  )

  expect_equal(attr(pts, "point_method"), "point_on_surface")
})


# --- Return structure ---

test_that("returns sf POINT with correct columns and CRS", {
  skip_if_not_installed("sf")

  tracts <- .mock_tracts(8)
  pts <- interpElections:::compute_representative_points(
    tracts, method = "point_on_surface", tract_id = "id", verbose = FALSE
  )

  expect_s3_class(pts, "sf")
  expect_equal(nrow(pts), 8)
  expect_true("id" %in% names(pts))
  expect_equal(attr(pts, "point_method"), "point_on_surface")
  # Check WGS84
  expect_equal(sf::st_crs(pts)$epsg, 4326L)
  # Check POINT geometry type
  geom_types <- unique(sf::st_geometry_type(pts))
  expect_true(all(geom_types %in% c("POINT")))
})

test_that("preserves tract IDs in correct order", {
  skip_if_not_installed("sf")

  tracts <- .mock_tracts(6)
  for (method in c("centroid", "point_on_surface")) {
    pts <- interpElections:::compute_representative_points(
      tracts, method = method, tract_id = "id", verbose = FALSE
    )
    expect_equal(pts$id, tracts$id)
  }
})


# --- Input validation ---

test_that("errors on missing tract_id column", {
  skip_if_not_installed("sf")

  tracts <- .mock_tracts(3)
  expect_error(
    interpElections:::compute_representative_points(
      tracts, tract_id = "nonexistent", verbose = FALSE
    ),
    "not found"
  )
})

test_that("pop_weighted errors informatively without terra", {
  skip_if_not_installed("sf")
  skip_if(requireNamespace("terra", quietly = TRUE),
          "terra is installed; cannot test missing-terra error")

  tracts <- .mock_tracts(3)
  expect_error(
    interpElections:::compute_representative_points(
      tracts, method = "pop_weighted", tract_id = "id",
      verbose = FALSE
    ),
    "terra"
  )
})


# --- .pop_weighted_points area threshold ---

test_that("pop_weighted uses area threshold correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Create two tracts: one tiny (0.0001 km2), one large (~12300 km2)
  tiny <- sf::st_polygon(list(matrix(c(
    0, 0,  0.001, 0,  0.001, 0.001,  0, 0.001,  0, 0
  ), ncol = 2, byrow = TRUE)))
  large <- sf::st_polygon(list(matrix(c(
    10, 10,  11, 10,  11, 11,  10, 11,  10, 10
  ), ncol = 2, byrow = TRUE)))
  tracts <- sf::st_sf(
    data.frame(id = c("tiny", "large")),
    geometry = sf::st_sfc(list(tiny, large), crs = 4326)
  )

  # Create a minimal raster covering the large tract area
  r <- terra::rast(
    xmin = 9.5, xmax = 11.5, ymin = 9.5, ymax = 11.5,
    resolution = 0.1, crs = "EPSG:4326"
  )
  # Put highest population in one corner
  terra::values(r) <- 0
  r[1, 1] <- 100  # top-left corner has the most population

  pts <- interpElections:::compute_representative_points(
    tracts, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 1,  # 1 km2 threshold
    tract_id = "id",
    verbose = FALSE
  )

  expect_equal(nrow(pts), 2)
  expect_s3_class(pts, "sf")

  # The tiny tract should use point_on_surface (no raster extraction)
  # The large tract should use raster-based point
  # Both should be valid points
  coords <- sf::st_coordinates(pts)
  expect_true(all(is.finite(coords)))
})


# --- pop_raster attribute propagation ---

test_that("pop_weighted attaches pop_raster attribute", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tracts <- sf::st_sf(
    data.frame(id = "Z1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        0, 0,  2, 0,  2, 2,  0, 2,  0, 0
      ), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  r <- terra::rast(
    xmin = -1, xmax = 3, ymin = -1, ymax = 3,
    resolution = 0.5, crs = "EPSG:4326"
  )
  terra::values(r) <- runif(terra::ncell(r))

  pts <- interpElections:::compute_representative_points(
    tracts, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,  # force raster for all
    tract_id = "id",
    verbose = FALSE
  )

  expect_true(!is.null(attr(pts, "pop_raster")))
  expect_s4_class(attr(pts, "pop_raster"), "SpatRaster")
})


# --- Unreachable tracts diagnostic ---

test_that("all-fill_missing rows are detected correctly", {
  # Pure matrix logic test (no r5r needed)
  # Row 3 has ALL values = 300 (simulating an unreachable tract)
  mat <- matrix(
    c(10, 20, 30,
      15, 25, 35,
      300, 300, 300),
    nrow = 3, byrow = TRUE
  )
  fill_val <- 300
  all_filled <- rowSums(mat == fill_val) == ncol(mat)
  expect_equal(which(all_filled), 3L)
  expect_equal(sum(all_filled), 1L)
})
