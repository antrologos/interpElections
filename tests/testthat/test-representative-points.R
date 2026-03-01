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

test_that("all-NA rows are detected correctly", {
  # Pure matrix logic test (no r5r needed)
  # Row 3 has ALL values = NA (simulating an unreachable tract)
  mat <- matrix(
    c(10, 20, 30,
      15, 25, 35,
      NA, NA, NA),
    nrow = 3, byrow = TRUE
  )
  all_na <- rowSums(!is.na(mat)) == 0L
  expect_equal(which(all_na), 3L)
  expect_equal(sum(all_na), 1L)
})


# --- Road tier classification ---

test_that(".road_tier classifies highway types correctly", {
  road_tier <- interpElections:::.road_tier

  # Tier 1: main municipal roads
  expect_equal(road_tier("primary"), 1L)
  expect_equal(road_tier("secondary_link"), 1L)
  expect_equal(road_tier("tertiary"), 1L)
  expect_equal(road_tier("residential"), 1L)
  expect_equal(road_tier("living_street"), 1L)


  # Tier 2: usually connected
  expect_equal(road_tier("unclassified"), 2L)
  expect_equal(road_tier("service"), 2L)

  # Tier 3: often disconnected
  expect_equal(road_tier("track"), 3L)
  expect_equal(road_tier("path"), 3L)
  expect_equal(road_tier("footway"), 3L)
  expect_equal(road_tier("cycleway"), 3L)
  expect_equal(road_tier("pedestrian"), 3L)

  # Unknown types default to tier 3
  expect_equal(road_tier("construction"), 3L)
  expect_equal(road_tier("something_unknown"), 3L)

  # Vectorized
  result <- road_tier(c("primary", "service", "track"))
  expect_equal(result, c(1L, 2L, 3L))
})

test_that("pop_weighted prefers tier 1 roads over tier 3", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Create a large tract (~12000 km2) with a single populated cluster
  # spanning several cells. A tier 3 road (track) and a tier 1 road
  # (residential) are both within 200m of cells in the cluster.
  # The algorithm should select tier 1.
  tract <- sf::st_sf(
    data.frame(id = "Z1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        -45.5, -21.5,  -44.5, -21.5,  -44.5, -20.5,  -45.5, -20.5,  -45.5, -21.5
      ), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  r <- terra::rast(
    xmin = -45.5, xmax = -44.5, ymin = -21.5, ymax = -20.5,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0
  # One contiguous cluster spanning 5 cells in row 1
  r[1, 1] <- 100
  r[1, 2] <- 90
  r[1, 3] <- 50
  r[1, 4] <- 50
  r[1, 5] <- 80

  # Track road near top-left cells, residential road near top-center cell
  track_road <- sf::st_sf(
    highway = "track",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-45.45, -20.55, -45.35, -20.55), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  residential_road <- sf::st_sf(
    highway = "residential",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-45.05, -20.55, -44.95, -20.55), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  roads <- rbind(track_road, residential_road)

  pts <- interpElections:::compute_representative_points(
    tract, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    osm_roads = roads,
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  expect_true(!is.null(diag))
  expect_true("Z1" %in% names(diag))

  # Should use tier 1 road (residential), not tier 3 (track)
  expect_equal(diag[["Z1"]]$road_tier, 1L)
  expect_true(diag[["Z1"]]$near_road)
})

test_that("pop_weighted falls back to tier 2 when no tier 1 roads exist", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tract <- sf::st_sf(
    data.frame(id = "Z1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        -45.5, -21.5,  -44.5, -21.5,  -44.5, -20.5,  -45.5, -20.5,  -45.5, -21.5
      ), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  r <- terra::rast(
    xmin = -45.5, xmax = -44.5, ymin = -21.5, ymax = -20.5,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0
  r[1, 1] <- 100
  r[1, 2] <- 50

  # Only tier 2 road (service) near the cluster
  service_road <- sf::st_sf(
    highway = "service",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-45.45, -20.55, -45.35, -20.55), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )

  pts <- interpElections:::compute_representative_points(
    tract, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    osm_roads = service_road,
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  expect_true(!is.null(diag))
  # Should fall back to tier 2
  expect_equal(diag[["Z1"]]$road_tier, 2L)
  expect_true(diag[["Z1"]]$near_road)
})

test_that("road_tier is NA when no roads are provided", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tract <- sf::st_sf(
    data.frame(id = "Z1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        -45.5, -21.5,  -44.5, -21.5,  -44.5, -20.5,  -45.5, -20.5,  -45.5, -21.5
      ), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  r <- terra::rast(
    xmin = -45.5, xmax = -44.5, ymin = -21.5, ymax = -20.5,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0
  r[1, 1] <- 100

  pts <- interpElections:::compute_representative_points(
    tract, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    osm_roads = NULL,
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  expect_true(!is.null(diag))
  expect_true(is.na(diag[["Z1"]]$road_tier))
  expect_false(diag[["Z1"]]$near_road)
  expect_false(diag[["Z1"]]$has_roads)
})
