# Tests for interpolate_election_br(), .br_resolve_muni(), .br_match_calibration()

test_that(".br_resolve_muni resolves Sao Paulo correctly", {
  info <- interpElections:::.br_resolve_muni("3550308")
  expect_equal(info$code_muni_ibge, "3550308")
  expect_equal(info$code_muni_tse, "71072")
  expect_equal(info$uf, "SP")
  expect_equal(info$nome_municipio, "SÃO PAULO")
})

test_that(".br_resolve_muni resolves Boa Vista correctly", {
  info <- interpElections:::.br_resolve_muni("1400100")
  expect_equal(info$code_muni_ibge, "1400100")
  expect_equal(info$code_muni_tse, "03018")
  expect_equal(info$uf, "RR")
})

test_that(".br_resolve_muni resolves Rio de Janeiro correctly", {
  info <- interpElections:::.br_resolve_muni("3304557")
  expect_equal(info$code_muni_ibge, "3304557")
  expect_equal(info$uf, "RJ")
})

test_that(".br_resolve_muni errors on invalid code", {
  expect_error(
    interpElections:::.br_resolve_muni("9999999"),
    "not found"
  )
})

test_that("census year auto-selection works correctly", {
  # Test the auto-selection logic inline (it's inside interpolate_election_br)
  auto_select <- function(year) {
    if (year <= 2004) 2000L
    else if (year <= 2016) 2010L
    else 2022L
  }

  expect_equal(auto_select(2000), 2000L)
  expect_equal(auto_select(2004), 2000L)
  expect_equal(auto_select(2008), 2010L)
  expect_equal(auto_select(2012), 2010L)
  expect_equal(auto_select(2016), 2010L)
  expect_equal(auto_select(2020), 2022L)
  expect_equal(auto_select(2024), 2022L)
})


# --- .br_match_calibration tests ---

# Helper to build mock tracts/electoral sf for calibration matching
.mock_tracts_for_calib <- function(n, census_year) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  df <- data.frame(id = seq_len(n))

  if (census_year %in% c(2000, 2010)) {
    df$pop_18_20 <- rpois(n, 30)
    df$pop_21_24 <- rpois(n, 40)
    df$pop_25_29 <- rpois(n, 50)
    df$pop_30_39 <- rpois(n, 80)
    df$pop_40_49 <- rpois(n, 70)
    df$pop_50_59 <- rpois(n, 60)
    df$pop_60_69 <- rpois(n, 40)
  } else {
    # Census 2022
    df$pop_15_19 <- rpois(n, 50)
    df$pop_20_24 <- rpois(n, 50)
    df$pop_25_29 <- rpois(n, 50)
    df$pop_30_39 <- rpois(n, 80)
    df$pop_40_49 <- rpois(n, 70)
    df$pop_50_59 <- rpois(n, 60)
    df$pop_60_69 <- rpois(n, 40)
  }
  sf::st_sf(df, geometry = sfc)
}

.mock_electoral_for_calib <- function(m) {
  pts <- sf::st_sfc(
    lapply(seq_len(m), function(i) sf::st_point(c(runif(1), runif(1)))),
    crs = 4326
  )
  df <- data.frame(
    id = seq_len(m),
    votantes_18_20 = rpois(m, 30),
    votantes_21_24 = rpois(m, 40),
    votantes_25_29 = rpois(m, 50),
    votantes_30_34 = rpois(m, 40),
    votantes_35_39 = rpois(m, 40),
    votantes_40_44 = rpois(m, 35),
    votantes_45_49 = rpois(m, 35),
    votantes_50_54 = rpois(m, 30),
    votantes_55_59 = rpois(m, 30),
    votantes_60_64 = rpois(m, 20),
    votantes_65_69 = rpois(m, 20)
  )
  sf::st_sf(df, geometry = pts)
}


test_that(".br_match_calibration for census 2010 produces 7 matched groups", {
  skip_if_not_installed("sf")

  set.seed(10)
  tracts <- .mock_tracts_for_calib(8, 2010)
  elec <- .mock_electoral_for_calib(5)

  result <- interpElections:::.br_match_calibration(2010, tracts, elec)

  expect_equal(length(result$calib_zones), 7)
  expect_equal(length(result$calib_sources), 7)

  # 7 calibration zone columns
  expect_equal(result$calib_zones, c(
    "pop_18_20", "pop_21_24", "pop_25_29",
    "pop_30_39", "pop_40_49", "pop_50_59", "pop_60_69"
  ))

  # 7 calibration source columns (some aggregated)
  expect_equal(result$calib_sources, c(
    "votantes_18_20", "votantes_21_24", "votantes_25_29",
    "votantes_30_39", "votantes_40_49", "votantes_50_59",
    "votantes_60_69"
  ))

  # Aggregated votantes columns should exist in returned electoral_sf
  elec_df <- sf::st_drop_geometry(result$electoral_sf)
  expect_true("votantes_30_39" %in% names(elec_df))
  expect_true("votantes_40_49" %in% names(elec_df))
  expect_true("votantes_50_59" %in% names(elec_df))
  expect_true("votantes_60_69" %in% names(elec_df))
})

test_that(".br_match_calibration for census 2000 produces same groups as 2010", {
  skip_if_not_installed("sf")

  set.seed(20)
  tracts <- .mock_tracts_for_calib(6, 2000)
  elec <- .mock_electoral_for_calib(4)

  result <- interpElections:::.br_match_calibration(2000, tracts, elec)

  expect_equal(result$calib_zones, c(
    "pop_18_20", "pop_21_24", "pop_25_29",
    "pop_30_39", "pop_40_49", "pop_50_59", "pop_60_69"
  ))
})

test_that(".br_match_calibration for census 2022 merges 15-24 bracket", {
  skip_if_not_installed("sf")

  set.seed(30)
  tracts <- .mock_tracts_for_calib(8, 2022)
  elec <- .mock_electoral_for_calib(5)

  result <- interpElections:::.br_match_calibration(2022, tracts, elec)

  # 6 groups for 2022 (15-24 merged)
  expect_equal(length(result$calib_zones), 6)
  expect_equal(length(result$calib_sources), 6)

  expect_equal(result$calib_zones, c(
    "pop_15_24", "pop_25_29", "pop_30_39",
    "pop_40_49", "pop_50_59", "pop_60_69"
  ))

  expect_equal(result$calib_sources, c(
    "votantes_15_24", "votantes_25_29", "votantes_30_39",
    "votantes_40_49", "votantes_50_59", "votantes_60_69"
  ))

  # pop_15_24 should be sum of pop_15_19 and pop_20_24
  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  orig_df <- sf::st_drop_geometry(tracts)
  expect_equal(
    tracts_df$pop_15_24,
    orig_df$pop_15_19 + orig_df$pop_20_24
  )

  # votantes_15_24 should be sum of votantes_18_20 and votantes_21_24
  elec_df <- sf::st_drop_geometry(result$electoral_sf)
  orig_elec <- sf::st_drop_geometry(elec)
  expect_equal(
    elec_df$votantes_15_24,
    orig_elec$votantes_18_20 + orig_elec$votantes_21_24
  )
})

test_that(".br_match_calibration aggregation is numerically correct", {
  skip_if_not_installed("sf")

  set.seed(50)
  tracts <- .mock_tracts_for_calib(4, 2010)
  elec <- .mock_electoral_for_calib(3)

  result <- interpElections:::.br_match_calibration(2010, tracts, elec)
  elec_df <- sf::st_drop_geometry(result$electoral_sf)
  orig_df <- sf::st_drop_geometry(elec)

  # votantes_30_39 = votantes_30_34 + votantes_35_39
  expect_equal(
    elec_df$votantes_30_39,
    orig_df$votantes_30_34 + orig_df$votantes_35_39
  )

  # votantes_60_69 = votantes_60_64 + votantes_65_69
  expect_equal(
    elec_df$votantes_60_69,
    orig_df$votantes_60_64 + orig_df$votantes_65_69
  )
})

test_that(".safe_sum handles missing columns gracefully", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))

  # Both present
  expect_equal(interpElections:::.safe_sum(df, c("a", "b")), c(5, 7, 9))
  # Only one present
  expect_equal(interpElections:::.safe_sum(df, c("a", "nonexistent")), c(1, 2, 3))
  # None present
  expect_equal(interpElections:::.safe_sum(df, c("x", "y")), c(0, 0, 0))
})

test_that("print.interpElections_result works for Brazilian result", {
  # Create a minimal mock object with unified class
  mat <- matrix(1:6, 3, 2)
  colnames(mat) <- c("CAND_13", "CAND_22")
  obj <- list(
    interpolated = mat,
    alpha = c(1, 1.5, 2),
    sources = data.frame(id = 1:4, CAND_13 = 1:4, CAND_22 = 5:8),
    optimization = list(method = "cpu_lbfgsb", value = 100),
    interp_cols = c("CAND_13", "CAND_22"),
    code_muni = "1400100",
    year = 2008L,
    census_year = 2010L
  )
  class(obj) <- "interpElections_result"
  expect_output(print(obj), "Brazilian election")
  expect_output(print(obj), "1400100")
  expect_output(print(obj), "census 2010")
  expect_output(print(obj), "Access interpolated sf")
})
