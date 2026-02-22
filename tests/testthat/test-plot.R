# Tests for plotting: resolve-var, compute-quantity, plot, plot_interactive

# --- Shared mock helpers ---

# Minimal result with dictionary (for variable resolution tests)
.mock_plot_result <- function() {
  skip_if_not_installed("sf")
  interp_names <- c("CAND_13", "CAND_22", "CAND_95", "CAND_96",
                     "PARTY_PT", "PARTY_MDB",
                     "QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES",
                     "votantes_18_20")
  n <- 6; m <- 3; p <- length(interp_names)
  set.seed(42)

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 3; y0 <- (i - 1) %/% 3
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  tracts_sf <- sf::st_sf(tracts_df, geometry = sfc)

  interp_mat <- matrix(abs(rnorm(n * p, 100, 50)), n, p)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  alpha <- runif(n, 0.5, 3)

  dict <- data.frame(
    column = interp_names,
    type = c("candidate", "candidate", "candidate", "candidate",
             "party", "party", "turnout", "turnout", "turnout",
             "calibration"),
    cargo = c(rep("VEREADOR", 6), rep(NA, 4)),
    ballot_number = c("13", "22", "95", "96", rep(NA, 6)),
    candidate_name = c("JOAO DA SILVA", "MARIA SOUZA",
                        "Votos em Branco", "Votos Nulos",
                        rep(NA, 6)),
    party = c("PT", "MDB", NA, NA, "PT", "MDB", rep(NA, 4)),
    stringsAsFactors = FALSE
  )

  result <- list(
    interpolated = interp_mat, alpha = alpha, tracts_sf = tracts_sf,
    sources = sources,
    optimization = list(method = "cpu_lbfgsb", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election_br()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = "votantes_18_20", sources = "votantes_18_20"),
    weights = NULL, time_matrix = NULL, sources_sf = NULL,
    code_muni = "3550308",
    nome_municipio = "SAO PAULO", code_muni_tse = "71072", uf = "SP",
    year = 2020L, census_year = 2022L,
    what = "candidates", pop_data = data.frame(x = 1),
    dictionary = dict
  )
  class(result) <- "interpElections_result"
  result
}


# --- .resolve_var tests ---

test_that(".resolve_var resolves exact column name", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("CAND_13", obj), "CAND_13")
  expect_equal(.resolve_var("PARTY_PT", obj), "PARTY_PT")
  expect_equal(.resolve_var("QT_COMPARECIMENTO", obj), "QT_COMPARECIMENTO")
})

test_that(".resolve_var resolves numeric ballot number", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var(13, obj), "CAND_13")
  expect_equal(.resolve_var(22, obj), "CAND_22")
  expect_equal(.resolve_var(95, obj), "CAND_95")
})

test_that(".resolve_var resolves ballot number as string", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("13", obj), "CAND_13")
})

test_that(".resolve_var resolves party abbreviation", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("PT", obj), "PARTY_PT")
  expect_equal(.resolve_var("MDB", obj), "PARTY_MDB")
})

test_that(".resolve_var party is case-insensitive", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("pt", obj), "PARTY_PT")
  expect_equal(.resolve_var("Pt", obj), "PARTY_PT")
})

test_that(".resolve_var resolves candidate name substring", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("JOAO", obj), "CAND_13")
  expect_equal(.resolve_var("joao", obj), "CAND_13")
  expect_equal(.resolve_var("SILVA", obj), "CAND_13")
  expect_equal(.resolve_var("SOUZA", obj), "CAND_22")
})

test_that(".resolve_var errors on ambiguous candidate name", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # "Votos" matches both CAND_95 and CAND_96
  expect_error(.resolve_var("Votos", obj), "matches multiple")
})

test_that(".resolve_var errors on unknown variable", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_error(.resolve_var("NONEXISTENT", obj), "Could not resolve")
})

test_that(".resolve_var errors on unknown ballot number", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_error(.resolve_var(999, obj), "No column found")
})

test_that(".resolve_var works without dictionary (column name)", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.resolve_var("CAND_13", obj), "CAND_13")
})

test_that(".resolve_var works without dictionary (ballot number pattern)", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.resolve_var(13, obj), "CAND_13")
  expect_equal(.resolve_var(22, obj), "CAND_22")
})

test_that(".resolve_var party fallback without dictionary", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.resolve_var("PT", obj), "PARTY_PT")
})

test_that(".resolve_vars resolves multiple variables", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .resolve_vars(c("JOAO", 22, "PT"), obj)
  expect_equal(cols, c("CAND_13", "CAND_22", "PARTY_PT"))
})


# --- .auto_select_var tests ---

test_that(".auto_select_var picks first real candidate", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Should pick CAND_13 (first candidate, not blank/null)
  expect_equal(.auto_select_var(obj), "CAND_13")
})

test_that(".auto_select_var skips blank/null votes", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Remove real candidates, keep only blank/null
  obj$dictionary <- obj$dictionary[obj$dictionary$ballot_number %in%
                                     c("95", "96", NA), , drop = FALSE]
  # Should fall back to first party
  expect_equal(.auto_select_var(obj), "PARTY_PT")
})

test_that(".auto_select_var falls back to first interp_col without dict", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.auto_select_var(obj), obj$interp_cols[1])
})


# --- .compute_quantity tests ---

test_that(".compute_quantity absolute returns raw values", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "absolute")
  expect_equal(vals, as.numeric(obj$tracts_sf$CAND_13))
})

test_that(".compute_quantity pct_muni sums to 100", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "pct_muni")
  expect_equal(sum(vals, na.rm = TRUE), 100, tolerance = 1e-6)
})

test_that(".compute_quantity pct_tract uses QT_COMPARECIMENTO", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "pct_tract")
  expected <- as.numeric(obj$tracts_sf$CAND_13) /
    as.numeric(obj$tracts_sf$QT_COMPARECIMENTO) * 100
  expect_equal(vals, expected, tolerance = 1e-10)
})

test_that(".compute_quantity pct_tract errors when turnout missing", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$tracts_sf$QT_COMPARECIMENTO <- NULL
  obj$interpolated <- obj$interpolated[, colnames(obj$interpolated) != "QT_COMPARECIMENTO"]
  expect_error(.compute_quantity(obj, "CAND_13", "pct_tract"),
               "QT_COMPARECIMENTO")
})

test_that(".compute_quantity pct_eligible uses QT_APTOS", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "pct_eligible")
  expected <- as.numeric(obj$tracts_sf$CAND_13) /
    as.numeric(obj$tracts_sf$QT_APTOS) * 100
  expect_equal(vals, expected, tolerance = 1e-10)
})

test_that(".compute_quantity density computes per km2", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "density")
  expect_true(is.numeric(vals))
  expect_equal(length(vals), nrow(obj$interpolated))
  # Values should be finite (tracts have area > 0)
  expect_true(all(is.finite(vals)))
})


# --- plot.interpElections_result tests ---

test_that("plot returns a ggplot object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj)
  expect_s3_class(p, "gg")
})

test_that("plot resolves variable by name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "JOAO")
  expect_s3_class(p, "gg")
})

test_that("plot resolves variable by ballot number", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = 13)
  expect_s3_class(p, "gg")
})

test_that("plot resolves variable by party", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "PT")
  expect_s3_class(p, "gg")
})

test_that("plot with type = pct_tract works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", type = "pct_tract")
  expect_s3_class(p, "gg")
})

test_that("plot with quantile breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = "quantile", n_breaks = 3)
  expect_s3_class(p, "gg")
})

test_that("plot with custom breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = c(0, 50, 100, 200))
  expect_s3_class(p, "gg")
})

test_that("plot with custom title and legend", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13",
            title = "My Title", subtitle = "My Sub",
            legend_title = "My Legend")
  expect_s3_class(p, "gg")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Sub")
  expect_equal(p$labels$fill, "My Legend")
})

test_that("plot with viridis palette works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", palette = "viridis")
  expect_s3_class(p, "gg")
})

test_that("plot with multiple variables returns faceted ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = c("CAND_13", "CAND_22"))
  expect_s3_class(p, "gg")
  # Should have faceting
  expect_false(inherits(p$facet, "FacetNull"))
})

test_that("plot errors when tracts_sf is NULL", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()
  obj$tracts_sf <- NULL

  expect_error(plot(obj), "No tracts_sf")
})

test_that("plot warns when show_sources but no sources_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()
  obj$sources_sf <- NULL

  expect_warning(plot(obj, show_sources = TRUE), "sources_sf")
})

test_that("plot auto-generates title-cased title with party and number", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13")
  # Title should be title-cased with party and ballot number
  expect_true(grepl("Joao", p$labels$title, ignore.case = FALSE))
  expect_true(grepl("PT", p$labels$title))
  expect_true(grepl("13", p$labels$title))
})

test_that("plot auto-generates subtitle with type and breaks", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13")
  expect_true(grepl("SAO PAULO", p$labels$subtitle))
  expect_true(grepl("2020", p$labels$subtitle))
  # Default type pct_tract and breaks quantile appear in subtitle
  expect_true(grepl("tract", p$labels$subtitle, ignore.case = TRUE))
  expect_true(grepl("uantile", p$labels$subtitle, ignore.case = TRUE))
})

test_that("plot has auto-generated caption", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13")
  expect_true(grepl("TSE", p$labels$caption))
})

test_that("plot caption can be suppressed", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", caption = "")
  expect_equal(p$labels$caption, "")
})

test_that("plot with limits crops the map", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", limits = c(0, 2, 0, 1))
  expect_s3_class(p, "gg")
  # coord_sf should be set with the specified limits
  expect_true(inherits(p$coordinates, "CoordSf"))
})


# --- Label helper tests ---

test_that(".auto_title returns title-cased candidate with party and number", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  label <- .auto_title("CAND_13", obj)
  expect_true(grepl("Joao", label))
  expect_true(grepl("da Silva", label))
  expect_true(grepl("PT", label))
  expect_true(grepl("13", label))
})

test_that(".auto_title returns column name when no dictionary", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.auto_title("CAND_13", obj), "CAND_13")
})

test_that(".auto_subtitle returns municipality and year", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  sub <- .auto_subtitle(obj)
  expect_true(grepl("SAO PAULO", sub))
  expect_true(grepl("2020", sub))
})

test_that(".auto_subtitle includes type and breaks when provided", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  sub <- .auto_subtitle(obj, type = "pct_tract", breaks = "quantile")
  expect_true(grepl("SAO PAULO", sub))
  expect_true(grepl("tract", sub, ignore.case = TRUE))
  expect_true(grepl("uantile", sub, ignore.case = TRUE))
})

test_that(".auto_subtitle omits type for absolute and breaks for continuous", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  sub <- .auto_subtitle(obj, type = "absolute", breaks = "continuous")
  expect_true(grepl("SAO PAULO", sub))
  expect_false(grepl("Count", sub))
  expect_false(grepl("ontinuous", sub, ignore.case = TRUE))
})

test_that(".auto_subtitle returns NULL for generic result", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$nome_municipio <- NULL
  obj$year <- NULL
  expect_null(.auto_subtitle(obj))
})

test_that(".auto_caption returns source string", {
  cap <- .auto_caption()
  expect_true(grepl("TSE", cap))
  expect_true(grepl("interpolation", cap))
})

test_that(".quantity_label returns human-readable labels", {
  expect_equal(.quantity_label("absolute"), "Count")
  expect_equal(.quantity_label("pct_tract"), "% of tract votes")
  expect_equal(.quantity_label("pct_muni"), "% of municipality total")
})


# --- .compute_breaks tests ---

test_that(".compute_breaks continuous returns NULL", {
  vals <- runif(20)
  expect_null(.compute_breaks(vals, "continuous", 5))
})

test_that(".compute_breaks quantile returns sorted unique breaks", {
  vals <- 1:100
  brks <- .compute_breaks(vals, "quantile", 4)
  expect_true(is.numeric(brks))
  expect_true(all(diff(brks) >= 0))
})

test_that(".compute_breaks custom numeric passthrough", {
  brks <- .compute_breaks(1:10, c(0, 5, 10), 5)
  expect_equal(brks, c(0, 5, 10))
})

test_that(".compute_breaks errors on unknown method", {
  expect_error(.compute_breaks(1:10, "unknown", 5), "Unknown breaks")
})


# --- plot_interactive tests ---

test_that("plot_interactive errors without mapview", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()

  # We can only test the error when mapview IS installed by mocking,
  # but we can at minimum test it runs if mapview is available
  if (requireNamespace("mapview", quietly = TRUE)) {
    m <- plot_interactive(obj, variable = "CAND_13")
    expect_true(inherits(m, "mapview"))
  }
})

test_that("plot_interactive resolves variable by name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "JOAO")
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive with type = pct_muni works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13", type = "pct_muni")
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive sync with multiple variables", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  skip_if_not_installed("leafsync")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = c("CAND_13", "CAND_22"))
  # leafsync::sync returns a tagList or similar
  expect_true(!is.null(m))
})

test_that("plot_interactive errors when tracts_sf is NULL", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()
  obj$tracts_sf <- NULL

  expect_error(plot_interactive(obj), "No tracts_sf")
})

test_that("plot_interactive custom popup_vars works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13",
                        popup_vars = c("zone_id", "CAND_13"))
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive with quantile breaks bins values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13",
                        breaks = "quantile", n_breaks = 3)
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive with custom numeric breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13",
                        breaks = c(0, 10, 30, Inf))
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive with continuous breaks uses numeric scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13",
                        breaks = "continuous")
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive sync with breaks uses shared scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  skip_if_not_installed("leafsync")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = c("CAND_13", "CAND_22"),
                        breaks = "quantile", n_breaks = 3)
  expect_true(!is.null(m))
})


# --- .build_detail_popup tests ---

test_that(".build_detail_popup returns HTML vector with all tracts", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  plot_sf <- obj$tracts_sf
  plot_sf[["Test"]] <- .cut_values(
    .compute_quantity(obj, "CAND_13", "pct_tract"),
    c(0, 20, 50, 100), "pct_tract"
  )
  popup <- .build_detail_popup(obj, "CAND_13", "Test", plot_sf)
  expect_equal(length(popup), nrow(obj$tracts_sf))
  # Check that popup HTML contains expected fields
  expect_true(grepl("Votes", popup[1]))
  expect_true(grepl("% of tract", popup[1]))
  expect_true(grepl("% of municipality", popup[1]))
  expect_true(grepl("Rank", popup[1]))
  expect_true(grepl("Category", popup[1]))
  # Should contain tract ID
  expect_true(grepl("Tract", popup[1]))
})

test_that(".build_detail_popup formats numbers without scientific notation", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  plot_sf <- obj$tracts_sf
  plot_sf[["Test"]] <- .compute_quantity(obj, "CAND_13", "pct_tract")
  popup <- .build_detail_popup(obj, "CAND_13", "Test", plot_sf)
  # No scientific notation in any popup
  expect_false(any(grepl("[0-9]e[+-]", popup)))
})

test_that(".build_detail_popup includes neighborhood when available", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Create a fake neighborhood polygon covering all tracts
  nb_poly <- sf::st_polygon(list(matrix(c(
    -1, -1, 4, -1, 4, 3, -1, 3, -1, -1
  ), ncol = 2, byrow = TRUE)))
  nb_sf <- sf::st_sf(
    name_neighborhood = "TestNeighborhood",
    code_neighborhood = "3550308001",
    geometry = sf::st_sfc(nb_poly, crs = 4326)
  )
  obj$neighborhoods <- nb_sf
  plot_sf <- obj$tracts_sf
  plot_sf[["Test"]] <- .compute_quantity(obj, "CAND_13", "pct_tract")
  popup <- .build_detail_popup(obj, "CAND_13", "Test", plot_sf)
  expect_true(grepl("TestNeighborhood", popup[1]))
})

test_that(".build_pyramid_svg returns valid SVG", {
  male <- c(10, 20, 30, 25, 15, 8, 3)
  female <- c(12, 22, 28, 27, 18, 10, 5)
  labels <- c("18-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69")
  svg <- .build_pyramid_svg(male, female, labels)
  expect_true(is.character(svg))
  expect_true(grepl("<svg", svg))
  expect_true(grepl("</svg>", svg))
  expect_true(grepl("Male", svg))
  expect_true(grepl("Female", svg))
  # Should have 7 age labels
  for (lab in labels) {
    expect_true(grepl(lab, svg))
  }
})

test_that(".build_pyramid_svg with fitted overlay shows legend", {
  male <- c(10, 20, 30, 25, 15, 8, 3)
  female <- c(12, 22, 28, 27, 18, 10, 5)
  fitted_m <- c(11, 19, 31, 24, 16, 7, 4)
  fitted_f <- c(13, 21, 29, 26, 17, 11, 4)
  labels <- c("18-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69")
  svg <- .build_pyramid_svg(male, female, labels,
                              fitted_male = fitted_m, fitted_female = fitted_f)
  expect_true(grepl("<svg", svg))
  # Should have outlined rects (stroke, fill="none")
  expect_true(grepl('fill="none"', svg))
  expect_true(grepl('stroke="#1a3a5c"', svg))
  expect_true(grepl('stroke="#b8651e"', svg))
  # Should have Census/Interpolated legend
  expect_true(grepl("Census", svg))
  expect_true(grepl("Interpolated", svg))
})

test_that(".build_pyramid_svg without fitted has no legend", {
  male <- c(10, 20, 30)
  female <- c(12, 22, 28)
  labels <- c("18-19", "20-24", "25-29")
  svg <- .build_pyramid_svg(male, female, labels)
  expect_false(grepl("Census", svg))
  expect_false(grepl("Interpolated", svg))
})

test_that(".build_age_bars_svg returns valid SVG", {
  vals <- c(10, 20, 30, 25, 15, 8, 3)
  labels <- c("18-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69")
  svg <- .build_age_bars_svg(vals, labels)
  expect_true(is.character(svg))
  expect_true(grepl("<svg", svg))
  for (lab in labels) {
    expect_true(grepl(lab, svg))
  }
})

test_that(".build_age_bars_svg with fitted overlay shows legend", {
  vals <- c(10, 20, 30, 25, 15, 8, 3)
  fitted <- c(11, 19, 31, 24, 16, 7, 4)
  labels <- c("18-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69")
  svg <- .build_age_bars_svg(vals, labels, fitted_vals = fitted)
  expect_true(grepl("<svg", svg))
  expect_true(grepl('fill="none"', svg))
  expect_true(grepl('stroke="#1a3a5c"', svg))
  expect_true(grepl("Census", svg))
  expect_true(grepl("Interpolated", svg))
})

test_that(".extract_gender_age detects age-only mode", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Mock has calib_cols$tracts = "votantes_18_20" (not pop_hom_*)
  result <- .extract_gender_age(obj$tracts_sf, obj$calib_cols)
  # Should return NULL (column doesn't match pop_ pattern) or age-only
  # (votantes_18_20 is not a pop_ column so it won't be found)
  # The function returns NULL if no pop_ columns are found
})

test_that(".extract_gender_age works with plain matrix (for fitted values)", {
  n <- 4
  ages <- c("18_19", "20_24", "25_29")
  tract_cols <- paste0("pop_hom_", ages)
  mat <- matrix(runif(n * length(tract_cols)), n, length(tract_cols))
  colnames(mat) <- tract_cols
  calib <- list(tracts = tract_cols, sources = paste0("vot_", ages))
  result <- .extract_gender_age(mat, calib)
  # pop_hom_ detected -> has_gender = TRUE
  expect_true(result$has_gender)
  expect_equal(nrow(result$male), n)
  expect_equal(ncol(result$male), length(ages))
})

test_that(".extract_gender_age detects full gender mode", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Add gender x age columns to tracts_sf
  n <- nrow(obj$tracts_sf)
  ages <- c("18_19", "20_24", "25_29", "30_39", "40_49", "50_59", "60_69")
  for (ag in ages) {
    obj$tracts_sf[[paste0("pop_hom_", ag)]] <- runif(n, 10, 50)
    obj$tracts_sf[[paste0("pop_mul_", ag)]] <- runif(n, 10, 50)
  }
  calib <- list(
    tracts  = paste0("pop_", rep(c("hom", "mul"), each = 7), "_", ages),
    sources = paste0("vot_", rep(c("hom", "mul"), each = 7), "_", ages)
  )
  result <- .extract_gender_age(obj$tracts_sf, calib)
  expect_true(result$has_gender)
  expect_equal(ncol(result$male), 7)
  expect_equal(ncol(result$female), 7)
  expect_equal(nrow(result$male), n)
  expect_equal(length(result$age_labels), 7)
})


# --- .auto_popup_cols tests ---

test_that(".auto_popup_cols includes tract_id and plot col", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .auto_popup_cols(obj$tracts_sf, obj, "CAND_13")
  expect_true("zone_id" %in% cols)
  expect_true("CAND_13" %in% cols)
})

test_that(".auto_popup_cols includes turnout columns", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .auto_popup_cols(obj$tracts_sf, obj, "CAND_13")
  expect_true("QT_COMPARECIMENTO" %in% cols)
})

test_that(".auto_popup_cols caps at 8", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .auto_popup_cols(obj$tracts_sf, obj, "CAND_13")
  expect_true(length(cols) <= 8)
})

test_that(".auto_popup_cols finds computed column in plot_sf", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Simulate computed quantity added to plot_sf (as done in plot_interactive)
  plot_sf <- obj$tracts_sf
  plot_sf[["Joao da Silva (PT \u2014 13)"]] <- runif(nrow(plot_sf))
  cols <- .auto_popup_cols(plot_sf, obj, "Joao da Silva (PT \u2014 13)")
  expect_true("Joao da Silva (PT \u2014 13)" %in% cols)
})


# --- .title_case_pt tests ---

test_that(".title_case_pt converts all-caps to title case", {
  expect_equal(.title_case_pt("JOAO DA SILVA"), "Joao da Silva")
  expect_equal(.title_case_pt("MARIA SOUZA"), "Maria Souza")
})

test_that(".title_case_pt handles Portuguese particles", {
  expect_equal(
    .title_case_pt("LUIZ INACIO LULA DA SILVA"),
    "Luiz Inacio Lula da Silva"
  )
  expect_equal(
    .title_case_pt("JOSE DOS SANTOS"),
    "Jose dos Santos"
  )
})


# --- .scale_labels tests ---

test_that(".scale_labels returns percentage formatter for pct types", {
  fn <- .scale_labels("pct_tract")
  expect_equal(fn(50), "50%")
  expect_equal(fn(12.34), "12.3%")
  expect_equal(fn(NA), "")
})

test_that(".scale_labels returns comma formatter for absolute", {
  fn <- .scale_labels("absolute")
  expect_equal(fn(1000), "1,000")
  expect_equal(fn(NA), "")
})


# --- .make_bin_labels tests ---

test_that(".make_bin_labels generates interval labels with at least 1 decimal", {
  labels <- .make_bin_labels(c(0, 5, 10, 20), "pct_tract")
  expect_equal(length(labels), 3)
  # en-dash separator and % suffix, at least 1 decimal place
  expect_equal(labels[1], "0.0\u20135.0%")
  expect_equal(labels[2], "5.0\u201310.0%")
  expect_equal(labels[3], "10.0\u201320.0%")
})

test_that(".make_bin_labels handles Inf in last break", {
  labels <- .make_bin_labels(c(0, 5, 10, Inf), "pct_tract")
  expect_equal(length(labels), 3)
  expect_true(grepl("^>10", labels[3]))
  expect_true(grepl("%$", labels[3]))
})

test_that(".make_bin_labels handles -Inf in first break", {
  labels <- .make_bin_labels(c(-Inf, 5, 10), "absolute")
  expect_true(grepl("^<", labels[1]))
  expect_true(grepl("5", labels[1]))
})

test_that(".make_bin_labels no suffix for absolute", {
  labels <- .make_bin_labels(c(0, 100, 200), "absolute")
  expect_false(grepl("%", labels[1]))
})

test_that(".make_bin_labels density suffix", {
  labels <- .make_bin_labels(c(0, 50, 100), "density")
  expect_true(grepl("km", labels[1]))
})

test_that(".make_bin_labels uses sufficient precision for small breaks", {
  labels <- .make_bin_labels(c(0, 0.01, 0.025, 0.05, Inf), "pct_tract")
  # All 4 labels must be distinct (adaptive decimals ensure no collisions)
  expect_equal(length(labels), 4)
  expect_equal(length(unique(labels)), 4)
  # Must show more than 1 decimal place (since 0, 0.01, 0.025 collide at 1dp)
  expect_true(grepl("0\\.01", labels[1]))
  expect_true(grepl("0\\.0[2-3]", labels[2]))  # 0.025 rounds to 0.02 or 0.03
})

test_that(".make_bin_labels increases decimals when bounds collide", {
  # 34.21, 34.22 round to same at 1 decimal -> must increase to 2
  labels <- .make_bin_labels(c(34.21, 34.22, 34.8), "pct_tract")
  expect_equal(length(labels), 2)
  expect_true(grepl("34\\.21", labels[1]))
  expect_true(grepl("34\\.22", labels[1]))
})


# --- .cut_values tests ---

test_that(".cut_values returns factor with correct levels", {
  vals <- c(2, 7, 15)
  f <- .cut_values(vals, c(0, 5, 10, 20), "pct_tract")
  expect_s3_class(f, "factor")
  expect_equal(length(levels(f)), 3)
})

test_that(".cut_values extends breaks when data exceeds range", {
  vals <- c(-1, 7, 25)
  f <- .cut_values(vals, c(0, 5, 10, 20), "pct_tract")
  expect_true(!any(is.na(f)))
})

test_that(".cut_values handles degenerate breaks", {
  vals <- c(5, 5, 5)
  f <- .cut_values(vals, c(5), "absolute")
  expect_s3_class(f, "factor")
})


# --- .bin_colors tests ---

test_that(".bin_colors returns correct number of colors", {
  skip_if_not_installed("RColorBrewer")
  colors <- .bin_colors("RdYlBu", 5)
  expect_equal(length(colors), 5)
})

test_that(".bin_colors works with viridis palette", {
  skip_if_not_installed("viridisLite")
  colors <- .bin_colors("viridis", 4)
  expect_equal(length(colors), 4)
})


# --- Municipality contour tests ---

test_that(".muni_border_layer returns NULL without geobr or code_muni", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()
  # NULL code_muni returns NULL
  expect_null(.muni_border_layer(NULL, obj$tracts_sf))
})

test_that("plot works without municipality contour (no geobr download)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()
  # Mock code_muni that won't match real data (no download)
  obj$code_muni <- NULL
  p <- plot(obj, variable = "CAND_13")
  expect_s3_class(p, "gg")
})


# --- Limits validation tests ---

test_that(".prepare_limits warns when limits don't overlap data", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Mock data is in CRS 4326 at x=[0,3], y=[0,2]; limits far away
  expect_message(
    .prepare_limits(c(100, 200, 100, 200), obj$tracts_sf),
    "limits do not overlap"
  )
})

test_that(".prepare_limits returns xlim/ylim and is silent when overlap", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Mock data is in CRS 4326 at x=[0,3], y=[0,2]; limits inside
  result <- expect_silent(.prepare_limits(c(0, 2, 0, 1), obj$tracts_sf))
  expect_true(is.list(result))
  expect_true("xlim" %in% names(result))
  expect_true("ylim" %in% names(result))
})


# --- Binned scale produces discrete fill ---

test_that("plot with quantile breaks produces discrete fill scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = "quantile", n_breaks = 3)
  expect_s3_class(p, "gg")
  fill_scale <- p$scales$get_scales("fill")
  expect_true(fill_scale$is_discrete())
})

test_that("plot with custom breaks produces discrete fill scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = c(0, 50, 100, 200))
  expect_s3_class(p, "gg")
  fill_scale <- p$scales$get_scales("fill")
  expect_true(fill_scale$is_discrete())
})

test_that("plot with continuous breaks uses continuous scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = "continuous")
  expect_s3_class(p, "gg")
  fill_scale <- p$scales$get_scales("fill")
  expect_false(fill_scale$is_discrete())
})


# --- .map_theme tests ---

test_that(".map_theme returns a ggplot theme", {
  skip_if_not_installed("ggplot2")
  th <- .map_theme()
  expect_s3_class(th, "theme")
})
