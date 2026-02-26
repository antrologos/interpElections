# Spatial autocorrelation diagnostics

#' Test residuals for spatial autocorrelation
#'
#' Computes Moran's I test for spatial autocorrelation in calibration
#' residuals. Significant spatial autocorrelation suggests model
#' misspecification — the IDW kernel may be missing spatial structure.
#'
#' @param result An `interpElections_result` object.
#' @param summary_fn Summary function for collapsing residuals across
#'   brackets: `"rmse"` (default), `"mean"`, `"mean_abs"`, `"max_abs"`.
#' @param residual_type Residual type: `"raw"`, `"pearson"`, or
#'   `"deviance"`. Default: `"raw"`.
#' @param nsim Integer. Number of permutations for the test. Default: 999.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly). Prints a Moran scatterplot and
#'   the test result. Returns NULL with a message if `spdep` is not
#'   installed.
#'
#' @export
plot_residual_autocorrelation <- function(result,
                                           summary_fn = "rmse",
                                           residual_type = c("raw", "pearson", "deviance"),
                                           nsim = 999L, ...) {
  residual_type <- match.arg(residual_type)
  if (!requireNamespace("spdep", quietly = TRUE)) {
    message("The 'spdep' package is required for spatial autocorrelation tests.\n",
            "Install with: install.packages(\"spdep\")")
    return(invisible(NULL))
  }
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The 'ggplot2' package is required.", call. = FALSE)

  resid_mat <- .compute_typed_residuals(result, residual_type)
  if (is.null(resid_mat)) {
    message("Cannot compute residuals.")
    return(invisible(NULL))
  }

  # Collapse to per-tract scalar
  resid_vec <- .collapse_residuals(resid_mat, summary_fn)

  # Spatial weights from tract adjacency
  nb <- spdep::poly2nb(result$tracts_sf, queen = TRUE)
  # Remove isolates for listw
  has_neighbors <- vapply(nb, function(x) !identical(x, 0L), logical(1))
  if (sum(has_neighbors) < 3L) {
    message("Too few tracts with neighbors for Moran's I test.")
    return(invisible(NULL))
  }
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  # Moran's I test
  moran_test <- spdep::moran.test(resid_vec, lw, zero.policy = TRUE)
  I_stat <- moran_test$estimate["Moran I statistic"]
  p_val <- moran_test$p.value

  # Moran scatterplot
  lag_resid <- spdep::lag.listw(lw, resid_vec, zero.policy = TRUE)
  moran_df <- data.frame(
    residual = resid_vec,
    lag_residual = lag_resid
  )

  type_label <- switch(residual_type,
    raw = "Raw", pearson = "Pearson", deviance = "Deviance")
  fn_label <- switch(summary_fn,
    rmse = "RMSE", mean = "Mean", mean_abs = "Mean |resid|",
    max_abs = "Max |resid|", summary_fn)

  p <- ggplot2::ggplot(moran_df,
                        ggplot2::aes(x = .data$residual,
                                     y = .data$lag_residual)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey70") +
    ggplot2::geom_vline(xintercept = 0, color = "grey70") +
    ggplot2::geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red",
                          linewidth = 0.8) +
    ggplot2::annotate("text", x = Inf, y = Inf,
                       label = sprintf("Moran's I = %.3f\np = %.4f",
                                       I_stat, p_val),
                       hjust = 1.1, vjust = 1.3, size = 3.5,
                       fontface = "bold") +
    ggplot2::labs(
      title = "Moran scatterplot of calibration residuals",
      subtitle = sprintf("%s residuals, %s per tract", type_label, fn_label),
      x = "Residual",
      y = "Spatially lagged residual"
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}


#' Moran's I and LISA cluster map for an interpolated variable
#'
#' Computes global Moran's I and optionally maps Local Indicators of
#' Spatial Association (LISA) clusters for an interpolated variable.
#'
#' @param result An `interpElections_result` object.
#' @param variable Variable to analyze. Accepts column name, ballot number,
#'   candidate name, or party abbreviation.
#' @param type Character. `"moran"` for the Moran scatterplot, or
#'   `"lisa"` for the LISA cluster map. Default: `"lisa"`.
#' @param quantity Quantity to analyze: `"pct_tract"` (default),
#'   `"absolute"`, `"pct_muni"`, `"pct_valid"`, `"pct_eligible"`,
#'   `"density"`. The default `"pct_tract"` divides raw vote counts by
#'   tract turnout, revealing political preference patterns rather than
#'   population density patterns. Matches the default of
#'   [plot.interpElections_result()].
#' @param significance Significance level for LISA. Default: 0.05.
#' @param nsim Number of permutations for LISA significance testing
#'   (conditional permutation test via [spdep::localmoran_perm()]).
#'   Default: 999. Only used when `type = "lisa"`.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly).
#'
#' @export
plot_moran <- function(result, variable = NULL,
                        type = c("lisa", "moran"),
                        quantity = "pct_tract",
                        significance = 0.05, nsim = 999L, ...) {
  type <- match.arg(type)
  quantity <- match.arg(quantity, c("absolute", "pct_tract", "pct_muni",
                                     "pct_valid", "pct_eligible", "density"))
  if (!requireNamespace("spdep", quietly = TRUE)) {
    message("The 'spdep' package is required.\n",
            "Install with: install.packages(\"spdep\")")
    return(invisible(NULL))
  }
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The 'ggplot2' package is required.", call. = FALSE)

  # Resolve variable
  if (is.null(variable)) {
    variable <- colnames(result$interpolated)[1L]
    message(sprintf("No variable specified. Using '%s'.", variable))
  }
  col <- tryCatch(.resolve_var(variable, result), error = function(e) {
    message(conditionMessage(e))
    NULL
  })
  if (is.null(col)) return(invisible(NULL))

  values <- tryCatch(.compute_quantity(result, col, quantity), error = function(e) {
    message(conditionMessage(e))
    NULL
  })
  if (is.null(values)) return(invisible(NULL))

  # Filter out NA observations (unreachable tracts) before spatial analysis
  valid <- !is.na(values)
  if (sum(valid) < 3L) {
    message("Too few non-NA tracts for spatial autocorrelation analysis.")
    return(invisible(NULL))
  }
  result_sub <- result
  if (any(!valid)) {
    result_sub$tracts_sf <- result$tracts_sf[valid, ]
    result_sub$interpolated <- result$interpolated[valid, , drop = FALSE]
    values <- values[valid]
  }

  # Spatial weights
  nb <- spdep::poly2nb(result_sub$tracts_sf, queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  if (type == "moran") {
    p <- .moran_scatter(values, lw, col, result_sub, quantity)
  } else {
    p <- .lisa_map(values, lw, col, result_sub, significance, nsim, quantity)
  }

  if (!is.null(p)) print(p)
  invisible(p)
}


#' @noRd
.moran_scatter <- function(values, lw, col, result, quantity = "absolute") {
  moran_test <- spdep::moran.test(values, lw, zero.policy = TRUE)
  I_stat <- moran_test$estimate["Moran I statistic"]
  p_val <- moran_test$p.value

  lag_vals <- spdep::lag.listw(lw, values, zero.policy = TRUE)
  df <- data.frame(value = values, lag_value = lag_vals)

  title <- tryCatch(.auto_title(col, result), error = function(e) col)
  qty_label <- .quantity_label(quantity)
  sub <- if (quantity != "absolute") qty_label else NULL

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value,
                                          y = .data$lag_value)) +
    ggplot2::geom_hline(yintercept = mean(lag_vals), color = "grey70") +
    ggplot2::geom_vline(xintercept = mean(values), color = "grey70") +
    ggplot2::geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red",
                          linewidth = 0.8) +
    ggplot2::annotate("text", x = Inf, y = Inf,
                       label = sprintf("Moran's I = %.3f\np = %.4f",
                                       I_stat, p_val),
                       hjust = 1.1, vjust = 1.3, size = 3.5,
                       fontface = "bold") +
    ggplot2::labs(
      title = sprintf("Moran scatterplot: %s", title),
      subtitle = sub,
      x = qty_label,
      y = sprintf("Spatially lagged %s", qty_label)
    ) +
    ggplot2::theme_minimal()
  p
}


#' @noRd
.lisa_map <- function(values, lw, col, result, significance, nsim,
                     quantity = "absolute") {
  # Local Moran's I (permutation-based)
  lisa <- spdep::localmoran_perm(values, lw, nsim = nsim,
                                  zero.policy = TRUE)
  # Permutation p-value column
  p_col <- "Pr(folded) Sim"
  if (!p_col %in% colnames(lisa)) {
    p_col <- grep("Pr.*Sim", colnames(lisa), value = TRUE)[1L]
  }
  if (is.null(p_col) || !p_col %in% colnames(lisa)) {
    p_col <- "Pr(z != E(Ii))"
  }
  p_vals <- lisa[, p_col]

  # Classify clusters
  z_val <- scale(values)[, 1]
  lag_z <- spdep::lag.listw(lw, z_val, zero.policy = TRUE)

  cluster <- character(length(values))
  cluster[] <- "Not significant"
  sig <- p_vals <= significance
  cluster[sig & z_val > 0 & lag_z > 0] <- "High-High"
  cluster[sig & z_val < 0 & lag_z < 0] <- "Low-Low"
  cluster[sig & z_val > 0 & lag_z < 0] <- "High-Low"
  cluster[sig & z_val < 0 & lag_z > 0] <- "Low-High"

  cluster <- factor(cluster,
                     levels = c("High-High", "Low-Low", "High-Low",
                                "Low-High", "Not significant"))

  plot_sf <- result$tracts_sf
  plot_sf$.lisa_cluster <- cluster

  lisa_colors <- c(
    "High-High"       = "#d7191c",
    "Low-Low"         = "#2c7bb6",
    "High-Low"        = "#fdae61",
    "Low-High"        = "#abd9e9",
    "Not significant" = "#f0f0f0"
  )

  n_sig <- sum(sig)
  title <- tryCatch(.auto_title(col, result), error = function(e) col)
  qty_label <- .quantity_label(quantity)
  sub <- sprintf("%d significant tracts (p < %.2f)", n_sig, significance)
  if (quantity != "absolute") sub <- paste0(qty_label, " \u2014 ", sub)

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.lisa_cluster),
      color = "white", linewidth = 0.05
    ) +
    ggplot2::scale_fill_manual(values = lisa_colors, drop = FALSE) +
    ggplot2::labs(
      title = sprintf("LISA clusters: %s", title),
      subtitle = sub,
      fill = "Cluster"
    ) +
    .map_theme()

  # Municipality border
  border_layer <- .muni_border_layer(result$code_muni, result$tracts_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  p
}
