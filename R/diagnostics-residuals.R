# Diagnostic plots and summaries for calibration residuals

#' Plot calibration residual diagnostics
#'
#' @param result An `interpElections_result` object.
#' @param type Character. Plot type: `"map"`, `"histogram"`, `"bracket"`,
#'   or `"scatter"`.
#' @param residual_type Character. `"raw"`, `"pearson"`, or `"deviance"`.
#' @param summary_fn Summary for collapsing across brackets in map mode.
#'   One of `"rmse"` (default), `"mean_abs"`, `"max_abs"`, `"mean"`, or
#'   an integer bracket index.
#' @param brackets Integer vector of bracket indices. NULL = all.
#' @param palette Color palette. Default: `"RdBu"`.
#' @param breaks Break method.
#' @param n_breaks Integer.
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly).
#' @export
plot_residuals <- function(result,
                           type = c("map", "histogram", "bracket", "scatter"),
                           residual_type = c("raw", "pearson", "deviance"),
                           summary_fn = "rmse", brackets = NULL,
                           palette = "RdBu", breaks = "quantile",
                           n_breaks = 5L, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "The 'ggplot2' package is required for plot_residuals().\n",
      "Install with: install.packages(\"ggplot2\")",
      call. = FALSE
    )
  }

  type <- match.arg(type)
  residual_type <- match.arg(residual_type)

  resid_mat <- .compute_typed_residuals(result, residual_type)
  if (is.null(resid_mat)) {
    stop("Could not compute residuals from result object.", call. = FALSE)
  }

  p <- switch(type,
    map       = .plot_resid_map(result, resid_mat, residual_type, summary_fn,
                                palette, breaks, n_breaks),
    histogram = .plot_resid_histogram(result, resid_mat, residual_type,
                                     brackets),
    bracket   = .plot_resid_bracket(result, resid_mat, residual_type,
                                    brackets),
    scatter   = .plot_resid_scatter(result, residual_type, brackets)
  )

  print(p)
  invisible(p)
}


# --- Map type ---

#' @noRd
.plot_resid_map <- function(result, resid_mat, residual_type, summary_fn,
                            palette, breaks, n_breaks) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for map plots.", call. = FALSE)
  }
  if (is.null(result$tracts_sf)) {
    stop("No tracts_sf in result object.", call. = FALSE)
  }

  values <- .collapse_residuals(resid_mat, summary_fn)

  plot_sf <- result$tracts_sf
  plot_sf$.plot_value <- values

  # Determine if diverging (directional) or sequential (magnitude)
  is_directional <- identical(summary_fn, "mean")

  # Title
  resid_label <- switch(residual_type,
    raw      = "Raw",
    pearson  = "Pearson",
    deviance = "Deviance"
  )
  if (is.numeric(summary_fn)) {
    summary_label <- paste0("bracket ", summary_fn)
  } else {
    summary_label <- toupper(summary_fn)
  }
  title <- paste0(resid_label, " residuals (", summary_label, ")")

  brk <- .compute_breaks(values, breaks, n_breaks)

  if (!is.null(brk)) {
    plot_sf$.plot_value <- .cut_values(values, brk, "absolute")
  }

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.plot_value),
      color = "white",
      linewidth = 0.05
    ) +
    .build_fill_scale(palette, brk, n_breaks, type = "absolute") +
    ggplot2::labs(title = title, fill = "Residual") +
    .map_theme()

  border_layer <- .muni_border_layer(result$code_muni, plot_sf,
                                     result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  if (!is.null(brk)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = ggplot2::unit(1.5, "cm"),
      keyheight = ggplot2::unit(0.3, "cm"),
      nrow = 1
    ))
  }

  p
}


# --- Histogram type ---

#' @noRd
.plot_resid_histogram <- function(result, resid_mat, residual_type, brackets) {
  labels <- .bracket_labels(result)
  k <- ncol(resid_mat)
  if (is.null(labels)) labels <- paste0("Bracket ", seq_len(k))

  idx <- seq_len(k)
  if (!is.null(brackets)) {
    idx <- brackets[brackets >= 1L & brackets <= k]
  }

  if (k == 1L && is.null(brackets)) {
    df <- data.frame(residual = as.numeric(resid_mat))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$residual)) +
      ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "red") +
      ggplot2::labs(
        x = paste0(tools::toTitleCase(residual_type), " residual"),
        y = "Density",
        title = paste0(tools::toTitleCase(residual_type),
                       " residual distribution")
      ) +
      ggplot2::theme_minimal()
  } else {
    long <- data.frame(
      bracket = rep(labels[idx], each = nrow(resid_mat)),
      residual = as.numeric(resid_mat[, idx]),
      stringsAsFactors = FALSE
    )
    long$bracket <- factor(long$bracket, levels = labels[idx])

    nrow_facet <- if (length(idx) > 7L) 2L else 1L

    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$residual)) +
      ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "red") +
      ggplot2::facet_wrap(~ .data$bracket, nrow = nrow_facet) +
      ggplot2::labs(
        x = paste0(tools::toTitleCase(residual_type), " residual"),
        y = "Density",
        title = paste0(tools::toTitleCase(residual_type),
                       " residual distribution by bracket")
      ) +
      ggplot2::theme_minimal()
  }

  p
}


# --- Bracket boxplot type ---

#' @noRd
.plot_resid_bracket <- function(result, resid_mat, residual_type, brackets) {
  labels <- .bracket_labels(result)
  k <- ncol(resid_mat)
  if (is.null(labels)) labels <- paste0("Bracket ", seq_len(k))

  idx <- seq_len(k)
  if (!is.null(brackets)) {
    idx <- brackets[brackets >= 1L & brackets <= k]
  }

  long <- data.frame(
    bracket = rep(labels[idx], each = nrow(resid_mat)),
    residual = as.numeric(resid_mat[, idx]),
    stringsAsFactors = FALSE
  )
  long$bracket <- factor(long$bracket, levels = labels[idx])

  p <- ggplot2::ggplot(long,
                        ggplot2::aes(x = .data$bracket,
                                     y = .data$residual)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      x = "Bracket",
      y = paste0(tools::toTitleCase(residual_type), " residual"),
      title = paste0(tools::toTitleCase(residual_type),
                     " residuals by bracket")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  p
}


# --- Scatter type (fitted vs observed) ---

#' @noRd
.plot_resid_scatter <- function(result, residual_type, brackets) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for scatter plots.", call. = FALSE)
  }

  .require_weights(result)
  W <- .get_weights(result)

  src_mat <- as.matrix(
    result$sources[, result$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"
  fitted_vals <- W %*% src_mat

  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  observed <- as.matrix(
    tracts_df[, result$calib_cols$tracts, drop = FALSE]
  )
  storage.mode(observed) <- "double"

  labels <- .bracket_labels(result)
  k <- ncol(observed)
  if (is.null(labels)) labels <- paste0("Bracket ", seq_len(k))

  idx <- seq_len(k)
  if (!is.null(brackets)) {
    idx <- brackets[brackets >= 1L & brackets <= k]
  }

  if (k == 1L && is.null(brackets)) {
    df <- data.frame(
      observed = as.numeric(observed),
      fitted = as.numeric(fitted_vals)
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$observed,
                                            y = .data$fitted)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                            color = "red") +
      ggplot2::labs(x = "Observed (census)", y = "Fitted (interpolated)",
                    title = "Fitted vs. observed") +
      ggplot2::theme_minimal()
  } else {
    long <- data.frame(
      bracket = rep(labels[idx], each = nrow(observed)),
      observed = as.numeric(observed[, idx]),
      fitted = as.numeric(fitted_vals[, idx]),
      stringsAsFactors = FALSE
    )
    long$bracket <- factor(long$bracket, levels = labels[idx])

    nrow_facet <- if (length(idx) > 7L) 2L else 1L

    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$observed,
                                              y = .data$fitted)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                            color = "red") +
      ggplot2::facet_wrap(~ .data$bracket, nrow = nrow_facet) +
      ggplot2::labs(x = "Observed (census)", y = "Fitted (interpolated)",
                    title = "Fitted vs. observed by bracket") +
      ggplot2::theme_minimal()
  }

  p
}


# --- Tabular summary ---

#' Tabular residual summary
#'
#' Computes per-bracket, per-tract, and overall residual summary statistics.
#'
#' @param result An `interpElections_result` object.
#' @param type Residual type: `"raw"`, `"pearson"`, or `"deviance"`.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{per_bracket}{Data frame with columns: bracket, mean, rmse,
#'       max_abs, pct_gt_2sd.}
#'     \item{per_tract}{Data frame with columns: tract_id, mean, rmse,
#'       worst_bracket.}
#'     \item{overall}{List with rmse, mean_bias, total_deviance.}
#'   }
#' @export
residual_summary <- function(result, type = c("raw", "pearson", "deviance")) {
  type <- match.arg(type)

  resid_mat <- .compute_typed_residuals(result, type)
  if (is.null(resid_mat)) {
    stop("Could not compute residuals from result object.", call. = FALSE)
  }

  labels <- .bracket_labels(result)
  k <- ncol(resid_mat)
  n <- nrow(resid_mat)
  if (is.null(labels)) labels <- paste0("Bracket ", seq_len(k))

  # Per-bracket statistics
  bracket_mean <- colMeans(resid_mat)
  bracket_rmse <- sqrt(colMeans(resid_mat^2))
  bracket_max_abs <- apply(resid_mat, 2, function(x) max(abs(x)))

  # pct_gt_2sd: percentage of tracts with |residual| > 2 * sd(residual)
  bracket_sd <- apply(resid_mat, 2, stats::sd)
  bracket_pct_gt_2sd <- vapply(seq_len(k), function(j) {
    threshold <- 2 * bracket_sd[j]
    if (threshold == 0) return(0)
    sum(abs(resid_mat[, j]) > threshold) / n * 100
  }, numeric(1))

  per_bracket <- data.frame(
    bracket = labels,
    mean = bracket_mean,
    rmse = bracket_rmse,
    max_abs = bracket_max_abs,
    pct_gt_2sd = bracket_pct_gt_2sd,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Per-tract statistics
  tract_ids <- NULL
  if (!is.null(result$tracts_sf) && !is.null(result$tract_id)) {
    if (requireNamespace("sf", quietly = TRUE)) {
      tracts_df <- sf::st_drop_geometry(result$tracts_sf)
      if (result$tract_id %in% names(tracts_df)) {
        tract_ids <- tracts_df[[result$tract_id]]
      }
    }
  }
  if (is.null(tract_ids)) tract_ids <- seq_len(n)

  tract_mean <- rowMeans(resid_mat)
  tract_rmse <- sqrt(rowMeans(resid_mat^2))
  worst_bracket_idx <- apply(abs(resid_mat), 1, which.max)
  worst_bracket <- labels[worst_bracket_idx]

  per_tract <- data.frame(
    tract_id = tract_ids,
    mean = tract_mean,
    rmse = tract_rmse,
    worst_bracket = worst_bracket,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Overall statistics
  overall_rmse <- sqrt(mean(resid_mat^2))
  overall_mean_bias <- mean(resid_mat)

  # Total deviance (Poisson) from raw residuals
  # Use the raw fitted and observed values
  total_deviance <- NA_real_
  W <- .get_weights(result)
  if (!is.null(W) && requireNamespace("sf", quietly = TRUE)) {
    src_mat <- as.matrix(
      result$sources[, result$calib_cols$sources, drop = FALSE]
    )
    storage.mode(src_mat) <- "double"
    fitted_vals <- W %*% src_mat

    tracts_df <- sf::st_drop_geometry(result$tracts_sf)
    observed <- as.matrix(
      tracts_df[, result$calib_cols$tracts, drop = FALSE]
    )
    storage.mode(observed) <- "double"

    ratio <- observed / pmax(fitted_vals, .Machine$double.eps)
    term <- observed * log(pmax(ratio, .Machine$double.eps)) -
      (observed - fitted_vals)
    total_deviance <- 2 * sum(term)
  }

  overall <- list(
    rmse = overall_rmse,
    mean_bias = overall_mean_bias,
    total_deviance = total_deviance
  )

  list(
    per_bracket = per_bracket,
    per_tract = per_tract,
    overall = overall
  )
}
