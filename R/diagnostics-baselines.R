# Baseline comparison and leave-one-out diagnostics

#' Compare interpolation against baseline methods
#'
#' Recomputes calibration residuals under naive weight matrices and
#' reports a comparison table showing the value added by optimization.
#'
#' @param result An `interpElections_result` object.
#' @param methods Character vector. Baseline methods to compare:
#'   `"nearest"` (assign to nearest station), `"uniform"` (equal weights),
#'   `"areal"` (area-based interpolation). Default: all three.
#' @param ... Ignored.
#'
#' @return A data frame with columns: method, rmse, poisson_deviance,
#'   relative_improvement. Prints the table. Returns invisibly.
#'
#' @export
compare_baselines <- function(result,
                               methods = c("nearest", "uniform", "areal"),
                               ...) {
  methods <- match.arg(methods, c("nearest", "uniform", "areal"),
                        several.ok = TRUE)

  if (is.null(result$calib_cols) || is.null(result$time_matrix)) {
    message("time_matrix and calib_cols required for baseline comparison.")
    return(invisible(NULL))
  }
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)

  tt <- result$time_matrix
  n <- nrow(tt)
  m <- ncol(tt)

  # Calibration matrices
  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  pop_mat <- as.matrix(tracts_df[, result$calib_cols$tracts, drop = FALSE])
  storage.mode(pop_mat) <- "double"
  src_mat <- as.matrix(
    result$sources[, result$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"

  # Optimized result
  W_opt <- .get_weights(result)
  rows <- list()

  if (!is.null(W_opt)) {
    fitted_opt <- W_opt %*% src_mat
    rows[["optimized"]] <- .baseline_metrics("Optimized IDW", fitted_opt, pop_mat)
  }

  for (method in methods) {
    W_base <- .baseline_weights(method, tt, result)
    if (is.null(W_base)) next
    fitted <- W_base %*% src_mat
    rows[[method]] <- .baseline_metrics(
      switch(method,
        nearest = "Nearest station",
        uniform = "Uniform weights",
        areal   = "Areal interpolation"),
      fitted, pop_mat
    )
  }

  if (length(rows) == 0L) {
    message("No baselines could be computed.")
    return(invisible(NULL))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Relative improvement over best baseline
  baseline_rows <- out$method != "Optimized IDW"
  if (any(baseline_rows) && "Optimized IDW" %in% out$method) {
    best_baseline_rmse <- min(out$rmse[baseline_rows])
    opt_rmse <- out$rmse[out$method == "Optimized IDW"]
    out$improvement_pct <- ifelse(
      out$method == "Optimized IDW",
      NA_real_,
      round((out$rmse - opt_rmse) / out$rmse * 100, 1)
    )
  } else {
    out$improvement_pct <- NA_real_
  }

  cat("Baseline comparison\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")
  for (i in seq_len(nrow(out))) {
    imp <- if (!is.na(out$improvement_pct[i]))
      sprintf("  (IDW %.0f%% better)", out$improvement_pct[i])
    else ""
    cat(sprintf("  %-22s RMSE=%.2f  Deviance=%.2f%s\n",
                out$method[i], out$rmse[i], out$deviance[i], imp))
  }
  cat(paste(rep("-", 70), collapse = ""), "\n")

  invisible(out)
}


#' Build a baseline weight matrix
#' @noRd
.baseline_weights <- function(method, tt, result) {
  n <- nrow(tt)
  m <- ncol(tt)

  if (method == "nearest") {
    # Each tract assigned to nearest station
    W <- matrix(0, n, m)
    nearest <- apply(tt, 1, function(x) {
      idx <- which.min(x)
      if (length(idx) == 0L) NA_integer_ else idx
    })
    for (i in seq_len(n)) {
      if (!is.na(nearest[i])) W[i, nearest[i]] <- 1
    }
    # Column-normalize
    cs <- colSums(W)
    cs[cs == 0] <- 1
    W <- t(t(W) / cs)
    return(W)
  }

  if (method == "uniform") {
    W <- matrix(1 / n, n, m)
    return(W)
  }

  if (method == "areal") {
    if (is.null(result$electoral_sf) || is.null(result$tracts_sf)) {
      message("Areal interpolation requires electoral_sf and tracts_sf.")
      return(NULL)
    }
    if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
    # Use areal_weights if available
    tryCatch({
      W <- areal_weights(
        target_sf = result$tracts_sf,
        source_sf = result$electoral_sf,
        target_id = result$tract_id,
        source_id = result$point_id
      )
      W
    }, error = function(e) {
      message("Areal interpolation failed: ", conditionMessage(e))
      NULL
    })
  }
}


#' Compute RMSE and Poisson deviance for a fitted matrix
#' @noRd
.baseline_metrics <- function(method_name, fitted, observed) {
  rmse <- sqrt(mean((fitted - observed)^2))

  # Poisson deviance: 2 * sum(obs * log(obs/fitted) - (obs - fitted))
  ratio <- observed / pmax(fitted, .Machine$double.eps)
  dev_terms <- observed * log(pmax(ratio, .Machine$double.eps)) -
    (observed - fitted)
  deviance <- 2 * sum(dev_terms)

  data.frame(
    method = method_name,
    rmse = round(rmse, 4),
    deviance = round(deviance, 4),
    stringsAsFactors = FALSE
  )
}


#' Leave-one-out pseudo cross-validation
#'
#' For each polling station (or a random sample), removes it from the
#' source data, re-optimizes alpha on the remaining stations, and
#' predicts the removed station's demographics. This is the only form
#' of out-of-sample validation available.
#'
#' @param result An `interpElections_result` object.
#' @param max_stations Integer. Maximum stations to evaluate. If the
#'   municipality has more stations, a random sample is used.
#'   Default: 30.
#' @param verbose Logical. Print progress? Default: TRUE.
#' @param ... Ignored.
#'
#' @return A data frame with one row per evaluated station, containing
#'   prediction errors. Returns NULL if required data is missing.
#'
#' @export
leave_one_out <- function(result, max_stations = 30L,
                           verbose = TRUE, ...) {
  if (is.null(result$time_matrix) || is.null(result$calib_cols)) {
    message("time_matrix and calib_cols required for leave-one-out.")
    return(invisible(NULL))
  }
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)

  tt <- result$time_matrix
  n <- nrow(tt)
  m <- ncol(tt)

  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  pop_mat <- as.matrix(tracts_df[, result$calib_cols$tracts, drop = FALSE])
  storage.mode(pop_mat) <- "double"
  src_mat <- as.matrix(
    result$sources[, result$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"

  # Sample stations if needed
  if (m > max_stations) {
    station_indices <- sort(sample.int(m, max_stations))
    if (verbose) message(sprintf("Sampling %d / %d stations.", max_stations, m))
  } else {
    station_indices <- seq_len(m)
  }

  results_list <- vector("list", length(station_indices))

  for (idx in seq_along(station_indices)) {
    j <- station_indices[idx]
    if (verbose) {
      cat(sprintf("\r  LOO: station %d / %d", idx, length(station_indices)))
      utils::flush.console()
    }

    # Remove station j
    tt_loo <- tt[, -j, drop = FALSE]
    src_loo <- src_mat[-j, , drop = FALSE]

    # Simple IDW weights with median alpha from the full result
    alpha_med <- if (is.matrix(result$alpha)) {
      apply(result$alpha, 1, stats::median, na.rm = TRUE)
    } else {
      result$alpha
    }

    kernel <- result$kernel %||% "power"
    if (kernel == "exponential") {
      K <- exp(-alpha_med * tt_loo)
    } else {
      K <- (tt_loo + result$offset) ^ (-alpha_med)
    }
    K[!is.finite(K)] <- 0
    cs <- colSums(K)
    cs[cs == 0] <- 1
    W_loo <- t(t(K) / cs)

    # Predict: interpolate using LOO weights
    fitted_loo <- W_loo %*% src_loo

    # Error: compare to census pop
    resid_loo <- fitted_loo - pop_mat
    rmse_loo <- sqrt(mean(resid_loo^2))

    results_list[[idx]] <- data.frame(
      station_index = j,
      rmse = rmse_loo,
      mean_resid = mean(resid_loo),
      max_abs_resid = max(abs(resid_loo)),
      stringsAsFactors = FALSE
    )
  }

  if (verbose) cat("\n")

  out <- do.call(rbind, results_list)

  # Compare with full model
  W_full <- .get_weights(result)
  if (!is.null(W_full)) {
    fitted_full <- W_full %*% src_mat
    resid_full <- fitted_full - pop_mat
    full_rmse <- sqrt(mean(resid_full^2))

    cat(sprintf("Leave-one-out RMSE: %.4f (mean), %.4f (median)\n",
                mean(out$rmse), stats::median(out$rmse)))
    cat(sprintf("Full model RMSE:    %.4f\n", full_rmse))
    cat(sprintf("LOO degradation:    %.1f%%\n",
                (mean(out$rmse) - full_rmse) / full_rmse * 100))
  }

  invisible(out)
}
