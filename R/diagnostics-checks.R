# Automated diagnostic checks for interpElections results

#' Run diagnostic checks on an interpolation result
#'
#' Performs automated validation of an interpElections result. Prints a
#' checklist of PASS/WARN/FAIL results. Only includes checks whose outcome
#' is not guaranteed by construction.
#'
#' @param result An `interpElections_result` object.
#' @param verbose Logical. Print the checklist? Default: TRUE.
#'
#' @return Invisibly, a list of class `"interpElections_diagnostics"` with
#'   a `checks` element containing all computed values.
#'
#' @export
diagnostics <- function(result, verbose = TRUE) {
  if (!inherits(result, "interpElections_result")) {
    stop("'result' must be an interpElections_result object.", call. = FALSE)
  }

  checks <- list()


  # ---- 1. Convergence ----
  checks$convergence <- .diag_convergence(result)

  # ---- 2. Alpha distribution ----
  checks$alpha_distribution <- .diag_alpha_distribution(result)

  # ---- 3. Residual RMSE ----
  checks$residual_rmse <- .diag_residual_rmse(result)

  # ---- 4. Residual bias ----
  checks$residual_bias <- .diag_residual_bias(result, checks$residual_rmse)

  # ---- 5. Population-voter gap ----
  checks$pop_voter_gap <- .diag_pop_voter_gap(result)

  # ---- 6. Unreachable pairs ----
  checks$unreachable_pairs <- .diag_unreachable_pairs(result)

  # ---- 7. Alpha spatial variation ----
  checks$alpha_spatial_cv <- .diag_alpha_spatial_cv(result)

  # ---- 8. Implied turnout ----
  checks$implied_turnout <- .diag_implied_turnout(result)

  out <- structure(
    list(checks = checks),
    class = "interpElections_diagnostics"
  )

  if (verbose) print(out)

  invisible(out)
}


#' Print method for interpElections_diagnostics
#'
#' @param x An `interpElections_diagnostics` object.
#' @param ... Ignored.
#'
#' @exportS3Method
print.interpElections_diagnostics <- function(x, ...) {
  cat("interpElections diagnostics\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  counts <- c(PASS = 0L, WARN = 0L, FAIL = 0L, SKIP = 0L)

  for (chk in x$checks) {
    status <- chk$status
    label <- chk$label
    detail <- chk$detail

    tag <- switch(status,
      pass = "[PASS]",
      warn = "[WARN]",
      fail = "[FAIL]",
      skip = "[SKIP]"
    )
    cat(sprintf(" %s %s: %s\n", tag, label, detail))

    key <- toupper(status)
    if (key %in% names(counts)) counts[key] <- counts[key] + 1L
  }

  cat(paste(rep("-", 50), collapse = ""), "\n")
  parts <- character(0)
  if (counts["PASS"] > 0) parts <- c(parts, sprintf("%d PASS", counts["PASS"]))
  if (counts["WARN"] > 0) parts <- c(parts, sprintf("%d WARN", counts["WARN"]))
  if (counts["FAIL"] > 0) parts <- c(parts, sprintf("%d FAIL", counts["FAIL"]))
  if (counts["SKIP"] > 0) parts <- c(parts, sprintf("%d SKIP", counts["SKIP"]))
  cat(paste(parts, collapse = ", "), "\n")

  invisible(x)
}


# ---- Individual check functions (return list with status, label, detail) ----

#' @noRd
.diag_convergence <- function(result) {
  if (is.null(result$optimization)) {
    return(list(
      status = "skip",
      label = "Convergence",
      detail = "no optimization in result (alpha user-supplied?)",
      value = NA
    ))
  }
  conv <- result$optimization$convergence
  if (identical(conv, 0L) || identical(conv, 0)) {
    list(
      status = "pass",
      label = "Convergence",
      detail = sprintf("optimizer converged (code %d)", conv),
      value = conv
    )
  } else {
    list(
      status = "warn",
      label = "Convergence",
      detail = sprintf("optimizer did NOT converge (code %d)", conv),
      value = conv
    )
  }
}


#' @noRd
.diag_alpha_distribution <- function(result) {
  alpha_vec <- .collapse_alpha(result$alpha, summary_fn = "median")
  med <- stats::median(alpha_vec)
  iqr <- stats::quantile(alpha_vec, c(0.25, 0.75))
  pct_high <- mean(alpha_vec > 15) * 100

  detail <- sprintf(
    "median=%.2f, IQR=[%.2f, %.2f], %.0f%% > 15",
    med, iqr[1], iqr[2], pct_high
  )

  status <- if (pct_high > 5) "warn" else "pass"

  list(
    status = status,
    label = "Alpha distribution",
    detail = detail,
    value = list(median = med, iqr = as.numeric(iqr), pct_high = pct_high)
  )
}


#' @noRd
.diag_residual_rmse <- function(result) {
  resid <- tryCatch(residuals(result), error = function(e) NULL)
  if (is.null(resid)) {
    return(list(
      status = "skip",
      label = "Residual RMSE",
      detail = "cannot compute (weights/time_matrix not available)",
      value = NA
    ))
  }

  overall_rmse <- sqrt(mean(resid^2))
  list(
    status = "pass",
    label = "Residual RMSE",
    detail = sprintf("%.4f (informational)", overall_rmse),
    value = overall_rmse
  )
}


#' @noRd
.diag_residual_bias <- function(result, rmse_check) {
  resid <- tryCatch(residuals(result), error = function(e) NULL)
  if (is.null(resid)) {
    return(list(
      status = "skip",
      label = "Residual bias",
      detail = "cannot compute (weights/time_matrix not available)",
      value = NA
    ))
  }

  col_means <- colMeans(resid)
  max_bias <- max(abs(col_means))

  # Get RMSE value

  rmse_val <- if (!is.na(rmse_check$value) && is.numeric(rmse_check$value)) {
    rmse_check$value
  } else {
    sqrt(mean(resid^2))
  }

  if (rmse_val == 0) {
    return(list(
      status = "pass",
      label = "Residual bias",
      detail = "max |bias| = 0 (zero residuals)",
      value = max_bias
    ))
  }

  ratio <- max_bias / rmse_val
  detail <- sprintf(
    "max |colMeans(resid)| = %.4f (%.1f%% of RMSE)",
    max_bias, ratio * 100
  )

  status <- if (ratio > 0.1) "warn" else "pass"

  list(
    status = status,
    label = "Residual bias",
    detail = detail,
    value = max_bias
  )
}


#' @noRd
.diag_pop_voter_gap <- function(result) {
  if (is.null(result$calib_cols) ||
      length(result$calib_cols$tracts) == 0 ||
      length(result$calib_cols$sources) == 0) {
    return(list(
      status = "skip",
      label = "Population-voter gap",
      detail = "no calibration columns available",
      value = NA
    ))
  }

  # Census total
  if (is.null(result$tracts_sf)) {
    return(list(
      status = "skip",
      label = "Population-voter gap",
      detail = "tracts_sf not available",
      value = NA
    ))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    return(list(
      status = "skip",
      label = "Population-voter gap",
      detail = "sf package not installed",
      value = NA
    ))
  }

  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  census_total <- sum(
    as.matrix(tracts_df[, result$calib_cols$tracts, drop = FALSE]),
    na.rm = TRUE
  )

  source_total <- sum(
    as.matrix(result$sources[, result$calib_cols$sources, drop = FALSE]),
    na.rm = TRUE
  )

  if (census_total == 0 && source_total == 0) {
    return(list(
      status = "pass",
      label = "Population-voter gap",
      detail = "both totals are zero",
      value = 0
    ))
  }

  denom <- max(census_total, source_total)
  gap_pct <- abs(census_total - source_total) / denom * 100

  detail <- sprintf(
    "census=%.0f, source=%.0f, gap=%.1f%%",
    census_total, source_total, gap_pct
  )

  status <- if (gap_pct > 20) {
    "fail"
  } else if (gap_pct > 5) {
    "warn"
  } else {
    "pass"
  }

  list(
    status = status,
    label = "Population-voter gap",
    detail = detail,
    value = gap_pct
  )
}


#' @noRd
.diag_unreachable_pairs <- function(result) {
  if (is.null(result$time_matrix)) {
    return(list(
      status = "skip",
      label = "Unreachable pairs",
      detail = "time_matrix not available (use keep_time = TRUE)",
      value = NA
    ))
  }

  tt <- result$time_matrix
  max_val <- max(tt, na.rm = TRUE)
  # Consider "at or near max" as >= 95% of max value
  threshold <- max_val * 0.95
  n_total <- length(tt)
  n_at_max <- sum(tt >= threshold, na.rm = TRUE)
  pct <- n_at_max / n_total * 100

  detail <- sprintf(
    "%.1f%% of pairs near max travel time (>= %.0f min)",
    pct, threshold
  )

  status <- if (pct > 10) {
    "fail"
  } else if (pct > 1) {
    "warn"
  } else {
    "pass"
  }

  list(
    status = status,
    label = "Unreachable pairs",
    detail = detail,
    value = pct
  )
}


#' @noRd
.diag_alpha_spatial_cv <- function(result) {
  alpha_vec <- .collapse_alpha(result$alpha, summary_fn = "median")

  if (length(alpha_vec) < 3) {
    return(list(
      status = "skip",
      label = "Alpha spatial variation",
      detail = "too few tracts for CV computation",
      value = NA
    ))
  }

  mu <- mean(alpha_vec)
  if (mu == 0) {
    return(list(
      status = "warn",
      label = "Alpha spatial variation",
      detail = "mean alpha is 0 (all identical)",
      value = 0
    ))
  }

  cv <- stats::sd(alpha_vec) / mu

  detail <- sprintf("CV of median-alpha = %.3f", cv)
  status <- if (cv < 0.1) "warn" else "pass"

  if (status == "warn") {
    detail <- paste0(detail, " (low variation: alpha may not be adapting)")
  }

  list(
    status = status,
    label = "Alpha spatial variation",
    detail = detail,
    value = cv
  )
}


#' @noRd
.diag_implied_turnout <- function(result) {
  W <- .get_weights(result)
  if (is.null(W)) {
    return(list(
      status = "skip",
      label = "Implied turnout",
      detail = "weights not available",
      value = NA
    ))
  }

  if (is.null(result$calib_cols) ||
      length(result$calib_cols$sources) == 0) {
    return(list(
      status = "skip",
      label = "Implied turnout",
      detail = "no calibration columns in result",
      value = NA
    ))
  }

  if (is.null(result$tracts_sf) ||
      !requireNamespace("sf", quietly = TRUE)) {
    return(list(
      status = "skip",
      label = "Implied turnout",
      detail = "tracts_sf or sf package not available",
      value = NA
    ))
  }

  # Source calibration matrix
  src_mat <- as.matrix(
    result$sources[, result$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"

  # Total voters allocated per tract
  implied_voters <- rowSums(W %*% src_mat)

  # Census population per tract
  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  pop_mat <- as.matrix(tracts_df[, result$calib_cols$tracts, drop = FALSE])
  storage.mode(pop_mat) <- "double"
  census_pop <- rowSums(pop_mat)

  # Avoid division by zero
  safe_pop <- ifelse(census_pop > 0, census_pop, NA_real_)
  turnout_rate <- implied_voters / safe_pop

  pct_over <- mean(turnout_rate > 1.1, na.rm = TRUE) * 100

  detail <- sprintf(
    "%.1f%% of tracts with implied turnout > 110%%",
    pct_over
  )

  status <- if (pct_over > 5) "warn" else "pass"

  list(
    status = status,
    label = "Implied turnout",
    detail = detail,
    value = pct_over
  )
}
