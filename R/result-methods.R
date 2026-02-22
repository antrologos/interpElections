# S3 methods for interpElections_result objects
# summary(), plot(), as.data.frame(), coef(), residuals()

#' Summarize an interpElections result
#'
#' Prints a detailed summary including calibration information,
#' optimization details, and per-variable statistics.
#'
#' @param object An `interpElections_result` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `object`.
#'
#' @exportS3Method
summary.interpElections_result <- function(object, ...) {
  x <- object
  cat("interpElections result summary\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")

  # Header
  if (!is.null(x$code_muni)) {
    if (!is.null(x$nome_municipio)) {
      cat(sprintf("Municipality: %s (%s) -- IBGE: %s, TSE: %s\n",
                  x$nome_municipio, x$uf,
                  x$code_muni, x$code_muni_tse))
      cat(sprintf("Election: %d | Census: %d\n",
                  x$year, x$census_year))
    } else {
      cat(sprintf("Municipality: %s (election %d, census %d)\n",
                  x$code_muni, x$year, x$census_year))
    }
  }

  n <- nrow(x$interpolated)
  m <- nrow(x$sources)
  p <- ncol(x$interpolated)
  cat(sprintf("Census tracts: %d | Sources: %d | Variables: %d\n\n", n, m, p))

  # Calibration info
  if (!is.null(x$calib_cols)) {
    n_calib <- length(x$calib_cols$tracts)
    if (n_calib > 7) {
      cat(sprintf("Calibration: gender x 7 age brackets (%d pairs)\n\n",
                  n_calib))
    } else {
      cat(sprintf("Calibration: %d age brackets\n\n", n_calib))
    }
  }

  # Optimization
  if (!is.null(x$optimization)) {
    cat(sprintf("Optimization: %s | Objective: %.4f | Convergence: %d\n",
                x$optimization$method, x$optimization$value,
                x$optimization$convergence))
  } else {
    cat("Optimization: skipped (alpha user-supplied)\n")
  }

  # Alpha distribution (always shown)
  q <- stats::quantile(x$alpha, c(0.25, 0.5, 0.75))
  cat(sprintf(
    "  Alpha: min=%.3f, Q1=%.3f, median=%.3f, Q3=%.3f, max=%.3f\n\n",
    min(x$alpha), q[1], q[2], q[3], max(x$alpha)
  ))

  # Per-variable summary grouped by type
  mat <- x$interpolated
  dict <- x$dictionary

  if (!is.null(dict) && nrow(dict) > 0) {
    .summary_by_type(mat, dict)
  } else {
    # No dictionary: flat listing with cap
    .summary_flat(mat, max_show = 15L)
  }

  # Object size
  cat(sprintf("\nObject size: %.1f MB",
              as.numeric(utils::object.size(x)) / 1e6))
  has_w <- !is.null(x$weights)
  has_t <- !is.null(x$time_matrix)
  has_s <- !is.null(x$sources_sf)
  has_n <- !is.null(x$neighborhoods)
  if (!has_w && !has_t && !has_s && !has_n) {
    cat(" (lightweight)")
  }
  cat("\n")
  if (has_n) {
    cat(sprintf("Neighborhoods: %d\n", nrow(x$neighborhoods)))
  }

  invisible(x)
}

# Print variable statistics grouped by dictionary type
.summary_by_type <- function(mat, dict) {
  type_order <- c("candidate", "party", "turnout",
                  "demographics", "calibration")
  type_labels <- c(candidate = "Candidates", party = "Parties",
                   turnout = "Turnout", demographics = "Demographics",
                   calibration = "Calibration")
  # Candidates/parties capped; calibration/turnout/demographics always full
  max_rows <- c(candidate = 4L, party = 4L,
                turnout = Inf, demographics = Inf, calibration = Inf)

  types_present <- intersect(type_order, unique(dict$type))
  for (tp in types_present) {
    sub <- dict[dict$type == tp, , drop = FALSE]
    nc <- nrow(sub)
    cat(sprintf("%s (%d):\n", type_labels[tp], nc))

    show_n <- min(nc, max_rows[tp])
    for (i in seq_len(show_n)) {
      col <- sub$column[i]
      label <- .format_dict_label(sub[i, ])
      if (col %in% colnames(mat)) {
        v <- mat[, col]
        if (nzchar(label)) {
          cat(sprintf("  %-24s %-22s total=%10.0f  mean=%8.1f  [%.1f, %.1f]\n",
                      col, label, sum(v), mean(v), min(v), max(v)))
        } else {
          cat(sprintf("  %-24s total=%10.0f  mean=%8.1f  [%.1f, %.1f]\n",
                      col, sum(v), mean(v), min(v), max(v)))
        }
      } else {
        cat(sprintf("  %-24s %s\n", col, label))
      }
    }
    remaining <- nc - show_n
    if (remaining > 0) {
      cat(sprintf("  ... %d more -- View(result$dictionary)\n", remaining))
    }
    cat("\n")
  }
}

# Flat variable listing with cap (when no dictionary)
.summary_flat <- function(mat, max_show = 15L) {
  cat("Interpolated variables:\n")
  cols <- colnames(mat)
  show_n <- min(length(cols), max_show)
  for (i in seq_len(show_n)) {
    col <- cols[i]
    v <- mat[, col]
    cat(sprintf("  %-28s total=%10.0f  mean=%8.1f  [%.1f, %.1f]\n",
                col, sum(v), mean(v), min(v), max(v)))
  }
  remaining <- length(cols) - show_n
  if (remaining > 0) {
    cat(sprintf("  ... and %d more variables\n", remaining))
  }
}


#' Convert result to data frame
#'
#' Drops geometry from `tracts_sf` and returns a plain data frame with
#' census tract IDs and interpolated values.
#'
#' @param x An `interpElections_result` object.
#' @param ... Ignored.
#'
#' @return A data frame.
#'
#' @exportS3Method
as.data.frame.interpElections_result <- function(x, ...) {
  if (!is.null(x$tracts_sf) && requireNamespace("sf", quietly = TRUE)) {
    sf::st_drop_geometry(x$tracts_sf)
  } else {
    as.data.frame(x$interpolated)
  }
}


#' Extract alpha coefficients
#'
#' Returns the alpha decay parameter vector, which plays the role of
#' "coefficients" in the IDW model.
#'
#' @param object An `interpElections_result` object.
#' @param ... Ignored.
#'
#' @return Numeric vector of length n (one alpha per census tract).
#'
#' @exportS3Method
coef.interpElections_result <- function(object, ...) {
  object$alpha
}


#' Compute calibration residuals
#'
#' Returns the matrix of calibration residuals (fitted minus observed)
#' for each census tract and calibration bracket. Requires the weight matrix
#' or travel time matrix to be present in the result (use
#' `keep = "weights"` or `keep = "time_matrix"` when running the
#' interpolation).
#'
#' @param object An `interpElections_result` object.
#' @param ... Ignored.
#'
#' @return Numeric matrix \[n x k\] of residuals (fitted - observed),
#'   where k is the number of calibration brackets. Returns `NULL`
#'   (with a message) if the required data is not available.
#'
#' @exportS3Method
residuals.interpElections_result <- function(object, ...) {
  if (is.null(object$calib_cols) ||
      length(object$calib_cols$tracts) == 0 ||
      length(object$calib_cols$sources) == 0) {
    message("No calibration columns available in result.")
    return(invisible(NULL))
  }
  has_W <- !is.null(object$weights) ||
    (!is.null(object$optimization) && !is.null(object$optimization$W))
  if (!has_W && is.null(object$time_matrix)) {
    message(
      "Cannot compute residuals without weights or time_matrix.\n",
      "Re-run with keep = c(\"weights\") or keep = c(\"time_matrix\")."
    )
    return(invisible(NULL))
  }

  # Source calibration values
  src_mat <- as.matrix(
    object$sources[, object$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"

  # Census tract calibration values
  if (is.null(object$tracts_sf)) {
    stop("Cannot compute residuals: tracts_sf not available", call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for residuals()", call. = FALSE)
  }
  tracts_df <- sf::st_drop_geometry(object$tracts_sf)
  pop_mat <- as.matrix(tracts_df[, object$calib_cols$tracts, drop = FALSE])
  storage.mode(pop_mat) <- "double"

  # Get the weight matrix
  if (!is.null(object$weights)) {
    W <- object$weights
  } else if (!is.null(object$optimization) && !is.null(object$optimization$W)) {
    W <- object$optimization$W
  } else {
    W <- compute_weight_matrix(object$time_matrix, object$alpha,
                                pop_mat, src_mat,
                                offset = object$offset,
                                method = object$optimization$method_type %||% "colnorm")
  }

  # Fitted = W %*% source_matrix
  fitted_vals <- W %*% src_mat

  # Residuals = fitted - observed
  resid <- fitted_vals - pop_mat
  colnames(resid) <- object$calib_cols$tracts
  resid
}
