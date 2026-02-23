# Shared internal helpers for diagnostic functions

#' Collapse n x k alpha matrix to n-vector using a summary function
#' @noRd
.collapse_alpha <- function(alpha, pop_matrix = NULL, summary_fn = "median") {
  if (!is.matrix(alpha) || ncol(alpha) == 1L) return(as.numeric(alpha))
  if (is.numeric(summary_fn) && length(summary_fn) == 1L) {
    idx <- as.integer(summary_fn)
    if (idx < 1L || idx > ncol(alpha))
      stop("Bracket index must be between 1 and ", ncol(alpha), call. = FALSE)
    return(alpha[, idx])
  }
  switch(summary_fn,
    median = apply(alpha, 1, stats::median),
    mean   = rowMeans(alpha),
    min    = apply(alpha, 1, min),
    max    = apply(alpha, 1, max),
    range  = apply(alpha, 1, function(x) diff(range(x))),
    pop_weighted = {
      if (is.null(pop_matrix))
        stop("pop_matrix required for pop_weighted summary", call. = FALSE)
      w <- pop_matrix / rowSums(pop_matrix)
      w[!is.finite(w)] <- 0
      rowSums(alpha * w)
    },
    stop("Unknown summary_fn: '", summary_fn, "'", call. = FALSE)
  )
}


#' Collapse n x k residual matrix to n-vector using a summary function
#' @noRd
.collapse_residuals <- function(resid_mat, summary_fn = "rmse") {
  if (!is.matrix(resid_mat) || ncol(resid_mat) == 1L)
    return(as.numeric(resid_mat))
  if (is.numeric(summary_fn) && length(summary_fn) == 1L) {
    idx <- as.integer(summary_fn)
    if (idx < 1L || idx > ncol(resid_mat))
      stop("Bracket index must be between 1 and ", ncol(resid_mat), call. = FALSE)
    return(resid_mat[, idx])
  }
  switch(summary_fn,
    rmse     = sqrt(rowMeans(resid_mat^2)),
    mean_abs = rowMeans(abs(resid_mat)),
    max_abs  = apply(resid_mat, 1, function(x) max(abs(x))),
    mean     = rowMeans(resid_mat),
    stop("Unknown summary_fn: '", summary_fn, "'", call. = FALSE)
  )
}


#' Human-readable bracket labels from calibration column names
#'
#' Converts column names like "pop_hom_18_20" to "Male 18-20".
#' @param result An interpElections_result object.
#' @return Character vector of bracket labels.
#' @noRd
.bracket_labels <- function(result) {
  cols <- result$calib_cols$tracts
  if (is.null(cols)) return(NULL)
  labels <- gsub("^pop_", "", cols)
  labels <- gsub("^hom_", "Male ", labels)
  labels <- gsub("^mul_", "Female ", labels)
  labels <- gsub("_mais$", "+", labels)
  labels <- gsub("_", "-", labels)
  labels
}


#' Check that weights are available in the result
#' @noRd
.require_weights <- function(result) {
  if (!is.null(result$weights)) return(invisible(TRUE))
  if (!is.null(result$optimization) && !is.null(result$optimization$W)) {
    return(invisible(TRUE))
  }
  stop(
    "Weight matrix not available in result.\n",
    "Weights are kept by default. This result may have been created ",
    "with an older version.",
    call. = FALSE
  )
}


#' Get the weight matrix from a result object
#' @noRd
.get_weights <- function(result) {
  if (!is.null(result$weights)) return(result$weights)
  if (!is.null(result$optimization) && !is.null(result$optimization$W))
    return(result$optimization$W)
  NULL
}


#' Check that electoral_sf is available
#' @noRd
.require_electoral_sf <- function(result) {
  if (!is.null(result$electoral_sf)) return(invisible(TRUE))
  stop(
    "electoral_sf not available in result.\n",
    "Re-run interpolation with keep = \"electoral_sf\".",
    call. = FALSE
  )
}


#' Get station coordinates from result
#'
#' Extracts point coordinates from electoral_sf, transformed to match
#' the CRS of tracts_sf so that raw XY values are consistent with
#' geom_sf panels. Returns a matrix with columns x, y.
#' @noRd
.get_station_coords <- function(result) {
  if (!is.null(result$electoral_sf) &&
      requireNamespace("sf", quietly = TRUE)) {
    esf <- result$electoral_sf
    # Transform to tracts CRS so coordinates match geom_sf panel
    if (!is.null(result$tracts_sf)) {
      esf <- sf::st_transform(esf, sf::st_crs(result$tracts_sf))
    }
    coords <- sf::st_coordinates(esf)
    return(coords[, 1:2, drop = FALSE])
  }
  NULL
}


#' Compute raw, Pearson, or deviance residuals
#' @noRd
.compute_typed_residuals <- function(result, residual_type = "raw") {
  raw_resid <- residuals(result)
  if (is.null(raw_resid)) return(NULL)

  if (residual_type == "raw") return(raw_resid)

  # Need fitted values for Pearson and deviance
  W <- .get_weights(result)
  if (is.null(W)) return(raw_resid)

  src_mat <- as.matrix(
    result$sources[, result$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"
  fitted_vals <- W %*% src_mat

  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  observed <- as.matrix(tracts_df[, result$calib_cols$tracts, drop = FALSE])
  storage.mode(observed) <- "double"

  if (residual_type == "pearson") {
    # (fitted - observed) / sqrt(fitted)
    denom <- sqrt(pmax(fitted_vals, .Machine$double.eps))
    resid <- (fitted_vals - observed) / denom
  } else if (residual_type == "deviance") {
    # sign(raw) * sqrt(2 * (obs * log(obs/fitted) - (obs - fitted)))
    ratio <- observed / pmax(fitted_vals, .Machine$double.eps)
    term <- observed * log(pmax(ratio, .Machine$double.eps)) -
      (observed - fitted_vals)
    resid <- sign(fitted_vals - observed) * sqrt(2 * pmax(term, 0))
  } else {
    stop("Unknown residual_type: '", residual_type, "'", call. = FALSE)
  }

  colnames(resid) <- result$calib_cols$tracts
  resid
}
