#' Re-interpolate with different electoral variables
#'
#' Takes a previous [interpolate_election_br()] result and re-runs the
#' interpolation pipeline using the stored travel time matrix, skipping
#' the expensive routing step. Useful for switching to different candidates,
#' parties, `what` types, or election rounds without recomputing travel times.
#'
#' The previous result must contain a `time_matrix` (always kept by default).
#'
#' When `reuse_weights = TRUE` (the default) and the stored weight matrix
#' is available, optimization is also skipped — making re-interpolation
#' near-instantaneous. Set `reuse_weights = FALSE` to force re-optimization
#' (e.g., when switching `turno` or `cargo`, which changes the calibration
#' brackets).
#'
#' @param result An `interpElections_result` object from
#'   [interpolate_election_br()].
#' @param what Character vector. Controls what information is interpolated.
#'   See [interpolate_election_br()]. If NULL (default), reuses the original.
#' @param candidates Character or numeric vector, or NULL. Filter candidates.
#'   See [interpolate_election_br()].
#' @param parties Character vector or NULL. Filter parties.
#'   See [interpolate_election_br()].
#' @param cargo Integer, character, or NULL. Override the electoral office.
#'   See [interpolate_election_br()].
#' @param turno Integer or NULL. Override the election round (1 or 2).
#' @param reuse_weights Logical. If TRUE (default), reuse the weight matrix
#'   from the original result to skip optimization. Automatically disabled
#'   when `turno` or `cargo` change (which alter calibration brackets).
#' @param optim An [optim_control()] object with optimization parameters.
#'   Only used when `reuse_weights = FALSE`.
#' @param keep Character vector or NULL. Extra objects to include in result.
#'   `weights`, `time_matrix`, `electoral_sf`, and `rep_points` are always
#'   kept. Options: `"pop_raster"`.
#' @param verbose Logical. Print progress messages. Default: TRUE.
#' @param ... Additional arguments forwarded to [interpolate_election_br()].
#'
#' @return An `interpElections_result` object (same structure as the
#'   original, with updated interpolation).
#'
#' @examples
#' \dontrun{
#' # First run
#' result <- interpolate_election_br(
#'   3550308, 2022,
#'   cargo = "presidente"
#' )
#'
#' # Re-interpolate for parties (reuses weights, near-instant)
#' result2 <- reinterpolate(result, what = "parties")
#'
#' # Re-interpolate for a different cargo (forces re-optimization)
#' result3 <- reinterpolate(result, cargo = "governador")
#'
#' # Re-interpolate for turno 2 (forces re-optimization)
#' result4 <- reinterpolate(result, turno = 2)
#'
#' # Force re-optimization even when calibration hasn't changed
#' result5 <- reinterpolate(result, what = "parties",
#'                           reuse_weights = FALSE)
#' }
#'
#' @seealso [interpolate_election_br()]
#'
#' @export
reinterpolate <- function(
    result,
    what           = NULL,
    candidates     = NULL,
    parties        = NULL,
    cargo          = NULL,
    turno          = NULL,
    reuse_weights  = TRUE,
    optim          = optim_control(),
    keep           = NULL,
    verbose        = TRUE,
    ...
) {
  if (!inherits(result, "interpElections_result")) {
    stop("`result` must be an interpElections_result object.", call. = FALSE)
  }
  if (is.null(result$time_matrix)) {
    stop(
      "No time_matrix found in the result.\n",
      "Re-run the original interpolation.",
      call. = FALSE
    )
  }
  if (is.null(result$code_muni)) {
    stop("No code_muni found in the result. reinterpolate() requires a ",
         "result from interpolate_election_br().", call. = FALSE)
  }

  # Inherit from original result
  municipality <- result$code_muni
  year         <- result$year
  if (is.null(what))  what  <- result$what
  if (is.null(turno)) turno <- result$turno %||% 1L

  # Determine if weights can be reused
  use_weights <- NULL
  if (reuse_weights && !is.null(result$weights)) {
    # Disable weight reuse when calibration changes
    turno_changed <- !is.null(turno) && !identical(as.integer(turno),
                                                    as.integer(result$turno))
    cargo_changed <- !is.null(cargo)
    if (turno_changed || cargo_changed) {
      if (verbose) {
        message("Calibration changed (turno/cargo). Re-optimizing weights.")
      }
    } else {
      use_weights <- result$weights
      if (verbose) {
        message("Reusing weights from previous result (skipping optimization).")
      }
    }
  }

  interpolate_election_br(
    municipality = municipality,
    year         = year,
    cargo        = cargo,
    turno        = turno,
    what         = what,
    candidates   = candidates,
    parties      = parties,
    time_matrix  = result$time_matrix,
    weights      = use_weights,
    optim        = optim,
    keep         = keep,
    verbose      = verbose,
    ...
  )
}
