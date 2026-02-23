#' Re-interpolate with different electoral variables
#'
#' Takes a previous [interpolate_election_br()] result and re-runs the
#' interpolation pipeline using the stored travel time matrix, skipping
#' the expensive routing step. Useful for switching to different candidates,
#' parties, `what` types, or election rounds without recomputing travel times.
#'
#' The previous result must contain a `time_matrix` (always kept
#' by default since v0.2).
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
#' @param keep Character vector or NULL. Heavy objects to include in result.
#'   See [interpolate_election_br()].
#' @param verbose Logical. Print progress messages. Default: TRUE.
#' @param ... Additional arguments forwarded to [interpolate_election_br()].
#'
#' @return An `interpElections_result` object (same structure as the
#'   original, with updated interpolation).
#'
#' @examples
#' \dontrun{
#' # First run (time_matrix is kept by default)
#' result <- interpolate_election_br(
#'   3550308, 2022,
#'   cargo = "presidente"
#' )
#'
#' # Re-interpolate for parties instead of candidates
#' result2 <- reinterpolate(result, what = "parties")
#'
#' # Re-interpolate for a different cargo
#' result3 <- reinterpolate(result, cargo = "governador")
#'
#' # Re-interpolate for turno 2 (runoff)
#' result4 <- reinterpolate(result, turno = 2)
#' }
#'
#' @seealso [interpolate_election_br()]
#'
#' @export
reinterpolate <- function(
    result,
    what       = NULL,
    candidates = NULL,
    parties    = NULL,
    cargo      = NULL,
    turno      = NULL,
    keep       = NULL,
    verbose    = TRUE,
    ...
) {
  if (!inherits(result, "interpElections_result")) {
    stop("`result` must be an interpElections_result object.", call. = FALSE)
  }
  if (is.null(result$time_matrix)) {
    stop(
      "No time_matrix found in the result.\n",
      "Re-run the original interpolation with keep = \"time_matrix\".",
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

  interpolate_election_br(
    municipality = municipality,
    year         = year,
    cargo        = cargo,
    turno        = turno,
    what         = what,
    candidates   = candidates,
    parties      = parties,
    time_matrix  = result$time_matrix,
    keep         = keep,
    verbose      = verbose,
    ...
  )
}
