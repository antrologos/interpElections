# Internal: CPU optimization with L-BFGS-B / BFGS cascade.
#
# When method = "auto", the cascade tries in order:
#   1. Parallel L-BFGS-B (box-constrained, multi-core via optimParallel)
#   2. Serial L-BFGS-B   (box-constrained, single-core via stats::optim)
#   3. BFGS              (unconstrained, alpha clamped post-hoc)
# Each step is attempted only if the previous failed or did not converge.

.optimize_cpu <- function(time_matrix, pop_matrix, source_matrix, alpha_init,
                          method, use_parallel, ncores, lower_bound,
                          upper_bound, maxit, verbose) {

  n <- nrow(time_matrix)

  # Wrappers that pass extra args to exported objective/gradient.
  # Clamp alpha to bounds before evaluation: L-BFGS-B may evaluate
  # at points slightly outside box constraints due to floating-point.
  fn <- function(a) {
    a <- pmin(pmax(a, lower_bound), upper_bound)
    idw_objective(a, time_matrix, pop_matrix, source_matrix)
  }
  gr <- function(a) {
    a <- pmin(pmax(a, lower_bound), upper_bound)
    idw_gradient(a, time_matrix, pop_matrix, source_matrix)
  }

  lower <- rep(lower_bound, n)
  upper <- rep(upper_bound, n)
  result <- NULL
  method_used <- NULL

  # --- Attempt 1: Parallel L-BFGS-B ---
  if (use_parallel && !requireNamespace("optimParallel", quietly = TRUE)) {
    if (verbose) message("optimParallel not installed; falling back to serial.")
    use_parallel <- FALSE
  }
  if (use_parallel && method %in% c("auto", "L-BFGS-B")) {
    if (verbose) message("Trying parallel L-BFGS-B optimization...")

    result <- tryCatch({
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      # Export the package namespace so workers can find internal functions
      parallel::clusterExport(
        cl,
        varlist = c("time_matrix", "pop_matrix", "source_matrix",
                     "fn", "gr"),
        envir = environment()
      )
      # Load the package on workers so internals are available
      parallel::clusterCall(cl, function() {
        if (requireNamespace("interpElections", quietly = TRUE)) {
          loadNamespace("interpElections")
        }
      })
      opt <- optimParallel::optimParallel(
        par = alpha_init,
        fn = fn,
        gr = gr,
        lower = lower,
        upper = upper,
        control = list(maxit = maxit),
        parallel = list(cl = cl)
      )
      opt
    }, error = function(e) {
      if (verbose) message("Parallel L-BFGS-B failed: ", e$message)
      NULL
    })
    if (!is.null(result) && result$convergence == 0) {
      method_used <- "cpu_lbfgsb_parallel"
    } else if (!is.null(result) && method == "auto") {
      # Keep non-converged result as fallback candidate
      if (verbose) {
        message(sprintf(
          "Parallel L-BFGS-B did not converge (code %d), trying next...",
          result$convergence
        ))
      }
      result <- NULL
    } else if (!is.null(result)) {
      method_used <- "cpu_lbfgsb_parallel"
      warning("Parallel L-BFGS-B did not converge (code ",
              result$convergence, "): ", result$message, call. = FALSE)
    }
  }

  # --- Attempt 2: Serial L-BFGS-B ---
  if (is.null(result) && method %in% c("auto", "L-BFGS-B")) {
    if (verbose) message("Trying serial L-BFGS-B optimization...")

    result <- tryCatch({
      stats::optim(
        par = alpha_init,
        fn = fn,
        gr = gr,
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = list(maxit = maxit)
      )
    }, error = function(e) {
      if (verbose) message("Serial L-BFGS-B failed: ", e$message)
      NULL
    })
    if (!is.null(result) && result$convergence == 0) {
      method_used <- "cpu_lbfgsb"
    } else if (!is.null(result) && result$convergence != 0 &&
               method != "auto") {
      # User explicitly requested L-BFGS-B: return with warning
      method_used <- "cpu_lbfgsb"
      warning("L-BFGS-B did not converge (code ", result$convergence, "): ",
              result$message, call. = FALSE)
    } else {
      result <- NULL
    }
  }

  # --- Attempt 3: BFGS ---
  if (is.null(result) && method %in% c("auto", "BFGS")) {
    if (verbose) message("Trying BFGS optimization...")

    result <- tryCatch({
      stats::optim(
        par = alpha_init,
        fn = fn,
        gr = gr,
        method = "BFGS",
        control = list(maxit = maxit)
      )
    }, error = function(e) {
      if (verbose) message("BFGS failed: ", e$message)
      NULL
    })
    if (!is.null(result)) {
      method_used <- "cpu_bfgs"
      if (result$convergence != 0) {
        warning("BFGS did not converge (code ", result$convergence, "): ",
                result$message, call. = FALSE)
      }
    }
  }

  if (is.null(result)) {
    stop("All CPU optimization methods failed", call. = FALSE)
  }

  # Clamp alpha to bounds (BFGS does not enforce box constraints)
  alpha_out <- pmin(pmax(result$par, lower_bound), upper_bound)

  # Recompute objective with clamped alpha if any were clamped
  final_value <- if (any(result$par != alpha_out)) {
    fn(alpha_out)
  } else {
    result$value
  }

  list(
    alpha = alpha_out,
    value = final_value,
    method = method_used,
    convergence = result$convergence,
    iterations = result$counts,
    message = result$message %||% ""
  )
}
