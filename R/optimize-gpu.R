# Internal: GPU optimization with ADAM via torch

.optimize_gpu <- function(time_matrix, pop_matrix, source_matrix, alpha_init,
                          device, dtype, iterations, lr_init, lr_decay,
                          lower_bound, upper_bound, grad_tol, grad_clip,
                          warmup_steps, verbose) {

  if (lr_decay <= 0 || lr_decay >= 1) {
    stop("lr_decay must be in (0, 1), got ", lr_decay, call. = FALSE)
  }

  # --- RStudio subprocess delegation ---
  # RStudio's embedded R session event loop (environment pane, autocompletion,
  # etc.) conflicts with CUDA kernel execution and can cause fatal crashes.
  # When running inside RStudio, we offload GPU optimization to a clean
  # subprocess via callr::r() where torch CUDA works without interference.
  if (.is_rstudio() && !identical(Sys.getenv("INTERPELECTIONS_SUBPROCESS"), "1")) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "GPU optimization inside RStudio requires the 'callr' package.\n",
        "Install with: install.packages('callr')\n",
        "This is needed because RStudio's event loop conflicts with CUDA.",
        call. = FALSE
      )
    }
    if (verbose) {
      message("  Running GPU optimization in subprocess (RStudio CUDA workaround)...")
    }
    result <- callr::r(
      function(time_matrix, pop_matrix, source_matrix, alpha_init,
               device, dtype, iterations, lr_init, lr_decay,
               lower_bound, upper_bound, grad_tol, grad_clip,
               warmup_steps, verbose, pkg_path) {
        Sys.setenv(INTERPELECTIONS_SUBPROCESS = "1")
        # Load the package from the same location
        if (nzchar(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))) {
          pkgload::load_all(pkg_path, quiet = TRUE)
        } else {
          library(interpElections)
        }
        interpElections:::.optimize_gpu(
          time_matrix = time_matrix,
          pop_matrix = pop_matrix,
          source_matrix = source_matrix,
          alpha_init = alpha_init,
          device = device,
          dtype = dtype,
          iterations = iterations,
          lr_init = lr_init,
          lr_decay = lr_decay,
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          grad_tol = grad_tol,
          grad_clip = grad_clip,
          warmup_steps = warmup_steps,
          verbose = verbose
        )
      },
      args = list(
        time_matrix = time_matrix,
        pop_matrix = pop_matrix,
        source_matrix = source_matrix,
        alpha_init = alpha_init,
        device = device,
        dtype = dtype,
        iterations = iterations,
        lr_init = lr_init,
        lr_decay = lr_decay,
        lower_bound = lower_bound,
        upper_bound = upper_bound,
        grad_tol = grad_tol,
        grad_clip = grad_clip,
        warmup_steps = warmup_steps,
        verbose = verbose,
        pkg_path = .find_package_root()
      ),
      show = verbose  # stream subprocess stdout/stderr to RStudio console
    )
    return(result)
  }

  # --- VRAM safety check ---
  # The forward + backward pass creates ~6 copies of the time_matrix
  # (original, t^(-a), column-standardized, plus autograd intermediates).
  bytes_per_elem <- if (dtype == "float32") 4 else 8
  n_elements <- as.double(nrow(time_matrix)) * ncol(time_matrix)
  estimated_mb <- (n_elements * bytes_per_elem * 6) / 1e6

  if (grepl("cuda", device, fixed = TRUE)) {
    vram_mb <- tryCatch({
      hw <- .detect_gpu_nvidia()
      if (hw$found && !is.na(hw$vram_mb)) hw$vram_mb else NA_real_
    }, error = function(e) NA_real_)

    if (!is.na(vram_mb) && estimated_mb > vram_mb * 0.8) {
      stop(sprintf(paste0(
        "Estimated GPU memory: %.0f MB (%.0f MB VRAM available).\n",
        "This municipality is too large for GPU optimization with %s.\n",
        "Use use_gpu = FALSE to fall back to CPU optimization."
      ), estimated_mb, vram_mb, dtype), call. = FALSE)
    }

    if (verbose && !is.na(vram_mb)) {
      message(sprintf("  Estimated GPU memory: %.0f MB / %.0f MB VRAM",
                      estimated_mb, vram_mb))
    }
  }

  torch_dtype <- .resolve_dtype(dtype)

  # Track GPU tensors for cleanup on exit (error or normal)
  gpu_tensors <- new.env(parent = emptyenv())
  on.exit({
    # Delete tensor references so GC can free GPU memory
    rm(list = ls(gpu_tensors), envir = gpu_tensors)
    gc(verbose = FALSE)
    if (requireNamespace("torch", quietly = TRUE)) {
      tryCatch(torch::cuda_empty_cache(),
               error = function(e) NULL)
    }
  }, add = TRUE)

  result <- tryCatch({
    # Transfer to GPU tensors
    t_torch <- torch::torch_tensor(
      time_matrix, device = device, dtype = torch_dtype)
    v_torch <- torch::torch_tensor(
      source_matrix, device = device, dtype = torch_dtype)
    p_torch <- torch::torch_tensor(
      pop_matrix, device = device, dtype = torch_dtype)

    a_torch <- torch::torch_tensor(
      matrix(alpha_init, ncol = 1),
      device = device,
      requires_grad = TRUE,
      dtype = torch_dtype
    )

    gpu_tensors$t <- t_torch
    gpu_tensors$v <- v_torch
    gpu_tensors$p <- p_torch
    gpu_tensors$a <- a_torch

    # Objective function (closure over tensors)
    f_torch <- function(a, t, p, v) {
      tmA <- t ^ (-a)
      tmA_std <- tmA / tmA$sum(1, keepdim = TRUE)
      v_hat <- torch::linalg_multi_dot(list(tmA_std, v))
      sum((v_hat - p)^2)
    }

    max_inner_iter <- 10000L
    # Pre-allocate conservatively; will grow if needed
    f_hist <- numeric(min(max_inner_iter * iterations, 50000L))
    hist_idx <- 0L
    k <- 1L
    rate <- lr_init
    lb <- lower_bound
    ub <- upper_bound
    grad_converged <- FALSE
    last_grad_norm <- NA_real_

    optimizer <- torch::optim_adam(a_torch, lr = rate, amsgrad = TRUE)
    diff_val <- 1
    multiplier <- 1

    # Outer loop: each iteration reduces the LR and doubles the convergence
    # threshold (multiplier), creating a coarse-to-fine annealing schedule.
    # Momentum is preserved across phases (only LR changes), retaining
    # Adam's curvature information from earlier steps.
    # Inner loop: run ADAM steps until objective change or gradient norm
    # indicates convergence.
    for (j in seq_len(iterations)) {
      inner_iter <- 0L
      while (abs(diff_val * multiplier) >= 1 &&
             inner_iter < max_inner_iter) {

        # Linear LR warmup in phase 1 to prevent cold-start oscillations
        if (j == 1L && warmup_steps > 0L && inner_iter < warmup_steps) {
          warmup_lr <- lr_init * ((inner_iter + 1L) / warmup_steps)
          for (pg in seq_along(optimizer$param_groups)) {
            optimizer$param_groups[[pg]]$lr <- warmup_lr
          }
        } else if (j == 1L && warmup_steps > 0L &&
                   inner_iter == warmup_steps) {
          # Restore full LR after warmup completes
          for (pg in seq_along(optimizer$param_groups)) {
            optimizer$param_groups[[pg]]$lr <- rate
          }
        }

        # Forward + backward pass (split from optimizer step so gradient
        # clipping can be applied between backward() and step())
        optimizer$zero_grad()
        loss_val <- f_torch(a_torch, t_torch, p_torch, v_torch)
        loss_val$backward()

        # Gradient clipping (optional): limit gradient norm to prevent
        # large steps on steep regions of the objective landscape.
        if (!is.null(grad_clip)) {
          torch::nn_utils_clip_grad_norm_(list(a_torch), grad_clip)
        }

        # ADAM parameter update (uses clipped gradients if enabled)
        optimizer$step()

        # Project alpha back to feasible region [lower_bound, upper_bound].
        # Lower clamp prevents negative alphas that invert the weight-distance
        # relationship. Upper clamp prevents runaway alphas in flat landscape
        # regions (alpha > ~10 produces identical weights regardless of value).
        torch::with_no_grad({
          a_torch$set_data(a_torch$clamp(min = lb, max = ub))
        })

        current <- as.numeric(loss_val$detach()$cpu())

        hist_idx <- hist_idx + 1L
        if (hist_idx > length(f_hist)) {
          f_hist <- c(f_hist, numeric(10000L))
        }
        f_hist[hist_idx] <- current

        if (hist_idx >= 2L) {
          diff_val <- f_hist[hist_idx] - f_hist[hist_idx - 1L]
        }

        # Gradient norm convergence check (first-order optimality)
        last_grad_norm <- as.numeric(a_torch$grad$norm()$cpu())
        if (last_grad_norm < grad_tol) {
          grad_converged <- TRUE
          k <- k + 1L
          inner_iter <- inner_iter + 1L
          break
        }

        k <- k + 1L
        inner_iter <- inner_iter + 1L
      }

      # Log per-phase summary (first, every 5th, and last)
      if (verbose && (j == 1L || j %% 5L == 0L || j == iterations)) {
        message(sprintf(
          "  Phase %d/%d: %d steps, objective=%.0f, grad_norm=%.2e",
          j, iterations, inner_iter, current, last_grad_norm))
      }

      # Early exit if gradient norm converged
      if (grad_converged) break

      if (j < iterations) {
        rate <- lr_decay * rate
        # Update LR on existing optimizer, preserving Adam momentum buffers
        # (exponential moving averages of gradients and squared gradients).
        # This retains curvature information across phases.
        for (pg in seq_along(optimizer$param_groups)) {
          optimizer$param_groups[[pg]]$lr <- rate
        }
        diff_val <- 1
        multiplier <- multiplier * 2
      }
    }

    # Trim history to actual length
    f_hist <- f_hist[seq_len(hist_idx)]

    # Clamp alpha in-place and compute final value (avoids extra tensor)
    torch::with_no_grad({
      a_torch$set_data(a_torch$clamp(min = lb, max = ub))
    })
    final_value <- as.numeric(torch::with_no_grad({
      f_torch(a_torch, t_torch, p_torch, v_torch)
    })$cpu())

    # Transfer clamped result back to CPU
    alpha_result <- as.numeric(a_torch$detach()$cpu())

    # Convergence: gradient norm criterion OR relative objective change
    converged <- grad_converged ||
      (hist_idx > 0L && abs(diff_val) < max(1, abs(current) * 1e-6))

    list(
      alpha = alpha_result,
      value = final_value,
      method = "gpu_adam",
      convergence = if (converged) 0L else 1L,
      iterations = k - 1L,
      message = sprintf(
        "ADAM optimizer on %s (%s), %d steps, final grad_norm=%.2e",
        device, dtype, k - 1L, last_grad_norm),
      history = f_hist,
      grad_norm_final = last_grad_norm
    )
  }, error = function(e) {
    stop(sprintf(
      "GPU optimization failed on device '%s': %s",
      device, e$message
    ), call. = FALSE)
  })

  result
}
