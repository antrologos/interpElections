# Internal: GPU optimization with ADAM via torch

.optimize_gpu <- function(time_matrix, pop_matrix, source_matrix, alpha_init,
                          device, dtype, iterations, lr_init, lr_decay,
                          lower_bound, upper_bound, verbose) {

  if (lr_decay <= 0 || lr_decay >= 1) {
    stop("lr_decay must be in (0, 1), got ", lr_decay, call. = FALSE)
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

    optimizer <- torch::optim_adam(a_torch, lr = rate, amsgrad = TRUE)
    diff_val <- 1
    multiplier <- 1

    loss_fn <- function() {
      optimizer$zero_grad()
      value <- f_torch(a_torch, t_torch, p_torch, v_torch)
      value$backward()
      value
    }

    # Outer loop: each iteration reduces the LR and doubles the convergence
    # threshold (multiplier), creating a coarse-to-fine annealing schedule.
    # A new optimizer is created at each outer iteration (resetting momentum),
    # matching the original researchers' implementation.
    # Inner loop: run ADAM steps until the change in objective < 1/multiplier.
    for (j in seq_len(iterations)) {
      inner_iter <- 0L
      while (abs(diff_val * multiplier) >= 1 &&
             inner_iter < max_inner_iter) {
        loss_val <- optimizer$step(loss_fn)

        # Project alpha back to feasible region [lower_bound, upper_bound].
        # Lower clamp prevents negative alphas that invert the weight-distance
        # relationship. Upper clamp prevents runaway alphas in flat landscape
        # regions (alpha > ~10 produces identical weights regardless of value).
        # Use $set_data() to avoid in-place modification of the autograd leaf.
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

        if (verbose && k %% 50 == 0) {
          message(sprintf(
            "  iter=%d, f=%.4f, diff=%.6f, lr=%.6f",
            k, current, diff_val, rate
          ))
        }
        k <- k + 1L
        inner_iter <- inner_iter + 1L
      }

      if (j < iterations) {
        rate <- lr_decay * rate
        # Create a new optimizer at the reduced LR, resetting momentum
        # buffers. This matches the original implementation's coarse-to-fine
        # annealing: each phase starts fresh with a smaller step size.
        optimizer <- torch::optim_adam(a_torch, lr = rate, amsgrad = TRUE)
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

    # Use relative convergence: change < 1e-6 * |current|
    converged <- hist_idx > 0L &&
      abs(diff_val) < max(1, abs(current) * 1e-6)

    list(
      alpha = alpha_result,
      value = final_value,
      method = "gpu_adam",
      convergence = if (converged) 0L else 1L,
      iterations = k - 1L,
      message = sprintf(
        "ADAM optimizer on %s (%s), %d steps",
        device, dtype, k - 1L),
      history = f_hist
    )
  }, error = function(e) {
    stop(sprintf(
      "GPU optimization failed on device '%s': %s",
      device, e$message
    ), call. = FALSE)
  })

  result
}
