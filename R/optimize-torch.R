# ============================================================
# Internal: ADAM + log-domain Sinkhorn via torch (GPU or CPU)
# ============================================================
#
# This is the single optimization engine for the package.
# Uses log-domain Sinkhorn iterations inside torch autograd,
# differentiated end-to-end through K Sinkhorn iterations.
# Works on both CPU and GPU (cuda/mps) devices.

.optimize_torch <- function(time_matrix, pop_matrix, source_matrix,
                             alpha_init, row_targets, sinkhorn_iter,
                             device, dtype, iterations, lr_init,
                             lr_decay, lower_bound, upper_bound,
                             grad_tol, grad_clip, warmup_steps,
                             verbose) {

  if (lr_decay <= 0 || lr_decay >= 1) {
    stop("lr_decay must be in (0, 1), got ", lr_decay, call. = FALSE)
  }

  # --- RStudio subprocess delegation ---
  if (.is_rstudio() && !identical(Sys.getenv("INTERPELECTIONS_SUBPROCESS"), "1")) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "Torch optimization inside RStudio requires the 'callr' package.\n",
        "Install with: install.packages('callr')",
        call. = FALSE
      )
    }
    if (verbose) {
      message("  Running torch optimization in subprocess...")
    }
    result <- callr::r(
      function(time_matrix, pop_matrix, source_matrix, alpha_init,
               row_targets, sinkhorn_iter,
               device, dtype, iterations, lr_init, lr_decay,
               lower_bound, upper_bound, grad_tol, grad_clip,
               warmup_steps, verbose, pkg_path) {
        Sys.setenv(INTERPELECTIONS_SUBPROCESS = "1")
        if (nzchar(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))) {
          pkgload::load_all(pkg_path, quiet = TRUE)
        } else {
          library(interpElections)
        }
        interpElections:::.optimize_torch(
          time_matrix = time_matrix,
          pop_matrix = pop_matrix,
          source_matrix = source_matrix,
          alpha_init = alpha_init,
          row_targets = row_targets,
          sinkhorn_iter = sinkhorn_iter,
          device = device, dtype = dtype,
          iterations = iterations,
          lr_init = lr_init, lr_decay = lr_decay,
          lower_bound = lower_bound, upper_bound = upper_bound,
          grad_tol = grad_tol, grad_clip = grad_clip,
          warmup_steps = warmup_steps,
          verbose = verbose
        )
      },
      args = list(
        time_matrix = time_matrix,
        pop_matrix = pop_matrix,
        source_matrix = source_matrix,
        alpha_init = alpha_init,
        row_targets = row_targets,
        sinkhorn_iter = sinkhorn_iter,
        device = device, dtype = dtype,
        iterations = iterations,
        lr_init = lr_init, lr_decay = lr_decay,
        lower_bound = lower_bound, upper_bound = upper_bound,
        grad_tol = grad_tol, grad_clip = grad_clip,
        warmup_steps = warmup_steps,
        verbose = verbose,
        pkg_path = .find_package_root()
      ),
      show = verbose
    )
    return(result)
  }

  # --- Memory safety check (GPU only) ---
  bytes_per_elem <- if (dtype == "float32") 4 else 8
  n_elements <- as.double(nrow(time_matrix)) * ncol(time_matrix)
  n_copies <- 3 * sinkhorn_iter + 10
  estimated_mb <- (n_elements * bytes_per_elem * n_copies) / 1e6

  if (grepl("cuda", device, fixed = TRUE)) {
    vram_mb <- tryCatch({
      hw <- .detect_gpu_nvidia()
      if (hw$found && !is.na(hw$vram_mb)) hw$vram_mb else NA_real_
    }, error = function(e) NA_real_)

    if (!is.na(vram_mb) && estimated_mb > vram_mb * 0.8) {
      stop(sprintf(paste0(
        "Estimated GPU memory (%d Sinkhorn iters): %.0f MB ",
        "(%.0f MB VRAM available).\n",
        "Reduce sinkhorn_iter or use use_gpu = FALSE."
      ), sinkhorn_iter, estimated_mb, vram_mb), call. = FALSE)
    }

    if (verbose && !is.na(vram_mb)) {
      message(sprintf("  Estimated memory: %.0f MB / %.0f MB VRAM",
                      estimated_mb, vram_mb))
    }
  }

  torch_dtype <- .resolve_dtype(dtype)
  n <- nrow(time_matrix)
  m <- ncol(time_matrix)

  if (verbose) {
    message(sprintf("  Sinkhorn torch (ADAM, %s, %s, %d phases, %d SK iters)",
                    device, dtype, iterations, sinkhorn_iter))
  }

  # Track tensors for cleanup
  gpu_tensors <- new.env(parent = emptyenv())
  on.exit({
    rm(list = ls(gpu_tensors), envir = gpu_tensors)
    gc(verbose = FALSE)
    if (requireNamespace("torch", quietly = TRUE)) {
      tryCatch(torch::cuda_empty_cache(), error = function(e) NULL)
    }
  }, add = TRUE)

  result <- tryCatch({
    # Transfer to tensors
    t_torch <- torch::torch_tensor(
      time_matrix, device = device, dtype = torch_dtype)
    log_t_torch <- torch::torch_log(t_torch)
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

    # Row targets in log space for log-domain Sinkhorn
    log_r_torch <- torch::torch_log(torch::torch_tensor(
      matrix(pmax(row_targets, 1e-30), ncol = 1),
      device = device, dtype = torch_dtype
    ))

    gpu_tensors$t <- t_torch
    gpu_tensors$log_t <- log_t_torch
    gpu_tensors$v <- v_torch
    gpu_tensors$p <- p_torch
    gpu_tensors$a <- a_torch
    gpu_tensors$log_r <- log_r_torch

    K <- as.integer(sinkhorn_iter)

    # Log-domain Sinkhorn objective (numerically stable, works in float32)
    # All ops (log, logsumexp, exp, matmul) support torch autograd.
    f_sinkhorn_torch <- function(a, log_t, p, v, log_r, sk_iter) {
      log_K <- -a * log_t  # [n x m] log-kernel

      # Initialize scaling factors in log space
      log_u <- torch::torch_zeros(c(n, 1L), device = device, dtype = torch_dtype)
      log_v <- torch::torch_zeros(c(1L, m), device = device, dtype = torch_dtype)

      for (i in seq_len(sk_iter)) {
        # Row update: log_u = log(r) - logsumexp_j(log_K_ij + log_v_j)
        log_u <- log_r - torch::torch_logsumexp(log_K + log_v, dim = 2L,
                                                  keepdim = TRUE)
        # Column update: log_v = log(c) - logsumexp_i(log_K_ij + log_u_i)
        # Column targets = 1, so log(c) = 0
        log_v <- -torch::torch_logsumexp(log_K + log_u, dim = 1L,
                                          keepdim = TRUE)
      }

      W <- torch::torch_exp(log_u + log_K + log_v)
      v_hat <- torch::linalg_multi_dot(list(W, v))
      sum((v_hat - p)^2)
    }

    max_inner_iter <- 10000L
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

    for (j in seq_len(iterations)) {
      inner_iter <- 0L
      while (is.finite(diff_val) &&
             abs(diff_val * multiplier) >= 1 &&
             inner_iter < max_inner_iter) {

        # Linear LR warmup in phase 1
        if (j == 1L && warmup_steps > 0L && inner_iter < warmup_steps) {
          warmup_lr <- lr_init * ((inner_iter + 1L) / warmup_steps)
          for (pg in seq_along(optimizer$param_groups)) {
            optimizer$param_groups[[pg]]$lr <- warmup_lr
          }
        } else if (j == 1L && warmup_steps > 0L &&
                   inner_iter == warmup_steps) {
          for (pg in seq_along(optimizer$param_groups)) {
            optimizer$param_groups[[pg]]$lr <- rate
          }
        }

        optimizer$zero_grad()
        loss_val <- f_sinkhorn_torch(a_torch, log_t_torch, p_torch, v_torch,
                                      log_r_torch, K)
        loss_val$backward()

        if (!is.null(grad_clip)) {
          torch::nn_utils_clip_grad_norm_(list(a_torch), grad_clip)
        }

        optimizer$step()

        torch::with_no_grad({
          a_torch$set_data(a_torch$clamp(min = lb, max = ub))
        })

        current <- as.numeric(loss_val$detach()$cpu())

        # NaN guard: if loss diverged, break immediately
        if (!is.finite(current)) {
          if (verbose) message("  Warning: NaN/Inf loss detected, stopping early")
          diff_val <- NaN
          break
        }

        hist_idx <- hist_idx + 1L
        if (hist_idx > length(f_hist)) {
          f_hist <- c(f_hist, numeric(10000L))
        }
        f_hist[hist_idx] <- current

        if (hist_idx >= 2L) {
          diff_val <- f_hist[hist_idx] - f_hist[hist_idx - 1L]
        }

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

      if (verbose && (j == 1L || j %% 5L == 0L || j == iterations)) {
        message(sprintf(
          "  Phase %d/%d: %d steps, objective=%.0f, grad_norm=%.2e",
          j, iterations, inner_iter, current, last_grad_norm))
      }

      if (grad_converged) break

      if (j < iterations) {
        rate <- lr_decay * rate
        for (pg in seq_along(optimizer$param_groups)) {
          optimizer$param_groups[[pg]]$lr <- rate
        }
        diff_val <- 1
        multiplier <- multiplier * 2
      }
    }

    f_hist <- f_hist[seq_len(hist_idx)]

    torch::with_no_grad({
      a_torch$set_data(a_torch$clamp(min = lb, max = ub))
    })
    final_value <- as.numeric(torch::with_no_grad({
      f_sinkhorn_torch(a_torch, log_t_torch, p_torch, v_torch,
                        log_r_torch, K)
    })$cpu())

    alpha_result <- as.numeric(a_torch$detach()$cpu())

    converged <- grad_converged ||
      (hist_idx > 0L && abs(diff_val) < max(1, abs(current) * 1e-6))

    if (verbose) {
      message(sprintf("  Converged in %d steps (%.1fs), objective=%s",
                      k - 1L, NA_real_,
                      format(round(final_value), big.mark = ",")))
    }

    list(
      alpha = alpha_result,
      value = final_value,
      method = paste0("torch_adam_sinkhorn_", device),
      convergence = if (converged) 0L else 1L,
      iterations = k - 1L,
      message = sprintf(
        "ADAM+logSinkhorn on %s (%s), %d steps, %d SK iters, grad_norm=%.2e",
        device, dtype, k - 1L, sinkhorn_iter, last_grad_norm),
      history = f_hist,
      grad_norm_final = last_grad_norm
    )
  }, error = function(e) {
    stop(sprintf(
      "Torch optimization failed on device '%s': %s",
      device, e$message
    ), call. = FALSE)
  })

  result
}
