# ============================================================
# Internal: Per-Bracket SGD via torch (GPU or CPU)
# ============================================================
#
# This is the single optimization engine for the package.
# Uses mini-batch per-bracket log-domain Sinkhorn inside torch
# autograd. Each demographic bracket gets its own Sinkhorn
# transport; gradients flow through all unrolled iterations.
# Works on both CPU and GPU (cuda/mps) devices.

.optimize_torch <- function(time_matrix, pop_matrix, source_matrix,
                             alpha_init, batch_size, sk_iter,
                             max_steps, lr_init, device, dtype,
                             lower_bound, upper_bound, verbose) {

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
               batch_size, sk_iter, max_steps, lr_init,
               device, dtype, lower_bound, upper_bound,
               verbose, pkg_path) {
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
          batch_size = batch_size, sk_iter = sk_iter,
          max_steps = max_steps, lr_init = lr_init,
          device = device, dtype = dtype,
          lower_bound = lower_bound, upper_bound = upper_bound,
          verbose = verbose
        )
      },
      args = list(
        time_matrix = time_matrix,
        pop_matrix = pop_matrix,
        source_matrix = source_matrix,
        alpha_init = alpha_init,
        batch_size = batch_size, sk_iter = sk_iter,
        max_steps = max_steps, lr_init = lr_init,
        device = device, dtype = dtype,
        lower_bound = lower_bound, upper_bound = upper_bound,
        verbose = verbose,
        pkg_path = .find_package_root()
      ),
      show = verbose
    )
    return(result)
  }

  torch_dtype <- .resolve_dtype(dtype)
  n <- nrow(time_matrix)
  m <- ncol(time_matrix)
  k <- ncol(pop_matrix)
  b <- min(as.integer(batch_size), n)
  lb <- lower_bound
  ub <- upper_bound

  # --- Per-bracket preprocessing (R-side) ---
  P_cpu <- pop_matrix
  V_cpu <- source_matrix
  r_mat <- matrix(0, k, n)
  c_mat <- matrix(0, k, m)
  skip <- logical(k)

  for (bi in seq_len(k)) {
    rb <- P_cpu[, bi]
    cb <- V_cpu[, bi]
    if (sum(cb) < 0.5 || sum(rb) < 0.5) {
      skip[bi] <- TRUE
      next
    }
    # Rescale row targets so sum(rb) == sum(cb)
    rb <- rb / sum(rb) * sum(cb)
    r_mat[bi, ] <- rb
    c_mat[bi, ] <- cb
  }

  active <- which(!skip)
  ka <- length(active)

  if (ka == 0L) {
    stop("All demographic brackets are empty (sum < 0.5). Cannot optimize.",
         call. = FALSE)
  }

  r_mat <- r_mat[active, , drop = FALSE]
  c_mat <- c_mat[active, , drop = FALSE]
  r_totals <- rowSums(r_mat)

  # --- Memory safety check (GPU only) ---
  bytes_per_elem <- if (dtype == "float32") 4 else 8
  estimated_mb <- as.double(ka) * b * m * bytes_per_elem *
    (2 + 2 * sk_iter) / 1e6

  if (grepl("cuda", device, fixed = TRUE)) {
    vram_mb <- tryCatch({
      hw <- .detect_gpu_nvidia()
      if (hw$found && !is.na(hw$vram_mb)) hw$vram_mb else NA_real_
    }, error = function(e) NA_real_)

    if (!is.na(vram_mb) && estimated_mb > vram_mb * 0.8) {
      stop(sprintf(paste0(
        "Estimated GPU memory (ka=%d, batch=%d, sk_iter=%d): %.0f MB ",
        "(%.0f MB VRAM available).\n",
        "Reduce batch_size, sk_iter, or use use_gpu = FALSE."
      ), ka, b, sk_iter, estimated_mb, vram_mb), call. = FALSE)
    }

    if (verbose && !is.na(vram_mb)) {
      message(sprintf("  Estimated memory: %.0f MB / %.0f MB VRAM",
                      estimated_mb, vram_mb))
    }
  }

  if (verbose) {
    message(sprintf(
      "  PB-SGD (%s, %s): ka=%d, batch=%d, sk_iter=%d, %d steps",
      device, dtype, ka, b, sk_iter, max_steps))
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
    log_t_torch <- torch::torch_log(torch::torch_tensor(
      time_matrix, device = device, dtype = torch_dtype))
    p_torch <- torch::torch_tensor(
      pop_matrix, device = device, dtype = torch_dtype)
    v_torch <- torch::torch_tensor(
      source_matrix, device = device, dtype = torch_dtype)

    a_torch <- torch::torch_tensor(
      rep(alpha_init, length.out = n),
      device = device,
      requires_grad = TRUE,
      dtype = torch_dtype
    )

    gpu_tensors$log_t <- log_t_torch
    gpu_tensors$p <- p_torch
    gpu_tensors$v <- v_torch
    gpu_tensors$a <- a_torch

    optimizer <- torch::optim_adam(list(a_torch), lr = lr_init, amsgrad = TRUE)

    best_loss <- Inf
    best_alpha <- alpha_init  # Guard: never return NULL
    losses <- numeric(max_steps)
    last_grad_norm <- NA_real_

    for (step in seq_len(max_steps)) {
      # LR schedule: halve at steps 200, 400, 600
      if (step %in% c(200L, 400L, 600L)) {
        for (pg in optimizer$param_groups) {
          pg$lr <- pg$lr * 0.5
        }
      }

      # Sample mini-batch
      idx <- sample.int(n, b)
      r_batch <- r_mat[, idx, drop = FALSE]
      batch_totals <- rowSums(r_batch)
      scale_f <- batch_totals / pmax(r_totals, 1e-30)
      c_batch <- c_mat * scale_f

      # Build 3D tensors for per-bracket Sinkhorn
      log_r_3d <- torch::torch_tensor(
        array(log(pmax(r_batch, 1e-30)), dim = c(ka, b, 1L)),
        device = device, dtype = torch_dtype)
      log_c_3d <- torch::torch_tensor(
        array(log(pmax(c_batch, 1e-30)), dim = c(ka, 1L, m)),
        device = device, dtype = torch_dtype)

      optimizer$zero_grad()

      # Build log-kernel for batch
      log_t_batch <- log_t_torch[idx, ]
      log_K <- -a_torch[idx]$unsqueeze(2L) * log_t_batch
      log_K_3d <- log_K$unsqueeze(1L)

      # Log-domain Sinkhorn iterations (per bracket)
      log_u <- torch::torch_zeros(c(ka, b, 1L),
                                   device = device, dtype = torch_dtype)
      log_v <- torch::torch_zeros(c(ka, 1L, m),
                                   device = device, dtype = torch_dtype)

      for (i in seq_len(sk_iter)) {
        log_u <- log_r_3d - torch::torch_logsumexp(
          log_K_3d + log_v, dim = 3L, keepdim = TRUE)
        log_v <- log_c_3d - torch::torch_logsumexp(
          log_K_3d + log_u, dim = 2L, keepdim = TRUE)
      }

      # Aggregate across brackets and column-normalize
      W_all <- torch::torch_exp(log_u + log_K_3d + log_v)
      W_total <- W_all$sum(dim = 1L)
      cs <- W_total$sum(dim = 1L, keepdim = TRUE) + 1e-30
      W_norm <- W_total / cs

      # Compute batch loss
      V_scaled <- v_torch * (as.double(b) / n)
      V_hat <- torch::torch_mm(W_norm, V_scaled)
      P_batch <- p_torch[idx, ]
      loss <- (V_hat - P_batch)$pow(2)$sum() * (as.double(n) / b)

      loss$backward()

      # Gradient masking: only update sampled tracts
      torch::with_no_grad({
        grad <- a_torch$grad
        mask <- torch::torch_zeros_like(grad)
        mask[idx] <- 1
        a_torch$grad <- grad * mask
      })

      # Gradient clipping (norm 5.0)
      torch::nn_utils_clip_grad_norm_(list(a_torch), 5.0)

      optimizer$step()

      # Clamp alpha to [lower_bound, upper_bound]
      torch::with_no_grad({
        a_torch$clamp_(lb, ub)
      })

      lv <- loss$item()
      losses[step] <- lv

      if (is.finite(lv) && lv < best_loss) {
        best_loss <- lv
        best_alpha <- as.numeric(a_torch$detach()$cpu())
      }

      last_grad_norm <- tryCatch(
        as.numeric(a_torch$grad$norm()$cpu()),
        error = function(e) NA_real_
      )

      if (verbose && step %% 100L == 0L) {
        message(sprintf("  Step %3d/%d: loss=%s, grad=%.2e",
                        step, max_steps,
                        format(round(lv), big.mark = ","),
                        last_grad_norm))
      }
    }

    list(
      alpha = best_alpha,
      value = best_loss,
      method = paste0("pb_sgd_sinkhorn_", device),
      convergence = 0L,
      iterations = max_steps,
      message = sprintf(
        "PB-SGD on %s (%s), %d steps, batch=%d, %d brackets, sk_iter=%d",
        device, dtype, max_steps, b, ka, sk_iter),
      history = losses,
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
