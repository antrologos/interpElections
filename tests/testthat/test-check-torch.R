# --- .detect_gpu_nvidia() -----------------------------------------------------

test_that(".detect_gpu_nvidia returns expected structure", {
  result <- interpElections:::.detect_gpu_nvidia()
  expect_type(result, "list")
  expect_named(result, c("found", "name", "vram_mb",
                          "driver_version", "cuda_driver_version"))
  expect_type(result$found, "logical")
})

test_that(".detect_gpu_nvidia found flag is consistent with fields", {
  result <- interpElections:::.detect_gpu_nvidia()
  if (result$found) {
    expect_false(is.na(result$name))
    expect_true(is.numeric(result$vram_mb))
    expect_false(is.na(result$driver_version))
  }
})


# --- .detect_gpu_apple() -----------------------------------------------------

test_that(".detect_gpu_apple returns expected structure", {
  result <- interpElections:::.detect_gpu_apple()
  expect_type(result, "list")
  expect_named(result, c("found", "chip"))
  expect_type(result$found, "logical")
})

test_that(".detect_gpu_apple returns found=FALSE on non-macOS", {
  skip_on_os("mac")
  result <- interpElections:::.detect_gpu_apple()
  expect_false(result$found)
})


# --- .detect_gpu_hardware() ---------------------------------------------------

test_that(".detect_gpu_hardware returns expected structure", {
  result <- interpElections:::.detect_gpu_hardware()
  expect_type(result, "list")
  expect_true("type" %in% names(result))
  expect_true("nvidia" %in% names(result))
  expect_true("apple" %in% names(result))
  expect_true(result$type %in% c("nvidia", "apple_silicon", "none"))
})

test_that(".detect_gpu_hardware type is consistent with sub-results", {
  result <- interpElections:::.detect_gpu_hardware()
  if (result$type == "nvidia") {
    expect_true(result$nvidia$found)
  } else if (result$type == "apple_silicon") {
    expect_true(result$apple$found)
  } else {
    expect_false(result$nvidia$found)
    expect_false(result$apple$found)
  }
})


# --- .check_torch_install() ---------------------------------------------------

test_that(".check_torch_install returns expected structure", {
  result <- interpElections:::.check_torch_install()
  expect_type(result, "list")
  expect_named(result, c("pkg_installed", "pkg_version",
                          "binaries_installed", "install_path"))
  expect_type(result$pkg_installed, "logical")
  expect_type(result$binaries_installed, "logical")
})

test_that(".check_torch_install version consistent with pkg_installed", {
  result <- interpElections:::.check_torch_install()
  if (result$pkg_installed) {
    expect_false(is.na(result$pkg_version))
  } else {
    expect_true(is.na(result$pkg_version))
  }
})


# --- .check_cuda_runtime() ---------------------------------------------------

test_that(".check_cuda_runtime returns expected structure", {
  result <- interpElections:::.check_cuda_runtime()
  expect_type(result, "list")
  expect_true("cuda_available" %in% names(result))
  expect_true("cuda_device_count" %in% names(result))
  expect_true("tensor_test_passed" %in% names(result))
  expect_type(result$cuda_available, "logical")
  expect_type(result$tensor_test_passed, "logical")
})


# --- .check_mps_runtime() ----------------------------------------------------

test_that(".check_mps_runtime returns expected structure", {
  result <- interpElections:::.check_mps_runtime()
  expect_type(result, "list")
  expect_named(result, c("mps_available", "tensor_test_passed"))
  expect_type(result$mps_available, "logical")
  expect_type(result$tensor_test_passed, "logical")
})


# --- check_torch() -----------------------------------------------------------

test_that("check_torch returns expected structure", {
  result <- check_torch(verbose = FALSE)
  expect_type(result, "list")
  expect_named(result, c(
    "torch_installed", "torch_version", "binaries_installed",
    "gpu_hardware", "gpu_name", "gpu_vram",
    "cuda_available", "cuda_runtime", "cuda_compute",
    "cudnn_available", "mps_available", "tensor_test",
    "device", "ready"
  ))
  expect_type(result$torch_installed, "logical")
  expect_type(result$cuda_available, "logical")
  expect_type(result$mps_available, "logical")
  expect_type(result$tensor_test, "logical")
  expect_type(result$ready, "logical")
  expect_true(result$device %in% c("cuda", "mps", "cpu"))
  expect_true(result$gpu_hardware %in% c("nvidia", "apple_silicon", "none"))
})

test_that("check_torch ready flag is consistent with sub-checks", {
  result <- check_torch(verbose = FALSE)
  if (result$ready) {
    expect_true(result$torch_installed)
    expect_true(result$binaries_installed)
    expect_true(result$tensor_test)
  }
})

test_that("check_torch verbose=TRUE produces messages", {
  expect_message(check_torch(verbose = TRUE), "torch")
})

test_that("check_torch verbose=FALSE is silent", {
  expect_silent(check_torch(verbose = FALSE))
})


# --- setup_torch() -----------------------------------------------------------

test_that("setup_torch skips install when torch is already present", {
  skip_if_not_installed("torch")
  # Should not attempt to download anything if torch + binaries exist
  result <- setup_torch(reinstall = FALSE, verbose = FALSE)
  expect_type(result, "list")
  expect_true("ready" %in% names(result))
})
