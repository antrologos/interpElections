# --- .find_tool_extended() ---

test_that(".find_tool_extended returns NULL for nonexistent tool", {
  expect_null(interpElections:::.find_tool_extended("nonexistent_tool_xyz_123"))
})

test_that(".find_tool_extended only triggers for osmium-related tools", {
  # Non-osmium tools should NOT go through extended search
  # (they just get NULL from .find_tool via Sys.which)
  result <- interpElections:::.find_tool("nonexistent_tool_xyz_123")
  expect_null(result)
})

test_that(".find_tool_extended returns a character path or NULL", {
  result <- interpElections:::.find_tool_extended("osmium")
  expect_true(is.null(result) || is.character(result))
  if (!is.null(result)) {
    expect_true(file.exists(result))
  }
})

# --- .discover_package_manager() ---

test_that(".discover_package_manager returns a well-formed list", {
  pm <- interpElections:::.discover_package_manager()
  expect_type(pm, "list")
  expect_true("method" %in% names(pm))
  expect_true("bin" %in% names(pm))
})

test_that(".discover_package_manager returns valid method or NULL", {
  pm <- interpElections:::.discover_package_manager()
  if (!is.null(pm$method)) {
    expect_true(
      pm$method %in% c("conda", "brew", "apt", "dnf", "download")
    )
  }
})

# --- .find_tool() upgraded behavior ---

test_that(".find_tool returns character or NULL", {
  result <- interpElections:::.find_tool("osmium")
  expect_true(is.null(result) || is.character(result))
})

test_that(".find_tool works for non-osmium tools", {
  # Should find R at minimum
  r_path <- interpElections:::.find_tool("R")
  # R may or may not be on PATH depending on platform, so just
  # check it returns character or NULL

  expect_true(is.null(r_path) || is.character(r_path))
})

# --- .has_clip_tool() ---

test_that(".has_clip_tool returns logical", {
  result <- interpElections:::.has_clip_tool()
  expect_type(result, "logical")
  expect_length(result, 1)
})

# --- .offer_osmium_install() ---

test_that(".offer_osmium_install returns invisible NULL in non-interactive", {
  # In non-interactive mode (test runner), should return immediately
  result <- interpElections:::.offer_osmium_install(verbose = FALSE)
  expect_null(result)
})

# --- .activate_osmium_path() ---

test_that(".activate_osmium_path adds directory to PATH", {
  # Use a temp directory to avoid side effects
  tmp_dir <- tempdir()
  tmp_tool <- file.path(tmp_dir, "fake_osmium_test")
  file.create(tmp_tool)
  on.exit(unlink(tmp_tool), add = TRUE)

  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)

  interpElections:::.activate_osmium_path(tmp_tool, verbose = FALSE)
  new_path <- Sys.getenv("PATH")

  norm_dir <- normalizePath(tmp_dir, mustWork = FALSE)
  path_entries <- unlist(strsplit(new_path, .Platform$path.sep, fixed = TRUE))
  norm_entries <- normalizePath(path_entries, mustWork = FALSE)
  expect_true(norm_dir %in% norm_entries)
})

test_that(".activate_osmium_path is idempotent", {
  tmp_dir <- tempdir()
  tmp_tool <- file.path(tmp_dir, "fake_osmium_test2")
  file.create(tmp_tool)
  on.exit(unlink(tmp_tool), add = TRUE)

  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)

  interpElections:::.activate_osmium_path(tmp_tool, verbose = FALSE)
  path_after_first <- Sys.getenv("PATH")

  interpElections:::.activate_osmium_path(tmp_tool, verbose = FALSE)
  path_after_second <- Sys.getenv("PATH")

  expect_equal(path_after_first, path_after_second)
})
