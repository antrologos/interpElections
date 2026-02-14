# --- Cache infrastructure tests ---

test_that("get_interpElections_cache_dir returns a valid path", {
  dir <- get_interpElections_cache_dir()
  expect_type(dir, "character")
  expect_true(nzchar(dir))
})

test_that("set_interpElections_cache_dir sets and reads custom dir", {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Also clean up config file
  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")
  had_config <- file.exists(config_file)
  old_config <- if (had_config) readLines(config_file, n = 1L) else NULL
  on.exit({
    if (!is.null(old_config)) {
      writeLines(old_config, config_file)
    } else if (file.exists(config_file)) {
      unlink(config_file)
    }
  }, add = TRUE)

  set_interpElections_cache_dir(tmp, verbose = FALSE)
  expect_equal(normalizePath(get_interpElections_cache_dir()),
               normalizePath(tmp))

  # Reset
  set_interpElections_cache_dir(NULL, verbose = FALSE)
  default <- tools::R_user_dir("interpElections", which = "cache")
  expect_equal(get_interpElections_cache_dir(), default)
})

test_that("interpElections_cache lists files in cache dir", {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(file.path(tmp, "tse"), recursive = TRUE)
  writeLines("test", file.path(tmp, "tse", "testfile.zip"))

  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")
  had_config <- file.exists(config_file)
  old_config <- if (had_config) readLines(config_file, n = 1L) else NULL
  on.exit({
    if (!is.null(old_config)) {
      writeLines(old_config, config_file)
    } else if (file.exists(config_file)) {
      unlink(config_file)
    }
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  set_interpElections_cache_dir(tmp, verbose = FALSE)

  files <- interpElections_cache(list_files = TRUE, verbose = FALSE)
  expect_length(files, 1)
  expect_true(grepl("testfile\\.zip$", files[1]))
})

test_that("interpElections_cache deletes by pattern", {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(file.path(tmp, "tse"), recursive = TRUE)
  writeLines("test1", file.path(tmp, "tse", "votes_2020.zip"))
  writeLines("test2", file.path(tmp, "tse", "turnout_2020.zip"))

  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")
  had_config <- file.exists(config_file)
  old_config <- if (had_config) readLines(config_file, n = 1L) else NULL
  on.exit({
    if (!is.null(old_config)) {
      writeLines(old_config, config_file)
    } else if (file.exists(config_file)) {
      unlink(config_file)
    }
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  set_interpElections_cache_dir(tmp, verbose = FALSE)

  interpElections_cache(delete_file = "votes", verbose = FALSE)
  remaining <- list.files(tmp, recursive = TRUE)
  expect_length(remaining, 1)
  expect_true(grepl("turnout", remaining))
})

test_that("interpElections_cache delete_file='all' clears everything", {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(file.path(tmp, "tse"), recursive = TRUE)
  writeLines("test", file.path(tmp, "tse", "file.zip"))

  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")
  had_config <- file.exists(config_file)
  old_config <- if (had_config) readLines(config_file, n = 1L) else NULL
  on.exit({
    if (!is.null(old_config)) {
      writeLines(old_config, config_file)
    } else if (file.exists(config_file)) {
      unlink(config_file)
    }
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  set_interpElections_cache_dir(tmp, verbose = FALSE)

  interpElections_cache(delete_file = "all", verbose = FALSE)
  # Directory is recreated empty after deletion
  expect_true(dir.exists(tmp))
  expect_length(list.files(tmp, recursive = TRUE), 0)
})

test_that(".cache_message produces expected messages", {
  expect_message(
    interpElections:::.cache_message(TRUE, TRUE, TRUE, "test.zip"),
    "Reading cached file"
  )
  expect_message(
    interpElections:::.cache_message(TRUE, FALSE, TRUE, "test.zip"),
    "existing temporary file"
  )
  expect_message(
    interpElections:::.cache_message(FALSE, TRUE, TRUE, "test.zip"),
    "Downloading and caching"
  )
  expect_message(
    interpElections:::.cache_message(FALSE, FALSE, TRUE, "test.zip"),
    "not caching"
  )
})

test_that(".save_to_cache and .load_from_cache round-trip", {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(tmp)

  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")
  had_config <- file.exists(config_file)
  old_config <- if (had_config) readLines(config_file, n = 1L) else NULL
  on.exit({
    if (!is.null(old_config)) {
      writeLines(old_config, config_file)
    } else if (file.exists(config_file)) {
      unlink(config_file)
    }
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  set_interpElections_cache_dir(tmp, verbose = FALSE)

  obj <- matrix(1:12, 3, 4)
  interpElections:::.save_to_cache(obj, "test_matrix.rds", "test")
  loaded <- interpElections:::.load_from_cache("test_matrix.rds", "test")
  expect_equal(loaded, obj)

  # Missing file returns NULL
  missing <- interpElections:::.load_from_cache("nonexistent.rds", "test")
  expect_null(missing)
})

test_that(".digest_simple produces consistent hashes", {
  h1 <- interpElections:::.digest_simple("hello world")
  h2 <- interpElections:::.digest_simple("hello world")
  h3 <- interpElections:::.digest_simple("different string")
  expect_equal(h1, h2)
  expect_false(h1 == h3)
  expect_type(h1, "character")
  expect_true(nchar(h1) == 32)  # MD5 produces 32-char hex string
})
