# --- Cache infrastructure tests ---

# Helper: set up a temporary cache dir and restore original on exit
.with_temp_cache <- function(env = parent.frame()) {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(tmp)

  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")
  had_config <- file.exists(config_file)
  old_config <- if (had_config) readLines(config_file, n = 1L) else NULL

  set_interpElections_cache_dir(tmp, verbose = FALSE)

  withr::defer({
    if (!is.null(old_config)) {
      writeLines(old_config, config_file)
    } else if (file.exists(config_file)) {
      unlink(config_file)
    }
    unlink(tmp, recursive = TRUE)
  }, envir = env)

  tmp
}


test_that("get_interpElections_cache_dir returns a valid path", {
  dir <- get_interpElections_cache_dir()
  expect_type(dir, "character")
  expect_true(nzchar(dir))
})

test_that("set_interpElections_cache_dir sets and reads custom dir", {
  tmp <- tempfile("interpElections_test_cache_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

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


# --- .cache_subdirs() ---

test_that(".cache_subdirs returns expected structure", {
  subdirs <- interpElections:::.cache_subdirs()
  expect_type(subdirs, "list")
  expect_true(all(c("votes", "turnout", "geocode", "profile",
                     "hidalgo", "osm", "electoral", "tracts",
                     "r5r", "travel_times", "bin") %in% names(subdirs)))
  # Votes should be under downloads/
  expect_equal(subdirs$votes, file.path("downloads", "votes"))
  expect_equal(subdirs$electoral, file.path("processed", "electoral"))
  expect_equal(subdirs$r5r, file.path("networks", "r5r"))
})


# --- interpElections_cache() ---

test_that("interpElections_cache lists files using new subdirs", {
  tmp <- .with_temp_cache()

  # Create file in new subdir structure
  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("test", file.path(votes_dir, "testfile.zip"))

  files <- interpElections_cache(list_files = TRUE, verbose = FALSE)
  expect_length(files, 1)
  expect_true(grepl("testfile\\.zip$", files[1]))
})

test_that("interpElections_cache shows per-category breakdown", {
  tmp <- .with_temp_cache()

  # Create files in multiple categories
  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("vote data", file.path(votes_dir, "votes_2020.zip"))

  tracts_dir <- file.path(tmp, "processed", "tracts")
  dir.create(tracts_dir, recursive = TRUE)
  writeLines("tract data", file.path(tracts_dir, "tracts_001.rds"))

  out <- capture.output(
    interpElections_cache(list_files = TRUE, verbose = TRUE),
    type = "message"
  )
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("downloads/votes", combined))
  expect_true(grepl("processed/tracts", combined))
})

test_that("interpElections_cache details=TRUE shows individual files", {
  tmp <- .with_temp_cache()

  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("data", file.path(votes_dir, "votes_2020_SP.zip"))

  out <- capture.output(
    interpElections_cache(details = TRUE, verbose = TRUE),
    type = "message"
  )
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("votes_2020_SP\\.zip", combined))
})

test_that("interpElections_cache deletes by pattern (relative path match)", {
  tmp <- .with_temp_cache()

  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("test1", file.path(votes_dir, "votes_2020.zip"))

  turnout_dir <- file.path(tmp, "downloads", "turnout")
  dir.create(turnout_dir, recursive = TRUE)
  writeLines("test2", file.path(turnout_dir, "turnout_2020.zip"))

  # Pattern "votes" matches the relative path downloads/votes/...
  interpElections_cache(delete_file = "votes", verbose = FALSE)
  remaining <- list.files(tmp, recursive = TRUE)
  expect_length(remaining, 1)
  expect_true(grepl("turnout", remaining))
})

test_that("interpElections_cache delete_file='all' clears everything", {
  tmp <- .with_temp_cache()

  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("test", file.path(votes_dir, "file.zip"))

  interpElections_cache(delete_file = "all", verbose = FALSE)
  expect_true(dir.exists(tmp))
  expect_length(list.files(tmp, recursive = TRUE), 0)
})


# --- interpElections_cache_clean() ---

test_that("interpElections_cache_clean deletes by category", {
  tmp <- .with_temp_cache()

  # Create files in two categories
  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("v", file.path(votes_dir, "votes.zip"))

  tracts_dir <- file.path(tmp, "processed", "tracts")
  dir.create(tracts_dir, recursive = TRUE)
  writeLines("t", file.path(tracts_dir, "tracts.rds"))

  # Clean only votes
  interpElections_cache_clean("votes", verbose = FALSE)
  expect_false(dir.exists(votes_dir))
  expect_true(file.exists(file.path(tracts_dir, "tracts.rds")))
})

test_that("interpElections_cache_clean 'downloads' clears all download subdirs", {
  tmp <- .with_temp_cache()

  for (sub in c("votes", "turnout", "geocode")) {
    d <- file.path(tmp, "downloads", sub)
    dir.create(d, recursive = TRUE)
    writeLines("x", file.path(d, paste0(sub, ".zip")))
  }
  # Also create processed (should survive)
  pd <- file.path(tmp, "processed", "electoral")
  dir.create(pd, recursive = TRUE)
  writeLines("y", file.path(pd, "data.rds"))

  interpElections_cache_clean("downloads", verbose = FALSE)
  expect_false(dir.exists(file.path(tmp, "downloads")))
  expect_true(file.exists(file.path(pd, "data.rds")))
})

test_that("interpElections_cache_clean 'all' clears everything", {
  tmp <- .with_temp_cache()

  votes_dir <- file.path(tmp, "downloads", "votes")
  dir.create(votes_dir, recursive = TRUE)
  writeLines("x", file.path(votes_dir, "test.zip"))

  interpElections_cache_clean("all", verbose = FALSE)
  expect_true(dir.exists(tmp))
  expect_length(list.files(tmp, recursive = TRUE), 0)
})

test_that("interpElections_cache_clean on empty category is silent", {
  tmp <- .with_temp_cache()

  # No files exist — should not error
  out <- capture.output(
    interpElections_cache_clean("votes", verbose = TRUE),
    type = "message"
  )
  expect_true(any(grepl("No cached data", out)))
})


# --- .cache_message ---

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


# --- .save_to_cache / .load_from_cache ---

test_that(".save_to_cache and .load_from_cache round-trip", {
  tmp <- .with_temp_cache()

  obj <- matrix(1:12, 3, 4)
  interpElections:::.save_to_cache(obj, "test_matrix.rds", "test")
  loaded <- interpElections:::.load_from_cache("test_matrix.rds", "test")
  expect_equal(loaded, obj)

  # Missing file returns NULL
  missing <- interpElections:::.load_from_cache("nonexistent.rds", "test")
  expect_null(missing)
})

test_that(".save_to_cache works with new subdirectory paths", {
  tmp <- .with_temp_cache()

  obj <- list(a = 1, b = "hello")
  subdirs <- interpElections:::.cache_subdirs()

  interpElections:::.save_to_cache(obj, "test.rds", subdirs$electoral)
  loaded <- interpElections:::.load_from_cache("test.rds", subdirs$electoral)
  expect_equal(loaded, obj)

  # File should be under processed/electoral/
  expected_path <- file.path(tmp, "processed", "electoral", "test.rds")
  expect_true(file.exists(expected_path))
})


# --- .electoral_cache_key ---

test_that(".electoral_cache_key produces consistent keys", {
  key1 <- interpElections:::.electoral_cache_key(
    "3550308", 2020, 13L, 1L, "candidates", NULL, NULL
  )
  key2 <- interpElections:::.electoral_cache_key(
    "3550308", 2020, 13L, 1L, "candidates", NULL, NULL
  )
  expect_equal(key1, key2)
  expect_true(grepl("^electoral_3550308_2020_", key1))
  expect_true(grepl("\\.rds$", key1))
})

test_that(".electoral_cache_key differs for different parameters", {
  key_cand <- interpElections:::.electoral_cache_key(
    "3550308", 2020, 13L, 1L, "candidates", NULL, NULL
  )
  key_party <- interpElections:::.electoral_cache_key(
    "3550308", 2020, 13L, 1L, "parties", NULL, NULL
  )
  key_year <- interpElections:::.electoral_cache_key(
    "3550308", 2022, 13L, 1L, "candidates", NULL, NULL
  )
  expect_false(key_cand == key_party)
  expect_false(key_cand == key_year)
})

test_that(".electoral_cache_key is order-insensitive for candidates/parties", {
  key_a <- interpElections:::.electoral_cache_key(
    "3550308", 2020, 13L, 1L, "candidates", c(22, 13), NULL
  )
  key_b <- interpElections:::.electoral_cache_key(
    "3550308", 2020, 13L, 1L, "candidates", c(13, 22), NULL
  )
  expect_equal(key_a, key_b)
})


# --- .digest_simple ---

test_that(".digest_simple produces consistent hashes", {
  h1 <- interpElections:::.digest_simple("hello world")
  h2 <- interpElections:::.digest_simple("hello world")
  h3 <- interpElections:::.digest_simple("different string")
  expect_equal(h1, h2)
  expect_false(h1 == h3)
  expect_type(h1, "character")
  expect_true(nchar(h1) == 32)  # MD5 produces 32-char hex string
})
