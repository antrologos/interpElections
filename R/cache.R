# --- Cache directory management ---

#' Get the interpElections cache directory
#'
#' Returns the path to the directory where interpElections stores cached
#' downloaded files (TSE data, Hidalgo geocoding, travel time matrices, etc.).
#' The default location is OS-appropriate (via [tools::R_user_dir()]).
#' A custom path can be set with [set_interpElections_cache_dir()].
#'
#' @return Character. Path to the cache directory.
#'
#' @family cache
#' @seealso [set_interpElections_cache_dir()], [interpElections_cache()]
#' @export
get_interpElections_cache_dir <- function() {
  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")

  if (file.exists(config_file)) {
    custom <- trimws(readLines(config_file, n = 1L, warn = FALSE))
    if (nzchar(custom) && dir.exists(custom)) return(custom)
  }

  tools::R_user_dir("interpElections", which = "cache")
}


#' Set a custom interpElections cache directory
#'
#' Persists a custom cache directory path across R sessions. When `path`
#' is `NULL`, resets to the default OS-appropriate location.
#'
#' @param path Character or NULL. Directory path for cached files.
#'   Created if it does not exist. `NULL` resets to the default.
#' @param verbose Logical. Print confirmation. Default: TRUE.
#'
#' @return Invisibly returns the cache directory path.
#'
#' @family cache
#' @seealso [get_interpElections_cache_dir()], [interpElections_cache()]
#' @export
set_interpElections_cache_dir <- function(path = NULL, verbose = TRUE) {
  config_dir <- tools::R_user_dir("interpElections", which = "config")
  config_file <- file.path(config_dir, "cache_dir")

  if (is.null(path)) {
    # Reset to default
    if (file.exists(config_file)) unlink(config_file)
    cache_dir <- tools::R_user_dir("interpElections", which = "cache")
    if (verbose) message("Cache directory reset to default: ", cache_dir)
    return(invisible(cache_dir))
  }

  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) {
    ok <- dir.create(path, recursive = TRUE)
    if (!ok) {
      stop("Failed to create cache directory: ", path, call. = FALSE)
    }
  }

  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  writeLines(path, config_file)

  if (verbose) message("Cache directory set to: ", path)
  invisible(path)
}


#' Manage the interpElections download cache
#'
#' Lists or deletes cached files. Use `delete_file = "all"` to clear
#' the entire cache.
#'
#' @param list_files Logical. If TRUE (default), prints and returns
#'   cached file paths.
#' @param delete_file Character or NULL. A pattern to match files for
#'   deletion (uses `grepl()`), or `"all"` to delete the entire cache.
#' @param verbose Logical. Print messages. Default: TRUE.
#'
#' @return Invisibly returns a character vector of cached file paths
#'   (before any deletion).
#'
#' @examples
#' \dontrun{
#' # List all cached files
#' interpElections_cache()
#'
#' # Delete all TSE files
#' interpElections_cache(delete_file = "tse")
#'
#' # Delete everything
#' interpElections_cache(delete_file = "all")
#' }
#'
#' @family cache
#' @seealso [get_interpElections_cache_dir()], [set_interpElections_cache_dir()]
#' @export
interpElections_cache <- function(
    list_files  = TRUE,
    delete_file = NULL,
    verbose     = TRUE
) {
  cache_dir <- get_interpElections_cache_dir()

  if (!dir.exists(cache_dir)) {
    if (verbose) message("Cache directory does not exist yet: ", cache_dir)
    return(invisible(character(0)))
  }

  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)

  if (list_files && verbose) {
    if (length(files) == 0) {
      message("Cache is empty.")
    } else {
      cache_prefix <- normalizePath(cache_dir, winslash = "/")
      file_paths <- normalizePath(files, winslash = "/")
      rel <- substring(file_paths, nchar(cache_prefix) + 2L)
      total_mb <- sum(file.size(files), na.rm = TRUE) / 1e6
      message(sprintf("interpElections cache (%s):", cache_dir))
      message(sprintf("  Total size: %.1f MB (%d files)", total_mb, length(files)))
      for (f in rel) message("  ", f)
    }
  }

  if (!is.null(delete_file)) {
    if (delete_file == "all") {
      unlink(cache_dir, recursive = TRUE)
      dir.create(cache_dir, recursive = TRUE)
      if (verbose) message("Deleted entire cache directory: ", cache_dir)
    } else {
      to_delete <- files[grepl(delete_file, basename(files), fixed = TRUE)]
      if (length(to_delete) == 0) {
        if (verbose) message("No cached files matched pattern: ", delete_file)
      } else {
        unlink(to_delete)
        if (verbose) {
          message(sprintf("Deleted %d file(s) matching '%s'",
                          length(to_delete), delete_file))
        }
      }
    }
  }

  invisible(files)
}


# --- Internal download helper ---

#' @noRd
.interpElections_download <- function(
    url,
    filename,
    subdir  = "tse",
    cache   = TRUE,
    force   = FALSE,
    verbose = TRUE
) {
  if (isTRUE(cache)) {
    dest_dir <- file.path(get_interpElections_cache_dir(), subdir)
  } else {
    dest_dir <- tempdir()
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  local_file <- file.path(dest_dir, filename)

  # Cache hit
  if (file.exists(local_file) && !isTRUE(force)) {
    .cache_message(file_exists = TRUE, cache = cache, verbose = verbose,
                   filename = filename)
    return(local_file)
  }

  # Cache miss — download
  .cache_message(file_exists = FALSE, cache = cache, verbose = verbose,
                 filename = filename)

  timeout <- getOption("interpElections.timeout", default = 300L)
  max_retries <- getOption("interpElections.retries", default = 3L)

  success <- FALSE
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 30L, timeout = timeout)
      # Delete partial file before retrying to avoid resume corruption
      if (file.exists(local_file) && attempt > 1L) {
        unlink(local_file)
      }
      curl::curl_download(url, local_file, handle = h, quiet = !verbose)
      TRUE
    }, error = function(e) {
      if (verbose) {
        message(sprintf("  Download attempt %d/%d failed: %s",
                        attempt, max_retries, conditionMessage(e)))
      }
      FALSE
    })

    if (isTRUE(result)) {
      # Validate file
      if (file.exists(local_file) && file.size(local_file) > 5000) {
        success <- TRUE
        break
      } else {
        if (verbose) {
          message(sprintf(
            "  Downloaded file is too small (%.0f bytes), likely corrupted. Retrying...",
            if (file.exists(local_file)) file.size(local_file) else 0
          ))
        }
        if (file.exists(local_file)) unlink(local_file)
      }
    }

    if (attempt < max_retries) {
      wait <- c(5, 15, 30)[min(attempt, 3)]
      if (verbose) message(sprintf("  Waiting %d seconds before retry...", wait))
      Sys.sleep(wait)
    }
  }

  if (!success) {
    if (file.exists(local_file)) unlink(local_file)
    stop(sprintf(
      paste0(
        "Failed to download after %d attempts.\n",
        "URL: %s\n",
        "You can try:\n",
        "  - Increase timeout: options(interpElections.timeout = 600)\n",
        "  - Check your internet connection\n",
        "  - Download manually and use the file path parameter"
      ),
      max_retries, url
    ), call. = FALSE)
  }

  if (verbose) {
    size_mb <- file.size(local_file) / 1e6
    message(sprintf("  Downloaded %.1f MB -> %s", size_mb, local_file))
  }

  local_file
}


#' @noRd
.cache_message <- function(file_exists, cache, verbose, filename = "") {
  if (!verbose) return(invisible(NULL))

  if (file_exists && isTRUE(cache)) {
    message(sprintf("  Reading cached file: %s", filename))
  } else if (file_exists && !isTRUE(cache)) {
    message(sprintf("  Using existing temporary file: %s", filename))
  } else if (!file_exists && isTRUE(cache)) {
    message(sprintf("  Downloading and caching locally: %s", filename))
  } else {
    message(sprintf(
      "  Downloading (not caching): %s. Set cache=TRUE for faster future use.",
      filename
    ))
  }
}


# --- Computed artifact caching ---

#' @noRd
.save_to_cache <- function(obj, filename, subdir = "computed") {
  cache_dir <- file.path(get_interpElections_cache_dir(), subdir)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  dest <- file.path(cache_dir, filename)
  # Atomic write: save to temp file first, then rename.
  # Use tempfile() in the same directory for cross-platform safety.
  tmp_file <- tempfile(tmpdir = cache_dir, fileext = ".rds.tmp")
  saveRDS(obj, tmp_file)
  renamed <- file.rename(tmp_file, dest)
  if (!renamed) {
    # file.rename() can fail on Windows if dest is locked
    unlink(tmp_file)
    warning("Failed to save cache file: ", dest,
            " (file may be locked by another process)", call. = FALSE)
  }
}

#' @noRd
.load_from_cache <- function(filename, subdir = "computed") {
  path <- file.path(get_interpElections_cache_dir(), subdir, filename)
  if (file.exists(path)) readRDS(path) else NULL
}
