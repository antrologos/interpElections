# --- Cache subdirectory structure ---

#' @noRd
.cache_subdirs <- function() {
  list(
    votes        = file.path("downloads", "votes"),
    turnout      = file.path("downloads", "turnout"),
    geocode      = file.path("downloads", "geocode"),
    profile      = file.path("downloads", "profile"),
    hidalgo      = file.path("downloads", "hidalgo"),
    legends      = file.path("downloads", "legends"),
    osm          = file.path("downloads", "osm"),
    electoral    = file.path("processed", "electoral"),
    tracts       = file.path("processed", "tracts"),
    r5r          = file.path("networks", "r5r"),
    travel_times = "travel_times",
    bin          = "bin"
  )
}


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
#' Lists or deletes cached files. By default shows a per-category size
#' breakdown. Use `details = TRUE` to see individual files.
#' Use `delete_file = "all"` to clear the entire cache.
#'
#' @param list_files Logical. If TRUE (default), prints a per-category
#'   summary and returns cached file paths.
#' @param delete_file Character or NULL. A pattern to match files for
#'   deletion (matched against relative paths via `grepl()`), or
#'   `"all"` to delete the entire cache. For targeted deletion by
#'   category, see [interpElections_cache_clean()].
#' @param details Logical. If TRUE, also prints individual filenames
#'   within each category. Default: FALSE.
#' @param verbose Logical. Print messages. Default: TRUE.
#'
#' @return Invisibly returns a character vector of cached file paths
#'   (before any deletion).
#'
#' @examples
#' \dontrun{
#' # Per-category summary
#' interpElections_cache()
#'
#' # Detailed listing (every file)
#' interpElections_cache(details = TRUE)
#'
#' # Delete files matching a pattern
#' interpElections_cache(delete_file = "2020")
#'
#' # Delete everything
#' interpElections_cache(delete_file = "all")
#' }
#'
#' @family cache
#' @seealso [get_interpElections_cache_dir()],
#'   [set_interpElections_cache_dir()],
#'   [interpElections_cache_clean()]
#' @export
interpElections_cache <- function(
    list_files  = TRUE,
    delete_file = NULL,
    details     = FALSE,
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
      sizes <- file.size(files)
      total_mb <- sum(sizes, na.rm = TRUE) / 1e6

      message(sprintf("interpElections cache (%s):", cache_dir))
      message(sprintf("  Total: %.1f MB (%d files)\n",
                       total_mb, length(files)))

      # Group by top 2 directory levels
      dir_parts <- strsplit(rel, "/")
      category <- vapply(dir_parts, function(p) {
        paste(utils::head(p, min(length(p) - 1L, 2L)), collapse = "/")
      }, character(1))
      # Files at root level
      category[category == ""] <- "."

      for (cat in sort(unique(category))) {
        mask <- category == cat
        cat_mb <- sum(sizes[mask], na.rm = TRUE) / 1e6
        cat_n <- sum(mask)
        label <- if (cat == ".") "(root)" else paste0(cat, "/")
        message(sprintf("  %-32s %8.1f MB  (%d file%s)",
                        label, cat_mb, cat_n,
                        if (cat_n == 1) "" else "s"))
        if (details) {
          for (f in rel[mask]) {
            f_mb <- sizes[which(rel == f)[1]] / 1e6
            message(sprintf("    %-50s %8.1f MB",
                            basename(f), f_mb))
          }
        }
      }

    }
  }

  if (!is.null(delete_file)) {
    if (delete_file == "all") {
      unlink(cache_dir, recursive = TRUE)
      dir.create(cache_dir, recursive = TRUE)
      if (verbose) message("Deleted entire cache directory: ", cache_dir)
    } else {
      # Match against relative path (not just basename)
      cache_prefix <- normalizePath(cache_dir, winslash = "/")
      file_paths <- normalizePath(files, winslash = "/")
      rel <- substring(file_paths, nchar(cache_prefix) + 2L)
      to_delete <- files[grepl(delete_file, rel, fixed = TRUE)]
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


#' Delete cached files by category
#'
#' Convenience function for clearing specific categories of cached data.
#' More discoverable than [interpElections_cache()] with `delete_file`.
#'
#' @param category Character. Which category to clear. One of:
#'   \describe{
#'     \item{`"all"`}{Delete the entire cache}
#'     \item{`"downloads"`}{All raw downloads (votes, turnout, geocode, profile, hidalgo, osm)}
#'     \item{`"processed"`}{All processed/cached results (electoral, tracts)}
#'     \item{`"networks"`}{All r5r network indices}
#'     \item{`"travel_times"`}{Cached travel time matrices}
#'     \item{`"votes"`}{TSE vote data ZIPs}
#'     \item{`"turnout"`}{TSE turnout data ZIPs}
#'     \item{`"geocode"`}{TSE polling station location ZIPs}
#'     \item{`"profile"`}{TSE voter profile ZIPs}
#'     \item{`"hidalgo"`}{Hidalgo geocoding fallback data}
#'     \item{`"osm"`}{OpenStreetMap road network extracts}
#'     \item{`"electoral"`}{Processed electoral data (br_prepare_electoral output)}
#'     \item{`"tracts"`}{Cached census tract geometries}
#'     \item{`"r5r"`}{r5r routing network indices}
#'   }
#' @param verbose Logical. Print messages. Default: TRUE.
#'
#' @return Invisibly returns the path(s) that were deleted.
#'
#' @examples
#' \dontrun{
#' # Clear only processed electoral data (forces re-computation next run)
#' interpElections_cache_clean("electoral")
#'
#' # Clear all raw downloads
#' interpElections_cache_clean("downloads")
#'
#' # Clear everything
#' interpElections_cache_clean("all")
#' }
#'
#' @family cache
#' @seealso [interpElections_cache()]
#' @export
interpElections_cache_clean <- function(
    category = c("all", "downloads", "processed", "networks",
                 "travel_times", "votes", "turnout", "geocode",
                 "profile", "hidalgo", "osm", "electoral",
                 "tracts", "r5r"),
    verbose = TRUE
) {
  category <- match.arg(category)
  cache_dir <- get_interpElections_cache_dir()

  if (!dir.exists(cache_dir)) {
    if (verbose) message("Cache directory does not exist yet.")
    return(invisible(character(0)))
  }

  if (category == "all") {
    unlink(cache_dir, recursive = TRUE)
    dir.create(cache_dir, recursive = TRUE)
    if (verbose) message("Deleted entire cache directory: ", cache_dir)
    return(invisible(cache_dir))
  }

  # Map category to subdirectory path(s)
  subdirs <- .cache_subdirs()
  target_dirs <- switch(category,
    downloads    = file.path(cache_dir, "downloads"),
    processed    = file.path(cache_dir, "processed"),
    networks     = file.path(cache_dir, "networks"),
    travel_times = file.path(cache_dir, subdirs$travel_times),
    votes        = file.path(cache_dir, subdirs$votes),
    turnout      = file.path(cache_dir, subdirs$turnout),
    geocode      = file.path(cache_dir, subdirs$geocode),
    profile      = file.path(cache_dir, subdirs$profile),
    hidalgo      = file.path(cache_dir, subdirs$hidalgo),
    osm          = file.path(cache_dir, subdirs$osm),
    electoral    = file.path(cache_dir, subdirs$electoral),
    tracts       = file.path(cache_dir, subdirs$tracts),
    r5r          = file.path(cache_dir, subdirs$r5r)
  )

  deleted <- character(0)
  for (td in target_dirs) {
    if (dir.exists(td)) {
      n_files <- length(list.files(td, recursive = TRUE))
      size_mb <- sum(file.size(
        list.files(td, recursive = TRUE, full.names = TRUE)
      ), na.rm = TRUE) / 1e6
      unlink(td, recursive = TRUE)
      deleted <- c(deleted, td)
      if (verbose) {
        message(sprintf("Deleted %s/ (%.1f MB, %d files)",
                        category, size_mb, n_files))
      }
    } else {
      if (verbose) message(sprintf("No cached data for category '%s'", category))
    }
  }

  invisible(deleted)
}


# --- Internal download helper ---

#' @noRd
.interpElections_download <- function(
    url,
    filename,
    subdir  = .cache_subdirs()$votes,
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
      curl::curl_download(url, local_file, handle = h, quiet = TRUE)
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
    message(sprintf("    Downloaded %s (%.1f MB)", filename, size_mb))
  }

  local_file
}


#' @noRd
.cache_message <- function(file_exists, cache, verbose, filename = "") {
  if (!verbose) return(invisible(NULL))

  if (file_exists) {
    message(sprintf("    Cached: %s", filename))
  } else {
    message(sprintf("    Downloading: %s...", filename))
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
