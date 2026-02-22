#' Download OSM and elevation data for r5r routing
#'
#' Downloads the OpenStreetMap road network (`.pbf` file) and optionally
#' satellite elevation data needed to build an r5r routing network.
#' The output directory can be passed directly to [compute_travel_times()]
#' as `network_path`.
#'
#' After downloading the provider extract (which may cover a whole state),
#' the `.pbf` file is clipped to the bounding box of `area_sf` using
#' `osmium` or `osmconvert` (if available on the system). This avoids
#' r5r's geographic extent limit (~975,000 km2).
#'
#' @param area_sf An `sf` object defining the area of interest. Its bounding
#'   box is used to select the appropriate OSM extract and to clip the
#'   downloaded file.
#' @param output_dir Character. Directory to save downloaded files. Created
#'   if it does not exist.
#' @param osm Logical. Download OpenStreetMap road network. Default: TRUE.
#' @param elevation Logical. Download elevation raster for more accurate
#'   walking/cycling routing on hilly terrain. Default: FALSE.
#' @param osm_provider Character. OSM extract provider for `osmextract`.
#'   Default: `"geofabrik"`. Alternatives: `"bbbike"`,
#'   `"openstreetmap_fr"`.
#' @param osm_url Character or NULL. Direct URL to the OSM `.pbf` extract.
#'   When provided, skips the automatic `osmextract::oe_match()` step and
#'   downloads from this URL directly. This is useful when `oe_match()`
#'   selects an overly broad extract (e.g., country-level instead of
#'   state-level). Default: NULL (auto-detect via `oe_match()`).
#' @param force Logical. Re-download even if files already exist.
#'   Default: FALSE.
#' @param verbose Logical. Default: TRUE.
#'
#' @return A list with paths to downloaded files:
#' \describe{
#'   \item{osm_pbf}{Character. Path to the OSM `.pbf` file (or NULL if
#'     `osm = FALSE`).}
#'   \item{elevation_tif}{Character. Path to the elevation `.tif` file
#'     (or NULL if `elevation = FALSE`).}
#'   \item{output_dir}{Character. The output directory path.}
#' }
#'
#' @details
#' Requires the `osmextract` package for OSM downloads and optionally
#' the `elevatr` package for elevation data. Both are suggested
#' dependencies of this package.
#'
#' For clipping large OSM extracts, `osmium-tool` (recommended) or
#' `osmconvert` must be installed. If neither is found, the function
#' will interactively offer to install one via [setup_osmium()] before
#' proceeding. In non-interactive mode, it stops with an actionable
#' error message. All dependency checks (R packages and clipping tools)
#' run at the start, before any downloads begin.
#'
#' @family spatial
#'
#' @seealso [compute_travel_times()] to use the downloaded data,
#'   [setup_osmium()] to install the required clipping tool.
#'
#' @export
download_r5r_data <- function(
    area_sf,
    output_dir,
    osm = TRUE,
    elevation = FALSE,
    osm_provider = "geofabrik",
    osm_url = NULL,
    force = FALSE,
    verbose = TRUE
) {
  # --- Check all dependencies upfront ---
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for download_r5r_data()",
         call. = FALSE)
  }
  if (osm && !requireNamespace("osmextract", quietly = TRUE)) {
    stop(
      "The 'osmextract' package is required for OSM downloads.\n",
      "Install with: install.packages('osmextract')",
      call. = FALSE
    )
  }
  if (elevation && !requireNamespace("elevatr", quietly = TRUE)) {
    stop(
      "The 'elevatr' package is required for elevation downloads.\n",
      "Install with: install.packages('elevatr')",
      call. = FALSE
    )
  }
  if (elevation && !requireNamespace("terra", quietly = TRUE)) {
    stop(
      "The 'terra' package is required for elevation downloads.\n",
      "Install with: install.packages('terra')",
      call. = FALSE
    )
  }

  # Check for OSM clipping tools before any downloads.
  # State-level .pbf files for large states (MG, SP, BA, etc.)
  # exceed r5r's geographic extent limit and MUST be clipped.
  if (osm && !.has_clip_tool()) {
    .offer_osmium_install(verbose = verbose)
    if (!.has_clip_tool()) {
      stop(
        "A clipping tool (osmium or osmconvert) is required ",
        "to extract the municipality area from state-level ",
        "OSM files.\n\n",
        "Run interpElections::setup_osmium() to install, ",
        "or install manually:\n",
        "  Windows: conda install -c conda-forge osmium-tool\n",
        "  macOS:   brew install osmium-tool\n",
        "  Linux:   sudo apt install osmium-tool",
        call. = FALSE
      )
    }
  }

  if (!dir.exists(output_dir)) {
    if (verbose) message("  Creating output directory: ", output_dir)
    dir.create(output_dir, recursive = TRUE)
  }

  result <- list(
    osm_pbf = NULL, elevation_tif = NULL,
    output_dir = output_dir
  )

  # --- OSM download ---
  if (osm) {
    if (verbose) message("  Downloading OSM data...")

    # Determine the OSM extract URL: either from explicit osm_url or
    # by auto-matching via osmextract::oe_match()
    if (!is.null(osm_url)) {
      matched <- list(url = osm_url)
      if (verbose) message("  Using explicit OSM URL: ", osm_url)
    } else {
      matched <- tryCatch(
        suppressMessages(osmextract::oe_match(area_sf, provider = osm_provider)),
        error = function(e) {
          stop(
            "Failed to match area to an OSM extract (provider: ",
            osm_provider, ").\n",
            "This requires an internet connection. Error: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }

    # Download the state-level .pbf to the persistent cache so it
    # can be reused across municipalities in the same state
    osm_cache_dir <- file.path(
      get_interpElections_cache_dir(), .cache_subdirs()$osm
    )
    if (!dir.exists(osm_cache_dir)) {
      dir.create(osm_cache_dir, recursive = TRUE)
    }

    osm_path <- tryCatch(
      suppressMessages(osmextract::oe_download(
        file_url = matched$url,
        provider = osm_provider,
        download_directory = osm_cache_dir,
        force_download = force,
        quiet = TRUE
      )),
      error = function(e) {
        stop(
          "Failed to download OSM data from ", osm_provider, ".\n",
          "Check your internet connection. Error: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    if (verbose) {
      osm_mb <- file.size(osm_path) / 1e6
      message(sprintf("  OSM extract: %s (%.0f MB)",
                      basename(osm_path), osm_mb))
    }

    # Clip the .pbf to the area_sf bbox so r5r doesn't exceed
    # its geographic extent limit (~975,000 km2)
    clipped <- .clip_pbf(osm_path, area_sf, output_dir,
                         force = force, verbose = verbose)
    if (!is.null(clipped)) {
      osm_path <- clipped
    } else {
      file_mb <- file.size(osm_path) / 1e6
      if (file_mb > 200) {
        stop(
          sprintf(
            paste0(
              "The downloaded OSM extract is %.0f MB and likely ",
              "exceeds r5r's geographic extent limit (~975,000 km2).\n",
              "A clipping tool is required to extract the ",
              "municipality area from the state-level file.\n\n",
              "Run interpElections::setup_osmium() to install ",
              "osmium-tool automatically, or install manually:\n",
              "  Windows: conda install -c conda-forge osmium-tool\n",
              "  macOS:   brew install osmium-tool\n",
              "  Linux:   sudo apt install osmium-tool\n\n",
              "Then re-run this function. The state-level .pbf is ",
              "already cached and will not be re-downloaded."
            ),
            file_mb
          ),
          call. = FALSE
        )
      } else if (verbose) {
        message(
          "  Note: clipping tools not available, but file is ",
          sprintf("small (%.0f MB) -- proceeding without clipping.",
                  file_mb)
        )
      }
    }

    result$osm_pbf <- osm_path
  }

  # --- Elevation download ---
  if (elevation) {
    elev_path <- file.path(output_dir, "elevation.tif")

    if (!file.exists(elev_path) || force) {
      if (verbose) message("  Downloading elevation data...")

      elev_raster <- elevatr::get_elev_raster(
        locations = area_sf,
        z = 9
      )

      terra::writeRaster(
        terra::rast(elev_raster),
        elev_path,
        overwrite = TRUE
      )

      if (verbose) {
        message("  Elevation data saved to: ", elev_path)
      }
    } else {
      if (verbose) {
        message("  Using cached elevation file")
      }
    }

    result$elevation_tif <- elev_path
  }

  result
}


#' Clip a .pbf file to a bounding box using osmium or osmconvert
#'
#' @param pbf_path Path to the input .pbf file.
#' @param area_sf sf object whose bbox defines the clip region.
#' @param output_dir Directory for the clipped output file.
#' @param force Re-clip even if output already exists.
#' @param verbose Print messages.
#' @return Path to the clipped .pbf file, or NULL if clipping
#'   tools are not available.
#' @noRd
.clip_pbf <- function(pbf_path, area_sf, output_dir,
                      force = FALSE, verbose = TRUE) {
  # Compute bbox in WGS84
  area_wgs <- sf::st_transform(area_sf, 4326)
  bb <- sf::st_bbox(area_wgs)

  # Build a deterministic output filename from the bbox
  bbox_str <- sprintf(
    "%.4f_%.4f_%.4f_%.4f",
    bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]
  )
  bbox_hash <- substr(.digest_simple(bbox_str), 1, 12)
  clipped_name <- sprintf("osm_clipped_%s.osm.pbf", bbox_hash)
  clipped_path <- file.path(output_dir, clipped_name)

  if (file.exists(clipped_path) && !force) {
    if (verbose) {
      clipped_mb <- file.size(clipped_path) / 1e6
      message(sprintf("  Using cached clipped OSM file (%.1f MB)",
                      clipped_mb))
    }
    return(clipped_path)
  }

  # Format bbox as W,S,E,N for osmium/osmconvert
  bbox_arg <- sprintf(
    "%.6f,%.6f,%.6f,%.6f",
    bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]
  )

  # Try osmium first (preferred)
  osmium <- .find_tool("osmium")
  if (!is.null(osmium)) {
    if (verbose) message("  Clipping with osmium...")
    args <- c("extract", "-b", bbox_arg, "-o", clipped_path,
              "--overwrite", pbf_path)
    err_out <- tryCatch(
      system2(osmium, args, stdout = TRUE, stderr = TRUE),
      error = function(e) conditionMessage(e)
    )
    rc <- attr(err_out, "status")
    if (is.null(rc)) rc <- 0L
    if (rc == 0 && file.exists(clipped_path)) {
      if (verbose) {
        clipped_mb <- file.size(clipped_path) / 1e6
        message(sprintf("  Clipped: %.1f MB", clipped_mb))
      }
      return(clipped_path)
    }
    if (verbose) message("  osmium clipping failed, trying ",
                         "osmconvert...")
  }

  # Try osmconvert on PATH
  osmconvert <- .find_tool("osmconvert")
  # Also check for osmconvert64 (common on Windows)
  if (is.null(osmconvert)) {
    osmconvert <- .find_tool("osmconvert64")
  }
  # Also check the interpElections cache (setup_osmium may have put it there)
  if (is.null(osmconvert)) {
    cached_bin <- file.path(
      get_interpElections_cache_dir(), "bin",
      if (.Platform$OS.type == "windows") "osmconvert.exe"
      else "osmconvert"
    )
    if (file.exists(cached_bin)) osmconvert <- cached_bin
  }

  if (!is.null(osmconvert)) {
    if (verbose) message("  Clipping with osmconvert...")
    base_args <- c(
      pbf_path,
      sprintf("-b=%s", bbox_arg),
      sprintf("-o=%s", clipped_path)
    )

    # Try default first, then with reduced hash memory (32-bit builds
    # cannot allocate the default 1200 MB)
    for (hash_opt in list(NULL, "--hash-memory=400")) {
      args <- c(base_args, hash_opt)
      err_out <- tryCatch(
        system2(osmconvert, args, stdout = TRUE, stderr = TRUE),
        error = function(e) conditionMessage(e)
      )
      rc <- attr(err_out, "status")
      if (is.null(rc)) rc <- 0L
      if (rc == 0 && file.exists(clipped_path)) {
        if (verbose) {
          clipped_mb <- file.size(clipped_path) / 1e6
          message(sprintf("  Clipped: %.1f MB", clipped_mb))
        }
        return(clipped_path)
      }
      # Remove partial output before retrying
      unlink(clipped_path)
    }

    if (verbose) {
      message("  osmconvert clipping failed (exit code ", rc, ")")
      err_lines <- utils::head(err_out, 5)
      if (length(err_lines) > 0) {
        message("  ", paste(err_lines, collapse = "\n  "))
      }
    }
    return(NULL)
  }

  if (verbose) {
    message(
      "No OSM clipping tool found (osmium, osmconvert). ",
      "Run interpElections::setup_osmium() to install."
    )
  }
  NULL
}


#' Find a system tool on PATH
#' @param tool_name Name of the executable.
#' @return Full path to the tool, or NULL if not found.
#' @noRd
.find_tool <- function(tool_name) {
  path <- Sys.which(tool_name)
  if (nzchar(path)) path else NULL
}


#' Install osmium-tool for OSM file clipping
#'
#' Attempts to install `osmium-tool` (or `osmconvert` as a fallback)
#' using the system package manager. This is required to clip large
#' state-level OSM extracts down to municipality-level bounding boxes
#' for r5r routing.
#'
#' @param method Character or NULL. Installation method to use.
#'   If NULL (default), auto-detects. Options:
#'   - `"conda"`: conda install (all platforms)
#'   - `"brew"`: Homebrew (macOS)
#'   - `"apt"`: apt-get (Debian/Ubuntu)
#'   - `"dnf"`: dnf (Fedora/RHEL)
#'   - `"download"`: Download osmconvert binary (Windows only)
#' @param verbose Logical. Default: TRUE.
#'
#' @return Invisibly, the path to the installed tool, or NULL if
#'   installation failed.
#'
#' @details
#' The preferred tool is `osmium-tool`, which is faster and more
#' reliable. `osmconvert` is used as a fallback when osmium is not
#' available.
#'
#' On Windows, the `"download"` method fetches a pre-compiled
#' `osmconvert.exe` binary and saves it to the interpElections cache.
#' This does not require conda or any other package manager.
#'
#' @examples
#' \dontrun{
#' setup_osmium()
#' setup_osmium(method = "conda")
#' setup_osmium(method = "download")  # Windows only
#' }
#'
#' @seealso [download_r5r_data()] which uses osmium for clipping.
#'
#' @export
setup_osmium <- function(method = NULL, verbose = TRUE) {
  # Check if already available
  osmium <- .find_tool("osmium")
  if (!is.null(osmium)) {
    if (verbose) message("[ok] osmium-tool already installed: ", osmium)
    return(invisible(osmium))
  }

  osmconvert <- .find_tool("osmconvert")
  if (is.null(osmconvert)) osmconvert <- .find_tool("osmconvert64")
  if (!is.null(osmconvert)) {
    if (verbose) message("[ok] osmconvert already installed: ", osmconvert)
    return(invisible(osmconvert))
  }

  # Also check cache
  cached_bin <- file.path(
    get_interpElections_cache_dir(), "bin",
    if (.Platform$OS.type == "windows") "osmconvert.exe"
    else "osmconvert"
  )
  if (file.exists(cached_bin)) {
    if (verbose) message("[ok] osmconvert found in cache: ", cached_bin)
    return(invisible(cached_bin))
  }

  is_windows <- .Platform$OS.type == "windows"
  is_mac <- Sys.info()[["sysname"]] == "Darwin"

  # Auto-detect method
  if (is.null(method)) {
    has_conda <- nzchar(Sys.which("conda"))
    if (has_conda) {
      method <- "conda"
    } else if (is_mac && nzchar(Sys.which("brew"))) {
      method <- "brew"
    } else if (!is_windows && nzchar(Sys.which("apt-get"))) {
      method <- "apt"
    } else if (!is_windows && nzchar(Sys.which("dnf"))) {
      method <- "dnf"
    } else if (is_windows) {
      method <- "download"
    } else {
      stop(
        "Could not detect a package manager.\n",
        "Install osmium-tool manually:\n",
        "  conda install -c conda-forge osmium-tool\n",
        "  brew install osmium-tool   (macOS)\n",
        "  apt install osmium-tool    (Debian/Ubuntu)\n",
        "  dnf install osmium-tool    (Fedora/RHEL)",
        call. = FALSE
      )
    }
  }

  if (verbose) message("Installing via ", method, "...")

  result <- switch(method,
    "conda" = {
      rc <- system2("conda", c("install", "-y", "-c", "conda-forge",
                                "osmium-tool"),
                     stdout = if (verbose) "" else FALSE,
                     stderr = if (verbose) "" else FALSE)
      if (rc == 0) .find_tool("osmium") else NULL
    },
    "brew" = {
      rc <- system2("brew", c("install", "osmium-tool"),
                     stdout = if (verbose) "" else FALSE,
                     stderr = if (verbose) "" else FALSE)
      if (rc == 0) .find_tool("osmium") else NULL
    },
    "apt" = {
      rc <- system2("sudo", c("apt-get", "install", "-y",
                               "osmium-tool"),
                     stdout = if (verbose) "" else FALSE,
                     stderr = if (verbose) "" else FALSE)
      if (rc == 0) .find_tool("osmium") else NULL
    },
    "dnf" = {
      rc <- system2("sudo", c("dnf", "install", "-y",
                               "osmium-tool"),
                     stdout = if (verbose) "" else FALSE,
                     stderr = if (verbose) "" else FALSE)
      if (rc == 0) .find_tool("osmium") else NULL
    },
    "download" = {
      .download_osmconvert(verbose = verbose)
    },
    stop("Unknown method: ", method, call. = FALSE)
  )

  if (!is.null(result)) {
    if (verbose) message("[ok] Installed successfully: ", result)
  } else {
    message(
      "[!!] Installation failed. Try installing manually:\n",
      "  conda install -c conda-forge osmium-tool\n",
      if (is_windows) "  Or download osmconvert from: https://wiki.openstreetmap.org/wiki/Osmconvert\n"
      else ""
    )
  }

  invisible(result)
}


#' Download osmconvert binary for Windows
#' @return Path to the downloaded binary, or NULL on failure.
#' @noRd
.download_osmconvert <- function(verbose = TRUE) {
  if (.Platform$OS.type != "windows") {
    if (verbose) {
      message("Binary download is only supported on Windows. ",
              "Use conda or your system package manager.")
    }
    return(NULL)
  }

  bin_dir <- file.path(get_interpElections_cache_dir(), "bin")
  if (!dir.exists(bin_dir)) dir.create(bin_dir, recursive = TRUE)
  dest <- file.path(bin_dir, "osmconvert.exe")

  if (file.exists(dest)) {
    if (verbose) message("osmconvert already downloaded: ", dest)
    return(dest)
  }

  # Direct .exe download from the osmconvert author's server.
  # Source: https://wiki.openstreetmap.org/wiki/Osmconvert
  url <- "http://m.m.i24.cc/osmconvert.exe"

  if (verbose) message("Downloading osmconvert for Windows...")

  dl_ok <- tryCatch({
    utils::download.file(url, dest, mode = "wb", quiet = !verbose)
    TRUE
  }, error = function(e) {
    if (verbose) {
      message("  Download failed: ", conditionMessage(e))
      message("  Try downloading manually from: ",
              "https://wiki.openstreetmap.org/wiki/Osmconvert")
    }
    FALSE
  })

  if (!dl_ok) {
    unlink(dest)
    return(NULL)
  }

  if (file.exists(dest)) {
    if (verbose) message("osmconvert saved to: ", dest)
    return(dest)
  }

  NULL
}


#' Check whether any OSM clipping tool is available
#' @return Logical.
#' @noRd
.has_clip_tool <- function() {
  if (!is.null(.find_tool("osmium"))) return(TRUE)
  if (!is.null(.find_tool("osmconvert"))) return(TRUE)
  if (!is.null(.find_tool("osmconvert64"))) return(TRUE)
  # Check interpElections cache (setup_osmium may have put it there)
  cached_bin <- file.path(
    get_interpElections_cache_dir(), "bin",
    if (.Platform$OS.type == "windows") "osmconvert.exe"
    else "osmconvert"
  )
  file.exists(cached_bin)
}


#' Interactively offer to install osmium-tool
#' @param verbose Logical.
#' @noRd
.offer_osmium_install <- function(verbose = TRUE) {
  if (!interactive()) return(invisible(NULL))

  if (verbose) {
    message(
      "\nNo OSM clipping tool found (osmium, osmconvert).\n",
      "This is required to clip state-level OSM files for r5r routing."
    )
  }

  answer <- readline("Install osmium-tool now? (Y/n): ")
  if (tolower(trimws(answer)) %in% c("", "y", "yes", "s", "sim")) {
    setup_osmium(verbose = verbose)
  }
  invisible(NULL)
}
