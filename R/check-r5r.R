# ---- Internal helpers --------------------------------------------------------

#' Parse Java major version from `java -version` output
#'
#' @param version_lines Character vector of lines from `java -version`.
#' @return Integer major version, or `NA_integer_` if parsing fails.
#' @noRd
.parse_java_version <- function(version_lines) {
  # java -version output looks like:
  #   openjdk version "21.0.2" 2024-01-16
  #   java version "1.8.0_351"
  # We want the major version number (21, 8, etc.)
  txt <- paste(version_lines, collapse = " ")
  m <- regmatches(txt, regexpr('"([^"]+)"', txt))
  if (length(m) == 0) return(NA_integer_)
  ver_str <- gsub('"', '', m)

  parts <- strsplit(ver_str, "[._-]")[[1]]
  major <- suppressWarnings(as.integer(parts[1]))
  if (is.na(major)) return(NA_integer_)

  # Old-style "1.8.x" means Java 8

  if (major == 1L && length(parts) >= 2L) {
    major <- suppressWarnings(as.integer(parts[2]))
  }
  major
}


#' Detect OS and architecture for Adoptium download
#' @return A list with `os` and `arch` strings matching Adoptium API naming.
#' @noRd
.detect_platform <- function() {
  sysname <- Sys.info()[["sysname"]]
  machine <- Sys.info()[["machine"]]

  os <- switch(sysname,
    "Windows" = "windows",
    "Darwin"  = "mac",
    "Linux"   = "linux",
    stop("Unsupported OS: ", sysname, call. = FALSE)
  )

  arch <- if (grepl("aarch64|arm64", machine, ignore.case = TRUE)) {
    "aarch64"
  } else {
    "x64"
  }

  list(os = os, arch = arch)
}


# ---- Exported functions ------------------------------------------------------

#' Check r5r and Java 21 setup
#'
#' Runs a diagnostic check for the r5r dependency chain: whether the `r5r`
#' package is installed and whether a suitable Java/JDK (version 21) is
#' available on the system.
#'
#' @return Invisibly, a list with components:
#' \describe{
#'   \item{r5r_installed}{Logical.}
#'   \item{java_found}{Logical.}
#'   \item{java_version}{Integer major version, or `NA`.}
#'   \item{java_sufficient}{Logical. TRUE if version is exactly 21.}
#'   \item{java_memory}{Character or NULL. Configured JVM max heap (e.g. `"4g"`).}
#'   \item{system_ram}{Character or NULL. Total system RAM (e.g. `"16 GB"`).}
#'   \item{ready}{Logical. TRUE if all checks pass.}
#' }
#'
#' @seealso [setup_java()] to install Java 21, [set_java_memory()] to
#'   configure JVM heap size.
#'
#' @examples
#' check_r5r()
#'
#' @export
check_r5r <- function() {
  # -- r5r package --
  r5r_installed <- requireNamespace("r5r", quietly = TRUE)
  if (r5r_installed) {
    r5r_ver <- as.character(utils::packageVersion("r5r"))
    message("[ok] r5r package installed (v", r5r_ver, ")")
  } else {
    message("[!!] r5r package not installed")
    message("
     Install with: install.packages('r5r')")
  }

  # -- Java --
  java_lines <- tryCatch(
    system2("java", "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  java_found <- !is.null(java_lines)
  java_version <- NA_integer_
  java_sufficient <- FALSE

  if (java_found) {
    java_version <- .parse_java_version(java_lines)
    if (!is.na(java_version)) {
      java_sufficient <- java_version == 21L
      if (java_sufficient) {
        message("[ok] Java ", java_version, " found")
      } else {
        message("[!!] Java ", java_version, " found, but r5r requires exactly 21")
      }
    } else {
      message("[!!] Java found but could not parse version")
    }
  } else {
    message("[!!] Java not found on PATH")
  }

  # -- Java memory --
  java_mem <- .get_java_memory()
  if (!is.null(java_mem$configured)) {
    message("[ok] Java max heap: ", java_mem$configured)
  } else {
    message("[--] Java max heap: not configured (JVM default, typically 256m)")
    message("     Set with: interpElections::set_java_memory(\"4g\")")
  }
  if (!is.null(java_mem$system_ram)) {
    message("     System RAM: ", java_mem$system_ram)
  }

  # -- Summary --
  ready <- r5r_installed && java_found && java_sufficient
  if (ready) {
    message("\nAll checks passed. r5r is ready to use.")
  } else {
    message("\nSome checks failed.")
    if (!java_found || !java_sufficient) {
      message("Run interpElections::setup_java() to download and install Java 21.")
    }
  }

  invisible(list(
    r5r_installed   = r5r_installed,
    java_found      = java_found,
    java_version    = java_version,
    java_sufficient = java_sufficient,
    java_memory     = java_mem$configured,
    system_ram      = java_mem$system_ram,
    ready           = ready
  ))
}


#' Set Java heap memory for r5r
#'
#' Configures the maximum heap size for the Java Virtual Machine used by r5r.
#' Large travel-time matrices can require several gigabytes of JVM heap.
#'
#' @param size Character. Memory size with unit suffix, e.g. `"4g"` for
#'   4 GB, `"512m"` for 512 MB, `"8g"` for 8 GB.
#' @param persist Logical. Also write to `~/.Renviron` so the setting
#'   persists across R sessions. Default: TRUE in interactive sessions.
#'
#' @details
#' This sets `options(java.parameters = "-Xmx{size}")`. It **must** be called
#' before `r5r` (or `rJava`) is loaded — once the JVM starts, heap size
#' cannot be changed without restarting R.
#'
#' A rule of thumb: allocate \eqn{\ge 2} GB per million OD pairs you expect
#' in your travel-time matrix. For a municipality with 5,000 census tracts
#' and 200 polling locations, that's 1 million pairs — 2-4 GB is usually
#' enough. Larger cities (e.g. Sao Paulo) may need 8-16 GB.
#'
#' @return Invisibly, the previous value of `getOption("java.parameters")`.
#'
#' @examples
#' \dontrun{
#' set_java_memory("4g")
#' set_java_memory("8g", persist = TRUE)
#' }
#'
#' @seealso [check_r5r()] to see current memory settings.
#'
#' @export
set_java_memory <- function(size, persist = interactive()) {
  if (!grepl("^[0-9]+[mgMG]$", size)) {
    stop("size must be a number followed by 'm' or 'g', e.g. '4g' or '512m'",
         call. = FALSE)
  }

  # Warn if rJava is already loaded (JVM already started)
  if ("rJava" %in% loadedNamespaces()) {
    warning(
      "rJava is already loaded -- the JVM heap cannot be changed in this session.\n",
      "Restart R and call set_java_memory() before loading r5r.",
      call. = FALSE
    )
  }

  prev <- getOption("java.parameters")
  param <- paste0("-Xmx", size)
  options(java.parameters = param)

  message("Java max heap set to: ", size, " (via options(java.parameters = \"", param, "\"))")

  if (persist) {
    .persist_renviron_var("_JAVA_OPTIONS", param)
    message("Saved to ~/.Renviron (takes effect on next R session).")
  }

  invisible(prev)
}


#' Get current Java memory configuration
#' @return List with `configured` (character or NULL) and `system_ram` (character or NULL).
#' @noRd
.get_java_memory <- function() {
  # Check R option
  java_params <- getOption("java.parameters")
  configured <- NULL
  if (!is.null(java_params)) {
    xmx <- grep("-Xmx", java_params, value = TRUE)
    if (length(xmx) > 0) {
      configured <- sub("-Xmx", "", xmx[length(xmx)])
    }
  }

  # If not in options, check _JAVA_OPTIONS env var
  if (is.null(configured)) {
    env_opts <- Sys.getenv("_JAVA_OPTIONS", "")
    if (nzchar(env_opts)) {
      m <- regmatches(env_opts, regexpr("-Xmx[0-9]+[mgMG]", env_opts))
      if (length(m) > 0) {
        configured <- sub("-Xmx", "", m)
      }
    }
  }

  # System RAM
  system_ram <- tryCatch(.get_system_ram(), error = function(e) NULL)

  list(configured = configured, system_ram = system_ram)
}


#' Get total system RAM as a human-readable string
#' @return Character like "16 GB", or NULL on failure.
#' @noRd
.get_system_ram <- function() {
  sysname <- Sys.info()[["sysname"]]

  bytes <- if (sysname == "Windows") {
    # Use PowerShell Get-CimInstance (wmic is deprecated on Windows 11+)
    out <- tryCatch(
      system2("powershell", c("-NoProfile", "-Command",
        "(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory"),
        stdout = TRUE, stderr = TRUE),
      error = function(e) NULL
    )
    if (!is.null(out)) {
      nums <- grep("^[0-9]", trimws(out), value = TRUE)
      if (length(nums) > 0) as.numeric(trimws(nums[1])) else NULL
    } else {
      NULL
    }
  } else if (sysname == "Darwin") {
    tryCatch({
      out <- system2("sysctl", c("-n", "hw.memsize"),
                      stdout = TRUE, stderr = TRUE)
      as.numeric(trimws(out))
    }, error = function(e) NULL)
  } else {
    # Linux: /proc/meminfo
    tryCatch({
      meminfo <- readLines("/proc/meminfo", n = 1)
      kb <- as.numeric(gsub("[^0-9]", "", meminfo))
      kb * 1024
    }, error = function(e) NULL)
  }

  if (is.null(bytes) || is.na(bytes)) return(NULL)
  gb <- round(bytes / 1024^3, 1)
  paste0(gb, " GB")
}


#' Set JAVA_HOME and update PATH for the current R session
#' @param jdk_home Path to the JDK root directory.
#' @noRd
.activate_java <- function(jdk_home) {
  Sys.setenv(JAVA_HOME = jdk_home)

  bin_dir <- file.path(jdk_home, "bin")
  current_path <- Sys.getenv("PATH")
  if (!grepl(normalizePath(bin_dir, winslash = "/", mustWork = FALSE),
             current_path, fixed = TRUE)) {
    Sys.setenv(PATH = paste(bin_dir, current_path, sep = .Platform$path.sep))
  }
}


#' Append or update JAVA_HOME in ~/.Renviron
#' @param jdk_home Path to the JDK root directory.
#' @param verbose Logical.
#' @noRd
.persist_java_home <- function(jdk_home, verbose = TRUE) {
  .persist_renviron_var("JAVA_HOME", paste0('"', jdk_home, '"'), verbose)
  if (verbose) {
    message("JAVA_HOME saved to ~/.Renviron (takes effect on next R session).")
  }
  invisible(NULL)
}


#' Append or update a variable in ~/.Renviron
#' @param var_name Character. The environment variable name.
#' @param value Character. The value to set.
#' @param verbose Logical.
#' @noRd
.persist_renviron_var <- function(var_name, value, verbose = FALSE) {
  home <- Sys.getenv("HOME")
  if (!nzchar(home)) home <- path.expand("~")
  renviron <- file.path(home, ".Renviron")
  new_line <- paste0(var_name, "=", value)
  pattern <- paste0("^\\Q", var_name, "\\E\\s*=")

  if (file.exists(renviron)) {
    lines <- readLines(renviron, warn = FALSE)
    idx <- grep(pattern, lines)

    if (length(idx) > 0) {
      if (lines[idx[1]] == new_line) return(invisible(NULL))
      lines[idx[1]] <- new_line
      if (length(idx) > 1) lines <- lines[-idx[-1]]
    } else {
      if (length(lines) > 0 && nchar(lines[length(lines)]) > 0) {
        lines <- c(lines, "")
      }
      lines <- c(lines, new_line)
    }
    writeLines(lines, renviron)
  } else {
    writeLines(new_line, renviron)
  }
  invisible(NULL)
}
