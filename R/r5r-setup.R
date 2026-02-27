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
#' \dontrun{
#' check_r5r()
#' }
#'
#' @keywords internal
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
#' @keywords internal
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


#' Recommend a JVM heap size based on system RAM
#'
#' Uses half of system RAM, capped between 2g and 8g.
#'
#' @return A string like `"4g"`, or `NULL` if system RAM cannot be detected.
#' @noRd
.recommend_heap_size <- function() {
  ram_str <- tryCatch(.get_system_ram(), error = function(e) NULL)
  if (is.null(ram_str)) return(NULL)

  gb <- as.numeric(sub(" GB$", "", ram_str))
  if (is.na(gb) || gb < 1) return(NULL)

  # Use half of system RAM, capped between 2 GB and 8 GB
  heap_gb <- max(2, min(8, floor(gb / 2)))
  paste0(heap_gb, "g")
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

# ---- Internal helpers for setup_java() --------------------------------------

#' Discover all Java installations on the system
#'
#' Scans the system for Java installations using platform-specific methods.
#' Results are deduplicated by normalized path.
#'
#' @param verbose Logical. Print discovery progress.
#' @return A data.frame with columns: `path`, `version` (integer),
#'   `vendor` (character), `source` (character, how the installation was found).
#' @noRd
.discover_java_installations <- function(verbose = TRUE) {
  candidates <- list()
  platform <- .detect_platform()
  java_exe <- if (platform$os == "windows") "java.exe" else "java"

  # Helper: add candidate with source tag
  add_candidate <- function(path, source) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (dir.exists(path) && file.exists(file.path(path, "bin", java_exe))) {
      candidates[[length(candidates) + 1L]] <<- list(path = path, source = source)
    }
  }

  # ---- JAVA_HOME env var (all platforms) ----
  tryCatch({
    java_home <- Sys.getenv("JAVA_HOME", "")
    if (nzchar(java_home)) add_candidate(java_home, "JAVA_HOME env var")
  }, error = function(e) NULL)

  # ---- Sys.which("java") (all platforms) ----
  tryCatch({
    java_path <- Sys.which("java")
    if (nzchar(java_path)) {
      resolved <- normalizePath(java_path, winslash = "/", mustWork = FALSE)
      jdk_home <- dirname(dirname(resolved))
      # Skip root-level dirs like /usr (macOS java shim resolves there)
      if (!jdk_home %in% c("/usr", "/usr/local")) {
        add_candidate(jdk_home, "PATH (Sys.which)")
      }
    }
  }, error = function(e) NULL)

  if (platform$os == "windows") {
    # ---- Windows: Registry via PowerShell ----
    reg_paths <- c(
      "HKLM:\\\\SOFTWARE\\\\JavaSoft\\\\JDK",
      "HKLM:\\\\SOFTWARE\\\\Eclipse Adoptium\\\\JDK",
      "HKLM:\\\\SOFTWARE\\\\Microsoft\\\\JDK",
      "HKLM:\\\\SOFTWARE\\\\Azul Systems\\\\Zulu",
      "HKLM:\\\\SOFTWARE\\\\AdoptOpenJDK\\\\JDK"
    )
    for (rp in reg_paths) {
      tryCatch({
        ps_cmd <- paste0(
          'Get-ChildItem "', rp,
          '" -ErrorAction SilentlyContinue | ',
          'ForEach-Object { (Get-ItemProperty $_.PSPath).Path }'
        )
        out <- suppressWarnings(
          system2("powershell", c("-NoProfile", "-Command", ps_cmd),
                  stdout = TRUE, stderr = TRUE)
        )
        out <- trimws(out)
        out <- out[nzchar(out)]
        for (p in out) add_candidate(p, paste0("Registry: ", rp))
      }, error = function(e) NULL)
    }

    # ---- Windows: Filesystem scan ----
    fs_dirs <- c(
      "C:/Program Files/Java",
      "C:/Program Files/Eclipse Adoptium",
      "C:/Program Files/Microsoft",
      "C:/Program Files/Zulu"
    )
    for (d in fs_dirs) {
      tryCatch({
        if (dir.exists(d)) {
          subdirs <- list.dirs(d, recursive = FALSE, full.names = TRUE)
          jdk_dirs <- grep("jdk", subdirs, value = TRUE, ignore.case = TRUE)
          for (p in jdk_dirs) add_candidate(p, paste0("Filesystem: ", d))
        }
      }, error = function(e) NULL)
    }
  } else if (platform$os == "mac") {
    # ---- macOS: /usr/libexec/java_home -V ----
    tryCatch({
      out <- suppressWarnings(
        system2("/usr/libexec/java_home", "-V",
                stdout = TRUE, stderr = TRUE)
      )
      for (line in out) {
        m <- regmatches(line, regexpr("/Library/Java/[^ \t]+", line))
        if (length(m) > 0) add_candidate(m, "/usr/libexec/java_home")
      }
    }, error = function(e) NULL)

    # ---- macOS: /Library/Java/JavaVirtualMachines ----
    tryCatch({
      jvm_dir <- "/Library/Java/JavaVirtualMachines"
      if (dir.exists(jvm_dir)) {
        subdirs <- list.dirs(jvm_dir, recursive = FALSE, full.names = TRUE)
        for (d in subdirs) {
          add_candidate(file.path(d, "Contents", "Home"),
                        "JavaVirtualMachines")
        }
      }
    }, error = function(e) NULL)

    # ---- macOS: Homebrew ----
    tryCatch({
      brew_prefix <- suppressWarnings(
        system2("brew", "--prefix", stdout = TRUE, stderr = TRUE)
      )
      if (length(brew_prefix) > 0) {
        opt_dir <- file.path(trimws(brew_prefix[1]), "opt")
        if (dir.exists(opt_dir)) {
          subdirs <- list.dirs(opt_dir, recursive = FALSE, full.names = TRUE)
          jdk_dirs <- grep("openjdk", subdirs, value = TRUE, ignore.case = TRUE)
          for (p in jdk_dirs) {
            libexec <- file.path(p, "libexec", "openjdk.jdk",
                                 "Contents", "Home")
            if (dir.exists(libexec)) {
              add_candidate(libexec, "Homebrew")
            } else {
              add_candidate(p, "Homebrew")
            }
          }
        }
      }
    }, error = function(e) NULL)
  } else {
    # ---- Linux: update-alternatives ----
    tryCatch({
      out <- suppressWarnings(
        system2("update-alternatives", c("--list", "java"),
                stdout = TRUE, stderr = TRUE)
      )
      for (p in out) {
        p <- trimws(p)
        if (nzchar(p)) add_candidate(dirname(dirname(p)),
                                      "update-alternatives")
      }
    }, error = function(e) NULL)

    # ---- Linux: /usr/lib/jvm ----
    tryCatch({
      jvm_dir <- "/usr/lib/jvm"
      if (dir.exists(jvm_dir)) {
        subdirs <- list.dirs(jvm_dir, recursive = FALSE, full.names = TRUE)
        jdk_dirs <- grep("j(dk|re|ava)", subdirs,
                         value = TRUE, ignore.case = TRUE)
        for (p in jdk_dirs) add_candidate(p, "/usr/lib/jvm")
      }
    }, error = function(e) NULL)
  }

  # ---- interpElections managed installation ----
  tryCatch({
    managed_dir <- file.path(tools::R_user_dir("interpElections", "data"),
                             "java", "jdk-21")
    add_candidate(managed_dir, "interpElections managed")
    # macOS: Adoptium tarballs extract with Contents/Home/ structure
    contents_home <- file.path(managed_dir, "Contents", "Home")
    if (dir.exists(contents_home)) {
      add_candidate(contents_home, "interpElections managed")
    }
  }, error = function(e) NULL)

  # ---- Deduplicate by normalized path ----
  if (length(candidates) == 0L) {
    return(data.frame(
      path    = character(0),
      version = integer(0),
      vendor  = character(0),
      source  = character(0),
      stringsAsFactors = FALSE
    ))
  }

  paths   <- vapply(candidates, `[[`, character(1), "path")
  sources <- vapply(candidates, `[[`, character(1), "source")
  norm_paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)

  keep <- !duplicated(tolower(norm_paths))
  paths      <- paths[keep]
  sources    <- sources[keep]
  norm_paths <- norm_paths[keep]

  # ---- Verify each by running java -version ----
  n <- length(paths)
  versions <- integer(n)
  vendors  <- character(n)
  valid    <- logical(n)

  for (i in seq_len(n)) {
    java_bin <- file.path(paths[i], "bin", java_exe)
    tryCatch({
      out <- suppressWarnings(
        system2(java_bin, "-version", stdout = TRUE, stderr = TRUE)
      )
      ver <- .parse_java_version(out)
      versions[i] <- if (is.na(ver)) 0L else ver

      txt <- paste(out, collapse = " ")
      vendors[i] <- if (grepl("Temurin|Adoptium|AdoptOpenJDK", txt,
                              ignore.case = TRUE)) {
        "Adoptium/Temurin"
      } else if (grepl("Oracle|Java\\(TM\\)", txt, ignore.case = TRUE)) {
        "Oracle"
      } else if (grepl("Microsoft", txt, ignore.case = TRUE)) {
        "Microsoft"
      } else if (grepl("Zulu|Azul", txt, ignore.case = TRUE)) {
        "Azul Zulu"
      } else if (grepl("OpenJDK|openjdk", txt, ignore.case = TRUE)) {
        "OpenJDK"
      } else {
        "Unknown"
      }
      valid[i] <- versions[i] > 0L
    }, error = function(e) {
      versions[i] <<- 0L
      vendors[i]  <<- "Unknown"
      valid[i]    <<- FALSE
    })
  }

  df <- data.frame(
    path    = norm_paths[valid],
    version = versions[valid],
    vendor  = vendors[valid],
    source  = sources[valid],
    stringsAsFactors = FALSE
  )

  if (verbose && nrow(df) > 0L) {
    message("Found ", nrow(df), " Java installation(s):")
    for (i in seq_len(nrow(df))) {
      message("  Java ", df$version[i], " (", df$vendor[i], ") at ",
              df$path[i], " [", df$source[i], "]")
    }
  } else if (verbose) {
    message("No Java installations found on system.")
  }

  df
}


#' Check for Java version conflicts
#'
#' Pure filtering: splits installations into Java 21 vs others.
#'
#' @param installations data.frame from [.discover_java_installations()].
#' @return List with `java21` (data.frame), `others` (data.frame),
#'   `has_conflict` (logical).
#' @noRd
.check_java_conflicts <- function(installations) {
  java21 <- installations[installations$version == 21L, , drop = FALSE]
  others <- installations[installations$version != 21L, , drop = FALSE]
  list(java21 = java21, others = others, has_conflict = nrow(others) > 0L)
}


#' Handle conflicting Java installations interactively
#'
#' When non-Java-21 installations are found, asks the user whether to remove
#' them. If the user declines, aborts with an error (only Java 21 is
#' acceptable for r5r). In non-interactive mode, always aborts.
#'
#' @param conflicts List from [.check_java_conflicts()].
#' @param verbose Logical.
#' @return invisible(TRUE) if no conflicts or conflicts were resolved.
#'   Throws an error if the user declines removal or in non-interactive mode.
#' @noRd
.handle_java_conflicts <- function(conflicts, verbose = TRUE) {
  if (!conflicts$has_conflict) return(invisible(TRUE))

  others <- conflicts$others

  message(
    "\nFound ", nrow(others),
    " incompatible Java installation(s):"
  )
  for (i in seq_len(nrow(others))) {
    message("  Java ", others$version[i], " (", others$vendor[i], ") at ",
            others$path[i])
  }

  if (!interactive()) {
    stop(
      "Incompatible Java version(s) found. ",
      "Run setup_java() interactively to handle removal.",
      call. = FALSE
    )
  }

  choice <- utils::menu(
    c("Yes, remove incompatible version(s)",
      "No, abort setup"),
    title = paste0(
      "\nr5r requires exactly Java 21. ",
      "Remove the incompatible version(s)?"
    )
  )

  if (choice != 1L) {
    stop(
      "Java setup aborted. Only Java 21 is compatible with r5r.\n",
      "Remove incompatible Java versions and try again.",
      call. = FALSE
    )
  }

  # Attempt to uninstall each conflicting version
  results <- logical(nrow(others))
  for (i in seq_len(nrow(others))) {
    results[i] <- isTRUE(.uninstall_java(
      path    = others$path[i],
      vendor  = others$vendor[i],
      source  = others$source[i],
      version = others$version[i],
      verbose = verbose
    ))
  }

  if (any(!results)) {
    failed <- others$path[!results]
    warning(
      "Some Java versions could not be removed: ",
      paste(failed, collapse = ", "), "\n",
      "setup_java() will continue, but you may need to remove them manually.",
      call. = FALSE, immediate. = TRUE
    )
  }

  invisible(TRUE)
}


#' Uninstall a Java installation (dispatcher)
#'
#' Routes to platform-specific uninstall logic.
#'
#' @param path Character. JDK home path.
#' @param vendor Character. Vendor name.
#' @param source Character. How the installation was discovered.
#' @param version Integer. Java major version.
#' @param verbose Logical.
#' @return invisible(logical) TRUE on success.
#' @noRd
.uninstall_java <- function(path, vendor, source, version, verbose = TRUE) {
  platform <- .detect_platform()
  if (verbose) message("  Removing Java ", version, " at ", path, "...")

  success <- tryCatch({
    if (platform$os == "mac") {
      .uninstall_java_mac(path, vendor, source, version, verbose)
    } else if (platform$os == "windows") {
      .uninstall_java_windows(path, vendor, source, version, verbose)
    } else {
      .uninstall_java_linux(path, vendor, source, version, verbose)
    }
  }, error = function(e) {
    if (verbose) message("  Error during removal: ", conditionMessage(e))
    FALSE
  })

  if (verbose) {
    if (isTRUE(success)) {
      message("  Successfully removed Java ", version, ".")
    } else {
      warning(
        "Could not automatically remove Java ", version, " at ", path, ".\n",
        "Please remove it manually and re-run setup_java().",
        call. = FALSE, immediate. = TRUE
      )
    }
  }

  invisible(success)
}


#' Uninstall Java on macOS
#'
#' Handles Homebrew installs, system JVMs in /Library/Java, and
#' interpElections-managed installs.
#'
#' @inheritParams .uninstall_java
#' @return logical TRUE on success.
#' @noRd
.uninstall_java_mac <- function(path, vendor, source, version, verbose = TRUE) {
  # --- Homebrew installs ---
  if (grepl("Homebrew", source, ignore.case = TRUE)) {
    formula <- .guess_brew_formula(path)
    if (!is.null(formula)) {
      if (verbose) message("    Uninstalling via Homebrew: ", formula)
      res <- tryCatch(
        system2("brew", c("uninstall", "--force", formula),
                stdout = TRUE, stderr = TRUE),
        error = function(e) NULL,
        warning = function(w) NULL
      )
      return(!is.null(res))
    }
  }

  # --- System JVMs in /Library/Java/JavaVirtualMachines ---
  bundle_path <- .get_jdk_bundle_path(path)
  if (!is.null(bundle_path) &&
      grepl("^/Library/Java/JavaVirtualMachines/", bundle_path)) {
    if (verbose) message("    Removing system JVM: ", bundle_path)
    # Sanitize path to prevent shell injection via osascript
    safe_path <- gsub('[\\\\"]', '', bundle_path)
    # Use osascript for native macOS admin password prompt
    script <- sprintf(
      'do shell script "rm -rf \\"%s\\"" with administrator privileges',
      safe_path
    )
    res <- tryCatch(
      system2("osascript", c("-e", script),
              stdout = TRUE, stderr = TRUE),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    return(!is.null(res) && !dir.exists(bundle_path))
  }

  # --- interpElections managed or other ---
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
    return(!dir.exists(path))
  }

  FALSE
}


#' Guess the Homebrew formula name from a JDK path
#'
#' @param path JDK home path (e.g.,
#'   `/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home`).
#' @return Character formula name (e.g., `"openjdk@17"`), or NULL.
#' @noRd
.guess_brew_formula <- function(path) {
  # Typical Homebrew JDK paths:
  #   /opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home
  #   /opt/homebrew/Cellar/openjdk@17/17.0.x/libexec/openjdk.jdk/Contents/Home
  #   /usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home
  m <- regmatches(path, regexpr("openjdk(@[0-9]+)?", path))
  if (length(m) > 0L) return(m[1])

  # Match Homebrew temurin formula (e.g. temurin@17, temurin17)
  # Use lookahead to avoid matching fragments inside .jdk extensions
  m <- regmatches(path, regexpr("temurin(@[0-9]+|[0-9]+)?(?=/)", path,
                                 perl = TRUE))
  if (length(m) > 0L && nzchar(m[1])) return(m[1])

  NULL
}


#' Get the .jdk bundle root path from a JDK home path
#'
#' On macOS, JDKs in /Library/Java/JavaVirtualMachines/ have structure:
#'   <name>.jdk/Contents/Home/  (the JDK home)
#' This function walks up to find the .jdk bundle root.
#'
#' @param path JDK home path.
#' @return Character path to the .jdk bundle, or NULL if not found.
#' @noRd
.get_jdk_bundle_path <- function(path) {
  # Walk up from path looking for a .jdk directory
  current <- normalizePath(path, winslash = "/", mustWork = FALSE)
  for (i in seq_len(5L)) {
    if (grepl("\\.jdk$", current)) return(current)
    parent <- dirname(current)
    if (parent == current) break # reached root
    current <- parent
  }
  NULL
}


#' Uninstall Java on Windows
#'
#' Searches the Windows Uninstall registry for matching Java entries and
#' runs the uninstaller. Falls back to directory removal.
#'
#' @inheritParams .uninstall_java
#' @return logical TRUE on success.
#' @noRd
.uninstall_java_windows <- function(path, vendor, source, version,
                                     verbose = TRUE) {
  # Try registry-based uninstall via PowerShell
  ps_cmd <- paste0(
    '$paths = @(',
    '"HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\*",',
    '"HKLM:\\SOFTWARE\\WOW6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\*"',
    '); ',
    '$apps = Get-ItemProperty $paths -EA SilentlyContinue | ',
    'Where-Object { ',
    '($_.DisplayName -like "*Java*" -or $_.DisplayName -like "*JDK*") ',
    '-and $_.DisplayName -notlike "*21*" ',
    '-and $_.InstallLocation -and ',
    '($_.InstallLocation.TrimEnd("\\") -eq "',
    gsub("/", "\\\\", path), '")',
    '}; ',
    'if ($apps) { ',
    'foreach ($app in $apps) { ',
    'if ($app.QuietUninstallString) { ',
    'cmd /c $app.QuietUninstallString ',
    '} elseif ($app.UninstallString) { ',
    'cmd /c $app.UninstallString /quiet ',
    '} } ',
    'Write-Output "DONE" ',
    '} else { Write-Output "NOTFOUND" }'
  )

  result <- tryCatch(
    system2("powershell", c("-NoProfile", "-Command", ps_cmd),
            stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )

  if (!is.null(result) && any(grepl("DONE", result))) {
    return(TRUE)
  }

  # Fallback: remove the directory
  if (verbose) message("    Registry uninstaller not found, removing directory.")
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
    return(!dir.exists(path))
  }

  FALSE
}


#' Uninstall Java on Linux
#'
#' Tries to find the owning package via dpkg or rpm, then removes it.
#' Falls back to directory removal.
#'
#' @inheritParams .uninstall_java
#' @return logical TRUE on success.
#' @noRd
.uninstall_java_linux <- function(path, vendor, source, version,
                                   verbose = TRUE) {
  java_bin <- file.path(path, "bin", "java")

  # Check if passwordless sudo is available (avoid hanging on password prompt)
  has_sudo <- tryCatch({
    code <- system2("sudo", c("-n", "true"),
                    stdout = FALSE, stderr = FALSE)
    code == 0L
  }, error = function(e) FALSE)

  # Try dpkg (Debian/Ubuntu)
  pkg <- tryCatch({
    out <- system2("dpkg", c("-S", java_bin),
                   stdout = TRUE, stderr = TRUE)
    if (length(out) > 0L && !any(grepl("not found", out, ignore.case = TRUE))) {
      sub(":.*$", "", trimws(out[1]))
    } else {
      NULL
    }
  }, error = function(e) NULL,
     warning = function(w) NULL)

  if (!is.null(pkg)) {
    if (!has_sudo) {
      if (verbose) {
        message("    sudo requires a password. Please remove manually:")
        message("    sudo apt-get remove -y ", pkg)
      }
      return(FALSE)
    }
    if (verbose) message("    Removing package: ", pkg)
    exit_code <- system2("sudo", c("apt-get", "remove", "-y", pkg),
                         stdout = TRUE, stderr = TRUE)
    return(is.null(attr(exit_code, "status")))
  }

  # Try rpm (Fedora/RHEL)
  pkg <- tryCatch({
    out <- system2("rpm", c("-qf", java_bin),
                   stdout = TRUE, stderr = TRUE)
    if (length(out) > 0L && !any(grepl("not owned", out, ignore.case = TRUE))) {
      trimws(out[1])
    } else {
      NULL
    }
  }, error = function(e) NULL,
     warning = function(w) NULL)

  if (!is.null(pkg)) {
    if (!has_sudo) {
      if (verbose) {
        message("    sudo requires a password. Please remove manually:")
        message("    sudo dnf remove -y ", pkg)
      }
      return(FALSE)
    }
    if (verbose) message("    Removing package: ", pkg)
    exit_code <- system2("sudo", c("dnf", "remove", "-y", pkg),
                         stdout = TRUE, stderr = TRUE)
    return(is.null(attr(exit_code, "status")))
  }

  # Fallback: remove the directory
  if (verbose) message("    Package manager not available, removing directory.")
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
    return(!dir.exists(path))
  }

  FALSE
}


#' Write or update a variable in a shell config file (idempotent)
#'
#' Reads the file, looks for an existing `export VAR_NAME=` line, and either
#' updates it or appends a new line. Same read-modify-write pattern as
#' [.persist_renviron_var()].
#'
#' @param file_path Path to the shell config file.
#' @param var_name Variable name.
#' @param value Value to set.
#' @return invisible(NULL)
#' @noRd
.persist_shell_var <- function(file_path, var_name, value) {
  new_line <- paste0('export ', var_name, '="', value, '"')
  pattern  <- paste0("^\\s*export\\s+", var_name, "\\s*=")

  if (file.exists(file_path)) {
    lines <- readLines(file_path, warn = FALSE)
    idx   <- grep(pattern, lines)

    if (length(idx) > 0L) {
      if (lines[idx[1]] == new_line) return(invisible(NULL))
      lines[idx[1]] <- new_line
      if (length(idx) > 1L) lines <- lines[-idx[-1]]
    } else {
      if (length(lines) > 0L && nchar(lines[length(lines)]) > 0L) {
        lines <- c(lines, "")
      }
      lines <- c(lines, new_line)
    }
    writeLines(lines, file_path)
  } else {
    writeLines(new_line, file_path)
  }
  invisible(NULL)
}


#' Set an environment variable at the OS level
#'
#' On Windows, uses PowerShell to set the variable at User scope.
#' On macOS/Linux, writes to the user's shell config file.
#'
#' @param var_name Character. Variable name.
#' @param value Character. Value to set.
#' @param verbose Logical.
#' @return invisible(logical) TRUE on success.
#' @noRd
.persist_env_var_os <- function(var_name, value, verbose = TRUE) {
  platform <- .detect_platform()

  success <- tryCatch({
    if (platform$os == "windows") {
      ps_cmd <- paste0(
        '[Environment]::SetEnvironmentVariable("', var_name,
        '", "', value, '", "User")'
      )
      system2("powershell", c("-NoProfile", "-Command", ps_cmd),
              stdout = TRUE, stderr = TRUE)
      TRUE
    } else {
      shell_path <- Sys.getenv("SHELL", "/bin/bash")
      config_file <- if (grepl("zsh", shell_path)) {
        path.expand("~/.zshrc")
      } else {
        path.expand("~/.bashrc")
      }

      .persist_shell_var(config_file, var_name, value)

      if (platform$os == "linux") {
        .persist_shell_var(path.expand("~/.profile"), var_name, value)
      }
      TRUE
    }
  }, error = function(e) {
    if (verbose) warning("Could not set ", var_name, " at OS level: ",
                         conditionMessage(e), call. = FALSE)
    FALSE
  })

  if (verbose && success) {
    if (platform$os == "windows") {
      message(var_name, " set at Windows User level.")
    } else {
      message(var_name, " written to shell config.")
    }
  }

  invisible(success)
}


#' Prepend JDK bin/ to PATH at OS level
#'
#' On Windows, modifies the User PATH via PowerShell.
#' On macOS/Linux, appends an `export PATH=` line to the shell config.
#' Idempotent: checks whether the directory is already present before adding.
#'
#' @param bin_dir Character. Path to JDK's bin/ directory.
#' @param verbose Logical.
#' @return invisible(logical) TRUE on success.
#' @noRd
.persist_path_prepend_os <- function(bin_dir, verbose = TRUE) {
  platform <- .detect_platform()
  bin_dir  <- normalizePath(bin_dir, winslash = "/", mustWork = FALSE)

  success <- tryCatch({
    if (platform$os == "windows") {
      ps_read <- '[Environment]::GetEnvironmentVariable("Path", "User")'
      current <- system2("powershell", c("-NoProfile", "-Command", ps_read),
                         stdout = TRUE, stderr = TRUE)
      current <- paste(trimws(current), collapse = "")

      bin_win <- gsub("/", "\\\\", bin_dir)
      if (!grepl(bin_win, current, fixed = TRUE, ignore.case = TRUE)) {
        new_path <- paste0(bin_win, ";", current)
        ps_write <- paste0(
          '[Environment]::SetEnvironmentVariable("Path", "',
          new_path, '", "User")'
        )
        system2("powershell", c("-NoProfile", "-Command", ps_write),
                stdout = TRUE, stderr = TRUE)
      }
      TRUE
    } else {
      shell_path <- Sys.getenv("SHELL", "/bin/bash")
      config_file <- if (grepl("zsh", shell_path)) {
        path.expand("~/.zshrc")
      } else {
        path.expand("~/.bashrc")
      }

      path_line <- paste0('export PATH="', bin_dir, ':$PATH"')

      .add_line_if_absent <- function(fp, line, marker) {
        if (file.exists(fp)) {
          lines <- readLines(fp, warn = FALSE)
          if (any(grepl(marker, lines, fixed = TRUE))) return()
          if (length(lines) > 0L && nchar(lines[length(lines)]) > 0L) {
            lines <- c(lines, "")
          }
          lines <- c(lines, line)
          writeLines(lines, fp)
        } else {
          writeLines(line, fp)
        }
      }

      .add_line_if_absent(config_file, path_line, bin_dir)
      if (platform$os == "linux") {
        .add_line_if_absent(path.expand("~/.profile"), path_line, bin_dir)
      }
      TRUE
    }
  }, error = function(e) {
    if (verbose) warning("Could not add ", bin_dir, " to PATH: ",
                         conditionMessage(e), call. = FALSE)
    FALSE
  })

  if (verbose && success) {
    message("JDK bin/ added to system PATH.")
  }
  invisible(success)
}


#' Warn about RStudio restart if needed
#'
#' If running inside RStudio, prints a message asking the user to restart
#' so that the new JAVA_HOME takes effect.
#'
#' @param jdk_home Path to JDK.
#' @param verbose Logical.
#' @return invisible(NULL)
#' @noRd
.configure_rstudio <- function(jdk_home, verbose = TRUE) {
  if (nzchar(Sys.getenv("RSTUDIO", ""))) {
    if (verbose) {
      message(
        "RStudio detected. Please restart RStudio for JAVA_HOME changes ",
        "to take full effect."
      )
    }
  }
  invisible(NULL)
}


#' Install and configure rJava
#'
#' Checks whether rJava is installed, offers to install it in interactive mode,
#' runs `R CMD javareconf` on macOS/Linux, and verifies with `.jinit()`.
#'
#' @param jdk_home Path to JDK.
#' @param verbose Logical.
#' @return List with `installed`, `loads_ok`, `javareconf_ok`.
#' @noRd
.setup_rjava <- function(jdk_home, verbose = TRUE) {
  result <- list(installed = FALSE, loads_ok = FALSE, javareconf_ok = NA)

  has_rjava <- requireNamespace("rJava", quietly = TRUE)

  if (!has_rjava) {
    if (interactive()) {
      choice <- utils::menu(
        c("Yes, install rJava", "No, skip"),
        title = "rJava is not installed. Install it now?"
      )
      if (choice != 1L) {
        if (verbose) message("Skipping rJava installation.")
        return(invisible(result))
      }
    } else {
      if (verbose) message("rJava not installed. Skipping (non-interactive).")
      return(invisible(result))
    }

    install_ok <- tryCatch({
      utils::install.packages("rJava", type = "binary", quiet = !verbose)
      TRUE
    }, error = function(e) FALSE, warning = function(w) TRUE)

    if (!install_ok) {
      install_ok <- tryCatch({
        utils::install.packages("rJava", type = "source", quiet = !verbose)
        TRUE
      }, error = function(e) {
        if (verbose) warning("rJava installation failed: ",
                             conditionMessage(e), call. = FALSE)
        FALSE
      })
    }

    if (!install_ok) return(invisible(result))
    has_rjava <- requireNamespace("rJava", quietly = TRUE)
  }

  result$installed <- has_rjava

  # R CMD javareconf on macOS/Linux
  platform <- .detect_platform()
  if (platform$os != "windows") {
    result$javareconf_ok <- tryCatch({
      r_bin <- file.path(R.home("bin"), "R")
      suppressWarnings(
        system2(r_bin, c("CMD", "javareconf", "-e"),
                stdout = TRUE, stderr = TRUE)
      )
      TRUE
    }, error = function(e) {
      if (verbose) warning("R CMD javareconf failed: ",
                           conditionMessage(e), call. = FALSE)
      FALSE
    })
  }

  # Verify rJava loads
  if (has_rjava) {
    result$loads_ok <- tryCatch({
      rJava::.jinit()
      TRUE
    }, error = function(e) {
      if (verbose) warning("rJava::.jinit() failed: ",
                           conditionMessage(e), call. = FALSE)
      FALSE
    })
  }

  if (verbose) {
    if (result$loads_ok) {
      message("[ok] rJava installed and loads successfully.")
    } else if (result$installed) {
      message("[!!] rJava installed but failed to initialize.")
    }
  }

  invisible(result)
}


#' Verify Java setup end-to-end
#'
#' Runs `java -version`, checks rJava, and calls [check_r5r()].
#'
#' @param jdk_home Path to JDK.
#' @param verbose Logical.
#' @return List with `java_ok`, `java_version`, `rjava_ok`, `r5r_ok`.
#' @noRd
.verify_java_setup <- function(jdk_home, verbose = TRUE) {
  result <- list(java_ok = FALSE, java_version = NA_integer_,
                 rjava_ok = FALSE, r5r_ok = FALSE)

  platform <- .detect_platform()
  java_bin <- file.path(jdk_home, "bin",
    if (platform$os == "windows") "java.exe" else "java")

  tryCatch({
    out <- suppressWarnings(
      system2(java_bin, "-version", stdout = TRUE, stderr = TRUE)
    )
    ver <- .parse_java_version(out)
    result$java_version <- ver
    result$java_ok <- !is.na(ver) && ver == 21L
  }, error = function(e) NULL, warning = function(w) NULL)

  result$rjava_ok <- tryCatch({
    requireNamespace("rJava", quietly = TRUE) && {
      rJava::.jinit()
      TRUE
    }
  }, error = function(e) FALSE)

  result$r5r_ok <- tryCatch({
    res <- check_r5r()
    res$ready
  }, error = function(e) FALSE)

  if (verbose) {
    message("\n--- Verification ---")
    if (result$java_ok) {
      message("[ok] Java ", result$java_version, " working at ", jdk_home)
    } else {
      message("[!!] Java check failed (version: ", result$java_version, ")")
    }
    if (result$rjava_ok) {
      message("[ok] rJava loads successfully")
    } else {
      message("[--] rJava not available or failed to load")
    }
    if (result$r5r_ok) {
      message("[ok] r5r is ready")
    } else {
      message("[--] r5r not fully ready")
    }
  }

  invisible(result)
}


# ---- Exported function -------------------------------------------------------

#' Download and configure Java 21 for r5r
#'
#' One-call setup that detects existing Java installations, downloads JDK 21
#' if needed, configures environment variables at both the R session and OS
#' level, optionally installs and configures rJava, and verifies the setup.
#'
#' @param install_dir Character. Where to install Java if downloading.
#'   Default uses [tools::R_user_dir()].
#' @param persist Logical. Write `JAVA_HOME` to `~/.Renviron` and set OS-level
#'   environment variables. Default: TRUE in interactive sessions.
#' @param setup_rjava Logical. Whether to install/configure rJava.
#'   Default: TRUE.
#' @param verbose Logical. Default: TRUE.
#'
#' @details
#' The function performs the following steps:
#' 1. Asks for consent (interactive mode only)
#' 2. Scans for existing Java installations on the system
#' 3. Offers to remove incompatible (non-21) Java versions
#' 4. Uses an existing Java 21 if found, or downloads Adoptium JDK 21
#' 5. Configures `JAVA_HOME` and `PATH` for the current R session
#' 6. Persists `JAVA_HOME` to `~/.Renviron`
#' 7. Sets `JAVA_HOME` at the OS level (Windows User env / shell config)
#' 8. Adds JDK `bin/` to the system PATH
#' 9. Optionally installs and configures rJava
#' 10. Runs a final verification
#'
#' Even if Java 21 is already installed, calling `setup_java()` ensures
#' all configuration (env vars, PATH, rJava) is correctly wired up.
#' The function is idempotent: you can always call it to fix a broken
#' configuration without re-downloading.
#'
#' @return Invisibly, the path to the JDK home directory.
#'
#' @seealso [check_r5r()] to diagnose without changing anything,
#'   [set_java_memory()] to configure JVM heap size.
#'
#' @examples
#' \dontrun{
#' setup_java()
#' setup_java(setup_rjava = FALSE)
#' }
#'
#' @export
setup_java <- function(
    install_dir = file.path(tools::R_user_dir("interpElections", "data"),
                            "java"),
    persist     = interactive(),
    setup_rjava = TRUE,
    verbose     = TRUE,
    .ask_consent = TRUE
) {
  # 1. Ask for consent in interactive mode
  if (.ask_consent && interactive()) {
    consent <- utils::menu(
      c("Yes, handle everything for me",
        "No, I will set up Java myself"),
      title = paste0(
        "setup_java() will scan your system, download Java 21 if needed,\n",
        "and configure all paths. Proceed?"
      )
    )
    if (consent != 1L) {
      stop("Java setup cancelled by user.", call. = FALSE)
    }
  }

  # 2. Discover existing installations
  if (verbose) message("=== Scanning for Java installations ===")
  installations <- .discover_java_installations(verbose = verbose)

  # 3. Check for conflicts and offer to remove incompatible versions
  conflicts <- .check_java_conflicts(installations)
  .handle_java_conflicts(conflicts, verbose = verbose)

  # 4. Use existing Java 21 or download
  if (nrow(conflicts$java21) > 0L) {
    jdk_home <- conflicts$java21$path[1]
    if (verbose) message("\nUsing existing Java 21 at: ", jdk_home)
  } else {
    if (verbose) message("\nNo Java 21 found. Downloading...")

    platform <- .detect_platform()
    ext <- if (platform$os == "windows") "zip" else "tar.gz"

    url <- sprintf(
      paste0("https://api.adoptium.net/v3/binary/latest/21/ga/",
             "%s/%s/jdk/hotspot/normal/eclipse?project=jdk"),
      platform$os, platform$arch
    )

    if (verbose) {
      message("Downloading Adoptium Temurin JDK 21 for ",
              platform$os, "/", platform$arch, "...")
    }

    if (!dir.exists(install_dir)) dir.create(install_dir, recursive = TRUE)

    archive_path <- file.path(install_dir, paste0("jdk-21.", ext))

    download_result <- tryCatch(
      utils::download.file(url, archive_path, mode = "wb", quiet = !verbose),
      error = function(e) e
    )
    if (inherits(download_result, "error")) {
      stop("Download failed: ", conditionMessage(download_result), "\n",
           "You can manually download from: https://adoptium.net/",
           call. = FALSE)
    }

    if (verbose) message("Extracting...")
    jdk_parent <- file.path(install_dir, "jdk-21")
    if (dir.exists(jdk_parent)) unlink(jdk_parent, recursive = TRUE)

    if (ext == "zip") {
      utils::unzip(archive_path, exdir = install_dir)
    } else {
      utils::untar(archive_path, exdir = install_dir)
    }

    extracted <- list.dirs(install_dir, recursive = FALSE, full.names = TRUE)
    jdk_dir <- grep("jdk-21", extracted, value = TRUE)
    jdk_dir <- jdk_dir[jdk_dir != jdk_parent]

    if (length(jdk_dir) == 0L) {
      stop("Could not find extracted JDK directory in: ", install_dir,
           call. = FALSE)
    }
    jdk_dir <- jdk_dir[1]

    file.rename(jdk_dir, jdk_parent)

    # macOS Adoptium tarballs extract with Contents/Home/ structure
    contents_home <- file.path(jdk_parent, "Contents", "Home")
    if (dir.exists(contents_home)) {
      jdk_home <- normalizePath(contents_home, winslash = "/")
    } else {
      jdk_home <- normalizePath(jdk_parent, winslash = "/")
    }

    unlink(archive_path)
    if (verbose) message("Java 21 installed to: ", jdk_home)
  }

  # 6. Activate for current session
  if (verbose) message("\n=== Configuring environment ===")
  .activate_java(jdk_home)
  if (verbose) message("JAVA_HOME and PATH set for current R session.")

  # 7. Persist to ~/.Renviron
  if (persist) {
    .persist_java_home(jdk_home, verbose = verbose)
  }

  # 8. Set OS-level JAVA_HOME
  if (persist) {
    .persist_env_var_os("JAVA_HOME", jdk_home, verbose = verbose)
  }

  # 9. Add bin/ to OS PATH
  if (persist) {
    bin_dir <- file.path(jdk_home, "bin")
    .persist_path_prepend_os(bin_dir, verbose = verbose)
  }

  # 10. RStudio restart warning
  .configure_rstudio(jdk_home, verbose = verbose)

  # 10b. Auto-configure Java heap if not already set (before rJava loads JVM)
  java_mem <- .get_java_memory()
  if (is.null(java_mem$configured)) {
    auto_size <- .recommend_heap_size()
    if (!is.null(auto_size)) {
      set_java_memory(auto_size, persist = persist)
    }
  }

  # 11. rJava setup
  if (setup_rjava) {
    if (verbose) message("\n=== rJava setup ===")
    .setup_rjava(jdk_home, verbose = verbose)
  }

  # 12. Final verification
  if (verbose) {
    .verify_java_setup(jdk_home, verbose = verbose)
  }

  invisible(jdk_home)
}
