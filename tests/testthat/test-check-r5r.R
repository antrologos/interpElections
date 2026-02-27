# --- .parse_java_version() ---------------------------------------------------

test_that(".parse_java_version parses OpenJDK 21 output", {
  lines <- c(
    'openjdk version "21.0.2" 2024-01-16',
    "OpenJDK Runtime Environment (build 21.0.2+13-58)",
    "OpenJDK 64-Bit Server VM (build 21.0.2+13-58, mixed mode, sharing)"
  )
  expect_equal(interpElections:::.parse_java_version(lines), 21L)
})

test_that(".parse_java_version parses Oracle Java 17", {
  lines <- c(
    'java version "17.0.6" 2023-01-17 LTS',
    "Java(TM) SE Runtime Environment (build 17.0.6+9-LTS-190)"
  )
  expect_equal(interpElections:::.parse_java_version(lines), 17L)
})

test_that(".parse_java_version handles legacy 1.8 format", {
  lines <- c('java version "1.8.0_351"')
  expect_equal(interpElections:::.parse_java_version(lines), 8L)
})

test_that(".parse_java_version returns NA for unparseable input", {
  expect_true(is.na(interpElections:::.parse_java_version(character(0))))
  expect_true(is.na(interpElections:::.parse_java_version("no version here")))
  expect_true(is.na(interpElections:::.parse_java_version("")))
})


# --- .detect_platform() ------------------------------------------------------

test_that(".detect_platform returns valid os and arch", {
  platform <- interpElections:::.detect_platform()
  expect_type(platform, "list")
  expect_true(platform$os %in% c("windows", "mac", "linux"))
  expect_true(platform$arch %in% c("x64", "aarch64"))
})


# --- interpElections:::check_r5r() -------------------------------------------------------------

test_that("check_r5r returns expected structure", {
  result <- interpElections:::check_r5r()
  expect_type(result, "list")
  expect_named(result, c(
    "r5r_installed", "java_found", "java_version",
    "java_sufficient", "java_memory", "system_ram", "ready"
  ))
  expect_type(result$r5r_installed, "logical")
  expect_type(result$java_found, "logical")
  expect_type(result$java_sufficient, "logical")
  expect_type(result$ready, "logical")
})

test_that("check_r5r ready flag is consistent with sub-checks", {
  result <- interpElections:::check_r5r()
  if (result$ready) {
    expect_true(result$r5r_installed)
    expect_true(result$java_found)
    expect_true(result$java_sufficient)
  }
})


# --- .get_java_memory() -------------------------------------------------------

test_that(".get_java_memory returns list with expected names", {
  mem <- interpElections:::.get_java_memory()
  expect_type(mem, "list")
  expect_true("configured" %in% names(mem))
  expect_true("system_ram" %in% names(mem))
})

test_that(".get_java_memory detects options(java.parameters)", {
  old <- getOption("java.parameters")
  on.exit(options(java.parameters = old))

  options(java.parameters = "-Xmx4g")
  mem <- interpElections:::.get_java_memory()
  expect_equal(mem$configured, "4g")

  options(java.parameters = c("-Xms512m", "-Xmx8g"))
  mem <- interpElections:::.get_java_memory()
  expect_equal(mem$configured, "8g")
})


# --- interpElections:::set_java_memory() -------------------------------------------------------

test_that("set_java_memory sets java.parameters option", {
  old <- getOption("java.parameters")
  on.exit(options(java.parameters = old))

  interpElections:::set_java_memory("2g", persist = FALSE)
  expect_equal(getOption("java.parameters"), "-Xmx2g")

  interpElections:::set_java_memory("512m", persist = FALSE)
  expect_equal(getOption("java.parameters"), "-Xmx512m")
})

test_that("set_java_memory validates input", {
  expect_error(interpElections:::set_java_memory("abc"), "number followed by")
  expect_error(interpElections:::set_java_memory("4"), "number followed by")
  expect_error(interpElections:::set_java_memory("g4"), "number followed by")
})

test_that("set_java_memory returns previous value invisibly", {
  old <- getOption("java.parameters")
  on.exit(options(java.parameters = old))

  options(java.parameters = "-Xmx1g")
  prev <- interpElections:::set_java_memory("2g", persist = FALSE)
  expect_equal(prev, "-Xmx1g")
})


# --- .persist_renviron_var() --------------------------------------------------

test_that(".persist_renviron_var writes and updates a variable", {
  tmp <- tempfile(fileext = ".Renviron")
  on.exit(unlink(tmp))

  # Patch HOME so .persist_renviron_var writes to tmp
  old_home <- Sys.getenv("HOME")
  Sys.setenv(HOME = dirname(tmp))
  on.exit(Sys.setenv(HOME = old_home), add = TRUE)

  # Rename to match expected filename
  renviron <- file.path(dirname(tmp), ".Renviron")
  on.exit(unlink(renviron), add = TRUE)

  # First write
  interpElections:::.persist_renviron_var("TEST_VAR", "hello")
  lines <- readLines(renviron)
  expect_true(any(grepl("TEST_VAR=hello", lines)))

  # Update
  interpElections:::.persist_renviron_var("TEST_VAR", "world")
  lines <- readLines(renviron)
  expect_true(any(grepl("TEST_VAR=world", lines)))
  expect_equal(sum(grepl("^TEST_VAR", lines)), 1L) # no duplicates
})


# --- .check_java_conflicts() ------------------------------------------------

test_that(".check_java_conflicts splits Java 21 from others", {
  installations <- data.frame(
    path    = c("/opt/java/21", "/opt/java/17", "/opt/java/11"),
    version = c(21L, 17L, 11L),
    vendor  = c("Adoptium", "Oracle", "OpenJDK"),
    source  = c("JAVA_HOME", "registry", "filesystem"),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.check_java_conflicts(installations)

  expect_type(result, "list")
  expect_true("java21" %in% names(result))
  expect_true("others" %in% names(result))
  expect_true("has_conflict" %in% names(result))

  expect_equal(nrow(result$java21), 1L)
  expect_equal(result$java21$version, 21L)
  expect_equal(nrow(result$others), 2L)
  expect_true(all(result$others$version != 21L))
  expect_true(result$has_conflict)
})

test_that(".check_java_conflicts: no conflict when only Java 21", {
  installations <- data.frame(
    path    = c("/opt/java/21a", "/opt/java/21b"),
    version = c(21L, 21L),
    vendor  = c("Adoptium", "Microsoft"),
    source  = c("JAVA_HOME", "registry"),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.check_java_conflicts(installations)
  expect_false(result$has_conflict)
  expect_equal(nrow(result$java21), 2L)
  expect_equal(nrow(result$others), 0L)
})

test_that(".check_java_conflicts: empty input produces no conflict", {
  installations <- data.frame(
    path    = character(0),
    version = integer(0),
    vendor  = character(0),
    source  = character(0),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.check_java_conflicts(installations)
  expect_false(result$has_conflict)
  expect_equal(nrow(result$java21), 0L)
  expect_equal(nrow(result$others), 0L)
})


# --- .persist_shell_var() ---------------------------------------------------

test_that(".persist_shell_var creates file with export line", {
  tmp <- tempfile(fileext = ".bashrc")
  on.exit(unlink(tmp))

  interpElections:::.persist_shell_var(tmp, "MY_VAR", "/opt/java")
  lines <- readLines(tmp)
  expect_true(any(grepl('export MY_VAR="/opt/java"', lines, fixed = TRUE)))
})

test_that(".persist_shell_var updates existing value", {
  tmp <- tempfile(fileext = ".bashrc")
  on.exit(unlink(tmp))

  writeLines(c("# header", 'export MY_VAR="/old/path"', "# footer"), tmp)

  interpElections:::.persist_shell_var(tmp, "MY_VAR", "/new/path")
  lines <- readLines(tmp)
  expect_true(any(grepl('export MY_VAR="/new/path"', lines, fixed = TRUE)))
  expect_false(any(grepl("/old/path", lines, fixed = TRUE)))
  # No duplicates
  expect_equal(sum(grepl("^\\s*export MY_VAR", lines)), 1L)
  # Other lines preserved
  expect_true(any(grepl("# header", lines)))
  expect_true(any(grepl("# footer", lines)))
})

test_that(".persist_shell_var is idempotent", {
  tmp <- tempfile(fileext = ".bashrc")
  on.exit(unlink(tmp))

  interpElections:::.persist_shell_var(tmp, "MY_VAR", "/opt/java")
  lines1 <- readLines(tmp)

  interpElections:::.persist_shell_var(tmp, "MY_VAR", "/opt/java")
  lines2 <- readLines(tmp)

  expect_identical(lines1, lines2)
})

test_that(".persist_shell_var removes duplicate lines", {
  tmp <- tempfile(fileext = ".bashrc")
  on.exit(unlink(tmp))

  writeLines(c(
    'export JAVA_HOME="/path/a"',
    "other stuff",
    'export JAVA_HOME="/path/b"'
  ), tmp)

  interpElections:::.persist_shell_var(tmp, "JAVA_HOME", "/path/c")
  lines <- readLines(tmp)
  expect_equal(sum(grepl("^\\s*export JAVA_HOME", lines)), 1L)
  expect_true(any(grepl('export JAVA_HOME="/path/c"', lines, fixed = TRUE)))
})


# --- .discover_java_installations() -----------------------------------------

test_that(".discover_java_installations returns expected structure", {
  df <- interpElections:::.discover_java_installations(verbose = FALSE)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("path", "version", "vendor", "source") %in% names(df)))
  expect_type(df$path, "character")
  expect_type(df$version, "integer")
  expect_type(df$vendor, "character")
  expect_type(df$source, "character")
})


# --- .configure_rstudio() ---------------------------------------------------

test_that(".configure_rstudio runs without error", {
  expect_silent(
    interpElections:::.configure_rstudio("/fake/jdk", verbose = FALSE)
  )
})


# --- .verify_java_setup() ---------------------------------------------------

test_that(".verify_java_setup returns expected structure", {
  result <- interpElections:::.verify_java_setup("/nonexistent/jdk",
                                                  verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("java_ok", "java_version", "rjava_ok", "r5r_ok") %in%
                    names(result)))
  expect_type(result$java_ok, "logical")
  expect_type(result$rjava_ok, "logical")
  expect_type(result$r5r_ok, "logical")
})


# --- .get_jdk_bundle_path() --------------------------------------------------

test_that(".get_jdk_bundle_path finds .jdk bundle from Contents/Home path", {
  result <- interpElections:::.get_jdk_bundle_path(
    "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home"
  )
  # normalizePath may prepend drive letter on Windows, so check suffix
  expect_true(grepl("temurin-17\\.jdk$", result))
})

test_that(".get_jdk_bundle_path returns .jdk path directly", {
  result <- interpElections:::.get_jdk_bundle_path(
    "/Library/Java/JavaVirtualMachines/oracle-jdk-17.jdk"
  )
  expect_true(grepl("oracle-jdk-17\\.jdk$", result))
})

test_that(".get_jdk_bundle_path returns NULL for non-.jdk paths", {
  expect_null(interpElections:::.get_jdk_bundle_path("/opt/java/jdk-17"))
  expect_null(interpElections:::.get_jdk_bundle_path("/usr/lib/jvm/java-17"))
})


# --- .guess_brew_formula() ---------------------------------------------------

test_that(".guess_brew_formula extracts openjdk@N from Homebrew paths", {
  path <- "/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home"
  expect_equal(interpElections:::.guess_brew_formula(path), "openjdk@17")
})

test_that(".guess_brew_formula extracts plain openjdk", {
  path <- "/usr/local/opt/openjdk/libexec/openjdk.jdk/Contents/Home"
  expect_equal(interpElections:::.guess_brew_formula(path), "openjdk")
})

test_that(".guess_brew_formula returns NULL for unrecognized paths", {
  expect_null(interpElections:::.guess_brew_formula("/some/random/path"))
})


# --- .handle_java_conflicts() ------------------------------------------------

test_that(".handle_java_conflicts returns TRUE when no conflicts", {
  no_conflict <- list(
    java21 = data.frame(
      path = "/opt/java/21", version = 21L,
      vendor = "Adoptium", source = "JAVA_HOME",
      stringsAsFactors = FALSE
    ),
    others = data.frame(
      path = character(0), version = integer(0),
      vendor = character(0), source = character(0),
      stringsAsFactors = FALSE
    ),
    has_conflict = FALSE
  )
  expect_true(
    interpElections:::.handle_java_conflicts(no_conflict, verbose = FALSE)
  )
})

test_that(".handle_java_conflicts errors in non-interactive mode with conflicts", {
  with_conflict <- list(
    java21 = data.frame(
      path = character(0), version = integer(0),
      vendor = character(0), source = character(0),
      stringsAsFactors = FALSE
    ),
    others = data.frame(
      path = "/opt/java/17", version = 17L,
      vendor = "Oracle", source = "JAVA_HOME",
      stringsAsFactors = FALSE
    ),
    has_conflict = TRUE
  )
  # Non-interactive always errors
  expect_error(
    interpElections:::.handle_java_conflicts(with_conflict, verbose = FALSE),
    "Incompatible Java"
  )
})
