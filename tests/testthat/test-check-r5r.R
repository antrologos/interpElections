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


# --- check_r5r() -------------------------------------------------------------

test_that("check_r5r returns expected structure", {
  result <- check_r5r()
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
  result <- check_r5r()
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


# --- set_java_memory() -------------------------------------------------------

test_that("set_java_memory sets java.parameters option", {
  old <- getOption("java.parameters")
  on.exit(options(java.parameters = old))

  set_java_memory("2g", persist = FALSE)
  expect_equal(getOption("java.parameters"), "-Xmx2g")

  set_java_memory("512m", persist = FALSE)
  expect_equal(getOption("java.parameters"), "-Xmx512m")
})

test_that("set_java_memory validates input", {
  expect_error(set_java_memory("abc"), "number followed by")
  expect_error(set_java_memory("4"), "number followed by")
  expect_error(set_java_memory("g4"), "number followed by")
})

test_that("set_java_memory returns previous value invisibly", {
  old <- getOption("java.parameters")
  on.exit(options(java.parameters = old))

  options(java.parameters = "-Xmx1g")
  prev <- set_java_memory("2g", persist = FALSE)
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
