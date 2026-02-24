# Download and configure Java 21 for r5r

One-call setup that detects existing Java installations, downloads JDK
21 if needed, configures environment variables at both the R session and
OS level, optionally installs and configures rJava, and verifies the
setup.

## Usage

``` r
setup_java(
  install_dir = file.path(tools::R_user_dir("interpElections", "data"), "java"),
  persist = interactive(),
  setup_rjava = TRUE,
  verbose = TRUE
)
```

## Arguments

- install_dir:

  Character. Where to install Java if downloading. Default uses
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html).

- persist:

  Logical. Write `JAVA_HOME` to `~/.Renviron` and set OS-level
  environment variables. Default: TRUE in interactive sessions.

- setup_rjava:

  Logical. Whether to install/configure rJava. Default: TRUE.

- verbose:

  Logical. Default: TRUE.

## Value

Invisibly, the path to the JDK home directory.

## Details

The function performs the following steps:

1.  Scans for existing Java installations on the system

2.  Warns about non-Java-21 versions that may conflict

3.  Uses an existing Java 21 if found, or downloads Adoptium JDK 21

4.  Configures `JAVA_HOME` and `PATH` for the current R session

5.  Persists `JAVA_HOME` to `~/.Renviron`

6.  Sets `JAVA_HOME` at the OS level (Windows User env / shell config)

7.  Adds JDK `bin/` to the system PATH

8.  Optionally installs and configures rJava

9.  Runs a final verification

Even if Java 21 is already installed, calling `setup_java()` ensures all
configuration (env vars, PATH, rJava) is correctly wired up. The
function is idempotent: you can always call it to fix a broken
configuration without re-downloading.

## See also

[`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)
to diagnose without changing anything,
[`set_java_memory()`](https://antrologos.github.io/interpElections/reference/set_java_memory.md)
to configure JVM heap size.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_java()
setup_java(setup_rjava = FALSE)
} # }
```
