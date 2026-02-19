# Download and configure Java 21 for r5r

Downloads the Adoptium Temurin JDK 21 for the current platform, extracts
it to a local directory, and configures the R session so that r5r can
find it. Optionally persists the configuration to `~/.Renviron`.

## Usage

``` r
setup_java(
  install_dir = file.path(tools::R_user_dir("interpElections", "data"), "java"),
  persist = interactive(),
  verbose = TRUE
)
```

## Arguments

- install_dir:

  Character. Where to install Java. Default uses
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) so Java
  lives alongside other R user data.

- persist:

  Logical. Write `JAVA_HOME` to `~/.Renviron` so it persists across R
  sessions. Default: TRUE in interactive sessions.

- verbose:

  Logical. Default: TRUE.

## Value

Invisibly, the path to the installed JDK.

## Details

The JDK is downloaded from the Eclipse Adoptium project
(`https://adoptium.net`). The archive is extracted to
`install_dir/jdk-21` and `JAVA_HOME` is set for the current session.

When `persist = TRUE`, the function appends (or updates) a `JAVA_HOME`
line in `~/.Renviron` so future R sessions find Java automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_java()
check_r5r()
} # }
```
