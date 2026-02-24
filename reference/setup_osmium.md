# Install osmium-tool for OSM file clipping

Attempts to install `osmium-tool` (or `osmconvert` as a fallback) using
the system package manager. This is required to clip large state-level
OSM extracts down to municipality-level bounding boxes for r5r routing.

## Usage

``` r
setup_osmium(method = NULL, verbose = TRUE)
```

## Arguments

- method:

  Character or NULL. Installation method to use. If NULL (default),
  auto-detects. Options:

  - `"conda"`: conda install (all platforms)

  - `"brew"`: Homebrew (macOS)

  - `"apt"`: apt-get (Debian/Ubuntu)

  - `"dnf"`: dnf (Fedora/RHEL)

  - `"download"`: Download osmconvert binary (Windows only)

- verbose:

  Logical. Default: TRUE.

## Value

Invisibly, the path to the installed tool, or NULL if installation
failed.

## Details

The preferred tool is `osmium-tool`, which is faster and more reliable.
`osmconvert` is used as a fallback when osmium is not available.

On Windows, the `"download"` method fetches a pre-compiled
`osmconvert.exe` binary and saves it to the interpElections cache. This
does not require conda or any other package manager.

## See also

[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)
which uses osmium for clipping.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_osmium()
setup_osmium(method = "conda")
setup_osmium(method = "download")  # Windows only
} # }
```
