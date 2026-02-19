# Manage the interpElections download cache

Lists or deletes cached files. By default shows a per-category size
breakdown. Use `details = TRUE` to see individual files. Use
`delete_file = "all"` to clear the entire cache.

## Usage

``` r
interpElections_cache(
  list_files = TRUE,
  delete_file = NULL,
  details = FALSE,
  verbose = TRUE
)
```

## Arguments

- list_files:

  Logical. If TRUE (default), prints a per-category summary and returns
  cached file paths.

- delete_file:

  Character or NULL. A pattern to match files for deletion (matched
  against relative paths via
  [`grepl()`](https://rdrr.io/r/base/grep.html)), or `"all"` to delete
  the entire cache. For targeted deletion by category, see
  [`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md).

- details:

  Logical. If TRUE, also prints individual filenames within each
  category. Default: FALSE.

- verbose:

  Logical. Print messages. Default: TRUE.

## Value

Invisibly returns a character vector of cached file paths (before any
deletion).

## See also

[`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md),
[`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md),
[`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md)

Other cache:
[`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md),
[`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md),
[`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Per-category summary
interpElections_cache()

# Detailed listing (every file)
interpElections_cache(details = TRUE)

# Delete files matching a pattern
interpElections_cache(delete_file = "2020")

# Delete everything
interpElections_cache(delete_file = "all")
} # }
```
