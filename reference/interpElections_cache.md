# Manage the interpElections download cache

Unified interface for cache management. The `action` parameter selects
one of four operations: list contents (`"list"`, the default), clean by
category (`"clean"`), query the cache directory path (`"dir"`), or set a
custom cache directory (`"set_dir"`).

## Usage

``` r
interpElections_cache(
  action = c("list", "clean", "dir", "set_dir"),
  category = NULL,
  path = NULL,
  delete_file = NULL,
  details = FALSE,
  verbose = TRUE
)
```

## Arguments

- action:

  Character. One of:

  `"list"`

  :   (Default) Print a per-category size breakdown and return cached
      file paths. Supports `delete_file` and `details`.

  `"clean"`

  :   Delete cached files by `category`.

  `"dir"`

  :   Return the current cache directory path.

  `"set_dir"`

  :   Set a custom cache directory via `path`. Pass `path = NULL` to
      reset to the OS default.

- category:

  Character (for `action = "clean"`). Which category to clear. One of:
  `"all"`, `"downloads"`, `"processed"`, `"networks"`, `"travel_times"`,
  `"pop_raster"`, `"votes"`, `"turnout"`, `"geocode"`, `"profile"`,
  `"hidalgo"`, `"osm"`, `"electoral"`, `"tracts"`, `"r5r"`. Default:
  `"all"`.

- path:

  Character or NULL (for `action = "set_dir"`). Directory path for
  cached files. Created if it does not exist. `NULL` resets to the
  default.

- delete_file:

  Character or NULL (for `action = "list"`). A pattern to match files
  for deletion (via [`grepl()`](https://rdrr.io/r/base/grep.html)), or
  `"all"` to delete the entire cache.

- details:

  Logical (for `action = "list"`). If TRUE, also prints individual
  filenames within each category. Default: FALSE.

- verbose:

  Logical. Print messages. Default: TRUE.

## Value

Depends on `action`:

- `"list"`:

  Invisibly returns a character vector of cached file paths (before any
  deletion).

- `"clean"`:

  Invisibly returns the path(s) that were deleted.

- `"dir"`:

  Character string: path to the cache directory.

- `"set_dir"`:

  Invisibly returns the (new) cache directory path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Per-category summary (default action)
interpElections_cache()

# Detailed listing (every file)
interpElections_cache(details = TRUE)

# Delete files matching a pattern
interpElections_cache(delete_file = "2020")

# Delete everything
interpElections_cache(delete_file = "all")

# Get cache directory
interpElections_cache("dir")

# Set custom cache directory
interpElections_cache("set_dir", path = "/tmp/my_cache")

# Reset to default directory
interpElections_cache("set_dir", path = NULL)

# Clean by category
interpElections_cache("clean", category = "votes")

# Clean everything
interpElections_cache("clean", category = "all")
} # }
```
