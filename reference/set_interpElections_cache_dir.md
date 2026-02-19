# Set a custom interpElections cache directory

Persists a custom cache directory path across R sessions. When `path` is
`NULL`, resets to the default OS-appropriate location.

## Usage

``` r
set_interpElections_cache_dir(path = NULL, verbose = TRUE)
```

## Arguments

- path:

  Character or NULL. Directory path for cached files. Created if it does
  not exist. `NULL` resets to the default.

- verbose:

  Logical. Print confirmation. Default: TRUE.

## Value

Invisibly returns the cache directory path.

## See also

[`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md),
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)

Other cache:
[`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md),
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md),
[`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md)
