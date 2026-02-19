# Get the interpElections cache directory

Returns the path to the directory where interpElections stores cached
downloaded files (TSE data, Hidalgo geocoding, travel time matrices,
etc.). The default location is OS-appropriate (via
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)). A custom
path can be set with
[`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md).

## Usage

``` r
get_interpElections_cache_dir()
```

## Value

Character. Path to the cache directory.

## See also

[`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md),
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)

Other cache:
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md),
[`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md),
[`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md)
