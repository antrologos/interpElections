# Get the interpElections cache directory

Returns the path to the directory where interpElections stores cached
downloaded files (TSE data, Hidalgo geocoding, travel time matrices,
etc.). The default location is OS-appropriate (via
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

## Usage

``` r
get_interpElections_cache_dir()
```

## Value

Character. Path to the cache directory.

## See also

[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
