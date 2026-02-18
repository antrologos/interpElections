# Delete cached files by category

Convenience function for clearing specific categories of cached data.
More discoverable than
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
with `delete_file`.

## Usage

``` r
interpElections_cache_clean(
  category = c("all", "downloads", "processed", "networks", "travel_times", "pop_raster",
    "votes", "turnout", "geocode", "profile", "hidalgo", "osm", "electoral", "tracts",
    "r5r"),
  verbose = TRUE
)
```

## Arguments

- category:

  Character. Which category to clear. One of:

  `"all"`

  :   Delete the entire cache

  `"downloads"`

  :   All raw downloads (votes, turnout, geocode, profile, hidalgo, osm)

  `"processed"`

  :   All processed/cached results (electoral, tracts)

  `"networks"`

  :   All r5r network indices

  `"travel_times"`

  :   Cached travel time matrices

  `"votes"`

  :   TSE vote data ZIPs

  `"turnout"`

  :   TSE turnout data ZIPs

  `"geocode"`

  :   TSE polling station location ZIPs

  `"profile"`

  :   TSE voter profile ZIPs

  `"hidalgo"`

  :   Hidalgo geocoding fallback data

  `"osm"`

  :   OpenStreetMap road network extracts

  `"electoral"`

  :   Processed electoral data (br_prepare_electoral output)

  `"tracts"`

  :   Cached census tract geometries

  `"r5r"`

  :   r5r routing network indices

- verbose:

  Logical. Print messages. Default: TRUE.

## Value

Invisibly returns the path(s) that were deleted.

## See also

[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)

Other cache:
[`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md),
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md),
[`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear only processed electoral data (forces re-computation next run)
interpElections_cache_clean("electoral")

# Clear all raw downloads
interpElections_cache_clean("downloads")

# Clear everything
interpElections_cache_clean("all")
} # }
```
