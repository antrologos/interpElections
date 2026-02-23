# Delete cached files by category

Internal workhorse for `interpElections_cache("clean", ...)`. Use
`interpElections_cache("clean", category = "votes")` instead of calling
this directly.

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

  Character. Which category to clear.

- verbose:

  Logical. Print messages. Default: TRUE.

## Value

Invisibly returns the path(s) that were deleted.

## See also

[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
