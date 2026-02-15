# Prepare census tract shapefiles with population data for a Brazilian municipality

Downloads census tract geometries via `geobr`, joins population data
from
[`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md),
optionally clips to an urban area mask, and removes unpopulated tracts.

## Usage

``` r
br_prepare_tracts(
  code_muni,
  pop_data,
  remove_unpopulated = TRUE,
  clip_sf = NULL,
  year = 2010,
  crs = "EPSG:5880",
  verbose = TRUE
)
```

## Arguments

- code_muni:

  Numeric or character. IBGE municipality code.

- pop_data:

  Data frame. Output of
  [`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md).
  Must contain `code_tract` and population bracket columns.

- remove_unpopulated:

  Logical. Remove tracts where `pop_total == 0`. Default: TRUE.

- clip_sf:

  Optional `sf` polygon object. If provided, census tracts are clipped
  to this geometry (e.g., to remove non-urban areas like parks, forests,
  and water bodies). Population is proportionally adjusted based on the
  fraction of area retained.

- year:

  Integer. Census tract geometry year. Default: 2010.

- crs:

  Character or integer. CRS for the output. Default: SIRGAS 2000 /
  Brazil Polyconic (EPSG:5880).

- verbose:

  Logical. Print progress messages. Default: TRUE.

## Value

An `sf` object with census tract polygons and columns: `code_tract`, all
`pop_*` bracket columns from `pop_data`, and `pop_total` (sum of all
brackets).

## Details

Requires the `geobr`, `sf`, and `dplyr` packages.

## See also

[`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md)
to generate `pop_data`.

Other Brazil helpers:
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md),
[`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pop <- br_prepare_population("3550308", year = 2010)
tracts <- br_prepare_tracts("3550308", pop)
} # }
```
