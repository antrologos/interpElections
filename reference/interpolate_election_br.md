# Interpolate Brazilian election data into census tracts

Downloads census data, electoral results, tract geometries, and road
networks automatically. Only requires a municipality identifier and an
election year. Internally calls
[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
after preparing all inputs.

## Usage

``` r
interpolate_election_br(
  municipality,
  year,
  cargo = NULL,
  turno = 1L,
  what = "candidates",
  candidates = NULL,
  parties = NULL,
  uf = NULL,
  census_year = NULL,
  clip_tracts_sf = NULL,
  min_tract_pop = 1,
  time_matrix = NULL,
  optim = optim_control(),
  routing = routing_control(),
  offset = 1,
  force = FALSE,
  keep = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- municipality:

  Numeric or character. Either a 7-digit IBGE municipality code (e.g.,
  `3550308` for Sao Paulo) or a municipality **name** (e.g.,
  `"Sao Paulo"`, `"São Paulo"`). Name matching is
  case/accent-insensitive. Use `uf` to disambiguate if the name exists
  in multiple states.

- year:

  Integer. Election year (even integer).

- cargo:

  Integer, character, or NULL. Electoral office(s). Accepts aliases like
  `"presidente"`, `"vereador"` or TSE codes. NULL = all offices for that
  year. See Details.

- turno:

  Integer. Election round: 1 (default) or 2 (runoff).

- what:

  Character vector. What to interpolate: `"candidates"` (default),
  `"parties"`, `"turnout"`, `"demographics"`. Multiple values can be
  combined.

- candidates:

  Character or numeric vector, or NULL. Filter candidates by ballot
  number or name substring. Default: all.

- parties:

  Character vector or NULL. Filter parties by TSE abbreviation (e.g.,
  `c("PT", "PL")`). Default: all.

- uf:

  Two-letter state abbreviation for disambiguation when `municipality`
  is a name. Default: NULL.

- census_year:

  Integer or NULL. Census year (2000, 2010, 2022). NULL = auto-selected
  from election year.

- clip_tracts_sf:

  `sf` polygon or NULL. Clip tracts to this geometry before
  interpolation.

- min_tract_pop:

  Minimum total population in a tract to include it. Tracts below this
  are dropped. Default: 1.

- time_matrix:

  Pre-computed travel time matrix \[n x m\], or NULL. If provided, skips
  routing (no r5r, no OSM download).

- optim:

  An
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)
  object with optimization parameters. Default:
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md).

- routing:

  A
  [`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md)
  object with routing parameters. Default:
  [`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md).
  When `gtfs_zip_path` is set but `departure_datetime` is NULL, the
  departure is auto-set to election day at 10 AM.

- offset:

  Numeric. Travel time offset. Default: 1.

- force:

  Re-download all cached data. Default: FALSE.

- keep:

  Character vector of extra objects to include in the result. `weights`
  and `time_matrix` are always kept. Options: `"electoral_sf"`,
  `"pop_raster"`, `"rep_points"`.

- verbose:

  Print progress messages. Default: TRUE.

- ...:

  Advanced arguments. `network_path` (character), `cache` (logical,
  default TRUE), `osm_provider` (character), `comparecimento_path`,
  `votacao_path`, `geocode_path`, `interp_sources`. Also accepts
  **deprecated** old-style parameters for backward compatibility.

## Value

An `interpElections_result` list. Key fields: `$tracts_sf` (sf with
interpolated columns), `$interpolated` (matrix \[n x p\]), `$alpha`
(decay parameters), `$optimization` (optimizer details).

## Quick start

    result <- interpolate_election_br(3550308, 2022)
    plot(result)

## Re-interpolation

Use
[`reinterpolate()`](https://antrologos.github.io/interpElections/reference/reinterpolate.md)
to re-run with different candidates/parties without recomputing travel
times:

    result <- interpolate_election_br(3550308, 2022)
    result2 <- reinterpolate(result, what = "parties")

## Advanced tuning

Use
[`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)
to adjust optimization parameters (epochs, learning rate, GPU, etc.) and
[`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md)
for travel-time computation (mode, GTFS, representative points, etc.).

## See also

[`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md),
[`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md),
[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
for the general-purpose wrapper,
[`reinterpolate()`](https://antrologos.github.io/interpElections/reference/reinterpolate.md)
for quick re-runs.

Other wrappers:
[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
