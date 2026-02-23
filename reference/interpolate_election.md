# One-step IDW interpolation from source points to census tracts

General-purpose wrapper that combines travel-time computation
(optional), alpha optimization, and interpolation into a single call.
Accepts R objects (sf data frames) directly. If no travel time matrix is
provided, OSM road network data is automatically downloaded and travel
times are computed via r5r.

## Usage

``` r
interpolate_election(
  tracts_sf,
  electoral_sf,
  calib_tracts,
  calib_sources,
  interp_sources = NULL,
  tract_id = "id",
  point_id = "id",
  time_matrix = NULL,
  optim = optim_control(),
  routing = routing_control(),
  min_tract_pop = 1,
  offset = 1,
  keep = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- tracts_sf:

  An `sf` polygon object. Target census tracts.

- electoral_sf:

  An `sf` point object. Source points (e.g., voting locations).

- calib_tracts:

  Character vector. Column names in `tracts_sf` to use as the
  calibration population matrix. Must match `calib_sources` in length.

- calib_sources:

  Character vector. Column names in `electoral_sf` to use as the source
  calibration matrix. Must match `calib_tracts` in length.

- interp_sources:

  Character vector or NULL. Column names in `electoral_sf` to
  interpolate. Default NULL means all numeric columns not in
  `calib_sources`.

- tract_id:

  Character. Name of the ID column in `tracts_sf`. Default: `"id"`.

- point_id:

  Character. Name of the ID column in `electoral_sf`. Default: `"id"`.

- time_matrix:

  Numeric matrix \[n x m\] or NULL. Pre-computed travel times. If
  provided, skips all travel time computation.

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

- min_tract_pop:

  Numeric. Minimum total population in `calib_tracts` for a census tract
  to be included. Default: 1.

- offset:

  Numeric. Travel time offset. Default: 1.

- keep:

  Character vector or NULL. Names of extra intermediate objects to
  include in the result. `weights` and `time_matrix` are always kept.
  Options: `"electoral_sf"`, `"pop_raster"`, `"rep_points"`.

- verbose:

  Logical. Print progress. Default: TRUE.

- ...:

  Advanced arguments. `network_path` (character) for pre-downloaded OSM
  data, `elevation_path` (character) for elevation GeoTIFF. Also accepts
  **deprecated** old-style parameters (`max_epochs`, `alpha_min`,
  `use_gpu`, `mode`, `gtfs_zip_path`, etc.) for backward compatibility.

## Value

A list of class `"interpElections_result"` with components:

- interpolated:

  Numeric matrix \[n x p\]. Interpolated values.

- alpha:

  Decay parameters used.

- tracts_sf:

  `sf` object with interpolated columns joined.

- sources:

  Data frame (no geometry) of source point data.

- optimization:

  `interpElections_optim` object.

- offset:

  Numeric. Offset value used.

- call:

  The matched call.

- tract_id, point_id:

  ID column names.

- interp_cols:

  Character vector. Interpolated column names.

- calib_cols:

  List with `$tracts` and `$sources`.

- weights:

  Weight matrix or NULL (opt-in via `keep`).

- time_matrix:

  Travel time matrix or NULL (opt-in via `keep`).

- electoral_sf:

  `sf` point object or NULL (opt-in via `keep`).

- pop_raster:

  Population raster or NULL (opt-in via `keep`).

- rep_points:

  Representative points or NULL (opt-in via `keep`).

## Details

For Brazilian elections, use
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
which auto-downloads all required data.

## See also

[`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md),
[`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md),
[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md),
[`compute_weight_matrix()`](https://antrologos.github.io/interpElections/reference/compute_weight_matrix.md),
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
for the Brazilian-specific wrapper.

Other wrappers:
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal: sf objects + column names (auto-downloads OSM)
result <- interpolate_election(
  tracts_sf    = census_tracts,
  electoral_sf = voting_stations,
  calib_tracts  = c("pop_18_24", "pop_25_34"),
  calib_sources = c("voters_18_24", "voters_25_34")
)

# With pre-computed travel times (skip r5r)
result <- interpolate_election(
  tracts_sf    = census_tracts,
  electoral_sf = voting_stations,
  calib_tracts  = c("pop_young", "pop_old"),
  calib_sources = c("voters_young", "voters_old"),
  time_matrix  = my_tt_matrix
)

# GPU optimization with custom routing
result <- interpolate_election(
  tracts_sf    = census_tracts,
  electoral_sf = voting_stations,
  calib_tracts  = c("pop_young", "pop_old"),
  calib_sources = c("voters_young", "voters_old"),
  optim   = optim_control(use_gpu = TRUE),
  routing = routing_control(mode = c("WALK", "TRANSIT"))
)
} # }
```
