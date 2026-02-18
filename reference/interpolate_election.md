# One-step IDW interpolation from source points to census tracts

High-level wrapper that combines travel-time computation (optional),
alpha optimization, and interpolation into a single call. If no travel
time matrix is provided, OSM road network data is automatically
downloaded and travel times are computed via r5r.

## Usage

``` r
interpolate_election(
  tracts_sf,
  electoral_sf,
  tract_id,
  point_id,
  calib_tracts,
  calib_sources,
  interp_sources = NULL,
  time_matrix = NULL,
  network_path = NULL,
  elevation_path = NULL,
  osm_buffer_km = 10,
  min_pop = 1,
  alpha = NULL,
  offset = 1,
  keep = NULL,
  use_gpu = NULL,
  verbose = TRUE,
  ...,
  .step_offset = 0L,
  .step_total = NULL
)
```

## Arguments

- tracts_sf:

  An `sf` polygon object. Target census tracts.

- electoral_sf:

  An `sf` point object. Source points (e.g., voting locations).

- tract_id:

  Character. Name of the ID column in `tracts_sf`.

- point_id:

  Character. Name of the ID column in `electoral_sf`.

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

- time_matrix:

  Numeric matrix \[n x m\] or NULL. Pre-computed travel times. If
  provided, skips all travel time computation.

- network_path:

  Character or NULL. Path to directory with an OSM `.pbf` file. If
  provided (and `time_matrix` is NULL), travel times are computed
  directly without downloading OSM data.

- elevation_path:

  Character or NULL. Path to elevation `.tif` file for r5r routing.

- osm_buffer_km:

  Numeric. Buffer in kilometers to expand the bounding box when
  auto-downloading OSM data. Default: 10.

- min_pop:

  Numeric. Minimum total population in `calib_tracts` for a census tract
  to be included. Default: 1.

- alpha:

  Numeric vector of length n, or NULL. Pre-computed decay parameters. If
  provided, optimization is skipped.

- offset:

  Numeric. Travel time offset. Default: 1.

- keep:

  Character vector or NULL. Names of heavy intermediate objects to
  include in the result. Default NULL (lightweight). Options:
  `"weights"` (column-standardized weight matrix \[n x m\]),
  `"time_matrix"` (travel time matrix \[n x m\]), `"sources_sf"` (source
  points as `sf` object with geometry), `"pop_raster"` (population
  density raster, when `point_method = "pop_weighted"`), `"rep_points"`
  (representative points `sf` object used for routing). These can be
  large for big municipalities. Travel times are cached on disk and can
  be reloaded without keeping them in memory.

- use_gpu:

  Logical or NULL. Passed to
  [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md).

- verbose:

  Logical. Print progress. Default: TRUE.

- ...:

  Additional arguments forwarded to
  [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md),
  [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md),
  and/or
  [`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md).
  Notable forwarded arguments: `point_method` (representative point
  method), `pop_raster` (population density raster), `pop_min_area`
  (area threshold for pop-weighted points). See
  [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)
  for details.

- .step_offset:

  Integer. Internal: offset added to step numbers when called from
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).
  Do not set manually.

- .step_total:

  Integer or NULL. Internal: total step count for unified progress
  display. Do not set manually.

## Value

A list of class `"interpElections_result"` with components:

- interpolated:

  Numeric matrix \[n x p\]. Interpolated values.

- alpha:

  Numeric vector of length n. Decay parameters used.

- tracts_sf:

  `sf` object with interpolated columns joined to census tracts.

- sources:

  Data frame (no geometry) of source point data.

- optimization:

  `interpElections_optim` object, or NULL if `alpha` was pre-supplied.

- offset:

  Numeric. Offset value used.

- call:

  The matched call.

- tract_id:

  Character. Name of the census tract ID column.

- point_id:

  Character. Name of the ID column in sources.

- interp_cols:

  Character vector. Names of interpolated columns.

- calib_cols:

  List with `$tracts` and `$sources` calibration columns.

- weights:

  Numeric matrix \[n x m\] or NULL. Present only when `keep` includes
  `"weights"`.

- time_matrix:

  Numeric matrix \[n x m\] or NULL. Present only when `keep` includes
  `"time_matrix"`.

- sources_sf:

  `sf` point object or NULL. Source points with geometry. Present only
  when `keep` includes `"sources_sf"`.

- pop_raster:

  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or NULL. Population density raster (cropped to municipality). Present
  only when `keep` includes `"pop_raster"` and
  `point_method = "pop_weighted"` was used.

- rep_points:

  `sf` POINT object or NULL. Representative points used for travel time
  routing. Present only when `keep` includes `"rep_points"`.

## See also

[`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md),
[`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md),
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
  tract_id     = "code_tract",
  point_id     = "id",
  calib_tracts  = c("pop_18_24", "pop_25_34"),
  calib_sources = c("voters_18_24", "voters_25_34")
)

# With pre-computed travel times (skip r5r)
result <- interpolate_election(
  tracts_sf    = census_tracts,
  electoral_sf = voting_stations,
  tract_id     = "code_tract",
  point_id     = "id",
  calib_tracts  = c("pop_young", "pop_old"),
  calib_sources = c("voters_young", "voters_old"),
  time_matrix  = my_tt_matrix
)
} # }
```
