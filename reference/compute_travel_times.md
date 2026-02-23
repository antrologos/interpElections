# Compute a travel-time matrix from census tract representative points to source points

Builds a travel-time matrix using the r5r routing engine. Computes
travel times from representative points of target census tracts to
geolocated source points (e.g., polling locations).

## Usage

``` r
compute_travel_times(
  tracts_sf,
  points_sf,
  network_path,
  tract_id = "id",
  point_id = "id",
  routing = routing_control(),
  verbose = TRUE,
  ...
)
```

## Arguments

- tracts_sf:

  An `sf` object with polygon geometries. Target census tracts.

- points_sf:

  An `sf` object with point geometries. Source points.

- network_path:

  Character. Path to the directory containing the OSM `.pbf` file for
  building the r5r network.

- tract_id:

  Character. Name of the ID column in `tracts_sf`. Default: `"id"`.

- point_id:

  Character. Name of the ID column in `points_sf`. Default: `"id"`.

- routing:

  A
  [`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md)
  object with routing parameters (mode, point_method, max_trip_duration,
  n_threads, gtfs_zip_path, departure_datetime, pop_raster,
  min_area_for_pop_weight, osm_buffer_km, fill_missing). Default:
  [`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md).

- verbose:

  Logical. Default: TRUE.

- ...:

  **Deprecated**. Old-style individual parameters (`point_method`,
  `pop_raster`, `min_area_for_pop_weight`, `mode`, `max_trip_duration`,
  `fill_missing`, `n_threads`, `departure_datetime`, `gtfs_zip_path`)
  are still accepted via `...` for backward compatibility but will be
  removed in a future release. Use `routing = routing_control(...)`
  instead.

## Value

A numeric matrix \[n_tracts x n_points\]. Travel times in minutes. Row
names = census tract IDs, column names = point IDs. Unreachable pairs
are filled with `fill_missing`.

## Details

Requires the `r5r` and `sf` packages. r5r requires exactly Java/JDK 21.
Use
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)
to obtain the OSM data needed for `network_path`.

## See also

[`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md)
for routing parameters,
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)
to download OSM and elevation data.

Other spatial:
[`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md),
[`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md),
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tt <- compute_travel_times(
  tracts_sf = tracts, points_sf = stations,
  network_path = "path/to/osm_data",
  tract_id = "code_tract", point_id = "id"
)

# Transit mode with GTFS
tt <- compute_travel_times(
  tracts_sf = tracts, points_sf = stations,
  network_path = "path/to/osm_data",
  routing = routing_control(
    mode = c("WALK", "TRANSIT"),
    gtfs_zip_path = "sptrans.zip",
    departure_datetime = as.POSIXct("2022-10-02 10:00:00")
  )
)
} # }
```
