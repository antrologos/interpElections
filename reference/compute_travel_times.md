# Compute a travel-time matrix from zone centroids to source points

Builds a travel-time matrix using the r5r routing engine. Computes
travel times from the centroids of target zones (e.g., census tracts) to
geolocated source points (e.g., polling locations).

## Usage

``` r
compute_travel_times(
  zones_sf,
  points_sf,
  network_path,
  zone_id = "id",
  point_id = "id",
  mode = "WALK",
  max_trip_duration = 300L,
  fill_missing = max_trip_duration,
  n_threads = 4L,
  departure_datetime = NULL,
  verbose = TRUE
)
```

## Arguments

- zones_sf:

  An `sf` object with polygon geometries. Target zones.

- points_sf:

  An `sf` object with point geometries. Source points.

- network_path:

  Character. Path to the directory containing the OSM `.pbf` file for
  building the r5r network.

- zone_id:

  Character. Name of the ID column in `zones_sf`. Default: `"id"`.

- point_id:

  Character. Name of the ID column in `points_sf`. Default: `"id"`.

- mode:

  Character. Routing mode. Default: `"WALK"`.

- max_trip_duration:

  Integer. Maximum trip duration in minutes. Default: 300.

- fill_missing:

  Numeric. Value to fill for unreachable origin-destination pairs.
  Default: same as `max_trip_duration`.

- n_threads:

  Integer. Number of r5r routing threads. Default: 4.

- departure_datetime:

  POSIXct or NULL. Departure time for transit-based routing. Required
  when `mode` includes transit components. Default: NULL (ignored for
  WALK/BICYCLE modes).

- verbose:

  Logical. Default: TRUE.

## Value

A numeric matrix \[n_zones x n_points\]. Travel times in minutes. Row
names = zone IDs, column names = point IDs. Unreachable pairs are filled
with `fill_missing`.

## Details

Requires the `r5r` and `sf` packages. r5r requires Java/JDK 21+. Use
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)
to obtain the OSM data needed for `network_path`.

## See also

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
  zones_sf = tracts, points_sf = stations,
  network_path = "path/to/osm_data",
  zone_id = "code_tract", point_id = "id"
)
} # }
```
