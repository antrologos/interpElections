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
  point_method = "point_on_surface",
  pop_raster = NULL,
  pop_min_area = 1,
  mode = "WALK",
  max_trip_duration = 300L,
  fill_missing = max_trip_duration,
  n_threads = 4L,
  departure_datetime = NULL,
  verbose = TRUE
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

- point_method:

  Character. Method for computing representative points for census
  tracts. One of `"point_on_surface"` (default), `"centroid"`, or
  `"pop_weighted"`. See
  [`compute_representative_points()`](https://antrologos.github.io/interpElections/reference/compute_representative_points.md)
  for details.

- pop_raster:

  A
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object, a file path to a GeoTIFF, or `NULL`. Population density raster
  for `point_method = "pop_weighted"`. If `NULL`, WorldPop data is
  auto-downloaded. Ignored for other methods.

- pop_min_area:

  Numeric. Minimum tract area in km² for applying the
  population-weighted method. Default: 1.

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

A numeric matrix \[n_tracts x n_points\]. Travel times in minutes. Row
names = census tract IDs, column names = point IDs. Unreachable pairs
are filled with `fill_missing`.

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
  tracts_sf = tracts, points_sf = stations,
  network_path = "path/to/osm_data",
  tract_id = "code_tract", point_id = "id"
)
} # }
```
