# Routing control parameters

Creates a control object for travel-time computation in
[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md).
All parameters have sensible defaults; override only what you need.

## Usage

``` r
routing_control(
  mode = "WALK",
  point_method = "pop_weighted",
  min_area_for_pop_weight = 1,
  max_trip_duration = 300L,
  n_threads = 4L,
  gtfs_zip_path = NULL,
  departure_datetime = NULL,
  pop_raster = NULL,
  osm_buffer_km = 10,
  fill_missing = NULL
)
```

## Arguments

- mode:

  Character. Routing mode, using r5r convention: `"WALK"` (default),
  `"BICYCLE"`, `"CAR"`, `"TRANSIT"`, or combinations like
  `c("WALK", "TRANSIT")`. Passed directly to
  [`r5r::travel_time_matrix()`](https://ipeagit.github.io/r5r/reference/travel_time_matrix.html).

- point_method:

  Character. Method for computing representative points of census
  tracts. One of `"pop_weighted"` (default, uses WorldPop raster),
  `"point_on_surface"`, or `"centroid"`. See
  [`compute_representative_points()`](https://antrologos.github.io/interpElections/reference/compute_representative_points.md).

- min_area_for_pop_weight:

  Numeric. Minimum tract area in km2 for applying the pop_weighted
  method. Smaller tracts fall back to point_on_surface. Default: 1.

- max_trip_duration:

  Integer. Maximum trip duration in minutes. Also used as fill value for
  unreachable pairs (unless `fill_missing` is set). Default: 300 (5
  hours walking).

- n_threads:

  Integer. Number of parallel threads for the r5r routing engine.
  Default: 4.

- gtfs_zip_path:

  Character or NULL. Path to a GTFS `.zip` file for transit routing.
  Copied into the network directory so r5r can auto-detect it. Default:
  NULL.

- departure_datetime:

  POSIXct or NULL. Departure time for transit-based routing. Required
  when `mode` includes transit. Default: NULL.

- pop_raster:

  A
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  file path to GeoTIFF, or NULL. Population raster for
  `point_method = "pop_weighted"`. If NULL, WorldPop data is
  auto-downloaded. Default: NULL.

- osm_buffer_km:

  Numeric. Buffer in km around the combined bounding box of tracts and
  points when clipping OSM data. Default: 10.

- fill_missing:

  Numeric or NULL. Value for unreachable OD pairs. NULL (default) uses
  `max_trip_duration`.

## Value

A list of class `"interpElections_routing_control"` with one element per
parameter.

## See also

[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md),
[`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)

## Examples

``` r
# Default settings (walking, 5h max)
routing_control()
#> interpElections routing control:
#>   mode: WALK 
#>   point_method: pop_weighted 
#>   min_area_for_pop_weight: 1 
#>   max_trip_duration: 300 min
#>   n_threads: 4 
#>   gtfs_zip_path: NULL 
#>   departure_datetime: NULL 
#>   pop_raster: NULL (auto-download) 
#>   osm_buffer_km: 10 
#>   fill_missing: Inf 

# Transit mode with GTFS
routing_control(
  mode = c("WALK", "TRANSIT"),
  gtfs_zip_path = "sptrans.zip",
  departure_datetime = as.POSIXct("2022-10-02 10:00:00")
)
#> interpElections routing control:
#>   mode: WALK;TRANSIT 
#>   point_method: pop_weighted 
#>   min_area_for_pop_weight: 1 
#>   max_trip_duration: 300 min
#>   n_threads: 4 
#>   gtfs_zip_path: sptrans.zip 
#>   departure_datetime: 2022-10-02 10:00:00 
#>   pop_raster: NULL (auto-download) 
#>   osm_buffer_km: 10 
#>   fill_missing: Inf 

# Bicycle with centroid-based points
routing_control(mode = "BICYCLE", point_method = "centroid")
#> interpElections routing control:
#>   mode: BICYCLE 
#>   point_method: centroid 
#>   min_area_for_pop_weight: 1 
#>   max_trip_duration: 300 min
#>   n_threads: 4 
#>   gtfs_zip_path: NULL 
#>   departure_datetime: NULL 
#>   pop_raster: NULL (auto-download) 
#>   osm_buffer_km: 10 
#>   fill_missing: Inf 
```
