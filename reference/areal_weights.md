# Compute area-weighted intersection matrix between two polygon layers

Builds a weight matrix that maps values from source polygons (e.g.,
census tracts) to target polygons (e.g., custom analysis zones) based on
the fraction of area shared between them.

## Usage

``` r
areal_weights(target_sf, source_sf, target_id = "id", source_id = "id")
```

## Arguments

- target_sf:

  An `sf` object with polygon geometries. The target zones to aggregate
  into.

- source_sf:

  An `sf` object with polygon geometries. The source zones (e.g., census
  tracts with interpolated data).

- target_id:

  Character. Name of the ID column in `target_sf`.

- source_id:

  Character. Name of the ID column in `source_sf`.

## Value

A numeric matrix \[n_target x n_source\]. Each column is standardized so
that the weights from all target zones sum to 1 (or 0 if the source zone
has no overlap). Row names = target IDs, column names = source IDs.

## Details

Requires the `sf` package. Geometries are made valid with
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
before computing intersections.

## See also

[`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md)
to apply the weights.

Other spatial:
[`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md),
[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md),
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)
