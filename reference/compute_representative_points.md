# Compute representative points for census tracts

Computes a single representative point for each census tract polygon.
Three methods are available: geometric point-on-surface (default),
centroid, or population-density-weighted using WorldPop raster data.

## Usage

``` r
compute_representative_points(
  tracts_sf,
  method = c("point_on_surface", "centroid", "pop_weighted"),
  pop_raster = NULL,
  pop_min_area = 1,
  tract_id = "id",
  verbose = TRUE
)
```

## Arguments

- tracts_sf:

  An `sf` polygon object containing census tract geometries.

- method:

  Character. Method for computing representative points:

  `"point_on_surface"`

  :   (Default) Uses
      [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).
      Guarantees the point falls inside the polygon, unlike centroids
      which can fall outside concave shapes.

  `"centroid"`

  :   Uses
      [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html).
      Classic geometric centroid. May fall outside concave polygons.

  `"pop_weighted"`

  :   Uses a population density raster (WorldPop Constrained 2020 by
      default) to find the most populated cell within each tract. Only
      applied to tracts with area \>= `pop_min_area`; smaller tracts use
      `point_on_surface`. Requires the `terra` package.

- pop_raster:

  A
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object, a file path to a GeoTIFF, or `NULL`. Population density raster
  for `method = "pop_weighted"`. If `NULL` (default), the WorldPop
  Brazil Constrained 2020 raster (~48 MB) is downloaded automatically
  and cached. Ignored for other methods.

- pop_min_area:

  Numeric. Minimum tract area in km² for applying the
  population-weighted method. Tracts smaller than this threshold use
  `point_on_surface` instead. Default: 1 (km²). Only used when
  `method = "pop_weighted"`.

- tract_id:

  Character. Name of the ID column in `tracts_sf`. Default: `"id"`.

- verbose:

  Logical. Print progress messages? Default: `TRUE`.

## Value

An `sf` POINT object in WGS84 (EPSG:4326) with one row per tract,
preserving the `tract_id` column. Carries an attribute `"point_method"`
recording which method was used.

## Examples

``` r
if (FALSE) { # \dontrun{
tracts <- br_prepare_tracts(code_muni = 3170701)
tracts$id <- tracts$code_tract

# Default: point on surface
pts <- compute_representative_points(tracts)

# Population-weighted for large tracts
pts_pop <- compute_representative_points(tracts, method = "pop_weighted")
} # }
```
