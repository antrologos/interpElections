# Download OSM and elevation data for r5r routing

Downloads the OpenStreetMap road network (`.pbf` file) and optionally
satellite elevation data needed to build an r5r routing network. The
output directory can be passed directly to
[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)
as `network_path`.

## Usage

``` r
download_r5r_data(
  area_sf,
  output_dir,
  osm = TRUE,
  elevation = FALSE,
  osm_provider = "geofabrik",
  osm_url = NULL,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- area_sf:

  An `sf` object defining the area of interest. Its bounding box is used
  to select the appropriate OSM extract and to clip the downloaded file.

- output_dir:

  Character. Directory to save downloaded files. Created if it does not
  exist.

- osm:

  Logical. Download OpenStreetMap road network. Default: TRUE.

- elevation:

  Logical. Download elevation raster for more accurate walking/cycling
  routing on hilly terrain. Default: FALSE.

- osm_provider:

  Character. OSM extract provider for `osmextract`. Default:
  `"geofabrik"`. Alternatives: `"bbbike"`, `"openstreetmap_fr"`.

- osm_url:

  Character or NULL. Direct URL to the OSM `.pbf` extract. When
  provided, skips the automatic
  [`osmextract::oe_match()`](https://docs.ropensci.org/osmextract/reference/oe_match.html)
  step and downloads from this URL directly. This is useful when
  `oe_match()` selects an overly broad extract (e.g., country-level
  instead of state-level). Default: NULL (auto-detect via `oe_match()`).

- force:

  Logical. Re-download even if files already exist. Default: FALSE.

- verbose:

  Logical. Default: TRUE.

## Value

A list with paths to downloaded files:

- osm_pbf:

  Character. Path to the OSM `.pbf` file (or NULL if `osm = FALSE`).

- elevation_tif:

  Character. Path to the elevation `.tif` file (or NULL if
  `elevation = FALSE`).

- output_dir:

  Character. The output directory path.

## Details

After downloading the provider extract (which may cover a whole state),
the `.pbf` file is clipped to the bounding box of `area_sf` using
`osmium` or `osmconvert` (if available on the system). This avoids r5r's
geographic extent limit (~975,000 km2).

Requires the `osmextract` package for OSM downloads and optionally the
`elevatr` package for elevation data. Both are suggested dependencies of
this package.

For clipping large OSM extracts, `osmium-tool` (recommended) or
`osmconvert` must be installed. If neither is found, the function will
interactively offer to install one via
[`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md)
before proceeding. In non-interactive mode, it stops with an actionable
error message. All dependency checks (R packages and clipping tools) run
at the start, before any downloads begin.

## See also

[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)
to use the downloaded data,
[`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md)
to install the required clipping tool.

Other spatial:
[`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md),
[`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md),
[`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)
