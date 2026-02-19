# Working with Results

This vignette is a reference guide for working with
`interpElections_result` objects after interpolation. It covers S3
methods, plotting options, residual analysis, validation, data export,
and areal aggregation. Examples use pre-computed results from **Niteroi
(RJ)** (~500 tracts, GPU) and **Belo Horizonte (MG)** (~4,000 tracts,
GPU).

## Loading Results

``` r
# Run the pipeline (these were computed with GPU)
result_nit <- interpolate_election_br(
  "Niteroi", year = 2022, cargo = "presidente",
  what = c("candidates", "parties", "turnout", "demographics"),
  keep = c("weights", "sources_sf", "time_matrix"),
  use_gpu = TRUE
)

result_bh <- interpolate_election_br(
  "Belo Horizonte", year = 2022, cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = c("weights", "sources_sf"),
  use_gpu = TRUE
)
```

## Print and Summary

The [`print()`](https://rdrr.io/r/base/print.html) method gives a
compact overview:

``` r
result_nit
```

    interpElections result -- Brazilian election
      Municipality: Niteroi (RJ)
      IBGE: 3303302 | TSE: 58718 | Election: 2022 | Census: 2010
      Census tracts: ~500 | Sources: ~100

      Variables: ~50
        Candidates:    13 (CAND_13, CAND_22, ...)
        Parties:       ~10 (PARTY_PT, PARTY_PL, ...)
        Turnout:       3 (QT_COMPARECIMENTO, QT_APTOS, QT_ABSTENCOES)
        Demographics:  ~15 (GENERO_FEMININO, EDUC_SUP_COMP, ...)
        Calibration:   7 (votantes_18_20, ...)

The [`summary()`](https://rdrr.io/r/base/summary.html) method adds
per-variable statistics grouped by type:

``` r
summary(result_nit)
```

The dictionary groups columns by type (candidate, party, turnout,
demographics, calibration) with metadata like candidate name, party
abbreviation, and ballot number:

``` r
View(result_nit$dictionary)
```

## Plotting

### Single variable

``` r
plot(result_nit, variable = "Lula")
```

![](figures/wr-nit-lula.png)

Variables can be referenced by:

- **Column name**: `"CAND_13"`, `"PARTY_PT"`, `"QT_COMPARECIMENTO"`
- **Ballot number**: `13`, `22`
- **Candidate name** (substring, case-insensitive): `"Lula"`,
  `"Bolsonaro"`
- **Party abbreviation**: `"PT"`, `"PL"`

### Absolute counts

``` r
plot(result_nit, variable = "Lula", type = "absolute")
```

![](figures/wr-nit-lula-abs.png)

### Faceted comparison

``` r
plot(result_nit, variable = c("Lula", "Bolsonaro"), type = "pct_tract")
```

![](figures/wr-nit-faceted.png)

### Plot types

The `type` parameter controls the quantity mapped:

| Type             | Description                                  |
|------------------|----------------------------------------------|
| `"pct_tract"`    | % of total tract votes (default)             |
| `"absolute"`     | Raw interpolated count                       |
| `"pct_muni"`     | % of municipality total                      |
| `"pct_valid"`    | % of valid votes (excludes blank/null)       |
| `"pct_eligible"` | % of eligible voters (requires turnout data) |
| `"density"`      | Count per km²                                |

### Break methods

The `breaks` parameter controls the color scale:

- `"quantile"` (default): equal-count bins
- `"continuous"`: smooth gradient
- `"jenks"`: natural breaks (requires classInt package)
- Custom numeric vector: `breaks = c(0, 20, 40, 60, 80, 100)`

``` r
plot(result_nit, variable = "Lula", breaks = "continuous")
plot(result_nit, variable = "Lula", breaks = "jenks")
```

### Additional options

``` r
# Overlay polling stations
plot(result_nit, variable = "Lula", show_sources = TRUE)

# Zoom into a region (lon/lat bounding box)
plot(result_nit, variable = "Lula",
     limits = c(-43.15, -43.05, -22.92, -22.86))

# Custom palette
plot(result_nit, variable = "Lula", palette = "viridis")
plot(result_nit, variable = "Lula", palette = "Spectral")

# Composable with ggplot2
library(ggplot2)
plot(result_nit, variable = "Lula") + theme_dark()
```

![](figures/wr-nit-sources.png)

### Interactive maps

``` r
# Opens in browser with hover tooltips and zoom
plot_interactive(result_nit, variable = "Lula")
```

### Large-scale example (Belo Horizonte)

``` r
plot(result_bh, variable = "Lula")
```

![](figures/wr-bh-lula.png)

## Extracting Alpha

The [`coef()`](https://rdrr.io/r/stats/coef.html) method returns the
optimized decay parameters:

``` r
alpha <- coef(result_nit)
summary(alpha)
```

![](figures/wr-nit-alpha-hist.png)

Map the alpha values spatially to see the urban/peripheral gradient:

``` r
result_nit$tracts_sf$alpha <- coef(result_nit)
ggplot(result_nit$tracts_sf) +
  geom_sf(aes(fill = alpha), color = "white", linewidth = 0.05) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1) +
  theme_void()
```

![](figures/wr-nit-alpha-map.png)

**Interpretation**: Low alpha (blue) = weight spread across many
stations (dense urban areas). High alpha (red) = weight concentrated on
the nearest station (periphery).

## Residual Analysis

The [`residuals()`](https://rdrr.io/r/stats/residuals.html) method
computes the calibration residuals (fitted minus observed) for each
tract and age bracket. Requires `keep = "weights"` or
`keep = "time_matrix"`.

``` r
resid <- residuals(result_nit)
str(resid)
#> num [1:~500, 1:7] ...

# Per-bracket summary
colMeans(resid)                        # mean residual per bracket
sqrt(colMeans(resid^2))                # RMSE per bracket
```

![](figures/wr-nit-residuals.png)

Residuals should be centered near zero. Large residuals may indicate
boundary effects (voters crossing municipality borders), data quality
issues, or census/election year mismatches.

## Validation Checklist

Five checks to apply to any result:

``` r
# 1. Total conservation: interpolated totals match source totals
colSums(result_nit$interpolated)[1:3]
colSums(result_nit$sources[, result_nit$interp_cols[1:3]])

# 2. Residual magnitude
resid <- residuals(result_nit)
sqrt(mean(resid^2))   # overall RMSE

# 3. Alpha distribution (no extreme piling at bounds)
summary(coef(result_nit))

# 4. Non-negative values
all(result_nit$interpolated >= -1e-10)

# 5. Convergence
result_nit$optimization$convergence   # 0 = success
```

## Exporting Results

``` r
# Plain data frame (no geometry)
df <- as.data.frame(result_nit)
write.csv(df, "niteroi_2022.csv", row.names = FALSE)

# GeoPackage with geometry (for GIS)
sf::st_write(result_nit$tracts_sf, "niteroi_2022.gpkg")

# Column metadata
result_nit$dictionary

# Source-level data (without geometry)
head(result_nit$sources)
```

## Areal Aggregation: From Tracts to Neighborhoods

Tract-level results can be reaggregated into larger zones
(neighborhoods, administrative regions, custom polygons) using
[`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md)
and
[`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md).

**Example: Belo Horizonte tracts to neighborhoods**

``` r
library(sf)
library(geobr)

# Download BH neighborhood boundaries
bairros <- read_neighborhood(year = 2010)
bairros <- bairros[bairros$code_muni == 3106200, ]

# Compute area-weighted intersection matrix
W_areal <- areal_weights(
  target_sf = bairros,
  source_sf = result_bh$tracts_sf,
  target_id = "code_neighborhood",
  source_id = "code_tract"
)

# Aggregate interpolated data
vote_data <- result_bh$interpolated
aggregated <- areal_interpolate(vote_data, W_areal)

# Conservation check
colSums(aggregated)[1:3]
colSums(vote_data)[1:3]   # should match
```

The weight matrix `W_areal` is `[n_neighborhoods x n_tracts]`. Each
column sums to 1 (each tract’s data is distributed proportionally to the
area overlap).
[`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md)
is simply `W_areal %*% data`.

This approach preserves total conservation: the neighborhood-level
totals equal the tract-level totals, which equal the original
municipality totals.

## The `keep` Parameter

Control which intermediate objects are retained in the result:

| Value           | Object               | Use case                                                                     |
|-----------------|----------------------|------------------------------------------------------------------------------|
| `"weights"`     | `result$weights`     | [`residuals()`](https://rdrr.io/r/stats/residuals.html), manual reweighting  |
| `"time_matrix"` | `result$time_matrix` | [`residuals()`](https://rdrr.io/r/stats/residuals.html) (alternative), reuse |
| `"sources_sf"`  | `result$sources_sf`  | `plot(..., show_sources = TRUE)`                                             |
| `"pop_raster"`  | `result$pop_raster`  | Inspect population density raster                                            |
| `"rep_points"`  | `result$rep_points`  | Inspect representative points                                                |

Default: `NULL` (lightweight, nothing kept). Recommended:

``` r
result <- interpolate_election_br("Niteroi", year = 2022,
  keep = c("weights", "sources_sf"))
```

This enables residual analysis and source point overlays without
excessive memory use.
