# interpElections

Spatial interpolation of electoral data via **Sinkhorn-balanced inverse
distance weighting**. Disaggregates point-level data (e.g., polling
station vote counts) into polygon-level estimates (e.g., census tracts)
using travel-time-based IDW with per-zone decay parameters calibrated
against demographic totals. Sinkhorn balancing enforces population
conservation and source conservation simultaneously.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("antrologos/interpElections")
```

Optional:
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
for GPU optimization,
[`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md)
for r5r travel-time routing,
[`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md)
for OSM data clipping.

## Quick example: Brazilian election

``` r
library(interpElections)

result <- interpolate_election_br("Varginha", year = 2022,
  cargo = "presidente", what = c("candidates", "turnout"))

summary(result)
plot(result, variable = "Lula")
```

Three lines: downloads census data, electoral results, and road
networks; computes travel times; optimizes per-tract decay; interpolates
all variables into census tracts.

## Quick example: Custom data

``` r
result <- interpolate_election(
  tracts_sf     = my_zones,
  electoral_sf  = my_sources,
  tract_id      = "zone_id",
  point_id      = "source_id",
  calib_tracts  = c("pop_young", "pop_old"),
  calib_sources = c("count_young", "count_old"),
  time_matrix   = my_travel_times
)
```

## How it works

For each tract *i* and source *j*, the IDW kernel is:

$$W_{ij} = \left( t_{ij} + 1 \right)^{- \alpha_{i}}$$

**Sinkhorn balancing** iteratively scales rows and columns so that row
sums are proportional to population and column sums equal 1 (each source
distributes exactly 100% of its data). The optimal alpha is found by
minimizing the squared error between interpolated and census
demographics via **torch ADAM** with log-domain Sinkhorn
differentiation.

## Vignettes

| Vignette                                                                                                | Description                                                             |
|---------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------|
| [Get Started](https://antrologos.github.io/interpElections/articles/get-started.html)                   | Brazilian examples (Varginha + Igrejinha), general case, core functions |
| [Methodology](https://antrologos.github.io/interpElections/articles/methodology.html)                   | Full pipeline walkthrough with equations, visualizations, and real data |
| [Working with Results](https://antrologos.github.io/interpElections/articles/working-with-results.html) | S3 methods, plotting, validation, areal aggregation                     |

## Function reference

| Category              | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Core Engine**       | [`sinkhorn_balance()`](https://antrologos.github.io/interpElections/reference/sinkhorn_balance.md), [`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md), [`sinkhorn_objective()`](https://antrologos.github.io/interpElections/reference/sinkhorn_objective.md), [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)                                                                                                                                 |
| **Wrappers**          | [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md), [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)                                                                                                                                                                                                                                                                                                                   |
| **Results**           | [`summary()`](https://rdrr.io/r/base/summary.html), [`plot()`](https://rdrr.io/r/graphics/plot.default.html), [`coef()`](https://rdrr.io/r/stats/coef.html), [`residuals()`](https://rdrr.io/r/stats/residuals.html), [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), [`plot_interactive()`](https://antrologos.github.io/interpElections/reference/plot_interactive.md)                                                                                                                                                       |
| **Brazilian Data**    | [`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md), [`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md), [`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md), [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)                                                                                                     |
| **Areal Aggregation** | [`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md), [`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md)                                                                                                                                                                                                                                                                                                                                             |
| **Setup**             | [`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md), [`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md), [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md), [`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md), [`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md), [`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md) |
| **Cache**             | [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md), [`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md), [`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md)                                                                                                                                                                           |

## GPU acceleration

``` r
setup_torch()     # one-time installation
use_gpu(TRUE)     # enable globally
```

GPU (CUDA/MPS) is recommended for municipalities with \>1,000 census
tracts. CPU works for all problem sizes.

## Citation

> Barbosa, R. & Gelape, L. (2025). *interpElections: Spatial
> Interpolation of Electoral Data via Sinkhorn-Balanced Inverse Distance
> Weighting*. R package version 0.1.0.
> <https://github.com/antrologos/interpElections>

## License

MIT
