# interpElections

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/antrologos/interpElections)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Spatial interpolation of electoral data via **column-normalized inverse
distance weighting**. Disaggregates point-level data (e.g., polling
station vote counts) into polygon-level estimates (e.g., census tracts)
using travel-time-based IDW with per-zone decay parameters calibrated
against demographic totals. Column normalization ensures source
conservation (each source distributes exactly 100% of its data).

## Installation

```r
# install.packages("remotes")
remotes::install_github("antrologos/interpElections")
```

Optional: `setup_torch()` for GPU optimization, `setup_java()` for r5r
travel-time routing.

## Quick start

```r
library(interpElections)

result <- interpolate_election_br(3550308, 2022)
plot(result)
```

Three lines: downloads census data, electoral results, and road
networks; computes travel times; optimizes per-tract decay; interpolates
all variables into census tracts.

## Advanced usage

Control objects group tuning parameters for optimization and routing:

```r
result <- interpolate_election_br(3550308, 2022,
  optim = optim_control(use_gpu = TRUE, max_epochs = 1000),
  routing = routing_control(mode = c("WALK", "TRANSIT"),
                            gtfs_zip_path = "sptrans.zip")
)
```

## Custom data

```r
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

$$W_{ij} = (t_{ij} + 1)^{-\alpha_i}$$

Weights are **column-normalized** so that column sums equal 1 (each
source distributes exactly 100% of its data). The optimal alpha is found
by minimizing the Poisson deviance between interpolated and census
demographics via **torch ADAM** with a log-barrier penalty.

## Vignettes

| Vignette | Description |
|---|---|
| [Get Started](https://antrologos.github.io/interpElections/articles/get-started.html) | Brazilian examples (Varginha + Igrejinha), general case, core functions |
| [Methodology](https://antrologos.github.io/interpElections/articles/methodology.html) | Full pipeline walkthrough with equations, visualizations, and real data |
| [Working with Results](https://antrologos.github.io/interpElections/articles/working-with-results.html) | S3 methods, plotting, validation, areal aggregation |

## Function reference

| Category | Functions |
|---|---|
| **Interpolation** | `interpolate_election_br()`, `interpolate_election()`, `reinterpolate()`, `optimize_alpha()`, `compute_weight_matrix()`, `compute_travel_times()` |
| **Control** | `optim_control()`, `routing_control()` |
| **Results** | `summary()`, `plot()`, `autoplot()`, `coef()`, `residuals()`, `as.data.frame()`, `plot_interactive()` |
| **Spatial** | `areal_weights()`, `areal_interpolate()` |
| **Setup** | `setup_torch()`, `use_gpu()`, `setup_java()`, `interpElections_cache()` |

## GPU acceleration

```r
setup_torch()     # one-time installation
use_gpu(TRUE)     # enable globally

# Or per-call via control objects:
result <- interpolate_election_br(3550308, 2022,
  optim = optim_control(use_gpu = TRUE))
```

GPU (CUDA/MPS) is recommended for municipalities with >1,000 census
tracts. CPU works for all problem sizes.

## Citation

> Barbosa, R. & Gelape, L. (2025). *interpElections: Spatial
> Interpolation of Electoral Data via Inverse Distance Weighting*.
> R package version 0.1.0.
> https://github.com/antrologos/interpElections

## License

MIT
