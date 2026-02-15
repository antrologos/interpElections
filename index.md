# interpElections

**Spatial interpolation of electoral data via inverse distance weighting
with per-zone optimized decay parameters.**

interpElections disaggregates data collected at source points (e.g.,
polling locations) into target polygons (e.g., census tracts) using
travel-time-based IDW weights. The decay parameter for each zone is
calibrated against known demographic totals, and the resulting weights
can then interpolate any variable — candidate votes, party totals,
turnout, or custom data.

The package provides:

- An **analytical gradient** for fast L-BFGS-B optimization on CPU and
  **GPU-accelerated** optimization via torch (ADAM on CUDA/MPS).
- End-to-end **Brazilian election helpers** that auto-download census,
  electoral, and road-network data for any municipality.
- A general-purpose API that works with **any point-to-polygon**
  disaggregation problem.

## Installation

interpElections is not yet on CRAN. Install from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("antrologos/interpElections")
```

**Optional dependencies** unlock additional capabilities:

| Dependency                                      | What it enables                                        |
|-------------------------------------------------|--------------------------------------------------------|
| [torch](https://torch.mlverse.org/)             | GPU-accelerated optimization (CUDA, MPS)               |
| [r5r](https://ipeagit.github.io/r5r/) + Java 21 | Travel-time computation via the R5 routing engine      |
| [sf](https://r-spatial.github.io/sf/)           | Spatial data handling (required for wrapper functions) |

Run
[`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md),
[`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md),
or
[`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)
to configure these after installation.

## Quick example

``` r
library(interpElections)

# Bundled example: 20 tracts, 8 source points, 3 demographic groups
tt  <- readRDS(system.file("extdata/example_tt_matrix.rds",
                            package = "interpElections"))
pop <- readRDS(system.file("extdata/example_pop_matrix.rds",
                            package = "interpElections"))
src <- readRDS(system.file("extdata/example_source_matrix.rds",
                            package = "interpElections"))

# Find optimal per-zone decay parameters
result <- optimize_alpha(tt, pop, src, use_gpu = FALSE, verbose = FALSE)
result
#> interpElections optimization result
#>   Method: cpu_lbfgsb_parallel | Convergence: 0 (success)
#>   Objective: ...
#>   Alpha: n=20, mean=..., range=[..., ...]

# Interpolate any source-level data into the 20 tracts
candidate_votes <- matrix(rpois(16, 200), nrow = 8, ncol = 2,
                           dimnames = list(NULL, c("Party_A", "Party_B")))
interpolated <- idw_interpolate(tt, result$alpha, candidate_votes)

# Total conservation: source totals == tract totals
colSums(candidate_votes)
colSums(interpolated)
```

See
[`vignette("introduction")`](https://antrologos.github.io/interpElections/articles/introduction.md)
for a full walkthrough: [Getting Started with
interpElections](https://antrologos.github.io/interpElections/articles/introduction.html).

## Brazilian elections in one call

For Brazilian municipal elections, a single function downloads census
data, electoral results, road networks, computes travel times, and runs
the full interpolation pipeline:

``` r
result <- interpolate_election_br(
  code_muni = 1400100,   # Boa Vista, RR (IBGE 7-digit code)
  year = 2020,           # Municipal election year
  what = "candidates"    # or "parties", "turnout", "demographics"
)

summary(result)
plot(result)
```

The package includes a crosswalk of all 5,710 Brazilian municipalities
(`data(muni_crosswalk)`) mapping IBGE codes to TSE codes and state
abbreviations.

See
[`vignette("brazilian-elections")`](https://antrologos.github.io/interpElections/articles/brazilian-elections.md)
for the complete workflow: [Interpolating Brazilian Electoral
Data](https://antrologos.github.io/interpElections/articles/brazilian-elections.html).

## Custom data (non-Brazilian)

The interpolation engine is not limited to elections. Any
point-to-polygon disaggregation problem works:

``` r
result <- interpolate_election(
  tracts_sf     = my_districts,
  electoral_sf  = my_schools,
  zone_id       = "district_id",
  point_id      = "school_id",
  calib_zones   = c("pop_young", "pop_old"),
  calib_sources = c("enroll_young", "enroll_old"),
  interp_sources = "budget",
  time_matrix   = my_distance_matrix
)
```

See
[`vignette("custom-data")`](https://antrologos.github.io/interpElections/articles/custom-data.md)
for a self-contained example with synthetic data: [Using interpElections
with Your Own
Data](https://antrologos.github.io/interpElections/articles/custom-data.html).

## How it works

For each tract *i* and source point *j*, the raw weight is:

$$W_{ij} = \left( t_{ij} + 1 \right)^{- \alpha_{i}}$$

where *t* is the travel time in minutes and *α* is a per-zone decay
parameter. The weight matrix is then **column-standardized** so that
each source distributes exactly 100% of its data across the tracts —
guaranteeing total conservation.

The optimal *α* vector is found by minimizing the squared error between
interpolated and observed demographic totals:

$$f(\alpha) = \sum\limits_{i,k}({\widehat{V}}_{ik} - P_{ik})^{2}$$

where *V̂* = *W*^(std)(*α*) × *V* (interpolated values) and *P* is the
known population matrix. An **analytical gradient** makes this feasible
for thousands of zones.

See
[`vignette("understanding-the-model")`](https://antrologos.github.io/interpElections/articles/understanding-the-model.md)
for the full mathematical treatment: [Understanding the IDW
Interpolation
Model](https://antrologos.github.io/interpElections/articles/understanding-the-model.html).

## Vignettes

interpElections ships with six vignettes that form a progressive
tutorial:

| \#  | Vignette                                                                                                                | Description                                                                         |
|-----|-------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| 1   | [Getting Started](https://antrologos.github.io/interpElections/articles/introduction.html)                              | Installation, quick example, package overview                                       |
| 2   | [Understanding the Model](https://antrologos.github.io/interpElections/articles/understanding-the-model.html)           | Mathematical foundation: IDW weights, column standardization, calibration, gradient |
| 3   | [Brazilian Elections](https://antrologos.github.io/interpElections/articles/brazilian-elections.html)                   | End-to-end Brazilian workflow: one-call wrapper, step-by-step pipeline, TSE data    |
| 4   | [Custom Data](https://antrologos.github.io/interpElections/articles/custom-data.html)                                   | Using the package with non-Brazilian point-to-polygon problems                      |
| 5   | [Working with Results](https://antrologos.github.io/interpElections/articles/working-with-results.html)                 | S3 methods, residual analysis, validation, areal aggregation                        |
| 6   | [Performance & Configuration](https://antrologos.github.io/interpElections/articles/performance-and-configuration.html) | GPU/CPU setup, optimization tuning, cache management                                |

## Function reference

### Core IDW engine

| Function                                                                                         | Purpose                                                             |
|--------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|
| [`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md)         | Build column-standardized weight matrix from travel times and alpha |
| [`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md) | Apply IDW weights to transfer source data into target zones         |
| [`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md)     | Evaluate sum-of-squared-errors loss at a given alpha                |
| [`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md)       | Analytical gradient of the objective (for L-BFGS-B)                 |
| [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)   | Find optimal per-zone alpha via GPU (ADAM) or CPU (L-BFGS-B)        |

### High-level wrappers

| Function                                                                                                         | Purpose                                              |
|------------------------------------------------------------------------------------------------------------------|------------------------------------------------------|
| [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)       | One-call interpolation from sf points to sf polygons |
| [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md) | Full Brazilian pipeline with auto-download           |

### Brazilian data helpers

| Function                                                                                                     | Purpose                                            |
|--------------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| [`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md) | Census population by age bracket per tract         |
| [`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md)         | Census tract geometries with population data       |
| [`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)   | TSE electoral data aggregated to polling locations |
| [`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)         | Raw candidate vote counts from TSE                 |
| [`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md)     | Turnout and abstention data from TSE               |
| [`br_download_geocode()`](https://antrologos.github.io/interpElections/reference/br_download_geocode.md)     | Polling station coordinates from TSE               |
| [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)   | Travel-time matrix via r5r routing                 |
| [`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)         | OSM road network download for r5r                  |

### Areal aggregation

| Function                                                                                             | Purpose                                                  |
|------------------------------------------------------------------------------------------------------|----------------------------------------------------------|
| [`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md)         | Area-weighted intersection matrix between polygon layers |
| [`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md) | Aggregate interpolated data into larger zones            |

### Setup and diagnostics

| Function                                                                                                                                                                            | Purpose                                      |
|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------|
| [`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md) / [`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md) | Install and verify GPU (torch) support       |
| [`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md) / [`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)       | Install and verify Java/r5r for travel times |
| [`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md)                                                                                          | Install osmium for OSM data clipping         |
| [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)                                                                                                    | Toggle GPU acceleration on/off               |

### Cache management

| Function                                                                                                                     | Purpose                        |
|------------------------------------------------------------------------------------------------------------------------------|--------------------------------|
| [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md) | Show current cache location    |
| [`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md) | Change cache location          |
| [`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)                 | List cached files and sizes    |
| [`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md)     | Delete cached data by category |

## GPU acceleration

For municipalities with more than ~1,000 census tracts, GPU acceleration
provides significant speedups. The package supports NVIDIA GPUs (CUDA)
and Apple Silicon (MPS):

``` r
setup_torch()           # one-time installation
use_gpu(TRUE)           # enable globally
result <- optimize_alpha(tt, pop, src)  # automatically uses GPU
```

See
[`vignette("performance-and-configuration")`](https://antrologos.github.io/interpElections/articles/performance-and-configuration.md)
for details: [Performance Tuning and
Configuration](https://antrologos.github.io/interpElections/articles/performance-and-configuration.html).

## Citation

If you use interpElections in academic work, please cite:

> Barbosa, R. & Gelape, L. (2025). *interpElections: Spatial
> Interpolation of Electoral Data via Inverse Distance Weighting*. R
> package version 0.1.0. <https://github.com/antrologos/interpElections>

## License

MIT
