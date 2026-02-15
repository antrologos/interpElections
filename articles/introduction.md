# Getting Started with interpElections

## What is interpElections?

Many types of data are collected at geographic locations that do not
match the spatial units we want to analyze. In elections, for example,
votes are tallied at **polling locations** (points), but researchers
often need estimates at the level of **census tracts** (polygons). Each
polling location serves voters from multiple tracts, and each tract’s
voters may travel to different polling locations. The relationship is
many-to-many, so there is no simple lookup table to convert between the
two.

Standard areal interpolation methods (which redistribute polygon data
based on area overlap) do not apply here because the source data lives
at points, not polygons. Instead, interpElections uses an **inverse
distance weighting** (IDW) approach that accounts for the spatial
proximity between census tracts and polling locations.

In one sentence: interpElections computes travel-time-based IDW weights
with **per-zone optimized decay parameters**, calibrated so that the
interpolated values match known demographic totals at the tract level.

## Installation

interpElections is not yet on CRAN. Install from GitHub or from a local
copy:

``` r
# From GitHub
remotes::install_github("antrologos/interpElections")

# Or from a local directory
devtools::install_local("interpElections")
```

The core interpolation functions have no heavy dependencies beyond base
R. Optional packages unlock additional capabilities:

- **torch** – GPU-accelerated optimization (ADAM optimizer on CUDA or
  MPS)
- **r5r** + Java (\>= 21) – travel time computation via the R5 routing
  engine
- **sf** – spatial data handling (required for the wrapper functions)

## Quick Example

The package ships with small example datasets: a 20-tract x 8-source
travel time matrix, population counts for 3 demographic groups in 20
tracts, and voter counts for the same 3 groups at 8 source points.

``` r
library(interpElections)
#> interpElections: some optional dependencies are missing:
#> - No OSM clipping tool found (osmium/osmconvert)
#>   Install with: interpElections::setup_osmium()
#> These are needed for the full interpolation pipeline.

tt  <- readRDS(system.file("extdata/example_tt_matrix.rds",
                            package = "interpElections"))
pop <- readRDS(system.file("extdata/example_pop_matrix.rds",
                            package = "interpElections"))
src <- readRDS(system.file("extdata/example_source_matrix.rds",
                            package = "interpElections"))

dim(tt)   # 20 tracts x 8 sources
#> [1] 20  8
dim(pop)  # 20 tracts x 3 groups
#> [1] 20  3
dim(src)  # 8 sources x 3 groups
#> [1] 8 3
```

Run the optimization to find the best decay parameter (alpha) for each
tract:

``` r
result <- optimize_alpha(tt, pop, src, use_gpu = FALSE, verbose = FALSE)
print(result)
#> interpElections optimization result
#>   Method:      cpu_lbfgsb
#>   Objective:   2150.0468
#>   Convergence: 0
#>   Alpha range: [0.000, 0.072]
#>   N zones:     20
#>   Elapsed:     0.4 secs
```

The result contains the optimal alpha vector, the final objective value
(sum of squared errors), and metadata about convergence.

## Interpolating Variables

Once the alpha values are calibrated, they can be used to interpolate
**any** variable from the source points into the target zones. Here we
create a synthetic candidate vote matrix and interpolate it.

``` r
# Synthetic vote counts: 8 sources x 2 candidates
set.seed(123)
candidate_votes <- matrix(
  rpois(8 * 2, lambda = 200),
  nrow = 8, ncol = 2,
  dimnames = list(colnames(tt), c("Candidate_A", "Candidate_B"))
)
candidate_votes
#>        Candidate_A Candidate_B
#> poll_1         192         217
#> poll_2         216         205
#> poll_3         176         205
#> poll_4         201         201
#> poll_5         224         192
#> poll_6         206         218
#> poll_7         182         211
#> poll_8         175         199
```

``` r
# Interpolate into 20 tracts
interpolated <- idw_interpolate(tt, result$alpha, candidate_votes)
head(interpolated)
#>         Candidate_A Candidate_B
#> tract_1    80.26303    84.22678
#> tract_2    78.54542    82.31001
#> tract_3    82.68261    86.76879
#> tract_4    88.57890    92.89904
#> tract_5    70.48203    73.85247
#> tract_6    74.96871    78.58623
```

A key property of column-standardized IDW is **total conservation**: the
column sums of the interpolated matrix equal the column sums of the
source matrix. Each source distributes 100% of its data across the
target zones.

``` r
colSums(candidate_votes)  # totals at sources
#> Candidate_A Candidate_B 
#>        1572        1648
colSums(interpolated)     # totals at tracts (should match)
#> Candidate_A Candidate_B 
#>        1572        1648
```

## Package Overview

interpElections provides functions at three levels of abstraction:

| Category       | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | Purpose                                                                                                    |
|----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| Core IDW       | [`idw_weights()`](https://antrologos.github.io/interpElections/reference/idw_weights.md), [`idw_interpolate()`](https://antrologos.github.io/interpElections/reference/idw_interpolate.md), [`idw_objective()`](https://antrologos.github.io/interpElections/reference/idw_objective.md), [`idw_gradient()`](https://antrologos.github.io/interpElections/reference/idw_gradient.md)                                                                                                                                                                                                                                                                                                                                                                                 | Low-level building blocks for computing weights, interpolating data, and evaluating the objective/gradient |
| Optimization   | [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | Finds optimal per-zone decay parameters (CPU or GPU)                                                       |
| Wrappers       | [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md), [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | High-level one-call workflows that handle data prep, travel times, optimization, and interpolation         |
| Brazilian data | [`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md), [`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md), [`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md), [`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md), [`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md), [`br_download_geocode()`](https://antrologos.github.io/interpElections/reference/br_download_geocode.md), [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md) | Download and prepare Brazilian census and electoral data                                                   |
| Areal          | [`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md), [`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | Secondary aggregation from tracts into arbitrary polygons                                                  |
| Setup          | [`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md), [`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md), [`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md), [`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md), [`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)                                                                                                                                                                                                                                                                                                         | Install and diagnose optional dependencies                                                                 |
| GPU            | [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Toggle GPU acceleration globally                                                                           |
| Cache          | [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md), [`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md), [`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md), [`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md)                                                                                                                                                                                                                                                                   | Manage persistent download cache                                                                           |

For the mathematical details behind IDW interpolation and the
optimization procedure, see
[`vignette("understanding-the-model")`](https://antrologos.github.io/interpElections/articles/understanding-the-model.md).

## Three Ways to Use the Package

### 1. One-call wrappers

The simplest approach. For Brazilian elections,
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
handles everything – downloading census data, electoral data, tract
geometries, road networks, computing travel times, optimizing alpha, and
interpolating:

``` r
result <- interpolate_election_br(code_muni = 3550308, year = 2020)
```

For non-Brazilian data or custom setups,
[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
provides the same workflow but takes user-supplied `sf` objects:

``` r
result <- interpolate_election(
  tracts_sf = my_zones, electoral_sf = my_points,
  zone_id = "id", point_id = "id",
  calib_zones = c("pop_young", "pop_old"),
  calib_sources = c("voters_young", "voters_old")
)
```

### 2. Step-by-step pipeline

For more control, call each step individually:

``` r
# Prepare inputs
pop   <- br_prepare_population(code_muni = 3550308, year = 2022)
tract <- br_prepare_tracts(code_muni = 3550308, pop_data = pop)
elec  <- br_prepare_electoral(code_muni_ibge = "3550308", ...)

# Compute travel times
tt <- compute_travel_times(zones_sf = tract, points_sf = elec, ...)

# Optimize and interpolate
opt   <- optimize_alpha(tt, pop_matrix, source_matrix)
votes <- idw_interpolate(tt, opt$alpha, candidate_matrix)
```

### 3. Core functions only

For maximum flexibility or integration into custom pipelines, use the
low-level functions directly:

``` r
# Build weights from a travel time matrix and alpha vector
W <- idw_weights(tt, alpha)

# Evaluate the objective and gradient at a given alpha
f <- idw_objective(alpha, tt, pop, src)
g <- idw_gradient(alpha, tt, pop, src)

# Interpolate by matrix multiplication
interpolated <- W %*% source_data
```

This level is useful when you want to plug the IDW engine into your own
optimization loop, compare different alpha values, or integrate with
other spatial methods.
