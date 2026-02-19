# Package index

## Core Sinkhorn Engine

Build weights, evaluate the objective, and optimize decay parameters.

- [`sinkhorn_balance()`](https://antrologos.github.io/interpElections/reference/sinkhorn_balance.md)
  : Balance a weight matrix using Sinkhorn/IPF iterations
- [`sinkhorn_weights()`](https://antrologos.github.io/interpElections/reference/sinkhorn_weights.md)
  : Compute Sinkhorn-balanced IDW weights
- [`sinkhorn_objective()`](https://antrologos.github.io/interpElections/reference/sinkhorn_objective.md)
  : Compute the per-bracket Sinkhorn-balanced interpolation objective
- [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
  : Find optimal decay parameters (alpha) for spatial interpolation

## High-Level Wrappers

One-call interpolation from sf objects.

- [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
  : One-step IDW interpolation from source points to census tracts
- [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
  : One-step interpolation for Brazilian elections

## Result Methods

S3 methods for inspecting and exporting interpElections_result objects.

- [`summary(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/summary.interpElections_result.md)
  : Summarize an interpElections result
- [`plot(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/plot.interpElections_result.md)
  [`autoplot(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/plot.interpElections_result.md)
  : Plot an interpolated variable as a choropleth map
- [`coef(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/coef.interpElections_result.md)
  : Extract alpha coefficients
- [`residuals(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/residuals.interpElections_result.md)
  : Compute calibration residuals
- [`as.data.frame(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/as.data.frame.interpElections_result.md)
  : Convert result to data frame
- [`plot_interactive()`](https://antrologos.github.io/interpElections/reference/plot_interactive.md)
  : Interactive map of interpolated results

## Brazilian Data Helpers

Download and prepare census, electoral, and geographic data for Brazil.

- [`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md)
  : Prepare Brazilian census population data by age bracket per census
  tract
- [`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md)
  : Prepare census tract shapefiles with population data for a Brazilian
  municipality
- [`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)
  : Prepare Brazilian electoral data at the polling-location level
- [`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
  : Download candidate vote data from TSE
- [`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md)
  : Download turnout/attendance data from TSE
- [`br_download_geocode()`](https://antrologos.github.io/interpElections/reference/br_download_geocode.md)
  : Download geocoded polling station data from TSE
- [`br_download_party_legends()`](https://antrologos.github.io/interpElections/reference/br_download_party_legends.md)
  : Download party legends from TSE
- [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)
  : Compute a travel-time matrix from census tract representative points
  to source points
- [`compute_representative_points()`](https://antrologos.github.io/interpElections/reference/compute_representative_points.md)
  : Compute representative points for census tracts
- [`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md)
  : Download OSM and elevation data for r5r routing
- [`muni_crosswalk`](https://antrologos.github.io/interpElections/reference/muni_crosswalk.md)
  : IBGE-TSE Municipality Code Crosswalk

## Areal Aggregation

Reaggregate interpolated results into larger zones.

- [`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md)
  : Compute area-weighted intersection matrix between two polygon layers
- [`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md)
  : Aggregate data from source zones to target polygons using areal
  weights

## Setup and Diagnostics

Configure GPU, Java, and routing dependencies.

- [`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
  : Install and configure torch with GPU support
- [`check_torch()`](https://antrologos.github.io/interpElections/reference/check_torch.md)
  : Check torch and GPU setup
- [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
  : Enable or disable GPU acceleration
- [`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md)
  : Download and configure Java 21 for r5r
- [`set_java_memory()`](https://antrologos.github.io/interpElections/reference/set_java_memory.md)
  : Set Java heap memory for r5r
- [`check_r5r()`](https://antrologos.github.io/interpElections/reference/check_r5r.md)
  : Check r5r and Java 21 setup
- [`setup_osmium()`](https://antrologos.github.io/interpElections/reference/setup_osmium.md)
  : Install osmium-tool for OSM file clipping

## Cache Management

Inspect and clean downloaded data caches.

- [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md)
  : Get the interpElections cache directory
- [`set_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/set_interpElections_cache_dir.md)
  : Set a custom interpElections cache directory
- [`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
  : Manage the interpElections download cache
- [`interpElections_cache_clean()`](https://antrologos.github.io/interpElections/reference/interpElections_cache_clean.md)
  : Delete cached files by category
