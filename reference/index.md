# Package index

## Interpolation

High-level wrappers and core optimization engine.

- [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
  : Interpolate Brazilian election data into census tracts
- [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
  : One-step IDW interpolation from source points to census tracts
- [`reinterpolate()`](https://antrologos.github.io/interpElections/reference/reinterpolate.md)
  : Re-interpolate with different electoral variables
- [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md)
  : Find optimal decay parameters (alpha) for spatial interpolation
- [`compute_weight_matrix()`](https://antrologos.github.io/interpElections/reference/compute_weight_matrix.md)
  : Compute the weight matrix
- [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md)
  : Compute a travel-time matrix from census tract representative points
  to source points

## Control Objects

Group tuning parameters for optimization and routing.

- [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)
  : Optimization control parameters
- [`routing_control()`](https://antrologos.github.io/interpElections/reference/routing_control.md)
  : Routing control parameters

## Results

S3 methods for inspecting, plotting, and exporting results.

- [`summary(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/summary.interpElections_result.md)
  : Summarize an interpElections result
- [`plot(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/plot.interpElections_result.md)
  [`autoplot(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/plot.interpElections_result.md)
  : Plot an interpolated variable as a choropleth map
- [`plot_interactive()`](https://antrologos.github.io/interpElections/reference/plot_interactive.md)
  : Interactive map of interpolated results
- [`coef(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/coef.interpElections_result.md)
  : Extract alpha coefficients
- [`residuals(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/residuals.interpElections_result.md)
  : Compute calibration residuals
- [`as.data.frame(`*`<interpElections_result>`*`)`](https://antrologos.github.io/interpElections/reference/as.data.frame.interpElections_result.md)
  : Convert result to data frame

## Diagnostics

Model validation, residual analysis, and weight matrix diagnostics.

- [`diagnostics()`](https://antrologos.github.io/interpElections/reference/diagnostics.md)
  : Run diagnostic checks on an interpolation result
- [`print(`*`<interpElections_diagnostics>`*`)`](https://antrologos.github.io/interpElections/reference/print.interpElections_diagnostics.md)
  : Print method for interpElections_diagnostics
- [`plot_convergence()`](https://antrologos.github.io/interpElections/reference/plot_convergence.md)
  : Plot optimization convergence diagnostics
- [`plot_alpha()`](https://antrologos.github.io/interpElections/reference/plot_alpha.md)
  : Plot alpha decay parameter diagnostics
- [`plot_residuals()`](https://antrologos.github.io/interpElections/reference/plot_residuals.md)
  : Plot calibration residual diagnostics
- [`residual_summary()`](https://antrologos.github.io/interpElections/reference/residual_summary.md)
  : Tabular residual summary
- [`plot_weights()`](https://antrologos.github.io/interpElections/reference/plot_weights.md)
  : Plot weight matrix diagnostics
- [`plot_connections()`](https://antrologos.github.io/interpElections/reference/plot_connections.md)
  : Plot tract-station connections
- [`weight_summary()`](https://antrologos.github.io/interpElections/reference/weight_summary.md)
  : Summarize weight matrix properties per tract
- [`plot_travel_times()`](https://antrologos.github.io/interpElections/reference/plot_travel_times.md)
  : Plot travel time matrix diagnostics
- [`plot_residual_autocorrelation()`](https://antrologos.github.io/interpElections/reference/plot_residual_autocorrelation.md)
  : Test residuals for spatial autocorrelation
- [`plot_moran()`](https://antrologos.github.io/interpElections/reference/plot_moran.md)
  : Moran's I and LISA cluster map for an interpolated variable
- [`compare_baselines()`](https://antrologos.github.io/interpElections/reference/compare_baselines.md)
  : Compare interpolation against baseline methods
- [`leave_one_out()`](https://antrologos.github.io/interpElections/reference/leave_one_out.md)
  : Leave-one-out pseudo cross-validation
- [`plot_income()`](https://antrologos.github.io/interpElections/reference/plot_income.md)
  : Plot income-vote ecological correlation
- [`plot_turnout_rates()`](https://antrologos.github.io/interpElections/reference/plot_turnout_rates.md)
  : Plot implied turnout rates by demographic bracket
- [`plot_ecological()`](https://antrologos.github.io/interpElections/reference/plot_ecological.md)
  : Cross-election consistency check

## Spatial Utilities

Reaggregate interpolated results into larger zones.

- [`areal_weights()`](https://antrologos.github.io/interpElections/reference/areal_weights.md)
  : Compute area-weighted intersection matrix between two polygon layers
- [`areal_interpolate()`](https://antrologos.github.io/interpElections/reference/areal_interpolate.md)
  : Aggregate data from source zones to target polygons using areal
  weights

## Setup

Configure GPU, Java, and cache.

- [`setup_java()`](https://antrologos.github.io/interpElections/reference/setup_java.md)
  : Download and configure Java 21 for r5r
- [`setup_torch()`](https://antrologos.github.io/interpElections/reference/setup_torch.md)
  : Install and configure torch with GPU support
- [`use_gpu()`](https://antrologos.github.io/interpElections/reference/use_gpu.md)
  : Enable or disable GPU acceleration
- [`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
  : Manage the interpElections download cache

## Data

Bundled reference datasets.

- [`muni_crosswalk`](https://antrologos.github.io/interpElections/reference/muni_crosswalk.md)
  : IBGE-TSE Municipality Code Crosswalk
- [`br_election_dates`](https://antrologos.github.io/interpElections/reference/br_election_dates.md)
  : Brazilian election dates
