# Tabular residual summary

Computes per-bracket, per-tract, and overall residual summary
statistics.

## Usage

``` r
residual_summary(result, type = c("raw", "pearson", "deviance"))
```

## Arguments

- result:

  An `interpElections_result` object.

- type:

  Residual type: `"raw"`, `"pearson"`, or `"deviance"`.

## Value

A list with three elements:

- per_bracket:

  Data frame with columns: bracket, mean, rmse, max_abs, pct_gt_2sd.

- per_tract:

  Data frame with columns: tract_id, mean, rmse, worst_bracket.

- overall:

  List with rmse, mean_bias, total_deviance.
