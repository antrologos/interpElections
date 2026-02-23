# Plot implied turnout rates by demographic bracket

Computes implied turnout rate per tract and bracket as the ratio of
interpolated voters to census population. Rates exceeding 100% indicate
boundary effects or registration mismatches.

## Usage

``` r
plot_turnout_rates(
  result,
  type = c("bracket", "map", "histogram"),
  summary_fn = "mean",
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- type:

  Character. `"bracket"` (boxplot), `"map"`, or `"histogram"`. Default:
  `"bracket"`.

- summary_fn:

  Summary for map mode. Default: `"mean"`.

- ...:

  Ignored.

## Value

A `ggplot` object (invisibly).
