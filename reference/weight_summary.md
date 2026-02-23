# Summarize weight matrix properties per tract

Computes per-tract statistics from the weight matrix, including
concentration measures (Herfindahl, entropy), effective number of
sources, and optionally travel-time-weighted means.

## Usage

``` r
weight_summary(result)
```

## Arguments

- result:

  An `interpElections_result` object.

## Value

A data frame with one row per tract.
