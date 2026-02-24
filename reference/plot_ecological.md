# Cross-election consistency check

Compares tract-level results between two election years to check whether
spatial patterns are consistent.

## Usage

``` r
plot_ecological(
  result,
  result2,
  variable = NULL,
  type = c("scatter", "map"),
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- result2:

  A second `interpElections_result` object (different year).

- variable:

  Variable name to compare. NULL auto-matches by party.

- type:

  Character. `"scatter"` or `"map"`. Default: `"scatter"`.

- ...:

  Ignored.

## Value

A `ggplot` object (invisibly).
