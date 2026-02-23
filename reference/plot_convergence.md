# Plot optimization convergence diagnostics

Multi-panel plot showing loss history, gradient norm, and learning rate
per epoch.

## Usage

``` r
plot_convergence(
  result,
  which = c("loss", "gradient", "lr"),
  log_y = TRUE,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- which:

  Character vector. Which panels to show: any combination of `"loss"`,
  `"gradient"`, `"lr"`. Default: all three.

- log_y:

  Logical. Use log scale for y-axis? Default: TRUE.

- ...:

  Ignored.

## Value

A ggplot object (invisibly). Prints the plot.
