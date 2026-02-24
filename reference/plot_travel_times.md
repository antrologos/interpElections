# Plot travel time matrix diagnostics

Visualize the distribution and spatial structure of travel times between
census tracts and polling stations.

## Usage

``` r
plot_travel_times(
  result,
  type = c("histogram", "heatmap", "map"),
  tract = NULL,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object.

- type:

  Character. Plot type: `"histogram"`, `"heatmap"`, or `"map"`. Default:
  `"histogram"`.

- tract:

  Tract ID or index for `type = "map"`. NULL picks a random tract.

- ...:

  Ignored.

## Value

A `ggplot` object (invisibly). Prints the plot.

## Details

- `"histogram"`: Distribution of all travel time values. Annotates the
  maximum value (potential fill value for unreachable pairs).

- `"heatmap"`: Travel time matrix as a tile plot, with tracts ordered by
  latitude of their centroid. Reveals spatial locality.

- `"map"`: For a selected tract, overlays station points colored by
  travel time from that tract.
