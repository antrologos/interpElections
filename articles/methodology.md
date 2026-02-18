# Methodology

This vignette walks through the full interpElections pipeline step by
step, using **real data from Varginha (MG)** in the 2022 presidential
election. It covers the problem formulation, data preparation,
travel-time computation, IDW kernel, Sinkhorn balancing, optimization,
interpolation, and validation.

Computationally heavy steps (`eval = FALSE`) show pre-rendered outputs
and figures. Small toy examples (`eval = TRUE`) can be run
interactively.

## 1. The Problem: Many-to-Many Spatial Disaggregation

A city has **N census tracts** and **M polling stations**. At each
station, votes are tallied: how many votes each candidate received, how
many voters showed up, the age profile of registered voters. At each
tract, the census records population counts by age, gender, education,
income — but not who they voted for.

We want **tract-level vote estimates**. But there is no direct mapping
between tracts and stations. The relationship is **many-to-many**: one
station serves voters from multiple tracts, and one tract’s voters may
go to multiple stations. There is no administrative boundary for a
“seção eleitoral” — a polling section is a *location*, not a *zone*.

This differs from standard areal interpolation (where source and target
are both polygons): our sources are **points**. Area-weighted
interpolation does not apply.

The solution: use **travel time** as a proxy for the tract-station
relationship. Voters tend to go to nearby stations. If we can estimate
how much each station “serves” each tract, we can disaggregate station
totals into tract-level estimates.

The pipeline:

1.  **Data**: Census population + tract geometries + electoral data at
    polling stations
2.  **Travel times**: Walking routes from tract representative points to
    stations (r5r on OpenStreetMap)
3.  **IDW kernel**: Convert travel times to raw weights with per-tract
    decay parameters
4.  **Sinkhorn balancing**: Enforce population conservation (row sums)
    and source conservation (column sums)
5.  **Optimization**: Find alpha that minimizes the mismatch between
    interpolated and census demographics
6.  **Interpolation**: Apply the optimized weights to any station-level
    variable

## 2. The Data: Census Population

``` r
pop_data <- br_prepare_population("Varginha", year = 2010)
head(pop_data)
```

      code_tract    code_muni pop_18_20 pop_21_24 pop_25_29 pop_30_39 pop_40_49 pop_50_59 pop_60_69
    1 3170701xxxxx  3170701      82       103       125       258       220       178       142
    2 3170701xxxxx  3170701      45        62        71       130       115        93        80
    ...

The `pop_*` columns contain census population counts by age bracket.
These brackets are chosen to match the TSE voter registration age groups
— they are the **calibration variables** that bridge census tracts and
polling stations.

| Census column | Age range   | TSE equivalent   |
|---------------|-------------|------------------|
| `pop_18_20`   | 18–20 years | `votantes_18_20` |
| `pop_21_24`   | 21–24 years | `votantes_21_24` |
| `pop_25_29`   | 25–29 years | `votantes_25_29` |
| `pop_30_39`   | 30–39 years | `votantes_30_39` |
| `pop_40_49`   | 40–49 years | `votantes_40_49` |
| `pop_50_59`   | 50–59 years | `votantes_50_59` |
| `pop_60_69`   | 60–69 years | `votantes_60_69` |

These 7 matched brackets are the key insight: they are observed at
**both** levels, enabling calibration.

![](figures/pop-pyramid.png)

## 3. The Data: Census Tract Geometries

``` r
tracts_sf <- br_prepare_tracts(3170701, pop_data)
```

This downloads tract geometries from `geobr`, removes green areas
(parks, water bodies with zero population), transforms to EPSG:5880
(equal-area projection for accurate spatial operations), and joins the
population columns.

![](figures/tracts-pop-map.png)

Note the urban-rural gradient: small, densely populated tracts in the
city center; large, sparsely populated tracts on the periphery.

## 4. The Data: Electoral Data at Polling Stations

``` r
electoral <- br_prepare_electoral(
  code_muni_ibge = "3170701",
  code_muni_tse  = "70700",
  uf = "MG", year = 2022,
  cargo = "presidente", turno = 1,
  what = c("candidates", "turnout")
)
```

The result contains one row per polling station with:

- **Vote columns** (`CAND_13`, `CAND_22`, …): votes per candidate
  (ballot number as suffix; 95 = blank, 96 = null)
- **Turnout** (`QT_COMPARECIMENTO`): total voters who showed up
- **Voter age profiles** (`votantes_18_19`, `votantes_20`, …,
  `votantes_65_69`): TSE’s fine-grained age registration brackets (12
  groups), which are aggregated to match the 7 census brackets
- **Coordinates** (`lat`, `long`): geocoded polling station locations
- **Column dictionary**: `attr(electoral, "column_dictionary")` contains
  metadata for each column (type, cargo, candidate name, party)

The `what` parameter controls what gets included:

- `"candidates"`: one column per candidate
- `"parties"`: aggregated by party (`PARTY_PT`, `PARTY_PL`, …)
- `"turnout"`: `QT_COMPARECIMENTO`, `QT_APTOS`, `QT_ABSTENCOES`
- `"demographics"`: `GENERO_*` and `EDUC_*` columns

![](figures/electoral-stations-map.png)

## 5. Calibration Variables: Comparing Source and Target

The calibration variables are the bridge between census tracts and
polling stations. The same age brackets are observed at both levels:

- **Census**: “How many people aged 18–20 live in this tract?” (from
  IBGE)
- **Electoral**: “How many registered voters aged 18–20 voted at this
  station?” (from TSE)

![](figures/age-pyramids-comparison.png)

The shapes are similar but totals differ: the census counts residents,
the TSE counts registered voters. Not everyone of voting age is
registered, and registration rates vary by age bracket.

The optimization finds alpha values so that the **interpolated** voter
age profiles match the **census** age profiles at each tract. If the
weights are correct for demographics, they should also be correct for
vote counts.

## 6. Representative Points: From Tracts to Routable Origins

Routing engines need **point-to-point** queries, but tracts are
polygons. We need a single representative point per tract. Three methods
are available:

| Method               | Function                                                                                | Guarantee                                   |
|----------------------|-----------------------------------------------------------------------------------------|---------------------------------------------|
| `"centroid"`         | [`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html)         | Fast. May fall outside concave polygons.    |
| `"point_on_surface"` | [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html) | Guaranteed inside. Default.                 |
| `"pop_weighted"`     | WorldPop raster lookup                                                                  | Most populated cell. Best for large tracts. |

``` r
pts_pos <- compute_representative_points(tracts_sf, method = "point_on_surface")
pts_pop <- compute_representative_points(tracts_sf, method = "pop_weighted")
```

The `pop_weighted` method downloads the WorldPop Brazil Constrained 2020
raster (~48 MB, cached) and finds the raster cell with the highest
population density within each tract. This matters for large peripheral
tracts where the geometric center may be in an uninhabited area.

The `pop_min_area` parameter (default: 1 km²) controls the threshold:
tracts smaller than this use `point_on_surface` regardless of the chosen
method.

## 7. Travel Times: Building the Distance Matrix

``` r
time_matrix <- compute_travel_times(
  tracts_sf = tracts_sf,
  points_sf = electoral_sf,
  tract_id = "id",
  point_id = "id"
)
```

This builds an **\[N tracts x M stations\] matrix** of walking travel
times in minutes, using the r5r routing engine on OpenStreetMap data.

- **Origins**: representative points (from section 6)
- **Destinations**: polling station coordinates
- **Mode**: walking (default), configurable
- **Max trip duration**: 300 minutes; unreachable pairs get this value

**Why travel time instead of straight-line distance?** A tract across a
river may be 500m away as the crow flies but 30 minutes by foot. Travel
times on the actual road network capture these geographic barriers.

The OSM data is downloaded via
[`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md),
which fetches state-level `.pbf` files and clips them to the
municipality bounding box using osmium.

![](figures/tt-heatmap.png)

The heatmap shows spatial structure: a block-diagonal pattern where
nearby tract-station pairs have short travel times.

![](figures/tt-accessibility-map.png)

Tracts in the center have short travel times to the nearest station;
peripheral tracts are farther away.

**The offset**: Before applying the IDW kernel, we add 1 to all travel
times (`time_matrix + 1`) to avoid singularity when $t = 0$.

## 8. The IDW Kernel: Turning Distance into Influence

The core formula for the raw weight between tract $i$ and station $j$:

$$W_{ij}^{\text{raw}} = \left( t_{ij} + 1 \right)^{- \alpha_{i}}$$

where:

- $t_{ij}$: travel time from tract $i$ to station $j$ (minutes)
- $\alpha_{i}$: decay parameter for tract $i$ (to be optimized)

Closer stations get more weight. The decay rate $\alpha$ controls how
quickly influence drops off with distance.

``` r
tt_range <- 0:60
alphas <- c(0.5, 1, 2, 5, 10)
decay_df <- do.call(rbind, lapply(alphas, function(a) {
  data.frame(time = tt_range, weight = (tt_range + 1)^(-a),
             alpha = paste0("alpha = ", a))
}))
decay_df$alpha <- factor(decay_df$alpha,
                          levels = paste0("alpha = ", alphas))

library(ggplot2)
ggplot(decay_df, aes(x = time, y = weight, color = alpha)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10() +
  labs(title = "Weight Decay Curves",
       subtitle = "Higher alpha = steeper decay = more concentrated weights",
       x = "Travel time (minutes)", y = "Weight (log scale)",
       color = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")
```

![](methodology_files/figure-html/weight-decay-1.png)

- $\alpha = 0.5$: nearly flat — all stations contribute almost equally
- $\alpha = 2$: moderate — stations within 10 minutes dominate
- $\alpha = 10$: steep cliff — only the nearest station matters

**Why per-tract alpha?** Urban centers with overlapping station
catchments need low alpha (spread weight). Isolated periphery with one
nearby station needs high alpha (concentrate weight). A single global
alpha cannot capture this heterogeneity.

## 9. Why Column Standardization Is Not Enough

The naive approach: divide each column by its sum so that each station
distributes 100% of its data:

$$W_{ij}^{\text{std}} = \frac{W_{ij}^{\text{raw}}}{\sum\limits_{i}W_{ij}^{\text{raw}}}$$

Column sums = 1 (**source conservation**: every vote is distributed
exactly once). But there is no constraint on row sums.

``` r
# Toy example: 5 tracts, 3 stations
tt_toy <- matrix(
  c(2,  10, 25, 30, 50,   # station 1: close to tract 1
    15,  3, 12, 28, 45,   # station 2: close to tract 2
    40, 35, 20,  5,  8),  # station 3: close to tracts 4-5
  nrow = 5, ncol = 3
)
pop_toy <- c(100, 300, 150, 200, 250)  # population per tract

alpha_toy <- rep(2, 5)
W_raw <- (tt_toy + 1)^(-alpha_toy)

# Column standardize
W_std <- t(t(W_raw) / colSums(W_raw))

cat("Column sums (source conservation):\n")
#> Column sums (source conservation):
round(colSums(W_std), 4)
#> [1] 1 1 1

cat("\nRow sums (NO population constraint):\n")
#> 
#> Row sums (NO population constraint):
round(rowSums(W_std), 4)
#> [1] 0.9751 0.9300 0.1439 0.6594 0.2917

cat("\nPopulation shares:\n")
#> 
#> Population shares:
round(pop_toy / sum(pop_toy), 4)
#> [1] 0.10 0.30 0.15 0.20 0.25
```

Tract 1 has only 10% of the population but receives a disproportionate
share of total weight because it is close to station 1. The row sums
don’t match population shares — downstream estimates will be biased
toward well-connected tracts.

## 10. Sinkhorn Balancing: Double Conservation

The **Sinkhorn algorithm** (also known as Iterative Proportional
Fitting, or the RAS algorithm) iteratively scales rows and columns to
enforce both marginal constraints simultaneously:

- **Row targets**:
  $r_{i} = \frac{\text{pop}_{i}}{\sum\text{pop}} \times m$
  (population-proportional)
- **Column targets**: $c_{j} = 1$ for all $j$ (source conservation)

&nbsp;

    W <- W_raw
    for iter in 1..max_iter:
      W <- W * (row_targets / rowSums(W))        # scale rows
      W <- t(t(W) * (col_targets / colSums(W)))  # scale columns
      if max marginal error < tol: break

Row scaling pulls each tract’s total weight toward its population share.
Column scaling readjusts so each station still distributes 100%. Repeat
until both constraints are satisfied.

``` r
# Continue from the toy example
m <- ncol(tt_toy)
row_targets <- pop_toy / sum(pop_toy) * m
col_targets <- rep(1, m)

# Run Sinkhorn step by step, tracking convergence
W <- W_raw
max_err_history <- numeric(50)
for (k in 1:50) {
  rs <- rowSums(W); rs[rs == 0] <- 1
  W <- W * (row_targets / rs)
  cs <- colSums(W); cs[cs == 0] <- 1
  W <- t(t(W) * (col_targets / cs))
  max_err_history[k] <- max(
    abs(rowSums(W) - row_targets),
    abs(colSums(W) - col_targets)
  )
}

# Plot convergence
conv_df <- data.frame(iteration = 1:50, max_error = max_err_history)
ggplot(conv_df, aes(x = iteration, y = max_error)) +
  geom_line(color = "#4575b4", linewidth = 0.8) +
  geom_point(color = "#4575b4", size = 1) +
  scale_y_log10() +
  labs(title = "Sinkhorn Convergence",
       subtitle = "Marginal error converges to machine precision in ~15 iterations",
       x = "Iteration", y = "Max marginal error (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![](methodology_files/figure-html/sinkhorn-demo-1.png)

``` r
# Verify the balanced matrix
W_balanced <- sinkhorn_balance(W_raw,
  row_targets = row_targets,
  col_targets = col_targets
)

cat("Row sums (should match targets):\n")
#> Row sums (should match targets):
cbind(target = round(row_targets, 4),
      actual = round(rowSums(W_balanced), 4))
#>      target actual
#> [1,]   0.30   0.30
#> [2,]   0.90   0.90
#> [3,]   0.45   0.45
#> [4,]   0.60   0.60
#> [5,]   0.75   0.75

cat("\nColumn sums (should be 1):\n")
#> 
#> Column sums (should be 1):
round(colSums(W_balanced), 4)
#> [1] 1 1 1
```

Compare before and after Sinkhorn:

``` r
# Heatmap comparison
W_std_long <- expand.grid(tract = 1:5, station = 1:3)
W_std_long$weight <- as.vector(W_std)
W_std_long$method <- "Column standardized"

W_bal_long <- expand.grid(tract = 1:5, station = 1:3)
W_bal_long$weight <- as.vector(W_balanced)
W_bal_long$method <- "Sinkhorn balanced"

both <- rbind(W_std_long, W_bal_long)
both$method <- factor(both$method,
  levels = c("Column standardized", "Sinkhorn balanced"))

ggplot(both, aes(x = station, y = tract, fill = weight)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(weight, 3)), size = 3) +
  facet_wrap(~method) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(title = "Weight Matrix: Before and After Sinkhorn",
       x = "Station", y = "Tract") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![](methodology_files/figure-html/sinkhorn-comparison-1.png)

Sinkhorn redistributes weight: over-weighted tracts (near many stations)
get less, under-weighted tracts get more. The geographic “shape” of each
row is preserved, but the “scale” changes to match population.

**Key properties**:

- The balanced matrix is **unique** for given row/column targets
- **Non-negative**: if input is non-negative, output is non-negative
- **Convergence guaranteed** when `sum(row_targets) = sum(col_targets)`
- Unreachable tracts (all-zero rows) remain zero and are flagged

## 11. The Calibration Objective

We have travel times, an IDW kernel parametrized by $\alpha$, and
Sinkhorn balancing. We need to find the optimal $\alpha$.

**Objective function**:

$$f(\alpha) = \sum\limits_{i}\sum\limits_{k}\left( {\widehat{V}}_{ik} - P_{ik} \right)^{2}$$

where:

- $\widehat{V} = W_{\text{sinkhorn}}(\alpha) \times S$ — interpolated
  voter demographics per tract
- $P$ — census demographics per tract
- $k$ = age bracket index (7 brackets)

**Intuition**: if the weights are correct, interpolating voter age
profiles from stations should recover census age profiles at tracts. The
optimal alpha minimizes the mismatch.

The forward pass for a given alpha vector:

1.  Build raw kernel:
    $W_{ij}^{\text{raw}} = \left( t_{ij} + 1 \right)^{- \alpha_{i}}$
2.  Run $K$ Sinkhorn iterations (K = 5 during optimization)
3.  Compute: $\widehat{V} = W_{\text{balanced}} \times S$
4.  Compute: $\text{loss} = \sum\left( \widehat{V} - P \right)^{2}$

**Why K = 5 during optimization?** Full Sinkhorn convergence (K = 1000)
is expensive. K = 5 gives an approximate balance that produces a
gradient pointing in the right direction. The final weight computation
(after optimization) uses full convergence.

``` r
# Simplified: all tracts share the same alpha
tt_adj <- tt_toy + 1
pop_matrix_toy <- matrix(pop_toy, ncol = 1)
src_matrix_toy <- matrix(c(250, 400, 350), ncol = 1)

alpha_range <- seq(0.1, 8, by = 0.1)
sse_values <- sapply(alpha_range, function(a) {
  sinkhorn_objective(
    alpha = rep(a, 5),
    time_matrix = tt_adj,
    pop_matrix = pop_matrix_toy,
    source_matrix = src_matrix_toy,
    row_targets = pop_toy / sum(pop_toy) * 3,
    sinkhorn_iter = 50
  )
})

ggplot(data.frame(alpha = alpha_range, sse = sse_values),
       aes(x = alpha, y = sse)) +
  geom_line(color = "#4575b4", linewidth = 0.8) +
  geom_vline(xintercept = alpha_range[which.min(sse_values)],
             linetype = "dashed", color = "#d73027") +
  labs(title = "Objective Function Landscape",
       subtitle = sprintf("Minimum at alpha = %.1f (shared across all tracts)",
                           alpha_range[which.min(sse_values)]),
       x = expression(alpha), y = "SSE") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![](methodology_files/figure-html/objective-landscape-1.png)

The landscape has a clear minimum. In practice, each tract has its own
alpha, creating a high-dimensional optimization problem.

``` r
# Varginha: objective before and after optimization
sse_init <- sinkhorn_objective(
  rep(1, nrow(time_matrix)), time_matrix + 1,
  pop_matrix, source_matrix, row_targets, sinkhorn_iter = 50
)
sse_opt <- sinkhorn_objective(
  optim_result$alpha, time_matrix + 1,
  pop_matrix, source_matrix, row_targets, sinkhorn_iter = 50
)
cat("SSE at alpha = 1 (initial):", format(sse_init, big.mark = ","), "\n")
cat("SSE at optimal alpha:      ", format(sse_opt, big.mark = ","), "\n")
```

## 12. Optimization: Torch ADAM with Log-Domain Sinkhorn

The objective function involves $K$ iterations of Sinkhorn scaling,
which is differentiable but writing the analytical gradient through $K$
unrolled iterations is impractical. Torch’s automatic differentiation
handles this: define the forward pass, get gradients for free.

**Log-domain Sinkhorn**: Standard Sinkhorn multiplies small numbers
(weights near zero for distant pairs) and can underflow in float32. The
log-domain reformulation uses logsumexp:

$$\log K_{ij} = - \alpha_{i} \cdot \log\left( t_{ij} + 1 \right)$$

Alternating updates ($K$ iterations):
$$\left. \log u_{i}\leftarrow\log r_{i} - \text{logsumexp}_{j}\left( \log K_{ij} + \log v_{j} \right) \right.$$$$\left. \log v_{j}\leftarrow\log c_{j} - \text{logsumexp}_{i}\left( \log K_{ij} + \log u_{i} \right) \right.$$

Balanced weights:
$W_{ij} = \exp\left( \log u_{i} + \log K_{ij} + \log v_{j} \right)$

All operations (log, exp, logsumexp, matmul) are differentiable in
torch.

**ADAM optimizer with multi-phase learning rate annealing**:

- Phase 1: linear warmup (10 steps from LR = 0 to `gpu_lr_init`)
- Subsequent phases: each runs inner iterations with decreasing LR
- LR decays by `gpu_lr_decay` (default 0.6) between phases
- Gradient clipping at norm 1.0
- Alpha clamped to `[lower_bound, upper_bound]` (default \[0, 20\])

``` r
optim_result <- optimize_alpha(
  time_matrix   = time_matrix,
  pop_matrix    = pop_matrix,
  source_matrix = source_matrix,
  row_targets   = row_targets,
  sinkhorn_iter = 5,       # K=5 inside ADAM (fast, approximate)
  use_gpu       = FALSE,   # CPU for Varginha (small: ~130 tracts)
  gpu_iterations = 20,     # 20 outer LR phases
  gpu_lr_init   = 0.1,     # initial learning rate
  gpu_lr_decay  = 0.6      # LR multiplier between phases
)
```

![](figures/optim-convergence.png)

Rapid descent in the first few phases, then fine-tuning as the learning
rate decays.

**Key tuning parameters**:

| Parameter                     | Default     | Effect                                                              |
|-------------------------------|-------------|---------------------------------------------------------------------|
| `sinkhorn_iter`               | 5           | Sinkhorn iterations per ADAM step. Higher = more accurate gradient. |
| `gpu_iterations`              | 20          | Number of outer LR phases.                                          |
| `gpu_lr_init`                 | 0.1         | Initial learning rate. Reduce for numerical stability.              |
| `gpu_lr_decay`                | 0.6         | LR multiplier between phases.                                       |
| `dtype`                       | `"float32"` | `"float64"` for more precision (2x memory).                         |
| `lower_bound` / `upper_bound` | 0, 20       | Alpha clamp range.                                                  |

## 13. The Effect of Alpha: Spatial Interpretation

![](figures/alpha-histogram.png)

Most alphas cluster in a moderate range, with tails extending toward 0
(uniform allocation) and the upper bound (nearest-station-only).

![](figures/alpha-map.png)

- **Low alpha** (blue, urban core): weight is spread across many nearby
  stations. Multiple stations overlap, and the optimizer distributes
  influence broadly.
- **High alpha** (red, periphery): weight is concentrated on the nearest
  station. Isolated tracts with one dominant station.

``` r
# How alpha changes the weight distribution for a fixed tract
tt_one_tract <- c(3, 8, 15, 25, 40)  # travel times to 5 stations
alphas_demo <- c(1, 3, 5, 10)
wt_df <- do.call(rbind, lapply(alphas_demo, function(a) {
  w <- (tt_one_tract + 1)^(-a)
  w <- w / sum(w)  # normalize for display
  data.frame(station = 1:5, weight = w,
             alpha = paste0("alpha = ", a))
}))
wt_df$alpha <- factor(wt_df$alpha,
                        levels = paste0("alpha = ", alphas_demo))

ggplot(wt_df, aes(x = factor(station), y = weight, fill = alpha)) +
  geom_col(position = "dodge") +
  labs(title = "Weight Distribution as Alpha Increases",
       subtitle = "Fixed tract with 5 stations at distances 3, 8, 15, 25, 40 min",
       x = "Station (sorted by distance)", y = "Normalized weight",
       fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![](methodology_files/figure-html/alpha-effect-1.png)

At $\alpha = 1$, weights are nearly uniform. At $\alpha = 10$, the
nearest station dominates with ~85% of the weight.

**Interpretation guide**:

- $\alpha < 1$: draws information almost equally from all stations.
  Unusual — check for data issues.
- $\alpha \in \lbrack 1,3\rbrack$: several nearby stations contribute.
  Typical for dense urban areas.
- $\alpha \in \lbrack 3,6\rbrack$: a few stations dominate. Typical for
  suburban areas.
- $\alpha > 6$: one station dominates almost entirely. Typical for
  isolated or peripheral tracts.
- $\left. \alpha\rightarrow 0 \right.$: uniform allocation (ignores
  geography).
- $\alpha = 20$ (upper bound): nearest station only. May indicate the
  optimizer hit the bound.

## 14. The Interpolation: From Weights to Tract-Level Estimates

The weight matrix $W$ encodes the geographic relationship between tracts
and stations. Multiplying by any station-level variable produces
tract-level estimates:

$$\widehat{X} = W \times V$$

where:

- $W$ \[N x M\]: Sinkhorn-balanced weight matrix
- $V$ \[M x 1\]: any variable measured at stations
- $\widehat{X}$ \[N x 1\]: estimated value at each tract

For multiple variables simultaneously:

$$\left. \widehat{X} = W \times V_{\text{matrix}}\quad\lbrack N \times M\rbrack \cdot \lbrack M \times P\rbrack\rightarrow\lbrack N \times P\rbrack \right.$$

``` r
# Practical example with Varginha data
W <- sinkhorn_weights(time_matrix, optim_result$alpha, offset = 1,
  row_targets = row_targets)

electoral_data <- as.matrix(
  sources[, c("CAND_13", "CAND_22", "QT_COMPARECIMENTO")]
)
interpolated <- W %*% electoral_data

# Derived quantities
pct_lula <- interpolated[, "CAND_13"] /
  interpolated[, "QT_COMPARECIMENTO"] * 100
```

![](figures/interp-lula.png)

![](figures/interp-bolsonaro.png)

Geographic polarization becomes visible at the tract level — a spatial
pattern that was previously impossible to observe with station-level
data alone.

**Calibration validation**: interpolated demographics should match
census demographics (the optimization objective).

![](figures/interp-scatter-calib.png)

Points cluster tightly around the 45-degree line, confirming that the
optimization found good weights.

**The reuse principle**: the weight matrix $W$ is a property of the
geography and the calibration. Once computed, it can interpolate **any**
variable measured at polling stations — candidates, parties, turnout,
gender composition, education level — without re-optimizing alpha.

## 15. Conservation Properties and Validation

**Column conservation** (source conservation):

$$\sum\limits_{i}W_{ij} = 1\quad\forall j$$

Each station distributes exactly 100% of its data. Consequence:
municipal totals are preserved exactly.

``` r
# Using our toy example
votes <- c(500, 800, 700)   # votes at 3 stations
interpolated_votes <- as.numeric(W_balanced %*% votes)

cat("Source total:", sum(votes), "\n")
#> Source total: 2000
cat("Interpolated total:", round(sum(interpolated_votes)), "\n")
#> Interpolated total: 2000
cat("Per-tract:", round(interpolated_votes), "\n")
#> Per-tract: 151 644 304 399 502
```

**Row conservation** (population proportionality):

$$\sum\limits_{j}W_{ij} = r_{i} = \frac{\text{pop}_{i}}{\sum\text{pop}} \times m$$

Tracts receive weight proportional to their population share.

**Calibration residuals**:

$$\text{residual}_{ik} = \sum\limits_{j}W_{ij} \cdot S_{jk} - P_{ik}$$

where $S$ is the source demographic matrix and $P$ is the census
population matrix.

![](figures/residuals-boxplot.png)

Residuals should be centered near zero with small magnitude. Outliers
may indicate boundary effects (voters crossing municipality borders),
data quality issues, or model limitations.

## 16. Performance and Tuning

| Problem size (tracts) | Recommendation           | Typical time |
|-----------------------|--------------------------|--------------|
| \< 200                | CPU                      | \< 5 seconds |
| 200–1,000             | CPU or GPU               | 5–30 seconds |
| 1,000–5,000           | GPU recommended          | 30s – 2 min  |
| \> 5,000              | GPU strongly recommended | 2–10 min     |

**Practical runtimes**:

- Varginha (~130 tracts, ~20 stations): 2–4s CPU
- Igrejinha (~50 tracts, ~10 stations): 1–2s CPU
- Niteroi (~500 tracts, ~100 stations): 5–15s GPU
- Belo Horizonte (~4,000 tracts, ~800 stations): 30–60s GPU
- Sao Paulo capital (~18,000 tracts, ~3,000 stations): 5–10 min GPU

**Memory**: approximately `3 * sinkhorn_iter + 10` copies of the N x M
matrix in GPU memory.

**RStudio**: torch inside RStudio IDE requires subprocess execution via
callr (automatic and transparent to the user). Standalone R scripts run
directly.

## 17. Setup: Torch, Java, and r5r

**Torch** (required for optimization):

``` r
setup_torch()    # installs torch + libtorch/lantern binaries
check_torch()    # diagnoses installation (R package, binaries, GPU)
```

GPU support: Windows (CUDA), macOS (MPS for Apple Silicon), Linux
(CUDA). CPU always works as fallback.

**Java + r5r** (required for travel time computation):

``` r
setup_java()     # downloads and installs Java 21+
check_r5r()      # checks Java version and r5r readiness
setup_osmium()   # installs osmium-tool for OSM clipping
```

Memory: `options(java.parameters = "-Xmx8g")` before loading r5r for
large municipalities.

**Cache management**:

``` r
get_interpElections_cache_dir()    # where downloads are stored
interpElections_cache()            # list cached files by category
interpElections_cache_clean()      # clear cache (all or by category)
```

## 18. Mathematical Appendix

**Notation**:

- $N$ = number of census tracts, $M$ = number of polling stations
- $K$ = number of calibration age brackets (7)
- $t \in {\mathbb{R}}^{N \times M}$: travel time matrix (minutes)
- $\alpha \in {\mathbb{R}}^{N}$: per-tract decay parameters
- $P \in {\mathbb{R}}^{N \times K}$: census population by age bracket
- $S \in {\mathbb{R}}^{M \times K}$: voter counts by age bracket
- $r \in {\mathbb{R}}^{N}$: row targets (population-proportional)
- $c \in {\mathbb{R}}^{M}$: column targets (= 1)

**IDW kernel**:

$$K_{ij} = \left( t_{ij} + 1 \right)^{- \alpha_{i}}$$

**Log-domain kernel**:

$$\log K_{ij} = - \alpha_{i} \cdot \log\left( t_{ij} + 1 \right)$$

**Sinkhorn iterations (log-domain)**:

Initialize: $\log u = 0$, $\log v = 0$

For $k = 1$ to $K_{\text{sink}}$:
$$\left. \log u_{i}\leftarrow\log r_{i} - \text{logsumexp}_{j}\left( \log K_{ij} + \log v_{j} \right) \right.$$$$\left. \log v_{j}\leftarrow\log c_{j} - \text{logsumexp}_{i}\left( \log K_{ij} + \log u_{i} \right) \right.$$

**Balanced weight matrix**:

$$W_{ij} = \exp\left( \log u_{i} + \log K_{ij} + \log v_{j} \right)$$

**Objective function**:

$$f(\alpha) = \parallel W(\alpha) \cdot S - P \parallel_{F}^{2} = \sum\limits_{i,k}\left( \sum\limits_{j}W_{ij}S_{jk} - P_{ik} \right)^{2}$$

**Gradient**: computed by torch autograd through $K$ unrolled Sinkhorn
iterations. Each operation in the forward pass (log, exp, logsumexp,
matrix multiply, subtraction, squaring) has a defined backward pass in
torch. The gradient flows backward through all $K$ Sinkhorn iterations
automatically:

$$\nabla_{\alpha}f = \frac{\partial}{\partial\alpha} \parallel \text{sinkhorn}_{K}\left( \text{kernel}(\alpha) \right) \cdot S - P \parallel_{F}^{2}$$
