# Diagnostics and Validation

interpElections provides 16 diagnostic functions for validating
interpolation results. This vignette demonstrates each one using **real
data** from four Brazilian municipalities of varying sizes:

| City           | State | Tracts | Sources | Character                    |
|:---------------|:------|-------:|--------:|:-----------------------------|
| Igrejinha      | RS    |    ~40 |     ~10 | Tiny rural municipality      |
| Varginha       | MG    |   ~280 |     ~37 | Small city (primary example) |
| Niteroi        | RJ    |   ~450 |     ~80 | Medium, dense urban area     |
| Belo Horizonte | MG    | ~5,100 |    ~600 | Major metropolitan area      |

Each section follows the pattern: **Intuition** (why it matters),
**Formal definition** (the math), **Applied example** (real output),
**Interpretation** (what to look for).

## Setup

``` r
library(interpElections)

# Load precomputed results (2022 presidential election)
result_igr  <- readRDS("precomputed/igrejinha_2022.rds")
result_vga  <- readRDS("precomputed/varginha_2022.rds")
result_vga2 <- readRDS("precomputed/varginha_2022_t2.rds")  # turno 2
result_nit  <- readRDS("precomputed/niteroi_2022.rds")
result_bh   <- readRDS("precomputed/belo_horizonte_2022.rds")
```

Quick overview of each result:

    #> interpElections result summary
    #> -------------------------------------------------- 
    #> Municipality: VARGINHA (MG) -- IBGE: 3170701, TSE: 54135
    #> Election: 2022 | Census: 2022 | Turno: 1 | Cargo: PRESIDENTE 
    #> Census tracts: 279 | Sources: 37 | Variables: 28
    #> 
    #> Calibration: gender x 7 age brackets (14 pairs)
    #> 
    #> Optimization: pb_sgd_colnorm_cpu | Objective: 4742.8657 | Convergence: 0
    #>   Alpha: min=1.000, Q1=1.373, median=1.564, Q3=1.839, max=11.032
    #> 
    #> Candidates (13):
    #>   CAND_30                  LUIZ FELIPE CHAVES D AVILA (NOVO) total=       908  mean=     3.3  [0.1, 7.0]
    #>   CAND_44                  SORAYA VIEIRA THRONICKE (UNIÃO) total=       385  mean=     1.4  [0.0, 3.0]
    #>   CAND_12                  CIRO FERREIRA GOMES (PDT) total=      1835  mean=     6.6  [0.2, 14.6]
    #>   CAND_13                  LUIZ INÁCIO LULA DA SILVA (PT) total=     24380  mean=    87.4  [2.0, 189.2]
    #>   ... 9 more -- View(result$dictionary)
    #> 
    #> Turnout (1):
    #>   QT_COMPARECIMENTO        turnout                total=     71308  mean=   255.6  [6.0, 555.5]
    #> 
    #> Calibration (14):
    #>   vot_hom_18_19            calibration            total=       917  mean=     3.3  [0.0, 12.3]
    #>   vot_hom_20_24            calibration            total=      3551  mean=    12.7  [0.1, 38.1]
    #>   vot_hom_25_29            calibration            total=      4033  mean=    14.5  [0.6, 41.5]
    #>   vot_hom_30_39            calibration            total=      8291  mean=    29.7  [1.1, 66.8]
    #>   vot_hom_40_49            calibration            total=      8164  mean=    29.3  [0.7, 67.4]
    #>   vot_hom_50_59            calibration            total=      6975  mean=    25.0  [0.5, 66.8]
    #>   vot_hom_60_69            calibration            total=      5737  mean=    20.6  [0.3, 47.0]
    #>   vot_mul_18_19            calibration            total=      1189  mean=     4.3  [0.0, 10.9]
    #>   vot_mul_20_24            calibration            total=      3699  mean=    13.3  [0.1, 42.4]
    #>   vot_mul_25_29            calibration            total=      4167  mean=    14.9  [0.6, 34.4]
    #>   vot_mul_30_39            calibration            total=      8661  mean=    31.0  [1.2, 70.5]
    #>   vot_mul_40_49            calibration            total=      8960  mean=    32.1  [0.9, 71.6]
    #>   vot_mul_50_59            calibration            total=      7639  mean=    27.4  [0.7, 60.2]
    #>   vot_mul_60_69            calibration            total=      6406  mean=    23.0  [0.4, 51.5]
    #> 
    #> 
    #> Object size: 181.3 MB
    #> Neighborhoods: 387

    #> interpElections result summary
    #> -------------------------------------------------- 
    #> Municipality: BELO HORIZONTE (MG) -- IBGE: 3106200, TSE: 41238
    #> Election: 2022 | Census: 2022 | Turno: 1 | Cargo: PRESIDENTE 
    #> Census tracts: 5109 | Sources: 407 | Variables: 28
    #> 
    #> Calibration: gender x 7 age brackets (14 pairs)
    #> 
    #> Optimization: pb_sgd_colnorm_cuda | Objective: 8123.8301 | Convergence: 0
    #>   Alpha: min=1.000, Q1=1.202, median=1.292, Q3=1.406, max=16.569
    #> 
    #> Candidates (13):
    #>   CAND_13                  LUIZ INÁCIO LULA DA SILVA (PT) total=    603542  mean=   118.1  [2.9, 320.5]
    #>   CAND_27                  JOSE MARIA EYMAEL (DC) total=       209  mean=     0.0  [0.0, 0.2]
    #>   CAND_30                  LUIZ FELIPE CHAVES D AVILA (NOVO) total=     14496  mean=     2.8  [0.1, 8.9]
    #>   CAND_14                  KELMON LUIS DA SILVA SOUZA (PTB) total=       861  mean=     0.2  [0.0, 0.6]
    #>   ... 9 more -- View(result$dictionary)
    #> 
    #> Turnout (1):
    #>   QT_COMPARECIMENTO        turnout                total=   1489578  mean=   291.6  [7.1, 788.7]
    #> 
    #> Calibration (14):
    #>   vot_hom_18_19            calibration            total=     21044  mean=     4.1  [0.0, 12.1]
    #>   vot_hom_20_24            calibration            total=     73593  mean=    14.4  [0.3, 42.5]
    #>   vot_hom_25_29            calibration            total=     81481  mean=    15.9  [0.4, 45.2]
    #>   vot_hom_30_39            calibration            total=    172752  mean=    33.8  [0.8, 92.7]
    #>   vot_hom_40_49            calibration            total=    170766  mean=    33.4  [0.8, 91.2]
    #>   vot_hom_50_59            calibration            total=    138388  mean=    27.1  [0.7, 72.3]
    #>   vot_hom_60_69            calibration            total=    105049  mean=    20.6  [0.5, 52.4]
    #>   vot_mul_18_19            calibration            total=     23556  mean=     4.6  [0.0, 13.8]
    #>   vot_mul_20_24            calibration            total=     77657  mean=    15.2  [0.3, 44.8]
    #>   vot_mul_25_29            calibration            total=     88270  mean=    17.3  [0.4, 49.0]
    #>   vot_mul_30_39            calibration            total=    190378  mean=    37.3  [0.9, 102.8]
    #>   vot_mul_40_49            calibration            total=    194436  mean=    38.1  [0.9, 104.3]
    #>   vot_mul_50_59            calibration            total=    165447  mean=    32.4  [0.8, 87.1]
    #>   vot_mul_60_69            calibration            total=    137434  mean=    26.9  [0.6, 68.8]
    #> 
    #> 
    #> Object size: 235.5 MB
    #> Neighborhoods: 865

## 1. Automated Health Check: `diagnostics()`

### Intuition

[`diagnostics()`](https://antrologos.github.io/interpElections/reference/diagnostics.md)
is a single-call “blood test” for your interpolation result. It runs 8
automated checks and reports PASS/WARN/FAIL for each. Use it as a first
step after every interpolation to catch problems early.

Only checks whose outcome is **not guaranteed by construction** are
included. Vote conservation, non-negativity, and column normalization
are all enforced by design and therefore omitted.

### Formal definition

| Check              | Statistic                                                                         | PASS                   | WARN          | FAIL   |
|:-------------------|:----------------------------------------------------------------------------------|:-----------------------|:--------------|:-------|
| Convergence        | Optimizer exit code                                                               | code = 0               | code != 0     | –      |
| Alpha distribution | % of tracts with $\alpha > 15$                                                    | \< 5%                  | \>= 5%        | –      |
| Residual RMSE      | $\sqrt{\text{mean}\left( r^{2} \right)}$                                          | always (informational) | –             | –      |
| Residual bias      | $\max\left| {\bar{r}}_{k} \right|/\text{RMSE}$                                    | ratio \< 10%           | ratio \>= 10% | –      |
| Pop-voter gap      | $\left| N_{census} - N_{source} \right|/\max\left( N_{census},N_{source} \right)$ | \< 5%                  | 5–20%         | \> 20% |
| Unreachable pairs  | % of $t_{ij}$ near max                                                            | \< 1%                  | 1–10%         | \> 10% |
| Alpha spatial CV   | $\text{sd}(\alpha)/\text{mean}(\alpha)$                                           | CV \>= 0.1             | CV \< 0.1     | –      |
| Implied turnout    | % of tracts with rate \> 110%                                                     | \< 5%                  | \>= 5%        | –      |

### Applied example

``` r
diagnostics(result_vga)
```

    #> interpElections diagnostics
    #> -------------------------------------------------- 
    #>  [PASS] Convergence: optimizer converged (code 0)
    #>  [PASS] Alpha distribution: median=1.55, IQR=[1.37, 1.82], 0% > 15
    #>  [PASS] Residual RMSE: 11.1614 (informational)
    #>  [WARN] Residual bias: max |colMeans(resid)| = 8.6344 (77.4% of RMSE)
    #>  [WARN] Population-voter gap: census=96829, source=78389, gap=19.0%
    #>  [PASS] Unreachable pairs: 0.5% of pairs near max travel time (>= 285 min)
    #>  [PASS] Alpha spatial variation: CV of median-alpha = 0.547
    #>  [PASS] Implied turnout: 0.7% of tracts with implied turnout > 110%
    #> -------------------------------------------------- 
    #> 6 PASS, 2 WARN

For a larger city:

``` r
diagnostics(result_bh)
```

    #> interpElections diagnostics
    #> -------------------------------------------------- 
    #>  [PASS] Convergence: optimizer converged (code 0)
    #>  [PASS] Alpha distribution: median=1.29, IQR=[1.21, 1.39], 0% > 15
    #>  [PASS] Residual RMSE: 7.0678 (informational)
    #>  [WARN] Residual bias: max |colMeans(resid)| = 1.8894 (26.7% of RMSE)
    #>  [PASS] Population-voter gap: census=1650864, source=1640251, gap=0.6%
    #>  [FAIL] Unreachable pairs: 15.1% of pairs near max travel time (>= 285 min)
    #>  [PASS] Alpha spatial variation: CV of median-alpha = 0.558
    #>  [PASS] Implied turnout: 2.1% of tracts with implied turnout > 110%
    #> -------------------------------------------------- 
    #> 6 PASS, 1 WARN, 1 FAIL

You can also access check values programmatically:

``` r
d <- diagnostics(result_vga, verbose = FALSE)
d$checks$convergence$value
d$checks$residual_rmse$value
d$checks$pop_voter_gap$value
```

### Interpretation

- **Convergence WARN**: The optimizer didn’t converge within the epoch
  limit. Try increasing `max_epochs` in
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md),
  or check if the learning rate is too high.
- **Alpha distribution WARN**: Many tracts have very high $\alpha$
  values ($> 15$), meaning they effectively use only the nearest
  station. This can happen when travel time data has poor coverage.
- **Pop-voter gap WARN/FAIL**: The total census population differs
  substantially from the total registered voters. This is expected to
  some degree (not everyone registers, some register outside their
  residence), but gaps \> 20% suggest data quality issues.
- **Unreachable pairs FAIL**: Many tract-station pairs have travel times
  at the ceiling. This means the routing engine couldn’t find paths —
  check OSM road network coverage.
- **Alpha spatial CV WARN**: Alpha values have very low variation across
  tracts (CV \< 0.1). The optimizer may not be adapting to local
  conditions, possibly because of insufficient data or poor convergence.

## 2. Optimization Convergence: `plot_convergence()`

### Intuition

The optimizer minimizes Poisson deviance by adjusting the decay
parameters $\alpha$.
[`plot_convergence()`](https://antrologos.github.io/interpElections/reference/plot_convergence.md)
shows the optimization trajectory: is the loss decreasing smoothly? Is
the gradient approaching zero? What learning rate schedule was used?

### Formal definition

- **Loss**: Poisson deviance
  $= 2\sum_{i,k}\left\lbrack y_{ik}\log\frac{y_{ik}}{{\widehat{y}}_{ik}} - \left( y_{ik} - {\widehat{y}}_{ik} \right) \right\rbrack$,
  where $y$ = observed census population and $\widehat{y}$ = fitted
  population from the model.
- **Gradient norm**: $\parallel \nabla_{\alpha}L \parallel_{2}$, the
  Euclidean norm of the gradient of the loss with respect to all alpha
  parameters.
- **Learning rate**: Step size in gradient descent. May decay over
  epochs via cosine annealing or other schedules.

### Applied examples

All three panels (loss, gradient, learning rate):

``` r
plot_convergence(result_vga)
```

![](figures/diag-s2-conv-vga-all.png)

Comparing with a GPU-optimized large city:

``` r
plot_convergence(result_bh)
```

![](figures/diag-s2-conv-bh-all.png)

Show only the loss panel:

``` r
plot_convergence(result_vga, which = "loss")
```

![](figures/diag-s2-conv-vga-loss.png)

Log scale for loss and gradient (useful when the values span orders of
magnitude):

``` r
plot_convergence(result_vga, which = c("loss", "gradient"), log_y = TRUE)
```

![](figures/diag-s2-conv-vga-log.png)

### Interpretation

- **Good convergence**: Smooth, monotonically decreasing loss that
  plateaus. Gradient norm approaches zero. The vertical dashed red line
  marks the best epoch.
- **Poor convergence**: Oscillating loss, gradient that doesn’t shrink,
  or loss that hasn’t plateaued by the last epoch. Consider increasing
  `max_epochs` or adjusting the learning rate.
- **Learning rate**: If using cosine annealing, you’ll see a decaying
  pattern. Flat LR = constant step size throughout.

## 3. Alpha Decay Parameters: `plot_alpha()`

### Intuition

The parameter $\alpha_{i}$ controls how sharply the weight of station
$j$ decays with travel time for tract $i$. Higher $\alpha$ means voters
are more localized (sharp decay — only nearby stations matter). Lower
$\alpha$ means voters come from a broader area (gradual decay — distant
stations still contribute).

$\alpha$ is a **matrix** with one row per tract and one column per
demographic bracket (14 brackets = 7 age groups $\times$ 2 genders).
Different brackets may have different decay patterns (e.g., young voters
may travel farther to vote).

### Formal definition

The weight before column normalization is:

$$K_{ij} = \left( t_{ij} + 1 \right)^{- \alpha_{i}}$$

where $t_{ij}$ is the travel time (in minutes) from tract $i$ to station
$j$. The final weight is $W_{ij} = K_{ij}/\sum_{i}K_{ij}$
(column-normalized so each station’s voters are fully distributed).

### Applied examples: Map mode

The map mode collapses the 14-bracket alpha matrix to a single scalar
per tract using `summary_fn`. This is the key visualization choice —
different summaries answer different questions.

**Median** (default) — the “typical” alpha across all brackets:

``` r
plot_alpha(result_vga, type = "map", summary_fn = "median")
```

![](figures/diag-s3-alpha-vga-map-median.png)

**Mean** — sensitive to extreme values in any bracket:

``` r
plot_alpha(result_vga, type = "map", summary_fn = "mean")
```

![](figures/diag-s3-alpha-vga-map-mean.png)

**Population-weighted** — weights each bracket by the tract’s population
in that bracket:

``` r
plot_alpha(result_vga, type = "map", summary_fn = "pop_weighted")
```

![](figures/diag-s3-alpha-vga-map-popwt.png)

**Range** ($\max - \min$) — measures how much $\alpha$ varies across
brackets within each tract:

``` r
plot_alpha(result_vga, type = "map", summary_fn = "range")
```

![](figures/diag-s3-alpha-vga-map-range.png)

**Single bracket** (e.g., bracket 1 = Male 18–20):

``` r
plot_alpha(result_vga, type = "map", summary_fn = 1)
```

![](figures/diag-s3-alpha-vga-map-bracket1.png)

**Large city comparison** (Belo Horizonte):

``` r
plot_alpha(result_bh, type = "map", summary_fn = "median")
```

![](figures/diag-s3-alpha-bh-map-median.png)

### Applied examples: Histogram mode

Density plots faceted by bracket. With 14 brackets arranged in a 2\$\$7
grid, you can compare age groups and genders at a glance.

``` r
plot_alpha(result_nit, type = "histogram")
```

![](figures/diag-s3-alpha-nit-hist-all.png)

Select specific brackets:

``` r
plot_alpha(result_nit, type = "histogram", brackets = c(1, 7, 8, 14))
```

![](figures/diag-s3-alpha-nit-hist-sel.png)

Small city (narrower distributions due to fewer tracts):

``` r
plot_alpha(result_igr, type = "histogram")
```

![](figures/diag-s3-alpha-igr-hist-all.png)

### Applied examples: Bracket boxplot

The most compact view — one boxplot per bracket on a single panel:

``` r
plot_alpha(result_bh, type = "bracket")
```

![](figures/diag-s3-alpha-bh-bracket.png)

Male brackets only:

``` r
plot_alpha(result_nit, type = "bracket", brackets = 1:7)
```

![](figures/diag-s3-alpha-nit-bracket-male.png)

### Interpretation

- **Urban core vs periphery**: Central tracts with many nearby stations
  tend to have higher $\alpha$ (sharper decay). Peripheral tracts with
  few nearby stations have lower $\alpha$ (they must draw from distant
  stations).
- **Age patterns**: Younger brackets may show different $\alpha$
  distributions than older ones, reflecting different mobility patterns.
- **Range map**: High range = the optimizer found very different decay
  rates for different brackets in that tract. This often occurs at the
  urban-rural boundary.
- **$\alpha > 15$**: Effectively nearest-station assignment. The decay
  is so steep that only the closest station has non-negligible weight.

## 4. Calibration Residuals: `plot_residuals()` + `residual_summary()`

### Intuition

The model calibrates against known census demographics (population by
age$\times$gender bracket). Residuals measure how well the model
reproduces these known quantities. Small residuals mean the model’s
spatial allocation of voters closely matches census demographics —
giving confidence that the interpolated (unknown) electoral variables
are also spatially accurate.

### Formal definition

Three types of residuals are available:

- **Raw**: $r_{ik} = {\widehat{y}}_{ik} - y_{ik}$ (fitted minus
  observed). Scale depends on population size.
- **Pearson**:
  $r_{ik}^{P} = \left( {\widehat{y}}_{ik} - y_{ik} \right)/\sqrt{{\widehat{y}}_{ik}}$.
  Standardized under the Poisson assumption — comparable across tracts
  of different sizes.
- **Deviance**:
  $r_{ik}^{D} = \text{sign}\left( {\widehat{y}}_{ik} - y_{ik} \right) \cdot \sqrt{2\left\lbrack y_{ik}\log\frac{y_{ik}}{{\widehat{y}}_{ik}} - \left( y_{ik} - {\widehat{y}}_{ik} \right) \right\rbrack}$.
  Individual Poisson deviance contributions.

### Applied examples: Map mode

For maps, `summary_fn` collapses across the 14 brackets to a single
value per tract.

**RMSE** (default) — magnitude of misfit:

``` r
plot_residuals(result_vga, type = "map", summary_fn = "rmse")
```

![](figures/diag-s4-resid-vga-map-rmse.png)

**Mean** — directional bias (positive = over-prediction, negative =
under-prediction):

``` r
plot_residuals(result_vga, type = "map", summary_fn = "mean")
```

![](figures/diag-s4-resid-vga-map-mean.png)

**Pearson residuals** (standardized):

``` r
plot_residuals(result_vga, type = "map", residual_type = "pearson",
               summary_fn = "rmse")
```

![](figures/diag-s4-resid-vga-map-pearson.png)

**Large city** (Belo Horizonte):

``` r
plot_residuals(result_bh, type = "map", summary_fn = "rmse")
```

![](figures/diag-s4-resid-bh-map-rmse.png)

**Single bracket**:

``` r
plot_residuals(result_vga, type = "map", summary_fn = 1)
```

![](figures/diag-s4-resid-vga-map-bracket1.png)

### Applied examples: Histogram mode

``` r
plot_residuals(result_nit, type = "histogram")
```

![](figures/diag-s4-resid-nit-hist-raw.png)

Compare with Pearson residuals:

``` r
plot_residuals(result_nit, type = "histogram", residual_type = "pearson")
```

![](figures/diag-s4-resid-nit-hist-pearson.png)

### Applied examples: Bracket boxplot

``` r
plot_residuals(result_bh, type = "bracket")
```

![](figures/diag-s4-resid-bh-bracket-raw.png)

Deviance scale:

``` r
plot_residuals(result_bh, type = "bracket", residual_type = "deviance")
```

![](figures/diag-s4-resid-bh-bracket-dev.png)

### Applied examples: Scatter (fitted vs observed)

``` r
plot_residuals(result_igr, type = "scatter")
```

![](figures/diag-s4-resid-igr-scatter.png)

Selected brackets:

``` r
plot_residuals(result_vga, type = "scatter", brackets = c(1, 14))
```

![](figures/diag-s4-resid-vga-scatter-sel.png)

### Tabular summary: `residual_summary()`

``` r
residual_summary(result_vga)
```

    #> $per_bracket
    #>         bracket      mean      rmse  max_abs pct_gt_2sd
    #> 1    Male 18-19 -2.827957  4.059031 20.53281  11.111111
    #> 2    Male 20-24 -4.939067  7.797788 30.04813  11.827957
    #> 3    Male 25-29 -3.688171  8.106033 51.24654   5.017921
    #> 4    Male 30-39 -8.189961 17.043889 83.32305   8.243728
    #> 5    Male 40-49 -6.788528 12.326740 51.39126   8.243728
    #> 6    Male 50-59 -3.684587 10.199236 34.77560   6.451613
    #> 7    Male 60-69 -2.093189 10.403941 33.86028   3.942652
    #> 8  Female 18-19 -1.896057  3.159372 15.21720   8.243728
    #> 9  Female 20-24 -4.594981  8.153665 40.75452   6.810036
    #> 10 Female 25-29 -3.767024  9.090355 58.74693   4.659498
    #> 11 Female 30-39 -8.634405 18.301023 76.87199   8.960573
    #> 12 Female 40-49 -7.537631 13.143567 49.36128  10.752688
    #> 13 Female 50-59 -4.498207 10.945480 35.17879   7.526882
    #> 14 Female 60-69 -2.953404 12.640834 43.72819   3.584229
    #> 
    #> $per_tract
    #>            tract_id          mean      rmse worst_bracket
    #> 1   317070105000001  -4.559499105  8.042145    Male 30-39
    #> 2   317070105000008  -5.817392760  6.508973  Female 30-39
    #> 3   317070105000009  -6.525675945  9.452732  Female 50-59
    #> 4   317070105000010  -4.178874830  8.154544  Female 60-69
    #> 5   317070105000013  -5.931557880  7.924279    Male 50-59
    #> 6   317070105000014  -4.797042481  6.637974  Female 60-69
    #> 7   317070105000015  -3.279786993  4.869772  Female 60-69
    #> 8   317070105000016  -3.730706410  5.979425    Male 40-49
    #> 9   317070105000017  -8.153168938 10.445935  Female 30-39
    #> 10  317070105000019  -3.441251999  5.645363    Male 60-69
    #> 11  317070105000021  -5.159772519  7.363183    Male 40-49
    #> 12  317070105000022  -2.972061822  5.124712  Female 25-29
    #> 13  317070105000023  -4.791757836  7.658873    Male 30-39
    #> 14  317070105000024  -4.737669076  7.037065    Male 25-29
    #> 15  317070105000025  -3.621190773  7.397953  Female 50-59
    #> 16  317070105000026  -2.616823935  4.447516  Female 30-39
    #> 17  317070105000028  -7.987001155 15.477373  Female 30-39
    #> 18  317070105000029  -5.503456559  7.982523  Female 40-49
    #> 19  317070105000031  -3.967276747  5.486863  Female 60-69
    #> 20  317070105000032  -5.376395712  8.290350    Male 20-24
    #> 21  317070105000033  -3.879079089  6.010987    Male 60-69
    #> 22  317070105000035  -4.305278323  6.026978  Female 30-39
    #> 23  317070105000036  -5.348486587  9.068216    Male 30-39
    #> 24  317070105000037  -3.768021776  5.766189  Female 60-69
    #> 25  317070105000039  -5.277827758  7.210824    Male 40-49
    #> 26  317070105000040  -5.646144655 14.743876    Male 50-59
    #> 27  317070105000041  -4.455607134  8.435646  Female 50-59
    #> 28  317070105000043  -2.993832041  5.159039    Male 40-49
    #> 29  317070105000044  -5.014125006 14.135927    Male 40-49
    #> 30  317070105000045  -5.647897544  9.526189  Female 40-49
    #> 31  317070105000047  -6.999989265 12.008012  Female 50-59
    #> 32  317070105000048  -4.209556734  7.569161    Male 30-39
    #> 33  317070105000049  -3.941751954  5.488531  Female 30-39
    #> 34  317070105000050  -5.176019395  8.946001  Female 60-69
    #> 35  317070105000051  -3.227461851  4.572083  Female 40-49
    #> 36  317070105000052  -5.075770996  7.014303  Female 40-49
    #> 37  317070105000054  -4.442495164  7.946705  Female 60-69
    #> 38  317070105000055  -4.390435462 10.719426  Female 50-59
    #> 39  317070105000056  -2.711726977  5.726792  Female 60-69
    #> 40  317070105000057  -3.616608517  7.121060  Female 60-69
    #> 41  317070105000059  -5.651705104  7.889638  Female 40-49
    #> 42  317070105000060  -5.790153105  7.271156    Male 50-59
    #> 43  317070105000061  -4.641782043  6.681543  Female 50-59
    #> 44  317070105000063  -4.383467078  8.409008  Female 60-69
    #> 45  317070105000065  -6.603790275  9.260719    Male 60-69
    #> 46  317070105000066  -5.740852701 11.129849    Male 60-69
    #> 47  317070105000067  -3.784408463  7.293882  Female 50-59
    #> 48  317070105000069  -7.222193384  9.665045    Male 30-39
    #> 49  317070105000070  -5.224082199  8.418305  Female 50-59
    #> 50  317070105000071  -5.103918010  8.003257  Female 60-69
    #> 51  317070105000074  -4.713231324  7.626746  Female 60-69
    #> 52  317070105000077  -4.014451494  9.175202  Female 60-69
    #> 53  317070105000078  -5.372129372  8.978801  Female 60-69
    #> 54  317070105000079  -5.519995512  8.232696  Female 40-49
    #> 55  317070105000082  -5.768358792  8.797538    Male 60-69
    #> 56  317070105000083  -4.762651150  8.180109  Female 50-59
    #> 57  317070105000086  -5.901150768  9.075647    Male 40-49
    #> 58  317070105000087  -5.426857241  8.490484  Female 50-59
    #> 59  317070105000089  -7.601965474 10.792977  Female 40-49
    #> 60  317070105000091  -5.986561842  8.902330  Female 60-69
    #> 61  317070105000094  -7.047298836  9.425062  Female 60-69
    #> 62  317070105000095  -7.177295583  9.361397    Male 40-49
    #> 63  317070105000096  -6.985742597 10.537407  Female 40-49
    #> 64  317070105000100 -10.100013077 15.473248  Female 40-49
    #> 65  317070105000101  -4.805998429  8.572757    Male 50-59
    #> 66  317070105000102  -9.600321553 14.281835  Female 50-59
    #> 67  317070105000103  -3.485435366  6.780881  Female 60-69
    #> 68  317070105000104  -5.146516573 10.326812  Female 60-69
    #> 69  317070105000105  -6.752081376 10.766817  Female 60-69
    #> 70  317070105000106  -4.824688452  7.758505  Female 60-69
    #> 71  317070105000107  -5.122550583  9.334595  Female 60-69
    #> 72  317070105000108  -4.594410447 10.428848  Female 60-69
    #> 73  317070105000112 -10.783400289 19.169802  Female 40-49
    #> 74  317070105000114  -7.540679964 12.078729  Female 50-59
    #> 75  317070105000115  -7.767283852 11.974748  Female 50-59
    #> 76  317070105000116  -5.109025610 10.132901    Male 50-59
    #> 77  317070105000117  -6.005351610  9.535741  Female 40-49
    #> 78  317070105000120  -4.976812928 14.786015  Female 30-39
    #> 79  317070105000121  -1.474780697  5.321110    Male 50-59
    #> 80  317070105000124  -9.454155052 12.625417  Female 30-39
    #> 81  317070105000125  -7.963680534 10.068015  Female 40-49
    #> 82  317070105000126  -4.357447510  6.378833  Female 60-69
    #> 83  317070105000127  -4.858176667  7.489459  Female 50-59
    #> 84  317070105000128  -6.625455365  9.590299    Male 60-69
    #> 85  317070105000129  -6.010697106  8.874241  Female 50-59
    #> 86  317070105000130  -5.749168601  6.748761    Male 20-24
    #> 87  317070105000131  -6.874421785 11.497939  Female 60-69
    #> 88  317070105000133  -6.152771191 13.877509  Female 60-69
    #> 89  317070105000134  -3.144991738  7.000318  Female 50-59
    #> 90  317070105000135  -4.986783383  8.782297  Female 60-69
    #> 91  317070105000136  -1.889998347  5.609723  Female 30-39
    #> 92  317070105000138  -1.812816386  3.358284  Female 60-69
    #> 93  317070105000139  -2.892750619  7.005628    Male 50-59
    #> 94  317070105000140  -1.702602786  4.920442    Male 50-59
    #> 95  317070105000141  -1.964337987  5.143543    Male 50-59
    #> 96  317070105000142  -1.908293409  3.862864    Male 40-49
    #> 97  317070105000143  -2.027490158  3.780440    Male 50-59
    #> 98  317070105000144  -4.366981808  6.967249    Male 40-49
    #> 99  317070105000147  -4.529938153 11.733861    Male 40-49
    #> 100 317070105000148  -7.074702612  9.785243  Female 40-49
    #> 101 317070105000149  -4.127573226  9.515252  Female 50-59
    #> 102 317070105000150  -0.101504033  2.484096  Female 60-69
    #> 103 317070105000151  -9.578011582 14.932277    Male 50-59
    #> 104 317070105000152  -4.137780659  9.054076  Female 60-69
    #> 105 317070105000153  -5.226112858  7.297822    Male 40-49
    #> 106 317070105000154  -6.081579275  7.714759  Female 50-59
    #> 107 317070105000155  -4.490538487  5.945292  Female 50-59
    #> 108 317070105000156  -3.447380184  6.443840  Female 60-69
    #> 109 317070105000157  -7.508003735 14.036262  Female 40-49
    #> 110 317070105000158  -4.596145022  6.139220    Male 60-69
    #> 111 317070105000159  -6.659892214 15.049241  Female 40-49
    #> 112 317070105000160  -5.930079966 10.937638  Female 50-59
    #> 113 317070105000161  -8.501208114 12.990829  Female 50-59
    #> 114 317070105000162  -6.506503868 12.028199    Male 50-59
    #> 115 317070105000164  -2.335947999  8.553090  Female 30-39
    #> 116 317070105000165   0.219658119  1.100615  Female 25-29
    #> 117 317070105000166  -6.911469577  9.260399  Female 50-59
    #> 118 317070105000168  -7.684448430 10.094578  Female 40-49
    #> 119 317070105000169  -6.286034136  8.366596  Female 60-69
    #> 120 317070105000171  -5.754687079 14.031668  Female 40-49
    #> 121 317070105000172  -5.408751298 16.282554    Male 40-49
    #> 122 317070105000174  -4.796378155 16.399113    Male 30-39
    #> 123 317070105000175  -2.760672767 10.383235  Female 30-39
    #> 124 317070105000176  -1.610918456  3.598584  Female 30-39
    #> 125 317070105000177   1.719798967  7.178287  Female 30-39
    #> 126 317070105000178  -4.618270361 14.223881  Female 30-39
    #> 127 317070105000179  -4.125619406  6.772092  Female 60-69
    #> 128 317070105000180  -3.830638937  6.557574  Female 60-69
    #> 129 317070105000181  -3.116283671  6.689733    Male 60-69
    #> 130 317070105000182  -6.237384966 19.645876  Female 30-39
    #> 131 317070105000183  -9.476729870 19.482365  Female 40-49
    #> 132 317070105000185  -4.436989940  6.514733  Female 50-59
    #> 133 317070105000186  -4.346734596  8.910282  Female 60-69
    #> 134 317070105000188  -7.218671765 10.369760  Female 40-49
    #> 135 317070105000189  -4.560795227  7.906853  Female 60-69
    #> 136 317070105000190  -3.525752309  5.011938  Female 40-49
    #> 137 317070105000191  -2.209252730  5.407580  Female 60-69
    #> 138 317070105000192  -4.021250599  7.303359  Female 30-39
    #> 139 317070105000193  -3.466915262  8.664347    Male 30-39
    #> 140 317070105000194  -5.993749024  9.652451    Male 20-24
    #> 141 317070105000196  -7.729417560 23.369906  Female 30-39
    #> 142 317070105000197   0.079699282  1.605916    Male 30-39
    #> 143 317070105000198  -0.530342426  1.694565    Male 60-69
    #> 144 317070105000199  -5.773685460  9.795838  Female 40-49
    #> 145 317070105000200  -7.451537815 14.007476    Male 40-49
    #> 146 317070105000201  -7.221924585 11.987679  Female 50-59
    #> 147 317070105000205  -7.478312723 20.081069  Female 40-49
    #> 148 317070105000206  -7.583998902 18.684539  Female 40-49
    #> 149 317070105000207  -2.226276622  6.781661    Male 20-24
    #> 150 317070105000208  -5.825423247 15.240798  Female 30-39
    #> 151 317070105000209  -7.385281010 18.344320  Female 30-39
    #> 152 317070105000210  -5.258293422 12.681091  Female 40-49
    #> 153 317070105000211  -3.815364124 11.312495  Female 40-49
    #> 154 317070105000212  -4.697426602 11.476398  Female 40-49
    #> 155 317070105000213  -5.251402772 13.371039  Female 30-39
    #> 156 317070105000214  -9.017900202 26.055061  Female 30-39
    #> 157 317070105000216  -6.704923700 20.839160    Male 30-39
    #> 158 317070105000217  -3.029136859 11.525926  Female 30-39
    #> 159 317070105000219 -10.209721796 30.462281    Male 30-39
    #> 160 317070105000220  -2.285550735  6.190082    Male 20-24
    #> 161 317070105000221  -3.499971865  6.079196  Female 60-69
    #> 162 317070105000222  -2.160133191  4.576107    Male 60-69
    #> 163 317070105000223  -1.589892376  4.406073  Female 60-69
    #> 164 317070105000224  -2.621427865  7.123151  Female 25-29
    #> 165 317070105000225  -2.123928507  5.206267    Male 60-69
    #> 166 317070105000226  -2.080119928  4.495681    Male 25-29
    #> 167 317070105000227  -4.030275763  7.158562    Male 30-39
    #> 168 317070105000228  -2.961436752  4.854407  Female 20-24
    #> 169 317070105000229  -3.762827911  6.148631  Female 60-69
    #> 170 317070105000230  -3.458054684  5.073844  Female 60-69
    #> 171 317070105000231  -3.920216966  6.135808    Male 30-39
    #> 172 317070105000232  -2.912997333  4.337222    Male 60-69
    #> 173 317070105000233  -2.658217242  3.910833    Male 30-39
    #> 174 317070105000234  -2.084527396  4.714393    Male 20-24
    #> 175 317070105000235  -3.994848607  6.580694  Female 30-39
    #> 176 317070105000236  -2.338782057  4.753211  Female 20-24
    #> 177 317070105000237  -5.148975144  8.889681  Female 40-49
    #> 178 317070105000238  -3.169750616  6.891712  Female 60-69
    #> 179 317070105000239  -2.638798003  4.943243  Female 50-59
    #> 180 317070105000240  -2.605970815  4.815049  Female 60-69
    #> 181 317070105000241  -3.373292463  9.865236  Female 50-59
    #> 182 317070105000242  -5.582670609  9.020344    Male 30-39
    #> 183 317070105000243  -3.011605312  5.024262    Male 30-39
    #> 184 317070105000244  -2.996765407  5.868940    Male 20-24
    #> 185 317070105000245  -6.217101274  8.017804  Female 40-49
    #> 186 317070105000246  -4.886043894 14.355249  Female 30-39
    #> 187 317070105000247  -3.955582045  5.279757    Male 25-29
    #> 188 317070105000248  -3.719277711  6.311721  Female 60-69
    #> 189 317070105000249  -2.771875681  5.218794  Female 60-69
    #> 190 317070105000250  -2.199008645  6.274830    Male 40-49
    #> 191 317070105000251  -5.956430160 13.224822  Female 30-39
    #> 192 317070105000252  -6.237168100  9.470165  Female 40-49
    #> 193 317070105000253  -5.715401678 11.893279  Female 30-39
    #> 194 317070105000254  -4.526958652  7.297146    Male 50-59
    #> 195 317070105000255  -5.275423051  6.918454  Female 50-59
    #> 196 317070105000256  -3.612772112  7.024843    Male 40-49
    #> 197 317070105000257  -4.656291228  6.602483  Female 40-49
    #> 198 317070105000258  -3.647852724  6.576518  Female 40-49
    #> 199 317070105000259  -3.825448740 10.505665  Female 60-69
    #> 200 317070105000260  -2.731550595  6.119313  Female 60-69
    #> 201 317070105000261  -4.619740715  8.194501    Male 60-69
    #> 202 317070105000262  -2.920902247  6.721618    Male 60-69
    #> 203 317070105000263  -5.044023319  6.759129    Male 40-49
    #> 204 317070105000264  -3.482541180  7.359363    Male 60-69
    #> 205 317070105000265  -3.660105481  5.549348  Female 50-59
    #> 206 317070105000266  -3.154622939  5.333756  Female 60-69
    #> 207 317070105000267  -2.881227788  5.069382    Male 40-49
    #> 208 317070105000268  -3.454984991  4.729498    Male 30-39
    #> 209 317070105000269  -5.932591908  8.715663  Female 60-69
    #> 210 317070105000270  -2.632622073  7.343952  Female 40-49
    #> 211 317070105000271  -6.974226857 10.263487    Male 40-49
    #> 212 317070105000272  -3.039229532  6.055542    Male 20-24
    #> 213 317070105000273  -7.491179776 11.615983  Female 40-49
    #> 214 317070105000274  -3.613668886  7.091210    Male 40-49
    #> 215 317070105000275  -3.644286051  7.565706  Female 50-59
    #> 216 317070105000276  -1.175212613  2.253284    Male 50-59
    #> 217 317070105000277  -4.900492426 11.581359  Female 50-59
    #> 218 317070105000278  -3.523019213  8.271168  Female 60-69
    #> 219 317070105000279 -11.481031596 23.106166  Female 30-39
    #> 220 317070105000281  -6.576763531  8.454906    Male 30-39
    #> 221 317070105000282  -8.458377586 21.495595    Male 40-49
    #> 222 317070105000283  -4.379311424  6.925351  Female 60-69
    #> 223 317070105000284  -3.658112460  7.679305  Female 60-69
    #> 224 317070105000285  -0.526046462  2.498439    Male 50-59
    #> 225 317070105000286  -1.784384342  3.215404    Male 60-69
    #> 226 317070105000287  -4.332851166  8.304033    Male 50-59
    #> 227 317070105000288  -6.028301666 11.274869    Male 50-59
    #> 228 317070105000289  -3.985582181  4.990903  Female 40-49
    #> 229 317070105000290  -7.673251480 23.492149    Male 30-39
    #> 230 317070105000291  -4.319334689 12.754558  Female 30-39
    #> 231 317070105000292  -7.331603258 29.552802  Female 30-39
    #> 232 317070105000293  -6.835327075 25.357930    Male 30-39
    #> 233 317070105000294  -3.509332555 13.543536  Female 40-49
    #> 234 317070105000295  -6.895382517 15.077349  Female 30-39
    #> 235 317070105000296  -4.170449647  8.022644    Male 60-69
    #> 236 317070105000297  -4.176608168  5.105153    Male 20-24
    #> 237 317070105000298  -6.267813116 14.369969  Female 30-39
    #> 238 317070105000299  -4.432795031 13.130784  Female 30-39
    #> 239 317070105000300  -8.894887463 19.627627    Male 30-39
    #> 240 317070105000301  -7.220276832 20.302309  Female 30-39
    #> 241 317070105000302  -9.324472048 30.102949  Female 30-39
    #> 242 317070105000303  -9.980109554 28.480730    Male 30-39
    #> 243 317070105000304  -5.158310003 20.597483  Female 30-39
    #> 244 317070105000305  -3.653094690 11.340652  Female 30-39
    #> 245 317070105000307  -7.422074989 27.872345  Female 30-39
    #> 246 317070105000308  -7.826705440 23.418095  Female 25-29
    #> 247 317070105000309  -4.041079629 15.869342    Male 30-39
    #> 248 317070105000310  -6.213428914 21.444799  Female 25-29
    #> 249 317070105000311  -1.621793070  2.921288  Female 60-69
    #> 250 317070105000312  -5.612424546  9.680878  Female 40-49
    #> 251 317070105000313  -5.938483465 12.798353  Female 40-49
    #> 252 317070105000314  -2.920277439  4.243816  Female 30-39
    #> 253 317070105000315  -1.403449935  3.510497  Female 50-59
    #> 254 317070105000316  -1.259709176  4.502293    Male 40-49
    #> 255 317070105000317   0.007519648  2.166268    Male 50-59
    #> 256 317070105000318  -0.645777131  3.036960  Female 50-59
    #> 257 317070105000319 -10.903202863 34.911173  Female 25-29
    #> 258 317070105000320  -7.178136535 25.853881    Male 30-39
    #> 259 317070105000321  -2.589425279  4.368163  Female 60-69
    #> 260 317070105000322   0.084782532  1.464751    Male 20-24
    #> 261 317070105000323  -2.386502712  4.753391  Female 20-24
    #> 262 317070105000324  -4.180621087  7.458454  Female 40-49
    #> 263 317070105000325  -1.209934297  5.631450  Female 60-69
    #> 264 317070105000326  -0.748309305  3.530488  Female 50-59
    #> 265 317070105000327  -1.656007806  4.123553  Female 40-49
    #> 266 317070105000328  -1.167363189  3.907636  Female 50-59
    #> 267 317070105000329  -4.752467183  7.332858    Male 20-24
    #> 268 317070105000330  -5.473691091  8.484049    Male 20-24
    #> 269 317070105000331  -2.822873680  3.945492  Female 25-29
    #> 270 317070105000332  -2.585965004  4.624353    Male 50-59
    #> 271 317070105000333  -8.139100229 16.031355    Male 40-49
    #> 272 317070105000334  -4.886458621  8.283490  Female 40-49
    #> 273 317070105000335  -4.640303917 15.987679    Male 30-39
    #> 274 317070105000336  -6.624475403 23.739846  Female 30-39
    #> 275 317070105000337  -0.804551760  2.989844    Male 60-69
    #> 276 317070105000338  -6.009582627 17.871760  Female 25-29
    #> 277 317070105000339  -7.871425957 14.466566  Female 40-49
    #> 278 317070105000340  -3.530530272  6.090355    Male 60-69
    #> 279 317070105000342  -3.089515660  6.128272    Male 50-59
    #> 
    #> $overall
    #> $overall$rmse
    #> [1] 11.16139
    #> 
    #> $overall$mean_bias
    #> [1] -4.720941
    #> 
    #> $overall$total_deviance
    #> [1] 16164.46

### Interpretation

- **Raw residuals**: Scale depends on tract population. A residual of 10
  means different things in a tract with 50 people vs 5,000. Use Pearson
  or deviance for fair comparison.
- **Pearson residuals**: Standardized — values around $\pm 2$ are
  typical. Values $> 3$ are outliers under the Poisson assumption.
- **Bracket comparison**: If one bracket consistently has larger
  residuals, the model may have trouble with that demographic group —
  check whether the census data for that bracket is reliable.
- **Scatter plot**: Points should cluster tightly around the 45-degree
  line. Systematic departures indicate bias.

## 5. Weight Structure

### Intuition

The weight matrix $W$ (dimensions $n \times m$, tracts $\times$ sources)
is the heart of the interpolation. It determines how voters from each
station are allocated to each tract. Understanding the structure of $W$
reveals:

- **Catchment areas**: which stations serve which tracts
- **Concentration**: does a tract depend on one station or many?
- **Fragility**: tracts dependent on a single station are sensitive to
  that station’s data

### Formal definition

Since $W$ is column-normalized ($\sum_{i}W_{ij} = 1$), the row sums are
NOT 1. For concentration measures, we row-normalize:
$p_{ij} = W_{ij}/\sum_{j}W_{ij}$.

- **Shannon entropy**: $H_{i} = - \sum_{j}p_{ij}\ln p_{ij}$
- **Effective number of sources**: $\exp\left( H_{i} \right)$ — the
  equivalent number of equal-weight stations
- **Herfindahl-Hirschman Index**: $\text{HHI}_{i} = \sum_{j}p_{ij}^{2}$
  — ranges from $1/m$ (perfect equality) to 1 (monopoly by one station)

### Tabular summary: `weight_summary()`

``` r
head(weight_summary(result_vga), 15)
```

    #>                        tract_id dominant_source dominant_weight top3_weight
    #> 317070105000001 317070105000001              26     0.113710351  0.12612049
    #> 317070105000008 317070105000008              26     0.035960075  0.07200471
    #> 317070105000009 317070105000009              14     0.020939918  0.04571853
    #> 317070105000010 317070105000010              14     0.032840371  0.05289709
    #> 317070105000013 317070105000013              14     0.020836243  0.05527316
    #> 317070105000014 317070105000014              14     0.024706738  0.04238994
    #> 317070105000015 317070105000015              10     0.040361520  0.04864066
    #> 317070105000016 317070105000016              10     0.011528280  0.02630788
    #> 317070105000017 317070105000017              10     0.020663628  0.05037505
    #> 317070105000019 317070105000019               9     0.009757796  0.02103461
    #> 317070105000021 317070105000021               9     0.014567442  0.02916011
    #> 317070105000022 317070105000022              12     0.008976594  0.02271826
    #> 317070105000023 317070105000023              12     0.021697788  0.05338061
    #> 317070105000024 317070105000024              15     0.014425963  0.04110649
    #> 317070105000025 317070105000025               9     0.044096258  0.05680156
    #>                   entropy effective_n_sources herfindahl
    #> 317070105000001 0.7273602            2.069610 0.73383488
    #> 317070105000008 3.0339539           20.779228 0.08094583
    #> 317070105000009 3.3561843           28.679550 0.04519548
    #> 317070105000010 2.8815606           17.842096 0.11191140
    #> 317070105000013 3.1993921           24.517621 0.05541503
    #> 317070105000014 3.1961708           24.438771 0.06344365
    #> 317070105000015 1.9450532            6.994004 0.35883469
    #> 317070105000016 3.3320708           27.996255 0.04656403
    #> 317070105000017 3.4162950           30.456364 0.04057307
    #> 317070105000019 3.4676934           32.062700 0.03637372
    #> 317070105000021 3.4604435           31.831091 0.03672944
    #> 317070105000022 3.3544168           28.628903 0.04327622
    #> 317070105000023 3.1026232           22.256257 0.06090369
    #> 317070105000024 3.2173313           24.961417 0.05158218
    #> 317070105000025 2.4817967           11.962738 0.19984869
    #>                 mean_travel_time_weighted
    #> 317070105000001                 0.3126993
    #> 317070105000008                 3.2039284
    #> 317070105000009                 4.6225923
    #> 317070105000010                 2.0331058
    #> 317070105000013                 3.2192116
    #> 317070105000014                 2.8360734
    #> 317070105000015                 0.7293393
    #> 317070105000016                 2.5934512
    #> 317070105000017                 6.4391421
    #> 317070105000019                 4.3478902
    #> 317070105000021                 6.3769464
    #> 317070105000022                 3.3738270
    #> 317070105000023                 3.6591524
    #> 317070105000024                 3.9138005
    #> 317070105000025                 1.5455184
    #> 
    #> ... (279 rows total)

### Entropy map: `plot_weights(type = "entropy")`

``` r
plot_weights(result_vga, type = "entropy")
```

![](figures/diag-s5-wt-vga-entropy.png)

Large city comparison:

``` r
plot_weights(result_bh, type = "entropy")
```

![](figures/diag-s5-wt-bh-entropy.png)

### Dominant station map: `plot_weights(type = "dominant")`

``` r
plot_weights(result_vga, type = "dominant")
```

![](figures/diag-s5-wt-vga-dominant.png)

``` r
plot_weights(result_nit, type = "dominant")
```

![](figures/diag-s5-wt-nit-dominant.png)

### Catchment mode: `plot_weights(type = "catchment")`

Auto-selects the tract with highest effective number of sources:

``` r
plot_weights(result_vga, type = "catchment")
```

![](figures/diag-s5-wt-vga-catchment-auto.png)

Filtered view with `top_k` and `threshold`:

``` r
plot_weights(result_vga, type = "catchment", tract = 1,
             top_k = 3, threshold = 0.05)
```

![](figures/diag-s5-wt-vga-catchment-filt.png)

### Connections: `plot_connections()`

Overview — one spoke per tract to its dominant station:

``` r
plot_connections(result_vga)
```

![](figures/diag-s5-conn-vga-overview.png)

Large city:

``` r
plot_connections(result_bh)
```

![](figures/diag-s5-conn-bh-overview.png)

Detail for a specific tract:

``` r
plot_connections(result_vga, tract = 1)
```

![](figures/diag-s5-conn-vga-detail1.png)

### Interpretation

- **Low entropy tracts** (effective N close to 1): The interpolation for
  this tract depends almost entirely on a single station. If that
  station’s data is atypical, the interpolated values may be unreliable.
- **Dominant station map**: Produces a Voronoi-like partition. If
  catchment areas don’t match geographic intuition, check the travel
  time matrix.
- **Connections**: The spoke pattern should make geographic sense —
  tracts should connect to nearby stations. Long-distance connections
  suggest routing issues.

## 6. Travel Time Data Quality: `plot_travel_times()`

### Intuition

The travel time matrix is the input that drives everything: $\alpha$
optimization, weight computation, and ultimately the spatial allocation
of voters. Poor travel time data (unreachable pairs, disconnected road
networks) directly degrades interpolation quality.

### Applied examples

**Histogram** — distribution of all $n \times m$ travel times:

``` r
plot_travel_times(result_vga, type = "histogram")
```

![](figures/diag-s6-tt-vga-hist.png)

Large city:

``` r
plot_travel_times(result_bh, type = "histogram")
```

![](figures/diag-s6-tt-bh-hist.png)

**Heatmap** — rows = tracts (ordered by latitude), columns = stations.
Block-diagonal structure reveals spatial locality:

``` r
plot_travel_times(result_vga, type = "heatmap")
```

![](figures/diag-s6-tt-vga-heatmap.png)

**Map** — station points colored by travel time for a specific tract:

``` r
plot_travel_times(result_vga, type = "map", tract = 1)
```

![](figures/diag-s6-tt-vga-map.png)

### Interpretation

- **Spike at maximum value**: These are “fill” values for unreachable
  pairs. A large spike means the routing engine couldn’t find paths for
  many pairs.
- **Bimodal distribution**: May indicate disconnected graph components
  (e.g., separated by a river without bridge in the OSM network).
- **Block-diagonal heatmap**: Expected — nearby tracts have similar
  travel times to the same stations. Absence of block structure suggests
  the travel times lack spatial coherence.

## 7. Spatial Autocorrelation: `plot_residual_autocorrelation()` + `plot_moran()`

### Intuition

**Residual autocorrelation**: If residuals are spatially clustered
(nearby tracts have similar residuals), the model is missing spatial
structure — a misspecification signal.

**Moran’s I on interpolated variables**: If the interpolated vote shares
show expected spatial concentration (e.g., PT strongholds in low-income
areas), the method is preserving realistic political geography.

### Formal definition

**Global Moran’s I**:
$$I = \frac{n}{\sum\limits_{i,j}w_{ij}} \cdot \frac{\sum\limits_{i,j}w_{ij}\left( x_{i} - \bar{x} \right)\left( x_{j} - \bar{x} \right)}{\sum\limits_{i}\left( x_{i} - \bar{x} \right)^{2}}$$

where $w_{ij}$ are spatial weights (queen contiguity). Range:
$\lbrack - 1, + 1\rbrack$. Positive = clustering, $\approx 0$ = random,
negative = dispersion.

**LISA (Local Moran’s I)**:
$I_{i} = \frac{\left( x_{i} - \bar{x} \right)}{s^{2}}\sum_{j}w_{ij}\left( x_{j} - \bar{x} \right)$.
Classification: HH (high-high cluster), LL (low-low), HL (high
surrounded by low), LH (low surrounded by high).

### Applied examples: Residual autocorrelation

``` r
plot_residual_autocorrelation(result_vga)
```

![](figures/diag-s7-moran-resid-vga.png)

Pearson residuals:

``` r
plot_residual_autocorrelation(result_vga, residual_type = "pearson")
```

![](figures/diag-s7-moran-resid-vga-pear.png)

### Applied examples: LISA cluster maps

``` r
plot_moran(result_vga, variable = "Lula", type = "lisa")
```

![](figures/diag-s7-lisa-vga-lula.png)

Moran scatterplot:

``` r
plot_moran(result_vga, variable = "Lula", type = "moran")
```

![](figures/diag-s7-moran-vga-lula.png)

Large city:

``` r
plot_moran(result_bh, variable = "Lula", type = "lisa")
```

![](figures/diag-s7-lisa-bh-lula.png)

By party:

``` r
plot_moran(result_nit, variable = "PT", type = "lisa")
```

![](figures/diag-s7-lisa-nit-pt.png)

### Interpretation

- **Significant Moran’s I on residuals** (p \< 0.05): The model has
  spatially structured error. Consider whether the travel time data has
  systematic problems in certain areas.
- **HH clusters of Lula votes**: Expected in low-income neighborhoods
  (PT strongholds). Their presence validates that the interpolation
  preserves known political geography.
- **HL/LH clusters**: Spatial outliers — tracts with high vote share
  surrounded by low (or vice versa). May indicate local effects or data
  irregularities.

## 8. Baseline Comparisons: `compare_baselines()` + `leave_one_out()`

### Intuition

Is the optimized IDW actually better than simpler alternatives? And how
robust is the interpolation to removing individual stations?

### Formal definition

**Baselines**: - **Nearest**: $W_{ij^{*}} = 1$ for
$j^{*} = \arg\min_{j}t_{ij}$, $W_{ij} = 0$ otherwise (binary assignment
to closest station) - **Uniform**: $W_{ij} = 1/m$ (equal weight to all
stations, column-normalized) - **Areal**: $W$ based on spatial
intersection areas between tract and station Voronoi cells

**Leave-one-out**: Remove station $j$, predict its demographics using
remaining $m - 1$ stations with the full-model alpha values. Compare
predicted vs actual. This is the only form of out-of-sample validation
possible (there is no tract-level ground truth).

### Applied examples: `compare_baselines()`

``` r
compare_baselines(result_vga)
```

    #> Baseline comparison
    #> ---------------------------------------------------------------------- 
    #>   Optimized IDW          RMSE=11.16  Deviance=16164.46
    #>   Nearest station        RMSE=36.79  Deviance=157454.96  (IDW 70% better)
    #>   Uniform weights        RMSE=15.81  Deviance=34553.92  (IDW 29% better)
    #> ----------------------------------------------------------------------

``` r
compare_baselines(result_bh, methods = c("nearest", "uniform"))
```

    #> Baseline comparison
    #> ---------------------------------------------------------------------- 
    #>   Optimized IDW          RMSE=7.07  Deviance=143495.36
    #>   Nearest station        RMSE=27.00  Deviance=1393922.64  (IDW 74% better)
    #>   Uniform weights        RMSE=12.96  Deviance=477898.82  (IDW 45% better)
    #> ----------------------------------------------------------------------

### Applied examples: `leave_one_out()`

``` r
leave_one_out(result_igr)
```

    #> 
    #>   LOO: station 1 / 17
    #>   LOO: station 2 / 17
    #>   LOO: station 3 / 17
    #>   LOO: station 4 / 17
    #>   LOO: station 5 / 17
    #>   LOO: station 6 / 17
    #>   LOO: station 7 / 17
    #>   LOO: station 8 / 17
    #>   LOO: station 9 / 17
    #>   LOO: station 10 / 17
    #>   LOO: station 11 / 17
    #>   LOO: station 12 / 17
    #>   LOO: station 13 / 17
    #>   LOO: station 14 / 17
    #>   LOO: station 15 / 17
    #>   LOO: station 16 / 17
    #>   LOO: station 17 / 17
    #> Leave-one-out RMSE: 7.2895 (mean), 7.1405 (median)
    #> Full model RMSE:    6.7769
    #> LOO degradation:    7.6%

``` r
leave_one_out(result_vga)
```

    #> 
    #>   LOO: station 1 / 30
    #>   LOO: station 2 / 30
    #>   LOO: station 3 / 30
    #>   LOO: station 4 / 30
    #>   LOO: station 5 / 30
    #>   LOO: station 6 / 30
    #>   LOO: station 7 / 30
    #>   LOO: station 8 / 30
    #>   LOO: station 9 / 30
    #>   LOO: station 10 / 30
    #>   LOO: station 11 / 30
    #>   LOO: station 12 / 30
    #>   LOO: station 13 / 30
    #>   LOO: station 14 / 30
    #>   LOO: station 15 / 30
    #>   LOO: station 16 / 30
    #>   LOO: station 17 / 30
    #>   LOO: station 18 / 30
    #>   LOO: station 19 / 30
    #>   LOO: station 20 / 30
    #>   LOO: station 21 / 30
    #>   LOO: station 22 / 30
    #>   LOO: station 23 / 30
    #>   LOO: station 24 / 30
    #>   LOO: station 25 / 30
    #>   LOO: station 26 / 30
    #>   LOO: station 27 / 30
    #>   LOO: station 28 / 30
    #>   LOO: station 29 / 30
    #>   LOO: station 30 / 30
    #> Leave-one-out RMSE: 12.1146 (mean), 12.0922 (median)
    #> Full model RMSE:    11.1614
    #> LOO degradation:    8.5%

### Interpretation

- **Positive improvement %**: The optimized IDW has lower RMSE/deviance
  than the baseline. The optimization adds value.
- **Negative improvement %**: This should not happen. If it does, the
  optimization may have overfitted or the data has issues.
- **LOO degradation**: LOO RMSE should be comparable to full-model RMSE.
  If LOO RMSE is much larger, the model may be overfitting to specific
  station configurations.

## 9. Ecological Validation

### 9a. Implied Turnout Rates: `plot_turnout_rates()`

#### Intuition

For each tract and demographic bracket, the implied turnout rate is:

$$\text{rate}_{i,b} = \frac{\text{fitted voters}_{i,b}}{\text{census population}_{i,b}}$$

In Brazil, voting is **compulsory** for ages 18–69, so expected turnout
is ~75–85%. For 16–17 and 70+, voting is optional and rates should be
lower. Rates exceeding 100% indicate tracts where the model allocates
more voters than residents — signaling boundary effects or registration
mismatches.

#### Applied examples

Bracket boxplot:

``` r
plot_turnout_rates(result_vga, type = "bracket")
```

![](figures/diag-s9-turnout-vga-bracket.png)

``` r
plot_turnout_rates(result_nit, type = "bracket")
```

![](figures/diag-s9-turnout-nit-bracket.png)

Spatial pattern:

``` r
plot_turnout_rates(result_bh, type = "map")
```

![](figures/diag-s9-turnout-bh-map.png)

Faceted histogram:

``` r
plot_turnout_rates(result_vga, type = "histogram")
```

![](figures/diag-s9-turnout-vga-hist.png)

#### Interpretation

- **Compulsory brackets clustering around 80%**: Expected for ages
  18–69.
- **Optional brackets lower**: 16–17 and 70+ should show lower turnout.
- **Rates \> 100%**: Voters registered in one municipality who may
  actually reside in another (commuters, students). These boundary
  effects are a known limitation of the model.

### 9b. Income-Vote Correlation: `plot_income()`

#### Intuition

The negative correlation between PT vote share and income is one of the
best-documented patterns in Brazilian political science (the “PT-income
gradient”). If the interpolation preserves this gradient, it builds
confidence that the spatial allocation of votes is realistic. If it
destroys it, something is wrong.

Income data comes from censobr (Census Bureau microdata), which provides
mean household head income by tract. No deflation is applied — within a
single census year, the correlation is scale-invariant.

#### Applied examples

Lula (PT) vs income — expect negative correlation:

``` r
plot_income(result_vga, variable = "Lula", type = "scatter")
```

![](figures/diag-s9-income-vga-lula.png)

Bolsonaro (PL) vs income — expect positive correlation:

``` r
plot_income(result_vga, variable = 22, type = "scatter")
```

![](figures/diag-s9-income-vga-bolso.png)

Larger city:

``` r
plot_income(result_bh, variable = "Lula", type = "scatter")
```

![](figures/diag-s9-income-bh-lula.png)

Side-by-side maps:

``` r
plot_income(result_nit, variable = "Lula", type = "map")
```

![](figures/diag-s9-income-nit-map.png)

### 9c. Cross-Round Consistency: `plot_ecological()`

#### Intuition

Political geography is sticky. Neighborhoods that vote strongly for a
candidate in round 1 should vote even more strongly in round 2 (where
only 2 candidates remain). A high correlation between turno 1 and turno
2 tract-level vote shares validates that the interpolation preserves
spatial patterns.

#### Applied examples

``` r
plot_ecological(result_vga, result_vga2,
                variable = "Lula", type = "scatter")
```

![](figures/diag-s9-ecol-vga-scatter.png)

Side-by-side maps:

``` r
plot_ecological(result_vga, result_vga2,
                variable = "Lula", type = "map")
```

![](figures/diag-s9-ecol-vga-map.png)

#### Interpretation

- **Strong positive correlation** between rounds: The spatial patterns
  are consistent, as expected. The method is recovering stable partisan
  geography.
- **Weak or no correlation**: The interpolation may be injecting noise
  rather than recovering signal. Investigate whether the travel time
  data or optimization is causing problems.

## 10. Quick Reference

| Function                                                                                                                     | Question it answers                   | Key arguments                         | When to worry                            |
|:-----------------------------------------------------------------------------------------------------------------------------|:--------------------------------------|:--------------------------------------|:-----------------------------------------|
| [`diagnostics()`](https://antrologos.github.io/interpElections/reference/diagnostics.md)                                     | Is anything obviously wrong?          | `verbose`                             | Any FAIL or multiple WARNs               |
| [`plot_convergence()`](https://antrologos.github.io/interpElections/reference/plot_convergence.md)                           | Did the optimizer converge?           | `which`, `log_y`                      | Oscillating loss, non-shrinking gradient |
| [`plot_alpha()`](https://antrologos.github.io/interpElections/reference/plot_alpha.md)                                       | How does distance decay vary?         | `type`, `summary_fn`, `brackets`      | Many tracts with $\alpha > 15$           |
| [`plot_residuals()`](https://antrologos.github.io/interpElections/reference/plot_residuals.md)                               | How well are demographics reproduced? | `type`, `residual_type`, `summary_fn` | Large systematic bias, Pearson \> 3      |
| [`residual_summary()`](https://antrologos.github.io/interpElections/reference/residual_summary.md)                           | Which brackets/tracts fit worst?      | `type`                                | One bracket much worse than others       |
| [`weight_summary()`](https://antrologos.github.io/interpElections/reference/weight_summary.md)                               | How concentrated are the weights?     | —                                     | Many tracts with effective N = 1         |
| [`plot_weights()`](https://antrologos.github.io/interpElections/reference/plot_weights.md)                                   | Where are catchment areas?            | `type`, `tract`, `top_k`              | Catchments that defy geography           |
| [`plot_connections()`](https://antrologos.github.io/interpElections/reference/plot_connections.md)                           | Which stations serve which tracts?    | `tract`, `top_k`, `threshold`         | Long-distance connections                |
| [`plot_travel_times()`](https://antrologos.github.io/interpElections/reference/plot_travel_times.md)                         | Is the travel time matrix clean?      | `type`, `tract`                       | Large spike at max value                 |
| [`plot_residual_autocorrelation()`](https://antrologos.github.io/interpElections/reference/plot_residual_autocorrelation.md) | Are residuals spatially clustered?    | `residual_type`, `summary_fn`         | Significant Moran’s I (p \< 0.05)        |
| [`plot_moran()`](https://antrologos.github.io/interpElections/reference/plot_moran.md)                                       | Is vote geography realistic?          | `variable`, `type`                    | No spatial clustering                    |
| [`compare_baselines()`](https://antrologos.github.io/interpElections/reference/compare_baselines.md)                         | Is IDW better than naive methods?     | `methods`                             | Negative improvement                     |
| [`leave_one_out()`](https://antrologos.github.io/interpElections/reference/leave_one_out.md)                                 | Is the model overfitting?             | `max_stations`                        | LOO RMSE \>\> full RMSE                  |
| [`plot_turnout_rates()`](https://antrologos.github.io/interpElections/reference/plot_turnout_rates.md)                       | Are implied turnout rates plausible?  | `type`                                | Many tracts \> 100%                      |
| [`plot_income()`](https://antrologos.github.io/interpElections/reference/plot_income.md)                                     | Does the PT-income gradient hold?     | `variable`, `type`                    | Wrong sign on correlation                |
| [`plot_ecological()`](https://antrologos.github.io/interpElections/reference/plot_ecological.md)                             | Are spatial patterns consistent?      | `variable`, `type`                    | Weak cross-round correlation             |
