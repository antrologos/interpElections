# Plot income-vote ecological correlation

Downloads tract-level household head income via `censobr` and produces a
scatter plot or side-by-side map of income vs. vote share.

## Usage

``` r
plot_income(
  result,
  variable = NULL,
  census_year = NULL,
  type = c("scatter", "map"),
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object (Brazilian).

- variable:

  Variable to correlate with income. Accepts column name, ballot number,
  candidate name, or party abbreviation.

- census_year:

  Census year for income data. NULL auto-detects from
  `result$census_year`. Must be 2000, 2010, or 2022.

- type:

  Character. `"scatter"` (default) or `"map"`.

- ...:

  Ignored.

## Value

A `ggplot` object (invisibly). Prints the plot.

## Details

Income data is downloaded from IBGE via the `censobr` package. No
deflation is applied — values are used as-is for within-year correlation
analysis.

Variable sources by census year:

- **2000**: `censobr::read_tracts(2000, "Basico")`, variable `VAR06`

- **2010**: `censobr::read_tracts(2010, "ResponsavelRenda")`, computed
  as `V022 / V020`

- **2022**: `censobr::read_tracts(2022, "ResponsavelRenda")`, variable
  `V06004`
