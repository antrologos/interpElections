# Prepare Brazilian census population data by age bracket per census tract

Downloads census population data from IBGE (via the `censobr` package),
groups ages into brackets, and returns a data frame with one row per
census tract. Supports census years 2000, 2010, and 2022.

## Usage

``` r
br_prepare_population(code_muni, year = 2010)
```

## Arguments

- code_muni:

  Numeric or character vector. IBGE municipality codes.

- year:

  Integer. Census year: 2000, 2010, or 2022. Default: 2010.

## Value

A data frame with columns: `code_muni`, `code_tract`, and population
bracket columns (`pop_*`). The exact brackets depend on the census year
(see Details).

## Details

Requires the `censobr`, `dplyr`, `tidyr`, and `data.table` packages.

**Census 2000 and 2010** produce the following voting-age brackets:
`pop_18_20`, `pop_21_24`, `pop_25_29`, `pop_30_39`, `pop_40_49`,
`pop_50_59`, `pop_60_69`.

**Census 2022** produces: `pop_15_19`, `pop_20_24`, `pop_25_29`,
`pop_30_39`, `pop_40_49`, `pop_50_59`, `pop_60_69`.

All years also produce: `pop_00_04`, `pop_05_09`, `pop_10_14`,
`pop_15_17` (or `pop_15_19` for 2022), `pop_70mais`.

Additionally, all years produce gender x literacy columns for the same
voting-age brackets: `pop_hom_alf_*`, `pop_hom_nalf_*`, `pop_mul_alf_*`,
`pop_mul_nalf_*` (literate/illiterate men/women). These are used by the
"full" calibration mode in
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

## See also

Other Brazil helpers:
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md),
[`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Census 2010 population for Sao Paulo
pop <- br_prepare_population(code_muni = "3550308", year = 2010)
head(pop)
} # }
```
