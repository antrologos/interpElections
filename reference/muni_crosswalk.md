# IBGE-TSE Municipality Code Crosswalk

Official crosswalk table mapping IBGE 7-digit municipality codes to TSE
5-digit electoral codes for all 5,710 Brazilian municipalities. Sourced
from GV-CEPESP.

## Usage

``` r
muni_crosswalk
```

## Format

A data frame with 5,710 rows and 4 columns:

- code_ibge:

  Character. IBGE 7-digit municipality code.

- code_tse:

  Character. TSE 5-digit municipality code.

- uf:

  Character. Two-letter state abbreviation (e.g., "SP", "RJ").

- nome:

  Character. Municipality name in uppercase.

## Source

GV-CEPESP, FGV:
<https://github.com/GV-CEPESP/cepespdata/blob/main/tabelas_auxiliares/dados/codigo_municipio_ibge_tse.csv>

## Examples

``` r
data(muni_crosswalk)
# Look up TSE code for Sao Paulo (IBGE 3550308)
muni_crosswalk[muni_crosswalk$code_ibge == "3550308", ]
#>      code_ibge code_tse uf      nome
#> 5360   3550308    71072 SP SÃO PAULO
```
