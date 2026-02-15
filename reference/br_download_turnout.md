# Download turnout/attendance data from TSE

Downloads the official turnout detail data (detalhe da votação por
seção) from the TSE open data portal. This file provides attendance
counts, abstentions, and vote type breakdowns per polling section.

## Usage

``` r
br_download_turnout(
  year,
  uf = NULL,
  code_muni_tse = NULL,
  cargo = NULL,
  turno = 1L,
  force = FALSE,
  cache = TRUE,
  verbose = TRUE
)
```

## Arguments

- year:

  Integer. Election year (e.g., 2008, 2012, 2016, 2020).

- uf:

  Character or NULL. Two-letter state abbreviation. Used to filter the
  nationwide file to a single state.

- code_muni_tse:

  Character or NULL. 5-digit TSE municipality code. If provided, filters
  results to this municipality only.

- cargo:

  Integer or NULL. Electoral office code to filter. If NULL, uses any
  office (turnout is the same across offices).

- turno:

  Integer. Election round (1 or 2). Default: 1.

- force:

  Logical. Re-download even if cached file exists. Default: FALSE.

- cache:

  Logical. If TRUE (default), downloaded files are stored persistently.
  See
  [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md).

- verbose:

  Logical. Default: TRUE.

## Value

A data frame with one row per section per office, using the original TSE
column names. Key columns:

- ANO_ELEICAO:

  Election year

- CD_MUNICIPIO:

  TSE municipality code (5-digit)

- NR_ZONA:

  Electoral zone number

- NR_SECAO:

  Section number

- CD_CARGO:

  Office code

- QT_COMPARECIMENTO:

  Number of voters who attended

- QT_APTOS:

  Number of eligible voters

- QT_ABSTENCOES:

  Number of abstentions

## Details

The TSE publishes this data as a single nationwide file at
`https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/`.
The file can be large (hundreds of MB). Results are filtered by
municipality and/or state immediately after reading.

## See also

[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
for candidate vote data,
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)
which can use this function internally,
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
to manage cached downloads.

Other Brazil downloads:
[`br_download_geocode()`](https://antrologos.github.io/interpElections/reference/br_download_geocode.md),
[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
