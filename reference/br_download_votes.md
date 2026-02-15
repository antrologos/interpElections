# Download candidate vote data from TSE

Downloads the official candidate vote data (votação por seção eleitoral)
from the TSE open data portal for a given election year and state. The
data is downloaded as a ZIP file containing a semicolon-delimited CSV.

## Usage

``` r
br_download_votes(
  year,
  uf,
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

  Character. Two-letter state abbreviation (e.g., `"SP"`, `"RJ"`), or
  `"BR"` to download the national file (used for presidential data).

- code_muni_tse:

  Character or NULL. 5-digit TSE municipality code. If provided, filters
  results to this municipality only.

- cargo:

  Integer or NULL. Electoral office code to filter (e.g., 13 = Vereador,
  11 = Prefeito). If NULL, returns all offices.

- turno:

  Integer. Election round (1 or 2). Default: 1.

- force:

  Logical. Re-download even if cached file exists. Default: FALSE.

- cache:

  Logical. If TRUE (default), downloaded files are stored in a
  persistent cross-session cache directory (see
  [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md)).
  If FALSE, files are stored in a temporary directory and lost when R
  restarts.

- verbose:

  Logical. Default: TRUE.

## Value

A data frame with one row per candidate per polling section, using the
original TSE column names. Key columns:

- ANO_ELEICAO:

  Election year

- CD_MUNICIPIO:

  TSE municipality code (5-digit)

- NR_ZONA:

  Electoral zone number

- NR_SECAO:

  Section number

- NR_LOCAL_VOTACAO:

  Polling location number

- CD_CARGO:

  Office code

- NR_VOTAVEL:

  Candidate number (or 95 = blank, 96 = null)

- NM_VOTAVEL:

  Candidate name

- QT_VOTOS:

  Number of votes

## Details

Data is downloaded from
`https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/`. Files
are cached persistently by default and reused on subsequent calls unless
`force = TRUE`. Use
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
to manage cached files.

The CSV files use semicolon (`;`) as delimiter and Latin-1 encoding.

**Note:** For general elections, presidential vote data (cargo 1) is
published in a national file (`uf = "BR"`) rather than in per-state
files.
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)
handles this automatically.

## See also

[`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md)
for attendance/turnout data,
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)
which uses this function internally,
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
to manage cached downloads.

Other Brazil downloads:
[`br_download_geocode()`](https://antrologos.github.io/interpElections/reference/br_download_geocode.md),
[`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md)
