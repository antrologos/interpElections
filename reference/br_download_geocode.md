# Download geocoded polling station data from TSE

Downloads the official polling station location data (eleitorado por
local de votação) from the TSE open data portal. This file includes
geographic coordinates (`NR_LATITUDE`, `NR_LONGITUDE`) for polling
locations, along with voter counts and addresses.

## Usage

``` r
br_download_geocode(
  year,
  uf = NULL,
  code_muni_tse = NULL,
  force = FALSE,
  cache = TRUE,
  verbose = TRUE
)
```

## Arguments

- year:

  Integer. Election year (e.g., 2010, 2012, 2016, 2020).

- uf:

  Character or NULL. Two-letter state abbreviation. If provided, filters
  results to this state.

- code_muni_tse:

  Character or NULL. 5-digit TSE municipality code. If provided, filters
  results to this municipality only.

- force:

  Logical. Re-download even if cached file exists. Default: FALSE.

- cache:

  Logical. If TRUE (default), downloaded files are stored persistently.
  See
  [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md).

- verbose:

  Logical. Default: TRUE.

## Value

A data frame with one row per section per round, using the original TSE
column names. Returns `NULL` if no data is available for the given year
(e.g., before 2010). Key columns:

- CD_MUNICIPIO:

  TSE municipality code (5-digit)

- NR_ZONA:

  Electoral zone number

- NR_LOCAL_VOTACAO:

  Polling location number

- NR_SECAO:

  Section number

- NR_LATITUDE:

  Latitude of polling location (-1 if missing)

- NR_LONGITUDE:

  Longitude of polling location (-1 if missing)

- NM_LOCAL_VOTACAO:

  Name of polling location

- DS_ENDERECO:

  Address of polling location

## Details

TSE data is available from 2010 onward. For years before 2010 (e.g.,
2008), this function returns `NULL` so the caller can fall back to
alternative sources.

Data is downloaded from
`https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/`.
Files are cached persistently by default and reused on subsequent calls
unless `force = TRUE`. Use
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
to manage cached files.

The CSV files use semicolon (`;`) as delimiter and Latin-1 encoding.
Missing coordinates are stored as `-1` in the TSE data.

## See also

[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)
which uses this function internally to geocode polling stations (with
Hidalgo fallback for missing coordinates),
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
to manage cached downloads.

Other Brazil downloads:
[`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md),
[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
