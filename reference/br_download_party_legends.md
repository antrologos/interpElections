# Download party legends from TSE

Downloads the official party legend data (consulta de legendas) from the
TSE open data portal for a given election year. This file maps party
numbers (`NR_PARTIDO`) to abbreviations (`SG_PARTIDO`) and full names
(`NM_PARTIDO`).

## Usage

``` r
br_download_party_legends(year, force = FALSE, cache = TRUE, verbose = TRUE)
```

## Arguments

- year:

  Integer. Election year (e.g., 2008, 2012, 2016, 2020, 2022).

- force:

  Logical. Re-download even if cached file exists. Default: FALSE.

- cache:

  Logical. If TRUE (default), downloaded files are stored persistently.
  See
  [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md).

- verbose:

  Logical. Default: TRUE.

## Value

A data frame with TSE party/coalition data. Key columns:

- NR_PARTIDO:

  Party number (2-digit)

- SG_PARTIDO:

  Party abbreviation (e.g., "PT", "MDB")

- NM_PARTIDO:

  Full party name

## Details

Data is downloaded from
`https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_coligacao/`.
This file contains coalition/party records for each election year, from
which the unique party number-to-abbreviation mapping is extracted.
Files are cached persistently by default and reused on subsequent calls
unless `force = TRUE`.

## See also

[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
for candidate vote data,
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md)
which uses this function internally,
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)
to manage cached downloads.

Other Brazil downloads:
[`br_download_geocode()`](https://antrologos.github.io/interpElections/reference/br_download_geocode.md),
[`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md),
[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
