# Prepare Brazilian electoral data at the polling-location level

Downloads TSE voter profile, attendance, and candidate vote data, merges
them with geocoded polling station coordinates, and aggregates to the
voting-location level. Returns a data frame ready for interpolation.

## Usage

``` r
br_prepare_electoral(
  code_muni_ibge,
  code_muni_tse,
  uf,
  year,
  cargo = NULL,
  turno = 1L,
  what = "candidates",
  candidates = NULL,
  parties = NULL,
  perfil_path = NULL,
  comparecimento_path = NULL,
  votacao_path = NULL,
  geocode_path = NULL,
  cache = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- code_muni_ibge:

  Character. 7-digit IBGE municipality code (e.g., `"3550308"` for Sao
  Paulo).

- code_muni_tse:

  Character. 5-digit TSE municipality code (e.g., `"71072"` for Sao
  Paulo). Typically obtained via
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md),
  which resolves this automatically.

- uf:

  Character. Two-letter state abbreviation (e.g., `"SP"`, `"RJ"`,
  `"MG"`).

- year:

  Integer. Election year. See
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
  for the distinction between municipal and general election years.

- cargo:

  Integer, character, or NULL. Which electoral office(s) to include.
  Accepts human-readable aliases (case-insensitive), TSE numeric codes,
  or a vector of either:

  |                        |          |                   |
  |------------------------|----------|-------------------|
  | Alias                  | TSE code | Election type     |
  | `"presidente"`         | 1        | General           |
  | `"governador"`         | 3        | General           |
  | `"senador"`            | 5        | General           |
  | `"deputado_federal"`   | 6        | General           |
  | `"deputado_estadual"`  | 7        | General           |
  | `"deputado_distrital"` | 8        | General (DF only) |
  | `"prefeito"`           | 11       | Municipal         |
  | `"vereador"`           | 13       | Municipal         |

  When `NULL` (the default), all offices present in the data file are
  included. For general election years, this automatically downloads the
  national file for presidential data.

  When multiple cargos are selected, output columns are prefixed (e.g.,
  `PREFEITO_CAND_*`, `VEREADOR_CAND_*`).

- turno:

  Integer. Election round: `1` (first round, the default) or `2`
  (runoff). The runoff only exists for presidente, governador, and
  prefeito (in cities with \>200k voters), and only when no candidate
  wins outright in the first round.

- what:

  Character vector. Controls what data columns are produced in the
  output. One or more of:

  - `"candidates"` **(default)**: One column per candidate with their
    vote counts, named `CAND_<ballot_number>`. Also includes
    `QT_COMPARECIMENTO` (total turnout). Special ballot numbers: 95 =
    blank votes (em branco), 96 = null votes (nulo).

  - `"parties"`: One column per party with total votes, named
    `PARTY_<abbreviation>` (e.g., `PARTY_PT`, `PARTY_MDB`). Also
    includes `QT_COMPARECIMENTO`.

  - `"turnout"`: Turnout statistics: `QT_COMPARECIMENTO` (voters who
    showed up), `QT_APTOS` (eligible voters), and `QT_ABSTENCOES`
    (abstentions), when available in the data.

  - `"demographics"`: Voter profile by gender (`GENERO_FEMININO`,
    `GENERO_MASCULINO`, `GENERO_NAO_INFORMADO`) and education level
    (`EDUC_ANALFABETO`, `EDUC_LE_ESCREVE`, `EDUC_FUND_INCOMP`,
    `EDUC_FUND_COMP`, `EDUC_MEDIO_INCOMP`, `EDUC_MEDIO_COMP`,
    `EDUC_SUP_INCOMP`, `EDUC_SUP_COMP`, `EDUC_NAO_INFORMADO`). Note:
    demographics come from the voter registration profile, not from vote
    data, so no `cargo` filter applies.

  Values can be combined:
  `what = c("candidates", "parties", "turnout", "demographics")`.

- candidates:

  Character or numeric vector, or NULL. Filter specific candidates (only
  applies when `"candidates" %in% what`):

  - **Numeric values** match the candidate's ballot number exactly.
    Example: `candidates = c(13, 22)`.

  - **Character values** perform accent-normalized, case-insensitive
    substring matching against the candidate's registered name. Example:
    `candidates = "LULA"` matches "LUIZ INACIO LULA DA SILVA".

  - **NULL** (the default): all candidates are kept.

- parties:

  Character vector or NULL. Filter specific parties (only applies when
  `"parties" %in% what`). Uses official TSE party abbreviations, matched
  case-insensitively. Example: `parties = c("PT", "PL", "MDB")`.
  **NULL** (the default): all parties are kept.

- perfil_path:

  Character or NULL. Path to a local voter profile CSV file. If NULL
  (the default), downloads from TSE.

- comparecimento_path:

  Character or NULL. Path to attendance parquet file. If NULL (the
  default), turnout is computed from the vote data itself.

- votacao_path:

  Character or NULL. Path to candidate votes parquet file. If NULL (the
  default), downloads from TSE.

- geocode_path:

  Character or NULL. Path to a CSV with geocoded polling station
  coordinates (columns: `nr_zona`, `nr_local_votacao`, `lat`, `long`).
  If NULL, coordinates are obtained from TSE and Danny Hidalgo's
  geocoding project.

- cache:

  Logical. If TRUE (default), downloaded files are stored persistently.
  See
  [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md).

- force:

  Logical. Re-download even if cached file exists. Default: FALSE.

- verbose:

  Logical. Print progress messages. Default: TRUE.

## Value

A data frame with one row per **voting location** (not per polling
section). Contains the following column groups:

**Always present:**

- `lat`, `long`: Coordinates of the voting location.

- `id`: Sequential integer ID.

- `votantes_18_20`, ..., `votantes_65_69`: Registered voters per age
  bracket at this location (used as calibration data).

**Conditional on `what`:**

- `"candidates"`: `CAND_<number>` columns + `QT_COMPARECIMENTO`.

- `"parties"`: `PARTY_<abbrev>` columns + `QT_COMPARECIMENTO`.

- `"turnout"`: `QT_COMPARECIMENTO`, `QT_APTOS`, `QT_ABSTENCOES`.

- `"demographics"`: `GENERO_*` and `EDUC_*` columns.

When multiple `cargo` values are selected, candidate and party columns
are prefixed (e.g., `PREFEITO_CAND_45`, `VEREADOR_PARTY_PT`).

## Details

Most users should use
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
instead, which calls this function internally and then runs the full
interpolation pipeline. Use `br_prepare_electoral()` directly when you
need the raw polling-location data without interpolation.

When `votacao_path` and `comparecimento_path` are NULL, data is
auto-downloaded from the official TSE open data portal using
[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md)
and
[`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md).

### Data sources

All data is downloaded from the TSE open data portal at
`https://cdn.tse.jus.br/estatistica/sead/odsele/`.

Presidential vote data (cargo 1) is published in a separate national
file (`votacao_secao_<year>_BR.zip`, ~250 MB) rather than in per-state
files. This function detects when presidential data is needed and
downloads the national file automatically.

Geocoded polling station coordinates come from two sources: official TSE
data (available from ~2020 onwards, with some missing coordinates) and
Danny Hidalgo's geocoding project
(<https://github.com/fdhidalgo/geocode_br_polling_stations>), which
covers earlier years. TSE coordinates take priority when available.

### Dependencies

Requires `dplyr`, `tidyr`, `data.table`, and `stringr` packages. When
reading local parquet files, the `arrow` package is also required.

## See also

[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
for the one-step wrapper,
[`br_download_votes()`](https://antrologos.github.io/interpElections/reference/br_download_votes.md),
[`br_download_turnout()`](https://antrologos.github.io/interpElections/reference/br_download_turnout.md),
[`interpElections_cache()`](https://antrologos.github.io/interpElections/reference/interpElections_cache.md)

Other Brazil helpers:
[`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md),
[`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ── Basic usage ────────────────────────────────────────────────
# All candidates in the 2020 municipal election for Boa Vista
elec <- br_prepare_electoral(
  code_muni_ibge = "1400100",
  code_muni_tse  = "01120",
  uf = "RR",
  year = 2020,
  what = c("candidates", "turnout")
)

# ── Specific cargo ─────────────────────────────────────────────
# Only the presidential race in 2022
elec <- br_prepare_electoral(
  code_muni_ibge = "3170701",
  code_muni_tse  = "54135",
  uf = "MG",
  year = 2022,
  cargo = "presidente"
)

# ── Filter specific candidates ─────────────────────────────────
# Only Lula and Bolsonaro in the 2022 presidential race
elec <- br_prepare_electoral(
  code_muni_ibge = "3170701",
  code_muni_tse  = "54135",
  uf = "MG",
  year = 2022,
  cargo = "presidente",
  candidates = c(13, 22)
)

# ── Party vote totals ──────────────────────────────────────────
elec <- br_prepare_electoral(
  code_muni_ibge = "3170701",
  code_muni_tse  = "54135",
  uf = "MG",
  year = 2020,
  cargo = "vereador",
  what = "parties",
  parties = c("PT", "MDB", "PL")
)

# ── Voter demographics ─────────────────────────────────────────
elec <- br_prepare_electoral(
  code_muni_ibge = "3170701",
  code_muni_tse  = "54135",
  uf = "MG",
  year = 2022,
  what = "demographics"
)
# -> GENERO_FEMININO, GENERO_MASCULINO, EDUC_SUP_COMP, ...
} # }
```
