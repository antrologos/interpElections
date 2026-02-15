# One-step interpolation for Brazilian elections

High-level wrapper that auto-downloads census data, electoral data,
tract geometries, and OSM road networks, then runs the full optimization
and interpolation pipeline. The user only needs to provide an IBGE
municipality code and an election year.

## Usage

``` r
interpolate_election_br(
  code_muni,
  year,
  comparecimento_path = NULL,
  votacao_path = NULL,
  network_path = NULL,
  time_matrix = NULL,
  cargo = NULL,
  turno = 1L,
  what = "candidates",
  candidates = NULL,
  parties = NULL,
  interp_sources = NULL,
  census_year = NULL,
  clip_sf = NULL,
  remove_unpopulated = TRUE,
  osm_buffer_km = 10,
  osm_provider = "openstreetmap_fr",
  keep = NULL,
  alpha = NULL,
  offset = 1,
  use_gpu = NULL,
  cache = TRUE,
  force = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- code_muni:

  Numeric or character. 7-digit IBGE municipality code (e.g., `3550308`
  for Sao Paulo, `3170701` for Varginha). The TSE code and state
  abbreviation are resolved automatically.

- year:

  Integer. Election year. Brazil holds two types of elections:

  - **Municipal** (even years divisible by 4): 2000, 2004, 2008, 2012,
    2016, 2020, 2024. Offices: prefeito, vereador.

  - **General/federal** (even years *not* divisible by 4): 2002, 2006,
    2010, 2014, 2018, 2022. Offices: presidente, governador, senador,
    deputado federal, deputado estadual.

- comparecimento_path:

  Character or NULL. Path to attendance/turnout parquet file. If NULL
  (the default), turnout is computed from the vote data itself.

- votacao_path:

  Character or NULL. Path to candidate votes parquet file. If NULL (the
  default), vote data is auto-downloaded from the TSE open data portal.

- network_path:

  Character or NULL. Path to a directory containing an OSM `.pbf` file
  for r5r routing. If NULL and `time_matrix` is also NULL, OSM data is
  auto-downloaded via
  [`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md).

- time_matrix:

  Numeric matrix or NULL. Pre-computed travel time matrix \[n x m\]. If
  provided, skips all travel time computation (no r5r, no OSM download).
  Useful for re-running with different parameters on the same
  municipality.

- cargo:

  Integer, character, or NULL. Which electoral office(s) to include.
  Accepts one or more human-readable aliases (case-insensitive) or TSE
  numeric codes:

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

  When `NULL` (the default), **all offices available** for the election
  year are included. When multiple cargos are selected, output columns
  are prefixed with the office name (e.g., `PRESIDENTE_CAND_13`,
  `GOVERNADOR_CAND_22`).

- turno:

  Integer. Election round: `1` (first round, the default) or `2`
  (runoff). The first round always exists. The second round (turno 2) is
  only held for presidente, governador, and prefeito (in cities with
  \>200k voters), and only when no candidate wins an outright majority.
  If turno 2 did not occur, the result will contain zero vote rows for
  those offices.

- what:

  Character vector. Controls **what information** is interpolated into
  census tracts. One or more of:

  - `"candidates"` **(default)**: Vote counts per candidate. Creates one
    column per candidate, named `CAND_<number>` (e.g., `CAND_13`,
    `CAND_22`). Also includes `QT_COMPARECIMENTO` (total turnout).

  - `"parties"`: Vote counts aggregated by party. Creates one column per
    party, named `PARTY_<abbreviation>` (e.g., `PARTY_PT`, `PARTY_PL`).
    Also includes `QT_COMPARECIMENTO`.

  - `"turnout"`: Turnout and abstention. Creates `QT_COMPARECIMENTO`
    (voters who showed up), `QT_APTOS` (eligible voters), and
    `QT_ABSTENCOES` (abstentions), when available.

  - `"demographics"`: Voter profile demographics. Creates `GENERO_*`
    columns (e.g., `GENERO_FEMININO`, `GENERO_MASCULINO`) and `EDUC_*`
    columns (e.g., `EDUC_SUP_COMP`, `EDUC_FUND_INCOMP`).

  Multiple values can be combined:
  `what = c("candidates", "parties", "turnout", "demographics")`.

- candidates:

  Character or numeric vector, or NULL. Filter specific candidates (only
  used when `"candidates" %in% what`):

  - **By number** (numeric): Matches the candidate's ballot number
    exactly. Example: `candidates = c(13, 22)` keeps only candidates 13
    (Lula) and 22 (Bolsonaro) in 2022.

  - **By name** (character): Performs accent-normalized,
    case-insensitive substring matching against the candidate's
    registered name. Example: `candidates = "LULA"` matches "LUIZ INACIO
    LULA DA SILVA".

  - `NULL` **(default)**: All candidates are included (including special
    codes 95 = votos em branco, 96 = votos nulos).

- parties:

  Character vector or NULL. Filter specific parties (only used when
  `"parties" %in% what`). Uses official TSE party abbreviations, matched
  case-insensitively:

  - Example: `parties = c("PT", "PL")` keeps only PT and PL.

  - `NULL` **(default)**: All parties are included.

- interp_sources:

  Character vector or NULL. Column names from the electoral data to
  interpolate. Default `NULL` auto-selects based on `what`. Override
  this only if you need fine-grained control over which columns are
  interpolated.

- census_year:

  Integer or NULL. Census year for population data (2000, 2010, or
  2022). If `NULL` (the default), auto-selected based on the election
  year:

  - Elections 2000-2004 use Census 2000

  - Elections 2008-2016 use Census 2010

  - Elections 2020+ use Census 2022

- clip_sf:

  `sf` polygon or NULL. Optional geometry to clip tracts (e.g., an urban
  area boundary). Tracts outside this polygon are removed before
  interpolation.

- remove_unpopulated:

  Logical. Remove zero-population tracts. Default: TRUE.

- osm_buffer_km:

  Numeric. Buffer in km for OSM bounding box expansion. Default: 10.

- osm_provider:

  Character. OSM extract provider for `osmextract`. Default:
  `"openstreetmap_fr"` (has state-level extracts for Brazil).
  Alternatives: `"geofabrik"`, `"bbbike"`. Only used when OSM data is
  auto-downloaded (no `network_path` or `time_matrix` provided).

- keep:

  Character vector or NULL. Names of heavy intermediate objects to
  include in the result. Default NULL (lightweight). Options:
  `"weights"`, `"time_matrix"`, `"sources_sf"`. See
  [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
  for details.

- alpha:

  Numeric vector or NULL. Pre-computed decay parameters (one per tract).
  If provided, the optimization step is skipped entirely. Useful for
  re-interpolating with a previously optimized alpha.

- offset:

  Numeric. Travel time offset. Default: 1.

- use_gpu:

  Logical or NULL. Passed to
  [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md).

- cache:

  Logical. If TRUE (default), downloaded files (TSE data, OSM networks,
  census tracts) are stored persistently across R sessions. See
  [`get_interpElections_cache_dir()`](https://antrologos.github.io/interpElections/reference/get_interpElections_cache_dir.md).
  Subsequent calls reuse cached files, making re-runs much faster.

- force:

  Logical. Re-download even if cached file exists. Default: FALSE.

- verbose:

  Logical. Default: TRUE.

- ...:

  Additional arguments forwarded to
  [`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md),
  [`optimize_alpha()`](https://antrologos.github.io/interpElections/reference/optimize_alpha.md),
  [`compute_travel_times()`](https://antrologos.github.io/interpElections/reference/compute_travel_times.md),
  and/or
  [`download_r5r_data()`](https://antrologos.github.io/interpElections/reference/download_r5r_data.md).

## Value

A list of class `"interpElections_result"` with components:

- interpolated:

  Numeric matrix \[n x p\]. Rows = census tracts, columns = interpolated
  variables.

- alpha:

  Numeric vector of length n. Optimized decay parameters.

- tracts_sf:

  `sf` object with interpolated columns joined in, ready for mapping
  with [`plot()`](https://rdrr.io/r/graphics/plot.default.html) or
  `ggplot2`.

- sources:

  Data frame of prepared electoral data (one row per voting location),
  without geometry.

- optimization:

  `interpElections_optim` or NULL (if alpha was pre-supplied).

- offset:

  Numeric. Offset value used.

- call:

  The matched call.

- zone_id:

  Character. Name of zone ID column.

- point_id:

  Character. Name of source point ID column.

- interp_cols:

  Character vector. Names of interpolated columns.

- calib_cols:

  List with `$zones` and `$sources` calibration columns.

- weights:

  Numeric matrix or NULL. Present only when `keep` includes `"weights"`.

- time_matrix:

  Numeric matrix or NULL. Present only when `keep` includes
  `"time_matrix"`.

- sources_sf:

  `sf` point object or NULL. Present only when `keep` includes
  `"sources_sf"`.

- code_muni:

  IBGE municipality code.

- year:

  Election year.

- census_year:

  Census year.

- what:

  Character vector of data types interpolated.

- pop_data:

  Data frame of census population by tract.

## Details

Internally calls
[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
after preparing all inputs.

## Election types

Brazil holds elections every two years, alternating between municipal
and general (federal/state) elections:

- **Municipal elections** (2000, 2004, 2008, 2012, 2016, 2020, 2024):
  elect prefeito (mayor) and vereador (city councilor).

- **General elections** (2002, 2006, 2010, 2014, 2018, 2022): elect
  presidente, governador, senador, deputado federal, and deputado
  estadual.

Presidential vote data is published by the TSE in a separate national
file (~250 MB). This function handles the download automatically when
`cargo` includes `"presidente"` or when `cargo = NULL` in a general
election year.

## Output columns

The `tracts_sf` output contains the original census tract geometry plus
interpolated columns. Column names follow these patterns:

- `CAND_<number>`:

  Interpolated vote count for candidate with ballot number `<number>`.
  Special numbers: 95 = blank votes (em branco), 96 = null votes (nulo).

- `PARTY_<abbrev>`:

  Interpolated total votes for party `<abbrev>` (e.g., `PARTY_PT`,
  `PARTY_PL`).

- `GENERO_<category>`:

  Interpolated voter count by gender (e.g., `GENERO_FEMININO`,
  `GENERO_MASCULINO`).

- `EDUC_<level>`:

  Interpolated voter count by education level (e.g., `EDUC_SUP_COMP`,
  `EDUC_FUND_INCOMP`).

- `QT_COMPARECIMENTO`:

  Total voters who showed up.

- `QT_APTOS`:

  Total eligible voters (when available).

- `QT_ABSTENCOES`:

  Total abstentions (when available).

When multiple `cargo` values are selected, candidate and party columns
are prefixed: `PRESIDENTE_CAND_13`, `GOVERNADOR_PARTY_PT`, etc.

## See also

[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)
for the general-purpose wrapper,
[`br_prepare_population()`](https://antrologos.github.io/interpElections/reference/br_prepare_population.md),
[`br_prepare_electoral()`](https://antrologos.github.io/interpElections/reference/br_prepare_electoral.md),
[`br_prepare_tracts()`](https://antrologos.github.io/interpElections/reference/br_prepare_tracts.md).

Other wrappers:
[`interpolate_election()`](https://antrologos.github.io/interpElections/reference/interpolate_election.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# ── Minimal usage ───────────────────────────────────────────────
# Just an IBGE code + year. Everything else is auto-downloaded.
result <- interpolate_election_br(
  code_muni = 3550308,   # Sao Paulo
  year = 2020
)

# The result includes an sf object ready for mapping
plot(result$tracts_sf["CAND_13"])


# ── Choosing a specific cargo ───────────────────────────────────
# Municipal election: only city councilors (vereador)
result <- interpolate_election_br(
  code_muni = 3170701, year = 2020,
  cargo = "vereador"
)

# General election: only the presidential race
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "presidente"
)

# Multiple offices at once (columns are prefixed)
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = c("presidente", "governador")
)
# -> columns: PRESIDENTE_CAND_13, GOVERNADOR_CAND_30, ...


# ── Turno (election round) ─────────────────────────────────────
# First round (default)
r1 <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "presidente", turno = 1
)

# Runoff (second round) -- only 2 candidates
r2 <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "presidente", turno = 2
)


# ── Choosing what to interpolate ───────────────────────────────
# Party vote totals instead of individual candidates
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "governador",
  what = "parties"
)
# -> columns: PARTY_PT, PARTY_PL, PARTY_MDB, ...

# Voter demographics (gender + education)
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  what = "demographics"
)
# -> columns: GENERO_FEMININO, EDUC_SUP_COMP, ...

# Turnout and abstention
result <- interpolate_election_br(
  code_muni = 3170701, year = 2020,
  cargo = "prefeito",
  what = "turnout"
)
# -> columns: QT_COMPARECIMENTO, QT_APTOS, QT_ABSTENCOES

# Everything at once
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "governador",
  what = c("candidates", "parties", "turnout", "demographics")
)


# ── Filtering candidates ───────────────────────────────────────
# By ballot number
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "presidente",
  candidates = c(13, 22)
)
# -> only CAND_13 (Lula) and CAND_22 (Bolsonaro)

# By name (accent-insensitive substring search)
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "presidente",
  candidates = "LULA"
)
# -> matches "LUIZ INACIO LULA DA SILVA"


# ── Filtering parties ──────────────────────────────────────────
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "presidente",
  what = "parties",
  parties = c("PT", "PL")
)
# -> only PARTY_PT and PARTY_PL


# ── Re-using a previous result ─────────────────────────────────
# Keep the time_matrix for reuse (opt-in via keep)
result <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "governador",
  keep = "time_matrix"
)

# Then reuse the alpha and travel time matrix
result2 <- interpolate_election_br(
  code_muni = 3170701, year = 2022,
  cargo = "governador",
  what = "parties",
  time_matrix = result$time_matrix,
  alpha = result$alpha
)


# ── Pre-computed travel times (skip r5r) ───────────────────────
result <- interpolate_election_br(
  code_muni = 3550308, year = 2020,
  time_matrix = my_tt_matrix
)
} # }
```
