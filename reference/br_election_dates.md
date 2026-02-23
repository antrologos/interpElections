# Brazilian election dates

Election dates for turno 1 (first round) and turno 2 (runoff) for all
Brazilian elections from 2000 to 2024. Used internally for auto-deriving
departure datetime when GTFS transit routing is enabled.

## Usage

``` r
br_election_dates
```

## Format

A data frame with 26 rows and 3 columns:

- year:

  Integer. Election year.

- turno:

  Integer. 1 = first round, 2 = runoff.

- date:

  Date. Election date.

## Source

TSE (Tribunal Superior Eleitoral)
