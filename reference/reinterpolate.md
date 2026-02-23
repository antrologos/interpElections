# Re-interpolate with different electoral variables

Takes a previous
[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)
result and re-runs the interpolation pipeline using the stored travel
time matrix, skipping the expensive routing step. Useful for switching
to different candidates, parties, `what` types, or election rounds
without recomputing travel times.

## Usage

``` r
reinterpolate(
  result,
  what = NULL,
  candidates = NULL,
  parties = NULL,
  cargo = NULL,
  turno = NULL,
  optim = optim_control(),
  keep = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- result:

  An `interpElections_result` object from
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

- what:

  Character vector. Controls what information is interpolated. See
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).
  If NULL (default), reuses the original.

- candidates:

  Character or numeric vector, or NULL. Filter candidates. See
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

- parties:

  Character vector or NULL. Filter parties. See
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

- cargo:

  Integer, character, or NULL. Override the electoral office. See
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

- turno:

  Integer or NULL. Override the election round (1 or 2).

- optim:

  An
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md)
  object with optimization parameters. Default:
  [`optim_control()`](https://antrologos.github.io/interpElections/reference/optim_control.md).

- keep:

  Character vector or NULL. Heavy objects to include in result. See
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

- verbose:

  Logical. Print progress messages. Default: TRUE.

- ...:

  Additional arguments forwarded to
  [`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md).

## Value

An `interpElections_result` object (same structure as the original, with
updated interpolation).

## Details

The previous result must contain a `time_matrix` (always kept by default
since v0.2).

**Note**: Alpha is re-optimized (which can take minutes). Only the
travel-time computation is skipped.

## See also

[`interpolate_election_br()`](https://antrologos.github.io/interpElections/reference/interpolate_election_br.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First run (time_matrix is kept by default)
result <- interpolate_election_br(
  3550308, 2022,
  cargo = "presidente"
)

# Re-interpolate for parties instead of candidates
result2 <- reinterpolate(result, what = "parties")

# Re-interpolate for a different cargo
result3 <- reinterpolate(result, cargo = "governador")

# Re-interpolate for turno 2 (runoff)
result4 <- reinterpolate(result, turno = 2)
} # }
```
