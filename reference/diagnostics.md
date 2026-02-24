# Run diagnostic checks on an interpolation result

Performs automated validation of an interpElections result. Prints a
checklist of PASS/WARN/FAIL results. Only includes checks whose outcome
is not guaranteed by construction.

## Usage

``` r
diagnostics(result, verbose = TRUE)
```

## Arguments

- result:

  An `interpElections_result` object.

- verbose:

  Logical. Print the checklist? Default: TRUE.

## Value

Invisibly, a list of class `"interpElections_diagnostics"` with a
`checks` element containing all computed values.
