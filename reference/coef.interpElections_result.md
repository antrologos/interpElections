# Extract alpha coefficients

Returns the alpha decay parameter vector, which plays the role of
"coefficients" in the IDW model.

## Usage

``` r
# S3 method for class 'interpElections_result'
coef(object, ...)
```

## Arguments

- object:

  An `interpElections_result` object.

- ...:

  Ignored.

## Value

Numeric vector of length n (one alpha per census tract).
