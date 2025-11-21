# Convert string to numeric

Convert string to numeric

## Usage

``` r
stringToNum(string, lloqMode = LLOQMode$`LLOQ/2`, uloqMode = ULOQMode$ULOQ)
```

## Arguments

- string:

  A string or a list of strings to be converted to numeric values

- lloqMode:

  How to treat entries below LLOQ, i.e., of a form "\<2": `LLOQ/2`
  (default): return the number divided by 2, `LLOQ`: return the
  numerical value, `ZERO`: return 0, `ignore`: return `NA`

- uloqMode:

  How to treat entries above ULOQ, i.e., of a form "\>2": `ULOQ`: return
  the numerical value, `ignore`: return `NA`

## Value

A numeric value or a list of numeric values

## Details

Tries to convert each string to a numeric with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html). If any conversion
fails and returns `NA`, the value is tested for being a LLOQ- or a ULOQ
value, i.e., of a form "\<2" or "\>2", respectively. If this is a case,
the returned value is defined by the parameters `lloqMode` and
`uloqMode`. In any other case where the string cannot be converted to a
numeric, `NA` is returned.
