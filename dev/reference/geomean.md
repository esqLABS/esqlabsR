# Calculate geometric mean of a numeric vector

Calculate geometric mean of a numeric vector

## Usage

``` r
geomean(x, na.rm = FALSE, trim = 0)
```

## Arguments

- x:

  Numeric array to calculate geometric mean for

- na.rm:

  A logical value indicating whether `NA` values should be stripped
  before the computation proceeds

- trim:

  Fraction (0 to 0.5) of observations to be trimmed from each end of `x`
  before the mean is computed. Values of trim outside that range are
  taken as the nearest endpoint

## Value

Geometric mean of `x`
