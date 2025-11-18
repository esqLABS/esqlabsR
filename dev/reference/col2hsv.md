# Returns the HSV values for a given R color name

Returns the HSV values for a given R color name

## Usage

``` r
col2hsv(color)
```

## Arguments

- color:

  vector of any of the three kinds of R color specifications, i.e.,
  either a color name (as listed by colors()), a hexadecimal string of
  the form "#rrggbb" or "#rrggbbaa" (see rgb), or a positive integer `i`
  meaning `palette()[i]`.

## Value

A matrix with a column for each color. The three rows of the matrix
indicate hue, saturation and value and are named "h", "s", and "v"
accordingly.

## Examples

``` r
col2hsv("yellow")
#>        [,1]
#> h 0.1666667
#> s 1.0000000
#> v 1.0000000
```
