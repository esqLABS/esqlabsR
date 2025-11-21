# esqLABS color palette

Returns the list of colors extrapolated between the esqLABS colors blue,
red, and green.

## Usage

``` r
esqlabsColors(nrOfColors)
```

## Arguments

- nrOfColors:

  Positive integer defining the number of colors to be generated.

## Value

A list of colors as HEX values.

## Details

For `nrOfColors` == 1, the esqLABS-blue is returned For `nrOfColors` ==
2, the esqLABS-blue and green are returned For `nrOfColors` == 3, the
esqLABS-blue, red, and green are returned For `nrOfColors` \> 3, the
three esqLABS colors are fixed, and the remaining colors are
extrapolated from blue to red to green. If `nrOfColors` is uneven, the
blue-to-red section becomes one color more than the red-to-green
section. In this implementation, blue-to-green is not considered.
