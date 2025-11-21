# Get the name of the molecule from a quantity

Returns the name of the molecule to which the quantity object is
associated. The quantity could be the amount of the molecule in a
container ('Organism\|VenousBlood\|Plasma\|Aciclovir'), a parameter of
the molecule ('Aciclovir\|Lipophilicity' or
'Organism\|VenousBlood\|Plasma\|Aciclovir\|Concentration'), or an
observer ("Organism\|PeripheralVenousBlood\|Aciclovir\|Plasma
(Peripheral Venous Blood)").

If the quantity is not associated with a molecule (e.g.
'Organism\|Weight'), an error is thrown.

## Usage

``` r
getMoleculeNameFromQuantity(quantity)
```

## Arguments

- quantity:

  A `Quantity` object

## Value

Name of the molecule the quantity is associated with.

## Examples

``` r
simulation <- loadSimulation(system.file("extdata", "Aciclovir.pkml", package = "ospsuite"))
quantity <- getQuantity(
  path = "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  container = simulation
)
getMoleculeNameFromQuantity(quantity = quantity)
#> [1] "Aciclovir"
```
