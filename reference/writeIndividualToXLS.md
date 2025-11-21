# Create a parameter set describing an individual and write it to the Excel file

Create a parameter set describing an individual and write it to the
Excel file

## Usage

``` r
writeIndividualToXLS(individualCharacteristics, outputXLSPath)
```

## Arguments

- individualCharacteristics:

  An `IndividualCharacteristics` object describing the individual. See
  `createIndividualCharacterstics` for more information.

- outputXLSPath:

  Path to the Excel file the parameter set will be written to

## Value

Path to the created Excel file

## See also

createIndividualCharacteristics crateIndividual

## Examples

``` r
if (FALSE) { # \dontrun{
simulation <- loadSimulation(pathToPKML)
humanIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Human, population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male, weight = 70
)
writeIndividualToXLS(humanIndividualCharacteristics, pathToExcelFile)
} # }
```
