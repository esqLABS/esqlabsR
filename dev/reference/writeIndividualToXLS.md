# Write the parameters of an individual to an Excel file

Writes the parameter values that describe `individualCharacteristics` to
an Excel file with the columns `Container Path`, `Parameter Name`,
`Value`, and `Units`, the layout of a model parameter sheet that
[`readParametersFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readParametersFromXLS.md)
reads. The parameters are the ones
[`applyIndividualParameters()`](https://esqlabs.github.io/esqlabsR/dev/reference/applyIndividualParameters.md)
sets: for a human individual, the parameters that vary between
individuals of the species; for any other species, also the derived
parameters and the species constants that scale a human model to that
species.

## Usage

``` r
writeIndividualToXLS(individualCharacteristics, outputXLSPath)
```

## Arguments

- individualCharacteristics:

  An `IndividualCharacteristics` object describing the individual, as
  returned by
  [`ospsuite::createIndividualCharacteristics()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/createIndividualCharacteristics.html).

- outputXLSPath:

  Path to the Excel file the parameter set will be written to

## Value

Path to the created Excel file

## Details

The sheet of a non-human individual describes every model of the
species, so it can hold parameters a given model does not have, for
example in the container `Organism|EndogenousIgG`. Applying such a row
to a model that lacks it stops the run unless parameters that are not
found are allowed, for example with `stopIfParameterNotFound = FALSE` in
[`initializeSimulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/initializeSimulation.md).
Remove the rows the model lacks, or allow them.

## See also

[`applyIndividualParameters()`](https://esqlabs.github.io/esqlabsR/dev/reference/applyIndividualParameters.md),
[`readParametersFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readParametersFromXLS.md)

## Examples

``` r
if (FALSE) { # \dontrun{
humanIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Human, population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male, weight = 70
)
writeIndividualToXLS(humanIndividualCharacteristics, pathToExcelFile)

# All parameters that scale a human model to a rat of 250 g
ratIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Rat, weight = 0.25
)
writeIndividualToXLS(ratIndividualCharacteristics, pathToExcelFile)
} # }
```
