# Apply an individual to the simulation. For human species, only parameters that do not override formulas are applied. For other species, all parameters returned by `createIndividual` are applied.

Apply an individual to the simulation. For human species, only
parameters that do not override formulas are applied. For other species,
all parameters returned by `createIndividual` are applied.

## Usage

``` r
applyIndividualParameters(individualCharacteristics, simulation)
```

## Arguments

- individualCharacteristics:

  `IndividualCharacteristics` describing an individual. Optional

- simulation:

  `Simulation` loaded from the PKML file

## Examples

``` r
if (FALSE) { # \dontrun{
simulation <- loadSimulation(filePath = modelPath)
humanIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Human, population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male, weight = 70
)
applyIndividualParameters(humanIndividualCharacteristics, simulation)
} # }
```
