# Apply an individual parametrization to a simulation

Sets the parameter values that describe `individualCharacteristics` in
`simulation`.

For a human individual, only the parameters that vary between
individuals of the same species are set; parameters defined by formulas
keep their formulas.

For any other species, the simulation is scaled to that species. Two
sets of values are applied: the individual parameters returned by
[`ospsuite::createIndividual()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/createIndividual.html),
including those that replace formulas (organ volumes, blood flow rates,
body weight), and the species constants of the PK-Sim database (tissue
composition, blood flow, gut geometry and transit, lumen pH, bile salt
concentrations), taken from an individual building block that
[`ospsuite::createIndividualBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/createIndividualBuildingBlock.html)
creates for the same characteristics. The values follow the body weight,
height, and age given in `individualCharacteristics`.

## Usage

``` r
applyIndividualParameters(individualCharacteristics, simulation)
```

## Arguments

- individualCharacteristics:

  `IndividualCharacteristics` describing an individual, as returned by
  [`ospsuite::createIndividualCharacteristics()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/createIndividualCharacteristics.html).

- simulation:

  `Simulation` loaded from the PKML file

## Value

`simulation`, invisibly, with the parameter values applied.

## Details

Scaling works from a simulation exported for a human individual to every
species PK-Sim supports, and between two non-human species. Scaling from
a non-human species to human is not supported: a human individual needs
age- and height-dependent parameters that are not part of an animal
model. The function therefore stops with an error when a human
individual is applied to a simulation built for another species. The
species is read from the individual stored in the simulation (PK-Sim
exports since OSP version 12); an older export counts as human when it
has the parameters `Organism|Height` and `Organism|Age`.

## Examples

``` r
if (FALSE) { # \dontrun{
simulation <- loadSimulation(filePath = modelPath)
humanIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Human, population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male, weight = 70
)
applyIndividualParameters(humanIndividualCharacteristics, simulation)

# Scale a human model to a rat of 250 g
ratIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Rat, weight = 0.25
)
applyIndividualParameters(ratIndividualCharacteristics, simulation)
} # }
```
