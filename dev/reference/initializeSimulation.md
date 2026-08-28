# Load a simulation and apply a set of parameters.

Helper method that combines a set of common steps performed before
running a simulation. This method applies individual parameters data set
and additional user-defined parameters to the simulation and runs the
simulation to its steady-state and applies the steady-state as new
initial conditions.

## Usage

``` r
initializeSimulation(
  simulation,
  individualCharacteristics = NULL,
  additionalParams = NULL,
  stopIfParameterNotFound = TRUE
)
```

## Arguments

- simulation:

  `Simulation` loaded from a PKML file

- individualCharacteristics:

  Optional `IndividualCharacteristics` describing an individual.

- additionalParams:

  Optional named list with lists 'paths', 'values', and 'units'.

- stopIfParameterNotFound:

  Logical. If `TRUE` (default), an error is thrown if any of the
  `additionalParams` does not exist. If `FALSE`, non-existent parameters
  are ignored.

## Examples

``` r
if (FALSE) { # \dontrun{
simulation <- loadSimulation(filePath = modelPath)
humanIndividualCharacteristics <- createIndividualCharacteristics(
  species = Species$Human, population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male, weight = 70
)
userParams <- readParametersFromXLS(parameterXLSPath)
initializeSimulation(simulation, humanIndividualCharacteristics, userParams)
simulationResults <- runSimulations(simulation = simulation)
} # }
```
