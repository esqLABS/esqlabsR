# Compare two simulations

Compare two simulations

## Usage

``` r
compareSimulations(simulation1, simulation2, compareFormulasByValue = FALSE)
```

## Arguments

- simulation1:

  First `Simulation` to compare

- simulation2:

  Second `Simulation` to compare

- compareFormulasByValue:

  If `FALSE` (default), parameters are considered not equal if the have
  the same value but different formulas (e.g., a constant vs. explicit
  formula). If `TRUE`, only values are compared.

## Value

Named list with following levels:

- `Parameters` with named lists `In1NotIn2`, `In2NotIn1`, and
  `Different`, holding the `Parameter` objects that are present in the
  first but not in the second simulation, present in the second but not
  in the first simulation, and present in both simulations but with
  different formulas and/or values, respectively.

## Details

The function compares two simulations and returns a list of entities
that differ:

- `Parameters`: a named list with a list of all `Parameter` entities
  that are:

- in simulation1 but not in simulation 2 (`In1NotIn2`)

- in simulation 2 but not in simulation 1 (`I21NotIn1`)

- a list `Different` with all parameters which values differ between the
  simulations. Two parameters are considered different if their formulas
  or values differ.

## See also

isParametersEqual

## Examples

``` r
if (FALSE) { # \dontrun{
humanSim <- loadSimulation(file.path(modelFolder, "DefaultHuman.pkml"))
ratSim <- loadSimulation(file.path(modelFolder, "DefaultRat.pkml"))
diffParams <- compareSimulationParameters(humanSim, ratSim)
} # }
```
