# Set the values of parameters in the simulation by path, if the `condition` is true.

Set the values of parameters in the simulation by path, if the
`condition` is true.

## Usage

``` r
setParameterValuesByPathWithCondition(
  parameterPaths,
  values,
  simulation,
  condition = function(path) {
     TRUE
 },
  units = NULL
)
```

## Arguments

- parameterPaths:

  A single or a list of parameter path

- values:

  A numeric value that should be assigned to the parameters or a vector
  of numeric values, if the value of more than one parameter should be
  changed. Must have the same length as `parameterPaths`

- simulation:

  Simulation used to retrieve parameter instances from given paths.

- condition:

  A function that receives a parameter path as an argument and returns
  `TRUE` of `FALSE`

- units:

  A string or a list of strings defining the units of the `values`. If
  `NULL` (default), values are assumed to be in base units. If not
  `NULL`, must have the same length as `parameterPaths`.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
condition <- function(path) {
  ospsuite::isExplicitFormulaByPath(
    path = path,
    simulation = sim
  )
}
setParameterValuesByPathWithCondition(
  c("Organism|Liver|Volume", "Organism|Volume"),
  c(2, 3),
  sim,
  condition
)
```
