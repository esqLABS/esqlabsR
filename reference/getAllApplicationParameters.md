# Get parameters of applications in the simulation

Get parameters of applications in the simulation

## Usage

``` r
getAllApplicationParameters(simulation, moleculeNames = NULL)
```

## Arguments

- simulation:

  A `Simulation` object

- moleculeNames:

  Names of the molecules which applications parameters will be returned.
  If `NUll`(default), applications for all molecules are returned.

## Value

A list of `Parameter` objects defining the applications in the
simulation.

## Details

Every application event has a `ProtocolSchemaItem` container that holds
parameters describing the dose, start time, infusion time etc. This
function returns a list of all constant parameters located under the
`ProtocolSchemaItem` container of applications defined for the
`moleculeNames`.

## Examples

``` r
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
simulation <- loadSimulation(simPath)
applicationParams <- getAllApplicationParameters(simulation = simulation)

applicationParams <- getAllApplicationParameters(
  simulation = simulation,
  moleculeNames = "Aciclovir"
)
```
