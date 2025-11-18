# Scenario

Simulation scenario

## Value

A new `Scenario` object.

## Active bindings

- `scenarioConfiguration`:

  `scenarioConfiguration` used for creation of this scenario. Read-only.

- `finalCustomParams`:

  Custom parameters to be used for the simulation. Read-only.

- `simulation`:

  Simulation object created from the `ScenarioConfiguration`. Read-only

- `population`:

  Population object in case the scenario is a population simulation.
  Read-only.

- `scenarioType`:

  Type of the scenario - individual or population. Read-only

## Methods

### Public methods

- [`Scenario$new()`](#method-Scenario-new)

- [`Scenario$print()`](#method-Scenario-print)

------------------------------------------------------------------------

### Method `new()`

Custom parameters to be used for the simulation. The final custom
parameters are a combination of parametrization through the excel files
and the custom parameters specified by the user through the
`customParams` argument of the `Scenario` constructor.

Simulation object. Read-only.

Initialize a new instance of the class. Initializes the scenario from
`ScenarioConfiguration` object.

#### Usage

    Scenario$new(
      scenarioConfiguration,
      customParams = NULL,
      stopIfParameterNotFound = TRUE
    )

#### Arguments

- `scenarioConfiguration`:

  An object of class `ScenarioConfiguration`.

- `customParams`:

  Custom parameters to be used for the simulation. A list containing
  vectors 'paths' with the full paths to the parameters, 'values' the
  values of the parameters, and 'units' with the units the values are
  in.

- `stopIfParameterNotFound`:

  Logical. If `TRUE` (default), an error is thrown if any of the custom
  defined parameter does not exist. If `FALSE`, non-existent parameters
  are ignored.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Scenario$print(...)

#### Arguments

- `...`:

  Rest arguments.
