# PITaskConfiguration

An object storing configuration for a parameter identification (PI)
task. This class holds references to PI task settings defined in
`ParameterIdentification.xlsx`.

## Value

A new `PITaskConfiguration` object.

## Active bindings

- `projectConfiguration`:

  `ProjectConfiguration` that will be used. Read-only.

- `taskName`:

  Name of the PI task. Key for lookup in `ParameterIdentification.xlsx`.
  Read-only.

- `scenarioConfiguration`:

  Named list of `ScenarioConfiguration` objects for the PI task.
  Read-only.

- `piConfiguration`:

  Named list of PI settings: algorithm, ciMethod,
  printEvaluationFeedback, autoEstimateCI, simulationRunOptions,
  objectiveFunctionOptions, algorithmOptions, ciOptions. Read-only.

- `piParameters`:

  Named list of parameter configurations from PIParameters sheet.
  Read-only.

- `piOutputMappings`:

  Named list of output mapping configurations from PIOutputMappings
  sheet. Read-only.

## Methods

### Public methods

- [`PITaskConfiguration$new()`](#method-PITaskConfiguration-new)

- [`PITaskConfiguration$print()`](#method-PITaskConfiguration-print)

- [`PITaskConfiguration$clone()`](#method-PITaskConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    PITaskConfiguration$new(
      taskName,
      projectConfiguration,
      scenarioConfiguration,
      piDefinitions = NULL
    )

#### Arguments

- `taskName`:

  Character. Name of the PI task (key for lookup in Excel).

- `projectConfiguration`:

  An object of class `ProjectConfiguration`.

- `scenarioConfiguration`:

  An object of class `ScenarioConfiguration` or a named list of
  `ScenarioConfiguration` objects.

- `piDefinitions`:

  Named list containing:

  - `piConfiguration`: Named list of PI settings

  - `piParameters`: Named list of PI parameter configurations

  - `piOutputMappings`: Named list of PI output mapping configurations

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    PITaskConfiguration$print(
      className = TRUE,
      projectConfiguration = FALSE,
      scenarioConfiguration = FALSE
    )

#### Arguments

- `className`:

  Whether to print the name of the class at the beginning. Default is
  TRUE.

- `projectConfiguration`:

  Whether to also print project configuration. Default is FALSE.

- `scenarioConfiguration`:

  Whether to also print scenario configurations. Default is FALSE.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PITaskConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
