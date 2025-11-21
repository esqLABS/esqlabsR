# ScenarioConfiguration

An object storing configuration of a specific scenario

## Value

A new `ScenarioConfiguration` object.

## Public fields

- `scenarioName`:

  Name of the simulated scenario

- `modelFile`:

  Name of the simulation to be loaded (must include the extension
  ".pkml"). Must be located in the "modelFolder".

- `applicationProtocol`:

  Name of the application protocol to be applied. Defined in the excel
  file "Applications.xlsx"

- `individualId`:

  Id of the individual as specified in "Individuals.xlsx". If `NULL`
  (default), the individual as defined in the simulation file will be
  simulated.

- `populationId`:

  Id of the population as specified in "Populations.xlsx", sheet
  "Demographics". If `ScenarioConfguration$simulationType` is
  `population`, a population will be created a the scenario will be
  simulated as a population simulation.

- `outputPaths`:

  a character vector or named vector of output paths for which the
  results will be calculated. If `NULL` (default), outputs as defined in
  the simulation are used. Can be a named vector where names serve as
  aliases for the paths, e.g., c("plasma" =
  "Organism\|VenousBlood\|Plasma\|AKB-9090\|Concentration in
  container").

## Active bindings

- `simulateSteadyState`:

  Boolean representing whether the simulation will be brought to a
  steady-state first

- `readPopulationFromCSV`:

  Boolean representing whether the a new population will be created
  (value is `FALSE`) or an existing population will be imported from a
  csv.

- `simulationTime`:

  Specified simulation time intervals. If `NULL` (default), simulation
  time as defined in the `Simulation` object will be used. Accepted are
  multiple time intervals separated by a ';'. Each time interval is a
  triplet of values \<StartTime, EndTime, Resolution\>, where
  `Resolution` is the number of simulated points per time unit defined
  in the column `TimeUnit`.

- `simulationTimeUnit`:

  Unit of the simulation time intervals.

- `steadyStateTime`:

  Time in minutes to simulate if simulating steady-state. May be `NULL`.

- `paramSheets`:

  A named list. Names of the sheets from the parameters-excel file that
  will be applied to the simulation

- `simulationType`:

  Type of the simulation - "Individual" or "Population". If
  "Population", population characteristics are created based on
  information stored in `populationsFile`. Default is "Individual"

- `projectConfiguration`:

  `ProjectConfiguration` that will be used in scenarios. Read-only

## Methods

### Public methods

- [`ScenarioConfiguration$new()`](#method-ScenarioConfiguration-new)

- [`ScenarioConfiguration$addParamSheets()`](#method-ScenarioConfiguration-addParamSheets)

- [`ScenarioConfiguration$removeParamSheets()`](#method-ScenarioConfiguration-removeParamSheets)

- [`ScenarioConfiguration$print()`](#method-ScenarioConfiguration-print)

- [`ScenarioConfiguration$clone()`](#method-ScenarioConfiguration-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    ScenarioConfiguration$new(projectConfiguration)

#### Arguments

- `projectConfiguration`:

  An object of class `ProjectConfiguration`.

------------------------------------------------------------------------

### Method `addParamSheets()`

Add the names of sheets in the parameters excel-file that will be
applied to the simulation

#### Usage

    ScenarioConfiguration$addParamSheets(sheetNames)

#### Arguments

- `sheetNames`:

  A name or a list of names of the excel sheet

------------------------------------------------------------------------

### Method `removeParamSheets()`

Remove the names of sheets in the parameters excel-file from the list of
sheets `paramSheets`

#### Usage

    ScenarioConfiguration$removeParamSheets(sheetNames = NULL)

#### Arguments

- `sheetNames`:

  A name or a list of names of the excel sheet. If `NULL` (default), all
  sheets are removed.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    ScenarioConfiguration$print(className = TRUE, projectConfiguration = FALSE)

#### Arguments

- `className`:

  Whether to print the name of the class at the beginning. default to
  TRUE.

- `projectConfiguration`:

  Whether to also print project configuration. default to TRUE.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ScenarioConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
