# Create scenarios from PKML files

Reads scenarios from PKML files, extracting output paths and simulation
time settings, and adds them to a `Project` in place. Output paths are
registered in `outputPaths` definitions (reusing an existing id when the
literal path is already registered, otherwise generating a readable
one), and scenario names are made unique against the scenarios already
on the project. The function mutates `project` directly and returns it
invisibly, like the other `add*` mutators; the created scenario names
are reported in a message.

## Usage

``` r
createScenariosFromPKML(
  pkmlFilePaths,
  project,
  scenarios = NULL,
  individual = NULL,
  population = NULL,
  application = NULL,
  parameterSets = NULL,
  outputPaths = NULL,
  simulationTime = NULL,
  simulationTimeUnit = NULL,
  steadyState = FALSE,
  steadyStateTime = NULL,
  steadyStateTimeUnit = NULL,
  overwriteFormulasInSS = FALSE,
  readPopulationFromCSV = FALSE,
  paramSheets = lifecycle::deprecated()
)
```

## Arguments

- pkmlFilePaths:

  Character vector of paths to PKML files to create scenarios from. Can
  be a single string (recycled for all scenarios) or a vector with the
  same length as the number of scenarios being created (determined by
  the longest vector argument).

- project:

  A `Project` object holding base information.

- scenarios:

  Character vector. Optional custom names for the scenarios. If `NULL`
  (default), scenario names will be extracted from the simulation names
  in the PKML files. If provided, must have the same length as
  `pkmlFilePaths`.

- individual:

  Character vector. Optional individual ids to use for scenarios. If
  `NULL` (default), no individual will be specified. Can be a single
  string (recycled for all scenarios) or a vector with the same length
  as `pkmlFilePaths`.

- population:

  Character vector. Optional population ids to use for scenarios. If
  `NULL` (default), no population will be specified. If provided, sets
  simulation type to "Population". Can be a single string (recycled for
  all scenarios) or a vector with the same length as `pkmlFilePaths`.

- application:

  Character vector. Optional application protocol ids to use for
  scenarios, each referencing `applications` definitions. If `NULL`
  (default), the scenario has no application protocol (the PKML file
  already embeds its own application). Values are used verbatim and are
  validated against `applications` definitions. Can be a single string
  (recycled for all scenarios) or a vector with the same length as
  `pkmlFilePaths`.

- parameterSets:

  Character vector. Optional parameter set ids to apply to scenarios
  (referencing `parameterSets` definitions). If `NULL` (default), no
  parameter sets will be applied. Can be a single string (recycled for
  all scenarios) or a vector with the same length as `pkmlFilePaths`. If
  providing multiple set ids per scenario, separate them with commas in
  the string.

- outputPaths:

  Character vector or named vector. Optional output paths to use for
  scenarios. If `NULL` (default), output paths will be extracted from
  the PKML files' output selections. Can be a single string (recycled
  for all scenarios) or a vector with the same length as
  `pkmlFilePaths`. If providing multiple paths per scenario, separate
  them with commas in the string. Named vectors are supported where the
  names become the registered output-path ids, e.g.,
  `c("plasma" = "Organism|VenousBlood|Plasma|Drug|Concentration")`. When
  a literal path is already registered in `outputPaths` definitions, its
  existing id is reused; unnamed new paths receive a readable generated
  id.

- simulationTime:

  Optional simulation time to use for scenarios. One interval is a
  length-3 numeric vector `c(start, end, resolution)` or the same
  triplet as a string `"start, end, resolution"`, where `resolution` is
  the number of simulated points per time unit defined in the
  `simulationTimeUnit`; several intervals go in one string separated by
  `';'`. If `NULL` (default), simulation time will be extracted from the
  PKML files' output schema intervals. Can be a single value (recycled
  for all scenarios) or, as a list or character vector, one value per
  entry of `pkmlFilePaths`.

- simulationTimeUnit:

  Character vector. Optional simulation time unit. It is the unit
  `simulationTime` is given in, and, when `simulationTime` is left to
  PKML extraction, the unit the extracted interval bounds are converted
  to. If `NULL` (default), it is taken from the PKML file's output
  schema intervals, or set to `"min"` (minutes) if not available. Can be
  a single string (recycled for all scenarios) or a vector with the same
  length as `pkmlFilePaths`.

- steadyState:

  Logical vector. Whether to simulate steady-state for each scenario.
  Default is `FALSE`. Can be a single logical value (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

- steadyStateTime:

  Numeric vector. Steady-state times in `steadyStateTimeUnit`. If `NULL`
  (default), `1000` is used (matching
  [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)).
  The value is stored in base units (minutes) on the scenario. Can be a
  single numeric value (recycled for all scenarios) or a vector with the
  same length as `pkmlFilePaths`.

- steadyStateTimeUnit:

  Character vector. Steady-state time units. Only used when
  `steadyState = TRUE` and `steadyStateTime` is provided. If `NULL`
  (default), `"min"` will be used. Can be a single string (recycled for
  all scenarios) or a vector with the same length as `pkmlFilePaths`.

- overwriteFormulasInSS:

  Logical vector. Whether to overwrite formula-defined parameters with
  their steady-state values. When `TRUE`, corresponds to
  `ignoreIfFormula = FALSE` in
  [`ospsuite::getSteadyState()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/getSteadyState.html)
  (formulas are overwritten). Default is `FALSE` (formula-defined
  parameters are kept unchanged). Can be a single logical value
  (recycled for all scenarios) or a vector with the same length as
  `pkmlFilePaths`.

- readPopulationFromCSV:

  Logical vector. Whether to read population from CSV for each scenario.
  Default is `FALSE`. Can be a single logical value (recycled for all
  scenarios) or a vector with the same length as `pkmlFilePaths`.

- paramSheets:

  **\[deprecated\]** Use `parameterSets` instead.

## Value

The `project`, invisibly, with the new scenarios added to `scenarios`
definitions and any new output paths registered in `outputPaths`
definitions.

## Details

This function extracts the following information from PKML files:

- **Output paths**: All selected outputs for the simulation from
  `outputSelections$allOutputs`.

- **Simulation time**: Time intervals with start time, end time, and
  resolution from `outputSchema$intervals`.

- **Simulation time unit**: Time unit from the output schema intervals
  (e.g., `"h"` for hours).

### Vector arguments and recycling

All arguments support vectorization to create scenarios with different
parameter values:

- **Length 1**: The value is recycled (applied to all scenarios).

- **Length \> 1**: All vector arguments must have the same length, which
  determines the number of scenarios.

- **Mixed lengths**: An error is thrown if vector arguments have
  inconsistent lengths.

The number of scenarios created is determined by the longest vector
argument. All shorter vectors (including `pkmlFilePaths`) are recycled
to match this length.

This allows you to efficiently create multiple scenarios in several
ways:

- **Same PKML, different settings**: Use a single PKML file with vectors
  of different parameter values.

- **Different PKMLs, same settings**: Use multiple PKML files with
  single parameter values.

- **Different PKMLs, different settings**: Use vectors of both PKML
  files and parameter values.

The function handles duplicate scenario names, both against each other
and against the scenarios already on the project, by appending indices
(e.g., `"Scenario"`, `"Scenario_2"`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Load project
project <- loadProject("Project.json")

# Read scenarios from a single PKML file into the project
createScenariosFromPKML(
  pkmlFilePaths = "path/to/simulation.pkml",
  project = project
)

# The project now holds the new scenarios (already written through to
# their definition files); run them
results <- runScenarios(project)

# Example of vector recycling: single value applied to all scenarios
createScenariosFromPKML(
  pkmlFilePaths = c("sim1.pkml", "sim2.pkml", "sim3.pkml"),
  project = project,
  individual = "Individual_001",
  steadyState = TRUE,
  steadyStateTime = 1000
)

# Example of vector arguments: different values per scenario
createScenariosFromPKML(
  pkmlFilePaths = c("pediatric.pkml", "adult.pkml", "elderly.pkml"),
  project = project,
  scenarios = c("Pediatric", "Adult", "Elderly"),
  individual = c("Child_001", "Adult_001", "Elderly_001"),
  steadyState = c(FALSE, TRUE, TRUE),
  steadyStateTime = c(NA, 2000, 1500)
)
} # }
```
