# esqlabsR 5.0.0

### BREAKING CHANGES

- `readScenarioConfigurationFromExcel()` has a new signature and requires a list of 
`scenarioNames` and a `ProjectConfiguration`. The output is a named list of `ScenarioConfiguration` 
objects.
- `initializeScenario()` does not update scenario configuration from excel any more.
- Output paths are not set from global variable `OutputPaths` any more but 
from the respective field of `ScenarioConfgiruation`

### MAJOR CHANGES

- `ScenarioConfiguration` gets a new field `outputPaths` which is a list of 
output paths for which the results will be calculated. If `NULL` (default), 
outputs as defined in the simulation are used.

* The workflow for running scenarios changed to:  
  - Create a `RunConfiguration` with `createDefaultProjectConfiguration()`
  - Create `ScenarioConfigurations`, e.g. with `readScenarioConfigurationFromExcel(scenarioNames, projectConfiguration)`
  - Run scenarios with `runScenarios(scenarioConfigurations)`
  
### MINOR CHANGES

- Function `stringToNum()` gets additional arguments `lloqMode` and `uloqMode`
that determine how entries of type "<number" and ">number" will be treated.

------

# esqlabsR 4.0.0

MAJOR CHANGES

- Three new functions to create configuration objects needed for data visualization workflows:
  
  * `createEsqlabsPlotConfiguration()`
  * `createEsqlabsPlotGridConfiguration()`
  * `createEsqlabsExportConfiguration()`

- New function `getAllApplicationParameters()` that returns all parameters of 
applications in a simulation
- New function `exportParametersToXLS()` to write parameter information into an 
excel file that can be loaded in MoBi or R using the `readParametersFromXLS()`
function.
- New function `writeExcel()` that is a wrapper for creating a directory 
(if not present) and writing to excel file using `writexl::write_xlsx()`.

BREAKING CHANGES

- The package requires R version >=4.1.

- The package gains new dependencies:

  * [`{ospsuite.parameteridentification}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/) 
  * [`{tlf}`](https://www.open-systems-pharmacology.org/TLF-Library/).

- Function `getSteadyState()` has been moved to package `{ospsuite.parameteridentification}`.

- Function `loadObservedData()` requires a `ProjectConfiguration` instead of a 
`ScenarioConfiguration`.

- `DataMapping`, `DataMappingConfiguration`, `XYData`, `DataConfiguration` and 
associated functions for creating standard figures are moved to `esqlabsRLegacy` 
[package](https://esqlabs.github.io/esqlabsRLegacy/).

------

# esqlabsR 3.0.0

NEW FUNCTIONS

- To carry out and visualize sensitivity analysis:

 * `sensitivityCalculation()`
 * `sensitivitySpiderPlot()`
 * `sensitivityTimeProfiles()`

- Classes and functions for standard esqLABS simulation workflow:

 * `ProjectConfiguration`
 * `ScenarioConfiguration`
 * `createDefaultProjectConfiguration()`
 * `readScenarioConfigurationFromExcel()`
 * `setApplications()`
 * `initializeScenario()`
 * `initializeScenario()`

- Maintenance and bug fixes.

- The package gains a new dependency: [`{ospsuite.utils}`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/).

------

# esqlabsR 2.0.0

- Maintenance and bug fixes.

------

# esqlabsR 1.0.0

- Initial release of the package.
