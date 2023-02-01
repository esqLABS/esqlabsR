# esqlabsR 5.0.0

### BREAKING CHANGES

- `Scenarios` excel file gets additional columns `SteadyStateTime`, `SteadyStateTimeUnit`, 
`PopulationId`, `OutputPathsIds`.
- `readScenarioConfigurationFromExcel()` has a new signature and requires a list of 
`scenarioNames` and a `ProjectConfiguration`. The output is a named list of `ScenarioConfiguration` 
objects.
- `initializeScenario()` does not update scenario configuration from excel any more.
- Output paths are not set from global variable `OutputPaths` any more but 
from the respective field of `ScenarioConfgiruation`
- `ProjectConfiguration` does not have field `$outputDevice` any more.
- Enum `GraphicsDevices` has been removed.

### MAJOR CHANGES

- Paths of model outputs are defined in the excel file `Scenarios.xlsx`. In the 
sheet `OutputPaths`, create an entry for each output. The column `OutputPath` is the full 
path to the output, while `OutputPathId` can be an identifier that conveniently allows 
to select the correct output.
In the `Scenarios` sheet, enter the IDs of all paths the outputs should be generated for,
separated by a `,`, e.g. `Aciclovir_PVB, Aciclovir_fat_cell`. 

If no outputs are specified, the outputs as defined in the simulation `.pkml` file
will be produced.

- `ScenarioConfiguration` gets a new field `populationId`, specifying the id of 
population as defined in the `PopulationParameters.xlsx` file, sheet `Demographics`.
If the field is `NULL`, the scenario is simulated as an individual simulation, 
otherwise a population simulation is performed.

- `runScenraios()` supports scenario configurations for population simulations

- Target folder for saving `*.pkml` simulations when `runScenarios(scenarioConfigurations, saveSimulationsToPKML = TRUE)`
changed from `Models/Simulations/<DateSuffix>` to `Results/SimulationResults/<DateSuffix>`.

- `sensitivityCalculation()` - fixed bug in wrong calculation of sensitivity values.
Please be aware that the results produced by earlier versions are wrong.

- `ScenarioConfiguration` gets a new field `outputPaths` which is a list of 
output paths for which the results will be calculated. If `NULL` (default), 
outputs as defined in the simulation are used.

* The workflow for running scenarios changed to:  
  - Create a `RunConfiguration` with `createDefaultProjectConfiguration()`
  - Create `ScenarioConfigurations`, e.g. with `readScenarioConfigurationFromExcel(scenarioNames, projectConfiguration)`
  - Run scenarios with `runScenarios(scenarioConfigurations)`
  
- `ProjectConfiguration` gets a new field `plotsFile`. It is the name of the excel file with plot definitions and must be located in the `paramsFolder`.
- Plots can be created by calling the new function `createPlotsFromExcel()`. It requires as input parameters `simulatedScenarios` (a list of simulated scenarios as returned by `runScenarios()`), `observedData` (a list of `DataSet` objects) and a `ProjectConfiguration` object `projectConfiguration`.

- `ScenarioConfiguration` supports custom functions to modify the `Simulation` 
object as the last step of scenario initialization. For this, the `ScenarioConfiguration` 
class gets new fields `customFunction` and `customFunctionArgs`.

- New function `saveScenarioResults()` to save results produced by the `runScenarios()` 
function to csv files.

- New function `loadScenarioResults()` to load results from csv files into a structure
as produced by the `runScenarios()` function.

### MINOR CHANGES

- Function `stringToNum()` gets additional arguments `lloqMode` and `uloqMode`
that determine how entries of type "<number" and ">number" will be treated.

- `readScenarioConfigurationFromExcel()` will read all scenarios defined in the 
`Scenarios.xlsx` file if no scenario names are specified (argument `scenarioNames = NULL`).

- Default name of the excel file with definition of individual biometrics changed 
from `IndividualPhysiology.xlsx` to `IndividualBiometrics.xlsx`.

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
