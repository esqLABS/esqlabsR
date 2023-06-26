# esqlabsR (development version)

- Protein ontogenies can be defined for populations and individuals. To specify ontogenies for 
proteins in the simulation, list the proteins you want to define ontogenies for 
in the column 'Protein' of files 'PopulationParameters.xlsx' or 'Individuals.xlsx', separated by a ','. Speficy the ontogenies available in PK-Sim (see article https://www.open-systems-pharmacology.org/OSPSuite-R/articles/create-individual.html#adding-enzyme-ontogenies) in the column 'Ontogenies'. The number of entries in the  both columns must be equal.

- Excel file 'PopulationParameters.xlsx' gets additional columns 'Protein' and 'Ontogeny'.
- Excel file 'Individuals.xlsx' gets additional columns 'Protein' and 'Ontogeny'.

# esqlabsR 5.0.0

### New functionalities
- New function `loadObservedDataFromPKML()` to load data from `*.pkml` located in 
the "PKML" sub-folder of the "Data" folder.

- New function `createScenarios()` to create `Scenario` objects from `ScenarioConfiguration` objects

- Plots can be created by calling the new function `createPlotsFromExcel()`. It requires as input parameters `simulatedScenarios` (a list of simulated scenarios as returned by `runScenarios()`), `observedData` (a list of `DataSet` objects) and a `ProjectConfiguration` object `projectConfiguration`.

- New function `createDataCombinedFromExcel()` creates `DataCombined` objects as 
defined in the `Plots.xlsx` file.

- New function `saveScenarioResults()` to save results produced by the `runScenarios()` 
function to csv files and corresponding simulations to pkml files.

- New function `loadScenarioResults()` to load results from csv files into a structure
as produced by the `runScenarios()` function.

- New function `compareSimulation()` to compare two simulations for differences.

- `runScenarios()` also returns a `Population` object for population simulations.
- `runScenarios()` gets a new argument `savePopulationToCSV`, with default value `FALSE`.

### BREAKING CHANGES

- Field `setTestParameters` removed from `ScenarioConfiguration`
- Function `initializeScenario()` has been removed
- Definition of simulation time in the `Scenarios.xlsx` file changed. The new expected format 
is a triplet of values <StartTime, EndTime, Resolution>, where `Resolution` is the number of 
simulated points per time unit defined in the column `TimeUnit`.
- Field `poinstPerMinute` of `ScenarioConfiguration` has been removed.
- Function `compareSimulationParameters()` has been removed and replaced by `compareSimulations()`
- `Scenarios` excel file gets additional columns `SteadyStateTime`, `SteadyStateTimeUnit`, 
`PopulationId`, `OutputPathsIds`.
- `readScenarioConfigurationFromExcel()` has a new signature and requires a list of 
`scenarioNames` and a `ProjectConfiguration`. The output is a named list of `ScenarioConfiguration` 
objects.
- Output paths are not set from global variable `OutputPaths` any more but 
from the respective field of `ScenarioConfgiruation`
- `ProjectConfiguration` does not have field `$outputDevice` any more.
- `ScenarioConfiguration` does not store `SimulationRunOptions` any more. Simulation run options must be passed to the `runScenarios()` function. Different run options cannot be used within one scenarios run.
- Enum `GraphicsDevices` has been removed.
- Function `initializeSimulation()` does not have arguments `simulateSteadyState`, `steadyStateTime` and `simulationRunOptions` any more.

### MAJOR CHANGES

- New class `Scenario` that represents a scenario created from a `ScenarioConfiguration`

- `ScenarioConfiguration` gets a new field `outputPaths` which is a list of 
output paths for which the results will be calculated. If `NULL` (default), 
outputs as defined in the simulation are used.

- Paths of model outputs are defined in the excel file `Scenarios.xlsx`. In the 
sheet `OutputPaths`, create an entry for each output. The column `OutputPath` is the full 
path to the output, while `OutputPathId` is an identifier that conveniently allows 
to select the correct output.
In the `Scenarios` sheet, enter the IDs of all paths the outputs should be generated for,
separated by a `,`, e.g. `Aciclovir_PVB, Aciclovir_fat_cell`. 

If no outputs are specified, the outputs as defined in the simulation `.pkml` file
will be produced.

- `ScenarioConfiguration` gets a new field `populationId`, specifying the id of 
the population as defined in the `PopulationParameters.xlsx` file, sheet `Demographics`.
If the field is `NULL`, the scenario is simulated as an individual simulation, 
otherwise a population simulation is performed.

- `ScenarioConfiguration` gets a new field `readPopulationFromCSV`. If `FALSE` (default), 
a new population is created from defined population demographics. If `TRUE`, a simulation
will be imported from a csv sheet located in the folder `Parameters/Populations` and 
named as the `PopulationId`.

- `runScenarios()` supports scenario configurations for population simulations

- Target folder for saving `*.pkml` simulations when `runScenarios(scenarioConfigurations, saveSimulationsToPKML = TRUE)`
changed from `Models/Simulations/<DateSuffix>` to `Results/SimulationResults/<DateSuffix>`.

- `sensitivityCalculation()` - fixed bug in wrong calculation of sensitivity values.
Please be aware that the results produced by earlier versions are wrong.

* The workflow for running scenarios changed to:  
  - Create a `ProjectConfiguration` with `createDefaultProjectConfiguration()`
  - Create `ScenarioConfigurations`, e.g. with `readScenarioConfigurationFromExcel(scenarioNames, projectConfiguration)`
  - Run scenarios with `runScenarios(scenarioConfigurations)`
  
  - Alternatively: 
  - Create a `ProjectConfiguration` with `createDefaultProjectConfiguration()`
  - Create `ScenarioConfigurations`, e.g. with `readScenarioConfigurationFromExcel(scenarioNames, projectConfiguration)`
  
  
- `ProjectConfiguration` gets a new field `plotsFile`. It is the name of the excel file with plot definitions and must be located in the `paramsFolder`.

- When defining an individual of other species than human in `ScenarioConfiguration` 
and applying it to a human model, missing species-specific parameters are applied and the 
scaling works properly. Supported scalingsa are: Human to rat, human to monkey, 
human to rabbit.

- `initializeSimulation()` does not perform steady-state run any more. This is done as part of the `runScenarios()` function.

### MINOR CHANGES

- Function `stringToNum()` gets additional arguments `lloqMode` and `uloqMode`
that determine how entries of type "<number" and ">number" will be treated.

- `readScenarioConfigurationFromExcel()` will read all scenarios defined in the 
`Scenarios.xlsx` file if no scenario names are specified (argument `scenarioNames = NULL`).

- Function `setApplications()` is deprecated.

- Dark grey frame around legends by default.

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

- Maintenance and bug fixes.

- The package gains a new dependency: [`{ospsuite.utils}`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/).

------

# esqlabsR 2.0.0

- Maintenance and bug fixes.

------

# esqlabsR 1.0.0

- Initial release of the package.
