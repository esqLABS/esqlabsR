# Package index

## Project workflow

- [`ProjectConfiguration`](https://esqlabs.github.io/esqlabsR/dev/reference/ProjectConfiguration.md)
  : ProjectConfiguration

- [`createDefaultProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDefaultProjectConfiguration.md)
  :

  Create a default `ProjectConfiguration`

- [`createProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createProjectConfiguration.md)
  :

  Create a `ProjectConfiguration`

- [`exampleProjectConfigurationPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectConfigurationPath.md)
  : Get the path to example ProjectConfiguration.xlsx

- [`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md)
  : Initialize esqlabsR Project Folders and required Files

- [`isProjectInitialized()`](https://esqlabs.github.io/esqlabsR/dev/reference/isProjectInitialized.md)
  : Check if a directory contains an esqlabsR project

- [`restoreProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProjectConfiguration.md)
  : Import project configuration from JSON to Excel files

- [`snapshotProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProjectConfiguration.md)
  : Export project configuration Excel files to JSON

- [`Scenario`](https://esqlabs.github.io/esqlabsR/dev/reference/Scenario.md)
  : Scenario

- [`ScenarioConfiguration`](https://esqlabs.github.io/esqlabsR/dev/reference/ScenarioConfiguration.md)
  : ScenarioConfiguration

- [`addScenarioConfigurationsToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenarioConfigurationsToExcel.md)
  : Add scenario configurations to project Excel files

- [`createScenarioConfigurationsFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenarioConfigurationsFromPKML.md)
  : Create scenario configurations from PKML files

- [`createScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenarios.md)
  :

  Create `Scenario` objects from `ScenarioConfiguration` objects

- [`loadScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadScenarioResults.md)
  : Load simulated scenarios from csv and pkml.

- [`readScenarioConfigurationFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/readScenarioConfigurationFromExcel.md)
  : Read scenario definition(s) from Excel file

- [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
  : Run a set of scenarios.

- [`saveScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveScenarioResults.md)
  : Save results of scenario simulations to csv.

- [`projectConfigurationStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectConfigurationStatus.md)
  : Check if Excel configuration files are in sync with JSON snapshot

- [`createPlotsFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlotsFromExcel.md)
  :

  Generate plots as defined in excel file
  `projectConfiguration$plotsFile`

## Helpers for data import, wrangling, and export

- [`readExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/readExcel.md)
  :

  Read XLSX files using
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
  with suppressed warnings

- [`readIndividualCharacteristicsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readIndividualCharacteristicsFromXLS.md)
  : Read individual characteristics from file

- [`readParametersFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readParametersFromXLS.md)
  : Read parameter values from a structured Excel file. Each excel sheet
  must consist of columns 'Container Path', 'Parameter Name', 'Value',
  and 'Units'

- [`readPopulationCharacteristicsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readPopulationCharacteristicsFromXLS.md)
  :

  Read an excel file containing information about population and create
  a `PopulationCharacteristics` object

- [`readScenarioConfigurationFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/readScenarioConfigurationFromExcel.md)
  : Read scenario definition(s) from Excel file

- [`extendParameterStructure()`](https://esqlabs.github.io/esqlabsR/dev/reference/extendParameterStructure.md)
  : Extend parameters structure with new entries

- [`extendPopulationByUserDefinedParams()`](https://esqlabs.github.io/esqlabsR/dev/reference/extendPopulationByUserDefinedParams.md)
  : Add user defined variability on parameters to a population.

- [`extendPopulationFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/extendPopulationFromXLS.md)
  : Add user defined variability on parameters to a population from an
  excel file.

- [`setApplications()`](https://esqlabs.github.io/esqlabsR/dev/reference/setApplications.md)
  :

  Set an application protocol in a `Simulation` from the Excel file

- [`setParameterValuesByPathWithCondition()`](https://esqlabs.github.io/esqlabsR/dev/reference/setParameterValuesByPathWithCondition.md)
  :

  Set the values of parameters in the simulation by path, if the
  `condition` is true.

- [`writeIndividualToXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/writeIndividualToXLS.md)
  : Create a parameter set describing an individual and write it to the
  Excel file

- [`writeParameterStructureToXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/writeParameterStructureToXLS.md)
  : Write parameter structure to excel that can be loaded in MoBi

- [`exportParametersToXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportParametersToXLS.md)
  : Export simulation parameters to excel

- [`createDataCombinedFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombinedFromExcel.md)
  : Generate DataCombined objects as defined in excel file

## Helpers for modeling and simulation

- [`compareSimulations()`](https://esqlabs.github.io/esqlabsR/dev/reference/compareSimulations.md)
  : Compare two simulations

- [`compareWithNA()`](https://esqlabs.github.io/esqlabsR/dev/reference/compareWithNA.md)
  :

  Compare values including `NA`

- [`calculateMeanDataSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/calculateMeanDataSet.md)
  :

  Calculate mean and standard deviation for the yValues of the given
  `DataSet` objects

- [`initializeSimulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/initializeSimulation.md)
  : Load a simulation and apply a set of parameters.

- [`geomean()`](https://esqlabs.github.io/esqlabsR/dev/reference/geomean.md)
  : Calculate geometric mean of a numeric vector

- [`geosd()`](https://esqlabs.github.io/esqlabsR/dev/reference/geosd.md)
  : Calculate geometric standard deviation of a numeric vector

- [`sampleRandomValue()`](https://esqlabs.github.io/esqlabsR/dev/reference/sampleRandomValue.md)
  : Sample a random value from a distribution

- [`applyIndividualParameters()`](https://esqlabs.github.io/esqlabsR/dev/reference/applyIndividualParameters.md)
  :

  Apply an individual to the simulation. For human species, only
  parameters that do not override formulas are applied. For other
  species, all parameters returned by `createIndividual` are applied.

- [`Distributions`](https://esqlabs.github.io/esqlabsR/dev/reference/Distributions.md)
  : Supported distributions for sampling

- [`LLOQMode`](https://esqlabs.github.io/esqlabsR/dev/reference/LLOQMode.md)
  :

  Possible entries for the `lloqMode` argument of `calculateMeans()`

- [`ULOQMode`](https://esqlabs.github.io/esqlabsR/dev/reference/ULOQMode.md)
  : Possible modes to treat values above the upper limit of
  quantification.

- [`loadObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadObservedData.md)
  : Load data from excel

- [`loadObservedDataFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadObservedDataFromPKML.md)
  : Load data from pkml

## Helpers for plotting

- [`col2hsv()`](https://esqlabs.github.io/esqlabsR/dev/reference/col2hsv.md)
  : Returns the HSV values for a given R color name

- [`esqlabsColors()`](https://esqlabs.github.io/esqlabsR/dev/reference/esqlabsColors.md)
  : esqLABS color palette

- [`createEsqlabsExportConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createEsqlabsExportConfiguration.md)
  :

  Create an instance of `ExportConfiguration` R6 class

- [`createEsqlabsPlotConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createEsqlabsPlotConfiguration.md)
  :

  Create an instance of `DefaultPlotConfiguration` R6 class

- [`createEsqlabsPlotGridConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createEsqlabsPlotGridConfiguration.md)
  :

  Create an instance of `PlotGridConfiguration` R6 class

- [`createDataCombinedFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombinedFromExcel.md)
  : Generate DataCombined objects as defined in excel file

## Input validation

- [`validateAllConfigurations()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateAllConfigurations.md)
  : Validate all configuration files in a project
- [`validationResult`](https://esqlabs.github.io/esqlabsR/dev/reference/validationResult.md)
  : validationResult
- [`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md)
  : Get summary of all validation results
- [`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md)
  : Check if validation results contain any critical errors
- [`isParametersEqual()`](https://esqlabs.github.io/esqlabsR/dev/reference/isParametersEqual.md)
  : Check if two parameters are equal with respect to certain
  properties.
- [`isProjectInitialized()`](https://esqlabs.github.io/esqlabsR/dev/reference/isProjectInitialized.md)
  : Check if a directory contains an esqlabsR project
- [`isTableFormulasEqual()`](https://esqlabs.github.io/esqlabsR/dev/reference/isTableFormulasEqual.md)
  : Check if two table formulas are equal.
- [`stringToNum()`](https://esqlabs.github.io/esqlabsR/dev/reference/stringToNum.md)
  : Convert string to numeric
- [`GenderInt`](https://esqlabs.github.io/esqlabsR/dev/reference/GenderInt.md)
  : Possible gender entries as integer values

## Sensitivity analysis

- [`loadSensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadSensitivityCalculation.md)
  : Load Sensitivity Calculation Results
- [`saveSensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveSensitivityCalculation.md)
  : Save Sensitivity Calculation Results
- [`sensitivityCalculation()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityCalculation.md)
  : Carry out and visualize sensitivity analysis (with OSPSuite)
- [`sensitivitySpiderPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivitySpiderPlot.md)
  : Sensitivity Spider Plot for Pharmacokinetic Parameters
- [`sensitivityTimeProfiles()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityTimeProfiles.md)
  : Time Profile plots for Sensitivity Analysis
- [`sensitivityTornadoPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/sensitivityTornadoPlot.md)
  : Tornado Plot for Sensitivity Analysis

## Shiny applications

## Miscellaneous

- [`enumPutList()`](https://esqlabs.github.io/esqlabsR/dev/reference/enumPutList.md)
  : Add a new key-value pairs to an enum, where the value is a list.

- [`getAllApplicationParameters()`](https://esqlabs.github.io/esqlabsR/dev/reference/getAllApplicationParameters.md)
  : Get parameters of applications in the simulation

- [`getEsqlabsRSetting()`](https://esqlabs.github.io/esqlabsR/dev/reference/getEsqlabsRSetting.md)
  : Get the value of a global esqlabsR setting.

- [`getIndexClosestToValue()`](https://esqlabs.github.io/esqlabsR/dev/reference/getIndexClosestToValue.md)
  : Find value in an array

- [`getMoleculeNameFromQuantity()`](https://esqlabs.github.io/esqlabsR/dev/reference/getMoleculeNameFromQuantity.md)
  : Get the name of the molecule from a quantity

- [`executeInParallel()`](https://esqlabs.github.io/esqlabsR/dev/reference/executeInParallel.md)
  : Parallelize the execution of a function over a list of arguments
  values

- [`pathFromClipboard()`](https://esqlabs.github.io/esqlabsR/dev/reference/pathFromClipboard.md)
  : Convert Windows filepaths for R

- [`removeFromList()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeFromList.md)
  : Remove an entry from a list

- [`sourceAll()`](https://esqlabs.github.io/esqlabsR/dev/reference/sourceAll.md)
  : Source all .R files located in a specific folder

- [`esqlabsRSettingNames`](https://esqlabs.github.io/esqlabsR/dev/reference/esqlabsRSettingNames.md)
  :

  Names of the settings stored in esqlabsEnv Can be used with
  [`getEsqlabsRSetting()`](https://esqlabs.github.io/esqlabsR/dev/reference/getEsqlabsRSetting.md)

## Inherited classes

- [`ExportConfiguration`](https://esqlabs.github.io/esqlabsR/dev/reference/ExportConfiguration.md)
  : ExportConfiguration
