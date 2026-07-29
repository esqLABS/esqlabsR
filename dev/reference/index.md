# Package index

## Project workflow

- [`Project`](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)
  [`ProjectConfiguration`](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)
  : Project

- [`loadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadProject.md)
  : Load a project from a JSON configuration file

- [`saveProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveProject.md)
  : Save the project to the disk

- [`reloadProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/reloadProject.md)
  : Discard a project's unsaved changes and re-read it from disk

- [`snapshotProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/snapshotProject.md)
  : Save the whole project to a single shareable snapshot file

- [`restoreProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/restoreProject.md)
  : Recreate a project folder from a snapshot file

- [`projectStatus()`](https://esqlabs.github.io/esqlabsR/dev/reference/projectStatus.md)
  : Check a loaded project for unsaved changes and outdated Excel files

- [`initProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/initProject.md)
  : Initialize esqlabsR Project Folders and required Files

- [`exampleProjectPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/exampleProjectPath.md)
  : Get the path to the example Project.json

- [`importProjectFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/importProjectFromExcel.md)
  : Import project configuration from Excel files

- [`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md)
  : Export a Project to Excel files

- [`createScenariosFromPKML()`](https://esqlabs.github.io/esqlabsR/dev/reference/createScenariosFromPKML.md)
  : Create scenarios from PKML files

- [`Scenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/Scenario.md)
  : Create a Scenario

- [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  : Add one or more scenarios programmatically to a Project

- [`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md)
  : Duplicate an existing scenario

- [`loadScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/loadScenarioResults.md)
  : Load simulated scenarios from csv and pkml.

- [`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md)
  : Remove one or more scenarios from a Project

- [`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md)
  : Rename an existing scenario

- [`runScenarios()`](https://esqlabs.github.io/esqlabsR/dev/reference/runScenarios.md)
  :

  Run a set of scenarios from a `Project`.

- [`saveScenarioResults()`](https://esqlabs.github.io/esqlabsR/dev/reference/saveScenarioResults.md)
  : Save results of scenario simulations to csv.

- [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  : Modify fields of an existing scenario

## Programmatic Project mutation

- [`addScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/addScenario.md)
  : Add one or more scenarios programmatically to a Project
- [`removeScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeScenario.md)
  : Remove one or more scenarios from a Project
- [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  : Modify fields of an existing scenario
- [`renameScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/renameScenario.md)
  : Rename an existing scenario
- [`duplicateScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/duplicateScenario.md)
  : Duplicate an existing scenario
- [`addIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/addIndividual.md)
  : Add one or more individuals to a Project
- [`removeIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeIndividual.md)
  : Remove one or more individuals from a Project
- [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md)
  : Modify fields of an existing individual
- [`addPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPopulation.md)
  : Add one or more populations to a Project
- [`removePopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePopulation.md)
  : Remove one or more populations from a Project
- [`setPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/setPopulation.md)
  : Modify fields of an existing population
- [`addApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/addApplication.md)
  : Add one or more application protocols to a Project
- [`removeApplication()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeApplication.md)
  : Remove one or more application protocols from a Project
- [`setApplicationParameterSets()`](https://esqlabs.github.io/esqlabsR/dev/reference/setApplicationParameterSets.md)
  : Replace the parameter-set references on one or more applications
- [`addOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/addOutputPath.md)
  : Add one or more output paths to a Project
- [`removeOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeOutputPath.md)
  : Remove one or more output paths from a Project
- [`setOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/setOutputPath.md)
  : Change the literal path of one or more existing output paths
- [`addObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/addObservedData.md)
  : Add observed data to a Project
- [`removeObservedData()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeObservedData.md)
  : Remove one or more observed-data sources from a Project
- [`getObservedDataNames()`](https://esqlabs.github.io/esqlabsR/dev/reference/getObservedDataNames.md)
  : Get names of all observed data in a Project
- [`addDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/addDataCombined.md)
  : Add one or more DataCombined to a Project
- [`removeDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeDataCombined.md)
  : Remove one or more DataCombined from a Project
- [`addPlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlot.md)
  : Add a plot configuration to a Project
- [`removePlot()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlot.md)
  : Remove one or more plot configurations from a Project
- [`addPlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPlotGrid.md)
  : Add one or more plot grids to a Project
- [`removePlotGrid()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePlotGrid.md)
  : Remove one or more plot grids from a Project
- [`addParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterSet.md)
  : Create one or more parameter sets
- [`removeParameterSet()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterSet.md)
  : Remove one or more parameter sets
- [`addParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addParameterEntry.md)
  : Add one or many parameter entries to a named parameter set
- [`removeParameterEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeParameterEntry.md)
  : Remove one or many parameter entries from a named parameter set
- [`addInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditions.md)
  : Create one or more initial-condition sets
- [`removeInitialConditions()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditions.md)
  : Remove one or more initial-condition sets
- [`addInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/addInitialConditionEntry.md)
  : Add one or many entries to a named initial-condition set
- [`removeInitialConditionEntry()`](https://esqlabs.github.io/esqlabsR/dev/reference/removeInitialConditionEntry.md)
  : Remove one or many entries from a named initial-condition set

## Parameter identification workflow

- [`PITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/PITask.md)
  : Create a Parameter Identification task
- [`PIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIParameter.md)
  : Create a Parameter Identification parameter
- [`PIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/PIOutputMapping.md)
  : Create a Parameter Identification output mapping
- [`runPI()`](https://esqlabs.github.io/esqlabsR/dev/reference/runPI.md)
  : Run Parameter Identification tasks defined in a Project
- [`addPITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPITask.md)
  : Add a Parameter Identification task to a Project
- [`removePITask()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePITask.md)
  : Remove one or more Parameter Identification tasks from a Project
- [`addPIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIParameter.md)
  : Add a parameter to an existing PI task
- [`removePIParameter()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIParameter.md)
  : Remove a parameter from a PI task
- [`addPIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/addPIOutputMapping.md)
  : Add an output mapping to an existing PI task
- [`removePIOutputMapping()`](https://esqlabs.github.io/esqlabsR/dev/reference/removePIOutputMapping.md)
  : Remove an output mapping from a PI task

## Deprecated parameter identification API

Legacy entry points retained for migration only. They emit a deprecation
warning and abort. Use the project-driven API above.

- [`createPITasks()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPITasks.md)
  **\[defunct\]** : Build Parameter Identification tasks (defunct)

## Helpers for data import, wrangling, and export

- [`readExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/readExcel.md)
  :

  Read XLSX files using
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
  with suppressed warnings

- [`readInitialConditionsFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readInitialConditionsFromXLS.md)
  : Read initial values (molecule start values) from a structured Excel
  file.

- [`readParametersFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/readParametersFromXLS.md)
  : Read parameter values from a structured Excel file. Each excel sheet
  must consist of columns 'Container Path', 'Parameter Name', 'Value',
  and 'Units'

- [`extendParameterStructure()`](https://esqlabs.github.io/esqlabsR/dev/reference/extendParameterStructure.md)
  : Extend parameters structure with new entries

- [`extendPopulationByUserDefinedParams()`](https://esqlabs.github.io/esqlabsR/dev/reference/extendPopulationByUserDefinedParams.md)
  : Add user defined variability on parameters to a population.

- [`extendPopulationFromXLS()`](https://esqlabs.github.io/esqlabsR/dev/reference/extendPopulationFromXLS.md)
  : Add user defined variability on parameters to a population from an
  excel file.

- [`setApplicationParameterSets()`](https://esqlabs.github.io/esqlabsR/dev/reference/setApplicationParameterSets.md)
  : Replace the parameter-set references on one or more applications

- [`setIndividual()`](https://esqlabs.github.io/esqlabsR/dev/reference/setIndividual.md)
  : Modify fields of an existing individual

- [`setOutputPath()`](https://esqlabs.github.io/esqlabsR/dev/reference/setOutputPath.md)
  : Change the literal path of one or more existing output paths

- [`setParameterValuesByPathWithCondition()`](https://esqlabs.github.io/esqlabsR/dev/reference/setParameterValuesByPathWithCondition.md)
  :

  Set the values of parameters in the simulation by path, if the
  `condition` is true.

- [`setPopulation()`](https://esqlabs.github.io/esqlabsR/dev/reference/setPopulation.md)
  : Modify fields of an existing population

- [`setScenario()`](https://esqlabs.github.io/esqlabsR/dev/reference/setScenario.md)
  : Modify fields of an existing scenario

- [`exportProjectToExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/exportProjectToExcel.md)
  : Export a Project to Excel files

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

- [`buildSimulations()`](https://esqlabs.github.io/esqlabsR/dev/reference/buildSimulations.md)
  : Build the simulations for a set of scenarios without running them

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
  : Load observed data declared in a Project

## Helpers for plotting

- [`col2hsv()`](https://esqlabs.github.io/esqlabsR/dev/reference/col2hsv.md)
  : Returns the HSV values for a given R color name

- [`esqlabsColors()`](https://esqlabs.github.io/esqlabsR/dev/reference/esqlabsColors.md)
  : esqLABS color palette

- [`createEsqlabsPlotConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createEsqlabsPlotConfiguration.md)
  :

  Create an instance of `DefaultPlotConfiguration` R6 class

- [`createEsqlabsPlotGridConfiguration()`](https://esqlabs.github.io/esqlabsR/dev/reference/createEsqlabsPlotGridConfiguration.md)
  :

  Create an instance of `PlotGridConfiguration` R6 class

- [`createDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md)
  [`createDataCombinedFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md)
  : Generate DataCombined objects from a Project

- [`createPlots()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlots.md)
  [`createPlotsFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlots.md)
  : Generate plots from a Project

## Input validation

- [`validateProject()`](https://esqlabs.github.io/esqlabsR/dev/reference/validateProject.md)
  : Validate a Project
- [`validationResult`](https://esqlabs.github.io/esqlabsR/dev/reference/validationResult.md)
  : validationResult
- [`validationSummary()`](https://esqlabs.github.io/esqlabsR/dev/reference/validationSummary.md)
  : validationSummary
- [`isAnyCriticalErrors()`](https://esqlabs.github.io/esqlabsR/dev/reference/isAnyCriticalErrors.md)
  : isAnyCriticalErrors
- [`isParametersEqual()`](https://esqlabs.github.io/esqlabsR/dev/reference/isParametersEqual.md)
  : Check if two parameters are equal with respect to certain
  properties.
- [`isProjectInitialized()`](https://esqlabs.github.io/esqlabsR/dev/reference/isProjectInitialized.md)
  : Check if a directory contains an esqlabsR project
- [`isTableFormulasEqual()`](https://esqlabs.github.io/esqlabsR/dev/reference/isTableFormulasEqual.md)
  : Check if two table formulas are equal.
- [`print(`*`<ValidationResults>`*`)`](https://esqlabs.github.io/esqlabsR/dev/reference/print.ValidationResults.md)
  : Print a project validation report
- [`format(`*`<ValidationResults>`*`)`](https://esqlabs.github.io/esqlabsR/dev/reference/format.ValidationResults.md)
  : Format a project validation report
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
- [`getObservedDataNames()`](https://esqlabs.github.io/esqlabsR/dev/reference/getObservedDataNames.md)
  : Get names of all observed data in a Project
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
  : Names of the settings stored in esqlabsEnv

## Deprecated

Legacy entry points retained for migration only. They emit a deprecation
warning. Use the new project-driven API above.

- [`Project`](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)
  [`ProjectConfiguration`](https://esqlabs.github.io/esqlabsR/dev/reference/Project.md)
  : Project
- [`createDataCombined()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md)
  [`createDataCombinedFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createDataCombined.md)
  : Generate DataCombined objects from a Project
- [`createPlots()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlots.md)
  [`createPlotsFromExcel()`](https://esqlabs.github.io/esqlabsR/dev/reference/createPlots.md)
  : Generate plots from a Project
