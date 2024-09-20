# esqlabsR (development version)

## Breaking changes

  - Function `sensitivityCalculation()` with renamed parameter `pkDataFilePath` 
    to `saOutputFilePath` and updated output files to include PK parameter units (\#696).

## Major Changes

## Minor improvements and bug fixes

  - Handle simulation failures in `sensitivityCalculation` (\#694).

  - Function `sensitivityCalculation()` supports new parameter `customOutputFunctions`, 
    allowing user-defined functions for PK parameter calculation (\#685).

  - Function `sensitivitySpiderPlot` now supports `xAxisType` and `yAxisType` options 
    for switching between percent and absolute values (\#695).
    
  - Function `sensitivityCalculation()` will throw more informative messages when
    `outputPaths` or `parameterPaths` is not a character vector or NULL (\#712).

# esqlabsR 5.2.0

## Breaking changes

  - `{esqlabsR}` now requires `{ospsuite}` version 12.1.0 or higher to benefit from
    embedded PK-Sim core files.

  - Function `exportSteadyStateToXLS` has been removed in favor of
    `ospsuite::exportSteadyStateToXLS` (\#598)

  - Function `sensitivitySpiderPlot` overhauled with new parameters. `yAxisType`
    to toggle between `percent`and `absolute` values. `xAxisScale` and
    `yAxisScale` to set axis scales to `log` or `lin`. `yAxisFacetScales` to
    choose between `free` and `fixed` scales for y-axis. Further plot
    customization options through `defaultPlotConfiguration` (\#632).
  
  - Function `sensitivityTimeProfiles` with new signature and enhanced visuals:
    - `xAxisScale` and `yAxisScale` to set axis scales to `log` or `lin` (\#669).
    - `xUnits` and `yUnits` to change units in the plot (\#688).
    - Enhanced plot customization options through `defaultPlotConfiguration` as
      described in the documentation (\#669).
    - Support for adding `observedData` (single or multiple `DataSet` objects) 
      to the plots (\#674).
      
  - Function `writeExcel()` is no longer exported. (\#672)

## Major Changes

  - New `sensitivityTornadoPlot` function for generating tornado plots that
    evaluate the impact of parameter changes on sensitivity analysis outcomes.
    It utilizes `parameterFactor` to depict the scaling effects of parameter
    modifications. Provides plot customization options through
    `defaultPlotConfiguration` (\#652).

  - New function `writeParameterStructureToXLS` to write a list of parameter
    paths, values, and units (e.g., imported using the `readParametersFromXLS()`
    function) to an Excel file. In contrast to `exportParametersToXLS()`, which
    writes an excel file for a list of `Parameter` objects, this function
    expects the parameter structure as used throughout the package.

## Minor improvements and bug fixes

  - Function `createScenarios` gets a new argument `stopIfParameterNotFound`. If
    `TRUE` (default), a scenario will not be created and an error is thrown if
    any user-defined parameter (e.g., provided in Excel files) is not found in
    the simulation. If `FALSE`, non-existing parameters are ignored. (\#559)

  - Constructor of a `Scenario` class gets a new argument
    `stopIfParameterNotFound`.

  - More explicit error is shown if x/yOffsetsUnit is not set when x/yOffset is
    defined.
    
  - Plotting functions have improved dynamic axis breaks, limits calculation, and 
    labeling (\#691).

# esqlabsR 5.1.3

## Minor improvements and bug fixes

  - Continuous Integration/Continuous Deployment pipeline improvements (\#590,
    \#592, \#601)
  - Several bug fixes (\#581, \#585, \#594, \#600)

# esqlabsR 5.1.2

## Minor improvements and bug fixes

  - Fixes a bug where scenario results could not be saved or loaded when
    scenario name contains a slash or a backslash (\#548)
  - Add support for `nsd` argument from `ospsuite::plotPopulationTimeProfile()`
    aggregation methods. A new column `nsd` was added to the `PlotConfiguration`
    sheet in the example `Plot.xlsx` (\#544).
  - Documentation includes PK-Sim installation instructions (\#537).
  - Better error message if some ids are not unique in the excel configuration
    files (\#568)
  - Handles better empty rows in the excel configuration files (\#569)
  - Cleaner NEWS file (\#527).

# esqlabsR 5.1.1

  - Update required dependencies versions

# esqlabsR 5.1.0

## Breaking Changes

  - When importing observed data using the default importer configuration, data
    set naming is grouped by `StudyId` at the first place.
      - Before: `{Molecule}_{Study Id}_{Subject
        Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}`
      - After: `{Study Id}_{Molecule}_{Subject
        Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}`
        
    This will result in different data set names, and plots specifying the
    data sets by the old naming will fail. For compatibility, use custom
    importer configuration with the old naming:
      
    ``` 
      importerConfiguration <- ospsuite::loadDataImporterConfiguration(
        configurationFilePath = projectConfiguration$dataImporterConfigurationFile
        )
        importerConfiguration$namingPattern <- "{Molecule}_{Study Id}_{Subject Id}_{Species}_{Organ}_{Compartment}_{Dose}_{Route}_{Group Id}"
    ```

## Major changes

  - `loadObservedData()` gets a new argument `importerConfiguration`. The user
    can now provide a custom importer configuration for loading the data.
    
  - Plots.xlsx, sheet 'plotConfiguration', now uses `xValuesLimits` and
    `yValuesLimits` to set axis limits of the plots by default. This approach
    filters data outside of the limits. See
    <https://ggplot2.tidyverse.org/reference/coord_cartesian.html#ref-examples>
    for more details. The user can still use `xAxisLimits` and `yAxisLimits`.

  - Sheet 'plotConfiguration' in the Excel file 'Plots' gets additional column
    'aggregation'. The value is passed to the function
    `plotPopulationTimeProfile()`. Supported values are listed in
    `ospsuite::DataAggregationMethods`.
      - `arithmetic`: population results are plotted as arithmetic mean +-
        arithmetic standard deviation
      - `geometric`: population results are plotted as geometric mean +-
        geometric standard deviation
      - `quantiles` (default): population results are plotted as quantiles
        defined in the column `quantiles`.

  - Protein ontogenies can be defined for populations and individuals. To
    specify ontogenies for proteins in the simulation, list the proteins you
    want to define ontogenies for in the column 'Protein' of files
    'PopulationParameters.xlsx' or 'Individuals.xlsx', separated by a ','.
    Specify the ontogenies available in PK-Sim (see article
    <https://www.open-systems-pharmacology.org/OSPSuite-R/articles/create-individual.html#adding-enzyme-ontogenies>)
    in the column 'Ontogenies'. The number of entries in the both columns must
    be equal.
    
  - Excel file 'PopulationParameters.xlsx' gets additional columns 'Protein' and
    'Ontogeny'.
    
  - Excel file 'Individuals.xlsx' gets additional columns 'Protein' and
    'Ontogeny'.

## Minor improvements and bug fixes

  - When a scenario fails, `runScenarios()` does not crash any more, but a
    warning is shown with the name of the failed scenario. The returned
    `outputValues` is `NULL`.
    
  - Throw a warning instead of an error if a path specified in
    `ProjectConfiguration` does not exist. `$outputFolder` existence is not
    checked anymore.
    
  - `stringToNum()` does not show a warning `NAs introduced by coercion` when a
    value cannot be converted to a numeric any more. For such values, `NA` is
    silently returned.
    
  - exportParametersToXLS - ignore parameters with NaN (\#480)
    
  - Show a meaningful error when no time unit is specified for a scenario
    (\#483)

  - New vignette/article about figure creation.
  
  - Plots are using new color palette.
  
  - Some modifications to plot configuration files for better plots. (\#456)

# esqlabsR 5.0.0

## Breaking Changes

  - Field `setTestParameters` removed from `ScenarioConfiguration`
  
  - Function `initializeScenario()` has been removed.
  
  - Definition of simulation time in the `Scenarios.xlsx` file changed. The new
    expected format is a triplet of values \<StartTime, EndTime, Resolution\>,
    where `Resolution` is the number of simulated points per time unit defined
    in the column `TimeUnit`.
    
  - Field `poinstPerMinute` of `ScenarioConfiguration` has been removed.
  - Function `compareSimulationParameters()` has been removed and replaced by
    `compareSimulations()`
    
  - `Scenarios` excel file gets additional columns `SteadyStateTime`,
    `SteadyStateTimeUnit`, `PopulationId`, `OutputPathsIds`.
    
  - `readScenarioConfigurationFromExcel()` has a new signature and requires a
    list of `scenarioNames` and a `ProjectConfiguration`. The output is a named
    list of `ScenarioConfiguration` objects.
    
  - Output paths are not set from global variable `OutputPaths` any more but
    from the respective field of `ScenarioConfgiruation`
    
  - `ProjectConfiguration` does not have field `$outputDevice` any more.
  
  - `ScenarioConfiguration` does not store `SimulationRunOptions` any more.
    Simulation run options must be passed to the `runScenarios()` function.
    Different run options cannot be used within one scenarios run.
    
  - Enum `GraphicsDevices` has been removed.
  
  - Function `initializeSimulation()` does not have arguments
    `simulateSteadyState`, `steadyStateTime` and `simulationRunOptions` any
    more.

## Major changes

  - New class `Scenario` that represents a scenario created from a
    `ScenarioConfiguration`.
    
  - `ScenarioConfiguration` gets a new field `outputPaths` which is a list of
    output paths for which the results will be calculated. If `NULL` (default),
    outputs as defined in the simulation are used.
    
  - Paths of model outputs are defined in the excel file `Scenarios.xlsx`. In
    the sheet `OutputPaths`, create an entry for each output. The column
    `OutputPath` is the full path to the output, while `OutputPathId` is an
    identifier that conveniently allows to select the correct output.  
    In the `Scenarios` sheet, enter the IDs of all paths the outputs should be
    generated for, separated by a `,`, e.g. `Aciclovir_PVB, Aciclovir_fat_cell`.
    If no outputs are specified, the outputs as defined in the simulation
    `.pkml` file will be produced.
    
  - `ScenarioConfiguration` gets a new field `populationId`, specifying the id
    of the population as defined in the `PopulationParameters.xlsx` file, sheet
    `Demographics`. If the field is `NULL`, the scenario is simulated as an
    individual simulation, otherwise a population simulation is performed.
    
  - `ScenarioConfiguration` gets a new field `readPopulationFromCSV`. If `FALSE`
    (default), a new population is created from defined population demographics.
    If `TRUE`, a simulation will be imported from a csv sheet located in the
    folder `Parameters/Populations` and named as the `PopulationId`.
    
  - `runScenarios()` supports scenario configurations for population
    simulations.
  
  - Target folder for saving `*.pkml` simulations when
    `runScenarios(scenarioConfigurations, saveSimulationsToPKML = TRUE)` changed
    from `Models/Simulations/<DateSuffix>` to
    `Results/SimulationResults/<DateSuffix>`.
    
  - `sensitivityCalculation()` - fixed bug in wrong calculation of sensitivity
    values. Please be aware that the results produced by earlier versions are
    wrong.
    
  - The workflow for running scenarios changed to:
      - Create a `ProjectConfiguration` with
        `createDefaultProjectConfiguration()`
      - Create `ScenarioConfigurations`, e.g. with
        readScenarioConfigurationFromExcel(scenarioNames,
        projectConfiguration)\`
      - Run scenarios with `runScenarios(scenarioConfigurations)` Alternatively:
      - Create a `ProjectConfiguration` with
        `createDefaultProjectConfiguration()`
      - Create `ScenarioConfigurations`, e.g. with
      
`readScenarioConfigurationFromExcel(scenarioNames, projectConfiguration)`

  - `ProjectConfiguration` gets a new field `plotsFile`. It is the name of the
    excel file with plot definitions and must be located in the `paramsFolder`.
    
  - When defining an individual of other species than human in
    `ScenarioConfiguration` and applying it to a human model, missing
    species-specific parameters are applied and the scaling works properly.
    Supported scalings are: Human to rat, human to monkey, human to rabbit.
    
  - `initializeSimulation()` does not perform steady-state run any more. This is
    done as part of the `runScenarios()` function.
    
  - New function `loadObservedDataFromPKML()` to load data from `*.pkml` located
    in the "PKML" sub-folder of the "Data" folder.
    
  - New function `createScenarios()` to create `Scenario` objects from
    `ScenarioConfiguration` objects.
    
  - Plots can be created by calling the new function `createPlotsFromExcel()`.
    It requires as input parameters `simulatedScenarios` (a list of simulated
    scenarios as returned by `runScenarios()`), `observedData` (a list of
    `DataSet` objects) and a `ProjectConfiguration` object
    `projectConfiguration`.
    
  - New function `createDataCombinedFromExcel()` creates `DataCombined` objects
    as defined in the `Plots.xlsx` file.
    
  - New function `saveScenarioResults()` to save results produced by the
    `runScenarios()` function to csv files and corresponding simulations to pkml
    files.
    
  - New function `loadScenarioResults()` to load results from csv files into a
    structure as produced by the `runScenarios()` function.
    
  - New function `compareSimulation()` to compare two simulations for
    differences.
    
  - `runScenarios()` also returns a `Population` object for population
    simulations.
    
  - `runScenarios()` gets a new argument `savePopulationToCSV`, with default
    value `FALSE`.
    
  - `esqlabsR` extends the `tlf::ExportConfiguration` class to dynamically
    calculate the height of the exported figure from the number of rows and the
    new parameter `heightPerRow`.

## Minor changes and bug fixes

  - Function `stringToNum()` gets additional arguments `lloqMode` and `uloqMode`
    that determine how entries of type "\<number" and "\>number" will be
    treated.
    
  - `readScenarioConfigurationFromExcel()` will read all scenarios defined in
    the `Scenarios.xlsx` file if no scenario names are specified (argument
    `scenarioNames = NULL`).
    
  - Function `setApplications()` is deprecated.
  
  - Dark gray frame around legends by default.


# esqlabsR 4.0.0

## Breaking changes

  - The package requires R version \>=4.1.

  - The package gains new dependencies:

    - [`{ospsuite.parameteridentification}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/)
    - [`{tlf}`](https://www.open-systems-pharmacology.org/TLF-Library/).

  - Function `getSteadyState()` has been moved to package
    `{ospsuite.parameteridentification}`.

  - Function `loadObservedData()` requires a `ProjectConfiguration` instead of a
    `ScenarioConfiguration`.

  - `DataMapping`, `DataMappingConfiguration`, `XYData`, `DataConfiguration` and
    associated functions for creating standard figures are moved to
    `esqlabsRLegacy` [package](https://esqlabs.github.io/esqlabsRLegacy/).

## Major changes

  - Three new functions to create configuration objects needed for data
    visualization workflows:
      - `createEsqlabsPlotConfiguration()`
      - `createEsqlabsPlotGridConfiguration()`
      - `createEsqlabsExportConfiguration()`

  - New function `getAllApplicationParameters()` that returns all parameters of
    applications in a simulation
    
  - New function `exportParametersToXLS()` to write parameter information into
    an excel file that can be loaded in MoBi or R using the
    `readParametersFromXLS()` function.
    
  - New function `writeExcel()` that is a wrapper for creating a directory (if
    not present) and writing to excel file using `writexl::write_xlsx()`.



# esqlabsR 3.0.0

## Major changes

  - To carry out and visualize sensitivity analysis:
      - `sensitivityCalculation()`
      - `sensitivitySpiderPlot()`
      - `sensitivityTimeProfiles()`

  - Classes and functions for standard esqLABS simulation workflow:
      - `ProjectConfiguration`
      - `ScenarioConfiguration`
      - `createDefaultProjectConfiguration()`
      - `readScenarioConfigurationFromExcel()`
      - `setApplications()`
      - `initializeScenario()`

  - Maintenance and bug fixes.

  - The package gains a new dependency:
    [`{ospsuite.utils}`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/).


# esqlabsR 2.0.0

  - Maintenance and bug fixes.

# esqlabsR 1.0.0

  - Initial release of the package.
