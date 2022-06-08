# esqlabsR 4.0.0

MAJOR CHANGES

- Three new functions to create configuration objects needed for data visualization workflows:
  
  * `createEsqlabsPlotConfiguration()`
  * `createEsqlabsPlotGridConfiguration()`
  * `createEsqlabsExportConfiguration()`

BREAKING CHANGES

- The package gains new dependencies:

  * [`{ospsuite.parameteridentification}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/) 
  * [`{tlf}`](https://www.open-systems-pharmacology.org/TLF-Library/).

- Function `getSteadyState()` has been moved to package `{ospsuite.parameteridentification}`.

- Function `loadObservedData()` requires a `ProjectConfiguration` instead of a 
`ScenarioConfiguration`.

- `DataMapping`, `DataMappingConfiguration`, `XYData`, `DataConfiguration` and 
associated functions for creating standard figures are moved to `esqlabsRLegacy` 
[package](https://esqlabs.github.io/esqlabsRLegacy/).

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

# esqlabsR 2.0.0

- Maintenance and bug fixes.

# esqlabsR 1.0.0

- Initial release of the package.
