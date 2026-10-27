# runScenarios rejects a simulationRunOptions that is not the class

    Code
      runScenarios(project, scenarios = "testscenario", simulationRunOptions = 4)
    Condition
      Error in `runScenarios()`:
      ! `simulationRunOptions` must be an <ospsuite::SimulationRunOptions>, not a number.
      i Build one with `ospsuite::SimulationRunOptions$new(numberOfCores = 4)`.
      i To change how the solver behaves, pass `solverSettings` instead, as `list(relTol = 1e-6)`.

---

    Code
      buildSimulations(project, scenarios = "testscenario", simulationRunOptions = list(
        relTol = 1e-06))
    Condition
      Error in `buildSimulations()`:
      ! `simulationRunOptions` must be an <ospsuite::SimulationRunOptions>, not a list.
      i Build one with `ospsuite::SimulationRunOptions$new(numberOfCores = 4)`.
      i To change how the solver behaves, pass `solverSettings` instead, as `list(relTol = 1e-6)`.

# buildSimulations reports an unsound solverSettings argument

    Code
      buildSimulations(project, scenarios = "testscenario", solverSettings = list(
        reltol = 1e-04, mxStep = 1.5))
    Condition
      Error in `buildSimulations()`:
      ! Invalid `solverSettings`:
      x solverSettings has unknown setting 'reltol'; the settings are absTol, checkForNegativeValues, h0, hMax, hMin, mxStep, relTol, useJacobian
      x solverSettings$mxStep must be a single whole number between 1 and 2147483647

# a parameter-set and an initial-condition path the model lacks are named

    Code
      runScenarios(project, scenarios = "testscenario", validate = FALSE)
    Condition
      Error in `runScenarios()`:
      ! Could not build scenario "testscenario".
      i Pass `stopIfFails = FALSE` to skip it and build the other scenarios.
      Caused by error in `initializeSimulation()`:
      ! The model of simulation "testscenario" has no parameter or molecule at these paths: "Organism|Liverr|Volume" and "Organism|VenousBlood|Plasma|Aciclovirr".
      i Pass `stopIfParameterNotFound = FALSE` to skip them.

# customParams values of another length stop with the field named

    Code
      buildSimulations(project, scenarios = "testscenario", customParams = list(
        paths = c("Organism|Liver|Volume", "Organism|Kidney|Volume",
          "Organism|Brain|Volume"), values = c(0.5, 0.6), units = "l"), validate = FALSE)
    Condition
      Error in `buildSimulations()`:
      ! `customParams$values` has length 2, but it must have length 1 or 3, one element per path.

# a user parameter path the model lacks still stops the build

    Code
      buildSimulations(project, scenarios = "testscenario", customParams = list(
        paths = "Organism|NoSuchContainer|NoSuchParameter", values = 1, units = ""))
    Condition
      Error in `buildSimulations()`:
      ! Could not build scenario "testscenario".
      Caused by error in `initializeSimulation()`:
      ! The model of simulation "testscenario" has no parameter or molecule at this path: "Organism|NoSuchContainer|NoSuchParameter".
      i Pass `stopIfParameterNotFound = FALSE` to skip it.

# .resolveScenarioPopulation warns when one id resolves two ways in a run

    Code
      invisible(.resolveScenarioPopulation(project$definitions$scenarios[[
        "populationscenariofromcsv"]], project, cache))
    Condition
      Warning:
      ! Population "testpopulation" resolves to more than one population in this run.
      i Scenario "populationscenariofromcsv" resolves it as "csv", another scenario in the same run resolves it differently.
      i Each scenario gets the population it asks for. Check readPopulationFromCSV on the scenarios sharing this population.

# a relative modelFile with NULL simulationsFolder aborts with a clear message

    Code
      .runScenariosFromProject(project, scenarioNames = "testscenario", validate = FALSE)
    Condition
      Error:
      ! Could not build scenario "testscenario".
      i Pass `stopIfFails = FALSE` to skip it and build the other scenarios.
      Caused by error in `.prepareScenario()`:
      x Cannot resolve the model file for scenario "testscenario".
      i modelFile "Aciclovir.pkml" is relative but the project has no simulationsFolder to resolve it against.

# a CSV-population scenario with NULL populationsFolder aborts with a clear message

    Code
      .runScenariosFromProject(project, scenarioNames = "populationscenariofromcsv",
        validate = FALSE)
    Condition
      Error:
      ! Could not build scenario "populationscenariofromcsv".
      i Pass `stopIfFails = FALSE` to skip it and build the other scenarios.
      Caused by error in `.resolveCsvPopulation()`:
      x Cannot resolve the population csv for scenario "populationscenariofromcsv".
      i populationId "testpopulation" is read from a csv but the project has no populationsFolder to resolve it against.

