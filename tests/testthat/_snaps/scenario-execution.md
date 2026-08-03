# a user parameter path the model lacks still stops the build

    Code
      buildSimulations(project, scenarios = "testscenario", customParams = list(
        paths = "Organism|NoSuchContainer|NoSuchParameter", values = 1, units = ""))
    Condition
      Error in `.validateEntitiesExist()`:
      ! `<caller>`: no entity exists for path "Organism|NoSuchContainer|NoSuchParameter" located under container <testscenario>!

# .resolveScenarioPopulation warns when one id resolves two ways in a run

    Code
      invisible(.resolveScenarioPopulation(project$definitions$scenarios[[
        "populationscenariofromcsv"]], project, cache))
    Condition
      Warning:
      ! Population "testpopulation" resolves to more than one population in this run. i Scenario "populationscenariofromcsv" resolves it as "csv", another scenario in the same run resolves it differently. i Each scenario gets the population it asks for. Check readPopulationFromCSV on the scenarios sharing this population.

# a relative modelFile with NULL simulationsFolder aborts with a clear message

    Code
      .runScenariosFromProject(project, scenarioNames = "testscenario", validate = FALSE)
    Condition
      Error in `.prepareScenario()`:
      ! x Cannot resolve the model file for scenario "testscenario". i modelFile "Aciclovir.pkml" is relative but the project has no simulationsFolder to resolve it against.

# a CSV-population scenario with NULL populationsFolder aborts with a clear message

    Code
      .runScenariosFromProject(project, scenarioNames = "populationscenariofromcsv",
        validate = FALSE)
    Condition
      Error in `.resolveCsvPopulation()`:
      ! x Cannot resolve the population csv for scenario "populationscenariofromcsv". i populationId "testpopulation" is read from a csv but the project has no populationsFolder to resolve it against.

