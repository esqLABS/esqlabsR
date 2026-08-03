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

# more than one ontogeny cannot be read

    Code
      .readOntogeniesFromList(c("CYP3A4:CYP3A4", "CYP2D6:CYP2C8"))
    Condition
      Error in `is.null(ontogenyString) || is.na(ontogenyString)`:
      ! 'length = 2' in coercion to 'logical(1)'

