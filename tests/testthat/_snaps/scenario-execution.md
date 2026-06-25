# a relative modelFile with NULL modelFolder aborts with a clear message

    Code
      esqlabsR:::.runScenariosFromProject(project, scenarioNames = "TestScenario",
        validate = FALSE)
    Condition
      Error in `.prepareScenario()`:
      ! x Cannot resolve the model file for scenario "TestScenario". i modelFile "Aciclovir.pkml" is relative but the project has no modelFolder to resolve it against.

