# saveScenarioResults reports the real error rather than a path warning

    Code
      invisible(saveScenarioResults(broken, project, outputFolder = resultsFolder))
    Condition
      Warning:
      x Failed to save results for scenario "TestScenario".
      i `<caller>`: argument "simulation" is of type <character>, but expected <Simulation>!

