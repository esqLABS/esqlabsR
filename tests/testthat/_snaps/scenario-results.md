# loadScenarioResults reports a results folder that does not exist

    Code
      loadScenarioResults("testscenario", resultsFolder = "no-such-folder")
    Condition
      Error in `loadScenarioResults()`:
      x Cannot load scenario results: the folder 'no-such-folder' does not exist.
      i Pass the folder that `saveScenarioResults()` returned.

# loadScenarioResults names the scenario and the files missing from the folder

    Code
      loadScenarioResults("testscenario", resultsFolder = "results")
    Condition
      Error in `loadScenarioResults()`:
      x Cannot load results for scenario "testscenario": 'testscenario.pkml' and 'testscenario.csv' are not in the results folder.
      i Looked in 'results'.
      i Check the scenario name and the folder. `saveScenarioResults()` writes the '.pkml' file only when `saveSimulationsToPKML` is `TRUE`.

# loadScenarioResults checks every scenario before loading any

    Code
      loadScenarioResults("first", resultsFolder = "results")
    Condition
      Error in `loadScenarioResults()`:
      x Cannot load results for scenario "first": 'first.pkml' is not in the results folder.
      i Looked in 'results'.
      i Check the scenario name and the folder. `saveScenarioResults()` writes the '.pkml' file only when `saveSimulationsToPKML` is `TRUE`.

---

    Code
      loadScenarioResults(c("first", "Group/B"), resultsFolder = "results")
    Condition
      Error in `loadScenarioResults()`:
      x Cannot load results for scenario "Group/B": 'Group_B.pkml' and 'Group_B.csv' are not in the results folder.
      i Looked in 'results'.
      i Check the scenario name and the folder. `saveScenarioResults()` writes the '.pkml' file only when `saveSimulationsToPKML` is `TRUE`.

# saveScenarioResults reports the real error rather than a path warning

    Code
      invisible(saveScenarioResults(broken, project, outputFolder = resultsFolder))
    Condition
      Warning:
      x Failed to save results for scenario "testscenario".
      i `<caller>`: argument "simulation" is of type <character>, but expected <Simulation>!

