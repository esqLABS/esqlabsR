# addScenario canonicalizes its id to a safe, lowercase form

    Code
      addScenario(project, "My/Scenario", modelFile = "Aciclovir.pkml")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "My/Scenario" -> "my_scenario"

# a scenarioName that disagrees with its list key aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario scenarioName "testscenario" does not match the key "renamed" it is stored under.
      i Store a scenario under a key equal to its scenarioName (or leave scenarioName unset).

# a write-back under a non-canonical key aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario id "Renamed" is not a canonical definition-file id.
      i Use `addScenario(project, "Renamed", ...)`, which canonicalizes it to "renamed", or store the scenario under the key "renamed".

# a scenario id with path separators is canonicalized, not rejected

    Code
      addScenario(project, "../escape", modelFile = "Aciclovir.pkml")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "../escape" -> "_escape"

---

    Code
      addScenario(project, "sub/evil", modelFile = "Aciclovir.pkml")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "sub/evil" -> "sub_evil"

# structural validation rejects a serializer-hostile scenario

    Code
      .validateScenarioStructure(sc, "ss")
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario "ss" has simulateSteadyState=TRUE but steadyStateTimeUnit is NULL.
      i Set steadyStateTimeUnit (e.g. "min") so the steady-state time can round-trip.

---

    Code
      .validateScenarioStructure(bad, "op")
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario "op" has outputPaths without ids.
      i Expected a named character vector: id-as-name, literal-path-as-value.

# an id differing only in case canonicalizes to an existing id

    Code
      addScenario(project, "MyScenario", modelFile = "Aciclovir.pkml")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "MyScenario" -> "myscenario"

---

    Code
      addScenario(project, "myscenario", modelFile = "Aciclovir.pkml")
    Condition
      Error in `addScenario()`:
      ! scenario "myscenario" already exists.
      i Pass `overwrite = TRUE` to replace it.

# a scenario file missing its name aborts naming the file

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind scenario has no usable name.
      x name must be a single non-empty string (it names the definition and its file).
      i Check '<project>/definitions/scenarios/populationscenario.json'.

# a scenario file whose name disagrees with its filename aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind scenario has a stored name that disagrees with its filename.
      x name is "differentname" but the file is "populationscenario".json.
      i The filename stem is the definition's id; rename the file or the name so they match. Check '<project>/definitions/scenarios/populationscenario.json'.

# a non-scalar scalar field fails load naming the scenario and field

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.assertScalarScenarioField()`:
      ! Scenario "populationscenario" has an invalid population.
      i Expected a single string or `null`; check 'definitions/scenarios/populationscenario.json'.
      i A hand-edit that turned `"population": null` into an empty object `{}` is the usual cause.

# a tree-loaded initialConditions id must match its filename stem

    Code
      spec$parse(list(rec), NULL)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind initialConditionSet has a stored id that disagrees with its filename.
      x id is "myset" but the file is "otherset".json.
      i The filename stem is the definition's id; rename the file or the id so they match. Check 'somewhere/otherset.json'.

# a programmatic observedData name that escapes its directory aborts

    Code
      esqlabsR:::.serializeObservedDataSet(entries)
    Condition
      Error in `.validateObservedDataId()`:
      ! observedData id "../escape" is not a single safe filename segment.
      x It must not contain a path separator or be "." / "..", so it cannot escape the observed-data definition directory.
      i Give the declaration an id that is a single safe filename segment, or rename what it derives one from: a file basename, or a programmatic name.

# a full-tree write aborts when a stale file cannot be removed

    Code
      esqlabsR:::.writeDefinitionTree(project$definitions$scenarios, "scenarios",
      project, project$info$projectDirPath)
    Condition
      Warning in `file.remove()`:
      cannot remove file '<project>/definitions/scenarios/orphandefinition.json', reason 'Permission denied'
      Error in `esqlabsR:::.writeDefinitionTree()`:
      ! Failed to delete 1 outdated definition file from the 'definitions' folder. x '<project>/definitions/scenarios/orphandefinition.json' i A file that cannot be deleted comes back as a definition the next time you `loadProject()`; check the file permissions and delete it manually.

