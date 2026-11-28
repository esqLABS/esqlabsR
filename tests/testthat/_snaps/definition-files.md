# addScenario canonicalizes its id to a safe, lowercase form

    Code
      addScenario(project, "My/Scenario", modelFile = "Aciclovir.pkml")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "My/Scenario" -> "my_scenario"

# a scenario id that disagrees with its list key aborts saveProject()

    Code
      saveProject(project)
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario id "testscenario" does not match the key "renamed" it is stored under.
      i Store a scenario under a key equal to its id.

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
      ! Scenario "ss" has steadyState=TRUE but steadyStateTimeUnit is NULL.
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

# a scenario file missing its id aborts naming the file

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind scenario has no usable id.
      x id must be a single non-empty string (it names the definition and its file).
      i Check '<project>/definitions/scenarios/populationscenario.json'.

# a scenario file whose id disagrees with its filename aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind scenario has a stored id that disagrees with its filename.
      x id is "differentname" but the file is "populationscenario".json.
      i The filename stem is the definition's id; rename the file or the id so they match. Check '<project>/definitions/scenarios/populationscenario.json'.

# an observedData file without an id aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind observedData has no usable id.
      x id must be a single non-empty string (it names the definition and its file).
      i Check '<project>/definitions/observed-data/cohort_b.json'.

# an observedData file whose id disagrees with its filename aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind observedData has a stored id that disagrees with its filename.
      x id is "elsewhere" but the file is "mismatch".json.
      i The filename stem is the definition's id; rename the file or the id so they match. Check '<project>/definitions/observed-data/mismatch.json'.

# an observedData file whose id is not a string aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind observedData has no usable id.
      x id must be a single non-empty string (it names the definition and its file).
      i Check '<project>/definitions/observed-data/obs.pkml.json'.

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

# observedData declarations that share an id cannot be saved

    Code
      .serializeObservedDataSet(entries)
    Condition
      Error in `.serializeObservedDataSet()`:
      x Two observed-data sources would be saved to the same definition file 'laskin_1982.json'.
      i Each source is saved to a file named after its id. Give the sources distinct ids, or remove one with `removeObservedData()`.

# an observedData id that is not canonical cannot be saved

    Code
      .serializeObservedDataSet(list(Laskin = list(id = "Laskin", type = "pkml",
        file = "laskin.pkml")))
    Condition
      Error in `.validateDefinitionTreeKey()`:
      ! observedData id "Laskin" is not a canonical definition-file id.
      i Use the add/set API, which canonicalizes it to "laskin", or store the definition under the key "laskin".
    Code
      .serializeObservedDataSet(list(`../escape` = list(id = "../escape", type = "programmatic")))
    Condition
      Error in `.validateDefinitionTreeKey()`:
      ! observedData id "../escape" is not a canonical definition-file id.
      i Use the add/set API, which canonicalizes it to "_escape", or store the definition under the key "_escape".

# a full-tree write aborts when a stale file cannot be removed

    Code
      .writeDefinitionTree(project$definitions$scenarios, "scenarios", project,
      project$info$projectDirPath)
    Condition
      Warning in `file.remove()`:
      cannot remove file '<project>/definitions/scenarios/orphandefinition.json', reason 'Permission denied'
      Error in `.writeDefinitionTree()`:
      ! Failed to delete 1 outdated definition file from the 'definitions' folder.
      x '<project>/definitions/scenarios/orphandefinition.json'
      i A file that cannot be deleted comes back as a definition the next time you `loadProject()`; check the file permissions and delete it manually.

