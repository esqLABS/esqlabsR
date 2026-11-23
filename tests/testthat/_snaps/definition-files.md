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

# an observedData file whose id disagrees with its filename aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.assertObservedDataTreeRecordId()`:
      ! An observedData definition file has an id that disagrees with its filename.
      x The declaration's id is "elsewhere" but the file is "mismatch".json.
      i The filename stem is the declaration's id; rename the file, or the id it declares (or the file it derives one from), so they match. Check '<project>/definitions/observed-data/mismatch.json'.

# an observedData file whose id is not a string aborts

    Code
      loadProject(project$info$projectFilePath)
    Condition
      Error in `.assertObservedDataTreeRecordId()`:
      ! An observedData definition file has an unusable id.
      x id must be a single non-empty string (it names the declaration and its file).
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

# observedData declarations whose ids differ only in case cannot be saved

    Code
      .serializeObservedDataSet(entries)
    Condition
      Error in `.serializeObservedDataSet()`:
      x Two observed-data sources would be saved to the same definition file 'laskin 1982.pkml.json'.
      i Each source is saved to a file named after its id: the id it was given, the name of its <DataSet>, or else its file basename. Ids that differ only in case are the same file on Windows and macOS.
      i Give the sources distinct ids, or remove one with `removeObservedData()`. For a <DataSet>, remove its source by its name, rename the <DataSet>, and add it again with `addObservedData()`.

# a programmatic observedData id that escapes its directory aborts

    Code
      .serializeObservedDataSet(entries)
    Condition
      Error in `.serializeObservedDataSet()`:
      ! observedData id "../escape" is not a single safe filename segment.
      x It must not contain a path separator or be "." / "..", so it cannot escape the observed-data definition directory.
      i Give the declaration an id that is a single safe filename segment, or rename what it takes its id from: a file basename, or the name of a <DataSet>.

# saveProject aborts when a definition file cannot be renamed to its id's new spelling

    Code
      saveProject(project)
    Condition
      Warning in `file.rename()`:
      cannot rename file '<project>/definitions/observed-data/Laskin 1982.pkml.json' to '<project>/definitions/observed-data/laskin 1982.pkml.json', reason 'Permission denied'
      Error in `.writeDefinitionTree()`:
      ! Failed to rename 1 definition file in the 'definitions' folder to the new spelling of its id.
      x '<project>/definitions/observed-data/Laskin 1982.pkml.json'
      i Check the file permissions, then save again.

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

