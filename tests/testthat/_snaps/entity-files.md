# addScenario canonicalizes its id to a safe, lowercase form

    Code
      addScenario(project, "My/Scenario", modelFile = "Aciclovir.pkml")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "My/Scenario" -> "my_scenario"

# a scenarioName that disagrees with its list key aborts the write

    Code
      project$.setSection("scenarios", scenarios)
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario scenarioName "testscenario" does not match the key "renamed" it is stored under.
      i Store a scenario under a key equal to its scenarioName (or leave scenarioName unset).

# a write-back under a non-canonical key aborts the write

    Code
      project$.setSection("scenarios", scenarios)
    Condition
      Error in `.validateScenarioStructure()`:
      ! Scenario id "Renamed" is not a canonical entity-file id.
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

# saveSnapshot refuses to overwrite the project's own container

    Code
      saveSnapshot(project)
    Condition
      Error in `saveSnapshot()`:
      ! A snapshot is a derived artifact and must be written to a location other than the project's own jsonPath.
      i Pass a `path` to a different file. The authoritative 'definitions/' tree and 'Project.json' container are already write-through, so there is nothing to save in place.

---

    Code
      saveSnapshot(project, project$jsonPath)
    Condition
      Error in `saveSnapshot()`:
      ! A snapshot is a derived artifact and must be written to a location other than the project's own jsonPath.
      i Pass a `path` to a different file. The authoritative 'definitions/' tree and 'Project.json' container are already write-through, so there is nothing to save in place.

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
      ! scenario "myscenario" already exists

# a scenario file missing its name aborts naming the file

    Code
      loadProject(project$jsonPath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! An entity file for kind scenario has no usable name.
      x name must be a single non-empty string (it names the entity and its file).
      i Check '<project>/definitions/scenarios/populationscenario.json'.

# a scenario file whose name disagrees with its filename aborts

    Code
      loadProject(project$jsonPath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! An entity file for kind scenario has a stored name that disagrees with its filename.
      x name is "differentname" but the file is "populationscenario".json.
      i The filename stem is the entity's id; rename the file or the name so they match. Check '<project>/definitions/scenarios/populationscenario.json'.

# a non-scalar scalar field fails load naming the scenario and field

    Code
      loadProject(project$jsonPath)
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
      ! An entity file for kind initialConditionSet has a stored id that disagrees with its filename.
      x id is "myset" but the file is "otherset".json.
      i The filename stem is the entity's id; rename the file or the id so they match. Check 'somewhere/otherset.json'.

# a programmatic observedData name that escapes its directory aborts

    Code
      esqlabsR:::.serializeObservedDataSet(entries)
    Condition
      Error in `.validateObservedDataId()`:
      ! observedData id "../escape" is not a single safe filename segment.
      x It must not contain a path separator or be "." / "..", so it cannot escape the observed-data entity directory.
      i Rename the source (its file basename or programmatic name) to a single safe filename segment.

