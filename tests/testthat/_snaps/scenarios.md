# addScenario aborts when a referenced individual is unknown

    Code
      addScenario(project, id = "bad", modelFile = "Aciclovir.pkml", individual = "Ghost")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `addScenario()`:
      ! Cannot add scenario "bad":
      x individual 'ghost' not found in individuals

# addScenario suggests the closest existing id for a dangling reference

    Code
      addScenario(project, id = "bad", modelFile = "Aciclovir.pkml", individual = "indiv2")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "bad":
      x individual 'indiv2' not found in individuals (did you mean 'indiv1'?)

---

    Code
      addScenario(project, id = "bad", modelFile = "Aciclovir.pkml", outputPaths = "aciclovir_pv")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "bad":
      x outputPaths not found in outputPaths: aciclovir_pv (did you mean 'aciclovir_pvb'?)

# addScenario leaves the reference error bare when no id is close

    Code
      addScenario(project, id = "bad", modelFile = "Aciclovir.pkml", outputPaths = "somethingentirelyunrelated")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "bad":
      x outputPaths not found in outputPaths: somethingentirelyunrelated

# addScenario aborts eagerly on a dangling initialConditions ref

    Code
      addScenario(project, id = "bad", modelFile = "Aciclovir.pkml",
        initialConditions = "ghostset")
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "bad":
      x initialConditions not found in project$definitions$initialConditions: ghostset

# addScenario names both simulation-time forms on a bad value

    Code
      addScenario(project, id = "bad", modelFile = "Aciclovir.pkml", simulationTime = c(
        0, 42))
    Condition
      Error in `addScenario()`:
      ! `simulationTime` must be a length-3 numeric vector `c(start, end, resolution)`, or the same grid as a string "0, 42, 48" (several intervals separated by ";"). To give a different grid per id, pass a list with one element per id.

# setScenario aborts eagerly on a dangling initialConditions ref

    Code
      setScenario(project, "sc", initialConditions = "ghostset")
    Condition
      Error in `setScenario()`:
      ! Cannot modify scenario "sc":
      x initialConditions not found in project$definitions$initialConditions: ghostset

# addScenario rejects NA-valued FK args

    Code
      addScenario(project, id = "S", modelFile = "Aciclovir.pkml", individual = NA_character_)
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "S" -> "s"
      Error in `addScenario()`:
      ! Cannot add scenario "s":
      x individual must be a non-empty string or NULL

---

    Code
      addScenario(project, id = "S", modelFile = "Aciclovir.pkml", outputPaths = c(
        "Output1", NA_character_))
    Condition
      Warning:
      Canonicalized 2 ids to a safe form:
      * "S" -> "s"
      * "Output1" -> "output1"
      Error in `addScenario()`:
      ! Cannot add scenario "s":
      x outputPaths must be a non-empty character vector with no NA or empty entries

# addScenario keeps rejecting a reference list holding a non-string

    Code
      addScenario(project, id = "badlist", modelFile = "Aciclovir.pkml", outputPaths = list(
        "aciclovir_pvb", 1))
    Condition
      Error in `addScenario()`:
      ! Cannot add scenario "badlist":
      x outputPaths must be a non-empty character vector with no NA or empty entries

# addScenario aborts on an existing id, replaces it with overwrite

    Code
      addScenario(project, id = existing, modelFile = "Aciclovir.pkml")
    Condition
      Error in `addScenario()`:
      ! scenario "populationscenario" already exists.
      i Pass `overwrite = TRUE` to replace it.

# setScenario aborts on a non-existent scenario, no file written

    Code
      setScenario(project, "Ghost", simulationTimeUnit = "min")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setScenario()`:
      ! Cannot modify scenario "ghost": it does not exist.
      i Use `addScenario()` to create it first.

# setScenario rejects an unknown foreign key like addScenario

    Code
      setScenario(project, "testscenario", individual = "Ghost")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `setScenario()`:
      ! Cannot modify scenario "testscenario":
      x individual 'ghost' not found in individuals

# renameScenario errors clearly on a non-existent id

    Code
      renameScenario(project, "Ghost", "renamed")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `renameScenario()`:
      ! Cannot rename scenario "ghost": it does not exist.
      i Available scenarios: "populationscenario", "populationscenariofromcsv", "testscenario", and "testscenario_steadystate"

# renameScenario errors when the target id already exists

    Code
      renameScenario(project, "testscenario", "populationscenario")
    Condition
      Error in `renameScenario()`:
      ! Cannot use "populationscenario": a scenario with that id already exists.

# renameScenario canonicalizes newId, warning and landing on the canonical form

    Code
      renameScenario(project, "testscenario", "New Name")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "New Name" -> "new_name"

# renameScenario warns when a dataCombined still references it

    Code
      renameScenario(project, "testscenario", "renamed")
    Condition
      Warning:
      Removed scenario "testscenario" is still referenced by 1 dataCombined definition:
      * dc_ref
      i These now have a dangling reference. Update or remove them.

# duplicateScenario errors on a non-existent source id

    Code
      duplicateScenario(project, "Ghost", "copy")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `duplicateScenario()`:
      ! Cannot duplicate scenario "ghost": it does not exist.
      i Available scenarios: "populationscenario", "populationscenariofromcsv", "testscenario", and "testscenario_steadystate"

# duplicateScenario errors when the target id already exists

    Code
      duplicateScenario(project, "testscenario", "populationscenario")
    Condition
      Error in `duplicateScenario()`:
      ! Cannot use "populationscenario": a scenario with that id already exists.

# addScenario aborts on a duplicate id in the batch

    Code
      addScenario(project, c("s1", "s1"), modelFile = "Aciclovir.pkml", individual = "indiv1",
      outputPaths = "aciclovir_pvb")
    Condition
      Error in `addScenario()`:
      ! duplicate scenario id in the batch: "s1"

# removeScenario warns when a dataCombined still references it

    Code
      removeScenario(project, "testscenario")
    Condition
      Warning:
      Removed scenario "testscenario" is still referenced by 1 dataCombined definition:
      * dc_ref
      i These now have a dangling reference. Update or remove them.

# addScenario aborts on a mismatched scalar field length

    Code
      addScenario(project, c("s1", "s2", "s3"), modelFile = c("A.pkml", "B.pkml"))
    Condition
      Error in `addScenario()`:
      ! `modelFile` must be length 1 or length 3 (the number of ids).
      x It is length 2.

# print.Scenario renders the configured fields

    Code
      print(project$definitions$scenarios[["testscenario"]])
    Output
      <Scenario>
        * Name: testscenario
        * Model: Aciclovir.pkml
        * Type: Individual
        * Individual: indiv1
        * Population: <empty string>
        * Protocol: aciclovir_iv_250mg
        * Parameter Sets: global
        * Initial Conditions: testinitialset
        * Output Paths: 1
        * Steady State: FALSE

