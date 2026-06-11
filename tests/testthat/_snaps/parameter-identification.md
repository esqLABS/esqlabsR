# PIParameter() errors on inverted bounds

    Code
      PIParameter(id = "x", scenarios = "S1", path = "Organism|x|y", minValue = 5,
        maxValue = 1, startValue = 3)
    Condition
      Error in `PIParameter()`:
      ! Parameter "Organism|x|y" has invalid bounds: Min=5, Start=3, Max=1. Expected: Min <= Start <= Max

# PIParameter() errors when start is outside [min, max]

    Code
      PIParameter(id = "x", scenarios = "S1", path = "Organism|x|y", minValue = 0,
        maxValue = 1, startValue = 10)
    Condition
      Error in `PIParameter()`:
      ! Parameter "Organism|x|y" has invalid bounds: Min=0, Start=10, Max=1. Expected: Min <= Start <= Max

# PIParameter() errors on empty scenarios

    Code
      PIParameter(id = "x", scenarios = character(0), path = "Organism|x|y",
      minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `PIParameter()`:
      ! Field `scenarios` on PIParameter "x" must be a non-empty character vector.

# PIOutputMapping() errors on missing required fields

    Code
      PIOutputMapping(id = "x", scenarios = "S1", outputPathId = "", observedDataId = "Laskin")
    Condition
      Error in `PIOutputMapping()`:
      ! Required field "outputPathId" is missing or empty on PIOutputMapping "x".

# PITask() errors when parameters is empty

    Code
      PITask(id = "x", scenarios = "S1", parameters = list(), outputMappings = list(
        PIOutputMapping(id = "m", scenarios = "S1", outputPathId = "PVB",
          observedDataId = "Laskin")))
    Condition
      Error in `PITask()`:
      ! Field "parameters" on PITask "x" must contain at least one entry.

# PITask() errors when outputMappings is empty

    Code
      PITask(id = "x", scenarios = "S1", parameters = list(PIParameter(id = "k",
        scenarios = "S1", path = "x|y", minValue = 0, maxValue = 1, startValue = 0.5)),
      outputMappings = list())
    Condition
      Error in `PITask()`:
      ! Field "outputMappings" on PITask "x" must contain at least one entry.

# PITask() errors on empty scenarios

    Code
      PITask(id = "x", scenarios = character(0), parameters = list(PIParameter(id = "k",
        scenarios = "S1", path = "x|y", minValue = 0, maxValue = 1, startValue = 0.5)),
      outputMappings = list(PIOutputMapping(id = "m", scenarios = "S1", outputPathId = "PVB",
        observedDataId = "Laskin")))
    Condition
      Error in `PITask()`:
      ! Field `scenarios` on PITask "x" must be a non-empty character vector.

# PITask() errors when parameters contains non-PIParameter elements

    Code
      PITask(id = "x", scenarios = "S1", parameters = list("not a record"),
      outputMappings = list(PIOutputMapping(id = "m", scenarios = "S1", outputPathId = "PVB",
        observedDataId = "Laskin")))
    Condition
      Error in `PITask()`:
      ! Element parameters[[1]] on PITask "x" must be a PIParameter.

# PITask() errors when outputMappings contains non-PIOutputMapping elements

    Code
      PITask(id = "x", scenarios = "S1", parameters = list(PIParameter(id = "k",
        scenarios = "S1", path = "x|y", minValue = 0, maxValue = 1, startValue = 0.5)),
      outputMappings = list(42))
    Condition
      Error in `PITask()`:
      ! Element outputMappings[[1]] on PITask "x" must be a PIOutputMapping.

# print(PIParameter) renders a compact summary

    Code
      print(p)
    Output
      <PIParameter>
        * Id: k_liver
        * Scenarios: S1, S2
        * Path: Organism|Liver|Volume
        * Units: l
        * Min / Start / Max: 0.5 / 1 / 2

# print(PIOutputMapping) renders a compact summary

    Code
      print(m)
    Output
      <PIOutputMapping>
        * Id: PVB_obs
        * Scenarios: S1
        * Output Path Id: Aciclovir_PVB
        * Observed Data Id: Laskin_GroupA
        * Scaling: lin
        * Weight: 1, 2, 3

# print(PITask) renders header, scenarios, parameter count, mapping count, algorithm

    Code
      print(t)
    Output
      <PITask>
        * Id: AciclovirSimple
        * Scenarios: S1, S2
        * Number of Parameters: 1
        * Number of Output Mappings: 1
        * Algorithm: Monte-Carlo
        * CI Method: hessian

# runPI(project) refuses to run when validation has critical errors

    Code
      runPI(project)
    Condition
      Error in `.abortValidationErrors()`:
      ! Cannot runPI: project has 3 critical validation errors.
      x [crossReferences] PI task 'T' references undefined scenarios: DoesNotExist
      x [crossReferences] PI task 'T', parameter 'p' references undefined scenarios: DoesNotExist
      x [crossReferences] PI task 'T', outputMapping 'm' references undefined scenarios: DoesNotExist
      i Run `validateProject(project)` for a full report.

# createPITasks() emits a soft-deprecation warning

    Code
      createPITasks()
    Condition
      Warning:
      `createPITasks()` was deprecated in esqlabsR 6.0.0.
      i Please use the `project` argument of `runPI()` instead.
      i createPITasks() is removed. runPI(project) builds and runs PI tasks in one step.
      Error in `createPITasks()`:
      ! `createPITasks()` has been removed. Use `runPI()`(`project`, piTaskNames = ...).

# runPI() with the legacy first-arg shape (non-Project) emits a soft-deprecation warning

    Code
      runPI(list(SomeTask = "fake"))
    Condition
      Warning:
      The `piTasks` argument of `runPI()` is deprecated as of esqlabsR 6.0.0.
      i Please use the `project` argument instead.
      i Pass a Project object loaded with loadProject() instead of a pre-built list of ParameterIdentification objects.
      Error in `runPI()`:
      ! `runPI()` now requires a <Project> object as its first argument. Migrate via `loadProject()` and a parameterIdentification section in your Project.json.

# addPITask() errors on unknown scenario id

    Code
      addPITask(project, id = "Bad", scenarios = "Ghost", parameters = list(
        PIParameter(id = "k", scenarios = "Ghost", path = "x|y", minValue = 0,
          maxValue = 1, startValue = 0.5)), outputMappings = list(PIOutputMapping(id = "m",
        scenarios = "Ghost", outputPathId = "Aciclovir_PVB", observedDataId = "Laskin")))
    Condition
      Error in `addPITask()`:
      ! Cannot add PI task "Bad":
      x scenarios not found in project$scenarios: Ghost

# addPITask() errors on unknown outputPathId

    Code
      addPITask(project, id = "Bad", scenarios = "TestScenario", parameters = list(
        PIParameter(id = "k", scenarios = "TestScenario", path = "x|y", minValue = 0,
          maxValue = 1, startValue = 0.5)), outputMappings = list(PIOutputMapping(id = "m",
        scenarios = "TestScenario", outputPathId = "DoesNotExist", observedDataId = "Laskin")))
    Condition
      Error in `addPITask()`:
      ! Cannot add PI task "Bad":
      x outputPathId 'DoesNotExist' not found in project$outputPaths

# addPITask() errors on duplicate id

    Code
      do.call(addPITask, c(list(project = project), args))
    Condition
      Error:
      ! Cannot add PI task "Dup":
      x PI task 'Dup' already exists

# removePITask() warns and no-ops on missing id

    Code
      removePITask(project, "NotThere")
    Condition
      Warning:
      PI task "NotThere" not found; no-op.

# addPIParameter() errors on unknown taskId

    Code
      addPIParameter(project, taskId = "Ghost", id = "p", scenarios = "TestScenario",
        path = "x|y", minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `addPIParameter()`:
      ! PI task "Ghost" not found

# addPIParameter() errors on unknown scenario id

    Code
      addPIParameter(project, taskId = "T", id = "ghost-param", scenarios = "Ghost",
        path = "a|b", minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `addPIParameter()`:
      ! scenarios not found: "Ghost"

# removePIParameter() warns and no-ops on missing id

    Code
      removePIParameter(project, taskId = "T", id = "ghost")
    Condition
      Warning:
      Parameter "ghost" not found in task "T"; no-op.

# addPIOutputMapping() errors on unknown outputPathId

    Code
      addPIOutputMapping(project, taskId = "T", id = "m2", scenarios = "TestScenario",
        outputPathId = "DoesNotExist", observedDataId = "L")
    Condition
      Error in `addPIOutputMapping()`:
      ! outputPathId "DoesNotExist" not found in project$outputPaths

# addPIParameter() errors on an explicit duplicate id

    Code
      addPIParameter(project, taskId = "T", id = "dup", path = "a|b", scenarios = "TestScenario",
        minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `addPIParameter()`:
      ! Parameter "dup" already exists in task "T"

# PIOutputMapping() validates scaling and the offset / factor / weight fields

    Code
      PIOutputMapping(id = "m", scenarios = "S1", outputPathId = "P", observedDataId = "D",
        xOffset = "not a number")
    Condition
      Error in `PIOutputMapping()`:
      ! Field `xOffset` on PIOutputMapping "m" is invalid: "not a number". Expected a finite numeric value.

---

    Code
      PIOutputMapping(id = "m", scenarios = "S1", outputPathId = "P", observedDataId = "D",
        weight = "heavy")
    Condition
      Error in `PIOutputMapping()`:
      ! Field `weight` on PIOutputMapping "m" is invalid: "heavy". Expected a finite numeric value.

---

    Code
      PIOutputMapping(id = "m", scenarios = "S1", outputPathId = "P", observedDataId = "D",
        scaling = "")
    Condition
      Error in `PIOutputMapping()`:
      ! Field `scaling` on PIOutputMapping "m" is invalid: "". Expected a non-empty string.

# addPITask() rejects malformed outputMappings with a typed error

    Code
      addPITask(project, id = "T", scenarios = "TestScenario", parameters = list(
        PIParameter(id = "k", scenarios = "TestScenario", path = "x|y", minValue = 0,
          maxValue = 1, startValue = 0.5)), outputMappings = list("not a mapping"))
    Condition
      Error in `PITask()`:
      ! Element outputMappings[[1]] on PITask "T" must be a PIOutputMapping.

# runPI(piTaskNames = ) aborts on an unknown task name

    Code
      runPI(project, piTaskNames = "Ghost")
    Condition
      Error in `runPI()`:
      ! Unknown `piTaskNames`: "Ghost". Available: "AciclovirSimple".

