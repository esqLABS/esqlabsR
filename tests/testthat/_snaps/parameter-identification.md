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

# PIParameter() errors on NA units

    Code
      PIParameter(id = "x", scenarios = "S1", path = "Organism|x|y", units = NA_character_,
        minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `PIParameter()`:
      ! Required field "units" is missing or empty on PIParameter "x".

# PIParameter() errors on non-scalar units

    Code
      PIParameter(id = "x", scenarios = "S1", path = "Organism|x|y", units = c("l",
        "ml"), minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `PIParameter()`:
      ! Required field "units" is missing or empty on PIParameter "x".

# PIOutputMapping() errors on missing required fields

    Code
      PIOutputMapping(id = "x", scenarios = "S1", outputPath = "", observedData = "Laskin")
    Condition
      Error in `PIOutputMapping()`:
      ! Required field "outputPath" is missing or empty on PIOutputMapping "x".

# PITask() errors when parameters or outputMappings is not a list

    Code
      PITask(id = "x", scenarios = "S1", parameters = "nope", outputMappings = list())
    Condition
      Error in `PITask()`:
      ! Field "parameters" on PITask "x" must be a list.

---

    Code
      PITask(id = "x", scenarios = "S1", parameters = list(), outputMappings = "nope")
    Condition
      Error in `PITask()`:
      ! Field "outputMappings" on PITask "x" must be a list.

# PITask() errors on a malformed scenarios entry

    Code
      PITask(id = "x", scenarios = c("S1", NA))
    Condition
      Error in `PITask()`:
      ! Field `scenarios` on PITask "x" must be a character vector of scenario ids with no NA or empty entries, or empty for none.

---

    Code
      PITask(id = "x", scenarios = "")
    Condition
      Error in `PITask()`:
      ! Field `scenarios` on PITask "x" must be a character vector of scenario ids with no NA or empty entries, or empty for none.

---

    Code
      PITask(id = "x", scenarios = 1)
    Condition
      Error in `PITask()`:
      ! Field `scenarios` on PITask "x" must be a character vector of scenario ids with no NA or empty entries, or empty for none.

# PITask() errors when parameters contains non-PIParameter elements

    Code
      PITask(id = "x", scenarios = "S1", parameters = list("not a record"),
      outputMappings = list(PIOutputMapping(id = "m", scenarios = "S1", outputPath = "PVB",
        observedData = "Laskin")))
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
        * Output Path Id: aciclovir_pvb
        * Observed Data Id: Laskin_GroupA
        * Scaling: lin
        * Weight: 1, 2, 3

# print(PITask) renders header, scenarios, parameter count, mapping count, algorithm

    Code
      print(t)
    Output
      <PITask>
        * Id: aciclovirsimple
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
      x [crossReferences] PI task 't' references undefined scenarios: DoesNotExist
      x [crossReferences] PI task 't', parameter 'p' references undefined scenarios: DoesNotExist
      x [crossReferences] PI task 't', outputMapping 'm' references undefined scenarios: DoesNotExist
      i Run `validateProject(project)` for a full report.

# .warnUnquantifiedUncertainty fires once per NA-uncertainty parameter

    Code
      .warnUnquantifiedUncertainty("myTask", fakeResult)
    Condition
      Warning:
      Parameter identification task "myTask": uncertainty could not be quantified for parameter "k_clear" (standard deviation, CV, and confidence interval are all "NA").
      i The reported estimate has no usable uncertainty even though convergence is reported. Likely causes: a singular or ill-conditioned Hessian, the estimate sitting at a parameter bound, or the objective being insensitive to this parameter.

# createPITasks() is defunct and aborts

    Code
      createPITasks()
    Condition
      Error:
      ! `createPITasks()` was deprecated in esqlabsR 6.0.0 and is now defunct.
      i Please use the `project` argument of `runPI()` instead.
      i createPITasks() is removed. runPI(project) builds and runs PI tasks in one step.

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

# setPITask() errors on an unknown task, an unknown scenario, and a field it cannot set

    Code
      setPITask(project, "ghost", scenarios = "x")
    Condition
      Error in `setPITask()`:
      ! Cannot modify PI task "ghost": it does not exist.
      i Use `addPITask()` to create it first.

---

    Code
      setPITask(project, "cfg", scenarios = "ghost")
    Condition
      Error in `setPITask()`:
      ! Cannot modify PI task "cfg":
      x scenarios not found in `project$definitions$scenarios`: "ghost"

---

    Code
      project$setPITask("cfg", parameters = list())
    Condition
      Error:
      ! `setPITask()` cannot set parameters.
      i It sets scenarios and configuration; use `addPIParameter()` / `addPIOutputMapping()` for a task's parameters and output mappings.

# addPITask() errors on unknown scenario id

    Code
      addPITask(project, id = "Bad", scenarios = "Ghost", parameters = list(
        PIParameter(id = "k", scenarios = "Ghost", path = "x|y", minValue = 0,
          maxValue = 1, startValue = 0.5)), outputMappings = list(PIOutputMapping(id = "m",
        scenarios = "Ghost", outputPath = "aciclovir_pvb", observedData = "Laskin")))
    Condition
      Warning:
      Canonicalized 2 ids to a safe form:
      * "Bad" -> "bad"
      * "Ghost" -> "ghost"
      Error in `addPITask()`:
      ! Cannot add PI task "bad":
      x scenarios not found in project$definitions$scenarios: ghost

# addPITask() errors on unknown outputPath

    Code
      addPITask(project, id = "Bad", scenarios = "testscenario", parameters = list(
        PIParameter(id = "k", scenarios = "testscenario", path = "x|y", minValue = 0,
          maxValue = 1, startValue = 0.5)), outputMappings = list(PIOutputMapping(id = "m",
        scenarios = "testscenario", outputPath = "DoesNotExist", observedData = "Laskin")))
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Bad" -> "bad"
      Error in `addPITask()`:
      ! Cannot add PI task "bad":
      x outputPath 'DoesNotExist' is neither a defined output-path id nor the model path of one. Pass an output-path id (a key in project$definitions$outputPaths) or the literal model path of a defined output path; define new ones with addOutputPath().

# addPITask() errors on duplicate id

    Code
      do.call(addPITask, c(list(project = project), args))
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Dup" -> "dup"
      Error:
      ! Cannot add PI task "dup":
      x PI task 'dup' already exists; pass overwrite = TRUE to replace it

# removePITask() warns and no-ops on missing id

    Code
      removePITask(project, "NotThere")
    Condition
      Warning:
      PI task "notthere" not found; no-op.
      Warning:
      Canonicalized 1 id to a safe form:
      * "NotThere" -> "notthere"

# addPIParameter() errors on unknown task

    Code
      addPIParameter(project, task = "Ghost", id = "p", scenarios = "testscenario",
        path = "x|y", minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `addPIParameter()`:
      ! PI task "ghost" not found

# addPIParameter() errors on unknown scenario id

    Code
      addPIParameter(project, task = "t", id = "ghost-param", scenarios = "Ghost",
        path = "a|b", minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `addPIParameter()`:
      ! scenarios not found: "ghost"

# removePIParameter() warns and no-ops on missing id

    Code
      removePIParameter(project, task = "t", id = "ghost")
    Condition
      Warning:
      Parameter "ghost" not found in task "t"; no-op.

# addPIOutputMapping() errors on unknown outputPath

    Code
      addPIOutputMapping(project, task = "t", id = "m2", scenarios = "testscenario",
        outputPath = "DoesNotExist", observedData = "L")
    Condition
      Error in `addPIOutputMapping()`:
      ! x outputPath "DoesNotExist" is neither a defined output-path id nor the model path of one. i Pass an output-path id (a key in `project$definitions$outputPaths`) or the literal model path of a defined output path; define new ones with `addOutputPath()`.

# addPIParameter() errors on an explicit duplicate id

    Code
      addPIParameter(project, task = "t", id = "dup", path = "a|b", scenarios = "testscenario",
        minValue = 0, maxValue = 1, startValue = 0.5)
    Condition
      Error in `addPIParameter()`:
      ! Parameter "dup" already exists in task "t".
      i Pass `overwrite = TRUE` to replace it.

# PIOutputMapping() validates scaling and the offset / factor / weight fields

    Code
      PIOutputMapping(id = "m", scenarios = "S1", outputPath = "P", observedData = "D",
        xOffset = "not a number")
    Condition
      Error in `PIOutputMapping()`:
      ! Field `xOffset` on PIOutputMapping "m" is invalid: "not a number". Expected a finite numeric value.

---

    Code
      PIOutputMapping(id = "m", scenarios = "S1", outputPath = "P", observedData = "D",
        weight = "heavy")
    Condition
      Error in `PIOutputMapping()`:
      ! Field `weight` on PIOutputMapping "m" is invalid: "heavy". Expected a finite numeric value.

---

    Code
      PIOutputMapping(id = "m", scenarios = "S1", outputPath = "P", observedData = "D",
        scaling = "")
    Condition
      Error in `PIOutputMapping()`:
      ! Field `scaling` on PIOutputMapping "m" is invalid: "". Expected a non-empty string.

# addPITask() rejects malformed outputMappings with a typed error

    Code
      addPITask(project, id = "t", scenarios = "testscenario", parameters = list(
        PIParameter(id = "k", scenarios = "testscenario", path = "x|y", minValue = 0,
          maxValue = 1, startValue = 0.5)), outputMappings = list("not a mapping"))
    Condition
      Error in `PITask()`:
      ! Element outputMappings[[1]] on PITask "t" must be a PIOutputMapping.

# runPI(tasks = ) aborts on an unknown task name

    Code
      runPI(project, tasks = "Ghost")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Ghost" -> "ghost"
      Error in `runPI()`:
      ! Unknown `tasks`: "ghost". Available: "aciclovirsimple".

# .parsePIOutputMappings loads a mapping the constructor would reject

    Code
      PIOutputMapping(id = "m1", scenarios = "s1", observedData = "d1")
    Condition
      Error in `PIOutputMapping()`:
      ! argument "outputPath" is missing, with no default

# .parsePIParameters loads a parameter the constructor would reject

    Code
      PIParameter(id = "p1", scenarios = "s1", path = "Aciclovir|Lipophilicity",
        maxValue = 2, startValue = 0)
    Condition
      Error in `PIParameter()`:
      ! argument "minValue" is missing, with no default

