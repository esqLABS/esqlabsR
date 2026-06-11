# validateProject() rejects non-Project inputs

    Code
      validateProject("not a project")
    Condition
      Error in `validateProject()`:
      ! `project` must be a <Project> object; got <character>.

# validatedSinceMutation is read-only

    Code
      project$validatedSinceMutation <- TRUE
    Condition
      Error:
      ! validatedSinceMutation is readonly

# .ensureValid aborts with a formatted summary on critical errors

    Code
      esqlabsR:::.ensureValid(project, sections = c("scenarios"), opName = "runScenarios")
    Condition
      Error in `.abortValidationErrors()`:
      ! Cannot runScenarios: project has 1 critical validation error.
      x [scenarios] Scenario 's1' has no modelFile
      i Run `validateProject(project)` for a full report.

# createPlots(validate = TRUE) aborts on a clearly broken project

    Code
      createPlots(project)
    Condition
      Error in `.abortValidationErrors()`:
      ! Cannot createPlots: project has 1 critical validation error.
      x [plots] plotConfiguration references unknown DataCombinedName: Ghost
      i Run `validateProject(project)` for a full report.

# .abortValidationErrors escapes glue metacharacters in messages

    Code
      esqlabsR:::.abortValidationErrors(results, "runScenarios")
    Condition
      Error in `esqlabsR:::.abortValidationErrors()`:
      ! Cannot runScenarios: project has 2 critical validation errors.
      x [scenarios] Scenario "Dose {mg}" is broken
      x [scenarios] Scenario S{1} also broken
      i Run `validateProject(project)` for a full report.

# removeObservedData warns when a dataCombined still references it

    Code
      removeObservedData(project, "MyObs")
    Condition
      Warning:
      Removed observedData "MyObs" is still referenced by 1 dataCombined entry:
      * DC1
      i These dataCombined entries now have a dangling reference. Update or remove them.

