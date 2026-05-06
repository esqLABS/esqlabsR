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
      ! validatedSinceMutation is read-only.

# .lookupSectionValidatorAdapter errors on unknown section

    Code
      esqlabsR:::.lookupSectionValidatorAdapter("doesNotExist")
    Condition
      Error in `esqlabsR:::.lookupSectionValidatorAdapter()`:
      ! No validator adapter found for section "doesNotExist".
      i Define `.doesNotExistValidatorAdapter <- function(project) ...` in the section's R file.

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

