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

# .validatePlots warns on plotType-irrelevant fields (non-blocking)

    Code
      cat(sort(msgs), sep = "\n")
    Output
      Plot 'p_ind' of plotType 'individual' sets 'quantiles', which only applies to plotType 'population' and is ignored.
      Plot 'p_pop' of plotType 'population' sets 'foldDistance', which only applies to plotType 'observedVsSimulated' and is ignored.

# .validateCrossReferences suggests a near match for an individual's parameterSets

    Code
      cat(msgs[grepl("Individual", msgs)], sep = "\n")
    Output
      Individual 'I1' references undefined parameterSets: PresysSet2 (did you mean 'PresysSet1'?)

# .validateCrossReferences suggests a near match for an application's parameterSets

    Code
      cat(msgs[grepl("Application", msgs)], sep = "\n")
    Output
      Application 'A1' references undefined parameterSets: PresysSet2 (did you mean 'PresysSet1'?)

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
      x [plots] plotConfiguration references unknown dataCombinedId: Ghost
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
      * dc1
      i These dataCombined entries now have a dangling reference. Update or remove them.

# print.ValidationResults renders a grouped summary with glyphs

    Code
      print(.fakeValidationResults())
    Output
      Validation report: 2 critical errors, 2 warnings.
      scenarios
        x [Invalid Reference] Scenario 'S1' references undefined individual 'ghost'
        ! [Data] Scenario 'S1' modelFile not found on disk
      parameterSets
        ! [Data] No parameter sets defined
      crossReferences
        x [Invalid Reference] dataCombined references undefined scenarios: ghost
      1 section OK.

# print.ValidationResults renders an all-OK line for a clean result

    Code
      print(clean)
    Output
      Validation report: 0 critical errors, 0 warnings.
      v No issues found.

