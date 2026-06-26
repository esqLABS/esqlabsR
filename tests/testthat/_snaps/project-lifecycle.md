# saveProject errors when project has no jsonPath and path is NULL

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      ! No `path` specified and the project has no jsonPath. Provide a `path` to save the project for the first time.

# initProject aborts non-interactively when a project exists and overwrite = FALSE

    Code
      initProject(destination = dir, type = "minimal", createExcel = FALSE)
    Condition
      Error in `initProject()`:
      ! The destination already contains an esqlabsR project and cannot prompt in a non-interactive session. Pass `overwrite = TRUE` to overwrite it.

# initProject aborts when the user declines the overwrite prompt

    Code
      initProject(destination = dir, type = "minimal", createExcel = FALSE)
    Condition
      Error in `initProject()`:
      ! Aborted by user.

