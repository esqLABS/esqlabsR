# a clean saveProject() is a no-op with the up-to-date message

    Code
      saveProject(project)
    Message
      Project is already up to date; nothing to save.

# saveProject() on an unbound in-memory project aborts

    Code
      saveProject(project)
    Condition
      Error in `saveProject()`:
      ! This project is not bound to a directory, so there is nothing to save in place.
      i Use `snapshotProject()` to write a portable single-file snapshot.
      i Use `initProject()` then `loadProject()` to give the project a home on disk.

# reloadProject() on an unbound in-memory project aborts

    Code
      reloadProject(project)
    Condition
      Error in `reloadProject()`:
      ! This project is not bound to a directory, so there is nothing to reload from.
      i `reloadProject()` re-reads a project's on-disk tree; an in-memory project has none.

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

