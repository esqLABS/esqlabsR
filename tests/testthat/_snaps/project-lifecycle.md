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
      ! This project does not have a project folder on disk yet, so it cannot be saved.
      i Use `snapshotProject()` to save it to a single file.
      i Or create a project folder with `initProject()` and load it with `loadProject()`.

# reloadProject() on an unbound in-memory project aborts

    Code
      reloadProject(project)
    Condition
      Error in `reloadProject()`:
      ! This project does not have a project folder on disk, so there is nothing to reload.
      i `reloadProject()` re-reads the project files from disk; this project was not loaded from a folder.

# initProject aborts non-interactively when a project exists and overwrite = FALSE

    Code
      initProject(destination = dir, type = "minimal", createExcel = FALSE)
    Condition
      Error in `initProject()`:
      ! The destination folder already contains an esqlabsR project. R is not running interactively, so esqlabsR cannot ask for confirmation; pass `overwrite = TRUE` to overwrite it.

# initProject aborts when the user declines the overwrite prompt

    Code
      initProject(destination = dir, type = "minimal", createExcel = FALSE)
    Condition
      Error in `initProject()`:
      ! Aborted by user.

