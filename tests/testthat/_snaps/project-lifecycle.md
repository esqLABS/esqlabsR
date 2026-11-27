# loadProject() on the esqlabsR 5 snapshot JSON says how to restore and import it

    Code
      loadProject(snapshot)
    Condition
      Error:
      x '<tmp-path>' is the JSON file that `snapshotProjectConfiguration()` saved in esqlabsR 5, not a project of the current format.
      i In esqlabsR 5.7.0, the last esqlabsR 5 release, restore the project's Excel files from it with `restoreProjectConfiguration()`.
      i Then, in this version, import the restored 'ProjectConfiguration.xlsx', written beside the JSON file, with `importProjectFromExcel()`.

# loadProject() on a folder holding only the esqlabsR 5 snapshot JSON gives the restore advice

    Code
      loadProject(dir)
    Condition
      Error:
      x '<tmp-path>' is the JSON file that `snapshotProjectConfiguration()` saved in esqlabsR 5, not a project of the current format.
      i In esqlabsR 5.7.0, the last esqlabsR 5 release, restore the project's Excel files from it with `restoreProjectConfiguration()`.
      i Then, in this version, import the restored 'ProjectConfiguration.xlsx', written beside the JSON file, with `importProjectFromExcel()`.

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
      i Create a project folder with `initProject()` and load it with `loadProject()`.

# initProject() rejects a name that is not a single string

    Code
      initProject(destination, createExcel = FALSE, name = c("a", "b"))
    Condition
      Error in `initProject()`:
      ! `name` must be a single non-NA string.

---

    Code
      initProject(destination, createExcel = FALSE, name = NA_character_)
    Condition
      Error in `initProject()`:
      ! `name` must be a single non-NA string.

# initProject does not scaffold over an unmigrated legacy Excel project

    Code
      initProject(destination = dir, type = "minimal", createExcel = FALSE)
    Condition
      Error in `initProject()`:
      ! The destination folder already contains an esqlabsR project. R is not running interactively, so esqlabsR cannot ask for confirmation; pass `overwrite = TRUE` to overwrite it.

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

