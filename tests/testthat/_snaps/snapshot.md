# snapshotProject errors over an existing file unless overwrite

    Code
      snapshotProject(project, dir = dir, name = "study")
    Condition
      Error in `snapshotProject()`:
      ! A snapshot file already exists at '<tmp-path>.esqlabsR'.
      i Pass `overwrite = TRUE` to replace it, or a different `name`.

# restoreProject with overwrite = TRUE rolls back in place and warns

    Code
      rolledBack <- restoreProject(out, dir, overwrite = TRUE)
    Condition
      Warning:
      Replaced the existing project in '<tmp-path>' with the snapshot.
      ! Any <Project> previously loaded from `dir` is now stale.
      i Rebind to the object `restoreProject()` returned, or `reloadProject()` the old handle.

