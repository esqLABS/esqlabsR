# snapshotProject errors over an existing file unless overwrite

    Code
      snapshotProject(project, dir = dir, name = "study")
    Condition
      Error in `snapshotProject()`:
      ! A snapshot file already exists at '<tmp-path>.esqlabsR'.
      i Pass `overwrite = TRUE` to replace it, or a different `name`.

# snapshotProject rejects a name that escapes dir

    Code
      snapshotProject(project, dir = dir, name = "../backup")
    Condition
      Error in `.validateSnapshotStem()`:
      ! `name` must be a single filename stem without path separators.
      x The stem "../backup" contains a path separator or is "." / "..", so it could write outside `dir`.
      i Pass a single filename segment (no path separator and not "." / ".."), or leave `name` as `NULL` for a timestamped default.

---

    Code
      snapshotProject(project, dir = dir, name = "sub/study")
    Condition
      Error in `.validateSnapshotStem()`:
      ! `name` must be a single filename stem without path separators.
      x The stem "sub/study" contains a path separator or is "." / "..", so it could write outside `dir`.
      i Pass a single filename segment (no path separator and not "." / ".."), or leave `name` as `NULL` for a timestamped default.

# restoreProject with overwrite = TRUE rolls back in place and warns

    Code
      rolledBack <- restoreProject(out, dir, overwrite = TRUE)
    Condition
      Warning:
      Replaced the existing project in '<tmp-path>' with the snapshot.
      ! Any <Project> previously loaded from `dir` is now stale.
      i Rebind to the object `restoreProject()` returned, or `reloadProject()` the old handle.

