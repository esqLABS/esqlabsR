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
      Error in `snapshotProject()`:
      ! `name` must be a single filename stem without path separators.
      x The stem "../backup" contains a path separator or is "." / "..", so it could write outside `dir`.
      i Pass a single filename segment (no path separator and not "." / ".."), or leave `name` as `NULL` for a timestamped default.

---

    Code
      snapshotProject(project, dir = dir, name = "sub/study")
    Condition
      Error in `snapshotProject()`:
      ! `name` must be a single filename stem without path separators.
      x The stem "sub/study" contains a path separator or is "." / "..", so it could write outside `dir`.
      i Pass a single filename segment (no path separator and not "." / ".."), or leave `name` as `NULL` for a timestamped default.

# restoreProject with overwrite = TRUE rolls back in place and warns

    Code
      rolledBack <- restoreProject(out, dir, overwrite = TRUE)
    Condition
      Warning:
      Replaced the project in '<tmp-path>' with the snapshot.
      ! Project objects loaded from this folder before the restore still contain the old project.
      i Continue with the project returned by `restoreProject()`, or call `reloadProject()` on the old object.

