# saveSnapshot refuses the own container path

    Code
      saveSnapshot(project, project$jsonPath)
    Condition
      Error in `saveSnapshot()`:
      ! A snapshot is a derived artifact and must be written to a location other than the project's own jsonPath.
      i Pass a `path` to a different file. The authoritative 'definitions/' tree and 'Project.json' container are already write-through, so there is nothing to save in place.

