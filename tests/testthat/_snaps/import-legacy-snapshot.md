# loadProject on a previous-version snapshot points at restoreProject

    Code
      loadProject(snapshot)
    Condition
      Error:
      x '<path>' is a previous-version project snapshot, not a project of the current format.
      i A previous-version snapshot has to be upgraded before it can be opened.
      i Upgrade it into a new folder with `restoreProject(<snapshot>, dir = <newFolder>)`, which returns the upgraded project.

