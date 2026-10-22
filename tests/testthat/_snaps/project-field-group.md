# a field with no setter is read-only and names itself when assigned

    Code
      group$schemaVersion <- "3.0"
    Condition
      Error:
      ! info$schemaVersion is read-only and cannot be assigned into.

# a group can supply its own read-only handler

    Code
      group$scenarios <- list()
    Condition
      Error in `onReadOnly()`:
      ! scenarios is read-only and cannot be assigned into.
      i To change a definition, edit its '.json' file or use an authoring function (e.g. `addScenario()` / `setScenario()` / `removeScenario()` and their per-section siblings).
      i To edit one record, read it, change the copy, then re-submit it with an authoring function: `sc <- project$definitions$scenarios[["id"]]; sc$field <- value; setScenario(project, "id", ...)`.

# .projectFieldReadOnlyError names the field and its group

    Code
      .projectFieldReadOnlyError("name", "info")
    Condition
      Error:
      ! info$name is read-only and cannot be assigned into.

