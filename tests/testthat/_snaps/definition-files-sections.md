# a definitions/<kind>/ path that is a file aborts the load

    Code
      loadProject(jsonPath)
    Condition
      Error in `.assertDefinitionTreePathIsDir()`:
      ! Project definition path '<project>/definitions/individuals' exists but is not a directory.
      x 'definitions/individuals' must be a directory of definition files.
      i A regular file here is a corrupted or mis-synced project tree.

# a definitions/ root that is a file aborts the load

    Code
      loadProject(jsonPath)
    Condition
      Error in `.assertDefinitionTreePathIsDir()`:
      ! Project definition path '<project>/definitions' exists but is not a directory.
      x 'definitions' must be a directory of definition files.
      i A regular file here is a corrupted or mis-synced project tree.

# a keyed file missing its id field aborts naming the file

    Code
      loadProject(jsonPath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind individual has no usable individualId.
      x individualId must be a single non-empty string (it names the definition and its file).
      i Check '<project>/definitions/individuals/adult_male.json'.

# a keyed file whose inner id disagrees with its filename aborts

    Code
      loadProject(jsonPath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind outputPath has a stored id that disagrees with its filename.
      x id is "different" but the file is "aciclovir_fat_cell".json.
      i The filename stem is the definition's id; rename the file or the id so they match. Check '<project>/definitions/output-paths/aciclovir_fat_cell.json'.

# two files with the same inner id cannot silently collapse

    Code
      loadProject(jsonPath)
    Condition
      Error in `.keyedTreeRecordId()`:
      ! A definition file for kind outputPath has a stored id that disagrees with its filename.
      x id is "aciclovir_fat_cell" but the file is "duplicate".json.
      i The filename stem is the definition's id; rename the file or the id so they match. Check '<project>/definitions/output-paths/duplicate.json'.

# an empty-object scalar field on a non-scenario kind aborts the load

    Code
      loadProject(jsonPath)
    Condition
      Error in `.assertNoEmptyObjectFields()`:
      ! A definition of kind individual has an invalid species.
      x species is an empty object `{}` where a single value or `null` was expected.
      i A hand-edit that turned `"species": null` into `{}` (the usual jsonlite round-trip) is the usual cause; restore the value or remove the field.
      i Check '<project>/definitions/individuals/adult_male.json'.

# a plots entry whose stored id differs from its map key aborts

    Code
      .serializePlotEntrySet(entries, "plotId", "plot")
    Condition
      Error in `.serializePlotEntrySet()`:
      ! A plot entry's plotId disagrees with its map key.
      x The map key is "p1" but plotId is "p2".
      i They must agree so the on-disk filename stays the authoritative key; store the entry under its plotId.

# a PI task whose $id differs from its map key aborts

    Code
      .serializePITaskSet(tasks)
    Condition
      Error in `.serializePITaskSet()`:
      ! A parameterIdentification task's id disagrees with its map key.
      x The map key is "task1" but id is "task2".
      i They must agree so the on-disk filename stays the authoritative key; store the task under its id.

