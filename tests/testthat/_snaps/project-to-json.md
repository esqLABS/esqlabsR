# .outputPathsToJson errors on a non-empty unnamed value

    Code
      esqlabsR:::.outputPathsToJson(project)
    Condition
      Error in `esqlabsR:::.outputPathsToJson()`:
      ! outputPaths must be a named map of id to path string.
      i Found 1 entry without an id.

