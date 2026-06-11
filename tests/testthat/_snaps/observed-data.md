# loadObservedData errors when an entry has invalid type

    Code
      loadObservedData(project)
    Condition
      Error in `.validateObservedDataEntry()`:
      ! x Invalid type "invalid_type" in `observedData` entry. i Must be one of: "excel", "pkml", "script", and "programmatic".

# loadObservedData errors when excel entry is missing required fields

    Code
      loadObservedData(project)
    Condition
      Error in `.validateObservedDataEntry()`:
      ! x `observedData` entry 1 (type "excel") is missing required field importerConfiguration.

# loadObservedData errors when dataFolder is not declared

    Code
      loadObservedData(project)
    Condition
      Error in `.resolveDataPath()`:
      ! dataFolder is not declared in `filePaths`; cannot resolve 'x.pkml'.

