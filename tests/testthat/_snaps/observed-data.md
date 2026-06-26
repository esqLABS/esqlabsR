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

# addObservedData rejects an under-specified config entry

    Code
      addObservedData(project, list(type = "excel", file = "x.xlsx"))
    Condition
      Error in `.validateObservedDataEntry()`:
      ! x `observedData` entry 2 (type "excel") is missing required field importerConfiguration.

# addObservedData rejects a duplicate config entry file

    Code
      addObservedData(project, list(type = "excel", file = "Aciclovir_TimeValuesData.xlsx",
        importerConfiguration = "esqlabs_dataImporter_configuration.xml", sheets = list(
          "Laskin 1982.Group A")))
    Condition
      Error in `addObservedData()`:
      ! observedData entry with file "Aciclovir_TimeValuesData.xlsx" already exists

